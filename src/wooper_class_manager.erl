% Copyright (C) 2007-2019 Olivier Boudeville
%
% This file is part of the Ceylan-WOOPER library.
%
% This library is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License or
% the GNU General Public License, as they are published by the Free Software
% Foundation, either version 3 of these Licenses, or (at your option)
% any later version.
% You can also redistribute it and/or modify it under the terms of the
% Mozilla Public License, version 1.1 or later.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License and the GNU General Public License
% for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License, of the GNU General Public License and of the Mozilla Public License
% along with this library.
% If not, see <http://www.gnu.org/licenses/> and
% <http://www.mozilla.org/MPL/>.
%
% Creation date: Friday, July 12, 2007.
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]



% Module corresponding to the WOOPER class manager singleton.
%
% The purpose of this process is, on a per-node basis, to create and notably to
% serve to instances the virtual table corresponding to the actual class they
% are corresponding to.
%
% This way each virtual table is computed only once per node, and no significant
% per-instance memory is used for the virtual table: all the instances of a
% given class just refer to a common virtual table stored by this manager, and
% each virtual table and the table of virtual tables itself are optimised, with
% regard to their respective load factor.

% Local registration only of this manager, as we want the WOOPER instances to
% benefit from a local direct reference to the same method table
% (theoretically), rather to waste memory with one copy of the table per
% instance (which is currently still the case in practice).
%
% In a distributed context, there should be exactly one class manager per node.
%
% The class manager may be launched:
%
% - either after an explicit, OTP-compliant initialisation phase (supervisor,
% gen_server, init/1 callback, etc.)
%
% - or implicitly, as done before the OTP integration: then done on-demand (as
% soon as a first WOOPER instance is created, hence with no a priori explicit
% initialisation)

-module(wooper_class_manager).


% See documentation at http://wooper.esperide.org.


% We retrofitted the class manager into a gen_server for OTP compliance:
-behaviour(gen_server).


% Service API:
-export([ start/0, start_link/0, start/1, start_link/1,
		  get_table/1, display/0, stop/0 ]).


% Non-OTP API:
-export([ get_manager/0, stop_automatic/0, ping/1 ]).



% gen_server callbacks:
%
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2,
		  terminate/2, code_change/3 ]).


-type manager_pid() :: pid().

-export_type([ manager_pid/0 ]).


% For wooper_class_manager_name:
-include("wooper_class_manager.hrl").


% For wooper_table_type:
-include("wooper_defines_exports.hrl").


% For myriad_spawn*:
-include("spawn_utils.hrl").


% State kept by the manager (a table of per-class tables):
-type state() :: ?wooper_table_type:?wooper_table_type().


% Approximate average method count for a given class, including inherited ones.
%
% (ideally should be slightly above the maximum number of actual methods)
%
-define( wooper_method_count_upper_bound, 32 ).



% Approximate average class count for the program.
%
% (ideally should be slightly above the maximum number of actual classes being
% instantiated)
%
-define( wooper_class_count_upper_bound, 128 ).


% Comment/uncomment to respectively disable and enable debug mode:
%-define(debug,).


-define( log_prefix, "[WOOPER class manager] " ).


% In seconds:
-define( registration_time_out, 5 ).


% To avoid warnings (note: display/1 is apparently a BIF, renamed to
% display_msg/1):
%
-export([ display_state/1, display_table_creation/1,
		  display_msg/1, display_msg/2 ]).




% Uncomment to activate debug mode:
%-define(wooper_debug_class_manager,).

-spec display_state( ?wooper_table_type:?wooper_table_type() ) -> void().
-spec display_table_creation( basic_utils:module_name() ) -> void().

-spec display_msg( string() ) -> void().
-spec display_msg( text_utils:format_string(), text_utils:format_values() ) ->
						 void().


-ifdef(wooper_debug_class_manager).


display_state( Tables ) ->
	wooper:log_info( ?log_prefix "Storing now ~B table(s).~n",
					 [ ?wooper_table_type:size( Tables ) ] ).


display_table_creation( Module ) ->
	wooper:log_info( ?log_prefix "Creating a virtual table "
					 "for module ~s.~n", [ Module ] ).


display_msg( String ) ->
	Message = text_utils:format( ?log_prefix "~s.~n", [ String ] ),
	wooper:log_info( Message ).

display_msg( FormatString, Values ) ->
	Message = text_utils:format( ?log_prefix FormatString ++ ".~n",
								 [ Values ] ),
	wooper:log_info( Message ).


-else. % wooper_debug_class_manager


display_state( _Tables ) ->
	ok.

display_table_creation( _Module ) ->
	ok.

display_msg( _String ) ->
	ok.

display_msg( _FormatString, _Values ) ->
	ok.

-endif. % wooper_debug_class_manager





% OTP-related section: first, the user-level API.


%% Explicit, OTP-based initialisation.


% Starts a new, blank, class manager, with no listening client, and returns its
% PID.
%
-spec start() -> manager_pid().
start() ->
	start( _MaybeClientPid=undefined ).


% Starts and links a new, blank, class manager, with no listening client, and
% returns its PID.
%
-spec start_link() -> manager_pid().
start_link() ->
	start_link( _MaybeClientPid=undefined ).



% Starts a new, blank, class manager, with possibly a listening client, and
% returns its PID.
%
-spec start( maybe( pid() ) ) -> manager_pid().
start( MaybeClientPid ) ->

	% A client might be useful for testing.

	case gen_server:start( { local, ?wooper_class_manager_name },
						   ?MODULE, _Args=[], _Opts=[] ) of

		{ ok, ManagerPid } ->
			case MaybeClientPid of

				undefined ->
					ok;

				ClientPid ->
					ClientPid ! { wooper_class_manager_pid, ManagerPid }

			end,
			ManagerPid;

		% Typically {error,Reason} or ignore:
		Unexpected ->
			throw( { wooper_class_manager_creation_failed, Unexpected } )

	end.



% Starts and links a new, blank, class manager, with possibly a listening
% client, and returns its PID.
%
% (same as start/1 except for the link)
%
-spec start_link( maybe( pid() ) ) -> manager_pid().
start_link( MaybeClientPid ) ->

	% A client might be useful for testing.

	case gen_server:start_link( { local, ?wooper_class_manager_name },
								?MODULE, _Args=[], _Opts=[] ) of

		{ ok, ManagerPid } ->
			case MaybeClientPid of

				undefined ->
					ok;

				ClientPid ->
					ClientPid ! { wooper_class_manager_pid, ManagerPid }

			end,
			ManagerPid;

		% Typically {error,Reason} or ignore:
		Unexpected ->
			throw( { wooper_class_manager_creation_failed, Unexpected } )

	end.



% Returns the virtual table associated to specified classname.
-spec get_table( wooper:classname() ) ->
					   ?wooper_table_type:?wooper_table_type().
get_table( Classname ) ->
	{ ok, Table } = gen_server:call( ?wooper_class_manager_name,
									 { get_table, Classname } ),
	Table.



% Displays runtime information about the class manager.
-spec display() -> void().
display() ->
	gen_server:cast( ?wooper_class_manager_name, display ).



% Stops (the OTP-way) the class manager.
-spec stop() -> void().
stop() ->
	gen_server:cast( ?wooper_class_manager_name, stop ).






% See loop/1 for the counterpart to gen_server callbacks.


% Inits the manager (gen_server callback).
%
% (also used by the non-OTP mode of operation)
%
-spec init( any() ) -> { 'ok', state() }.
init( _Args=[] ) ->

	display_msg( "Starting (init) WOOPER class manager on node ~s (PID: ~w).",
				 [ node(), self() ] ),

	% With this creation procedure, we are supposed to start from scratch
	% through an initialisation phase (as opposed to uncoordinated creations
	% from instances that may overlap), so by design no concurrent launch is
	% expected to happen here.

	% Registering already done by gen_server:start*.

	% Infinite time-out:
	{ ok, get_initial_state() }.



% Handling OTP-based requests (gen_server callback):
handle_call( { get_table, Classname }, _From, _State=Tables ) ->

	 display_msg( "handle_call: get_table for ~s", [ Classname ] ),

	{ NewTables, TargetTable } = get_virtual_table_for( Classname, Tables ),

	{ reply, { ok, TargetTable }, _NewState=NewTables }.



% Handling OTP-based oneways (gen_server callback):
handle_cast( stop, State ) ->

	display_msg( "handle_cast: stop" ),

	stop_common(),

	{ stop, normal, State };


handle_cast( display, State=Tables ) ->

	wooper:log_info( ?log_prefix "Internal state is: ~s~n",
					 [ ?wooper_table_type:toString( Tables ) ] ),

	% Const:
	{ noreply, State }.



% Handling out-of-bound, direct messages (gen_server callback):
handle_info( Info, State ) ->

	trace_utils:warning_fmt( "The WOOPER class manager received an unexpected, "
							 "hence ignored, handle_info message: ~w.",
							 [ Info ] ),

	{ noreply, State }.


% Optional callback:
terminate( Reason, _State ) ->

	trace_utils:trace_fmt( "WOOPER class manager terminated (reason: ~w).",
						   [ Reason ] ),

	ok.


% Optional callback:
code_change( OldVsn, State, Extra ) ->

	trace_utils:trace_fmt( "Code change for the WOOPER class manager; "
						   "old version is ~w, and extra information is ~w",
						   [ OldVsn, Extra ] ),

	{ ok, State }.





% Non-OTP section: WOOPER's base/historic mode of operation.



%% Implicit, non-OTP initialisation.


% Returns the PID of the WOOPER class manager.
%
% If it is already running, finds it and returns its PID, otherwise launches it
% (in an ad hoc, non-OTP way), and returns the relevant PID as well (even if
% multiple instances thereof were concurrently spawned).
%
-spec get_manager() -> manager_pid().
get_manager() ->

	% Could have been named start_automatic/0 as well.

	case naming_utils:is_registered( ?wooper_class_manager_name,
									 _LookUpScope=local ) of

		not_registered ->

			% Not linking, at least for consistency with the case where it is
			% already launched:
			%
			% (init_automatic/0 not exported)
			%
			?myriad_spawn( fun() -> init_automatic() end ),

			% We do not return readily the PID of the just-spawned process, as
			% its launch might be concurrent with other, quasi-simultaneous
			% launches. We report only the one that is for sure the winner (the
			% others areto terminate immediately):
			%
			naming_utils:wait_for_local_registration_of(
			  ?wooper_class_manager_name, ?registration_time_out );


		ManagerPid ->
			ManagerPid

	end.



% Initializes the class manager, when launched in a non-OTP way.
-spec init_automatic() -> no_return().
init_automatic() ->

	% Two first WOOPER instances being created nearly at the same time might
	% trigger the creation of two class managers, should the second instance
	% detect that no manager is registered, whereas the first manager is created
	% but not registered yet. That would result in superfluous class
	% managers. Up to one should exist.
	%
	% Note: as the register call cannot be performed at once (not an atomic
	% operation), there must remain a tiny window for a race condition to
	% happen).
	%
	try naming_utils:register_as( self(), ?wooper_class_manager_name,
								  local_only ) of

		% Registration success, we are the one, and we use our custom main loop:
		_ ->
			{ ok, InitialState } = init( _Args=[] ),
			loop( InitialState )

	catch

		% Catches only the case where a manager was already registered; let it
		% be the only one and, stop the current one:
		%
		throw:{ local_registration_failed, _Name, already_registered,
				_OtherPid } ->

			% The client instances should use the first manager only.
			%
			% (no looping performed, hence terminating this second, extraneous
			% manager)
			ok

	end.



% Stops the class manager, when not using the OTP way.
-spec stop_automatic() -> void().
stop_automatic() ->

	case naming_utils:is_registered( ?wooper_class_manager_name, local ) of

		not_registered ->
			trace_utils:warning( "No WOOPER class manager to stop." ),
			ok;

		ManagerPid ->
			display_msg( "Stopping WOOPER class manager." ),
			ManagerPid ! stop,
			ok

	end.



% Section common to all kinds of modes of operation (OTP or not).


% Returns the initial state of this manager, i.e. an (initially empty) table of
% per-class tables.
%
-spec get_initial_state() -> state().
get_initial_state() ->
	% Empty class table:
	?wooper_table_type:new( ?wooper_class_count_upper_bound ).



% Stops the class manager.
%
% Used by all modes of operation (OTP or not).
%
-spec stop_common() -> void().
stop_common() ->
	display_msg( "Stopping WOOPER class manager." ).



% Manager main loop, serves virtual tables on request (mostly on instances
% creation).
%
-spec loop( ?wooper_table_type:?wooper_table_type() ) -> no_return() | 'ok' .
loop( Tables ) ->

	display_state( Tables ),

	receive

		{ get_table, Classname, Pid } ->
			{ NewTables, TargetTable } =
				get_virtual_table_for( Classname, Tables ),

			Pid ! { wooper_virtual_table, TargetTable },
			loop( NewTables );

		display ->
			wooper:log_info( ?log_prefix "Internal state is: ~s~n",
							 [ ?wooper_table_type:toString( Tables ) ] ),
			loop( Tables );

		stop ->
			unregister( ?wooper_class_manager_name ),
			stop_common();

		Unexpected ->
			trace_utils:error_fmt( "The WOOPER class manager received an "
								   "unexpected, thus ignored, message: ~w",
								   [ Unexpected ] ),
			loop( Tables )

	end.



% Look-up specified table.
%
% If found, returns it immediately, otherwise constructs it, stores the result
% and returns it as well.
%
% Virtual tables are stored in a ?wooper_table_type.
%
% Returns a pair formed of the new set of virtual tables and of the requested
% table.
%
-spec get_virtual_table_for( basic_utils:module_name(),
							 ?wooper_table_type:?wooper_table_type() ) ->
								   { ?wooper_table_type:?wooper_table_type(),
									 ?wooper_table_type:?wooper_table_type() }.
get_virtual_table_for( Module, Tables ) ->

	case ?wooper_table_type:lookupEntry( Module, Tables ) of

		{ value, Table } ->

			% Cache hit, no change in internal data:
			{ Tables, Table };

		key_not_found ->

			% Time to create this virtual table and to store it:
			display_table_creation( Module ),
			ModuleTable = create_method_table_for( Module ),

			%trace_utils:debug_fmt( "Virtual table for ~s: ~s",
			%         [ Module, ?wooper_table_type:toString( ModuleTable ) ] ),

			% Each class had its virtual table optimised:
			%OptimisedModuleTable =
			%    ?wooper_table_type:optimise( ModuleTable ),

			% Here the table could be patched with destruct/1, if defined.
			ClassTable =
				?wooper_table_type:addEntry( Module, ModuleTable, Tables ),

			% And the table of virtual tables was itself optimised each time a
			% new class was introduced:
			%
			{ %?wooper_table_type:optimise( ClassTable ),
			  ClassTable,
			  % OptimisedModuleTable }
			  ModuleTable }

	end.




% Creates recursively (indirectly thanks to 'update_method_table_with') the
% virtual table corresponding to specified module.
%
-spec create_method_table_for( basic_utils:module_name() ) ->
						?wooper_table_type:?wooper_table_type().
create_method_table_for( TargetModule ) ->

	lists:foldl(

		fun( Module, Hashtable ) ->
			update_method_table_with( Module, Hashtable )
		end,

		create_local_method_table_for( TargetModule ),

		TargetModule:get_superclasses() ).



% Updates specified virtual table with the method of specified module
% (i.e. precomputes the virtual table for the related class).
%
% In case of key collision, the values specified in ?Wooper_Table_Type have
% priority over the ones relative to Module. Hence methods redefined in child
% classes are selected, rather than the ones of the mother class.
%
-spec update_method_table_with( basic_utils:module_name(),
		  ?wooper_table_type:?wooper_table_type() ) ->
						  ?wooper_table_type:?wooper_table_type().
update_method_table_with( Module, Hashtable ) ->
	?wooper_table_type:merge( Hashtable, create_method_table_for( Module ) ).



% Tells whether the function Name/Arity should be registered into the method
% virtual table.
%
select_function( _,0 )                                                -> false ;

select_function( new,_ )                                              -> false ;
select_function( new_link,_ )                                         -> false ;
select_function( new_passive,_ )                                      -> false ;

select_function( synchronous_new,_ )                                  -> false ;
select_function( synchronous_new_link,_ )                             -> false ;
select_function( synchronous_timed_new,_ )                            -> false ;
select_function( synchronous_timed_new_link,_ )                       -> false ;

select_function( remote_synchronisable_new,_ )                        -> false ;
select_function( remote_synchronisable_new_link,_ )                   -> false ;

select_function( remote_new,_ )                                       -> false ;
select_function( remote_new_link,_ )                                  -> false ;
select_function( remote_synchronous_new,_ )                           -> false ;
select_function( remote_synchronous_new_link,_ )                      -> false ;
select_function( remote_synchronous_timed_new,_ )                     -> false ;
select_function( remote_synchronous_timed_new_link,_ )                -> false ;

select_function( construct,_ )                                        -> false ;
select_function( destruct,1 )                                         -> false ;
select_function( delete_any_instance_referenced_in,_ )                -> false ;
select_function( delete_synchronously_any_instance_referenced_in,_ )  -> false ;
select_function( delete_synchronously_instances,_ )                   -> false ;

select_function( wooper_check_undefined,_ )                           -> false ;
select_function( wooper_construct_and_run,_ )                         -> false ;
select_function( wooper_construct_and_run_synchronous,_ )             -> false ;
select_function( wooper_debug_listen,_ )                              -> false ;
select_function( wooper_destruct,_ )                                  -> false ;
select_function( wooper_display_instance,_ )                          -> false ;
select_function( wooper_display_loop_state,_ )                        -> false ;
select_function( wooper_display_state,_ )                             -> false ;
select_function( wooper_display_virtual_table,_ )                     -> false ;
select_function( wooper_get_all_attributes,_ )                        -> false ;
select_function( wooper_get_state_description,_ )                     -> false ;
select_function( wooper_get_virtual_table_description,_ )             -> false ;
select_function( wooper_pop_from_attribute,_ )                        -> false ;
select_function( wooper_effective_method_execution,4 )                -> false ;
select_function( wooper_execute_method,3 )                            -> false ;
select_function( wooper_execute_method_as,4 )                         -> false ;

% Might be useful, currently still enabled:
%select_function( wooper_get_instance_description,1 )                 -> false ;

select_function( wooper_handle_local_oneway_execution,3 )             -> false ;
select_function( wooper_handle_local_request_execution,3 )            -> false ;
select_function( wooper_handle_remote_oneway_execution,3 )            -> false ;
select_function( wooper_handle_remote_request_execution,4 )           -> false ;
select_function( wooper_main_loop,1 )                                 -> false ;

select_function( addKeyValueToAttribute, 4 )                          -> false ;
select_function( addToAttribute, 3 )                                  -> false ;
select_function( appendToAttribute, 3 )                               -> false ;
select_function( concatToAttribute, 3  )                              -> false ;
select_function( decrementAttribute, 2 )                              -> false ;
select_function( deleteFromAttribute, 3 )                             -> false ;
select_function( getAttribute, 2 )                                    -> false ;
select_function( getAttributes, 2 )                                   -> false ;
select_function( hasAttribute, 2 )                                    -> false ;
select_function( incrementAttribute, 2 )                              -> false ;
select_function( popFromAttribute, 2 )                                -> false ;
select_function( removeAttribute, 2 )                                 -> false ;
select_function( setAttribute, 3 )                                    -> false ;
select_function( setAttributes, 2 )                                   -> false ;
select_function( subtractFromAttribute, 3 )                           -> false ;
select_function( toggleAttribute, 2 )                                 -> false ;

select_function( post_deserialise_hook, 1 )                           -> false ;
select_function( post_serialise_hook, 3 )                             -> false ;
select_function( pre_deserialise_hook, 2 )                            -> false ;
select_function( pre_serialise_hook, 1 )                              -> false ;

select_function( executeOneway,_ )                                    -> false ;
select_function( executeConstOneway,_ )                               -> false ;
select_function( executeOnewayAs,_ )                                  -> false ;
select_function( executeConstOnewayAs,_ )                             -> false ;

select_function( executeRequest,_ )                                   -> false ;
select_function( executeConstRequest,_ )                              -> false ;
select_function( executeRequestAs,_ )                                 -> false ;
select_function( executeConstRequestAs,_ )                            -> false ;

select_function( module_info,1)                                       -> false ;
% Includes 'wooper_get_instance_description/1', which could be useful to debug:
select_function( _, _ )                                               -> true.




% Returns an hashtable appropriate for method look-up, for the specified module.
-spec create_local_method_table_for( basic_utils:module_name() ) ->
							   ?wooper_table_type:?wooper_table_type().
create_local_method_table_for( Module ) ->

	% Typically if Module is misspelled:
	Exports = try
				  Module:module_info( exports )
			  catch

				  error:undef ->
					  throw( { class_not_found, Module } )

			  end,


	% Filter-out functions that should not be callable via RMI:
	lists:foldl(

		% Filter-out functions that should not be callable via RMI:
		fun( { Name, Arity }, Hashtable ) ->
			case select_function(  Name, Arity ) of

				true ->
					?wooper_table_type:addEntry( { Name, Arity }, Module,
												 Hashtable );

				false ->
					Hashtable

			end
		end,

		?wooper_table_type:new( ?wooper_method_count_upper_bound ),

		Exports ).



% ping specified WOOPER instance, designated by its PID or registered name
% (locally, otherwise, if not found, globally).
%
% Returns pong if it could be successfully ping'ed, otherwise returns pang.
%
-spec ping( naming_utils:registration_name() | wooper:instance_pid() ) ->
				  'pong' | 'pang'.
ping( Target ) when is_pid( Target ) ->

	Target ! { ping, self() },
	receive

		{ pong, Target } ->
			pong

		after 500 ->
			pang

	end;

ping( Target ) when is_atom( Target ) ->

	case naming_utils:is_registered( Target ) of

		not_registered ->
			pang;

		Pid ->
			ping( Pid )

	end.
