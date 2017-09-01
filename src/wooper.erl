% Copyright (C) 2012-2017 Olivier Boudeville
%
% This file is part of the WOOPER library.
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
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)


% Module containing some facilities for WOOPER users, better placed in a
% standalone module than duplicated in each class.
%
-module(wooper).



% First, all the various exports:


% Very generic:
%
-export([ get_class_name/1, get_attribute_pairs/1, state_to_string/1 ]).


% Communication helpers:
%
-export([ execute_request/3, execute_request/4, send_requests/3,

		  send_requests_and_wait_acks/4, send_requests_and_wait_acks/5,
		  wait_for_request_answers/2, wait_for_request_answers/3,
		  wait_for_request_acknowledgements/2,

		  obtain_results_for_requests/3,

		  send_request_series/2, obtain_results_for_request_series/2,

		  send_and_listen/3, receive_result/0 ]).



% Creation helpers:
%
-export([ create_hosting_process/2,
		  construct_and_run/2, construct_and_run_synchronous/3 ]).



% Destruction helpers:
%
-export([ delete_any_instance_referenced_in/2,
		  delete_synchronously_any_instance_referenced_in/2,
		  safe_delete_synchronously_any_instance_referenced_in/2,
		  delete_synchronously_instance/1, delete_synchronously_instances/1,
		  safe_delete_synchronously_instances/1
		]).


% Infrequently-called functions for state management:
%
-export([ get_all_attributes/1 ]).



% For log and error reporting:
%
-export([ log_info/1, log_info/2,
		  log_warning/1, log_warning/2,
		  log_error/1, log_error/2, log_error/3,
		  on_failed_request/6, on_failed_oneway/5
		]).


-ifdef(wooper_debug).

% State-related helpers (only available in debug mode):
-export([
		 virtual_table_to_string/1,
		 instance_to_string/1,
		 display_state/1,
		 display_virtual_table/1,
		 display_instance/1
		]).

-endif. % wooper_debug



% Basics:
-export([ get_class_manager/0, default_exit_handler/3, default_down_handler/5,
		  default_node_up_handler/3, default_node_down_handler/3 ]).


% Defined here because embodied instances rely on the main loop which needs that
% information to destruct the corresponding instance:
%
-define( wooper_superclasses, [] ).


% For the name of the registered process that keeps the per-class method
% hashtables:
%
-include("wooper_class_manager.hrl").


% For synchronous_time_out, state_holder and al:
-include("wooper_defines_exports.hrl").


% Actual definitions the shorthands in wooper_types_exports.hrl refer to:

% An atom prefixed with 'class_':
%
-type class_name() :: atom().

-type classname() :: class_name().


% A method name (ex: 'setColor'):
%
-type method_name() :: meta_utils:function_name().

-type request_name() :: method_name().
-type oneway_name()  :: method_name().


% A method argument can be any type:
-type method_argument() :: any().

% Standalone (non-list) arguments may also be specified:
-type method_arguments() :: method_argument() | [ method_argument() ].


% Method qualifiers not implemented yet:
-type qualifier() :: any().


% Special case of construction parameters:
-type construction_parameters() :: [ method_argument() ].


-type requests_outcome() :: 'success' | { 'failure', [ pid() ] }.

% To be specified more closely maybe:
-type method_internal_result() :: any().

-type request_result( T ) :: T.

-type request_return( T ) :: { state(), request_result( T ) }.
-type oneway_return() :: state().


-type attribute_name() :: atom().
-type attribute_value() :: any().

-type attribute_entry() :: { attribute_name(), attribute_value() }.


% PID of a WOOPER instance.
-type instance_pid() :: pid().


% Atom used to denote an acknowlegment (i.e. a conventional symbol to ensure
% that a request was synchronously executed):
%
-type ack_atom() :: atom().

-type request_result() :: wooper:request_result().


% Otherwise getClassName/1, get_superclasses/0, etc. are unused:
-include("wooper_classes_exports.hrl").

% Otherwise wooper_execute_method_with/4 is unused:
-include("wooper_execute_internal_exports.hrl").


%-opaque state() :: #state_holder{}.
-type state() :: #state_holder{}.




% We prefer having it prefixed by wooper:
-export_type([ class_name/0, classname/0,
			   method_name/0, request_name/0, oneway_name/0,
			   method_argument/0, method_arguments/0, qualifier/0,
			   construction_parameters/0,
			   requests_outcome/0, method_internal_result/0,
			   request_result/1, request_result/0,
			   request_return/1, oneway_return/0,
			   attribute_name/0, attribute_value/0, attribute_entry/0,
			   instance_pid/0, state/0 ]).



% For getAttr and al:
-include("wooper_state_exports.hrl").


% Otherwise executeRequest/3 and all reported as unused:
-include("wooper_execute_exports.hrl").

%-include("wooper_execute_internal_exports.hrl").


% Now, function definitions:


% For getAttribute/2 and al:
-include("wooper_state_functions.hrl").

% For executeRequest/2:
-include("wooper_execute_functions.hrl").

% For wooper_execute_method/3:
-include("wooper_execute_internal_functions.hrl").

% For get_superclasses/1:
-include("wooper_classes_functions.hrl").





% Section for communication helpers.
%
% Generally no wooper result expected to be already in the message queue or to
% be received during these operations.


% A WOOPER request execution (hence a synchronous call) not yielding a result in
% specified duration (in milliseconds) will lead to a notification to be
% displayed on the console (yet the result will still be waited for
% indefinitively)
%
-ifdef(wooper_debug).

-define( notify_long_wait_after, 2500 ).

-else. % wooper_debug

-define( notify_long_wait_after, 60000 ).

-endif. % wooper_debug



% First, just a simple shorthand: one caller, one request, one result, waited
% indefinitively.


% Sends specified request to specified instance, waits indefinitively for its
% returned value (supposing none is already waiting among the received
% messages), and returns it.
%
% (helper)
%
-spec execute_request( request_name(), method_arguments(), instance_pid() ) ->
							 request_result().
execute_request( RequestName, RequestArgs, TargetInstancePID ) ->

	TargetInstancePID ! { RequestName, RequestArgs, self() },

	execute_request_waiter( TargetInstancePID, RequestName, RequestArgs ).



% (helper)
%
execute_request_waiter( TargetInstancePID, RequestName, RequestArgs ) ->

	receive

		{ wooper_result, Res } ->
			Res

	after ?notify_long_wait_after ->

		trace_utils:warning_fmt( "Still awaiting an answer "
								 "from WOOPER instance ~p, after having called "
								 "request '~s' on it with following "
								 "parameters:~n~p",
								 [ TargetInstancePID, RequestName,
								   RequestArgs ] ),

		execute_request_waiter( TargetInstancePID, RequestName, RequestArgs )

	end.



% Sends specified request to specified instance and waits indefinitively for its
% specified, expected returned value (supposing none is already waiting among
% the received messages) that is usually an atom.
%
% (helper)
%
-spec execute_request( request_name(), method_arguments(), instance_pid(),
					   any() ) -> basic_utils:void().
execute_request( RequestName, RequestArgs, TargetInstancePID,
				 ExpectedResult ) ->

	TargetInstancePID ! { RequestName, RequestArgs, self() },

	execute_request_waiter( ExpectedResult, TargetInstancePID, RequestName,
							RequestArgs ).



% (helper)
%
execute_request_waiter( ExpectedResult, TargetInstancePID, RequestName,
						RequestArgs ) ->

	receive

		{ wooper_result, ExpectedResult } ->
			ok

	after ?notify_long_wait_after ->

		trace_utils:warning_fmt( "Still awaiting the expected answer '~p' "
								 "from WOOPER instance ~p, after having called "
								 "request '~s' on it with following "
								 "parameters:~n~p",
								 [ ExpectedResult, TargetInstancePID,
								   RequestName, RequestArgs ] ),

		execute_request_waiter( ExpectedResult, TargetInstancePID,
								RequestName, RequestArgs )

	end.




% Second: one caller, one request, multiple callees, with a time-out or not.


% Sends specified request (based on its names and arguments) to each of the
% specified target instances.
%
% (helper)
%
-spec send_requests( request_name(), method_arguments(),
					 [ instance_pid() ] ) -> basic_utils:void().
send_requests( RequestName, RequestArgs, TargetInstancePIDs ) ->

	Request = { RequestName, RequestArgs, self() },

	[ InstancePid ! Request || InstancePid <- TargetInstancePIDs ].




% Sends specified request (based on its name and arguments) to each of the
% specified target instances, and waits (indefinitively) for their
% acknowledgement, which shall be their only result (i.e. these methods should
% be requests only for synchronisation).
%
% No time-out: answers will be waited indefinitely.
%
-spec send_requests_and_wait_acks( request_name(), method_arguments(),
				   [ instance_pid() ], ack_atom() ) -> basic_utils:void().
send_requests_and_wait_acks( RequestName, RequestArgs, TargetInstancePIDs,
							 AckAtom ) ->

	send_requests( RequestName, RequestArgs, TargetInstancePIDs ),

	wait_indefinitively_for_request_answers( TargetInstancePIDs, AckAtom ).



% Sends specified request (based on its names and arguments) to each of the
% specified target instances, and waits for their acknowledgement; returns
% whether it succeeded or if some instances triggered a time-out.
%
-spec send_requests_and_wait_acks( request_name(), method_arguments(),
		  [ instance_pid() ], time_utils:time_out(), ack_atom() ) ->
										 requests_outcome().
send_requests_and_wait_acks( RequestName, RequestArgs, TargetInstancePIDs,
							 Timeout, AckAtom ) ->

	send_requests( RequestName, RequestArgs, TargetInstancePIDs ),

	wait_for_request_answers( TargetInstancePIDs, Timeout, AckAtom ).



% Waits for an acknowledgement answer, based on specified atom and on the PID of
% each of the specified requested instances, indefinitively (no time-out).
%
% Allows to trigger requests (supposing returning all the same, specified atom)
% in parallel yet being able to wait synchronously for them, and know which, if
% any, did not answer.
%
% (helper)
%
-spec wait_for_request_answers( [ instance_pid() ], ack_atom() ) ->
									  requests_outcome().
wait_for_request_answers( RequestedPidList, AckAtom ) ->
	wait_indefinitively_for_request_answers( RequestedPidList, AckAtom ).



% Waits for an acknowledgement answer, based on specified atom, from the
% specified requested instances, unless the specified time-out is exceeded
% (specified as integer milliseconds or the 'infinity' atom).
%
% Allows to trigger requests in parallel yet being able to wait synchronously
% for them.
%
% (helper)
%
-spec wait_for_request_answers( [ instance_pid() ], time_utils:time_out(),
								ack_atom() ) -> requests_outcome().
wait_for_request_answers( RequestedPidList, _Timeout=infinity, AckAtom ) ->
	wait_indefinitively_for_request_answers( RequestedPidList, AckAtom );

wait_for_request_answers( RequestedPidList, Timeout, AckAtom ) ->

	InitialTimestamp = time_utils:get_timestamp(),

	wait_for_request_answers( RequestedPidList, InitialTimestamp, Timeout,
							  AckAtom ).


% Waits until end of time if necessary.
%
% (helper)
%
wait_indefinitively_for_request_answers( _RequestedPidList=[], _AckAtom ) ->
	success;

wait_indefinitively_for_request_answers( RequestedPidList, AckAtom ) ->

	receive

		{ wooper_result, { AckAtom, SenderPid } } ->

			NewPidList = list_utils:delete_existing( SenderPid,
													 RequestedPidList ),

			wait_indefinitively_for_request_answers( NewPidList, AckAtom )

	end.



% Wait until specified time-out is reached.
%
% (helper)
%
wait_for_request_answers( RequestedPidList, InitialTimestamp, Timeout,
						  AckAtom ) ->
	wait_for_request_answers( RequestedPidList, InitialTimestamp, Timeout,
							  _DefaultPollDuration=1000, AckAtom ).


wait_for_request_answers( _RequestedPidList=[], _InitialTimestamp, _Timeout,
						  _PollDuration, _AckAtom ) ->
	success;

wait_for_request_answers( RequestedPidList, InitialTimestamp, Timeout,
						  PollDuration, AckAtom ) ->

	receive

		{ wooper_result, { AckAtom, SenderPid } } ->

			NewPidList = list_utils:delete_existing( SenderPid,
													 RequestedPidList ),

			wait_for_request_answers( NewPidList, InitialTimestamp, Timeout,
									  PollDuration, AckAtom )

	after PollDuration ->

			NewDuration = time_utils:get_duration_since( InitialTimestamp ),

			case NewDuration > Timeout of

				true ->
					{ failure, RequestedPidList };

				false ->
					% Still waiting then:
					wait_for_request_answers( RequestedPidList,
						   InitialTimestamp, Timeout, PollDuration, AckAtom )

			end

	end.



% Waits (indefinitively) that the specified number of requests returned as
% result the specified acknowledgement atom.
%
-spec wait_for_request_acknowledgements( basic_utils:count(), ack_atom() ) ->
											   basic_utils:void().
wait_for_request_acknowledgements( _Count=0, _AckAtom ) ->
	ok;

wait_for_request_acknowledgements( Count, AckAtom ) ->

	receive

		{ wooper_result, AckAtom } ->
			wait_for_request_acknowledgements( Count-1, AckAtom )

	end.




% Sends the specified request to all specified instances for execution, in
% parallel, and returns the corresponding results, in indiscriminated order.
%
% Note: no specified order is enforced in the result list; hence this helper is
% meant to be used when we can collect each result regardless of its specific
% sender.
%
% No time-out enforced.
%
% (exported helper)
%
-spec obtain_results_for_requests( request_name(), method_arguments(),
								   [ instance_pid() ] ) -> [ request_result() ].
obtain_results_for_requests( RequestName, RequestArgs, TargetInstancePIDs ) ->

	send_requests( RequestName, RequestArgs, TargetInstancePIDs ),

	% Of course we expect that no previously received WOOPER message is
	% remaining in the queue.

	collect_wooper_messages( _Count=length( TargetInstancePIDs ), _Acc=[] ).



% Collects specified number of WOOPER messages, and returns a list of the
% corresponding results.
%
% (helper)
%
collect_wooper_messages( _Count=0, Acc ) ->
	Acc;

collect_wooper_messages( Count, Acc ) ->

	receive

		{ wooper_result, Res } ->
			collect_wooper_messages( Count-1, [ Res | Acc ] )

	end.



% Second: one caller,multiple requests, one callee (expected to answer them in
% order - which is the general case in WOOPER where there is no selective
% receive).
%
% No other results expected to be received once the request series has been
% triggered. No time-out enforced.



% Sends specified series of requests (based on its names and arguments) to the
% specified target instance.
%
% (helper)
%
-spec send_request_series( [ { request_name(), method_arguments() } ],
						   instance_pid() ) -> basic_utils:void().
send_request_series( _Requests=[], _TargetInstancePID ) ->
	ok;

send_request_series( _Requests=[ { RequestName, RequestArgs } | T ],
					 TargetInstancePID ) ->

	ActualRequest = { RequestName, RequestArgs, self() },

	TargetInstancePID ! ActualRequest,

	send_request_series( T, TargetInstancePID ).




% Sends the specified request to all specified instances for execution, in
% parallel, and returns the corresponding results, in indiscriminated order.
%
% Note: no specified order is enforced in the result list; hence this helper is
% meant to be used when we can collect each result regardless of its specific
% sender.
%
% (exported helper)
%
-spec obtain_results_for_request_series(
		[ { request_name(), method_arguments() } ], instance_pid() ) ->
											   [ request_result() ].
obtain_results_for_request_series( Requests, TargetInstancePID ) ->

	send_request_series( Requests, TargetInstancePID ),

	% Requests sent in-order, so will answers be received:
	wait_request_series( _WaitCount=length( Requests ), _Acc=[] ).



% (helper)
wait_request_series( _WaitCount=0, Acc ) ->
	lists:reverse( Acc );

wait_request_series( WaitCount, Acc ) ->

	receive

		{ wooper_result, R } ->
			wait_request_series( WaitCount-1, [ R | Acc ] )

	end.



% Section for creation helpers.



% Creates (asynchronously) a blank process, waiting to embody a WOOPER instance
% once it will have received its class and construction parameters, and link it
% to the caller and to the specified process.
%
-spec create_hosting_process( net_utils:node_name(), pid() ) ->
									instance_pid().
create_hosting_process( Node, ToLinkWithPid ) ->

	WaitFun = fun() ->

		% Closure; not atomic:
		erlang:link( ToLinkWithPid ),

		receive

			  { embody, [ Class, ConstructionParameters ] } ->

				%io:format( "Process ~w becoming asynchronously an instance "
				%		   "of class '~s', constructed from following "
				%		   "parameters:~n~p.~n",
				%		   [ self(), Class, ConstructionParameters ] ),

				% Never returns:
				construct_and_run( Class, ConstructionParameters );


			  % We might need to notify another process than the caller:
			  { embody, [ Class, ConstructionParameters ], ToNotifyPid } ->

				%io:format( "Process ~w becoming synchronously an instance "
				%		   "of class '~s', constructed from following "
				%		   "parameters:~n~p.~n",
				%		   [ self(), Class, ConstructionParameters ] ),

				% Never returns:
				construct_and_run_synchronous( Class, ConstructionParameters,
											   ToNotifyPid )

		end

	end,

	spawn_link( Node, WaitFun ).



% Deactivated, as that check would happen after a corresponding error cause
% (type class_X:new/N reported as 'undef').

-ifdef(wooper_deactivated).

-ifdef(wooper_debug).


% Used only in debug mode:
-spec check_classname_and_arity( class_name(), construction_parameters() ) ->
									   basic_utils:void().
check_classname_and_arity( Classname, ConstructionParameters ) ->

	% Normally useless, as called by the module itself:
	case code_utils:is_beam_in_path( Classname ) of

		not_found ->
			throw( { beam_not_found_for, Classname } );

		_ ->
			ok

	end,

	% Includes the state:
	ArgCount = length( ConstructionParameters ) + 1,


	case meta_utils:is_function_exported( _Module=Classname,
						_Function=construct, _Arity=ArgCount ) of

		true ->
			ok;

		false ->

			ExportedFunctions = meta_utils:list_exported_functions( Classname ),

			case _ConstructArities=[ Arity
					|| { construct, Arity } <- ExportedFunctions ] of


				[] ->
					trace_utils:error_fmt( "Error, no 'construct' exported "
										   "in '~s' (regardless of arity).",
										   [ Classname ] ),
					throw( { no_exported_construct, Classname } );

				[ FoundArity ] when FoundArity > ArgCount ->

					ExtraCount  = FoundArity - ArgCount,

					trace_utils:error( "Error, no ~s:construct/~B found, "
						"whereas construct/~B is exported; ~B extra "
						"construction parameter(s) specified.",
						[ Classname, ArgCount, FoundArity, ExtraCount ] ),

					throw( { extra_construction_parameters_specified,
							 Classname, ExtraCount } );


				% Here ArgCount > FoundArity:
				[ FoundArity ] ->

					LackingCount  = ArgCount - FoundArity,

					trace_utils:error( "Error, no ~s:construct/~B found, "
						"whereas construct/~B is exported; ~B lacking "
						"construction parameter(s) specified.",
						[ Classname, ArgCount, FoundArity, LackingCount ] ),

					throw( { lacking_construction_parameters_specified,
							 Classname, LackingCount } );


				ConstructArities ->

					trace_utils:error( "Error, no ~s:construct/~B found, "
						"whereas exported for following arities: ~w.",
						[ Classname, ArgCount, ConstructArities ] ),

					throw( { invalid_construction_parameters_specified,
							 Classname, ArgCount, ConstructArities } )

			end

	end.


-endif. % wooper_debug

-endif. % wooper_deactivated



% Constructs the initial state of an instance of specified class, using
% specified construction parameters, and enters its main loop.
%
% (helper)
%
-spec construct_and_run( class_name(), construction_parameters() ) ->
							   no_return().


-ifdef(wooper_debug).

construct_and_run( Classname, ConstructionParameters ) ->

	%trace_utils:debug_fmt( "wooper:construct_and_run for class ~p "
	%                       "and parameters ~p.~n",
	%						[ Classname, ConstructionParameters ] ),

	%check_classname_and_arity( Classname, ConstructionParameters ),

	BlankState = get_blank_state( Classname ),

	try apply( Classname, construct,
			   [ BlankState | ConstructionParameters ] ) of

		ConstructState when is_record( ConstructState, state_holder ) ->

			% Enforces a closer-to-ideal load factor of the hashtable if needed,
			% as by convention no attribute should be introduced outside of the
			% constructor:
			%
			TunedTable = ?wooper_hashtable_type:optimise(
							ConstructState#state_holder.attribute_table ),

			ReadyState = ConstructState#state_holder{
						   attribute_table=TunedTable },

			% Otherwise, in wooper_destruct/1 and all, ?MODULE will be 'wooper'
			% instead of the right class:
			%
			apply( Classname, wooper_main_loop, [ ReadyState ] );


		Other ->

			log_error( "~nWOOPER error for PID ~w of class ~s: "
				"constructor did not return a state, but returned '~p' instead."
				" Construction parameters were:~n~p.",
				[ self(), Classname, Other, ConstructionParameters ] ),

			throw( { invalid_constructor, Classname } )

	catch

		Reason:ErrorTerm ->
			trigger_error( Reason, ErrorTerm, Classname,
						   ConstructionParameters )

	end.



-else. % wooper_debug


construct_and_run( Classname, ConstructionParameters ) ->

	BlankState = get_blank_state( Classname ),

	ConstructState = try

		apply( Classname, construct, [ BlankState | ConstructionParameters ] )

	catch

		Reason:ErrorTerm ->
			trigger_error( Reason, ErrorTerm, Classname,
						   ConstructionParameters )

	end,

	% Enforces a closer-to-ideal load factor of the hashtable if needed, as by
	% convention no attribute should be introduced outside of the constructor:
	%
	TunedTable = ?wooper_hashtable_type:optimise(
							ConstructState#state_holder.attribute_table ),


	ReadyState = ConstructState#state_holder{ attribute_table=TunedTable },

	% Otherwise, in wooper_destruct/1 and all, ?MODULE will be 'wooper' instead
	% of the right class:
	%
	apply( Classname, wooper_main_loop, [ ReadyState ] ).


-endif. % wooper_debug






% Constructs synchronously the initial state of an instance of specified class,
% using specified construction parameters, and enters its main loop.
%
% (helper)
%
-spec construct_and_run_synchronous( class_name(), construction_parameters(),
									 pid() ) -> no_return().


-ifdef(wooper_debug).

construct_and_run_synchronous( Classname, ConstructionParameters,
							   SpawnerPid ) ->

	%check_classname_and_arity( Classname, ConstructionParameters ),

	BlankState = get_blank_state( Classname ),

	try apply( Classname, construct,
			   [ BlankState | ConstructionParameters ] ) of

		ConstructState when is_record( ConstructState, state_holder ) ->

			% Notify early:
			SpawnerPid ! { spawn_successful, self() },

			% Enforces a closer-to-ideal load factor of the hashtable if needed,
			% as by convention no attribute should be introduced outside of the
			% constructor:
			%
			TunedTable = ?wooper_hashtable_type:optimise(
							ConstructState#state_holder.attribute_table ),

			ReadyState = ConstructState#state_holder{
						   attribute_table=TunedTable },

			% Otherwise, in wooper_destruct/1 and all, ?MODULE will be 'wooper'
			% instead of the right class:
			%
			% (never returns)
			%
			apply( Classname, wooper_main_loop, [ ReadyState ] );



		Other ->

			log_error( "~nWOOPER error for PID ~w of class ~s: "
					   "constructor did not return a state, but returned '~p' "
					   "instead. Construction parameters were:~n~p.~n",
					   [ self(), Classname, Other, ConstructionParameters ] ),

			throw( { invalid_constructor, Classname } )

	catch

		Reason:ErrorTerm ->
			trigger_error( Reason, ErrorTerm, Classname,
						   ConstructionParameters )

	end.



-else. % not in wooper_debug:


construct_and_run_synchronous( Classname, ConstructionParameters,
							   SpawnerPid ) ->

	BlankState = get_blank_state( Classname ),

	% Faulty returns (non-state) not detected here:
	ConstructState = try

			 apply( Classname, construct,
					[ BlankState | ConstructionParameters ] )

	catch

		Reason:ErrorTerm ->
			trigger_error( Reason, ErrorTerm, Classname,
						   ConstructionParameters )

	end,

	% Notify early:
	SpawnerPid ! { spawn_successful, self() },

	% Enforces a closer-to-ideal load factor of the hashtable if needed, as by
	% convention no attribute should be introduced outside of the constructor:
	%
	TunedTable = ?wooper_hashtable_type:optimise(
							ConstructState#state_holder.attribute_table ),

	ReadyState = ConstructState#state_holder{ attribute_table=TunedTable },

	% Otherwise, in wooper_destruct/1 and all, ?MODULE will be 'wooper' instead
	% of the right class:
	%
	% (never returns)
	%
	apply( Classname, wooper_main_loop, [ ReadyState ] ).


-endif. % not wooper_debug.



% Helpers.


% Returns the state of a blank WOOPER instance of specified class.
%
% (helper)
%
-spec get_blank_state( class_name() ) -> wooper:state().
get_blank_state( Classname ) ->

	#state_holder{

		virtual_table   = retrieve_virtual_table( Classname ),

		attribute_table = ?wooper_hashtable_type:new(
									?wooper_attribute_count_upper_bound ),

		actual_class    = Classname,

		request_sender  = undefined

	}.



% Returns the WOOPER Class Manager.
%
% If it is already running, finds it and returns its atom, otherwise launches
% it, and returns that same atom as well.
%
-spec get_class_manager() -> naming_utils:registration_name().
get_class_manager() ->

	case lists:member( ?wooper_class_manager_name, registered() ) of

		true ->
			?wooper_class_manager_name;

		_ ->
			spawn( ?wooper_class_manager_name, start, [ self() ] ),
			% Only dealing with registered managers (instead of using directly
			% their PID) allows to be sure only one instance (singleton) is
			% being used, to avoid the case of two managers being launched at
			% the same time (the second will then terminate immediately).
			%
			receive

				class_manager_registered ->
					?wooper_class_manager_name

			% 10-second time-out:
			after 10000 ->

				log_error( "wooper:get_class_manager: "
					"unable to find WOOPER class manager after 10 seconds.~n"
					"Please check that WOOPER has been compiled beforehand." ),

				undefined

			end

	end.




% Section for default handlers.



% WOOPER default EXIT handler.
%
% Returns an updated state.
%
% Can be overridden by defining or inheriting the onWOOPERExitReceived/3 oneway.
%
-spec default_exit_handler( wooper:state(), pid(), any() ) -> wooper:state().
default_exit_handler( State, Pid, ExitType ) ->

	wooper:log_warning( "WOOPER default EXIT handler of the ~w "
						"instance ~w ignored the following EXIT message "
						"from ~w:~n'~p'.~n",
						[ State#state_holder.actual_class, self(), Pid,
						  ExitType ] ),

	State.



% WOOPER default DOWN handler.
%
% Returns an updated state.
%
% Can be overridden by defining or inheriting the onWOOPERDownNotified/5 oneway.
%
-spec default_down_handler( wooper:state(), monitor_utils:monitor_reference(),
							monitor_utils:monitored_element_type(),
							monitor_utils:monitored_element(),
							basic_utils:exit_reason() ) ->  wooper:state().
default_down_handler( State, _MonitorReference, _MonitoredType,
					  _MonitoredElement, _ExitReason=normal ) ->
	% Normal exits not notified:
	State;

default_down_handler( State, MonitorReference, MonitoredType, MonitoredElement,
					  ExitReason ) ->

	wooper:log_warning( "WOOPER default DOWN handler of the ~w "
						"instance ~w ignored the following down notification "
						"'~s' for monitored element ~p of type '~p' "
						"(monitor reference: ~w).~n",
						[ State#state_holder.actual_class, self(), ExitReason,
						  MonitoredElement, MonitoredType, MonitorReference ] ),

	State.




% WOOPER default node up handler.
%
% Returns an updated state.
%
% Can be overridden by defining or inheriting the onWOOPERNodeConnection/3
% oneway.
%
-spec default_node_up_handler( wooper:state(), net_utils:atom_node_name(),
					   monitor_utils:monitor_node_info() ) -> wooper:state().
default_node_up_handler( State, Node, MonitorNodeInfo ) ->

	wooper:log_warning( "WOOPER default node up handler of the ~w "
						"instance ~w ignored the connection notification "
						"for node '~s' (information: ~p).~n",
						[ State#state_holder.actual_class, self(), Node,
						  MonitorNodeInfo ] ),

	State.



% WOOPER default node down handler.
%
% Returns an updated state.
%
% Can be overridden by defining or inheriting the onWOOPERNodeDisconnection/3
% oneway.
%
-spec default_node_down_handler( wooper:state(), net_utils:atom_node_name(),
						 monitor_utils:monitor_node_info() ) -> wooper:state().
default_node_down_handler( State, Node, MonitorNodeInfo ) ->

	wooper:log_warning( "WOOPER default node down handler of the ~w "
						"instance ~w ignored the disconnection notification "
						"for node '~s' (information: ~p).~n",
						[ State#state_holder.actual_class, self(), Node,
						  MonitorNodeInfo ] ),

	State.





% Returns the virtual table corresponding to the specified class.
%
% (helper)
%
-spec retrieve_virtual_table( class_name() ) ->
				   ?wooper_hashtable_type:?wooper_hashtable_type().
retrieve_virtual_table( Classname ) ->

	% For per-instance virtual table: wooper_create_method_table_for(?MODULE).
	get_class_manager() ! { get_table, Classname, self() },
	receive

		{ virtual_table, Table } ->
			%?wooper_hashtable_type:display( Table ),
			Table

	end.



% Triggers specified construction error (notify and throw).
%
% (helper)
%
-spec trigger_error( basic_utils:exception_class(), term(), class_name(),
					 [ method_arguments() ] ) -> no_return().
trigger_error( Reason, ErrorTerm, Classname, ConstructionParameters ) ->

	% Construction failed:
	% (error term would often be unreadable with ~p)

	Arity = length( ConstructionParameters ) + 1,

	log_error( "~nWOOPER error for PID ~w, "
			   "constructor (~s:construct/~B) failed (cause: ~p):~n~n"
			   " - with error term:~n  ~p~n~n"
			   " - stack trace was (latest calls first):~n~s~n"
			   " - for construction parameters:~n  ~p~n",
			   [ self(), Classname, Arity, Reason, ErrorTerm,
				 code_utils:interpret_stacktrace(), ConstructionParameters ] ),

	throw( { wooper_constructor_failed, self(), Classname, Arity,
			 ConstructionParameters, ErrorTerm } ).





% Methods for getting information about an instance.


% Returns the actual class name of the current instance.
%
% (helper)
%
-spec get_class_name( wooper:state() ) -> class_name().
get_class_name( State ) ->

	% Note: a mere ?MODULE would not work (ex: case of an inherited method,
	% compiled with the module name of the parent class).

	State#state_holder.actual_class.



% Returns the (user-level) attributes known of WOOPER for the specified state
% (i.e. all attributes except the ones used internally by WOOPER).
%
% (helper)
%
-spec get_attribute_pairs( wooper:state() ) -> [ attribute_entry() ].
get_attribute_pairs( State ) ->

	AllAttrs = get_all_attributes( State ),

	ReservedAttrs = get_wooper_reserved_attribute_names(),

	% Remove WOOPER internals:
	filter_wooper_attributes( AllAttrs, ReservedAttrs, _Acc=[] ).



% Removes from the specified atttributes the ones used internally by WOOPER (so
% that only class-specific ones remain).
%
% (internal helper)
%
filter_wooper_attributes( _AttrPairs=[], _ReservedAttrs, Acc ) ->
	Acc;

filter_wooper_attributes( _AttrPairs=[ AttrEntry={ Name, _Value } | T ],
						  ReservedAttrs, Acc ) ->

	case lists:member( Name, ReservedAttrs ) of

		true ->
			filter_wooper_attributes( T, ReservedAttrs, Acc );

		false ->
			filter_wooper_attributes( T, ReservedAttrs, [ AttrEntry | Acc ] )

	end.



% Returns a list of the attribute names that are used internally by WOOPER.
%
-spec get_wooper_reserved_attribute_names() -> [ attribute_name() ].
get_wooper_reserved_attribute_names() ->
	[].



% Returns a textual representation of the attributes of the specified state.
%
-spec state_to_string( wooper:state() ) -> string().
state_to_string( State ) ->

	% Not using get_attribute_pairs/1 to rely on the full state:
	Attributes = get_all_attributes( State ),

	% We prefer having the attributes sorted by their name, in alphabetical
	% order:
	%
	SortedAttributes = lists:keysort( _Index=1, Attributes ),

	lists:foldl(

		fun( { AttName, AttrValue }, Acc ) ->
			Acc ++ io_lib:format( "     * ~s = ~s~n",
				[ text_utils:term_to_string( AttName ),
				  text_utils:term_to_string( AttrValue, _MaxDepth=16,
											 _MaxLength=100 ) ] )

		end,

		io_lib:format( "State of ~w:~nInstance of ~s with ~B attribute(s):~n",
			[ self(), get_class_name( State ), length( Attributes ) ] ),

		SortedAttributes ).



-ifdef(wooper_debug).


% Returns a textual representation of the virtual table corresponding to the
% specified state.
%
% (helper)
%
-spec virtual_table_to_string( wooper:state() ) -> string().
virtual_table_to_string( State ) ->

	lists:foldl(

	  fun( { { Name, Arity }, Module }, String ) ->
			  String ++ io_lib:format( "     * ~s/~B -> ~s~n",
									   [ Name, Arity, Module ] )
	  end,

	  _Acc=io_lib:format( "Virtual table of ~w:~n(method name/arity -> "
						  "module defining that method)~n", [ self() ] ),

	  _List=?wooper_hashtable_type:enumerate(
			   State#state_holder.virtual_table ) ).



% Returns a textual representation of this instance, including its state and
% virtual table.
%
% (helper)
%
-spec instance_to_string( wooper:state() ) -> string().
instance_to_string( State ) ->
	io_lib:format( "Inspection of instance ~w:~n~n  + ~s~n  + ~s",
				   [ self(), state_to_string( State ),
					 virtual_table_to_string( State ) ] ).



% Displays the inner state of this instance.
%
% This is not a method.
%
-spec display_state( wooper:state() ) -> basic_utils:void().
display_state( State ) ->
	error_logger:info_msg( "~s~n", [ state_to_string( State ) ] ).



% Displays the virtual table of this instance.
%
% This is not a method.
%
-spec display_virtual_table( wooper:state() ) -> basic_utils:void().
display_virtual_table( State ) ->
	error_logger:info_msg( "~s~n", [ virtual_table_to_string( State ) ] ).


% Displays information about this instance.
%
% This is not a method.
%
-spec display_instance( wooper:state() ) -> basic_utils:void().
display_instance( State ) ->
	error_logger:info_msg( "~s~n", [ instance_to_string( State ) ] ).


-endif. % wooper_debug



% Returns all the attributes of this instance, as a list of { AttributeName,
% AttributeValue } pairs.
%
-spec get_all_attributes( wooper:state() ) -> [ attribute_entry() ].
get_all_attributes( State ) ->
	?wooper_hashtable_type:enumerate( State#state_holder.attribute_table ).



% Log section.


% Reports (on a best-effort basis) the specified information to the user,
% typically by displaying an information report on the console.
%
-spec log_info( string() ) -> basic_utils:void().
log_info( String ) ->
	error_logger:info_msg( String ++ "\n" ).


% Reports (on a best-effort basis) the specified information to the user,
% typically by displaying an information report on the console.
%
-spec log_info( text_utils:format_string(), [ term() ] ) ->
					  basic_utils:void().
log_info( FormatString, ValueList ) ->
	error_logger:info_msg( FormatString ++ "\n", ValueList ).



% Reports (on a best-effort basis) the specified warning to the user,
% typically by displaying a warning report on the console.
%
-spec log_warning( string() ) -> basic_utils:void().
log_warning( String ) ->
	error_logger:warning_msg( String ++ "\n" ),

	% Wait a bit, as error_msg seems asynchronous:
	system_utils:await_output_completion( ?wooper_warning_display_waiting ).


% Reports (on a best-effort basis) the specified warning to the user,
% typically by displaying a warning report on the console.
%
-spec log_warning( text_utils:format_string(), [ term() ] ) ->
					  basic_utils:void().
log_warning( FormatString, ValueList ) ->
	error_logger:warning_msg( FormatString ++ "\n", ValueList ),

	% Wait a bit, as error_msg seems asynchronous:
	system_utils:await_output_completion( ?wooper_warning_display_waiting ).



% Reports (as synchronously as possible, in order to avoid loosing this
% notification) the specified error to the user, typically by displaying an
% error report on the console (non-halting function, ex: no exception thrown).
%
-spec log_error( string() ) -> basic_utils:void().
log_error( Message ) ->

	error_logger:error_msg( Message ++ "\n" ),

	% Wait a bit, as error_msg seems asynchronous:
	system_utils:await_output_completion( ?wooper_error_display_waiting ).



% Reports (as synchronously as possible, in order to avoid loosing this
% notification) the specified error to the user, typically by displaying an
% error report on the console (non-halting function, ex: no exception thrown).
%
-spec log_error( text_utils:format_string(), [ term() ] ) ->
						   basic_utils:void().
log_error( FormatString, ValueList ) ->

	error_logger:error_msg( FormatString
							++ "=END OF WOOPER ERROR REPORT FOR ~w ===~n~n~n",
							ValueList ++ [ self() ] ),

	% Wait a bit, as error_msg seems asynchronous:
	system_utils:await_output_completion( ?wooper_error_display_waiting ).



% Reports (as synchronously as possible, in order to avoid loosing this
% notification) the specified error about the current WOOPER instance
% (preferably thanks to its state, otherwise with the current executed module,
% so with fewer information) to the user, typically by displaying an error
% report on the console (non-halting function, ex: no exception thrown).
%
-spec log_error( text_utils:format_string(), [ term() ],
		 wooper:state() | basic_utils:module_name() ) -> basic_utils:void().
log_error( FormatString, ValueList, State )
  when is_record( State, state_holder ) ->
	io:format( "~n", [] ),

	% Node information distracting:
	%log_error( "WOOPER error for ~s instance of PID ~w on node ~s"
	%		   ++ FormatString,
	%		   [ State#state_holder.actual_class, self(),
	%            node() | ValueList ] );
	log_error( "WOOPER error for ~s instance of PID ~w"
			   ++ FormatString,
			   [ State#state_holder.actual_class, self() | ValueList ] );

log_error( FormatString, ValueList, ModuleName ) when is_atom( ModuleName ) ->
	io:format( "~n", [] ),
	% Node information distracting:
	%log_error( "WOOPER error for instance of PID ~w on node ~s triggered "
	%		   "in module ~s" ++ FormatString,
	%		   [ self(), ModuleName, node() | ValueList ] ).
	log_error( "WOOPER error for instance of PID ~w triggered "
			   "in module ~s" ++ FormatString,
			   [ self(), ModuleName | ValueList ] ).




% Called by WOOPER whenever a request fails, to report it on the console and to
% the caller, and have the process instance exit.
%
-spec on_failed_request( request_name(), method_arguments(), pid(),
						 atom(), term(), wooper:state() ) -> no_return().
on_failed_request( RequestAtom, ArgumentList, CallerPid, Reason, ErrorTerm,
				   State ) ->

	Arity = length( ArgumentList ) + 1,

	ModulePrefix = lookup_method_prefix( RequestAtom, Arity, State ),

	log_error( ": request ~s~s/~B failed (cause: ~s):~n~n"
			   " - with error term:~n  ~p~n~n"
			   " - stack trace was (latest calls first):~n~s~n"
			   " - caller being process ~w"
			   " - for request parameters:~n  ~p~n",
			   [ ModulePrefix, RequestAtom, Arity, Reason, ErrorTerm,
				  code_utils:interpret_stacktrace(), CallerPid, ArgumentList ],
			   State ),

	% ArgumentList and actual method module not propagated back to the caller:
	%
	ErrorReason = { request_failed, State#state_holder.actual_class,
					self(), RequestAtom, { Reason, ErrorTerm } },

	CallerPid ! { wooper_error, ErrorReason },

	% We do not want a duplicate error message, yet we cannot use 'normal' as
	% linked processes would not be triggered:
	%
	exit( request_failed ).



% Called by WOOPER whenever a oneway fails, to report it on the console and to
% the caller, and have the process instance exit.
%
-spec on_failed_oneway( oneway_name(), method_arguments(), atom(), term(),
						wooper:state() ) -> no_return().
on_failed_oneway( OnewayAtom, ArgumentList, Reason, ErrorTerm, State ) ->

	Arity = length( ArgumentList ) + 1,

	ModulePrefix = lookup_method_prefix( OnewayAtom, Arity, State ),

	log_error( ": oneway ~s~s/~B failed (cause: ~s):~n~n"
			   " - with error term:~n  ~p~n~n"
			   " - stack trace was (latest calls first):~n~s~n"
			   " - for oneway parameters:~n  ~p~n",
			   [ ModulePrefix, OnewayAtom, Arity, Reason, ErrorTerm,
				  code_utils:interpret_stacktrace(), ArgumentList ], State ),

			% No caller to notify, for oneways.

			% We do not want a duplicate error message, yet we cannot use
			% 'normal' as linked processes would not be triggered:
			%
			exit( oneway_failed ).



% Looks up the module defining specified method, and returns a textual prefix
% specifying it, if found.
%
% Used for error management, hence designed not to fail.
%
% (helper)
%
-spec lookup_method_prefix( method_name(), arity(), wooper:state() ) ->
								  string().
lookup_method_prefix( MethodAtom, Arity, State ) ->

	try wooper_lookup_method( State, MethodAtom, Arity ) of

		{ value, Module } ->
			text_utils:format( "~s:", [ Module ] );

		key_not_found ->
			""

	catch

		_:_ ->
			""

	end.



% Helper function to test requests.
%
% Allows to test from the shell an instance by sending it requests (hence
% needing a receive, whereas the caller is the shell), and waiting for any kind
% of message sent back.
%
% Returns the actual result or received value.
%
% Available even when debug mode is off.
%
-spec send_and_listen( instance_pid(), request_name(), method_arguments() ) ->
							 term().
send_and_listen( InstancePid, RequestName, Arguments ) ->

	InstancePid ! { RequestName, Arguments, self() },

	receive

		{ wooper_result, Result } ->

			%io:format( "Result of call to '~w' with arguments '~w': ~s~n",
			%	[ RequestName, Arguments,
			%	 text_utils:term_to_string( Result ) ] ),

			Result;

		Anything ->

			%io:format( "Answer to call to '~w' with arguments '~w': ~s~n",
			%	[ RequestName, Arguments,
			%	  text_utils:term_to_string( Anything ) ] ),

			Anything

	end.



% Returns the result corresponding to the first pending WOOPER request (the
% latest sent one), or blocks.
%
% (helper)
%
-spec receive_result() -> request_result( any() ).
receive_result() ->

	receive

		{ wooper_result, R } ->
			R

	end.






% Deletion-related section.



% Deletes (asynchronously: "fire and forget") the WOOPER instance(s) potentially
% stored in the specified attribute list.
%
% Sets the corresponding attribute(s) to 'undefined', returns an updated state.
%
% Ex: in a destructor: DeleteState = delete_any_instance_referenced_in( [
% first_pid_attr, second_pid_attr ], State ) or
% delete_any_instance_referenced_in( my_pid_attr, State ).
%
% (helper)
%
-spec delete_any_instance_referenced_in( [ attribute_name() ], wooper:state() )
									   -> wooper:state().
delete_any_instance_referenced_in( _Attributes=[], State ) ->
	State;


delete_any_instance_referenced_in( [ PidAttribute | T ], State ) ->

	NewState = case ?getAttr(PidAttribute) of

		undefined ->
			State;

		Pid when is_pid( Pid ) ->
			Pid ! delete,
			setAttribute( State, PidAttribute, undefined )

	end,
	delete_any_instance_referenced_in( T, NewState );


delete_any_instance_referenced_in( PidAttribute, State ) ->

	case ?getAttr(PidAttribute) of

		undefined ->
			State;

		Pid when is_pid( Pid ) ->
			Pid ! delete,
			setAttribute( State, PidAttribute, undefined )

	end.





% Deletes (synchronously, in a parallel yet blocking manner) the WOOPER
% instance(s) potentially stored in specified attribute list (a standalone
% attribute may be specified as well).
%
% Sets the corresponding attribute(s) to 'undefined', returns an updated state.
%
% Ex: in a destructor: NewState =
% delete_synchronously_any_instance_referenced_in( [ first_pid_attr,
% second_pid_attr ], State ) or
% delete_synchronously_any_instance_referenced_in( my_pid_attr, State ).
%
-spec delete_synchronously_any_instance_referenced_in(
	[ attribute_name() ] | attribute_name(), wooper:state() ) -> wooper:state().
delete_synchronously_any_instance_referenced_in( Attributes, State ) ->
	delete_synchronously_any_instance_referenced_in( Attributes,
										 _PreTestLiveliness=false, State ).


% Deletes safely (pre-testing whether the specified process still exists before
% attempting to delete it, in order to avoid having to wait for a synchronous
% time-out) and synchronously, in a parallel yet blocking manner, the WOOPER
% instance(s) potentially stored in specified attribute list (a standalone
% attribute may be specified as well).
%
% Sets the corresponding attribute(s) to 'undefined', returns an updated state.
%
% Ex: in a destructor: NewState =
% safe_delete_synchronously_any_instance_referenced_in( [ first_pid_attr,
% second_pid_attr ], State ) or
% safe_delete_synchronously_any_instance_referenced_in( my_pid_attr, State ).
%
-spec safe_delete_synchronously_any_instance_referenced_in(
	[ attribute_name() ] | attribute_name(), wooper:state() ) -> wooper:state().
safe_delete_synchronously_any_instance_referenced_in( Attributes, State ) ->
	delete_synchronously_any_instance_referenced_in( Attributes,
										 _PreTestLiveliness=true, State ).



delete_synchronously_any_instance_referenced_in( _Attributes=[],
												 _PreTestLiveliness, State ) ->
	State;

delete_synchronously_any_instance_referenced_in( Attributes, PreTestLiveliness,
								 State ) when is_list( Attributes ) ->

	% Triggers the deletion of selected instances:
	{ TargetAttributes, TargetPids } = delete_pid_from( Attributes,
												PreTestLiveliness, State ),

	%io:format( "delete_synchronously_any_instance_referenced_in:~n"
	%			" - attributes are: ~p~n"
	%			" - PIDs are: ~p~n", [ TargetAttributes, TargetPids ] ),

	% Waits for their completion:
	wait_for_deletion_ack( TargetPids ),

	%io:format( "(all deletion acks received for ~p)~n", [ TargetAttributes ] ),

	% Erases deleted PIDs:
	UndefinedAttributes = [ { AttrName, undefined } ||
							  AttrName <- TargetAttributes ],

	setAttributes( State, UndefinedAttributes );


delete_synchronously_any_instance_referenced_in( Attribute, PreTestLiveliness,
												 State ) ->

	case ?getAttr( Attribute ) of

		undefined ->
			State;

		Pid when is_pid( Pid ) ->
			case PreTestLiveliness andalso not basic_utils:is_alive( Pid ) of

				% Only case where no actual deletion is needed:
				true ->
					ok;

				false ->
					Pid ! { synchronous_delete, self() },

					receive

						{ deleted, Pid } ->
							ok

					end

			end,

			setAttribute( State, Attribute, undefined )

	end.




% Helper, which sends delete messages to all PIDs found in the list of
% attributes, and returns a list of the attributes and a list of the PIDs.
%
% If PreTestLiveliness is true, checks first that the process is not already
% dead, to avoid waiting for a synchronous time-out.
%
delete_pid_from( Attributes, PreTestLiveliness, State ) ->

	DeleteMessage = { synchronous_delete, self() },

	delete_pid_from( Attributes, DeleteMessage, PreTestLiveliness, State,
					 _AccAttr=[], _AccPid=[] ).


delete_pid_from( _Attributes=[], _DeleteMessage, _PreTestLiveliness, _State,
				 AccAttr, AccPid ) ->
	{ AccAttr, AccPid };

delete_pid_from( [ Attr | T ], DeleteMessage, PreTestLiveliness, State,
				 AccAttr, AccPid ) ->

	case ?getAttr( Attr ) of

		undefined ->
			delete_pid_from( T, DeleteMessage, PreTestLiveliness, State,
							 AccAttr, AccPid ) ;

		Pid when is_pid( Pid ) ->

			case PreTestLiveliness andalso not basic_utils:is_alive( Pid ) of

				% Only case where no deletion oneway shall be sent:
				true ->
					delete_pid_from( T, DeleteMessage, PreTestLiveliness,
									 State, [ Attr | AccAttr ], AccPid );

				false ->
					%io:format( "Deleting now ~s (PID: ~w).~n",
					% [ Attr, Pid ] ),
					Pid ! DeleteMessage,
					delete_pid_from( T, DeleteMessage, PreTestLiveliness,
							 State, [ Attr | AccAttr ], [ Pid | AccPid ] )

			end

	end.



% Deletes specified instance synchronously.
%
% Will wait forever the effective termination of the specified instance.
%
-spec delete_synchronously_instance( instance_pid() ) -> basic_utils:void().
delete_synchronously_instance( InstancePid ) ->

	%io:format( "delete_synchronously_instance for ~p.~n", [ InstancePid ] ),

	InstancePid ! { synchronous_delete, self() },

	receive

		{ deleted, InstancePid } ->
			ok

	end.



% Deletes specified instances synchronously (yet in parallel).
%
% Will wait forever the effective termination of all instances (and will
% regularly write a message on the console if waiting for too long) .
%
% (exported helper)
%
-spec delete_synchronously_instances( [ instance_pid() ] ) ->
											basic_utils:void().
delete_synchronously_instances( InstanceList ) ->

	%io:format( "delete_synchronously_instances for ~p.~n", [ InstanceList ] ),

	DeleteMessage = { synchronous_delete, self() },

	[ I ! DeleteMessage || I <- InstanceList ],

	wait_for_deletion_ack( InstanceList ).



% Helper used to wait for the receiving of all deletion acknowledgements:
%
% Could almost use basic_utils:wait_for_acks/3.
%
wait_for_deletion_ack( _WaitedPids=[] ) ->
	ok;

wait_for_deletion_ack( WaitedPids ) ->

	receive

		{ deleted, Pid } ->

			case lists:member( Pid, WaitedPids ) of

				false ->
					throw( { unexpected_pid_deletion, Pid } );

				true ->
					NewWaitedPids = lists:delete( Pid, WaitedPids ),
					wait_for_deletion_ack( NewWaitedPids )

			end

	% Note that this time-out is reset at each ack:
	after ?synchronous_time_out ->

			case examine_waited_deletions( WaitedPids, _Acc=[] ) of

				[] ->
					ok;

				NewWaitedPids ->
					io:format( "(still waiting for the synchronous deletion of "
							   "following live WOOPER instance(s): ~p)~n",
							   [ NewWaitedPids ] ),

					% Warns, but does not trigger failures:
					wait_for_deletion_ack( NewWaitedPids )

			end

	end.



examine_waited_deletions( _WaitedPids=[], Acc ) ->
	Acc;

examine_waited_deletions( _WaitedPids=[ Pid | T ], Acc ) ->

	%io:format( "Testing whether ~p is alive...~n", [ Pid ] ),

	% Manages processes that are not local as well:
	case basic_utils:is_alive( Pid ) of

		true ->
			examine_waited_deletions( T, [ Pid | Acc ] );

		false ->
			io:format( "Stopped waiting for the deletion of instance "
					   "whose PID is ~p: not found alive.~n", [ Pid ] ),

			examine_waited_deletions( T, Acc )

	end.



% Deletes specified instances synchronously (yet in parallel), safely, knowing
% there might be duplicates in the specified list and that some instances may
% even be already dead.
%
% Will wait forever the effective termination of all instances (and will
% regularly write a message on the console if waiting for too long) .
%
% (exported helper)
%
-spec safe_delete_synchronously_instances( [ instance_pid() ] ) ->
												 basic_utils:void().
safe_delete_synchronously_instances( InstanceList ) ->

	% Testing for liveliness allows to avoid synchronous time-outs:
	FilteredInstanceList = [ InstancePid ||
			InstancePid <- list_utils:uniquify( InstanceList ),
			basic_utils:is_alive( InstancePid ) ],

	delete_synchronously_instances( FilteredInstanceList ).
