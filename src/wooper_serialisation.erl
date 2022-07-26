% Copyright (C) 2012-2022 Olivier Boudeville
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
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
% Creation date: 2012.


% @doc Module containing facilities for the <b>serialisation and deserialisation
% of WOOPER instances</b>.
%
% Notes:
%
% - offsetting as much as possible code from the counterpart class header in
% this module allows for smaller, cache-friendly BEAMs, and short compilation
% times
%
% - introducing a class_Serialisable will much improve (and change) the way
% serialisation and deserialisation are to happen
%
-module(wooper_serialisation).




% First, all kinds of exports:

% Not working: at headerfile "wooper_state_functions.hrl"


% For getAttr/1, and for setAttributes/2 and all being unused:
-include("wooper_state_exports.hrl").

% For synchronous_time_out:
-include("wooper_defines_exports.hrl").

% For attribute_name/0:
-include("wooper_types_exports.hrl").


% Otherwise executeRequest/3 and all reported as unused:
-include("wooper_execute_exports.hrl").

% To silence wooper_execute_method_as/4 being unused:
-include("wooper_execute_internal_exports.hrl").

% To silence wooper_execute_method_as/4 and all being unused:
-include("wooper_serialisation_exports.hrl").


% For myriad_spawn*:
-include_lib("myriad/include/spawn_utils.hrl").


% Instance loading:
-export([ load/1, load/3, load_link/1, load_link/3,

		  synchronous_load/1, synchronous_load/3, synchronous_load_link/1,
		  synchronous_load_link/3,

		  remote_synchronisable_load_link/2, remote_synchronisable_load_link/4,
		  remote_synchronous_timed_load_link/2,
		  remote_synchronous_timed_load_link/4 ]).



% Instance deserialisation:
-export([ deserialise/4 ]).



% Serialisation helpers:
-export([ handle_private_processes/2, mute_attributes/2,
		  check_attributes_equal/3, replace_attribute/3, replace_attributes/3,
		  merge_list_for/3, merge_lists_for/3,
		  list_restoration_markers/0, serialise_term/1 ]).



-type entry_transformer() ::
			fun( ( attribute_entry(), user_data() ) ->
					{ attribute_entry(), user_data() } ).
% Any function that is able to transform the state of an instance.
%
% Note: see basic_utils:traverse_term/4, which may be useful in that context.



-type term_serialisation() :: [ attribute_entry() ].
% The serialisation form of an instance, as an Erlang term.
% This is the term that will be passed to an actual serializer.


-type serialisation() :: bin_serialisation() | term().
% Designates the serialisation form of an instance, which may or may not be a
% binary (e.g. our default bin_serialisation/0 versus a JSON-based term).


-type bin_serialisation() :: binary().
% The serialisation form of an instance, as an ext_binary, that is a binary data
% object, structured according to the Erlang external term format.
%
% However erlang:ext_binary/0 is not exported:
%-type bin_serialisation() :: erlang:ext_binary().


-export_type([ entry_transformer/0, term_serialisation/0,
			   serialisation/0, bin_serialisation/0 ]).


-type restoration_marker() :: ?process_restoration_marker
							| ?file_restoration_marker
							| ?term_restoration_marker.


% Exported, as wooper_serialisation_functions.hrl has to be included, yet this
% leads to have the function spotted as unused:
%
-export([ serialise/3 ]).


% Now, function definitions:


% For executeRequest/2:
-include("wooper_execute_functions.hrl").

% For getAttribute/2, setAttribute/3, etc.:
-include("wooper_state_functions.hrl").


% For wooper_deserialise/4:
-include("wooper_serialisation_functions.hrl").


% For wooper_execute_method/3:
-include("wooper_execute_internal_functions.hrl").


% Implementation notes:


% About serialised elements
%
% The goal of a serialisation is to store in a serialised form the full state
% of, here, a WOOPER instance.
%
% This includes at least all values of all WOOPER attributes, knowing that these
% terms may reference transient values that cannot be reproducibly recreated
% (ex: PID, references, open files or sockets); we introduced hooks/methods that
% can be overridden so that an application-specific entry transformer can manage
% them.
%
% As we do not have a typed (class-specific) state (record-like) data-structure
% or, currently, a reliable way of ensuring that all instances of a given class
% have the exact same list of attributes, we have to store not only the values,
% but also the keys; a major progress in terms of compactness still lies there.
%
% The serialisation should also encompass the following process-level
% information:
%
%  - the process dictionary (key/values, both of which can be also transient
%  values); this includes the random state of that instance process
%
%  - the processes to which this instance is linked
%
%  - whether this process traps EXIT messages
%
%  - possibly: group leader, priority, monitors
%
% Use erlang:process_info/1 to figure out process-level available information
% that may have to be restored and thus saved.
%
% The WOOPER serialisation mechanisms do not account for these process-level
% currently (ex: links may be dictated by the application logic and thus may not
% have to be stored), except one: the current random state of the serialised
% process, which is transparently managed by WOOPER so that the deserialisation
% will lead to restoring the right random state.


% About serialisation formats.
%
% Although any kind of serialisation format could be used, here, at least by
% default, we rely on the Erlang Term Format
% (https://www.erlang.org/doc/apps/erts/erl_ext_dist.html), at it is a compact,
% well-integrated, binary, at least reasonably efficient solution.



% In this section, we define, for the current class, loading counterparts to the
% new operators, i.e. the various ways of creating an instance of that class
% from a deserialisation form (loading), instead of through a normal
% construction process.
%
% For example, synchronous_new_link becomes synchronous_load_link.


% Transformers are expected not to depend on the order of their calls, as
% loadings can happen in parallel.

% No updated user data is ever sent back, for the sake of API uniformity.


% Shorthands:

-type user_data() :: basic_utils:user_data().
-type node_name() :: net_utils:node_name().



% @doc Spawns an instance of this class, based on the specified serialisation
% information.
%
% Returns the PID of the instance created by this loading.
%
% This creation will be asynchronous: this function returns a legit PID as soon
% as the creation is triggered, without waiting for it to complete.
%
-spec load( bin_serialisation() ) -> instance_pid().
load( BinSerialisation ) ->
	load( BinSerialisation, _MaybeEntryTransformer=undefined,
		  _UserData=undefined ).



% @doc Spawns an instance of this class, based on the specified serialisation
% information, on any entry transformer and user data.
%
% Returns the PID of the instance created by this loading.
%
% This creation will be asynchronous: this function returns a legit PID as soon
% as the creation is triggered, without waiting for it to complete.
%
-spec load( bin_serialisation(), maybe( entry_transformer() ), user_data() ) ->
													instance_pid().
load( BinSerialisation, MaybeEntryTransformer, UserData ) ->
	?myriad_spawn(
		fun() ->
			deserialise( BinSerialisation, MaybeEntryTransformer, UserData,
						 _ListenerPid=undefined )
		end ).



% @doc Spawns an instance of this class, based on the specified serialisation
% information, and links it to the current process.
%
% Returns the PID of the instance created by this loading.
%
% This creation will be asynchronous: this function returns a legit PID as soon
% as the creation is triggered, without waiting for it to complete.
%
-spec load_link( bin_serialisation() ) -> instance_pid().
load_link( BinSerialisation ) ->
	load_link( BinSerialisation, _MaybeEntryTransformer=undefined,
			   _UserData=undefined ).



% @doc Spawns an instance of this class, based on the specified serialisation
% information, on entry transformer and user data, and links it to the current
% process.
%
% Returns the PID of the instance created by this loading.
%
% This creation will be asynchronous: this function returns a legit PID as soon
% as the creation is triggered, without waiting for it to complete.
%
-spec load_link( bin_serialisation(), maybe( entry_transformer() ),
				 user_data() ) -> instance_pid().
load_link( BinSerialisation, MaybeEntryTransformer, UserData ) ->
	?myriad_spawn_link(
		fun() ->
			deserialise( BinSerialisation, MaybeEntryTransformer, UserData,
						 _ListenerPid=undefined )
		end ).



% @doc Spawns an instance of this class, based on the specified serialisation
% information.
%
% Returns the PID of the instance created by this loading.
%
% This creation is synchronous: the call will return only when the created
% process reports that it is up and running.
%
-spec synchronous_load( bin_serialisation() ) -> instance_pid().
synchronous_load( BinSerialisation ) ->
	synchronous_load( BinSerialisation, _MaybeEntryTransformer=undefined,
					  _UserData=undefined ).



% @doc Spawns an instance of this class, based on the specified serialisation
% information, on any entry transformer and user data.
%
% Returns the PID of the instance created by this loading.
%
% This creation is synchronous: the call will return only when the created
% process reports that it is up and running.
%
-spec synchronous_load( bin_serialisation(), maybe( entry_transformer() ),
						user_data() ) -> instance_pid().
synchronous_load( BinSerialisation, MaybeEntryTransformer, UserData ) ->

	CreatorPid = self(),

	SpawnedPid = ?myriad_spawn(
		fun() ->
			deserialise( BinSerialisation, MaybeEntryTransformer, UserData,
						 _ListenerPid=CreatorPid )
		end ),

	% Blocks until the spawned process answers:
	receive

		{ spawn_successful, SpawnedPid } ->
			SpawnedPid

	end.



% @doc Spawns an instance of this class, based on the specified serialisation
% information, and links it to the current process.
%
% Returns the PID of the instance created by this loading.
%
% This creation is synchronous: the call will return only when the created
% process reports that it is up and running.
%
-spec synchronous_load_link( bin_serialisation() ) -> instance_pid().
synchronous_load_link( BinSerialisation ) ->
	synchronous_load_link( BinSerialisation, _MaybeEntryTransformer=undefined,
						   _UserData=undefined ).



% @doc Spawns an instance of this class, based on the specified serialisation
% information, on any entry transformer and user data, and links it to the
% current process.
%
% Returns the PID of the instance created by this loading.
%
% This creation is synchronous: the call will return only when the created
% process reports that it is up and running.
%
-spec synchronous_load_link( bin_serialisation(), maybe( entry_transformer() ),
							 user_data() ) -> instance_pid().
synchronous_load_link( BinSerialisation, MaybeEntryTransformer, UserData ) ->

	CreatorPid = self(),

	SpawnedPid = ?myriad_spawn_link(
		fun() ->
			deserialise( BinSerialisation, MaybeEntryTransformer, UserData,
						 _ListenerPid=CreatorPid )
		end ),

	% Blocks until the spawned process answers:
	receive

		{ spawn_successful, SpawnedPid } ->
			SpawnedPid

	end.


% We did not feel the specific need to define:
% - synchronous_timed_load
% - synchronous_timed_load_link
% - remote_load
% - remote_load_link
% - remote_synchronous_load
% - remote_synchronous_load_link
% - remote_synchronous_timed_load
%
% (but they can be added if wanted)



% @doc Spawns on the specified node an instance of this class, based on the
% specified serialisation information, and links it to the current process.
%
% Returns the PID of the created instance.
%
% This creation is asynchronous (the PID is directly returned), however a
% {spawn_successful, SpawnedPid} message will be received by the calling process
% once (if ever) the instance is up and running. This allows performing the
% actual instance creations in parallel, by waiting sets of concurrent
% creations.
%
-spec remote_synchronisable_load_link( node_name(), bin_serialisation() ) ->
											instance_pid().
remote_synchronisable_load_link( Node, BinSerialisation ) ->
	remote_synchronisable_load_link( Node, BinSerialisation,
		_MaybeEntryTransformer=undefined, _UserData=undefined ).



% @doc Spawns on the specified node an instance of this class, based on the
% specified serialisation information, on any entry transformer and user data,
% and links it to the current process.
%
% Returns the PID of the created instance.
%
% This creation is asynchronous (the PID is directly returned), however a
% {spawn_successful, SpawnedPid} message will be received by the calling process
% once (if ever) the instance is up and running. This allows performing the
% actual instance creations in parallel, by waiting sets of concurrent
% creations.
%
-spec remote_synchronisable_load_link( node_name(),
	bin_serialisation(), maybe( entry_transformer() ), user_data() ) ->
												instance_pid().
remote_synchronisable_load_link( Node, BinSerialisation, MaybeEntryTransformer,
								 UserData ) ->

	CreatorPid = self(),

	?myriad_spawn_link( Node,
		fun() ->
			deserialise( BinSerialisation, MaybeEntryTransformer, UserData,
						 _ListenerPid=CreatorPid )
		end ).



% @doc Spawns on the specified node an instance of this class, based on the
% specified serialisation information, on any entry transformer and user data,
% and links it to the current process.
%
% Returns the PID of the instance created by this loading, or the time_out
% atom.
%
% This creation is asynchronous (the PID is directly returned), however a
% {spawn_successful, SpawnedPid} message will be received by the calling process
% once (if ever) the instance is up and running. This allows performing the
% actual instance creations in parallel, by waiting sets of concurrent
% creations.
%
-spec remote_synchronous_timed_load_link( node_name(),
							bin_serialisation() ) -> instance_pid().
remote_synchronous_timed_load_link( Node, BinSerialisation ) ->
	remote_synchronous_timed_load_link( Node, BinSerialisation,
		_MaybeEntryTransformer=undefined, _UserData=undefined ).



% @doc Spawns on the specified node an instance of this class, based on the
% specified serialisation information, on entry transformer and user data, and
% links it to the current process.
%
% Returns the PID of the instance created by this loading, or the time_out
% atom.
%
% This creation is asynchronous (the PID is directly returned), however a
% {spawn_successful, SpawnedPid} message will be received by the calling process
% once (if ever) the instance is up and running. This allows performing the
% actual instance creations in parallel, by waiting sets of concurrent
% creations.
%
-spec remote_synchronous_timed_load_link( node_name(),
	bin_serialisation(), maybe( entry_transformer() ), user_data() ) ->
												instance_pid().
remote_synchronous_timed_load_link( Node, BinSerialisation,
									MaybeEntryTransformer, UserData ) ->

	CreatorPid = self(),

	SpawnedPid = ?myriad_spawn_link( Node,
		fun() ->
			deserialise( BinSerialisation, MaybeEntryTransformer,
						 UserData, _ListenerPid=CreatorPid )
		end ),

	% Blocks until the spawned process answers or a time-out occurs:
	receive

		{ spawn_successful, SpawnedPid } ->
			SpawnedPid

	after ?synchronous_time_out ->

		trace_utils:error_fmt( "(remote_synchronous_timed_load_link: "
			"throwing time-out on node ~p for module ~p after ~p milliseconds)",
			[ Node, ?MODULE, ?synchronous_time_out ] ),

		throw( { remote_synchronous_linked_time_out, Node, ?MODULE } )

	end.



% @doc Deserialises the specified instance from its serialised form (as a
% binary), to obtain its corresponding state, using any specified entry
% transformer and user data, then having the current executing process embody
% this instance from then on.
%
% Does not return, as the WOOPER main loop will manage, from then on, this just
% deserialised instance.
%
% (helper, as the receiver process may not even be already a WOOPER instance)
%
% Note: the hosting process is not created here, as, for an increased
% parallelism, we expect deserialisations to happen directly from the final
% instance processes; so we consider here that the process executing this helper
% is the final hosting one.
%
-spec deserialise( bin_serialisation(), maybe( entry_transformer() ),
				   user_data(), maybe( pid() ) ) -> no_return().
deserialise( BinSerialisation, MaybeEntryTransformer, UserData,
			 MaybeListenerPid ) ->

	{ Classname, SerialisedEntries } = binary_to_term( BinSerialisation ),

	% First we extract the WOOPER extra information:
	{ MaybeRandomState, OtherEntries } =
		option_list:extract( wooper_random_state, SerialisedEntries ),

	HookedEntries =
		pre_deserialise_hook( { Classname, OtherEntries }, UserData ),

	VirtualTableKey = wooper:retrieve_virtual_table_key( Classname ),

	{ TransformedEntries, FinalUserData } = case MaybeEntryTransformer of

		undefined ->
			{ HookedEntries, UserData };

		EntryTransformer ->
			lists:foldl( EntryTransformer,
						 _Acc0={ _ResultingEntries=[], UserData },
						 _List=HookedEntries )

	end,

	% Sent as soon as available (rather than at the end):
	MaybeListenerPid =:= undefined orelse
		( MaybeListenerPid ! { onDeserialisation, [ self(), FinalUserData ] } ),

	% Now we have the right attributes enumerated.

	% We need to bypass any constructor here.

	AttributeTable = ?wooper_table_type:add_entries( TransformedEntries,
										?wooper_table_type:new() ),

	% If ever useful:
	OptimisedAttributeTable = ?wooper_table_type:optimise( AttributeTable ),

	% Must be restored as well:
	MaybeRandomState =:= undefined orelse
		random_utils:set_random_state( MaybeRandomState ),

	VirtualTable = persistent_term:get( VirtualTableKey ),

	ForgedState = #state_holder{ virtual_table=VirtualTable,
								 attribute_table=OptimisedAttributeTable,
								 actual_class=Classname,
								 request_sender=undefined },


	% We could check here that no serialisation marker remains, with a specific
	% entry transformer and list_restoration_markers/0.

	FinalState = post_deserialise_hook( ForgedState ),

	% That's as simple as that!

	Classname:wooper_main_loop( FinalState ).




% @doc Handles private processes (through the name of the corresponding
% attributes), which are processes that are internal to an instance that is to
% be serialised, so that any next serialisation will see instead of their
% (former) PID a serialisation marker.
%
% Returns an updated state.
%
% (helper, typically used in pre_serialise_hook/1, to avoid trying to serialise
% internal processes)
%
-spec handle_private_processes( [ attribute_name() ], wooper:state() ) ->
										wooper:state().
handle_private_processes( PrivateAttributeNames, State ) ->

	lists:foldl(
		fun( PrivateAttrName, AccState ) ->

			NewValue = case getAttribute( AccState, PrivateAttrName ) of

				undefined ->
					undefined;

				Pid when is_pid( Pid ) ->

					% We just hide these PIDs on the serialised form: after
					% serialisation, the live state will still reference them.
					%
					?process_restoration_marker

			end,

			setAttribute( AccState, PrivateAttrName, NewValue )

		end,
		_Acc0=State,
		_List=PrivateAttributeNames ).



% @doc Mutes the specified attributes (that is, replaces any attribute value not
% equal to 'undefined' by a term restoration marker), for example so that they
% can escape the serialisation process.
%
% Typically used in pre_serialise_hook/2, to store only relevant, useful
% information.
%
% (helper)
%
-spec mute_attributes( [ attribute_name() ], wooper:state() ) -> wooper:state().
mute_attributes( AttributeNameList, State ) ->

	lists:foldl(
		fun( AttrName, AccState ) ->

			case hasAttribute( AccState, AttrName ) of

				true ->
					case getAttribute( AccState, AttrName ) of

						undefined ->
							% Let it as is:
							AccState;

						_ ->
							setAttribute( AccState, AttrName,
										  ?term_restoration_marker )

					end;

				false ->
					throw( { unknown_attribute, AttrName, AccState } )

			end

		end,
		_Acc0=State,
		_List=AttributeNameList ).



% @doc Checks that the specified attributes have the same value in the specified
% state and in the specified entries, otherwise throws an exception.
%
% (helper)
%
-spec check_attributes_equal( [ attribute_name() ], [ attribute_entry() ],
							  wooper:state() ) -> void().
check_attributes_equal( _AttributeNames=[], _AttributeEntries, _State ) ->
	ok;

check_attributes_equal( _AttributeNames=[ AttributeName | T ], AttributeEntries,
						State ) ->

	{ AttributeValue, RemainingEntries } =
		option_list:extract( _K=AttributeName, AttributeEntries ),

	case ?getAttr(AttributeName) of

		AttributeValue ->
			check_attributes_equal( T, RemainingEntries, State );

		OtherValue ->
			throw( { attribute_value_mismatch, AttributeName,
					 { OtherValue, AttributeValue } } )

	end.



% @doc Replaces the value held in the specified state by the one of the
% specified attribute found in the specified entry.
%
% (helper)
%
-spec replace_attribute( attribute_name(), [ attribute_entry() ],
				wooper:state() ) -> { [ attribute_entry() ], wooper:state() }.
replace_attribute( AttributeName, AttributeEntries, State ) ->

	case hasAttribute( State, AttributeName ) of

		true ->
			{ ToSetValue, RemainingEntries } =
				option_list:extract( _K=AttributeName, AttributeEntries ),

			NewState = setAttribute( State, AttributeName, ToSetValue ),

			{ RemainingEntries, NewState };

		false ->
			throw( { unknown_attribute, AttributeName, State } )

	end.



% @doc Replaces the values held in the specified state by the ones of the
% specified attributes found in the specified entries.
%
% (helper)
%
-spec replace_attributes( [ attribute_name() ], [ attribute_entry() ],
				wooper:state() ) -> { [ attribute_entry() ], wooper:state() }.
replace_attributes( AttributeNames, AttributeEntries, State ) ->

	lists:foldl(
		fun( AttrName, { AccEntries, AccState } ) ->
			replace_attribute( AttrName, AccEntries, AccState )
		end,
		_Acc0={ AttributeEntries, State },
		_List=AttributeNames ).



% @doc Extracts the value (supposedly, any type of list, or a set) of the
% specified attribute from the specified entries, and append that list to the
% corresponding one found in the specified state, stored under the same
% attribute name.
%
% Returns the remaining entries, and an updated state.
%
-spec merge_list_for( attribute_name(), [ attribute_entry() ],
			wooper:state() ) -> { [ attribute_entry() ], wooper:state() }.
merge_list_for( AttributeName, AttributeEntries, State ) ->

	{ ToMergeValue, RemainingEntries } =
		option_list:extract( _K=AttributeName, AttributeEntries ),

	InitialValue = ?getAttr(AttributeName),

	MergedValue = case ToMergeValue of

		PlainList when is_list( PlainList ) ->
			InitialValue ++ PlainList;

		% We suppose it is a set (that cannot match a list)
		Set ->
			set_utils:union( InitialValue, Set )

	end,

	MergedState = setAttribute( State, AttributeName, MergedValue ),

	{ RemainingEntries, MergedState }.



% @doc Extracts the value (supposedly, any type of list, or a set) of each of
% the specified attributes from the specified entries, and append that list to
% the corresponding one found in the specified state, stored under the same
% attribute name.
%
% Returns the remaining entries, and an updated state.
%
-spec merge_lists_for( [ attribute_name() ], [ attribute_entry() ],
			wooper:state() ) -> { [ attribute_entry() ], wooper:state() }.
merge_lists_for( AttributeNames, AttributeEntries, State ) ->

	lists:foldl(
		fun( AttrName, { AccEntries, AccState } ) ->
			merge_list_for( AttrName, AccEntries, AccState )
		end,
		_Acc0={ AttributeEntries, State },
		_List=AttributeNames ).



% @doc Returns a list of the known restoration markers.
%
% (helper)
%
-spec list_restoration_markers() -> [ restoration_marker() ].
list_restoration_markers() ->
	[ ?process_restoration_marker, ?file_restoration_marker,
	  ?term_restoration_marker ].



% @doc The actual function in charge of serialising a term prepared for
% serialisation.
%
% Of course other forms of serialisation could be introduced at this level.
%
-spec serialise_term( term() ) -> bin_serialisation().
serialise_term( Term ) ->
	% The 'deterministic' option does not seem crucial here:
	term_to_binary( Term, _Opts=[ { compressed, 9 }, { minor_version, 2 } ] ).
