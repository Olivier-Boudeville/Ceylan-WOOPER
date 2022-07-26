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


% Modular WOOPER header gathering all serialisation-related functions.


% Note: because of the hooks (which must be class-specific) and of the lack of a
% common ancestor to all WOOPER classes (we could have defined methods like
% serialise/3 and all in this module; a class_Serialisable abstract class will
% be defined in the future), some serialisation services are provided through an
% header file (this one), short of being able to be defined in
% wooper_serialisation.erl (which would be preferable).


% @doc Serialises the specified instance (that is the state thereof), using the
% specified entry transformer (if any) and user data.
%
% Returns a binary corresponding to a {InnerPair, UpdatedUserData} pair made of:
%
% 1. an inner pair {Classname, Entries}, converted into a binary and containing
% the corresponding class name (as an atom) and the associated serialisation (as
% a list of transformed terms corresponding to attributes)
%
% 2. the resulting user data, possibly modified by the successive operations
% done by the entry transformer
%
% This is a method, but as it is defined unconditionally in the WOOPER header
% (in the future, a WOOPER object superclass will be available, defining a
% default, overridable serialise/3 method), it cannot be overridden on a
% per-class basis. Hence the needs for serialisation hooks. We still need to
% specify the classname however, as at deserialisation time this information
% will be necessary.
%
% Notes that we do not take the 'request_sender' field into account for
% serialisation, as, by design, there is indeed such a caller (since serialise/3
% is a request), but it is of no interest here/
%
% (const request)
%
-spec serialise( wooper:state(),
				 maybe( wooper_serialisation:entry_transformer() ),
				 basic_utils:user_data() ) -> const_request_return(
	   { wooper_serialisation:bin_serialisation(), basic_utils:user_data() } ).
serialise( State, _MaybeEntryTransformer=undefined, UserData ) ->

	% Here no entry transformer is to be used; just a raw serialisation.

	% Hooks may be defined on a per-class basis:

	PreState = #state_holder{
		attribute_table=AttributeTable,
		actual_class=Classname } = pre_serialise_hook( State ),

	trace_utils:debug_fmt( " - serialising, with no transformer, instance ~p "
						   "of class ~ts", [ self(), Classname ] ),

	% There are, for all Erlang processes, some extra information that are
	% contextual, implicit, like: whether they are linked (and with whom), their
	% process dictionary, whether they trap exits, etc. (see implementation
	% notes); refer to the 'About serialised elements' section of the
	% implementation notes.

	% So, let's add the WOOPER extra information:
	CurrentRandomState = random_utils:get_random_state(),

	RandomAttribute = { wooper_random_state, CurrentRandomState },


	% Retrieving all attribute key/value pairs (sorting is probably a bit
	% cleaner):
	%
	Entries = lists:sort(
		[ RandomAttribute | ?wooper_table_type:enumerate( AttributeTable )  ] ),

	% By default returns {Classname, Entries}:
	FullContent = post_serialise_hook( Classname, Entries, PreState ),

	SerialisedContent = wooper_serialisation:serialise_term( FullContent ),

	Res = { SerialisedContent, UserData },

	% Yes, the returned state is 'State', as we do not want to continue with any
	% state forged for the serialisation (ex: with transformed local processes),
	% we want to continue as we were before the serialisation!
	%
	wooper:const_return_result( Res );


serialise( State, EntryTransformer, UserData ) ->

	% Here an entry transformer is to be used, for a smarter serialisation (ex:
	% PID-aware).

	% Hooks may be defined on a per-class basis:

	PreState = #state_holder{
		attribute_table=AttributeTable,
		actual_class=Classname } = pre_serialise_hook( State ),

	trace_utils:debug_fmt( " - serialising, with transformer, "
		"instance ~p of class ~ts", [ self(), Classname ] ),

	% There are, for all Erlang processes, some extra information that are
	% contextual, implicit, like: whether they are linked (and with whom), their
	% process dictionary, whether they trap exits, etc. (see implementation
	% notes); refer to the 'About serialised elements' section of the
	% implementation notes.

	% So, let's add the WOOPER extra information:
	CurrentRandomState = random_utils:get_random_state(),

	RandomAttribute = { wooper_random_state, CurrentRandomState },

	% Retrieving all attribute key/value pairs (sorting is probably a bit
	% cleaner):
	%
	Entries = lists:sort(
		[ RandomAttribute | ?wooper_table_type:enumerate( AttributeTable ) ] ),

	%trace_utils:debug_fmt( "Original entries:~n ~p", [ Entries ] ),

	% Applying the entry transformer on each of them:
	{ TransformedEntries, FinalUserData } = lists:foldl( EntryTransformer,
		_Acc0={ _ResultingEntries=[], UserData }, _List=Entries ),

	%trace_utils:debug_fmt( "Transformed entries: ~n~p",
	%                       [ TransformedEntries ] ),

	% No need to reverse the transformed list.

	% By default returns {Classname, TransformedEntries}:
	FullContent =
		post_serialise_hook( Classname, TransformedEntries, PreState ),

	SerialisedContent = wooper_serialisation:serialise_term( FullContent ),

	Res = { SerialisedContent, FinalUserData },

	% Yes, returning the initial 'State', as we do not want to continue with any
	% state forged for the serialisation (ex: with transformed local processes),
	% we want to continue as we were before the serialisation!
	%
	wooper:const_return_result( Res ).




% Hooks section.



% For some classes, the implementor may want to define pre/post hooks for
% serialisation/deserialisation.
%
% In this case the 'wooper_serialisation_hooks' preprocessor flag shall be
% defined, and so the four corresponding hooks.
%
-ifndef(wooper_serialisation_hooks).



% Default do-nothing hooks:


% @doc Hook triggered just before serialisation.
%
% We are here to return a state directly suitable for serialisation, for example
% with no transient technical identifiers (like PID, open files, etc.) - unless
% a later entry transformer is able to manage them.
%
-spec pre_serialise_hook( wooper:state() ) -> wooper:state().
pre_serialise_hook( State ) ->
	State.



% @doc Hook triggered just after the serialisation step having led to selecting
% entries in a term serialisation; last possible transformation before the
% actual storage.
%
% The value returned by this hook will be converted "as is" by the serializer
% (e.g. in a binary), that will be sent to the serialisation sink (e.g. a file).
%
% (we do not want to return a state, as we do not want a state modified by the
% serialisation be mistakenly used afterwards)
%
-spec post_serialise_hook( classname(),
		wooper_serialisation:term_serialisation(), wooper:state() ) -> term().
post_serialise_hook( Classname, Entries, _State ) ->
	{ Classname, Entries }.



% @doc Hook triggered just before deserialisation.
%
% Default version corresponding to post_serialise_hook/3.
%
-spec pre_deserialise_hook( term(), basic_utils:user_data() ) ->
									wooper_serialisation:term_serialisation().
pre_deserialise_hook( _SerialisationTerm={ _Classname, Entries }, _UserData ) ->
	Entries.



% @doc Hook triggered just after deserialisation.
-spec post_deserialise_hook( wooper:state() ) -> wooper:state().
post_deserialise_hook( State ) ->
	State.


-endif. % wooper_serialisation_hooks
