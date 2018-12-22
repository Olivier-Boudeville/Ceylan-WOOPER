% Copyright (C) 2003-2018 Olivier Boudeville
%
% This file is part of the Ceylan-WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]


-module(class_Reptile).


% Determines what are the mother classes of this class (if any):
-superclasses([ class_Creature ]).


% With this class, we will test serialisation hooks:
-define(wooper_serialisation_hooks,).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").



% Triggered just before serialisation.
%
-spec pre_serialise_hook( wooper:state() ) -> wooper:state().
pre_serialise_hook( State ) ->
	io:format( "Pre-serialising a reptile!~n" ),
	State.



% Triggered just after serialisation.
%
% (using WOOPER default hook implementation augmented of an io:format)
%
-spec post_serialise_hook( classname(),
						   wooper_serialisation:term_serialisation(),
						   wooper:state() ) -> term().
post_serialise_hook( Classname, Entries, _State ) ->
	io:format( "Post-serialising a reptile!~n" ),
	{ Classname, Entries }.



% Triggered just before deserialisation.
%
-spec pre_deserialise_hook( term(), basic_utils:user_data() ) ->
							wooper_serialisation:term_serialisation().
pre_deserialise_hook( _SerialisedEntries={ _Classname, Entries }, _UserData ) ->
	io:format( "Pre-deserialising a reptile!~n" ),
	Entries.



% Triggered just after deserialisation.
%
-spec post_deserialise_hook( wooper:state() ) -> wooper:state().
post_deserialise_hook( State ) ->
	io:format( "Post-deserialising a reptile!~n" ),
	State.



% Import common types without module prefix:
-include("ecosystem_types.hrl").


-attributes([]).


% Constructs a new Reptile.
-spec construct( wooper:state(), age(), gender() ) -> wooper:state().
construct( State, Age, Gender ) ->
	class_Creature:construct( State, Age, Gender ).
	% To test constructor checking:
	%an_unexpected_initial_state.


% Overridden destructor
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->
	io:format( "Deleting a Reptile." ),
	State.
	% To test destructor checking use instead:
	%an_unexpected_final_state.



% Method implementations.


% Sets correctly the age of this Mammal (not like faulty implementation of the
% Creature mother class).
%
% Overridden from Creature, useful to show the use of executeOneway.
% Note: used to test WOOPER management of error conditions.
%
% (oneway)
%
-spec setAge( wooper:state(), age() ) -> oneway_return().
setAge( State, NewAge ) ->
	%throw( exception_throw_test_from_oneway ),
	%exit( exception_exit_test_from_oneway ),
	wooper:return_state_only( setAttribute( State, age, NewAge ) ).



% All reptiles are cold-blooded.
%
% Note: used to test WOOPER management of error conditions.
%
% (const request)
%
-spec isHotBlooded( wooper:state() ) -> request_return( boolean() ).
isHotBlooded( State ) ->
	%throw( exception_throw_test_from_request ),
	%exit( exception_exit_test_from_request ),
	wooper:return_state_result( State, false ).


% All reptiles can moult:
%
% (const request)
%
-spec canMoult( wooper:state() ) -> request_return( boolean() ).
canMoult( State ) ->
	wooper:return_state_result( State, true ).
