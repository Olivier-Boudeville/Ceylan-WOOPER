% Copyright (C) 2003-2019 Olivier Boudeville
%
% This file is part of the Ceylan-WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
%
-module(class_SimplestClass).


% Determines what are the mother classes of this class (if any):
-define( superclasses, [] ).

-define( class_attributes, [ age, gender ] ).

% Non-method exported functions:
-export([ example_fun/0, toString/1 ]).

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Import common types without module prefix:
-include("ecosystem_types.hrl").



% Constructs a new Creature.
-spec construct( wooper:state(), age(), gender() ) -> wooper:state().
construct( State, Age, Gender ) ->
	% No mother class.
	setAttributes( State, [ { age, Age }, { gender, Gender } ] ).



% This useless destructor overriding was made to silence Dialyzer (which is not
% able to determine that this function will never be called, as WOOPER performs
% the appropriate test is made beforehand):
%
% Allows to test also the automatic destructor generation:
%-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->
	State.



% Method implementations.


% Returns the age of this creature.
-spec getAge( wooper:state() ) -> request_return( age() ).
getAge( State ) ->
	wooper:return_state_result( State, ?getAttr(age) ).



% Sets the age of this creature.
-spec setAge( wooper:state(), age() ) -> oneway_return().
setAge( State, _NewAge ) ->
	% Mother implementation chosen faulty to check override:
	wooper:return_state_only( setAttribute( State, age, 36 ) ).



% Increments the age of this creature.
-spec declareBirthday( wooper:state() ) -> oneway_return().
declareBirthday( State ) ->
	wooper:return_state_only(
		setAttribute( State, age, ?getAttr(age)+1 ) ).



% Returns the gender of this creature.
-spec getGender( wooper:state() ) -> request_return( gender() ).
getGender( State ) ->
	wooper:return_state_result( State, ?getAttr(gender) ).



% Returns a class-specific arbitrary number.
%
% (request)
-spec getArbitraryNumber( wooper:state() ) -> request_return( number() ).
getArbitraryNumber( State ) ->
	wooper:return_state_result( State, 10 ).



% Tests direct (synchronous) self-invocation of methods (oneway).
%
% To be called only from a Mammal instance, as there is an hardcoded
% pattern-matching that should work only for a Mammal.
%
% Must not be called from the Creature test, otherwise will fail.
%
% (oneway).
-spec testDirectMethodExecution( wooper:state(), age() ) -> oneway_return().
testDirectMethodExecution( State, NewAge ) ->

	io:format( "Testing executeOneway.~n" ),

	% Note: the version of setAge called in the context of a Creature sets in on
	% purpose to a fixed value (36), regardless of the specified age, whereas
	% the Mammal version of setAge behaves as expected:
	NewState = executeOneway( State, setAge, NewAge ),

	% Use this instead to test error management:
	%NewState = executeOneway(test_not_a_state,setAge,NewAge),
	%NewState = executeOneway(State,42,NewAge),

	% NewAge is expected to be 347:
	NewAge = getAttribute( NewState, age ),

	io:format( "Testing executeRequest.~n" ),
	% 15 from Mammal child classes, not 10 from here:

	{ OtherState, 15 } = executeRequest( NewState, getArbitraryNumber ,[] ),

	%{ OtherState, 15 } = executeRequest( test_not_a_state, getArbitraryNumber,
	% [] ),

	%{ OtherState, 15 } = executeRequest( NewState, 43, [] ),

	io:format( "Direct self-invocation success.~n" ),

	wooper:return_state_only( OtherState ).



% Allows to test that calling an attribute macro with a state parameter returned
% by a function will trigger that function only once.
%
% Indeed a faulty implementation, due to a macro pitfall, used to make a
% statement like 'setAttribute( f(State), attr, value )' call f/1 twice.
%
% The returned value of the setAttribute call was correct, but any side-effect
% triggered by f (sending a message, writing a trace, etc.) happened twice.
%
% (oneway)
-spec testSingleExecution( wooper:state() ) -> oneway_return().
testSingleExecution( State ) ->
	wooper:return_state_only( setAttribute( side_effect_function( State ),
		age, 10 ) ).



-spec side_effect_function( wooper:state() ) -> wooper:state().
side_effect_function( State ) ->
	io:format( "~n### This message must not be displayed more than once.~n" ),
	State.





% Helper function.


% Just to show it can exist:
-spec example_fun() -> 'ok'.
example_fun() ->
	ok.


% This looks like a method, but it is not (returning only a string):
%
% (function)
-spec toString( wooper:state() ) -> string().
toString( State ) ->
	table:toString( State#state_holder.attribute_table ).
