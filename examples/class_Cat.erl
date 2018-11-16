% Copyright (C) 2003-2018 Olivier Boudeville
%
% This file is part of the Ceylan-WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]


% Cat-based example.
%
% Guaranteed to be implemented by a cat.
%
-module(class_Cat).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Mammal, class_ViviparousBeing ] ).


% Member method declarations.
-define( wooper_method_export, getTeatCount/1, canEat/2, getWhiskerColor/1,
		 terminate/2, toString/1 ).


% Static method declarations.
-define( wooper_static_method_export, get_mean_life_expectancy/0 ).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Import common types without module prefix:
-include("ecosystem_types.hrl").



% Constructs a new Cat.
-spec construct( wooper:state(), age(), gender(), fur_color(), whisker_color() )
			   -> wooper:state().
construct( State, Age, Gender, FurColor, WhiskerColor ) ->

	% First the direct mother classes:
	MammalState = class_Mammal:construct( State, Age, Gender, FurColor ),
	ViviparousMammalState = class_ViviparousBeing:construct( MammalState ),

	% Then the class-specific attributes:
	setAttribute( ViviparousMammalState, whisker_color, WhiskerColor ).



-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	io:format( "Deleting cat ~w! (overridden destructor)~n", [ self() ] ),

	% To test:
	% basic_utils:crash(),

	State.



% No guarantee on biological fidelity:
-spec getTeatCount( wooper:state() ) -> request_return( teat_count() ).
getTeatCount( State ) ->
	?wooper_return_state_result( State, 6 ).


% Cats are supposed carnivorous though:
-spec canEat( wooper:state(), food() ) -> request_return( boolean() ).
canEat( State, soup ) ->
	?wooper_return_state_result( State, true );

canEat( State, chocolate ) ->
	?wooper_return_state_result( State, true );

canEat( State, croquette ) ->
	?wooper_return_state_result( State, true );

canEat( State, meat ) ->
	?wooper_return_state_result( State, true );

canEat( State, _OtherFood ) ->
	?wooper_return_state_result( State, false ).


% Const request:
-spec getWhiskerColor( wooper:state() ) -> request_return( color() ).
getWhiskerColor( State )->

	io:format( "getWhiskerColor/1 request called by ~w.~n", [ ?getSender() ] ),

	?wooper_return_state_result( State, ?getAttr(whisker_color) ).



% Requests this cat to terminate, based on specified halting procedure.
%
% (oneway)
%
-spec terminate( wooper:state(), 'crash' ) -> no_return().
terminate( State, crash ) ->

	basic_utils:crash(),

	?wooper_return_state_only( State ).



-spec toString( wooper:state() ) -> request_return( string() ).
toString( State ) ->

	% Would be available only on debug mode:
	%Description = wooper:instance_to_string( State ),

	Description = text_utils:format( "cat instance with whiskers of color ~p.",
									 [ ?getAttr(whisker_color) ] ),

	?wooper_return_state_result( State, Description ).



% Static section.


% Returns the mean life expectancy of a cat, in years.
% (static)
-spec get_mean_life_expectancy() -> age().
get_mean_life_expectancy() ->
	18.
