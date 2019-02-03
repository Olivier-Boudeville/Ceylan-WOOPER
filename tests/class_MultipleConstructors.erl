% Copyright (C) 2003-2019 Olivier Boudeville
%
% This file is part of the Ceylan-WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]


% Allows testing the support of multiple constructors: three different ones are
% defined, none exported.
%
-module(class_MultipleConstructors).


% Determines what are the mother classes of this class (if any):
-define( superclasses, [] ).

-define( class_attributes, [ name, gender ] ).

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


-type name() :: text_utils:ustring().
-type gender() :: atom().


% Constructs a new instance from two construction parameters.
%
-spec construct( wooper:state(), name(), gender() ) -> wooper:state().
construct( State, Name, Gender ) ->
	% No mother class.
	setAttributes( State, [ { name, Name }, { gender, Gender } ] ).



% Constructs a new instance from a single construction parameter.
%
% Of course multiple clauses may exist:
%
-spec construct( wooper:state(), name() ) -> wooper:state().
construct( State, Name="Murdock" ) ->
	% No mother class.
	setAttributes( State, [ { name, Name }, { gender, undefined } ] );

construct( State, Name ) ->
	% No mother class.
	setAttributes( State, [ { name, Name }, { gender, unknown } ] ).


% Simplest possible signature:
-spec construct( wooper:state() ) -> wooper:state().
construct( State ) ->
	% No mother class.
	setAttributes( State, [ { name, "Terry" }, { gender, unknown } ] ).


% Overriding the default destructor:
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->
	io:format( "  I am ~s, and I am just destructed.~n", [ ?getAttr(name) ] ),
	State.



% Method implementations.


% Returns the name of this instance.
%
% (const request)
%

-spec getName( wooper:state() ) -> const_request_return( name() ).
getName( State ) ->
	wooper:const_return_result( ?getAttr(name) ).


% Returns the gender of this instance.
%
% (const request; hence wooper:const_return_result/1 should have been used)
%
-spec getGender( wooper:state() ) -> const_request_return( gender() ).
getGender( State ) ->
	wooper:const_return_result( ?getAttr(gender) ).
