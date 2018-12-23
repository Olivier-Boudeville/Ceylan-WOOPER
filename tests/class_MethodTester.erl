% Copyright (C) 2003-2018 Olivier Boudeville
%
% This file is part of the Ceylan-WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]


% Allows testing the support of methods.
%
-module(class_MethodTester).


% Determines what are the mother classes of this class (if any):
-superclasses([]).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


-type name() :: text_utils:ustring().

-attributes([ name ]).


% Simplest possible signature:
-spec construct( wooper:state() ) -> wooper:state().
construct( State ) ->

	trace_utils:trace( "construction" ),

	% No mother class.
	setAttribute( State, name, "Terry" ).



-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	trace_utils:trace( "destruction" ),

	io:format( "  I am ~s, and I am just destructed.~n", [ ?getAttr(name) ] ),
	State.



% Method implementations.


% Returns the name of this instance.
%
% (const request)
%
-spec getName( wooper:state() ) -> request_return( name() ).
getName( State ) ->

	trace_utils:trace( "getName/1" ),

	wooper:return_state_result( State, ?getAttr(name) ).


% Sets the name of this instance.
%
% (oneway)
%
-spec setName( wooper:state(), name() ) -> oneway_return().
setName( State, Name ) ->

	trace_utils:trace( "setName/2" ),

	NewState = setAttribute( State, name, Name ),
	wooper:return_state_only( NewState ).


% Returns a value established in a static context.
%
-spec get_static_info( integer(), integer() ) -> integer().
get_static_info( A, B ) ->

	trace_utils:trace( "get_static_info/2" ),

	wooper:return_static( A + B + 10 ).
