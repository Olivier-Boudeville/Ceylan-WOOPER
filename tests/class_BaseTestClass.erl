% Copyright (C) 2003-2018 Olivier Boudeville
%
% This file is part of the Ceylan-WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]


-module(class_BaseTestClass).


% Determines what are the mother classes of this class (if any):
-superclasses([]).

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


-type name() :: text_utils:ustring().
-type gender() :: maybe( 'male' | 'female' ).


% Class-specific attributes:
-attributes([ { name, 'name()', [ const, protected ], "Name of this creature" },
			  { gender, 'gender()', "Gender of this creature" },
			  { age, 'integer()', { initial, 0 },
				"The current age of this creature" } ]).


% Constructs a new instance.
%
-spec construct( wooper:state(), name(), gender() ) -> wooper:state().
construct( State, Name, Gender ) ->
	% No mother class.
	setAttributes( State, [ { name, Name }, { gender, Gender } ] ).


% This useless destructor overriding was made to silence Dialyzer (which is not
% able to determine that this function will never be called, as WOOPER performs
% the appropriate test is made beforehand):
%
% Allows to test also the automatic destructor generation:
%-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->
	State.



% Method implementations.

% One method of each nature, with or without a spec.


% Returns the name of this creature.
%
% (const request)
%
-spec getName( wooper:state() ) -> request_return( name() ).
getName( State ) ->
	Name = ?getAttr(name),
	wooper:return_state_result( State, Name ).


% Sets the name of this creature.
%
% (oneway)
%
setName( State, Name ) ->
	NewState = setAttribute( State, name, Name ),
	wooper:return_state_only( NewState ).


% Returns a mean count.
%
% (static)
%
get_some_mean_count() ->
	wooper:return_static( 6 ).
