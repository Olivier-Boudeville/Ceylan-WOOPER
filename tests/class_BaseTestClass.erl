% Copyright (C) 2014-2018 Olivier Boudeville
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
% Creation date: Wednesday, December 24, 2014
%
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


% No specific destruct/1.


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


% Returns some mean count.
%
% (static)
%
get_some_mean_count() ->
	wooper:return_static( 6 ).
