% Copyright (C) 2003-2019 Olivier Boudeville
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

% Modular WOOPER header gathering elements used to define a class.


% We define here functions whose only purpose is to make available a
% preprocessor define (like in: '-define(superclasses,[A,B]).') to the WOOPER
% parse transform:
%
% - a define is invisible to it as long it has been specified yet not used
%
% - the function there will be discarded, once it will have allowed the parse
% transform to determine said define (ex: [A,B] here)


-ifdef(superclasses).

% Not even needed: -export([ wooper_get_superclasses/0 ]).

% Reuse that define, since it has been specified:
%
% Note: if your compiler points to these lines, you must have introduced a parse
% (syntax) error in your 'superclasses' define.
%
wooper_get_superclasses() ->
	?superclasses.


% We prefer defining such an helper pseudo-function iff a define has been
% specified:

%% -else. % superclasses

%% % Specifying nothing means no superclass:
%% wooper_get_superclasses() ->
%%	[].

-endif. % superclasses



-ifdef(class_attributes).

% Not even needed: -export([ wooper_get_class_attributes/0 ]).


% Reuse that define, since it has been specified:
wooper_get_class_attributes() ->

	% Note: if your compiler points to these lines, you must have introduced a
	% parse (syntax) error in your 'class_attributes' define.
	%
	?class_attributes.


% We prefer defining such an helper pseudo-function iff a define has been
% specified:

%% -else. % class_attributes

%% % Specifying nothing means no superclass:
%% wooper_get_class_attributes() ->
%%	[].

-endif. % class_attributes
