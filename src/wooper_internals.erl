% Copyright (C) 2018-2018 Olivier Boudeville
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
% Creation date: Friday, April 13, 2018
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]



% Gathering of internal helpers.
%
-module(wooper_internals).


-export([ raise_error/1, raise_error/2 ]).


% To better report errors:
-define( origin_layer, "WOOPER" ).


% Raises a (compile-time, rather ad hoc) error when applying this parse
% transform, to stop the build on failure and report the actual error.
%
-spec raise_error( term() ) -> no_return().
raise_error( ErrorTerm ) ->
	raise_error( ErrorTerm, _Context=undefined ).



% Raises a (compile-time, rather ad hoc) error, with specified context, when
% applying this parse transform, to stop the build on failure and report the
% actual error.
%
-spec raise_error( term(), ast_base:form_context() ) -> no_return().
raise_error( ErrorTerm, Context ) ->
	ast_utils:raise_error( ErrorTerm, Context, ?origin_layer ).
