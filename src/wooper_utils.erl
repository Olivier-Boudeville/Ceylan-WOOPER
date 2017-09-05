% Copyright (C) 2017-2017 Olivier Boudeville
%
% This file is part of the WOOPER library.
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
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)


% Module containing some extra facilities for WOOPER users.
%
-module(wooper_utils).


% Section related to the possible use of Python.
%
% (see also: python_utils, in Common)
%
-export([ pep8_class_to_wooper_class/1, wooper_class_to_pep8_class/1 ]).


% Deduces the Erlang equivalent name, according to the WOOPER conventions, of a
% class that is actually implemented in Python and which name follows the PEP 8
% convention.
%
% Ex: 'MyFoobarExample' resulting in 'class_MyFoobarExample'.
%
-spec pep8_class_to_wooper_class( python_utils:pep8_class_name() | string() )
								-> wooper:classname().
pep8_class_to_wooper_class( ClassName ) when is_atom( ClassName ) ->
	pep8_class_to_wooper_class( text_utils:atom_to_string( ClassName ) );

pep8_class_to_wooper_class( ClassNameStr ) ->
	%io:format("~n ~n called pep8_class_to_wooper_class ~s ~n ~n ~n", [ ClassNameStr ] ),

	text_utils:string_to_atom( "class_" ++ ClassNameStr ).



% Deduces the Python equivalent name, according to the PEP 8 convention, of an
% Erlang class which name follows the WOOPER conventions.
%
% Ex: "class_MyFoobarExample" resulting in "MyFoobarExample".
%
-spec wooper_class_to_pep8_class( wooper:classname() | string() ) ->
										python_utils:pep8_class_name().
wooper_class_to_pep8_class( ClassName ) when is_atom( ClassName ) ->
	wooper_class_to_pep8_class( text_utils:atom_to_string( ClassName ) );

wooper_class_to_pep8_class( ClassNameString ) ->

	case text_utils:split_after_prefix( "class_", ClassNameString ) of

		no_prefix ->
			throw( { invalid_wooper_classname, ClassNameString } );

		PythonClassName ->
			text_utils:string_to_atom( PythonClassName )

	end.
