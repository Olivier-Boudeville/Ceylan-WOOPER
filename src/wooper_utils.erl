% Copyright (C) 2017-2018 Olivier Boudeville
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



% Section related to the possible use of Java.
%
% (see also: java_utils, in Common)
%
-export([ java_class_to_wooper_class/1, wooper_class_to_java_class/1,
		  get_java_package_and_class_for/1 ]).




% Deduces the Erlang equivalent name, according to the WOOPER conventions, of a
% class that is actually implemented in Python and whose name follows the PEP8
% convention.
%
% Ex: 'MyFoobarExample' resulting in 'class_MyFoobarExample'.
%
-spec pep8_class_to_wooper_class( python_utils:pep8_class_name() | string() )
								-> wooper:classname().
pep8_class_to_wooper_class( ClassName ) when is_atom( ClassName ) ->
	pep8_class_to_wooper_class( text_utils:atom_to_string( ClassName ) );

pep8_class_to_wooper_class( ClassNameStr ) ->
	text_utils:string_to_atom( "class_" ++ ClassNameStr ).



% Deduces the Python equivalent name, according to the PEP8 convention, of an
% Erlang class whose name follows the WOOPER conventions.
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





% Deduces the Erlang equivalent name, according to the WOOPER conventions, of a
% class that is actually implemented in Java.
%
% Ex: 'MyFoobarExample' resulting in 'class_MyFoobarExample'.
%
-spec java_class_to_wooper_class( java_utils:java_class_name() | string() )
								-> wooper:classname().
java_class_to_wooper_class( ClassName ) when is_atom( ClassName ) ->
	java_class_to_wooper_class( text_utils:atom_to_string( ClassName ) );

java_class_to_wooper_class( ClassNameStr ) ->
	text_utils:string_to_atom( "class_" ++ ClassNameStr ).



% Deduces the Java equivalent name of an Erlang class whose name follows the
% WOOPER conventions.
%
% Ex: "class_MyFoobarExample" resulting in "MyFoobarExample".
%
-spec wooper_class_to_java_class( wooper:classname() ) ->
										java_utils:java_string_class_name().
wooper_class_to_java_class( ClassName ) when is_atom( ClassName ) ->

	ClassNameString = text_utils:atom_to_string( ClassName ),

	case text_utils:split_after_prefix( "class_", ClassNameString ) of

		no_prefix ->
			throw( { invalid_wooper_classname, ClassName } );

		JavaClassName ->
			JavaClassName

	end.




% Returns the Java package and class that correspond to the specified WOOPER
% classname.
%
% So for example a WOOPER classname equal to
% 'class_BigPackage__MyPackage__MyExample' is to be translated into:
% { 'bigpackage.mypackage', 'MyExample' }.
%
-spec get_java_package_and_class_for( wooper:classname() ) ->
		{ java_utils:java_package_name(), java_utils:java_class_name() }.
get_java_package_and_class_for( WOOPERClassname ) ->

	% For instance, let's suppose WOOPERClassname is
	% 'class_BigPackage__MyPackage__MyExample'.

	% Then JavaPackageAndClass = "BigPackage__MyPackage__MyExample":
	JavaPackageAndClass = wooper_class_to_java_class( WOOPERClassname ),

	% [ "BigPackage", "MyPackage", "MyExample" ]:
	SplitElems = string:split( JavaPackageAndClass, _Pattern="__",
							   _Where=all ),

	{ JavaClassElem, JavaPackageElems } = list_utils:extract_last_element(
											SplitElems ),

	% Then JavaClass is 'MyExample':
	JavaClass = text_utils:string_to_atom( JavaClassElem ),

	% And JavaPackage becomes, based on [ "BigPackage", "MyPackage" ],
	% 'bigpackage.mypackage':
	%
	JavaPackage = text_utils:string_to_atom( text_utils:join( _Sep=".",
			   [ text_utils:to_lowercase( E ) || E <- JavaPackageElems ] ) ),

	{ JavaPackage, JavaClass }.
