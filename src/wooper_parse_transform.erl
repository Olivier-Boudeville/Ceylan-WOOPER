% Copyright (C) 2014 Olivier Boudeville
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
% Creation date: Wednesday, December 24, 2014




% Overall parse transform for the WOOPER layer.
%
%
-module(wooper_parse_transform).



% Implementation notes:
%
% Calls in turn the common parse transform, once WOOPER-level operations have
% been done.
%
% One will get: 'undefined parse transform 'wooper_parse_transform'' as soon as
% a compiled module called by the parse transform (ex: text_utils.beam) will not
% be found (hence even if the transform itself is available).

-export([ parse_transform/2, transform/1, info_to_string/1 ]).


% Describe a member attribute of the state of a given class:
%
-record( attribute_info, {

		   % The name of an attribute:
		   name :: wooper:attribute_name(),

		   % Type of an attribute:
		   type :: meta_utils:type(),

		   % Qualifiers (if any) that apply to this attribute:
		   qualifiers = [] :: [ wooper:qualifier() ]

} ).


-type attribute_info() :: #attribute_info{}.

-type ast() :: meta_utils:ast().



% The parse transform itself, transforming the specified Abstract Format code
% into another one.
%
-spec parse_transform( ast(), list() ) -> ast().
parse_transform( AST, Options ) ->

	%io:format( "  (applying parse transform '~p')~n", [ ?MODULE ] ),

	%io:format( "Input AST:~n~p~n", [ AST ] ),

	{ WOOPERAST, Info } = transform( AST ),

	io:format( "~s~n", [ info_to_string( Info ) ] ),

	OutputAST = common_parse_transform:parse_transform( WOOPERAST, Options ),

	%io:format( "~n~nOutput AST:~n~p~n", [ OutputAST ] ),

	OutputAST.



% Regarding the WOOPER parse transform.

% All WOOPER-related symbols (ex: atoms, functions, etc.) are to be prefixed by
% 'wooper_'. This prefix shall be considered as reserved for WOOPER internals
% (all wooper_* symbols are forbidden to the user).

% Previously, for simplicity, some values (ex: the superclasses) were defined
% thanks to macro defines (ex: '-define( wooper_foo, 42 )'). Now they are
% specified thanks to attributes -ex: '-wooper_foo( 42 ).' and when there was
% previously ?wooper_foo, we defined a replaced that with the definition of a
% wooper_get_foo() function.


% Regarding WOOPER superclasses.
%
% They used to be defined with:
% '-define( wooper_superclasses, [ class_A, class_B ] ).'.
%
% Now they are defined with the optional:
% '-wooper_superclasses( [ class_A, class_B ] ).



% A record to store information gathered about an AST.
%
-record( class_info, {


		% Name of that class:
		class = undefined :: wooper:class_name(),


		% Ordered list of the superclasses of this class:
		superclasses = undefined :: [ wooper:class_name() ],


		% Parse-level attributes (ex: '-my_attribute( my_value ).'):
		attributes = [] :: [ meta_utils:attribute() ],


		% Include files (typically *.hrl files):
		includes = [] :: [ file_utils:file_name() ],


		% All function exports (including methods):
		function_exports = [] :: [ { meta_utils:function_name(), arity() } ],


		% All type exports:
		type_exports = [] :: [ { meta_utils:type_name(),
								 meta_utils:type_arity() } ],


		% The class-specific attribute definitions:
		class_specific_attributes = [] :: [ attribute_info() ],


		% All inherited attribute definitions for this class:
		inherited_attributes = [] :: [ attribute_info() ],


		% Definitions of the constructors of that class:
		constructor_definitions = [] :: [ meta_utils:function_info() ],


		% Definitions of the destructor (if any) of that class:
		destructor_definition = undefined :: meta_utils:function_info(),


		% Definitions of the member methods of that class:
		member_method_definitions = [] :: [ meta_utils:function_info() ],


		% Definitions of the static methods of that class:
		static_method_definitions = [] :: [ meta_utils:function_info() ],


		% Definitions of the other functions of that class:
		function_definitions = [] :: [ meta_utils:function_info() ],


		% The number of the last line in the original source file:
		last_line :: basic_utils:count()

} ).


-type class_info() :: #class_info{}.



% Transforms specified AST for WOOPER.
%
-spec transform( meta_utils:ast() ) -> { meta_utils:ast(), class_info() }.
transform( AST ) ->

	% Starts with blank information:
	{ Info, RevAST } = get_info( AST ),

	LastLine = Info#class_info.last_line,

	ExpForm = meta_utils:form_to_ast( io_lib:format( "-export([ f/0 ]).", [] ),
									  LastLine + 1 ),

	FunForm = meta_utils:form_to_ast( io_lib:format( "f() -> 4.", [] ),
									  LastLine + 1 ),

	NewRevAST = [ { eof, LastLine + 1 }, FunForm | RevAST ],

	NewAST =  [ ExpForm | lists:reverse( NewRevAST ) ],

	{ NewAST, Info }.






% Returns a reversed AST with no eof tuple, and the class information that were
% gathered.
%
% (here we simply examine the list of the top-level forms - no specific
% recursing)
%

% Class/module section:

% Any invalid or duplicated module declaration will be caught by the compiler
% anyway:
%
get_info( AST ) ->
	get_info( AST, #class_info{}, _Acc=[] ).


get_info( _AST=[ F={ attribute, _Line, module, Module } | T ],
		  W=#class_info{ class=undefined }, Acc ) ->
	check_class_name( Module ),
	get_info( T, W#class_info{ class=Module }, [ F | Acc ] );


% Superclasses section:
%
get_info( _AST=[ F={ attribute, _Line, wooper_superclasses, Classes } | T ],
		  W=#class_info{ superclasses=undefined }, Acc )
  when is_list( Classes ) ->

	[ check_class_name( C ) || C <- Classes ],

	get_info( T, W#class_info{ superclasses=Classes }, [ F | Acc ] );


get_info( _AST=[ { attribute, Line, wooper_superclasses, Classes } | _T ],
		  #class_info{ superclasses=undefined }, _Acc )
  when is_list( Classes ) ->
	meta_utils:raise_error( { invalid_superclasses_definition, { line, Line },
							  Classes } );

get_info( _AST=[ { attribute, Line, wooper_superclasses, Classes } | _T ],
		  #class_info.superclasses=OtherClasses, _Acc ) ->
	meta_utils:raise_error( { multiple_superclasses_definition, { line, Line },
							  Classes,OtherClasses  } );

% Expected to be defined once, and not kept as will be added back later:
get_info( _AST=[ _F={ eof, Line } ], W=#class_info{ last_line=undefined },
		Acc ) ->
	{ W#class_info{ last_line=Line }, Acc };

get_info( _AST=[ H | T ], Infos, Acc ) ->
	get_info( T, Infos, [ H | Acc ] ).

% Useless because of eof:
%get_info( _AST=[], Infos, Acc ) ->
%	{ Infos, Acc }.



%% -spec get_superclasses() -> [ class_name() ].
%% get_superclasses() ->
%%	?wooper_superclasses.


% Ensures that specified name is a legit class name.
%
check_class_name( Name ) ->

	case text_utils:atom_to_string( Name ) of

		"class_" ++ _ ->
			ok;

		InvalidName ->
			meta_utils:raise_error( { invalid_class_name, InvalidName } )

	end.



-spec info_to_string( class_info() ) -> text_utils:ustring().
info_to_string( #class_info{ class=Class, superclasses=Classes,
							 last_line=LastLine } ) ->

	Infos = [ text_utils:format( "superclasses: ~p", [ Classes ] ),
			  text_utils:format( "line count: ~B", [ LastLine ] )
			  ],

	text_utils:format( "~s information:~s", [ Class,
							text_utils:strings_to_string( Infos ) ] ).
