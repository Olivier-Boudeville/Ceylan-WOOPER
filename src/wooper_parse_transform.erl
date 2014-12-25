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

-export([ parse_transform/2, transform/1, generate_ast/1,
		  class_info_to_string/1 ]).


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

	io:format( "  (applying parse transform '~p')~n", [ ?MODULE ] ),

	%io:format( "Input AST:~n~p~n", [ AST ] ),

	{ WOOPERAST, _ClassInfo } = transform( AST ),

	%io:format( "~s~n", [ class_info_to_string( ClassInfo ) ] ),

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



% A record to store and centralise information gathered about an AST.
%
% Allows to perform checkings and to reorder the returned version of it.
%
% We preserve the forms whenever possible, for example to preserver line
% numbers.
%
-record( class_info, {


		% Name of that class:
		class = undefined :: wooper:class_name(),


		% Module definition:
		module_definition = undefined :: ast(),


		% Ordered list of the superclasses of this class:
		superclasses = undefined :: [ wooper:class_name() ],


		% Parse-level attributes (ex: '-my_attribute( my_value ).'):
		parse_attributes = [] :: [ meta_utils:attribute() ],

		% Parse attribute definitions:
		parse_attribute_defs = [] :: [ ast() ],


		% Include files (typically *.hrl files):
		%
		% (expected to remain empty, as the preprocessor is supposed to have
		% already been run)
		%
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
	ClassInfo = get_class_info( AST ),

	%io:format( "~n~s~n", [ class_info_to_string( ClassInfo ) ] ),

	%LastLine = Info#class_info.last_line,

	%ExpForm = meta_utils:form_to_ast( io_lib:format( "-export([ f/0 ]).", [] ),
	%								  LastLine + 1 ),

	%FunForm = meta_utils:form_to_ast( io_lib:format( "f() -> 4.", [] ),
	%								  LastLine + 1 ),

	%NewRevAST = [ { eof, LastLine + 1 }, FunForm | RevAST ],

	%ExpForm = undefined,

	%NewAST =  [ ExpForm | lists:reverse( RevAST ) ],

	%NewAST = generate_ast( Info ),

	NewAST = AST,

	{ NewAST, ClassInfo }.







% Returns the class information that were gathered.
%
% (here we simply examine the list of the top-level forms - no specific
% recursing)
%
get_class_info( AST ) ->
	get_info( AST, #class_info{} ).



% Class/module section:

% Any invalid or duplicated module declaration will be caught by the compiler
% anyway:
%
get_info( _AST=[ F={ attribute, _Line, module, Module } | T ],
		  W=#class_info{ class=undefined, module_definition=undefined } ) ->

	check_class_name( Module ),

	get_info( T, W#class_info{ class=Module, module_definition=F } );


% Superclasses section:
%
get_info( _AST=[ F={ attribute, _Line, wooper_superclasses, Classes } | T ],
		  W=#class_info{ superclasses=undefined,
						 parse_attributes=Attributes,
						 parse_attribute_defs=AttributeDefs } )
  when is_list( Classes ) ->

	[ check_class_name( C ) || C <- Classes ],

	get_info( T, W#class_info{
				   superclasses=Classes,
				   % We record this attribute as well:
				   parse_attributes=[ { wooper_superclasses, Classes }
									  | Attributes ],
				   parse_attribute_defs=[ F | AttributeDefs ] } );


get_info( _AST=[ { attribute, Line, wooper_superclasses, Classes } | _T ],
		  #class_info{ superclasses=undefined } ) when is_list( Classes ) ->
	meta_utils:raise_error( { invalid_superclasses_definition, { line, Line },
							  Classes } );

get_info( _AST=[ { attribute, Line, wooper_superclasses, Classes } | _T ],
		  #class_info.superclasses=OtherClasses ) ->
	meta_utils:raise_error( { multiple_superclasses_definition, { line, Line },
							  Classes,OtherClasses  } );


% Spec attributes:
%get_info( _AST=[ F={ attribute, _Line, spec, _FunSpec } | T ],
%		  W=#class_info{ parse_attributes=Attributes,
%						 parse_attribute_defs=AttributeDefs } ) ->
	
% Other non-WOOPER attribute section:
%
get_info( _AST=[ F={ attribute, _Line, AttributeName, AttributeValue } | T ],
		  W=#class_info{ parse_attributes=Attributes,
						 parse_attribute_defs=AttributeDefs } ) ->

	get_info( T, W#class_info{
				   parse_attributes=[ { AttributeName, AttributeValue }
									  | Attributes ],
				   parse_attribute_defs=[ F | AttributeDefs ] } );


% Expected to be defined once, and not kept as will be added back later:
get_info( _AST=[ _F={ eof, Line } ], W=#class_info{ last_line=undefined } ) ->
	W#class_info{ last_line=Line };

get_info( _AST=[ _H | T ], Infos ) ->
	%io:format( "WARNING: ~p not managed.~n", [ H ] ),
	%meta_utils:raise_error( { unhandled_form, H } ),
	get_info( T, Infos ).

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


% Returns a full AST from specified class information.
%
generate_ast( #class_info{

				 %class=Class,
				 module_definition=ModuleDef,
				 %superclasses=Superclasses,
				 %parse_attributes=ParseAttributes,
				 parse_attribute_defs=_ParseAttributeDefs,
				 %includes=Includes,
				 function_exports=_FunctionExports,
				 type_exports=_TypeExports,
				 class_specific_attributes=_ClassSpecificAttributes,
				 inherited_attributes=_InheritedAttributes,
				 constructor_definitions=_ConstructorDefs,
				 destructor_definition=_DestructorDef,
				 member_method_definitions=_MemberMethodDefs,
				 static_method_definitions=_StaticMethodDefs,
				 function_definitions=_FunctionDefs,
				 last_line=LastLine } ) ->

	% Let's start by writing the module declaration; nothing found in erl_syntax
	% for that, but rather than doing it by hand we can reuse it directly:
	ModuleForm = [ ModuleDef ],


	NewLastLine = LastLine,

	FinalForm = [ NewLastLine | ModuleForm ],

	lists:reverse( FinalForm ).






-spec class_info_to_string( class_info() ) -> text_utils:ustring().
class_info_to_string( #class_info{ class=Class,
							 module_definition=ModuleDef,
							 superclasses=Classes,
							 parse_attributes=_ParseAttributes,
							 parse_attribute_defs=ParseAttributeDefs,
							 includes=Includes,
							 function_exports=FunctionExports,
							 type_exports=TypeExports,
							 class_specific_attributes=ClassAttributes,
							 inherited_attributes=InheritedAttributes,
							 constructor_definitions=ConstructorDefs,
							 destructor_definition=DestructorDef,
							 member_method_definitions=MemberMethodDefs,
							 static_method_definitions=StaticMethodDefs,
							 function_definitions=FunctionDefs,
							 last_line=LastLine
						   } ) ->

	Infos = [

			  text_utils:format( "module definition: ~p", [ ModuleDef ] ),
			  text_utils:format( "superclasses: ~p", [ Classes ] ),
			  %text_utils:format( "parse attributes: ~p", [ ParseAttributes ] ),
			  text_utils:format( "parse attribute definitions: ~p",
								 [ ParseAttributeDefs ] ),
			  text_utils:format( "includes: ~p", [ Includes ] ),
			  text_utils:format( "function exports: ~p", [ FunctionExports ] ),
			  text_utils:format( "type exports: ~p", [ TypeExports ] ),
			  text_utils:format( "class-specific attributes: ~p",
								 [ ClassAttributes ] ),
			  text_utils:format( "inherited attributes: ~p",
								 [ InheritedAttributes ] ),
			  text_utils:format( "constructors: ~p", [ ConstructorDefs ] ),
			  text_utils:format( "destructor: ~p", [ DestructorDef ] ),
			  text_utils:format( "member methods: ~p", [ MemberMethodDefs ] ),
			  text_utils:format( "static methods: ~p", [ StaticMethodDefs ] ),
			  text_utils:format( "functions: ~p", [ FunctionDefs ] ),
			  text_utils:format( "line count: ~B", [ LastLine ] )
			  ],

	text_utils:format( "Information about class '~s':~s", [ Class,
								   text_utils:strings_to_string( Infos ) ] ).
