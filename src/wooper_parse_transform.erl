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

% Calls in turn the common parse transform, once WOOPER-level operations have
% been done.
%
% One will get: 'undefined parse transform 'wooper_parse_transform'' as soon as
% a compiled module called by the parse transform (ex: text_utils.beam) will not
% be found (hence even if the transform itself is available) or a non-exported
% (or even not existing) function is called (ex: text_utils:format/1).

% We must discriminate between methods and functions, and identify among methods
% the requests, the oneways and the static ones.
%
% For that we can rely either on the type specs (if any - but we decided that
% they should better be mandatory) or on the function definition itself (relying
% then on the wooper return primitives).
%
% More precisely, both for the type spec and the actual code (all clauses):
%
% - a request shall return its state and value thanks to wooper:request_return/2
%
% - a oneway shall return its state thanks to wooper:oneway_return/1
%
% - a static method (as opposed to the previous two member methods) shall return
% this value thanks to wooper:static_return/1



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


% AST subpart of a clause of a function:
%
-type clause() :: any().



% The WOOPER type of an Erlang function:
%
-type function_type() :: 'constructor' | 'destructor'
					   | 'request' | 'oneway' | 'static' | 'function'.



% For function_info:
-include("meta_utils.hrl").


% For debugging:
-export([ get_info/2, get_class_info/1, check_class_info/1 ]).


% The parse transform itself, transforming the specified Abstract Format code
% into another one.
%
-spec parse_transform( ast(), list() ) -> ast().
parse_transform( AST, Options ) ->

	io:format( "  (applying parse transform '~p')~n", [ ?MODULE ] ),

	%io:format( "Input AST:~n~p~n", [ AST ] ),

	{ WOOPERAST, ClassInfo } = transform( AST ),

	io:format( "~s~n", [ class_info_to_string( ClassInfo ) ] ),

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
% We preserve the forms whenever possible (in *_def* counterpart fields),
% notably to keep line numbers.
%
-record( class_info, {


		% Name of that class:
		class = undefined :: wooper:class_name(),

		% Classname (module) definition:
		class_def = undefined :: ast(),


		% Ordered list of the superclasses of this class (corresponding form
		% will be stripped):
		%
		superclasses = undefined :: [ wooper:class_name() ],

		% Superclass definition (one attribute):
		%
		superclass_def = undefined :: ast(),


		% We merely touch compilation options (ex: '{compile, { inline, [ {
		% FunName, Arity } ] } }'):
		%
		compilation_option_defs = [] :: [ ast() ],


		% Parse-level attributes (ex: '-my_attribute( my_value ).') not
		% corresponding to other fields of interest:
		%
		parse_attributes = [] :: [ meta_utils:attribute() ],

		% Parse attribute definitions:
		%
		parse_attribute_defs = [] :: [ ast() ],


		% Include files (typically *.hrl files, but also includes the .erl
		% source module):
		%
		% (expected to remain empty, as the preprocessor is supposed to have
		% already been run)
		%
		includes = [] :: [ file_utils:file_name() ],

		% Include definitions:
		%
		include_defs = [] :: [ ast() ],


		% Type definitions:
		%
		% (few information gathered)
		%
		type_definitions = [] :: [ { meta_utils:type_name(),
									 meta_utils:type_arity() } ],


		% The abstract forms corresponding to type definitions:
		%
		type_definition_defs = [] :: [ ast() ],


		% All type exports:
		type_exports = [] :: [ { meta_utils:type_name(),
								 meta_utils:type_arity() } ],

		% Type export definitions:
		type_export_defs = [] :: [ ast() ],


		% Whether a function (possibly any kind of method) is exported is
		% recorded primarily in its respective function_info record through a
		% boolean, while the forms for the exports of all functions (including
		% methods) are recorded here (better that way, as an export attribute
		% may define any number of exports and we want to record its line):
		%
		function_exports = [] :: [ ast() ],


		% The class-specific attribute definitions (AST forms stripped, hence
		% not kept):
		%
		class_specific_attributes = [] :: [ attribute_info() ],


		% All inherited attribute definitions for this class:
		%
		inherited_attributes = [] :: [ attribute_info() ],


		% All information about the constructor(s) of that class:
		%
		constructors :: table:table( arity(), meta_utils:function_info() ),


		% All information about the destructor (if any) of that class:
		%
		destructor = undefined :: meta_utils:function_info(),


		% All information about the class-specific (member) request methods of
		% that class:
		%
		requests = table:table( meta_utils:function_id(),
								meta_utils:function_info() ),


		% All information about the class-specific (member) oneway methods of
		% that class:
		%
		oneways = table:table( meta_utils:function_id(),
							   meta_utils:function_info() ),


		% All information about the static methods of that class:
		%
		statics = table:table( meta_utils:function_id(),
							   meta_utils:function_info() ),



		% All information about the other, plain functions of that class:
		%
		functions = table:table( meta_utils:function_id(),
								 meta_utils:function_info() ),


		% The number of the last line in the original source file:
		%
		% (added code will be put afterwards)
		%
		last_line :: basic_utils:count()

} ).


-type class_info() :: #class_info{}.



% Transforms specified AST for WOOPER.
%
-spec transform( meta_utils:ast() ) -> { meta_utils:ast(), class_info() }.
transform( AST ) ->

	% Starts with blank information:
	ClassInfo = get_class_info( AST ),
	%ClassInfo = ok,

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

	InitClassInfo = #class_info{

					   constructors = table:new(),
					   destructor = undefined,
					   requests = table:new(),
					   oneways = table:new(),
					   statics = table:new(),
					   functions = table:new()

					  },

	ReadClassInfo = get_info( AST, InitClassInfo ),

	check_class_info( ReadClassInfo ),

	ReadClassInfo.




% Class/module section:

% Any invalid or duplicated module declaration will be caught by the compiler
% anyway:
%
% (we wanted to tell the users to use -wooper_classname(my_name) instead of
% -module(my_name) but apparently -module must be found before the
% parse-transform is triggered, so that the preprocessor can rely on the ?MODULE
% macro, otherwise the input AST contains:
% '{error,{L,epp,{undefined,'MODULE',none}}}'). So we finally stick to -module.
%
%get_info( _AST=[ { attribute, Line, wooper_classname, Classname } | T ],
get_info( _AST=[ F={ attribute, _Line, module, Classname } | T ],
		  W=#class_info{ class=undefined, class_def=undefined } ) ->

	check_class_name( Classname ),

	% Transforms that in a standard module definition:
	%NewDef = { attribute, Line, module, Classname },

	get_info( T, W#class_info{ class=Classname, class_def=F } );


% Superclasses section:
%
get_info( _AST=[ F={ attribute, _Line, wooper_superclasses, Classes } | T ],
		  W=#class_info{ superclasses=undefined,
						 superclass_def=undefined } ) when is_list( Classes ) ->

	[ check_class_name( C ) || C <- Classes ],

	get_info( T, W#class_info{
				   superclasses=Classes,
				   superclass_def=F } );


get_info( _AST=[ { attribute, Line, wooper_superclasses, Classes } | _T ],
		  #class_info{ superclasses=undefined } ) when is_list( Classes ) ->
	meta_utils:raise_error( { invalid_superclasses_definition, { line, Line },
							  Classes } );

get_info( _AST=[ { attribute, Line, wooper_superclasses, Classes } | _T ],
		  #class_info.superclasses=OtherClasses ) ->
	meta_utils:raise_error( { multiple_superclasses_definition, { line, Line },
							  Classes,OtherClasses  } );


% Compilation options section:
%
get_info( _AST=[ F={ attribute, _Line, compile, _Options } | T ],
		  W=#class_info{ compilation_option_defs=Opts } ) ->

	get_info( T, W#class_info{ compilation_option_defs=[ F | Opts ] } );


% Include section:
%
get_info( _AST=[ F={ attribute, _Line, file, Filename } | T ],
		  W=#class_info{ includes=Inc, include_defs=IncDefs } ) ->
	get_info( T, W#class_info{ includes=[ Filename | Inc ],
							   include_defs=[ F | IncDefs ]
							 } );


% Type definition section:
%
get_info( _AST=[ F={ attribute, _Line, type,
					 { TypeName, TypeDef, _SubTypeList } } | T ],
		  W=#class_info{ type_definitions=TypeDefs,
						 type_definition_defs=TypeDefsDefs } ) ->
	get_info( T, W#class_info{
				   type_definitions =[ { TypeName, TypeDef } | TypeDefs ],
				   type_definition_defs =[ F | TypeDefsDefs ]
							 } );


% Type export section:
%
get_info( _AST=[ F={ attribute, _Line, export_type, DeclaredTypes } | T ],
		  W=#class_info{ type_exports=TypeExports,
						 type_export_defs=TypeExportDefs } )
  when is_list( DeclaredTypes ) ->
	get_info( T, W#class_info{
				   type_exports= DeclaredTypes ++ TypeExports,
				   type_export_defs=[ F | TypeExportDefs ]
				  } );


% Function export section:
%
get_info( _AST=[ F={ attribute, _Line, export, _Filenames } | T ],
		  W=#class_info{ function_exports=FunExports } ) ->
	get_info( T, W#class_info{ function_exports=[ F | FunExports ] } );


% Function definition section:
%
get_info( _AST=[ Form={ function, _Line, Name, Arity, Clauses } | T ],
		  W=#class_info{

			   constructors=Constructors,
			   destructor=Destructor,
			   requests=Requests,
			   oneways=Oneways,
			   statics=Statics,
			   functions=Functions

			  } ) ->

	% Other clauses could be checked as well:
	%
	% (when adding a function-like element, we may not check if ever there was a
	% pre-existing one - multiple definitions will be rejected by the compiler
	% anyway)
	%
	NewClassInfo = case identify_function( Name, Arity, hd( Clauses ) ) of

		function ->
			NewFunctions = add_function( Name, Arity, Form, Functions ),
			W#class_info{ functions=NewFunctions };

		constructor ->
			NewConstructors = add_constructor( Arity, Form, Constructors ),
			W#class_info{ constructors=NewConstructors };

		destructor ->
			NewDestructor = register_destructor( Form, Destructor ),
			W#class_info{ destructor=NewDestructor };

		request ->
			NewRequests = add_request( Name, Arity, Form, Requests ),
			W#class_info{ requests=NewRequests };

		oneway ->
			NewOneways = add_oneway( Name, Arity, Form, Oneways ),
			W#class_info{ oneways=NewOneways };

		static ->
			NewStatics = add_static( Name, Arity, Form, Statics ),
			W#class_info{ statics=NewStatics }

	end,

	io:format( "function ~s/~B with ~B clauses registered.~n",
			   [ Name, Arity, length( Clauses ) ] ),

	get_info( T, NewClassInfo );


% Spec attributes:
get_info( _AST=[ _F={ attribute, _Line, spec, _FunSpec } | T ],
		  W ) ->
	% Currently dropped!
	get_info( T, W );


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

get_info( _AST=[ H | T ], Infos ) ->
	io:format( "WARNING: ~p not managed.~n", [ H ] ),
	%meta_utils:raise_error( { unhandled_form, H } ),
	get_info( T, Infos ).

% Useless because of eof:
%get_info( _AST=[], Infos, Acc ) ->
%	{ Infos, Acc }.



%% -spec get_superclasses() -> [ class_name() ].
%% get_superclasses() ->
%%	?wooper_superclasses.



% Adds specified function into the corresponding table.
%
add_function( Name, Arity, Form, FunctionTable ) ->

	FunId = { Name, Arity },

	% Its spec might have been found before its definition:

	FunInfo = case table:lookupEntry( FunId, FunctionTable ) of

		key_not_found ->
					  % New entry then:
					  #function_info{
						 name=Name,
						 arity=Arity,
						 definition=Form
						 % Implicit:
						 %spec=undefined
						};

		{ value, F=#function_info{ definition=undefined } } ->
					  % Just add the form then:
					  F#function_info{ definition=Form };

		% Here a definition was already set:
		_ ->
					  meta_utils:raise_error(
						{ multiple_definition_for, FunId } )

	end,

	table:addEntry( _K=FunId, _V=FunInfo, FunctionTable ).



% Adds specified constructor into the corresponding table.
%
add_constructor( Arity, Form, ConstructorTable ) ->

	% Its spec might have been found before its definition:

	FunInfo = case table:lookupEntry( Arity, ConstructorTable ) of

		key_not_found ->
					  % New entry then:
					  #function_info{
						 name=construct,
						 arity=Arity,
						 definition=Form
						 % Implicit:
						 %spec=undefined
						};

		{ value, F=#function_info{ definition=undefined } } ->
					  % Just add the form then:
					  F#function_info{ definition=Form };

		% Here a definition was already set:
		_ ->
					  meta_utils:raise_error(
						{ multiple_definition_for_constructor, Arity } )

	end,

	table:addEntry( _K=Arity, _V=FunInfo, ConstructorTable ).



% Registers specified destructor.
%
register_destructor( Form, undefined ) ->
	% No spec:
	#function_info{ name=destruct, arity=1, definition=Form };

register_destructor( Form, F=#function_info{ definition=undefined } ) ->
	% Already a spec:
	F#function_info{ definition=Form };

register_destructor( _Form, _FunInfo ) ->
	% Here a definition was already set:
	meta_utils:raise_error( multiple_destructors_defined ).



% Adds specified request into the corresponding table.
%
add_request( Name, Arity, Form, RequestTable ) ->

	RequestId = { Name, Arity },

	% Its spec might have been found before its definition:

	RequestInfo = case table:lookupEntry( RequestId, RequestTable ) of

		key_not_found ->
					  % New entry then:
					  #function_info{
						 name=Name,
						 arity=Arity,
						 definition=Form
						 % Implicit:
						 %spec=undefined
						};

		{ value, F=#function_info{ definition=undefined } } ->
					  % Just add the form then:
					  F#function_info{ definition=Form };

		% Here a definition was already set:
		_ ->
					  meta_utils:raise_error(
						{ multiple_definition_for_request, RequestId } )

	end,

	table:addEntry( _K=RequestId, _V=RequestInfo, RequestTable ).



% Adds specified oneway into the corresponding table.
%
add_oneway( Name, Arity, Form, OnewayTable ) ->

	OnewayId = { Name, Arity },

	% Its spec might have been found before its definition:

	OnewayInfo = case table:lookupEntry( OnewayId, OnewayTable ) of

		key_not_found ->
					  % New entry then:
					  #function_info{
						 name=Name,
						 arity=Arity,
						 definition=Form
						 % Implicit:
						 %spec=undefined
						};

		{ value, F=#function_info{ definition=undefined } } ->
					  % Just add the form then:
					  F#function_info{ definition=Form };

		% Here a definition was already set:
		_ ->
					  meta_utils:raise_error(
						{ multiple_definition_for_oneway, OnewayId } )

	end,

	table:addEntry( _K=OnewayId, _V=OnewayInfo, OnewayTable ).



% Adds specified static method into the corresponding table.
%
add_static( Name, Arity, Form, StaticTable ) ->

	StaticId = { Name, Arity },

	% Its spec might have been found before its definition:

	StaticInfo = case table:lookupEntry( StaticId, StaticTable ) of

		key_not_found ->
					  % New entry then:
					  #function_info{
						 name=Name,
						 arity=Arity,
						 definition=Form
						 % Implicit:
						 %spec=undefined
						};

		{ value, F=#function_info{ definition=undefined } } ->
					  % Just add the form then:
					  F#function_info{ definition=Form };

		% Here a definition was already set:
		_ ->
					  meta_utils:raise_error(
						{ multiple_definition_for_static, StaticId } )

	end,

	table:addEntry( _K=StaticId, _V=StaticInfo, StaticTable ).



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
				 class_def=ClassDef,
				 %superclasses=Superclasses,
				 %superclass_def=SuperclassDef,
				 %compilation_option_defs=CompileOptDefs,
				 %parse_attributes=ParseAttributes,
				 parse_attribute_defs=_ParseAttributeDefs,
				 %includes=Includes,
				 %include_defs=IncludeDefs,
				 %type_definitions=TypeDefs,
				 %type_definition_defs=TypeDefDefs
				 type_exports=_TypeExports,
				 %type_export_defs=TypeExportDefs,
				 function_exports=_FunctionExports,
				 class_specific_attributes=_ClassSpecificAttributes,
				 inherited_attributes=_InheritedAttributes,
				 constructors=_Constructors,
				 destructor=_Destructor,
				 requests=_Requests,
				 oneways=_Oneways,
				 statics=_Statics,
				 functions=_Functions,
				 last_line=LastLine } ) ->

	% Let's start by writing the module declaration; nothing found in erl_syntax
	% for that, but rather than doing it by hand we can reuse it directly:
	ModuleForm = [ ClassDef ],


	NewLastLine = LastLine,

	FinalForm = [ NewLastLine | ModuleForm ],

	lists:reverse( FinalForm ).



% Tells whether specified clause belongs to a request, a oneway, a constuctor,
% etc.
%
-spec identify_function( basic_utils:function_name(), arity(), clause() ) ->
							   function_type().
identify_function( _Name=construct, _Arity, _Clause ) ->
	constructor;

identify_function( _Name=destruct, _Arity=1, _Clause ) ->
	destructor;

identify_function( _Name=destruct, Arity, _Clause ) ->
	meta_utils:raise_error( { destructor_arity_must_be_one, Arity } );

identify_function( Name, _Arity,
				   _Clause={ clause, _Line, _Vars, _, AST } ) ->

	case lists:member( Name, get_new_variation_names() ) of

		true ->
			%meta_utils:raise_error( { new_variations_are_reserved, Name } );
			fixme;

		false ->
			ok

	end,

	StringName = text_utils:atom_to_string( Name ),

	case StringName of

		"wooper" ++ _ ->
			%meta_utils:raise_error( { wooper_prefix_is_reserved, Name } );
			fixme;

		_ ->
			ok

	end,

	%io:format( "Inferring ~p:~p:~n", [ Name, Arity ] ),

	% We have here either a function, a request, a oneway or a static method. To
	% discriminate, we simply rely on how values are returned:
	infer_function_type( AST ).



% Infers the type of a function based on the code of one of its clauses.
%
% Note that a method *must* return:
%
% 1. with an appropriate WOOPER construct (ex: wooper:oneway_result/1)
%
% 2. directly from its body (not from an helper function being called)
%
infer_function_type( AST ) ->

	%io:format( "Inferring from code : ~p~n", [ AST ] ),

	% If no wooper return pseudo-call is detected, by default it will be a
	% function:
	%
	{ _SameAST, FunType } = meta_utils:traverse_term( _TargetTerm=AST,
									 _TypeDescription=tuple,
									 _TermTransformer=fun infer_fun_type/2,
									 _UserData=function ),

	FunType.



% A meta_utils:term_transformer():
%
% We simply look-up elements like:
%
% { call, L1,
%         { remote, L2,
%                  { atom, L3, wooper },
%                  { atom, L4, oneway_return }
%         },
%         [ { var, L5, 'AState' } ]
% }
% and determine the type from, here, oneway_return.
%
-spec infer_fun_type( term(), basic_utils:user_data() ) ->
								   { term(), basic_utils:user_data() }.
infer_fun_type( Term={ call, CallLine,
		 { remote, _L2,
				  { atom, _L3, wooper },
				  { atom, _L4, Return }
		 },
		 ArgList }, CurrentType )->

	Len = length( ArgList ),

	DetectedType = case Return of

		request_return when Len =:= 2 ->
						   request;

		oneway_return when Len =:= 1 ->
						   oneway;

		static_return ->
						   static;

		_OtherFunction ->
						   CurrentType

	end,

	% As we do not have a way to stop the traversal, we take advantage of that
	% to check consistency:
	NewType = case CurrentType of

				  % Possibly overriding defaults:
				  function ->
					  DetectedType;

				  % Matches, confirms detection:
				  DetectedType ->
					  DetectedType;

				  OtherType ->
					  meta_utils:raise_error( { inconsistent_function_type,
											CurrentType, OtherType, CallLine } )

	end,

	{ Term, NewType };


infer_fun_type( Term, CurrentType ) ->
	{ Term, CurrentType }.




% Ensures that the described class respects appropriate constraints for WOOPER
% generation, besides the ones checked during the AST exploration and the ones
% that will be checked by the compiler.
%
-spec check_class_info( class_info() ) -> basic_utils:void().
check_class_info( #class_info{

						 constructors=Constructors

					 } ) ->

	case table:isEmpty( Constructors ) of

		true ->
			meta_utils:raise_error( no_constructor_defined );

		false ->
			ok

	end.

	% For each clause of each constructor, we should check that the constructors
	% of direct superclasses have all a fair chance of being called.



% Returns a list of the names of the class_X:*new* operators that are generated
% by WOOPER to branch on the construct/N and thus shall not be defined by the
% user.
%
get_new_variation_names() ->
	[ new_link, synchronous_new, synchronous_new_link, synchronous_timed_new,
	  synchronous_timed_new_link, remote_new, remote_new_link,
	  remote_synchronous_new, remote_synchronous_new_link,
	  remote_synchronisable_new_link, remote_synchronous_timed_new,
	  remote_synchronous_timed_new_link ].




-spec class_info_to_string( class_info() ) -> text_utils:ustring().
class_info_to_string( #class_info{
						 class=Class,
						 class_def=ClassDef,
						 superclasses=Superclasses,
						 superclass_def=SuperclassesDef,
						 compilation_option_defs=CompileOptDefs,
						 parse_attributes=ParseAttributes,
						 parse_attribute_defs=ParseAttributeDefs,
						 includes=Includes,
						 include_defs=IncludeDefs,
						 type_definitions=TypeDefs,
						 type_definition_defs=TypeDefsDefs,
						 type_exports=TypeExports,
						 type_export_defs=TypeExportDefs,
						 function_exports=FunctionExports,
						 class_specific_attributes=ClassAttributes,
						 inherited_attributes=InheritedAttributes,
						 constructors=Constructors,
						 destructor=Destructor,
						 requests=Requests,
						 oneways=Oneways,
						 statics=Statics,
						 functions=Functions,
						 last_line=LastLine
						} ) ->

	SuperclassStrings = case Superclasses of

						   undefined ->
							   [ text_utils:format( "no superclass~n", [] ) ];

							Superclasses ->
								[

								 text_utils:format( "~B superclasses: ~p~n",
									[ length( Superclasses ), Superclasses ] ),

								 text_utils:format(
								   "superclasses definition: ~p~n",
								   [ SuperclassesDef ] )
								]
	end,


	Infos = [

			  text_utils:format( "class: ~p~n", [ Class ] ),
			  text_utils:format( "module definition: ~p~n", [ ClassDef ] )

			 ] ++ SuperclassStrings ++ [

			  text_utils:format( "~B compile option definitions: ~p~n",
								 [ length( CompileOptDefs ), CompileOptDefs ] ),

			  text_utils:format( "~B parse attributes: ~p~n",
								 [ length( ParseAttributes ),
								   ParseAttributes ] ),

			  text_utils:format( "parse attribute definitions: ~p~n",
								 [ ParseAttributeDefs ] ),

			  text_utils:format( "~B includes: ~p~n",
								 [ length( Includes ), Includes ] ),
			  text_utils:format( "include definitions: ~p~n", [ IncludeDefs ] ),

			  text_utils:format( "~B type definitions: ~p~n",
								 [ length( TypeDefs ), TypeDefs ] ),

			  text_utils:format( "type definitions: ~p~n",
								 [ TypeDefsDefs ] ),

			  text_utils:format( "~B type exports: ~p~n",
								 [ length( TypeExports ), TypeExports ] ),

			  text_utils:format( "type export definitions: ~p~n",
								 [ TypeExportDefs ] ),

			  text_utils:format( "~B function exports: ~p~n",
					 [ length( FunctionExports ), FunctionExports ] ),

			  text_utils:format( "~B class-specific attributes: ~p~n",
					 [ length( ClassAttributes ), ClassAttributes ] ),

			  text_utils:format( "~B inherited attributes: ~p~n",
					 [ length( InheritedAttributes ), InheritedAttributes ] ),

			  text_utils:format( "~B constructors, with arities: ~p~n",
								 [ table:size( Constructors ),
								   table:keys( Constructors ) ] ),

			  text_utils:format( "destructor: ~p~n", [ Destructor ] ),

			  text_utils:format( "~B request methods: ~p~n",
								 [ table:size( Requests ),
								   table:keys( Requests ) ] ),

			  text_utils:format( "~B oneway methodss: ~p~n",
								 [ table:size( Oneways ),
								   table:keys( Oneways ) ] ),

			  text_utils:format( "~B static methods: ~p~n",
								 [ table:size( Statics ),
								   table:keys( Statics ) ] ),

			  text_utils:format( "~B plain functions: ~p~n",
								 [ table:size( Functions ),
								   table:keys( Functions ) ] ),

			  text_utils:format( "line count: ~B", [ LastLine ] )

			  ],

	text_utils:format( "Information about class '~s':~n~s", [ Class,
								   text_utils:strings_to_string( Infos ) ] ).
