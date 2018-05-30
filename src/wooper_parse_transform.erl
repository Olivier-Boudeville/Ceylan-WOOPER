% Copyright (C) 2014-2018 Olivier Boudeville
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
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
% Creation date: Wednesday, December 24, 2014



% Overall parse transform for the WOOPER layer.
%
% It is meant to be applied to ASTs describing (WOOPER) classes (not standard
% modules).
%
-module(wooper_parse_transform).



% Implementation notes:

% Calls in turn the Myriad parse transform, before and after the WOOPER-level
% operations have been completed (respectively to obtain a module_info as input
% for WOOPER, and to transform adequately, as standard Erlang code, any
% WOOPER-injected code that would rely on Myriad conventions).
%
% One will get: 'undefined parse transform 'wooper_parse_transform'' as soon as
% a compiled module called by the parse transform (ex: text_utils.beam) will not
% be found (hence even if the transform itself is available) or a non-exported
% (or even not existing) function is called (ex: text_utils:format/1).

% We must discriminate here between methods and functions, and identify, among
% detected methods: the requests, the oneways and the static ones.
%
% For that we can rely either on the type specs (if any, as technically they
% remain optional - but we decided that, conventionally, they should better be
% mandatory) or on the function definition itself (relying then on the WOOPER
% return primitives).
%
% More precisely, both for the type spec and the actual code (all clauses):
%
% - a request shall return its state and value thanks to a call to
% wooper:request_return/2
%
% - a oneway shall return its state thanks to a call to wooper:oneway_return/1
%
% - a static method (as opposed to the previous two member methods) shall return
% this value thanks to a call to wooper:static_return/1



% Regarding the WOOPER parse transform.

% All WOOPER-related symbols (ex: atoms, functions, etc.) are to be prefixed by
% 'wooper_'. This prefix shall be considered as reserved for WOOPER internals
% (all wooper_* symbols are forbidden to the user).

% Previously, for simplicity, some values (ex: the superclasses) were defined
% thanks to macro defines (ex: '-define( wooper_foo, 42 )'). Now they are
% specified thanks to attributes -ex: '-wooper_foo( 42 ).' and when there was
% previously ?wooper_foo, we replaced that with the definition of a
% wooper_get_foo() function.


% Regarding WOOPER superclasses.
%
% They used to be defined with:
% '-define( wooper_superclasses, [ class_A, class_B ] ).'.
%
% Now they are defined with the optional:
%
% '-wooper_superclasses( [ class_A, class_B ] ).


% To better report errors
-define( origin_layer, "WOOPER" ).


% Used for iterated (re)composition of class information:
-type compose_pair() :: { ast_info:function_table(), class_info() }.


% Local shorthands:

-type ast() :: ast_base:ast().
-type module_info() :: ast_info:module_info().


-export([ run_standalone/1, parse_transform/2, apply_wooper_transform/1,
		  generate_ast/1, class_info_to_string/1 ]).




% The specific type of an Erlang function, from the WOOPER point of view:
%
-type function_type() :: 'constructor' | 'destructor'
					   | 'request' | 'oneway' | 'static' | 'function'.



% For function_info:
-include("ast_info.hrl").

% For class_info, attribute_info, etc.:
-include("wooper_info.hrl").

-type attribute_info() :: #attribute_info{}.


% For debugging:
-export([ get_class_info/1, check_class_info/1 ]).


% tmp:
-export([add_function/4, add_constructor/3, register_destructor/2 ,add_request/4, add_oneway/4, add_static/4, identify_function/3, infer_function_type/1, infer_fun_type/2,
		 get_new_variation_names/0]).


-type class_info() :: #class_info{}.



% Runs the WOOPER parse transform defined here in a standalone way (i.e. without
% being triggered by the usual, integrated compile process).
%
% This allows to benefit from all compilation error and warning messages,
% whereas they are seldom available from a code directly run as a parse
% transform (ex: 'undefined parse transform 'foobar'' as soon as a function or a
% module is not found).
%
-spec run_standalone( file_utils:file_name() ) -> { ast(), class_info() }.
run_standalone( FileToTransform ) ->

	InputAST = ast_utils:erl_to_ast( FileToTransform ),

	% Returns { WOOPERAST, ClassInfo }:
	apply_wooper_transform( InputAST ).



% The parse transform itself, transforming the specified (WOOPER-based) Abstract
% Format code first into a Myriad-based information being itself converted in
% turn into an Erlang-compliant Abstract Format code.
%
-spec parse_transform( ast(), list() ) -> ast().
parse_transform( InputAST, _Options ) ->

	trace_utils:info_fmt( "(applying parse transform '~p')", [ ?MODULE ] ),

	%trace_utils:trace_fmt( "WOOPER input AST:~n~p~n", [ InputAST ] ),

	%ast_utils:write_ast_to_file( InputAST, "InputAST.txt" ),

	% In the context of this direct parse transform, the class_info is of no
	% use afterwards and thus can be dropped:
	%
	{ WOOPERAST, _ClassInfo } = apply_wooper_transform( InputAST ),

	%trace_utils:trace_fmt( "WOOPER output AST:~n~p~n", [ WOOPERAST ] ),

	WOOPERAST.



% Transforms specified AST for WOOPER.
%
-spec apply_wooper_transform( ast() ) -> { ast(), class_info() }.
apply_wooper_transform( InputAST ) ->

	%trace_utils:debug_fmt( "  (applying parse transform '~p')~n",
	%  [ ?MODULE ] ),

	% First preprocesses the AST based on the Myriad parse transform, in order
	% to benefit from its corresponding module_info record:
	%
	{ MyriadAST, ModuleInfo } = myriad_parse_transform:apply_myriad_transform(
								  InputAST ),

	% Then promote this Myriad-level  information into a WOOPER one:
	ClassInfo = get_class_info( ModuleInfo ),
	%ClassInfo = ok,

	%trace_utils:debug_fmt( "~s", [ class_info_to_string( ClassInfo ) ] ),

	%LastLine = Info#class_info.last_line,

	%ExpForm = meta_utils:form_to_ast( io_lib:format( "-export([ f/0 ]).", [] ),
	%								  LastLine + 1 ),

	%FunForm = meta_utils:form_to_ast( io_lib:format( "f() -> 4.", [] ),
	%								  LastLine + 1 ),

	%NewRevAST = [ { eof, LastLine + 1 }, FunForm | RevAST ],

	%ExpForm = undefined,

	%NewAST =  [ ExpForm | lists:reverse( RevAST ) ],

	%OutputAST = generate_ast( Info ),
	OutputAST = MyriadAST,

	{ OutputAST, ClassInfo }.




% Returns the class information that were gathered in the module ones.
%
-spec get_class_info( module_info() ) -> class_info().
get_class_info( ModuleInfo ) ->

	EmptyTable = table:new(),

	InitClassInfo = #class_info{ function_exports=EmptyTable,
								 constructors=EmptyTable,
								 destructor=undefined,
								 requests=EmptyTable,
								 oneways=EmptyTable,
								 statics=EmptyTable,
								 functions=EmptyTable },

	% We handle there only WOOPER-specific needs:

	ExtractedClassInfo = recompose_module_info_for_class( ModuleInfo,
														  InitClassInfo ),

	check_class_info( ExtractedClassInfo ),

	ExtractedClassInfo.



% Recomposes class information from module-level ones.
%
% The goal is to pick the relevant WOOPER-level information (from the module
% info) and populate from them the specified class information.
%
-spec recompose_module_info_for_class( module_info(), class_info() ) ->
											 class_info().
recompose_module_info_for_class(
  _ModuleInfo=#module_info{ module=ModuleEntry,
							parse_attributes=ParseAttrTable,
							functions=FunctionTable,
							last_line=LastLine },
  ClassInfo ) ->

	ClassInClassInfo = manage_classname( ModuleEntry, ClassInfo ),

	SuperClassInfo = manage_superclasses( ParseAttrTable, ClassInClassInfo ),

	% We extract elements (ex: constructors) from the function table, yet we do
	% not modify specifically the other related information (ex: exports).

	% Managing { FunctionTable, ClassInfo } pairs:
	InitialPair = { FunctionTable, SuperClassInfo },

	ConstructPair = manage_constructors( InitialPair ),

	DestructPair = manage_destructor( ConstructPair ),

	% ...

	_FinalPair = { FinalFunctionTable, FinalClassInfo } = DestructPair,

	ReturnedClassInfo = FinalClassInfo#class_info{
						  functions=FinalFunctionTable,
						  last_line=LastLine },

	trace_utils:debug_fmt( "Recomposed class information: ~s",
						   [ class_info_to_string( ReturnedClassInfo ) ] ),

	ReturnedClassInfo.



% Registers the corresponding classname into specified class information.
%
-spec manage_classname( module_entry(), class_info() ) -> class_info().
manage_classname( _ModuleEntry=undefined, _ClassInfo ) ->
	raise_error( no_module_name_defined );

manage_classname( _ModuleEntry={ _ModuleName=Classname, ModuleDef }, ClassInfo ) ->
	check_classname( Classname ),
	ClassDef = ModuleDef,
	ClassInfo#class_info{ class={ Classname, ClassDef } }.



% Registers the declared superclasses (if any) into specified class information.
%
-spec manage_superclasses( ast_info:attribute_table(), class_info() ) ->
							  class_info().
manage_superclasses( ParseAttrTable, ClassInfo ) ->

	case table:lookupEntry( wooper_superclasses, ParseAttrTable ) of

		{ value, E={ SuperclassList, _SuperclassDef } } ->
			[ check_classname( Class ) || Class <- SuperclassList ],
			NewParseAttrTable = table:removeEntry( wooper_superclasses,
												   ParseAttrTable ),
			ClassInfo#class_info{ superclasses=E,
								  parse_attributes=NewParseAttrTable };

		key_not_found ->
			raise_error( superclasses_not_defined )

	end.




% Class/module section:

% Any invalid or duplicated module declaration will be caught by the compiler
% anyway.


% We wanted the users to rely on a define such as '-classname(class_MyName)'
% instead of '-module(class_MyName)', yet apparently -module should be found
% *before* the parse-transform is ever triggered (we collected the very first
% InputAST we can get to check, it is already unusable if a module declaration
% was lacking), so that the preprocessor can rely on the ?MODULE macro
% afterwards; otherwise the input AST contains forms such as
% '{error,{L,epp,{undefined,'MODULE',none}}}' instead of the forms that referred
% to ?MODULE (as a result these are lost, unrecoverable information).
%
% Only possible work-around: have the actual modules compiled by a specific
% program, driving the compilation by itself, instead of being inserted as a
% mere parse transform. Later maybe!
%
% For the moment, we stick to requiring a
% -module(class_XXX) declaration.
%
%% get_info( _AST=[ { 'attribute', Line, 'classname', Classname } | T ],
%%		  C=#class_info{ class=undefined, class_def=undefined } ) ->

%%	trace_utils:debug_fmt( "Intercepting WOOPER classname declaration for "
%%						   "'~s'.", [ Classname ] ),

%%	check_classname( Classname ),

%%	% Transforms that in a standard module definition:
%%	NewDef = { 'attribute', Line, 'module', Classname },

%%	get_info( T, C#class_info{ class=Classname, class_def=NewDef } );


%% % We accept (only) the Erlang-standard, direct '-module(XXX).' declaration for
%% % now:
%% get_info( _AST=[ F={ 'attribute', _Line, 'module', Classname } | T ],
%%		  C=#class_info{ class=undefined, class_def=undefined } ) ->

%%	%trace_utils:debug_fmt( "Intercepting module-based classname declaration "
%%	%					   "for '~s'.", [ Classname ] ),

%%	check_classname( Classname ),

%%	get_info( T, C#class_info{ class=Classname, class_def=F } );


%% % The fact that no '-module(XXX).' can be found in the source file results in
%% % forms such as {error,{85,epp,{undefined,'MODULE',none}}} that we want to
%% % filter-out, as we will introduce a relevant module form afterwards:
%% %
%% get_info( _AST=[ F={ 'error',{ _Line, 'epp',
%%								 { 'undefined', 'MODULE', 'none' } } } | T ],
%%		  C ) ->

%%	% Problems ahead:
%%	trace_utils:debug_fmt( "Dropping module-related error form ~p.", [ F ] ),

%%	get_info( T, C );



%% % Compilation options section:
%% %
%% get_info( _AST=[ F={ attribute, _Line, compile, _Options } | T ],
%%		  C=#class_info{ compilation_option_defs=Opts } ) ->

%%	get_info( T, C#class_info{ compilation_option_defs=[ F | Opts ] } );


%% % Include section:
%% %
%% get_info( _AST=[ F={ attribute, _Line, file, Filename } | T ],
%%		  C=#class_info{ includes=Inc, include_defs=IncDefs } ) ->
%%	get_info( T, C#class_info{ includes=[ Filename | Inc ],
%%							   include_defs=[ F | IncDefs ] } );


%% % Type definition section:
%% %
%% get_info( _AST=[ F={ attribute, _Line, type,
%%					 { TypeName, TypeDef, _SubTypeList } } | T ],
%%		  C=#class_info{ type_definitions=TypeDefs,
%%						 type_definition_defs=TypeDefsDefs } ) ->
%%	get_info( T, C#class_info{
%%				   type_definitions =[ { TypeName, TypeDef } | TypeDefs ],
%%				   type_definition_defs =[ F | TypeDefsDefs ] } );


%% % Type export section:
%% %
%% get_info( _AST=[ F={ attribute, _Line, export_type, DeclaredTypes } | T ],
%%		  C=#class_info{ type_exports=TypeExports,
%%						 type_export_defs=TypeExportDefs } )
%%   when is_list( DeclaredTypes ) ->
%%	get_info( T, C#class_info{
%%				   type_exports= DeclaredTypes ++ TypeExports,
%%				   type_export_defs=[ F | TypeExportDefs ] } );


%% % Function export section:
%% %
%% get_info( _AST=[ F={ attribute, _Line, export, _Filenames } | T ],
%%		  C=#class_info{ function_exports=FunExports } ) ->
%%	get_info( T, C#class_info{ function_exports=[ F | FunExports ] } );



% Extracts the constructors found in the specified function table, and
% interprets them to enrich the specified class information.
%
% Returns an updated pair thereof.
%
-spec manage_constructors( compose_pair() ) -> compose_pair().
manage_constructors( { FunctionTable, ClassInfo } ) ->
	{ FunctionTable, ClassInfo }.



% Extracts the destructors found in the specified function table, and
% interprets them to enrich the specified class information.
%
% Returns an updated pair thereof.
%
-spec manage_destructor( compose_pair() ) -> compose_pair().
manage_destructor( { FunctionTable, ClassInfo } ) ->
	{ FunctionTable, ClassInfo }.






%% % Function definition section:
%% %
%% get_info( _AST=[ Form={ function, _Line, Name, Arity, Clauses } | T ],
%%		  C=#class_info{ constructors=Constructors,
%%						 destructor=Destructor,
%%						 requests=Requests,
%%						 oneways=Oneways,
%%						 statics=Statics,
%%						 functions=Functions } ) ->

%%	% Other clauses could be checked as well:
%%	%
%%	% (when adding a function-like element, we may not check if ever there was a
%%	% pre-existing one - multiple definitions will be rejected by the compiler
%%	% anyway)
%%	%
%%	NewClassInfo = case identify_function( Name, Arity, hd( Clauses ) ) of

%%		function ->
%%			NewFunctions = add_function( Name, Arity, Form, Functions ),
%%			C#class_info{ functions=NewFunctions };

%%		constructor ->
%%			NewConstructors = add_constructor( Arity, Form, Constructors ),
%%			C#class_info{ constructors=NewConstructors };

%%		destructor ->
%%			NewDestructor = register_destructor( Form, Destructor ),
%%			C#class_info{ destructor=NewDestructor };

%%		request ->
%%			NewRequests = add_request( Name, Arity, Form, Requests ),
%%			C#class_info{ requests=NewRequests };

%%		oneway ->
%%			NewOneways = add_oneway( Name, Arity, Form, Oneways ),
%%			C#class_info{ oneways=NewOneways };

%%		static ->
%%			NewStatics = add_static( Name, Arity, Form, Statics ),
%%			C#class_info{ statics=NewStatics }

%%	end,

%%	trace_utils:debug_fmt( "function ~s/~B with ~B clauses registered.",
% %						   [ Name, Arity, length( Clauses ) ] ),

%%	get_info( T, NewClassInfo );


%% % Spec attributes:
%% get_info( _AST=[ _F={ attribute, _Line, spec, _FunSpec } | T ], W ) ->
%%	% Currently dropped!
%%	get_info( T, W );


%% % Other non-WOOPER attribute section:
%% %
%% get_info( _AST=[ F={ attribute, _Line, AttributeName, AttributeValue } | T ],
%%		  C=#class_info{ parse_attributes=Attributes,
%%						 parse_attribute_defs=AttributeDefs } ) ->

%%	get_info( T, C#class_info{
%%				   parse_attributes=[ { AttributeName, AttributeValue }
%%									  | Attributes ],
%%				   parse_attribute_defs=[ F | AttributeDefs ] } );


%% % Expected to be defined once, and not kept as will be added back later:
%% get_info( _AST=[ _F={ eof, Line } ], C=#class_info{ last_line=undefined } ) ->
%%	C#class_info{ last_line=Line };

%% get_info( _AST=[ H | T ], Infos ) ->
%%	trace_utils:warning_fmt( "~p not managed.", [ H ] ),
%%	%raise_error( { unhandled_form, H } ),
%%	get_info( T, Infos ).

%% % Useless because of eof:
%% %get_info( _AST=[], Infos, Acc ) ->
%% %	{ Infos, Acc }.



%% -spec manage_superclasses() -> [ classname() ].
%% manage_superclasses() ->
%%	?wooper_superclasses.



% Adds specified function into the corresponding table.
%
add_function( Name, Arity, Form, FunctionTable ) ->

	FunId = { Name, Arity },

	% Its spec might have been found before its definition:

	FunInfo = case table:lookupEntry( FunId, FunctionTable ) of

		key_not_found ->
					  % New entry then:
					  #function_info{ name=Name,
									  arity=Arity,
									  location=undefined,
									  line=undefined,
									  clauses=Form
									  % Implicit:
									  %spec=undefined
									  %callback=undefined
									  %exported=[]
									 };

		{ value, F=#function_info{ clauses=undefined } } ->
			% Just add the form then:
			F#function_info{ clauses=Form };

		% Here a definition was already set:
		_ ->
			raise_error( { multiple_definition_for, FunId } )

	end,

	table:addEntry( _K=FunId, _V=FunInfo, FunctionTable ).



% Adds specified constructor into the corresponding table.
%
add_constructor( Arity, Form, ConstructorTable ) ->

	% Its spec might have been found before its definition:

	FunInfo = case table:lookupEntry( Arity, ConstructorTable ) of

		key_not_found ->
			% New entry then:
			#function_info{ name=construct,
							arity=Arity,
							clauses=Form
							% Implicit:
							%spec=undefined
							};

		{ value, F=#function_info{ clauses=undefined } } ->
			% Just add the form then:
			F#function_info{ clauses=Form };

		% Here a definition was already set:
		_ ->
			raise_error( { multiple_definition_for_constructor, Arity } )

	end,

	table:addEntry( _K=Arity, _V=FunInfo, ConstructorTable ).



% Registers specified destructor.
%
register_destructor( Form, undefined ) ->
	% No spec:
	#function_info{ name=destruct, arity=1, clauses=Form };

register_destructor( Form, F=#function_info{ clauses=undefined } ) ->
	% Already a spec:
	F#function_info{ clauses=Form };

register_destructor( _Form, _FunInfo ) ->
	% Here a definition was already set:
	raise_error( multiple_destructors_defined ).



% Adds specified request into the corresponding table.
%
add_request( Name, Arity, Form, RequestTable ) ->

	RequestId = { Name, Arity },

	% Its spec might have been found before its definition:

	RequestInfo = case table:lookupEntry( RequestId, RequestTable ) of

		key_not_found ->
			% New entry then:
			#function_info{ name=Name,
							arity=Arity,
							clauses=Form
							% Implicit:
							%spec=undefined
						   };

		{ value, F=#function_info{ clauses=undefined } } ->
			% Just add the form then:
			F#function_info{ clauses=Form };

		% Here a definition was already set:
		_ ->
			raise_error( { multiple_definition_for_request, RequestId } )

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
			#function_info{ name=Name,
							arity=Arity,
							clauses=Form
							% Implicit:
							%spec=undefined
						  };

		{ value, F=#function_info{ clauses=undefined } } ->
			% Just add the form then:
			F#function_info{ clauses=Form };

		% Here a definition was already set:
		_ ->
			raise_error( { multiple_definition_for_oneway, OnewayId } )

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
			#function_info{ name=Name,
							arity=Arity,
							clauses=Form
							% Implicit:
							%spec=undefined
						  };

		{ value, F=#function_info{ clauses=undefined } } ->
			% Just add the form then:
			F#function_info{ clauses=Form };

		% Here a definition was already set:
		_ ->
			raise_error( { multiple_definition_for_static, StaticId } )

	end,

	table:addEntry( _K=StaticId, _V=StaticInfo, StaticTable ).



% Ensures that specified name is a legit class name.
%
check_classname( Name ) when is_atom( Name ) ->

	case text_utils:atom_to_string( Name ) of

		"class_" ++ _ ->
			ok;

		InvalidName ->
			raise_error( { invalid_classname, no_class_prefix, InvalidName } )

	end;

check_classname( Other ) ->
	raise_error( { invalid_classname, not_atom, Other } ).




% Returns a full AST from specified class information.
%
generate_ast( #class_info{

				 class={ _Class, ClassLocDef },
				 superclasses={ _Superclasses, SuperclassesLocDef },
				 %superclass_def=SuperclassDef,
				 %compilation_option_defs=CompileOptDefs,
				 %parse_attributes=ParseAttributes,
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
	ModuleForm = [ ClassLocDef, SuperclassesLocDef ],


	NewLastLine = LastLine,

	FinalForm = [ NewLastLine | ModuleForm ],

	lists:reverse( FinalForm ).



% Tells whether specified clause belongs to a request, a oneway, a constuctor,
% etc.
%
-spec identify_function( basic_utils:function_name(), arity(),
						 basic_utils:fixme() ) -> function_type().
identify_function( _Name=construct, _Arity, _Clause ) ->
	constructor;

identify_function( _Name=destruct, _Arity=1, _Clause ) ->
	destructor;

identify_function( _Name=destruct, Arity, _Clause ) ->
	raise_error( { destructor_arity_must_be_one, Arity } );

identify_function( Name, _Arity,
				   _Clause={ clause, _Line, _Vars, _, AST } ) ->

	case lists:member( Name, get_new_variation_names() ) of

		true ->
			%raise_error( { new_variations_are_reserved, Name } );
			fixme;

		false ->
			ok

	end,

	StringName = text_utils:atom_to_string( Name ),

	case StringName of

		"wooper" ++ _ ->
			%raise_error( { wooper_prefix_is_reserved, Name } );
			fixme;

		_ ->
			ok

	end,

	%trace_utils:debug_fmt( "Inferring ~p/~B.", [ Name, Arity ] ),

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

	%trace_utils:debug_fmt( "Inferring from code : ~p", [ AST ] ),

	% If no wooper return pseudo-call is detected, by default it will be a
	% function:
	%
	{ _SameAST, FunType } = ast_transform:traverse_term( _TargetTerm=AST,
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
infer_fun_type( Term={ call, CallLine, { remote, _L2,
										 { atom, _L3, wooper },
										 { atom, _L4, Return } },
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

	% As we do not have a way to stop the transformation, we take advantage of
	% that to check consistency:
	%
	NewType = case CurrentType of

		% Possibly overriding defaults:
		function ->
			DetectedType;

		% Matches, confirms detection:
		DetectedType ->
			DetectedType;

		OtherType ->
			raise_error( { inconsistent_function_type, CurrentType, OtherType,
						   CallLine } )

	end,

	{ Term, NewType };


infer_fun_type( Term, CurrentType ) ->
	{ Term, CurrentType }.




% Ensures that the described class respects appropriate constraints for WOOPER
% generation, besides the ones checked during the AST exploration and the ones
% that will be checked by the compiler.
%
-spec check_class_info( class_info() ) -> basic_utils:void().
check_class_info( #class_info{ constructors=Constructors } ) ->

	case table:isEmpty( Constructors ) of

		true ->
			raise_error( no_constructor_defined );

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




-spec class_info_to_string( class_info() ) -> text_utils:ustring().
class_info_to_string( #class_info{
						 class={ Class, ClassLocDef },
						 superclasses={ Superclasses, SuperclassesLocDef },
						 compilation_option_defs=CompileOptDefs,
						 parse_attributes=ParseAttrTable,
						 includes=Includes,
						 include_defs=IncludeDefs,
						 type_definitions=TypeDefs,
						 type_definition_defs=TypeDefsDefs,
						 type_exports=TypeExports,
						 type_export_defs=TypeExportDefs,
						 function_exports=FunctionExportTable,
						 class_specific_attributes=ClassAttributes,
						 inherited_attributes=InheritedAttributes,
						 constructors=Constructors,
						 destructor=Destructor,
						 requests=Requests,
						 oneways=Oneways,
						 statics=Statics,
						 functions=Functions,
						 last_line=LastLineLocDef } ) ->

	SuperclassStrings = case Superclasses of

		undefined ->
			[ text_utils:format( "no superclass~n", [] ) ];

		Superclasses ->
			[

			 text_utils:format( "~B superclasses: ~p~n",
								[ length( Superclasses ), Superclasses ] ),

			 text_utils:format( "superclasses definition: ~p~n",
								[ SuperclassesLocDef ] ) ]
	end,

	ParseAttributes = [ { AttrName, AttrValue } ||
						 { AttrName, { AttrValue, _AttrLocDef } }
							  <- table:enumerate( ParseAttrTable ) ],

	{ _LocLine, { eof, LastLine } } = LastLineLocDef,

	LastLineString = text_utils:format( "line count: ~B", [ LastLine ] ),

	Infos = [

			  text_utils:format( "class: ~p~n", [ Class ] ),
			  text_utils:format( "module definition: ~p~n", [ ClassLocDef ] )

			 ] ++ SuperclassStrings ++ [

			  text_utils:format( "~B compile option definitions: ~p~n",
								 [ length( CompileOptDefs ), CompileOptDefs ] ),

			  text_utils:format( "~B parse attributes: ~p~n",
								 [ length( ParseAttributes ),
								   ParseAttributes ] ),

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

			  text_utils:format( "~B function exports~n",
					 [ table:size( FunctionExportTable ) ] ),

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

			  LastLineString ],

	text_utils:format( "Information about class '~s':~n~s",
					   [ Class, text_utils:strings_to_string( Infos ) ] ).
