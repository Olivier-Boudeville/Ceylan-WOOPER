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


% We consider here that the ?table type (defined in meta_utils.hrl) is actually
% map_hashtable, and thus can be designated as just table.


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


% Regarding function/method exports:
%
% We could have preferred that methods are auto-exported (defining them would
% have sufficed, with no particular export declaration), yet:
%
% - a pseudo-export line (i.e. '-export([ setColor/2, ...]).') would have to be
% generated and placed
%
% - knowing that we want -spec lines (even in the form '-oneway setColor(...,
% ... ) -> ...') to remain optional, the kind of a method
% (oneway/request/static) would have to be inferred at compilation-time, which
% is not easy (ex: scanning for wooper:oneway_return/1 through all "leaves" of
% the call graph); moreover, all standard functions could be static ones, and
% thus would be exported, which is not desirable
%
% As a result, we preferred relying on explicit exports:
%
% - '-export([ f/1, g/0 ])' for simple functions, as usual
% - '-oneway_export([ setColor/2, resetFoo/1 ]) for oneway methods
% - '-request_export([ getColor/1, isFooReset/1 ]) for request methods
% - '-static_export([ get_mean_count/1, get_default_name/0 ]) for static methods
%
% As for constructor(s) and destructor (if any), there are auto-exported (i.e.
% all construct/N and destruct/1 functions).


% Regarding function/method type specifications:
%
% - example for a (plain) function:
%       -spec f( float() ) -> integer().
%
% - example for a oneway method:
%       -oneway_spec setColor( wooper:state(), color() ) -> void().
%
% - example for a request method:
%       -request_spec getColor( wooper:state() ) -> color().
%
% - example for a static method:
%       -static_spec get_mean_count( foo() ) -> count().


% To better report errors:
-define( origin_layer, "WOOPER" ).


% Used for iterated (re)composition of class information:
-type compose_pair() :: { ast_info:function_table(), class_info() }.



-export([ run_standalone/1, run_standalone/2,
		  parse_transform/2, apply_wooper_transform/1,
		  generate_class_info_from/1, generate_module_info_from/1 ]).




% The specific type of an Erlang function, from the WOOPER point of view:
%
-type function_type() :: 'constructor' | 'destructor'
					   | 'request' | 'oneway' | 'static' | 'function'.



% For function_info:
-include("ast_info.hrl").

% For class_info, attribute_info, etc.:
-include("wooper_info.hrl").




% Local shorthands:

-type ast() :: ast_base:ast().
-type located_form() :: ast_info:located_form().
-type module_info() :: ast_info:module_info().
%-type attribute_info() :: wooper_info:attribute_info().
-type class_info() :: wooper_info:class_info().
-type function_info() :: ast_info:function_info().
-type marker_table() :: ast_info:section_marker_table().


% For debugging:
-export([ check_class_info/1 ]).


% tmp:
-export([add_function/4, add_constructor/3, register_destructor/2 ,add_request/4, add_oneway/4, add_static/4, identify_function/3, infer_function_type/1, infer_fun_type/2,
		 get_new_variation_names/0, get_attribute_forms/1
]).



% Runs the WOOPER parse transform defined here in a standalone way (i.e. without
% being triggered by the usual, integrated compilation process), with no
% specific preprocessor option.
%
% This allows to benefit from all compilation error and warning messages,
% whereas they are seldom available from a code directly run as a parse
% transform (ex: 'undefined parse transform 'foobar'' as soon as a function or a
% module is not found).
%
-spec run_standalone( file_utils:file_name() ) -> { ast(), class_info() }.
run_standalone( FileToTransform ) ->
	run_standalone( FileToTransform, _PreprocessorOptions=[] ).



% Runs the WOOPER parse transform defined here in a standalone way (i.e. without
% being triggered by the usual, integrated compilation process), with specified
% preprocessor options.
%
% This allows to benefit from all compilation error and warning messages,
% whereas they are seldom available from a code directly run as a parse
% transform (ex: 'undefined parse transform 'foobar'' as soon as a function or a
% module is not found).
%
-spec run_standalone( file_utils:file_name(),
			  [ ast_utils:preprocessor_option() ] ) -> { ast(), class_info() }.
run_standalone( FileToTransform, PreprocessorOptions ) ->

	InputAST = ast_utils:erl_to_ast( FileToTransform, PreprocessorOptions ),

	% Returns { WOOPERAST, ClassInfo }:
	apply_wooper_transform( InputAST ).



% The parse transform itself, transforming the specified (WOOPER-based) Abstract
% Format code first into a Myriad-based information being itself converted in
% turn into an Erlang-compliant Abstract Format code.
%
-spec parse_transform( ast(), list() ) -> ast().
parse_transform( InputAST, _Options ) ->


	%trace_utils:trace_fmt( "WOOPER input AST:~n~p~n", [ InputAST ] ),

	%ast_utils:write_ast_to_file( InputAST, "WOOPER-input-AST.txt" ),

	% In the context of this direct parse transform, the class_info is of no
	% use afterwards and thus can be dropped:
	%
	{ WOOPERAST, _ClassInfo } = apply_wooper_transform( InputAST ),

	%trace_utils:trace_fmt( "WOOPER output AST:~n~p~n", [ WOOPERAST ] ),

	%ast_utils:write_ast_to_file( WOOPERAST, "WOOPER-output-AST.txt" ),

	WOOPERAST.



% Transforms specified AST for WOOPER.
%
-spec apply_wooper_transform( ast() ) -> { ast(), class_info() }.
apply_wooper_transform( InputAST ) ->

	trace_utils:debug_fmt( "  (applying parse transform '~p')", [ ?MODULE ] ),

	%trace_utils:debug_fmt( "~n## INPUT ####################################" ),
	%trace_utils:debug_fmt( "WOOPER input AST:~n~p~n~n", [ InputAST ] ),

	ast_utils:write_ast_to_file( InputAST, "WOOPER-input-AST.txt" ),

	% This allows to compare input and output ASTs more easily:
	ast_utils:write_ast_to_file( lists:sort( InputAST ),
								 "WOOPER-input-AST-sorted.txt" ),


	% First preprocesses the AST based on the Myriad parse transform, in order
	% to benefit from its corresponding module_info record:
	% (no Myriad-level transformation performed yet)
	%
	InputModuleInfo = ast_info:extract_module_info_from_ast( InputAST ),

	%trace_utils:debug_fmt( "Module information, directly as obtained "
	%					   "from Myriad (untransformed): ~s",
	%					   [ ast_info:module_info_to_string( InputModuleInfo ) ] ),

	% Then promote this Myriad-level information into a WOOPER one:
	ClassInfo = generate_class_info_from( InputModuleInfo ),

	% Finally perform WOOPER-specific transformation:
	NewClassInfo = transform_class_info( ClassInfo ),

	%trace_utils:debug_fmt( "Transformed class information: ~s",
	%                     [ class_info_to_string( NewClassInfo ) ] ),

	% Then translates back this class information in module information:
	NewModuleInfo = generate_module_info_from( NewClassInfo ),

	% Allows to have still functional class modules during the WOOPER
	% developments:
	%
	EnableWOOPERParseTransform = true,
	%EnableWOOPERParseTransform = false,

	ModuleInfoOfInterest = case EnableWOOPERParseTransform of

		true ->
			NewModuleInfo;

		false ->
			% Do-nothing operation then:
			InputModuleInfo

	end,

	trace_utils:debug_fmt(
	  "Module information before Myriad transformation: ~s",
	  [ ast_info:module_info_to_string( ModuleInfoOfInterest ) ] ),

	% And finally obtain the corresponding updated AST thanks to Myriad:
	TransformedModuleInfo = myriad_parse_transform:transform_module_info(
							  ModuleInfoOfInterest ),

	%trace_utils:debug_fmt(
	%  "Module information after Myriad transformation: ~s",
	%  [ ast_info:module_info_to_string( TransformedModuleInfo ) ] ),

	OutputAST = ast_info:recompose_ast_from_module_info( TransformedModuleInfo ),

	%trace_utils:debug_fmt( "~n~nWOOPER output AST:~n~p~n", [ OutputAST ] ),

	%OutputASTFilename = text_utils:format(
	%           "WOOPER-output-AST-for-module-~s.txt",
	%			[ element( 1, TransformedModuleInfo#module_info.module ) ] ),

	%ast_utils:write_ast_to_file( OutputAST, OutputASTFilename ),

	%ast_utils:write_ast_to_file( lists:sort( OutputAST ),
	%							 "WOOPER-output-AST-sorted.txt" ),

	{ OutputAST, NewClassInfo }.




% Returns the class-level information that were gathered from the specified
% module-level ones.
%
% (reciprocal of generate_module_info_from/1)
%
-spec generate_class_info_from( module_info() ) -> class_info().
generate_class_info_from( ModuleInfo ) ->

	% We handle there only WOOPER-specific needs:

	ExtractedClassInfo = create_class_info_from( ModuleInfo ),

	check_class_info( ExtractedClassInfo ),

	ExtractedClassInfo.



% Recomposes (WOOPER) class information from (Myriad) module-level ones.
%
% The goal is to pick the relevant WOOPER-level information (from the module
% info), to transform them and to populate from the result the specified class
% information.
%
-spec create_class_info_from( module_info() ) -> class_info().
create_class_info_from(
  % We basically reuse (as they are, or after relevant transformations) all
  % information gathered from the module:
  _ModuleInfo=#module_info{ module=ModuleEntry,
							compilation_options=CompileOptTable,
							compilation_option_defs=CompileOptDefs,
							parse_attributes=ParseAttrTable,
							remote_spec_defs=RemoteSpecDefs,
							includes=Includes,
							include_defs=IncludeDefs,
							type_exports=TypeExportTable,
							types=TypeTable,
							records=RecordTable,
							function_imports=FunctionImportTable,
							function_imports_defs=FunctionImportDefs,
							function_exports=FunctionExportTable,
							functions=FunctionTable,
							optional_callbacks_defs=OptCallbacksDefs,
							last_line=LastLine,
							markers=MarkerTable,
							errors=Errors,
							unhandled_forms=UnhandledForms } ) ->

	BlankClassInfo = wooper_info:init_class_info(),

	% For a starting basis, let's init first all the fields that we do not plan
	% to update:
	%
	% (commented: the fields updated afterwards)
	%
	VerbatimClassInfo = BlankClassInfo#class_info{
						  %class
						  %superclasses
						  %attributes
						  %inherited_attributes
						  compilation_options=CompileOptTable,
						  compilation_option_defs=CompileOptDefs,
						  parse_attributes=ParseAttrTable,
						  remote_spec_defs=RemoteSpecDefs,
						  includes=Includes,
						  include_defs=IncludeDefs,
						  type_exports=TypeExportTable,
						  types=TypeTable,
						  records=RecordTable,
						  function_imports=FunctionImportTable,
						  function_imports_defs=FunctionImportDefs,
						  function_exports=FunctionExportTable,
						  functions=FunctionTable,
						  %constructors
						  %destructor
						  %request_exports
						  %requests
						  %oneway_exports
						  %oneways
						  %static_exports
						  %statics
						  optional_callbacks_defs=OptCallbacksDefs,
						  last_line=LastLine,
						  markers=MarkerTable,
						  errors=Errors,
						  unhandled_forms=UnhandledForms },


	% Then taking care of the missing fields, roughly in their original order:

	ClassInClassInfo = manage_classname( ModuleEntry, VerbatimClassInfo ),

	SuperClassInfo = manage_superclasses( ParseAttrTable, ClassInClassInfo ),

	% We extract elements (ex: constructors) from the function table, yet we do
	% not modify specifically the other related information (ex: exports).

	% Managing { FunctionTable, ClassInfo } pairs, in which the first element is
	% the reference, most up-to-date version to use - not any counterpart that
	% could be found in the second element:
	%
	InitialPair = { FunctionTable, SuperClassInfo },

	ConstructPair = manage_constructors( InitialPair, MarkerTable ),

	DestructPair = manage_destructor( ConstructPair, MarkerTable ),

	% ...

	_FinalPair = { FinalFunctionTable, FinalClassInfo } = DestructPair,

	ReturnedClassInfo = FinalClassInfo#class_info{
						  functions=FinalFunctionTable },

	%trace_utils:debug_fmt( "Recomposed class information: ~s",
	%	   [ wooper_info:class_info_to_string( ReturnedClassInfo ) ] ),

	ReturnedClassInfo.



% Registers the corresponding classname into specified class information.
%
-spec manage_classname( module_entry(), class_info() ) -> class_info().
manage_classname( _ModuleEntry=undefined, _ClassInfo ) ->
	raise_error( no_module_name_defined );

manage_classname( _ModuleEntry={ _ModuleName=Classname, ModuleDef },
				  ClassInfo ) ->
	check_classname( Classname ),
	ClassDef = ModuleDef,
	ClassInfo#class_info{ class={ Classname, ClassDef } }.



% Registers the declared superclasses (if any) into specified class information.
%
-spec manage_superclasses( ast_info:attribute_table(), class_info() ) ->
							  class_info().
manage_superclasses( ParseAttrTable, ClassInfo ) ->

	case table:lookupEntry( wooper_superclasses, ParseAttrTable ) of

		% A single declaration expected:
		{ value, [ E={ SuperclassList, _SuperclassDef } ] } ->
			[ check_classname( Class ) || Class <- SuperclassList ],
			ShrunkParseAttrTable = table:removeEntry( wooper_superclasses,
													  ParseAttrTable ),
			% Any prior value superseded, no merging here:
			ClassInfo#class_info{ superclasses=E,
								  parse_attributes=ShrunkParseAttrTable };

		% Cannot be empty, hence more than one declaration:
		{ value, L } ->
			raise_error( { multiple_superclass_declarations, L } );

		key_not_found ->

			%trace_utils:warning( "no superclass specified" ),
			ClassInfo#class_info{ superclasses={ [], undefined } }

			%raise_error( superclasses_not_defined )

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


%% % We accept (only) the Erlang-standard, direct '-module(XXX).' declaration
%% for % now:

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
-spec manage_constructors( compose_pair(), marker_table() ) -> compose_pair().
manage_constructors( { FunctionTable, ClassInfo }, _MarkerTable ) ->

	% First element is a list of { arity(), function_info() } pairs
	% corresponding to the defined constructors, while the second element is the
	% input function table once these corresponding entries have been removed:
	%
	{ ConstructPairs, ShrunkFunctionTable } = extract_constructors_from(
												FunctionTable ),

	case ConstructPairs of

		[] ->
			% Better than throw( no_constructor_found ):
			{ FunctionTable, ClassInfo#class_info{ errors=
				[ no_constructor_found | ClassInfo#class_info.errors ] } };

		_ ->
			trace_utils:debug_fmt( "~B constructor(s) found.",
								   [ length( ConstructPairs ) ] ),

			% Returns { NewFunctionTable, NewClassInfo }:
			manage_new_operators( ConstructPairs, ShrunkFunctionTable,
								  ClassInfo )

	end.



% Returns a list of { arity(), function_info() } pairs and the shrunk table from
% which they were extracted.
%
% (helper)
%
extract_constructors_from( FunctionTable ) ->

	% We are looking, among the keys (i.e. function ids), for those matching
	% {construct,N}, knowing that the associated values are function_info():

	FunIdInfos = table:enumerate( FunctionTable ),

	filter_constructors( FunIdInfos, _AccPairs=[], _AccFunInfos=[] ).



filter_constructors( _FunIdInfos=[], AccPairs, AccFunInfos ) ->
	{ AccPairs, table:new( AccFunInfos ) };

filter_constructors( _FunIdInfos=[ { { construct, Arity }, FunInfo } | T ],
					 AccPairs, AccFunInfos ) ->
	filter_constructors( T, [ { Arity, FunInfo } | AccPairs ], AccFunInfos );

% 'Other' expected to be { { _NonConstructFunName, Arity }, FunInfo }:
%
filter_constructors( _FunIdInfos=[ Other | T ], AccPairs, AccFunInfos ) ->
	filter_constructors( T, AccPairs, [ Other | AccFunInfos ] ).



% Adds the new operators and all their relevant variations for each of the
% specified constructors construct/N.
%
% Returns { NewFunctionTable, NewClassInfo }.
%
manage_new_operators( _ConstructPairs=[], FunctionTable, ClassInfo ) ->
	{ FunctionTable, ClassInfo };

manage_new_operators( _ConstructPairs=[ { Arity, FunInfo } | T ], FunctionTable,
			  ClassInfo=#class_info{ %function_exports=FunExportTable,
									 constructors=Constructors } ) ->

	trace_utils:debug_fmt( "Processing constructor of arity ~B: ~s",
				   [ Arity, ast_info:function_info_to_string( FunInfo ) ] ),

	% First, for the class developer, exporting a constructor is not mandatory;
	% so, if this constructor is not exported, let's do it automatically:
	%
	NewFunInfo = case FunInfo#function_info.exported of

		[] ->
			FunInfo#function_info{ exported=[ xxxlocate_at ] };

		_ ->
			FunInfo

	end,

	NewConstructors = table:addNewEntry( _K=Arity, _V=NewFunInfo,
										 Constructors ),

	RegisteredClassInfo = ClassInfo#class_info{ constructors=NewConstructors },

	% Then, for a constructor of arity N, we have to automatically define and
	% export here following 14 functions, which all are new operator variations
	% V (7 base ones, each doubled depending on whether an (atomic) link is
	% wanted as well between the creator process and the created instance):
	%
	% - V1: new/N-1 and new_link/N-1
	% - V2: synchronous_new/N-1 and synchronous_new_link/N-1
	% - V3: synchronous_timed_new/N-1 and synchronous_timed_new_link/N-1
	% - V4: remote_new/N and remote_new_link/N
	% - V5: remote_synchronous_new/N and remote_synchronous_new_link/N
	% - V6: remote_synchronisable_new/N and remote_synchronisable_new_link/N
	% - V7: remote_synchronous_timed_new/N and
	%       remote_synchronous_timed_new_link/N
	%

	%V1FunTable = add_v1_operators(

	% We have also to record that these new functions are exported:
	% function_exports.

	manage_new_operators( T, FunctionTable, RegisteredClassInfo ).



% Extracts any destructor found in the specified function table, interprets that
% information to update the specified class information.
%
% Returns an updated pair thereof.
%
-spec manage_destructor( compose_pair(), marker_table() ) -> compose_pair().
manage_destructor( { FunctionTable, ClassInfo }, MarkerTable ) ->

	FunIdInfos = table:enumerate( FunctionTable ),

	% First, check that no destruct/N with N =/= 1 exists:
	{ DestructFunInfo, FilteredFunIdInfos } =
		case scan_for_destructors( FunIdInfos ) of

			undefined ->
				% None defined, adding thus a basic, do-nothing one:
				{ get_default_destructor_info( MarkerTable ), FunIdInfos };

			% Found and extracted as: { DestrFunInfo, OtherFunIdInfos } ->
			Other ->
				Other

		end,

	ShrunkFunctionTable = table:new( FilteredFunIdInfos ),

	NewClassInfo = ClassInfo#class_info{ destructor=DestructFunInfo },

	trace_utils:debug_fmt( "Destructor info: ~s",
		[ wooper_info:destructor_to_string( DestructFunInfo,
					_DoIncludeForms=true, _IndentationLevel=1 ) ] ),

	{ ShrunkFunctionTable, NewClassInfo }.





% Checks arities and extracts any destruct/1 found, returning it and the list of
% remaining pairs.
%
% (helper)
%
scan_for_destructors( FunIdInfos ) ->
	scan_for_destructors( FunIdInfos, _Acc={ undefined, [] } ).


scan_for_destructors( _FunIdInfos=[], _Acc={ undefined, _AllFunIdInfos } ) ->
	% No need to return a list of pairs already known of the caller:
	undefined;

% Here Acc is { DestFunInfo, RemainingFunIdInfos }:
scan_for_destructors( _FunIdInfos=[], Acc ) ->
	Acc;

scan_for_destructors( _FunIdInfos=[ { { destruct, 1 }, DestFunInfo } | T ],
					  _Acc={ undefined, AccFunIdInfos } ) ->
	scan_for_destructors( T, { DestFunInfo, AccFunIdInfos } );

scan_for_destructors( _FunIdInfos=[ { { destruct, N }, _DestFunInfo } | _T ],
					  _Acc ) ->
	throw( { disallowed_destructor_arity, N } );

scan_for_destructors( _FunIdInfos=[ Other | T ],
					  _Acc={ DestElem, OtherFunIdInfos } ) ->
	scan_for_destructors( T, { DestElem, [ Other | OtherFunIdInfos ] } ).



% Returns a function information corresponding to the default destructor, which is:
%
% -spec destruct( wooper:state() ) -> wooper:state().
% destruct( State ) ->
%	State.
%
-spec get_default_destructor_info( marker_table() ) -> function_info().
get_default_destructor_info( MarkerTable ) ->

	Line = 0,

	% First, let's define the destructor spec, which is:
	%   -spec destruct( wooper:state() ) -> wooper:state().

	% Corresponds to wooper:state():
	StateType = { remote_type, Line,
				  [ {atom,Line,wooper}, {atom,Line,state}, [] ] },

	SpecForm = { attribute, Line, spec, { {destruct,1},
	   [ { type, Line, 'fun',
		   [ { type, Line, product, _Params=[ StateType ] }, _Result=StateType ]
		 } ] } },

	% Then let's define the destructor function itself, based on:
	%   destruct( State ) ->
	%     State.

	StateVar = { var, Line, 'State' },

	% In AST, we shall have { function, Line, destruct, 1, [ DestructClause ] }:
	DestructClause = { clause, Line, _Pattern=[ StateVar ], [],
					   _Body=[ StateVar ] },

	% The spec and definition are to be placed together at this definition marker:
	DefLocation = ast_info:get_default_definition_function_location(
					MarkerTable ),

	% While the export is to be done in (and will be automatically declared in
	% that export form):
	%
	ExportLocation = ast_info:get_default_export_function_location(
					   MarkerTable ),

	#function_info{ name=destruct,
					arity=1,
					location=DefLocation,
					line=Line,
					clauses=[ DestructClause ],
					spec={ DefLocation, SpecForm },
					callback=false,
					exported=[ ExportLocation ] }.



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
%% get_info( _AST=[ _F={ eof, Line } ], C=#class_info{ last_line=undefined } )
%% -> C#class_info{ last_line=Line };

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
			raise_error( { multiple_definitions_for_request, RequestId } )

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
			raise_error( { multiple_definitions_for_oneway, OnewayId } )

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
			raise_error( { multiple_definitions_for_static, StaticId } )

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



% Returns the (located) forms that correspond to known (class-level) attributes.
%
-spec get_attribute_forms( wooper_info:attribute_table() ) ->
								 [ located_form() ].
get_attribute_forms( _AttributeTable ) ->

	% Currently not managed (no consequence of class-level attribute declaration
	% onto the resulting AST:
	[].

	% The key (attribute name) is duplicated in the attribute_info value:
	% ... = table:values( AttributeTable ),






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
-spec check_class_info( class_info() ) -> void().
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



% Transforms (at the WOOPER level) specified class information.
%
-spec transform_class_info( class_info() ) -> class_info().
transform_class_info( ClassInfo ) ->
	ClassInfo.



% Generates back (Myriad-level) module-level information from specified
% class-level information.
%
% (reciprocal of generate_class_info_from/1)
%
-spec generate_module_info_from( class_info() ) -> module_info().
generate_module_info_from( #class_info{
				 class=ClassEntry,
				 superclasses={ _Superclasses, MaybeSuperclassesLocDef },

				 attributes=AttributeTable,

				 % No impact onto the class-related module itself:
				 inherited_attributes=_InheritedAttributeTable,

				 compilation_options=CompileOptTable,
				 compilation_option_defs=CompileOptDefs,

				 parse_attributes=ParseAttrTable,

				 remote_spec_defs=RemoteSpecDefs,

				 includes=Includes,
				 include_defs=IncludeDefs,

				 type_exports=TypeExportTable,
				 types=TypeTable,

				 records=RecordTable,

				 function_imports=FunctionImportTable,
				 function_imports_defs=FunctionImportDefs,

				 function_exports=FunctionExportTable,
				 functions=FunctionTable,

				 constructors=_ConstructorTable,
				 destructor=MaybeDestructor,

				 request_exports=_RequestExportTable,
				 requests=_RequestTable,

				 oneway_exports=_OnewayExportTable,
				 oneways=_OnewayTable,

				 static_exports=_StaticExportTable,
				 statics=_StaticTable,

				 optional_callbacks_defs=OptCallbackDefs,

				 last_line=LastLine,

				 markers=MarkerTable,

				 errors=Errors,

				 unhandled_forms=UnhandledForms } ) ->

	% Adds back the relevant (WOOPER-related) parse attributes that were
	% interpreted (even though they might not be so useful now):
	%
	FullParseAttrTable = gather_parse_attributes( ParseAttrTable,
									  MaybeSuperclassesLocDef, AttributeTable ),

	% In addition to the plain, classical functions already in
	% FunctionExportTable and Functions, we have to add back constructors,
	% destructor and methods in the function-related fields, i.e. regarding
	% export and definition:

	% For constructors:
	WithConstrExpTable = FunctionExportTable,
	WithConstrFunTable = FunctionTable,

	% For destructor:
	WithDestrFunTable = case MaybeDestructor of

		undefined ->
			WithConstrFunTable;

		DestructFunInfo ->

			DestructId = { destruct, 1 },

			% Expected to have already been appropriately exported.

			table:addNewEntry( DestructId, DestructFunInfo,
							   WithConstrFunTable )

	end,

	% For methods:
	WithMthdExpTable = WithConstrExpTable,
	WithMthdFunTable = WithDestrFunTable,

	AllExportTable = WithMthdExpTable,
	AllFunctionTable = WithMthdFunTable,

	% Directly returned (many fields can be copied verbatim):
	#module_info{

		% Untouched:
		module=ClassEntry,
		compilation_options=CompileOptTable,
		compilation_option_defs=CompileOptDefs,

		parse_attributes=FullParseAttrTable,

		% Untouched:
		remote_spec_defs=RemoteSpecDefs,
		includes=Includes,
		include_defs=IncludeDefs,
		type_exports=TypeExportTable,
		types=TypeTable,
		records=RecordTable,
		function_imports=FunctionImportTable,
		function_imports_defs=FunctionImportDefs,
		function_exports=AllExportTable,
		functions=AllFunctionTable,
		optional_callbacks_defs=OptCallbackDefs,
		last_line=LastLine,
		markers=MarkerTable,
		errors=Errors,
		unhandled_forms=UnhandledForms }.




% Recreates a complete table of parse attributes, from specified arguments.
%
-spec gather_parse_attributes( ast_info:attribute_table(),
	   maybe( ast_info:located_form() ), wooper_info:attribute_table()  ) ->
									 ast_info:attribute_table().
gather_parse_attributes( ParseAttrTable, MaybeSuperclassesLocDef,
						 _AttributeTable ) ->

	% AttributeTable not yet populated.

	case MaybeSuperclassesLocDef of

		undefined ->
			ParseAttrTable;

		SuperclassesLocDef ->
			% V must be a list of {AttrValue,LocForm} pairs:
			table:addNewEntry( _K=wooper_superclasses,
							   _V=[ SuperclassesLocDef ], ParseAttrTable )

	end.



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
