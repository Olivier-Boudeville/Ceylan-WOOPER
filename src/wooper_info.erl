% Copyright (C) 2018-2018 Olivier Boudeville
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
% Creation date: Friday, April 13, 2018
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]



% Centralisation of class-level information.
%
-module(wooper_info).


% For the attribute_info record:
-include("wooper_info.hrl").



-type class_info() :: #class_info{}.


-type attribute_info() :: #attribute_info{}.


% Stores all class-level information (i.e. metadata) regarding attributes (class
% ones, not parse ones).
%
-type attribute_table() :: table:table( attribute_name(), attribute_info() ).


% The form corresponding to a method specification:
-type method_spec() :: meta_utils:form().


% The type specification of a method:
-type located_method_spec() :: { location(), method_spec() }.




% Method export tables.


% Table storing the export declarations for oneway methods.
%
% Quite similar to ast_info:function_export_table().
%
-type oneway_export_table() :: table:table( location(),
						   { ast_base:line(), [ wooper:oneway_id() ] } ).


% Table storing the export declarations for request methods.
%
% Quite similar to ast_info:function_export_table().
%
-type request_export_table() :: table:table( location(),
						   { ast_base:line(), [ wooper:request_id() ] } ).


% Table storing the export declarations for static methods.
%
% Quite similar to ast_info:function_export_table().
%
-type static_export_table() :: table:table( location(),
						   { ast_base:line(), [ wooper:static_id() ] } ).




% A table storing information about the constructors involved, regarding a
% class:
%
-type constructor_table() :: table:table( arity(), ast_info:function_info() ).



% Method definition tables.


% Table storing reference definitions of oneway methods.
%
% Quite similar to ast_info:function_table().
%
-type oneway_table() :: table:table( wooper:oneway_id(),
									 wooper_info:oneway_info() ).



% Table storing reference definitions of request methods.
%
% Quite similar to ast_info:function_table().
%
-type request_table() :: table:table( wooper:request_id(),
									  wooper_info:request_info() ).



% Table storing reference definitions of static methods.
%
% Quite similar to ast_info:function_table().
%
-type static_table() :: table:table( wooper:static_id(),
									 wooper_info:static_info() ).




-export_type([ class_info/0, attribute_info/0, attribute_table/0,
			   method_spec/0, located_method_spec/0,
			   oneway_export_table/0, request_export_table/0,
			   static_export_table/0,
			   constructor_table/0,
			   oneway_table/0, request_table/0, static_table/0 ]).


% Shorthands:
-type attribute_name() :: wooper:attribute_name().


-export([ attribute_info_to_string/1,

		  oneway_info_to_string/1, request_info_to_string/1,
		  static_info_to_string/1,

		  class_info_to_string/1 ]).



% Returns a new, blank instance of the class_info record, typically to be fed
% with an input AST afterwards.
%
-spec init_class_info() -> class_info().
init_class_info() ->

	EmptyTable = table:new(),

	% All other fields expected to have a default value defined, or being
	% initialised at record construction:
	%
	#class_info{ class=undefined,
				 superclasses={ [], undefined }
				 attributes=EmptyTable,
				 inherited_attributes=EmptyTable,
				 compilation_options=EmptyTable,
				 parse_attributes=EmptyTable,
				 type_exports=EmptyTable,
				 types=EmptyTable,
				 records=EmptyTable,
				 function_exports=EmptyTable,
				 constructors=EmptyTable,
				 destructor=undefined,
				 requests=EmptyTable,
				 oneways=EmptyTable,
				 statics=EmptyTable,
				 functions=EmptyTable }.



-spec class_info_to_string( class_info() ) -> text_utils:ustring().
class_info_to_string( ClassInfo ) ->
	class_info_to_string( ClassInfo, _IncludeForms=false ).



% Returns a textual information of specified class information.
%
-spec class_info_to_string( class_info(), boolean() ) -> text_utils:ustring().
class_info_to_string( #class_info{
						 class=ClassEntry,
						 superclasses=SuperclassesEntry,
						 attributes=AttributeTable,
						 inherited_attributes=InheritedAttributes,
						 compilation_options=CompileOpts,
						 compilation_option_defs=_CompileOptDefs,
						 parse_attributes=ParseAttributeTable,
						 remote_spec_defs=RemoteSpecDefs,
						 includes=Includes,
						 include_defs=IncludeDefs,
						 type_exports=TypeExportTable,
						 types=TypeExportDefs,
						 records=Records,
						 function_imports=FunctionImports,
						 function_imports_defs=FunctionImportDefs,
						 function_exports=FunctionExportTable,
						 functions=FunctionTable,
						 constructors=Constructors,
						 destructor=Destructor,
						 request_exports=RequestExports,
						 requests=Requests,
						 oneway_exports=OnewayExports,
						 oneways=Oneways,
						 static_exports=StaticExports,
						 statics=Statics,
						 optional_callbacks_defs=OptCallbackDefs,
						 last_line=LastLineLocDef,
						 errors=Errors,
						 unhandled_forms=UnhandledForms },
					  IncludeForms ) ->

	% To mark an additional offset for the sublists:
	NextIndentationLevel = 1,

	ClassnameStrings = class_entry_to_strings( ClassEntry, IncludeForms ),

	SuperclassStrings = superclasses_to_strings( SuperclassesEntry,
												 IncludeForms ),

	ClassSpecificAttrStrings = class_specific_attributes_to_string(
								AttributeTable, IncludeForms ),

	InheritedAttrStrings = inherited_attributes_to_string( InheritedAttributes,
														   IncludeForms ),

	CompilationOptStrings = ast_info:compilation_options_to_string( CompileOpts,
						  CompileOptDefs, IncludeForms, NextIndentationLevel ),

	ParseAttrStrings = ast_info:parse_attribute_table_to_string(
						 ParseAttributeTable, IncludeForms ),

	RemoteSpecDefStrings =

	IncludeStrings = ast_info:includes_to_string( Includes, IncludeDefs,
												  NextIndentationLevel ),


	Infos = ClassnameStrings ++ SuperclassStrings
		++ ClassSpecificAttrStrings ++ InheritedAttrString
		++ CompilationOptString ++ ParseAttrStrings ++ IncludeStrings






			  text_utils:format( "~B type definitions: ~p~n",
								 [ length( TypeDefs ), TypeDefs ] ),

			  text_utils:format( "type definitions: ~p~n", [ TypeDefsDefs ] ),

			  ast_info:type_exports_to_string( TypeExportTable,
											   NextIndentationLevel ),


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

			  ast_info:last_line_to_string( LastLineLocDef ) ],

	text_utils:format( "Information about class '~s':~n~s",
					   [ Class, text_utils:strings_to_string( Infos ) ] ).



% Returns a textual representation of the name of the class corresponding to
% specified entry.
%
-spec class_entry_to_strings( class_entry(), boolean() ) ->
									[ text_utils:ustring() ].
class_entry_to_strings( _ClassEntry=undefined, _IncludeForms ) ->
	[ "(unnamed class)" ];

class_entry_to_strings( _ClassEntry={ ThisClassname, _ClassLocDef },
						_IncludeForms=false ) ->
	[ text_utils:atom_to_string( ThisClassname ) ];

class_entry_to_strings( _ClassEntry={ ThisClassname, ClassLocDef },
						_IncludeForms=true ) ->
	[ text_utils:atom_to_string( ThisClassname ),
	  text_utils:format( "classname located definition: ~p",
						 [ ClassLocDef ] ) ].



% Returns a textual representation of the superclasses corresponding to
% specified entry.
%
-spec superclasses_to_strings( superclasses_entry(), boolean() ) ->
									[ text_utils:ustring() ].
superclasses_to_strings( _SuperclassesEntry={ _SuperclassNames=[],
								  _SuperclassesLocDef }, _IncludeForms ) ->
	[ "no known superclass" ];

superclasses_to_strings( _SuperclassesEntry={ SuperclassNames,
									  SuperclassesLocDef }, IncludeForms ) ->

	BaseString = text_utils:format( "~B known superclasses: ~p",
							 [ length( SuperclassNames ), SuperclassNames ] ),

	case IncludeForms of

		true ->
			DefString = text_utils:format( "superclasses definition: ~p",
										   [ SuperclassesLocDef ] ),
			[ BaseString, DefString ];


		false ->
			[ BaseString ]

	end.



% Returns a textual representation of the specified class-specific attributes.
%
-spec class_specific_attributes_to_string( attribute_table(), boolean() ) ->
												[ text_utils:ustring() ].
class_specific_attributes_to_string( AttributeTable, IncludeForms ) ->

	% Attribute names are also aming their information:
	case table:values( AttributeTable ) of

		[] ->
			[ "no known class-specific attribute" ];

		AttrInfos ->

			AttrStrings = [ attribute_info_to_string( AttrInfo, IncludeForms )
							|| AttrInfo <- AttrInfos ],

			text_utils:format( "~B known class-specific attribute(s): ~s",
							   [ length( AttrInfos ),
								 text_utils:strings_to_string( AttrStrings ) ] )

	end.



% Returns a textual representation of the specified inherited (class)
% attributes.
%
-spec inherited_attributes_to_string( attribute_table(), boolean() ) ->
											[ text_utils:ustring() ].

inherited_attributes_to_string( AttributeTable, IncludeForms ) ->

	% Attribute names are also aming their information:
	case table:values( AttributeTable ) of

		[] ->
			[ "no known inherited attribute" ];

		AttrInfos ->

			AttrStrings = [ attribute_info_to_string( AttrInfo, IncludeForms )
							|| AttrInfo <- AttrInfos ],

			text_utils:format( "~B known inherited attribute(s): ~s",
							   [ length( AttrInfos ),
								 text_utils:strings_to_string( AttrStrings ) ] )

	end.
