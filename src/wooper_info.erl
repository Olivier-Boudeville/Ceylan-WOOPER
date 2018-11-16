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
-type method_spec() :: ast_base:form().


% The type specification of a method:
-type located_method_spec() :: { ast_info:location(), method_spec() }.





% Method export tables.



% Table storing the export declarations for oneway methods.
%
% Quite similar to ast_info:function_export_table().
%
-type oneway_export_table() :: table:table( ast_info:location(),
						   { ast_base:line(), [ wooper:oneway_id() ] } ).



% Table storing the export declarations for request methods.
%
% Quite similar to ast_info:function_export_table().
%
-type request_export_table() :: table:table( ast_info:location(),
						   { ast_base:line(), [ wooper:request_id() ] } ).



% Table storing the export declarations for static methods.
%
% Quite similar to ast_info:function_export_table().
%
-type static_export_table() :: table:table( ast_info:location(),
						   { ast_base:line(), [ wooper:static_id() ] } ).




% A table storing information about the constructors involved, regarding a
% class:
%
-type constructor_table() :: table:table( arity(), ast_info:function_info() ).




% Method definition tables.



% Table storing reference definitions of oneway methods.
%
% Quite similar to ast_info:function_table().
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
			   oneway_table/0, request_table/0, static_table/0,
			   oneway_info/0, request_info/0, static_info/0 ]).


% Shorthands:
-type attribute_name() :: wooper:attribute_name().


-export([ init_class_info/0,

		  class_info_to_string/1, class_info_to_string/2,
		  class_info_to_string/3,

		  class_entry_to_string/2, superclasses_to_string/3,
		  class_specific_attributes_to_string/3,

		  inherited_attributes_to_string/3,
		  attribute_info_to_string/2,

		  constructors_to_string/3, destructor_to_string/3,

		  requests_to_string/3, request_info_to_string/3,
		  oneways_to_string/3, oneway_info_to_string/3,
		  static_methods_to_string/3, static_method_info_to_string/3,

		  get_wooper_builtins/0, get_metadata_builtins/0, get_state_builtins/0,
		  get_execution_builtins/0, get_inner_builtins/0, get_helper_builtins/0,
		  get_serialisation_builtins/0 ]).



% Returns a new, blank instance of the class_info record, typically to be fed
% with an input AST afterwards.
%
-spec init_class_info() -> class_info().
init_class_info() ->

	EmptyTable = table:new(),

	% All other fields (commented out) expected to have a default value defined,
	% or being initialised at record construction:
	%
	#class_info{ class=undefined,
				 superclasses={ [], undefined },
				 attributes=EmptyTable,
				 inherited_attributes=EmptyTable,
				 compilation_options=EmptyTable,
				 %compilation_option_defs
				 parse_attributes=EmptyTable,
				 %remote_spec_defs
				 %includes
				 %include_defs
				 type_exports=EmptyTable,
				 types=EmptyTable,
				 records=EmptyTable,
				 function_imports=EmptyTable,
				 %function_imports_defs
				 function_exports=EmptyTable,
				 functions=EmptyTable,
				 constructors=EmptyTable,
				 new_operators=EmptyTable,
				 destructor=undefined,
				 request_exports=EmptyTable,
				 requests=EmptyTable,
				 oneway_exports=EmptyTable,
				 oneways=EmptyTable,
				 static_exports=EmptyTable,
				 statics=EmptyTable,
				 %optional_callbacks_defs
				 %debug_mode
				 %last_line
				 markers=EmptyTable
				 %errors
				 %unhandled_forms
			   }.



% Returns a textual description of specified class information, not including
% forms, and based on a default indentation level.
%
% Note: here the location information is dropped for all located definitions.
%
-spec class_info_to_string( class_info() ) -> text_utils:ustring().
class_info_to_string( ClassInfo ) ->
	class_info_to_string( ClassInfo, _DoIncludeForms=false ).


% Returns a textual description of specified class information, including forms
% if requested, and with specified indentation level.
%
% Note: here the location information is dropped for all located definitions.
%
-spec class_info_to_string( class_info(), boolean() ) -> text_utils:ustring().
class_info_to_string( ClassInfo, DoIncludeForms ) ->
	class_info_to_string( ClassInfo, DoIncludeForms, _IndentationLevel=0 ).


% Returns a textual description of specified class information, including forms
% if requested, and with specified indentation level.
%
% Note: here the location information is dropped for all located definitions.
%
-spec class_info_to_string( class_info(), boolean(),
					 text_utils:indentation_level() ) -> text_utils:ustring().
class_info_to_string( #class_info{
						 class=ClassEntry,
						 superclasses=SuperclassesEntry,
						 attributes=AttributeTable,
						 inherited_attributes=InheritedAttributes,
						 compilation_options=CompileOpts,
						 compilation_option_defs=CompileOptDefs,
						 parse_attributes=ParseAttributeTable,
						 remote_spec_defs=RemoteSpecDefs,
						 includes=Includes,
						 include_defs=IncludeDefs,
						 type_exports=TypeExportTable,
						 types=TypeTable,
						 records=RecordTable,
						 function_imports=FunctionImportTable,
						 function_imports_defs=FunctionImportDefs,
						 function_exports=_FunctionExportTable,
						 functions=FunctionTable,
						 constructors=ConstructorTable,
						 new_operators=NewOperatorTable,
						 destructor=DestructorInfo,
						 request_exports=_RequestExports,
						 requests=RequestTable,
						 oneway_exports=_OnewayExports,
						 oneways=OnewayTable,
						 static_exports=_StaticExports,
						 statics=StaticTable,
						 optional_callbacks_defs=OptCallbacksDefs,
						 debug_mode=IsDebugMode,
						 last_line=LastLineLocDef,
						 markers=MarkerTable,
						 errors=Errors,
						 unhandled_forms=UnhandledForms },
					  DoIncludeForms,
					  IndentationLevel ) ->

	% For this textual description, we mostly rely on the higher-level
	% information available.

	% As the next strings will be collected at a level of their own:
	NextIndentationLevel = IndentationLevel + 1,

	% Information gathered in the order of the fields (basically in a compatible
	% order with the ast_info:module_info_to_string/3 counterpart function):

	ClassnameString = class_entry_to_string( ClassEntry, DoIncludeForms ),

	DebugString = case IsDebugMode of

		true ->
			"in debug mode";

		false ->
			"not in debug mode"

	end,

	Infos = [ superclasses_to_string( SuperclassesEntry, DoIncludeForms,
									  NextIndentationLevel ),

			  class_specific_attributes_to_string( AttributeTable,
									   DoIncludeForms, NextIndentationLevel ),

			  inherited_attributes_to_string( InheritedAttributes,
									   DoIncludeForms, NextIndentationLevel ),

			  ast_info:compilation_options_to_string( CompileOpts,
						CompileOptDefs, DoIncludeForms, NextIndentationLevel ),

			  ast_info:optional_callbacks_to_string( OptCallbacksDefs,
								   DoIncludeForms, NextIndentationLevel ),

			  DebugString,

			  ast_info:parse_attribute_table_to_string( ParseAttributeTable,
										 DoIncludeForms, NextIndentationLevel ),

			  ast_info:remote_spec_definitions_to_string( RemoteSpecDefs,
										DoIncludeForms, NextIndentationLevel ),

			  ast_info:includes_to_string( Includes, IncludeDefs,
										DoIncludeForms, NextIndentationLevel ),

			  % No form to manage:
			  ast_info:type_exports_to_string( TypeExportTable,
											   NextIndentationLevel ),

			  ast_info:types_to_string( TypeTable, DoIncludeForms,
										NextIndentationLevel ),

			  ast_info:records_to_string( RecordTable, NextIndentationLevel ),

			  ast_info:function_imports_to_string( FunctionImportTable,
				  FunctionImportDefs, DoIncludeForms, NextIndentationLevel ),

			  ast_info:functions_to_string( FunctionTable, DoIncludeForms,
											NextIndentationLevel ),

			  constructors_to_string( ConstructorTable, DoIncludeForms,
									  NextIndentationLevel ),

			  "regarding new operators, " ++ ast_info:functions_to_string(
				   NewOperatorTable, DoIncludeForms, NextIndentationLevel ),

			  destructor_to_string( DestructorInfo, DoIncludeForms,
									NextIndentationLevel ),

			  requests_to_string( RequestTable, DoIncludeForms,
								  NextIndentationLevel ),

			  oneways_to_string( OnewayTable, DoIncludeForms,
								 NextIndentationLevel ),

			  static_methods_to_string( StaticTable, DoIncludeForms,
										NextIndentationLevel ),

			  ast_info:last_line_to_string( LastLineLocDef ),

			  ast_info:markers_to_string( MarkerTable, NextIndentationLevel ),

			  ast_info:errors_to_string( Errors, NextIndentationLevel ),

			  ast_info:unhandled_forms_to_string( UnhandledForms,
							 DoIncludeForms, NextIndentationLevel ) ],

		text_utils:format( "Information about class ~s:~s", [ ClassnameString,
				 text_utils:strings_to_string( Infos, IndentationLevel ) ] ).








% Returns a textual representation of the name of the class corresponding to
% specified entry, possibly with forms.
%
-spec class_entry_to_string( class_entry(), boolean() ) -> text_utils:ustring().
class_entry_to_string( _ClassEntry=undefined, _DoIncludeForms ) ->
	"(unnamed class)";

class_entry_to_string( _ClassEntry={ ThisClassname, _ClassLocDef },
						_DoIncludeForms=false ) ->
	text_utils:atom_to_string( ThisClassname );

class_entry_to_string( _ClassEntry={ ThisClassname,
						_ClassLocDef={ _Loc, Form } },
						_DoIncludeForms=true ) ->
	text_utils:format( "~s (represented as form '~p')",
					   [ ThisClassname, Form ] ).



% Returns a textual representation of the superclasses corresponding to
% specified entry.
%
-spec superclasses_to_string( superclasses_entry(), boolean(),
					  text_utils:indentation_level() ) -> text_utils:ustring().
superclasses_to_string( _SuperclassesEntry={ _SuperclassNames=[],
											 _SuperclassesLocDef },
						_DoIncludeForms,
						_IndentationLevel ) ->
	"no known superclass";

superclasses_to_string( _SuperclassesEntry={ SuperclassNames,
											 SuperclassesLocDef },
						DoIncludeForms,
						_NextIndentationLevel ) ->

	BaseString = text_utils:format( "~B known superclasses: ~p",
							 [ length( SuperclassNames ), SuperclassNames ] ),

	case DoIncludeForms of

		true ->
			text_utils:format( "~s (defined as ~p)",
							   [ BaseString, SuperclassesLocDef ] );

		false ->
			BaseString

	end.



% Returns a textual representation of the specified class-specific attributes.
%
-spec class_specific_attributes_to_string( attribute_table(), boolean(),
				text_utils:indentation_level() ) -> text_utils:ustring().
class_specific_attributes_to_string( AttributeTable, DoIncludeForms,
									 IndentationLevel ) ->

	% Attribute names are also in their information record:
	case table:values( AttributeTable ) of

		[] ->
			"no known class-specific attribute";

		AttrInfos ->

			AttrStrings = [ attribute_info_to_string( AttrInfo, DoIncludeForms )
							|| AttrInfo <- AttrInfos ],

			text_utils:format( "~B known class-specific attribute(s): ~s",
							   [ length( AttrInfos ),
								 text_utils:strings_to_string( AttrStrings,
														IndentationLevel ) ] )

	end.



% Returns a textual representation of the specified inherited (class)
% attributes.
%
-spec inherited_attributes_to_string( attribute_table(), boolean(),
		  text_utils:indentation_level() ) -> text_utils:ustring().
inherited_attributes_to_string( AttributeTable, DoIncludeForms,
								IndentationLevel ) ->

	% Attribute names are also in their information record:
	case table:values( AttributeTable ) of

		[] ->
			"no known inherited attribute";

		AttrInfos ->

			AttrStrings = [ attribute_info_to_string( AttrInfo, DoIncludeForms )
							|| AttrInfo <- AttrInfos ],

			text_utils:format( "~B known inherited attribute(s): ~s",
							   [ length( AttrInfos ),
								 text_utils:strings_to_string( AttrStrings,
														 IndentationLevel ) ] )

	end.



% Returns a textual representation of the specified attribute information.
%
-spec attribute_info_to_string( attribute_info(), boolean() ) ->
									  text_utils:ustring().
attribute_info_to_string( _AttributeInfo, _DoIncludeForms ) ->
	"attribute info".



% Returns a textual representation of the specified constructor information.
%
-spec constructors_to_string( constructor_table(), boolean(),
					  text_utils:indentation_level() ) -> text_utils:ustring().
constructors_to_string( ConstructorTable, DoIncludeForms, IndentationLevel ) ->

	case table:enumerate( ConstructorTable ) of

		[] ->
			"no constructor defined";

		ArityFunInfoPairs ->

			% Sort by increasing arity:
			SortedPairs = lists:keysort( _Index=1, ArityFunInfoPairs ),

			ConstructString = text_utils:strings_to_string( [
				begin

					FunString = ast_info:function_info_to_string(
						  ConstructFunInfo, DoIncludeForms, IndentationLevel ),

					text_utils:format( "of arity ~B, implemented as: ~s",
									   [ Arity, FunString ] )

				end || { Arity, ConstructFunInfo } <- SortedPairs ] ),

			text_utils:format( "~B constructor(s) defined: ~s",
							   [ length( SortedPairs ), ConstructString ] )

	end.



% Returns a textual representation of the specified destructor information.
%
-spec destructor_to_string( maybe( ast_info:function_info() ), boolean(),
					text_utils:indentation_level() ) -> text_utils:ustring().
destructor_to_string( _DestructorInfo=undefined, _DoIncludeForms,
					  _IndentationLevel ) ->
	"no destructor defined";

destructor_to_string( DestructorFunInfo, DoIncludeForms, IndentationLevel ) ->
	text_utils:format( "following destructor defined: ~s",
					   [ ast_info:function_info_to_string( DestructorFunInfo,
								DoIncludeForms, IndentationLevel ) ] ).



% Returns a textual representation of the specified information about requests.
%
-spec requests_to_string( wooper_info:request_table(), boolean(),
				  text_utils:indentation_level() ) -> text_utils:ustring().
requests_to_string( RequestTable, DoIncludeForms, IndentationLevel ) ->

	case table:values( RequestTable ) of

		[] ->
			"no request defined";

		ReqInfos ->

			ReqString = text_utils:strings_to_string( [ request_info_to_string(
							ReqInfo, DoIncludeForms, IndentationLevel )
										|| ReqInfo <- ReqInfos ] ),
			text_utils:format( "~B request(s) defined: ~s",
							   [ length( ReqInfos ), ReqString ] )

	end.



% Returns a textual representation of the specified request information.
%
-spec request_info_to_string( request_info(), boolean(),
				   text_utils:indentation_level() ) -> text_utils:ustring().
request_info_to_string( _RequestInfo, _DoIncludeForms, _IndentationLevel ) ->
	"request info".



% Returns a textual representation of the specified information about oneways.
%
-spec oneways_to_string( wooper_info:oneway_table(), boolean(),
				  text_utils:indentation_level() ) -> text_utils:ustring().
oneways_to_string( OnewayTable, DoIncludeForms, IndentationLevel ) ->

	case table:values( OnewayTable ) of

		[] ->
			"no oneway defined";

		OnwInfos ->

			OnwString = text_utils:strings_to_string( [ oneway_info_to_string(
							OnwInfo, DoIncludeForms, IndentationLevel )
										|| OnwInfo <- OnwInfos ] ),
			text_utils:format( "~B oneway(s) defined: ~s",
							   [ length( OnwInfos ), OnwString ] )

	end.



% Returns a textual representation of the specified oneway information.
%
-spec oneway_info_to_string( oneway_info(), boolean(),
				   text_utils:indentation_level() ) -> text_utils:ustring().
oneway_info_to_string( _OnewayInfo, _DoIncludeForms, _IndentationLevel ) ->
	"oneway info".



% Returns a textual representation of the specified information about static
% methods.
%
-spec static_methods_to_string( wooper_info:static_table(), boolean(),
				  text_utils:indentation_level() ) -> text_utils:ustring().
static_methods_to_string( StaticTable, DoIncludeForms, IndentationLevel ) ->

	case table:values( StaticTable ) of

		[] ->
			"no static method defined";

		StatInfos ->

			StatString = text_utils:strings_to_string( [
				static_method_info_to_string( StatInfo, DoIncludeForms,
							 IndentationLevel ) || StatInfo <- StatInfos ] ),

			text_utils:format( "~B static method(s) defined: ~s",
							   [ length( StatInfos ), StatString ] )

	end.



% Returns a textual representation of the specified static method information.
%
-spec static_method_to_string( static_info(), boolean(),
				   text_utils:indentation_level() ) -> text_utils:ustring().
static_method_info_to_string( _StaticInfo, _DoIncludeForms,
							  _IndentationLevel ) ->
	"static info".





% Section for built-in functions, notably to be able to filter them out.
%
% Note: over time, at least some of them are meant to be implemented by the
% WOOPER parse trandsform, and thus are to disappear from here.


% Returns the function identifiers of all WOOPER builtins.
%
-spec get_wooper_builtins() -> [ ast_info:function_id() ].
get_wooper_builtins() ->
	get_metadata_builtins() ++ get_state_builtins() ++ get_execution_builtins()
		++ get_inner_builtins() ++ get_helper_builtins()
		++ get_serialisation_builtins().



% Returns the function identifiers of the WOOPER builtins regarding class
% metadata.
%
-spec get_metadata_builtins() -> [ ast_info:function_id() ].
get_metadata_builtins() ->
	[ {getSuperclasses,1},
	  {get_superclasses,0},

	  {getClassname,1},
	  {get_classname,0} ].



% Returns the function identifiers of the WOOPER builtins regarding state.
%
-spec get_state_builtins() -> [ ast_info:function_id() ].
get_state_builtins() ->
	[ {hasAttribute,2},
	  {removeAttribute,2},

	  {getAttribute,2},
	  {getAttributes,2},

	  {setAttribute,3},
	  {setAttributes,2},

	  {incrementAttribute,2},
	  {decrementAttribute,2},


	  {appendToAttribute,3},
	  {popFromAttribute,2},

	  {deleteFromAttribute,3},
	  {concatToAttribute,3},

	  {addToAttribute,3},
	  {subtractFromAttribute,3},

	  {toggleAttribute,2},

	  {addKeyValueToAttribute,4} ].



% Returns the function identifiers of the WOOPER builtins regarding behaviour
% (execution).
%
-spec get_execution_builtins() -> [ ast_info:function_id() ].
get_execution_builtins() ->
	[ {executeRequest,2},
	  {executeRequest,3},
	  {executeRequestWith,3},
	  {executeRequestWith,4},

	  {executeOneway,2},
	  {executeOneway,3},
	  {executeOnewayWith,3},
	  {executeOnewayWith,4} ].



% Returns the function identifiers of the WOOPER inner builtins (defined for its
% own mode of operation).
%
-spec get_inner_builtins() -> [ ast_info:function_id() ].
get_inner_builtins() ->
	[ {wooper_main_loop,1},
	  {wooper_lookup_method,3},

	  {wooper_execute_method,3},
	  {wooper_execute_method_with,4},
	  {wooper_effective_method_execution,4},

	  {wooper_handle_local_request_execution,3},
	  {wooper_handle_local_request_execution_with,4},
	  {wooper_handle_remote_request_execution,4},

	  {wooper_handle_local_oneway_execution,3},
	  {wooper_handle_local_oneway_execution_with,4},
	  {wooper_handle_remote_oneway_execution,3},

	  {chain_parent_destructors,1},
	  {trigger_destruct_error,4},

	  {wooper_destruct,1} ].



% Returns the function identifiers of the WOOPER helper builtins.
%
-spec get_helper_builtins() -> [ ast_info:function_id() ].
get_helper_builtins() ->
	[ {is_wooper_debug,0},
	  {wooper_check_undefined,2} ].



% Returns the function identifiers of the WOOPER serialisation builtins.
%
-spec get_serialisation_builtins() -> [ ast_info:function_id() ].
get_serialisation_builtins() ->
	[ {pre_serialise_hook,1},
	  {serialise,3},
	  {post_deserialise_hook,1},
	  {post_serialise_hook,3},

	  {pre_deserialise_hook,2} ].
