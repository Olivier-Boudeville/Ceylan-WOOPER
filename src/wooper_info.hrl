% Copyright (C) 2003-2019 Olivier Boudeville
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




% Describes a member attribute of the state of a given class: stores all
% class-level information (i.e. metadata) regarding this attribute.
%
-record( attribute_info, {


	% The name of this attribute (ex: 'total_surface'):
	name :: wooper:attribute_name(),


	% The type of this attribute, currently as an atom (later: as
	% type_utils:type()).
	%
	% Ex: '{ atom(), [ float() ] }'.
	%
	type = undefined :: maybe( type_utils:type_description() ),


	% The qualifiers (if any) that apply to this attribute:
	qualifiers = [] :: [ wooper:attribute_qualifier() ],


	% Textual description (if any) of that attribute (free text):
	description = undefined :: maybe( text_utils:bin_string() )

 } ).



% Note: method information do not have an 'exported' field like functions, as
% they must be exported in all cases anyway (ex: to be callable for child
% classes).



% Stores and centralises WOOPER-level information gathered about a given
% request.
%
% This record is to strictly supersede the Myriad-level function_info one.
%
% See also: the function_info counterpart Myriad records, defined in
% ast_info.hrl.
%
-record( request_info, {

		   % The name of that request:
		   name = undefined :: wooper:request_name(),


		   % The arity of that request:
		   arity = undefined :: wooper:method_arity(),


		   % Qualifiers applying to this request:
		   qualifiers = [] :: wooper:method_qualifiers(),


		   % Corresponds to (our view of) the location of the full form for the
		   % definition (first clause) of this request (not of the spec):
		   %
		   location = undefined :: maybe( ast_info:location() ),


		   % Corresponds to the line of the first defined clause (in its source
		   % file):
		   %
		   % (this information is not redundant with the previous field, as they
		   % are different types of locations)
		   %
		   line = undefined :: maybe( ast_base:line() ),


		   % Request actual definition, a (non-located) list of the abstract
		   % forms of its clauses:
		   %
		   clauses = [] :: [ meta_utils:clause_def() ],


		   % The type specification (if any) of that request, as an abstract
		   % form:
		   spec = undefined :: maybe( wooper_info:located_method_spec() )

} ).


-type request_info() :: #request_info{}.



% Stores and centralises WOOPER-level information gathered about a given oneway
% method.
%
% This record is to strictly supersede the Myriad-level function_info one.
%
% See also: the function_info counterpart Myriad records, defined in
% ast_info.hrl.
%
-record( oneway_info, {
		   % The name of that oneway:
		   name = undefined :: wooper:oneway_name(),


		   % The arity of that oneway:
		   arity = undefined :: wooper:method_arity(),


		   % Qualifiers applying to this oneway:
		   qualifiers = [] :: wooper:method_qualifiers(),


		   % Corresponds to (our view of) the location of the full form for the
		   % definition (first clause) of this oneway (not of the spec):
		   %
		   location = undefined :: maybe( ast_info:location() ),


		   % Corresponds to the line of the first defined clause (in its source
		   % file):
		   %
		   % (this information is not redundant with the previous field, as they
		   % are different types of locations)
		   %
		   line = undefined :: maybe( ast_base:line() ),


		   % Oneway actual definition, a (non-located) list of the abstract
		   % forms of its clauses:
		   %
		   clauses = [] :: [ meta_utils:clause_def() ],


		   % The type specification (if any) of that oneway, as an abstract
		   % form:
		   spec = undefined :: maybe( wooper_info:located_method_spec() )


} ).


-type oneway_info() :: #oneway_info{}.



% Stores and centralises WOOPER-level information gathered about a given static
% method.
%
% This record is to strictly supersede the Myriad-level function_info one.
%
% See also: the function_info counterpart Myriad records, defined in
% ast_info.hrl.
%
-record( static_info, {

		   % The name of that static method:
		   name = undefined :: wooper:static_name(),


		   % The arity of that static method:
		   arity = undefined :: wooper:method_arity(),


		   % Qualifiers applying to this static method:
		   qualifiers = [] :: wooper:method_qualifiers(),


		   % Corresponds to (our view of) the location of the full form for the
		   % definition (first clause) of this static (not of the spec):
		   %
		   location = undefined :: maybe( ast_info:location() ),


		   % Corresponds to the line of the first defined clause (in its source
		   % file):
		   %
		   % (this information is not redundant with the previous field, as they
		   % are different types of locations)
		   %
		   line = undefined :: maybe( ast_base:line() ),


		   % Static actual definition, a (non-located) list of the abstract
		   % forms of its clauses:
		   %
		   clauses = [] :: [ meta_utils:clause_def() ],


		   % The type specification (if any) of that static, as an abstract
		   % form:
		   spec = undefined :: maybe( wooper_info:located_method_spec() )

} ).


-type static_info() :: #static_info{}.



% Description of the class name:
-type class_entry() :: maybe( { wooper:classname(), ast_info:located_form() } ).


% Description of the superclasses:
-type superclasses_entry() :: { [ wooper:classname() ],
								maybe( ast_info:located_form() ) }.




% Stores and centralises WOOPER-level information gathered about a given class.
%
% This record is to strictly supersede the Myriad-level module_info one.
%
% See also: the (superseded here) {module,function}_info counterpart Myriad
% records, defined in ast_info.hrl.
%
-record( class_info, {


		% Name of that class:
		class :: class_entry(),


		% Ordered list of the superclasses of this class and its associated
		% located form (if any is known):
		%
		superclasses :: superclasses_entry(),


		% All the class-specific attributes of the instances (hence not
		% comprising inherited ones):
		%
		attributes :: wooper_info:attribute_table(),


		% All inherited attribute definitions for this class:
		%
		inherited_attributes :: wooper_info:attribute_table(),


		% A table, whose keys are compilation options (ex: no_auto_import,
		% inline, etc.) and whose values are aggregated lists of their
		% associated values (ex: [{size,1}] and [{get_bucket_index,2},{f/1}]).
		%
		% Note: for the 'inline' key, if full inlining is enabled ( '-compile(
		% inline ).'), then its associated key is not a list of function
		% identifiers, but 'all'.
		%
		compilation_options :: ast_info:compile_option_table(),


		% We merely touch compilation options (ex: '{compile, { inline, [ {
		% FunName, Arity } ] } }'):
		%
		compilation_option_defs = [] :: [ ast_info:located_form() ],


		% Other (unrecognised, not corresponding to other fields of interest)
		% parse-level attributes (ex: '-my_attribute( my_value ).'), as a table
		% associating, to an attribute name (an atom key), a list of pairs
		% comprising each a value and an AST form.
		%
		% Such attributes, also named "wild attributes", mostly correspond to
		% user-defined ones.
		%
		parse_attributes :: ast_info:attribute_table(),


		% As remote function specifications can be defined, like:
		% -spec Mod:Name(...) -> ...
		%
		remote_spec_defs = [] :: [ ast_info:located_form() ],


		% Include files (typically *.hrl files, but also includes the .erl
		% source module):
		%
		% (expected to remain empty, as the preprocessor is supposed to have
		% already been run)
		%
		includes = [] :: [ file_utils:file_name() ],


		% Include definitions:
		%
		include_defs = [] :: [ ast_info:located_form() ],


		% Whether a type (possibly any kind of it; ex: opaque or not) is
		% exported is recorded primarily in its own type_info record through a
		% list of locations, while the information sufficient to reconstruct the
		% actual forms for the exports of all types are recorded here.
		%
		% Note: it is better that way, as a type export attribute may define any
		% number of exports, and we need to record its definition line.
		%
		% (this field must be kept synchronised with the table in the
		% 'types' field)
		%
		type_exports :: ast_info:type_export_table(),


		% All information, indexed by type identifiers, about all the types
		% defined in that module:
		%
		types :: ast_info:type_table(),


		% All information (notably: field descriptions), indexed by record
		% names, about all the records known of that module:
		%
		records :: ast_info:record_table(),


		% Lists the functions imported by that module, per-module.
		%
		function_imports :: ast_info:function_import_table(),


		% The definitions of the function imports:
		%
		function_imports_defs = [] :: [ ast_info:located_form() ],


		% Whether a plain function (i.e. not a method) is exported is recorded
		% primarily in its own record through a list of locations, while the
		% information sufficient to reconstruct the actual forms for the exports
		% of all functions are recorded here.
		%
		% Note: it is better that way, as a function export attribute may define
		% any number of exports, and we need to record its definition line.
		%
		% (this field must be kept synchronised with the table in the
		% corresponding 'functions' field)
		%
		function_exports :: ast_info:function_export_table(),


		% All information about the other, plain functions defined for that
		% class:
		%
		% (this field must be kept synchronised with the table in the
		% 'function_exports' field)
		%
		functions :: ast_info:function_table(),


		% All information about the constructor(s) of that class:
		%
		constructors :: wooper_info:constructor_table(),


		% All information about the new operators (ex: remote_new_link/N) of
		% that class:
		%
		new_operators :: ast_info:function_table(),


		% All information about the destructor (if any) of that class:
		%
		destructor = undefined :: maybe( ast_info:function_info() ),


		% Whether a request is exported is recorded primarily in its own
		% request_info record through a list of locations, while the information
		% sufficient to reconstruct the actual forms for the exports of all are
		% recorded here.
		%
		% Note: it is better that way, as a request export attribute may define
		% any number of exports, and we need to record its definition line.
		%
		% (this field must be kept synchronised with the table in the
		% corresponding 'requests' field)
		%
		request_exports :: wooper_info:request_export_table(),


		% All information about the class-specific (member) request methods
		% defined for that class:
		%
		% (this field must be kept synchronised with the table in the
		% 'request_exports' field)
		%
		requests :: wooper_info:request_table(),



		% Whether a oneway is exported is recorded primarily in its own
		% oneway_info record through a list of locations, while the information
		% sufficient to reconstruct the actual forms for the exports of all are
		% recorded here.
		%
		% Note: it is better that way, as a oneway export attribute may define
		% any number of exports, and we need to record its definition line.
		%
		% (this field must be kept synchronised with the table in the
		% corresponding 'oneways' field)
		%
		oneway_exports :: wooper_info:oneway_export_table(),


		% All information about the class-specific (member) oneway methods
		% defined for that class:
		%
		% (this field must be kept synchronised with the table in the
		% 'oneway_exports' field)
		%
		oneways :: wooper_info:oneway_table(),



		% Whether a static method is exported is recorded primarily in its own
		% static_info record through a list of locations, while the information
		% sufficient to reconstruct the actual forms for the exports of all are
		% recorded here.
		%
		% Note: it is better that way, as a static method export attribute may
		% define any number of exports, and we need to record its definition
		% line.
		%
		% (this field must be kept synchronised with the table in the
		% corresponding 'statics' field)
		%
		static_exports :: wooper_info:static_export_table(),


		% All information about the class-specific (member) static methods
		% defined for that class:
		%
		% (this field must be kept synchronised with the table in the
		% 'static_exports' field)
		%
		statics :: wooper_info:static_table(),



		% The definitions of the list of optional callbacks:
		%
		optional_callbacks_defs = [] :: [ ast_info:located_form() ],


		% Tells whether this class shall be compiled and run in debug mode,
		% hence with extended checks for example (slower, safer, easier to
		% debug).
		%
		debug_mode = true :: boolean(),


		% The number of the last line in the original source file:
		%
		% (any added code will be put afterwards)
		%
		last_line :: ast_base:line(),


		% Section markers, offering reference locations to AST transformations.
		%
		markers :: ast_info:section_marker_table(),


		% Error information collected when traversing the AST.
		%
		errors = [] :: [ ast_info:error() ],


		% List of all the located forms that are unhandled, which might be
		% typically errors, like:
		%
		% '{error,{LineNumber,erl_parse, ["syntax error before: ","')'"]}}''.
		%
		unhandled_forms = [] :: [ ast_info:located_form() ]


} ).




% Uncomment to enable WOOPER-level traces:
%-define( enable_wooper_traces, ).


-ifdef(enable_wooper_traces).

-define( display_trace( S ), trace_utils:trace( "[WOOPER] " ++ S ) ).

-define( display_trace( S, F ),
		 ast_utils:trace_fmt( "[WOOPER] " ++ S, F ) ).

-else. % enable_wooper_traces

% To avoid variables being reported as unused depending on the mode:

-define( display_trace( S ),
		 basic_utils:ignore_unused( { wooper_trace_disabled, S } ) ).

-define( display_trace( S, F ),
		 basic_utils:ignore_unused({ wooper_trace_disabled, S, F } ) ).

-endif. % enable_wooper_traces
