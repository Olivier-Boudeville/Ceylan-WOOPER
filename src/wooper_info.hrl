% Copyright (C) 2003-2018 Olivier Boudeville
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




% Describes a member attribute of the state of a given class: stores all
% class-level information (i.e. metadata) regarding this attribute.
%
-record( attribute_info, {


	% The name of this attribute (ex: 'surface'):
	name :: wooper:attribute_name(),


	% The type of this attribute, currently as a string (later: as
	% type_utils:type()).
	%
	% Ex: "{ atom(), [ float() ] }".
	%
	type = "" :: maybe( type_utils:type_description() ),


	% The qualifiers (if any) that apply to this attribute:
	qualifiers = [] :: [ wooper:attribute_qualifier() ],


	% The value to which this attribute will be set prior entering in any actual
	% (class) constructor:
	%
	initial_value = undefined :: maybe( wooper:attribute_value() ),


	% Textual description (if any) of that attribute (free text):
	description = "" :: text_utils:string()

 } ).



% Description of the class name:
-type class_entry() :: basic_utils:maybe( { wooper:classname(),
											ast_info:located_form() } ).



% Stores and centralises WOOPER-level information gathered about a given class.
%
% This record is to strictly supersede the Myriad-level module_info one.
%
% See also: the {module,function}_info counterpart Common records, defined in
% ast_info.hrl.
%
-record( class_info, {


		% Name of that class:
		class :: { wooper:classname(), ast_info:located_form() },


		% Ordered list of the superclasses of this class (corresponding form
		% will be stripped):
		%
		superclasses :: { [ wooper:classname() ], ast_info:located_form() },


		% All the attributes of the instances of that class (including inherited
		% ones):
		%
		attributes :: wooper_info:attribute_table(),


		% We merely touch compilation options (ex: '{compile, { inline, [ {
		% FunName, Arity } ] } }'):
		%
		compilation_option_defs = [] :: [ ast() ],


		% Other parse-level attributes (ex: '-my_attribute( my_value ).'), not
		% corresponding to other fields of interest:
		%
		parse_attributes = [] :: [ ast_info:attribute() ],


		% Include files (typically *.hrl files, but also includes the .erl
		% source module):
		%
		% (expected to remain empty, as the preprocessor is supposed to have
		% already been run)
		%
		includes = [] :: [ file_utils:file_name() ],

		% Include definitions:
		%
		include_defs = [] :: [ ast_base:ast() ],


		% Type definitions:
		%
		% (few information gathered)
		%
		type_definitions = [] :: [ { type_utils:type_name(),
									 type_utils:type_arity() } ],


		% The abstract forms corresponding to type definitions:
		%
		type_definition_defs = [] :: [ ast_base:ast() ],


		% All type exports:
		type_exports = [] :: [ { type_utils:type_name(),
								 type_utils:type_arity() } ],

		% Type export definitions:
		type_export_defs = [] :: [ ast_base:ast() ],


		% Whether a function (possibly any kind of it) is exported is recorded
		% primarily in its own function_info record through a list of locations,
		% while the information sufficient to reconstruct the actual forms for
		% the exports of all functions are recorded here.
		%
		% Note: it is better that way, as a function export attribute may define
		% any number of exports, and we need to record its definition line.
		%
		% (this field must be kept synchronised with the table in the
		% 'functions' field)
		%
		function_exports :: ast_info:function_export_table(),


		% The class-specific attribute definitions (AST forms stripped, hence
		% not kept):
		%
		class_specific_attributes = [] :: [ attribute_info() ],


		% All inherited attribute definitions for this class:
		%
		inherited_attributes = [] :: [ attribute_info() ],


		% All information about the constructor(s) of that class:
		%
		constructors :: table:table( arity(), ast_info:function_info() ),


		% All information about the destructor (if any) of that class:
		%
		destructor = undefined :: ast_info:function_info(),


		% All information about the class-specific (member) request methods
		% defined for that class:
		%
		requests = table:table( ast_info:function_id(),
								ast_info:function_info() ),


		% All information about the class-specific (member) oneway methods
		% defined for that class:
		%
		oneways = table:table( ast_info:function_id(),
							   ast_info:function_info() ),


		% All information about the static methods defined for that class:
		%
		statics = table:table( ast_info:function_id(),
							   ast_info:function_info() ),



		% All information about the other, plain functions defined for that
		% class:
		%
		functions = ast_info:function_table(),


		% The number of the last line in the original source file:
		%
		% (any added code will be put afterwards)
		%
		last_line :: ast_base:line()

} ).
