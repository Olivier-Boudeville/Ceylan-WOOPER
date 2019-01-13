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


% Centralizes, on behalf of the WOOPER parse transform, the support for the
% state management, including instance attributes.
%
-module(wooper_state_management).


-export([ manage_attributes/1, attributes_to_string/1 ]).


% For the class_info record:
-include("wooper_info.hrl").


-type attribute_info() :: wooper_info:attribute_info().

-type attribute_table() :: wooper_info:attribute_table().


% Shorthands:

%-type form_element() :: ast_base:form_element().

-type class_info() :: wooper_info:class_info().



% Processes the class-specific attributes.
%
-spec manage_attributes( class_info() ) -> class_info().
manage_attributes( ClassInfo=#class_info{ attributes=AttributeTable,
										  parse_attributes=ParseAttrTable } ) ->

	% Has '-attributes(...).' been specified?
	case table:hasEntry( attributes, ParseAttrTable ) of

		true ->
			{ AttrDeclarations, ShrunkParseAttrTable } =
				table:extractEntry( attributes, ParseAttrTable ),

			%trace_utils:debug_fmt( "Parsed ttribute declaration entry:~n~p",
			%					   [ AttrDeclarations ] ),

			NewAttributeTable = register_attributes( AttrDeclarations,
													 AttributeTable ),

			%trace_utils:debug_fmt( "As class-specific attributes, we have ~s",
			%				   [ attributes_to_string( NewAttributeTable ) ] ),

			ClassInfo#class_info{ attributes=NewAttributeTable,
								  parse_attributes=ShrunkParseAttrTable };


		false ->
			trace_utils:warning( "No attribute declaration found." ),
			ClassInfo

	end.


% Registers (and checks) specified attributes.
% (helper)
%
register_attributes( [ _SingleAttrDeclaration={ Declaration, _Form } ],
					 AttributeTable ) ->

	%trace_utils:debug_fmt( "Attribute declaration: ~p", [ Declaration ] ),

	register_helper( Declaration, AttributeTable );

register_attributes( MultipleAttrDeclarations, _AttributeTable ) ->
	wooper_internals:raise_error( { multiple_attribute_declarations,
			 [ List || { List, _Form } <- MultipleAttrDeclarations ] } ).


% (helper)
register_helper( _Declaration=[], AttributeTable ) ->
	AttributeTable;

register_helper( _Declaration=[ AttrDeclaration | T ], AttributeTable ) ->

	NewAttributeTable = register_attribute( AttrDeclaration, AttributeTable ),

	register_helper( T, NewAttributeTable );


% Not even a list:
register_helper( Declaration, _AttributeTable ) ->
	wooper_internals:raise_error(
	  { invalid_attribute_declaration, Declaration } ).



% (helper)
register_attribute( AttributeName, AttributeTable )
  when is_atom( AttributeName ) ->
	Description = undefined,
	register_attribute( { AttributeName, Description },
						AttributeTable );

register_attribute( { AttributeName, Description }, AttributeTable ) ->
	Type = 'any()',
	register_attribute( { AttributeName, Type, Description },
						AttributeTable );

register_attribute( { AttributeName, Type, Description },
					AttributeTable ) ->
	Qualifier = 'none',
	register_attribute( { AttributeName, Type, Qualifier,
						  Description },
						AttributeTable );

register_attribute( { AttributeName, Type, Qualifier, Description },
					AttributeTable ) ->

	CheckedAttrName = check_attribute_name( AttributeName ),
	CheckedType = check_type( Type ),
	CheckedQualifiers = check_qualifiers( Qualifier ),
	CheckedDescription = check_description( Description ),

	AttrInfo = #attribute_info{
				  name=CheckedAttrName,
				  type=CheckedType,
				  qualifiers=CheckedQualifiers,
				  description=CheckedDescription },

	case table:hasEntry( CheckedAttrName, AttributeTable ) of

		true ->
			wooper_internals:raise_error(
			  { multiple_definitions_for_attribute, CheckedAttrName } );

		false ->
			table:addEntry( CheckedAttrName, AttrInfo, AttributeTable )

	end.


% Checks of attribute meta-data:


% Vetting specified attribute name:
check_attribute_name( AttributeName ) when is_atom( AttributeName ) ->
	AttributeName;

check_attribute_name( AttributeName ) ->
	wooper_internals:raise_error( { invalid_attribute_name, AttributeName } ).


% Vetting specified attribute type.
%
% Note that we would have preferred to support directly a type specification
% (like: integer()), rather than having to enclose it between single quotes to
% make it an atom (like: 'integer()'), however unfortunately it is not possible
% with the Erlang parser: it returns {error,{24,erl_parse,"bad attribute"}} and
% the actual type information, whose parsing failed, is then lost.
%
check_type( Type ) when is_atom( Type ) ->
	Type;

check_type( Type ) ->
	wooper_internals:raise_error( { invalid_attribute_type, Type } ).


% Vetting specified attribute qualifier(s):
check_qualifiers( Qualifiers ) when is_list( Qualifiers ) ->
	% It could be checked that no initial is specified if a const is.
	[ check_qualifier( Q ) || Q <- Qualifiers ];

check_qualifiers( Qualifier ) ->
	check_qualifiers( [ Qualifier ] ).


% Vetting specified attribute qualifier:
check_qualifier( public ) ->
	public;

check_qualifier( protected ) ->
	protected;

check_qualifier( private ) ->
	private;

check_qualifier( Q={ initial, _Value } ) ->
	% Check also that Value is of the declared type.
	Q;

check_qualifier( Q={ const, _Value } ) ->
	% Check also that Value is of the declared type.
	Q;

check_qualifier( Q=const ) ->
	Q;

check_qualifier( Q=none ) ->
	Q.


% Vetting specified attribute description:
check_description( _Description=undefined ) ->
	undefined;

check_description( Description ) when is_list( Description ) ->

	case text_utils:is_string( Description ) of
		true ->
			text_utils:string_to_binary( Description );

		false ->
			wooper_internals:raise_error(
			  { invalid_attribute_description, Description } )

	end;

check_description( Description ) ->
	wooper_internals:raise_error(
	  { invalid_attribute_description_type, Description } ).



% Returns a textual description of specified attribute table.
%
-spec attributes_to_string( attribute_table() ) -> text_utils:ustring().
attributes_to_string( AttributeTable ) ->

	AttrInfos = table:values( AttributeTable ),

	AttrStrings = [ attribute_to_string( AttrInfo )
					|| AttrInfo <- AttrInfos ],

	case length( AttrStrings ) of

		0 ->
			"no attribute defined";

		L ->
			text_utils:format( "~B attributes defined: ~s",
				[ L, text_utils:strings_to_sorted_string( AttrStrings ) ] )

	end.



% Returns a textual description of specified attribute information.
%
-spec attribute_to_string( attribute_info() ) -> text_utils:ustring().
attribute_to_string( #attribute_info{ name=Name,
									   type=Type,
									   qualifiers=Qualifiers,
									   description=Description } ) ->

	TypeString = case Type of

		undefined ->
			"undefined type";

		_ ->
			text_utils:format( "type '~s'", [ Type ] )

	end,

	DescString = case Description of

		undefined ->
			"with no associated description";

		_ ->
			text_utils:format( "whose description is '~s'",
							   [ Description ] )

	end,

	text_utils:format( "attribute named '~s' of ~s, with "
					   "qualifiers ~p, ~s",
					   [ Name, TypeString, Qualifiers, DescString ] ).
