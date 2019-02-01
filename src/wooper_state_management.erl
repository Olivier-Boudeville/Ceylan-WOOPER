% Copyright (C) 2014-2019 Olivier Boudeville
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


% For the function_info record:
-include("ast_info.hrl").


% Implementation notes:

% For attributes, we would have liked the user to be able to define them with:
% -attributes([ { name, name(), [ const, protected ], "Some name" }, ... ] ).
%
% However we then end up with {error,{24,erl_parse,"bad attribute"}} (because of
% name() being interpreted as an unexpected function call), and this user
% attribute information is lost.
%
% The parentheses are necessary for types, as they can be polymorphic, so
% instead, we can:
%
% - either hide the type from the parser, like in:
% -attributes([ { name, 'name()', [ const, protected ], "Some name" }, ... ] ).
%
% - or use the define parse attribute, with is more permissive, for macros:
% -define( attributes, [ { name, name(), [ const, protected ], "Some name" },
%                          ... ] ).
%
% We finally preferred the latter to the former, even if it somehow is
% inconsistent with the -superclasses([...]) attribute (as '-attributes(...).'
% would have thus been expected in turn).
%
% We finally chose to support the recommended '-define(superclasses,[...]).',
% for the sake of homogeneity/least surprise.


% Processes the class-specific attributes.
%
-spec manage_attributes( class_info() ) -> class_info().
manage_attributes( ClassInfo=#class_info{ class={ Classname, _LocForm },
										  attributes=AttributeTable,
										  functions=FunctionTable } ) ->

	%trace_utils:trace( "Managing class attributes." ),

	% Now the sole means of declaring the class attributes is by specifying a
	% class_attributes define (rather than a type-limiting -attributes parse
	% attribute.
	%
	% So we rely on the automatically-defined wooper_get_class_attributes/0 to
	% unveil (at compilation time) the value of this attribute, should it be
	% defined:

	AttrFunKey = { wooper_get_class_attributes, 0 },

	case table:hasEntry( AttrFunKey, FunctionTable ) of

		true ->

			% Attributes read, we get rid of this automatically-defined
			% pseudo-function:
			%
			{ _AttrFunInfo=#function_info{ clauses=[
					   { clause, _Line, _Patterns=[], _Guards=[],
						 _Body=[ AttrListForm ] } ] },
			  ShrunkFunctionTable } = table:extractEntry( AttrFunKey,
														  FunctionTable ),

			%trace_utils:debug_fmt( "Class attribute declaration form:~n~p",
			%					   [ AttrListForm ] ),

			NewAttributeTable =
				register_attributes_from_form( AttrListForm, AttributeTable,
											   Classname ),

			%trace_utils:debug_fmt( "As class-specific attributes, we have ~s",
			%				   [ attributes_to_string( NewAttributeTable ) ] ),

			ClassInfo#class_info{ attributes=NewAttributeTable,
								  functions=ShrunkFunctionTable };


		false ->
			trace_utils:warning( "No attribute declaration found." ),
			ClassInfo

	end.



% Registers (and checks) specified attributes.
% (helper)
%
register_attributes_from_form( AttrListForm, AttributeTable, Classname ) ->

	AttrFormList = try

						ast_generation:form_to_list( AttrListForm )

				   catch _:_ ->

						wooper_internals:raise_usage_error(
						  "invalid 'class_attributes' define: expecting a list "
						  "(of attribute declarations).", [], Classname )

				   end,

	%trace_utils:debug_fmt( "Attribute declaration forms:~n  ~p",
	%					   [ AttrFormList ] ),

	register_helper( AttrFormList, AttributeTable, Classname ).



% Note: there is no point in trying to obtain a line number from the forms
% related to class_attributes; indeed, this is just a (preprocessor) define, so
% it has no source location, and any line obtained from a related form would
% point to the wooper_get_class_attributes/0 pseudo-function that we introduced
% - not to the location of said attributes in the file.

% (helper)
register_helper( _AttrFormList=[], AttributeTable, _Classname ) ->
	AttributeTable;

% All attributes are expected to be declared either as a single atom or a
% tuple with 2, 3 or 4 elements:
%
% Single atom:
register_helper( _AttrFormList=[ AttrForm={atom,_,_AttrName} | T ],
				 AttributeTable, Classname ) ->

	% Only the name is specified here:
	NewAttributeTable = register_attribute( _AttrNameForm=AttrForm,
		_TypeForm=undefined, _QualifiersForm=undefined,
		_DescriptionForm=undefined, AttributeTable, Classname ),

	register_helper( T, NewAttributeTable, Classname );

% 4 elements:
register_helper( _AttrFormList=[ _AttrForm={ tuple,_, [ AttrNameForm,
	  TypeForm, QualifiersForm, DescriptionForm ] } | T ], AttributeTable,
				 Classname ) ->

	NewAttributeTable = register_attribute( AttrNameForm, TypeForm,
			  QualifiersForm, DescriptionForm, AttributeTable, Classname ),

	register_helper( T, NewAttributeTable, Classname );

% 3 elements:
register_helper( _AttrFormList=[ _AttrForm={ tuple,_,
					[ AttrNameForm, TypeForm, DescriptionForm ] } | T ],
				 AttributeTable, Classname ) ->

	NewAttributeTable = register_attribute( AttrNameForm, TypeForm,
		_QualifiersForm=undefined, DescriptionForm, AttributeTable, Classname ),

	register_helper( T, NewAttributeTable, Classname );

% 2 elements:
register_helper( _AttrFormList=[ _AttrForm={ tuple,_,
	  [ AttrNameForm, DescriptionForm ] } | T ], AttributeTable, Classname ) ->

	NewAttributeTable = register_attribute( AttrNameForm, _TypeForm=undefined,
		_QualifiersForm=undefined, DescriptionForm, AttributeTable, Classname ),

	register_helper( T, NewAttributeTable, Classname );

% Errors:
register_helper( _AttrForm=[ { tuple,_, Forms } | _T ], _AttributeTable,
				 Classname ) ->
	wooper_internals:raise_usage_error( "invalid attribute declaration tuple in "
		"the 'class_attributes' define (expecting a size of 2, 3 or 4; "
		"got ~B elements).",
		[ length( Forms ) ], Classname );

register_helper( _OtherAttrForm, _AttributeTable, Classname ) ->
	wooper_internals:raise_usage_error( "invalid attribute declaration in "
		"the 'class_attributes' define (neither an atom nor a tuple).", [],
		Classname ).



% (helper)
register_attribute( AttrNameForm, TypeForm, QualifiersForm, DescriptionForm,
					AttributeTable, Classname ) ->

	AttrName = handle_attribute_name( AttrNameForm, Classname ),

	Type = handle_attribute_type( TypeForm, Classname, AttrName ),

	Qualifiers = handle_attribute_qualifiers( QualifiersForm, Classname,
											  AttrName ),

	Description = handle_attribute_description( DescriptionForm, Classname,
												AttrName ),

	AttrInfo = #attribute_info{ name=AttrName,
								type=Type,
								qualifiers=Qualifiers,
								description=Description },

	case table:hasEntry( AttrName, AttributeTable ) of

		true ->
			wooper_internals:raise_usage_error( "multiple declarations for "
				   "class attribute '~s'.", [ AttrName ], Classname );

		false ->
			table:addEntry( AttrName, AttrInfo, AttributeTable )

	end.



% Checks of attribute meta-data:


% Vetting specified attribute name:
handle_attribute_name( _NameForm={atom,_,AtomName}, _Classname ) ->
	AtomName;

handle_attribute_name( _OtherForm, Classname ) ->
	wooper_internals:raise_usage_error( "invalid name for class attribute.", [],
										Classname ).



% Vetting specified attribute type.
%
% Currently, for any future use, we store the user-specified type in its
% abstract form; for example, if having declared an attribute of type 'color()',
% the corresponding '{call,_,{atom,_,color}}' form will be stored.
%
handle_attribute_type( _TypeForm=undefined, _Classname, _AttrName ) ->
	undefined;

handle_attribute_type( TypeForm, _Classname, _AttrName ) when is_tuple( TypeForm ) ->

	%trace_utils:warning_fmt( "Storing attribute type as its raw form:~n  ~p",
	%						 [ TypeForm ] ),

	TypeForm;

% Probably never triggered:
handle_attribute_type( _TypeForm, Classname, AttrName ) ->
	wooper_internals:raise_usage_error( "invalid type for class attribute '~s'.",
										[ AttrName ], Classname ).



% Vetting specified attribute qualifier(s):
handle_attribute_qualifiers( _Qualifiers=undefined, _Classname, _AttrName ) ->
	[];

handle_attribute_qualifiers( _Qualifiers={atom,_,none}, _Classname,
							 _AttrName ) ->
	[];

handle_attribute_qualifiers( Qualifiers={cons,_,_H,_T}, Classname, AttrName ) ->

	% We have a list of qualifiers here (as a form):
	QualifierList = ast_generation:form_to_list( Qualifiers ),

	% It could be checked that no initial is specified if a const is.
	[ handle_attribute_qualifier( Q, Classname, AttrName ) || Q <- QualifierList ];

% A single qualifier shall be promoted to a list:
handle_attribute_qualifiers( Qualifier, Classname, AttrName ) ->
	[ handle_attribute_qualifier( Qualifier, Classname, AttrName ) ].



% Vetting specified attribute qualifier:
handle_attribute_qualifier( {atom,_,public}, _Classname, _AttrName ) ->
	public;

handle_attribute_qualifier( {atom,_,protected}, _Classname, _AttrName ) ->
	protected;

handle_attribute_qualifier( {atom,_,private}, _Classname, _AttrName ) ->
	private;

handle_attribute_qualifier( {tuple,_,[ {atom,_,initial}, ValueForm ]},
							_Classname, _AttrName ) ->

	Value = ast_value:get_immediate_value( ValueForm ),

	% It should be checked also that Value is of the declared type.
	{ initial, Value };

handle_attribute_qualifier( {tuple,_,[ {atom,_,const}, ValueForm ]},
							_Classname, _AttrName ) ->

	Value = ast_value:get_immediate_value( ValueForm ),

	% It should be checked also that Value is of the declared type.
	{ const, Value };

handle_attribute_qualifier( {atom,_,const}, _Classname, _AttrName ) ->
	const;

handle_attribute_qualifier( {atom,_,Other}, Classname, AttrName ) ->
	wooper_internals:raise_usage_error(
	  "invalid qualifier '~s' for class attribute '~s'.", [ Other, AttrName ],
	  Classname );

handle_attribute_qualifier( _UnexpectedForm, Classname, AttrName ) ->
	wooper_internals:raise_usage_error(
	  "invalid qualifier for class attribute '~s'.", [ AttrName ], Classname ).



% Vetting specified attribute description:
handle_attribute_description( _DescriptionForm=undefined, _Classname,
							  _AttrName ) ->
	undefined;

handle_attribute_description( _DescriptionForm={string,_,Description},
							  _Classname, _AttrName ) ->
	Description;

handle_attribute_description( _DescriptionForm, Classname, AttrName ) ->
	wooper_internals:raise_usage_error(
	  "invalid description (not a string) for class attribute '~s'.",
	  [ AttrName ], Classname ).



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
									  type=TypeForm,
									  qualifiers=Qualifiers,
									  description=Description } ) ->

	TypeString = case TypeForm of

		undefined ->
			"undefined type";

		_ ->
			text_utils:format( "type whose form is ~p", [ TypeForm ] )

	end,

	DescString = case Description of

		undefined ->
			"with no associated description";

		_ ->
			text_utils:format( "whose description is '~s'",
							   [ Description ] )

	end,

	text_utils:format( "attribute named '~s' of ~s, with "
					   "qualifiers ~w, and ~s",
					   [ Name, TypeString, Qualifiers, DescString ] ).
