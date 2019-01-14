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


% Centralizes, on behalf of the WOOPER parse transform, the support for classes,
% inheritance, etc.
%
-module(wooper_class_management).


-export([ check_classname/1, manage_classname/2, manage_superclasses/2 ]).



% For the function_info record:
-include("ast_info.hrl").

% For the class_info record:
-include("wooper_info.hrl").


% Shorthands:

-type class_info() :: wooper_info:class_info().




% Implementation notes:

% The implementation of getClassname/1 might be created here rather than bein
% defined in the wooper_classes_functions.hrl header file.


% Regarding WOOPER superclasses.
%
% They used to be defined with:
% '-define( wooper_superclasses, [ class_A, class_B ] ).'.
%
% Now they are either defined with the optional:
%
% '-superclasses( [ class_A, class_B ] ).
%
% (which has been finally preferred to '-wooper_superclasses([]).').
%
% or with the alternative (optional as well):
% '-define( superclasses, [ class_A, class_B ] ).'
%
% This latter form is less interesting than the former, yet it allows to follow
% the same convention as '-define( class_attributes, [...])', which is more
% constrained (due to the parentheses involved in a type declaration, it cannot
% be a wild attribute like -attributes or -class_attributes ).


% Ensures that specified name is a legit class name, and returns it.
%
-spec check_classname( any() ) -> atom().
check_classname( Name ) when is_atom( Name ) ->

	case text_utils:atom_to_string( Name ) of

		"class_" ++ _ ->
			Name;

		InvalidName ->
			wooper_internals:raise_error( { invalid_classname,
											no_class_prefix, InvalidName } )

	end;

check_classname( Other ) ->
	wooper_internals:raise_error( { invalid_classname, not_atom, Other } ).



% Registers the corresponding classname into specified class information.
%
-spec manage_classname( module_entry(), class_info() ) -> class_info().
manage_classname( _ModuleEntry=undefined, _ClassInfo ) ->
	wooper_internals:raise_error( no_module_name_defined );

manage_classname( _ModuleEntry={ _ModuleName=Classname, ModuleDef },
				  ClassInfo ) ->
	check_classname( Classname ),
	ClassDef = ModuleDef,
	ClassInfo#class_info{ class={ Classname, ClassDef } }.



% Registers the declared superclasses (if any) into specified class information.
%
-spec manage_superclasses( ast_info:attribute_table(), class_info() ) ->
							  class_info().
manage_superclasses( ParseAttrTable,
					 ClassInfo=#class_info{ functions=FunctionTable,
											markers=MarkerTable  } ) ->

	{ Superclasses, RegisteredClassInfo } =
		case table:lookupEntry( superclasses, ParseAttrTable ) of

		% A single declaration expected:
		{ value, [ E={ SuperclassList, _SuperclassDef } ] } ->
			Classnames = [ check_classname( Cl ) || Cl <- SuperclassList ],
			ShrunkParseAttrTable = table:removeEntry( superclasses,
													  ParseAttrTable ),
			% Any prior value superseded, no merging here:
			{ Classnames,
			  ClassInfo#class_info{ superclasses=E,
									parse_attributes=ShrunkParseAttrTable } };

		% Cannot be empty, hence more than one declaration:
		{ value, L } ->
			wooper_internals:raise_error(
			  { multiple_superclass_declarations, L } );

		key_not_found ->
			%trace_utils:warning( "no superclass specified" ),
			{ [], ClassInfo#class_info{ superclasses={ [], undefined } } }

			%wooper_internals:raise_error( superclasses_not_defined )

		end,

	% We now have to implement what used to be:

	% Static method (i.e. a mere function) that returns the list of the
	% superclasses of that class.
	%
	% Generally not to be called by the user, see getSuperclasses/1 instead.
	%
	%-spec get_superclasses() -> [ wooper:classname() ].
	%get_superclasses() ->
	%	?superclasses.

	% Now to be auto-generated and to become:

	%get_superclasses() ->
	%	wooper:static( Superclasses ).
	%
	% i.e. actually:
	%get_superclasses() ->
	%	Superclasses.

	Line = 0,

	GetSupName = get_superclasses,
	GetSupArity = 0,

	GeSupFunId = { GetSupName, GetSupArity },

	GetSupSpecForm = { attribute, Line, spec, { GeSupFunId,
				[ { type, Line, 'fun',
					[ { type, Line, product, [] },
					  { type, Line, list,
						[ { remote_type, Line,
							[ {atom,Line,wooper},
							  {atom,Line,classname}, [] ] } ] } ] } ] } },

	% Already checked to be a list of atoms:
	ClassesForm = ast_generation:list_atoms( Superclasses ),

	% Will end up in a { function, Line, GetSupName, GetSupArity, [ GetSupClause
	% ]... } form:
	%
	% Note: we could have directly returned the list L of superclasses, but we
	% prefer returning wooper:return_static( L ) so that the WOOPER parse
	% transform detects this function as all other static methods.
	%
	GetSupClause = { clause, Line, [], [], [
		{ call, Line, { remote, Line, {atom,Line,wooper},
						{atom,Line,return_static} },
		  [ ClassesForm ] } ] } ,

	% Where this generated get_superclasses/0 will be defined:
	DefinitionLoc =
		ast_info:get_default_definition_function_location( MarkerTable ),

	% Where this generated get_superclasses/0 will be exported:
	ExportLoc = ast_info:get_default_export_function_location( MarkerTable ),

	GetSupInfo = #function_info{ name=GetSupName,
								 arity=GetSupArity,
								 location=DefinitionLoc,
								 line=Line,
								 clauses=[ GetSupClause ],
								 spec={ DefinitionLoc, GetSupSpecForm },
								 callback=false,
								 exported=[ ExportLoc ] },

	% Ensure not already defined (ex: by an unwary user):
	NewFunctionTable = table:addNewEntry( GeSupFunId, GetSupInfo,
										  FunctionTable ),

	RegisteredClassInfo#class_info{ functions=NewFunctionTable }.
