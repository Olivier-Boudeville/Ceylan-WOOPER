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


% Centralizes, on behalf of the WOOPER parse transform, the support for instance
% construction.
%
-module(wooper_instance_construction).


-export([ manage_constructors/1 ]).


% For the function_info record:
-include("ast_info.hrl").

% For the class_info record:
-include("wooper_info.hrl").


% Shorthands:

-type line() :: ast_base:line().
-type form_element() :: ast_base:form_element().
-type compose_pair() :: wooper_parse_transform:compose_pair().
-type operator_table() :: wooper_parse_transform:operator_table().



% Extracts the constructors found in the specified function table, and
% interprets them to enrich the specified class information.
%
% Returns an updated pair thereof.
%
-spec manage_constructors( compose_pair() ) -> compose_pair().
manage_constructors( { FunctionTable, ClassInfo } ) ->

	% First element is a list of { arity(), function_info() } pairs
	% corresponding to the defined constructors (construct/N) , while the second
	% element is the input function table obtained once these corresponding
	% entries have been removed:
	%
	{ ConstructPairs, ShrunkFunctionTable } =
		extract_constructors_from( FunctionTable ),

	case ConstructPairs of

		[] ->
			% Better than throw( no_constructor_found ):
			NewErrors = [ no_constructor_found | ClassInfo#class_info.errors ],
			{ FunctionTable, ClassInfo#class_info{ errors=NewErrors } };

		_ ->
			%trace_utils:debug_fmt( "~B constructor(s) found.",
			%					   [ length( ConstructPairs ) ] ),

			% Returns { NewFunctionTable, NewClassInfo }:
			manage_new_operators( ConstructPairs, ShrunkFunctionTable,
								  ClassInfo )

	end.



% Returns a list of { arity(), function_info() } pairs and the shrunk function
% table from which they were extracted.
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
					  ClassInfo=#class_info{
								   class={ Classname, _ClassLocForm },
								   constructors=Constructors,
								   new_operators=NewOpTable,
								   debug_mode=IsDebugMode,
								   markers=MarkerTable  } ) ->

	%trace_utils:debug_fmt( "Processing constructor of arity ~B: ~s",
	%			   [ Arity, ast_info:function_info_to_string( FunInfo ) ] ),

	% Where the generated 'new*' operators (and possibly the constructor) will
	% be exported:
	%
	ExportLoc = ast_info:get_default_export_function_location( MarkerTable ),

	% First, for the class developer, exporting a constructor is not mandatory;
	% so, if this constructor is not exported, let's do it automatically:
	%
	NewFunInfo = wooper_method_management:ensure_exported( FunInfo,
														   MarkerTable ),


	% Registering this (possibly updated) constructor in the dedicated table:
	NewConstructors = table:addNewEntry( _K=Arity, _V=NewFunInfo,
										 Constructors ),

	% Then, for a constructor of arity N, we have to automatically define and
	% export here following 14 functions, which are all new operator variations
	% V (7 base ones, each doubled to support whether or not an atomic link is
	% wanted as well between the creator process and the created instance):
	%
	% - V1: new/N-1 and new_link/N-1 (N-1, as no State parameter expected here)
	% - V2: synchronous_new/N-1 and synchronous_new_link/N-1
	% - V3: synchronous_timed_new/N-1 and synchronous_timed_new_link/N-1
	% - V4: remote_new/N and remote_new_link/N
	% - V5: remote_synchronous_new/N and remote_synchronous_new_link/N
	% - V6: remote_synchronisable_new/N and remote_synchronisable_new_link/N
	% - V7: remote_synchronous_timed_new/N and
	%       remote_synchronous_timed_new_link/N


	% Where the generated 'new*' operators will be defined:
	DefinitionLoc = ast_info:get_default_definition_function_location(
					  MarkerTable ),

	V1OpTable = add_v1_operators( Classname, Arity, ExportLoc, DefinitionLoc,
								  IsDebugMode, NewOpTable ),

	V2OpTable = add_v2_operators( Classname, Arity, ExportLoc, DefinitionLoc,
								  IsDebugMode, V1OpTable ),

	V3OpTable = add_v3_operators( Classname, Arity, ExportLoc, DefinitionLoc,
								  IsDebugMode, V2OpTable ),

	V4OpTable = add_v4_operators( Classname, Arity, ExportLoc, DefinitionLoc,
								  IsDebugMode, V3OpTable ),

	V5OpTable = add_v5_operators( Classname, Arity, ExportLoc, DefinitionLoc,
								  IsDebugMode, V4OpTable ),

	V6OpTable = add_v6_operators( Classname, Arity, ExportLoc, DefinitionLoc,
								  IsDebugMode, V5OpTable ),

	V7OpTable = add_v7_operators( Classname, Arity, ExportLoc, DefinitionLoc,
								  IsDebugMode, V6OpTable ),

	FinalClassInfo = ClassInfo#class_info{ constructors=NewConstructors,
										   new_operators=V7OpTable },

	manage_new_operators( T, FunctionTable, FinalClassInfo ).




% Adds the V1 operators, i.e. new/N-1 and new_link/N-1, by updating the
% specified operator table.
%
-spec add_v1_operators( wooper:classname(), arity(), ast_base:form_location(),
			ast_base:form_location(), boolean(), operator_table() ) ->
							  operator_table().
add_v1_operators( Classname, Arity, ExportLocation, DefinitionLoc, _IsDebugMode,
				  OperatorTable ) ->

	% Let's create from scratch the corresponding operator definition, export
	% and spec:
	%
	% (Arity N is the one of the corresponding constructor)

	NewArity = Arity - 1,

	%trace_utils:debug_fmt( "Adding {new,new_link}/~B.", [ NewArity ] ),

	% Let's start with new/N-1:

	NewName = new,

	NewId = { NewName, NewArity },

	% Conventional virtual line to denote generated code:
	Line = 0,

	% Its definition is, if N=3 (hence NewArity=2):
	%
	% new( A, B ) ->
	%     spawn( fun() ->
	%			   wooper:construct_and_run( class_Foo, [ A, B ] )
	%            end ).


	% Preparing the form elements involved:


	% For the header of the new function, i.e. for 'new( A, B ) ->'.
	%
	% Ex: [ {var,0,'Myriad_Param_1'}, {var,0,'Myriad_Param_2'} ]
	%
	HeaderParams = ast_generation:get_header_params( NewArity ),

	% For the call to wooper:construct_and_run/2:
	RunCall = get_run_call( Line ),

	% For the application of the parameters to the later function,
	% i.e. for 'wooper:construct_and_run( class_Foo, [ A, B ] )'.
	%
	% Ex: [ {atom,0,class_Foo}, { cons, 0, {var,0,'Myriad_Param_1'},
	% { cons, 0, {var,0,'Myriad_Param_2'}, {nil,0} } } ].
	%
	CallParams = [ {atom,Line,Classname},
				   ast_generation:list_variables( NewArity ) ],

	NewClause = { clause, Line, HeaderParams, [],
				  [{ call, Line, {atom,Line,spawn},
					 [ {'fun', Line,
						{ clauses,
						  [ { clause, Line,[],[],
							  [ { call, Line, RunCall, CallParams } ]
							}]
						}}]
				   }]},


	% We also generate the corresponding spec, which is:
	%
	% '-spec new( wooper:construction_parameter(),
	%             wooper:construction_parameter() ) -> pid().':

	% First listing the types of the expected parameters:
	ConstructParamTypes = get_construction_types( NewArity, Line ),

	PidType = forge_pid_type(),

	NewSpecForm = { attribute, Line, spec, { NewId,
	   [ { type, Line, 'fun',
		   [ { type, Line, product, ConstructParamTypes },
			 _Result=PidType ]
		 } ] } },

	NewOpInfo = #function_info{ name=NewName,
								arity=NewArity,
								location=DefinitionLoc,
								line=Line,
								clauses=[ NewClause ],
								% Spec close to definition:
								spec={ DefinitionLoc, NewSpecForm },
								callback=false,
								exported=[ ExportLocation ] },

	% Now, let's do the same for the new_link counterpart:

	NewLinkName = new_link,

	NewLinkId = { NewLinkName, NewArity },

	% Its definition is, if N=3 (hence NewArity=2):
	%
	% new_link( A, B ) ->
	%     spawn_link( fun() ->
	%		  wooper:construct_and_run( class_Foo, [ A, B ] )
	% end ).

	NewLinkClause = { clause, Line, HeaderParams, [],
					  [{ call, Line, {atom,Line,spawn_link},
						 [ {'fun', Line,
							{ clauses,
							  [ { clause, Line,[],[],
								  [ { call, Line, RunCall, CallParams } ]
								}]
							}}]
					   }]},

	NewLinkSpecForm = { attribute, Line, spec, { NewLinkId,
	   [ { type, Line, 'fun',
		   [ { type, Line, product, ConstructParamTypes },
			 _Result=PidType ]
		 } ] } },

	NewLinkOpInfo = #function_info{ name=NewLinkName,
									arity=NewArity,
									location=DefinitionLoc,
									line=Line,
									clauses=[ NewLinkClause ],
									% Spec close to definition:
									spec={ DefinitionLoc, NewLinkSpecForm },
									callback=false,
									exported=[ ExportLocation ] },

	% Ensure not already defined (ex: by an unwary user):
	table:addNewEntries( [ { NewId, NewOpInfo }, { NewLinkId, NewLinkOpInfo } ],
						 OperatorTable ).



% Adds the V2 operators, i.e. synchronous_new/N-1 and synchronous_new_link/N-1,
% by updating the specified operator table; they correspond roughly to the V1
% ones, augmented with a receive clause.
%
-spec add_v2_operators( wooper:classname(), arity(), ast_base:form_location(),
			ast_base:form_location(), boolean(), operator_table() ) ->
							  operator_table().
add_v2_operators( Classname, Arity, ExportLocation, DefinitionLoc, _IsDebugMode,
				  OperatorTable ) ->

	% Same strategy as add_v1_operators/5:

	SyncNewArity = Arity - 1,

	%trace_utils:debug_fmt( "Adding {synchronous_new,synchronous_new_link}/~B.",
	%					   [ SyncNewArity ] ),

	% Let's start with:
	SyncNewName = synchronous_new,

	SyncNewId = { SyncNewName, SyncNewArity },

	Line = 0,

	% Its definition is, if N=3 (hence SyncNewArity=2):
	% ('S' standing for statement)
	%
	% synchronous_new( A, B ) ->
	% [S1]  CreatorPid = self(),
	% [S2]  SpawnedPid = spawn( fun() ->
	%		  wooper:construct_and_run_synchronous( class_Foo, [ A, B ],
	%                                               CreatorPid )
	%                       end ),
	%
	% [S3]  receive
	%
	%		  { spawn_successful, SpawnedPid } ->
	%			  SpawnedPid
	%
	% end.


	S1 = { match, Line, {var,Line,'CreatorPid'},
		   { call, Line, {atom,Line,self}, [] } },


	% For the call to wooper:construct_and_run_synchronous/2:
	SyncRunCall = get_sync_run_call( Line ),

	CallParams = [ {atom,Line,Classname},
				   ast_generation:list_variables( SyncNewArity ),
				   {var,Line,'CreatorPid'} ],

	S2 = { match, Line, {var,Line,'SpawnedPid'},
		   { call, Line, {atom,Line,spawn},
			[ {'fun',Line,
			  { clauses,
				[ {clause,Line,[],[],
				   [ {call,Line,SyncRunCall,CallParams} ] } ] } } ] } },

	S3 = get_receive( Line ),

	HeaderParams = ast_generation:get_header_params( SyncNewArity ),

	SyncNewClause = { clause, Line, HeaderParams, [],
					  [ S1, S2, S3 ] },


	% Then, its spec:

	ConstructParamTypes = get_construction_types( SyncNewArity, Line ),

	PidType = forge_pid_type(),

	SyncNewSpecForm = { attribute, Line, spec, { SyncNewId,
	   [ { type, Line, 'fun',
		   [ { type, Line, product, ConstructParamTypes },
			 _Result=PidType ]
		 } ] } },

	SyncNewOpInfo = #function_info{ name=SyncNewName,
									arity=SyncNewArity,
									location=DefinitionLoc,
									line=Line,
									clauses=[ SyncNewClause ],
									spec={ DefinitionLoc, SyncNewSpecForm },
									callback=false,
									exported=[ ExportLocation ] },


	% Next, roughly the same but with a link:

	SyncNewLinkName = synchronous_new_link,

	SyncNewLinkId = { SyncNewLinkName, SyncNewArity },

	S2Link = { match, Line, {var,Line,'SpawnedPid'},
			   { call, Line, {atom,Line,spawn_link},
				 [ {'fun',Line,
					{ clauses,
					  [ {clause,Line,[],[],
						 [ {call,Line,SyncRunCall,CallParams} ] } ] } } ] } },

	SyncNewLinkClause = { clause, Line, HeaderParams, [],
						  [ S1, S2Link, S3 ] },

	SyncNewLinkSpecForm = { attribute, Line, spec, { SyncNewLinkId,
	   [ { type, Line, 'fun',
		   [ { type, Line, product, ConstructParamTypes },
			 _Result=PidType ]
		 } ] } },

	SyncNewLinkOpInfo = #function_info{
						   name=SyncNewLinkName,
						   arity=SyncNewArity,
						   location=DefinitionLoc,
						   line=Line,
						   clauses=[ SyncNewLinkClause ],
						   spec={ DefinitionLoc, SyncNewLinkSpecForm },
						   callback=false,
						   exported=[ ExportLocation ] },

	% Ensure not already defined (ex: by an unwary user):
	table:addNewEntries( [ { SyncNewId, SyncNewOpInfo },
						   { SyncNewLinkId, SyncNewLinkOpInfo } ],
						 OperatorTable ).



% Adds the V3 operators, i.e. synchronous_timed_new/N-1 and
% synchronous_timed_new_link/N-1, by updating the specified operator table; they
% correspond roughly to the V2 ones, augmented with an after clause.
%
-spec add_v3_operators( wooper:classname(), arity(), ast_base:form_location(),
			ast_base:form_location(), boolean(), operator_table() ) ->
							  operator_table().
add_v3_operators( Classname, Arity, ExportLocation, DefinitionLoc, IsDebugMode,
				  OperatorTable ) ->

	% Same strategy as add_v2_operators/5:

	OpArity = Arity - 1,

	%trace_utils:debug_fmt(
	%  "Adding {synchronous_timed_new,synchronous_timed_new_link}/~B.",
	%  [ OpArity ] ),

	% Let's start with:
	OpNewName = synchronous_timed_new,

	OpNewId = { OpNewName, OpArity },

	Line = 0,

	% Its definition is, if N=3 (hence OpArity=2):
	% ('S' standing for statement)
	%
	% synchronous_timed_new( A, B ) ->
	% [S1]  CreatorPid = self(),
	% [S2]  SpawnedPid = spawn( fun() ->
	%		  wooper:construct_and_run_synchronous( class_Foo, [ A, B ],
	%                                               CreatorPid )
	%                           end ),
	%
	% [S3]  receive
	%
	%		  { spawn_successful, SpawnedPid } ->
	%			  SpawnedPid
	%
	% [S4]  after 5000 ->
	%
	%			throw( { synchronous_time_out, ?MODULE } )
	%
	%       end.


	S1 = { match, Line, {var,Line,'CreatorPid'},
		   { call, Line, {atom,Line,self}, [] } },


	% For the call to wooper:construct_and_run_synchronous/2:
	SyncRunCall = get_sync_run_call( Line ),

	CallParams = [ {atom,Line,Classname},
				   ast_generation:list_variables( OpArity ),
				   {var,Line,'CreatorPid'} ],

	S2 = { match, Line, {var,Line,'SpawnedPid'},
		   { call, Line, {atom,Line,spawn},
			 [ {'fun', Line,
				{ clauses,
				  [ { clause,Line,[],[],
					  [ { call,Line,SyncRunCall,CallParams } ] } ] } } ] } },

	S3 = get_local_receive_with_after( Classname, IsDebugMode, Line ),

	HeaderParams = ast_generation:get_header_params( OpArity ),

	OpNewClause = { clause, Line, HeaderParams, [], [ S1, S2, S3 ] },


	% Then, its spec:

	ConstructParamTypes = get_construction_types( OpArity, Line ),

	PidType = forge_pid_type(),

	OpSpecForm = { attribute, Line, spec, { OpNewId,
	   [ { type, Line, 'fun',
		   [ { type, Line, product, ConstructParamTypes },
			 _Result=PidType ]
		 } ] } },

	OpNewInfo = #function_info{ name=OpNewName,
								arity=OpArity,
								location=DefinitionLoc,
								line=Line,
								clauses=[ OpNewClause ],
								spec={ DefinitionLoc, OpSpecForm },
								callback=false,
								exported=[ ExportLocation ] },


	% Next, roughly the same but with a link:

	OpNewLinkName = synchronous_timed_new_link,

	OpNewLinkId = { OpNewLinkName, OpArity },

	S2Link = { match, Line, {var,Line,'SpawnedPid'},
			   { call, Line, {atom,Line,spawn_link},
				 [ {'fun',Line,
					{ clauses,
					  [ {clause,Line,[],[],
						 [ {call,Line,SyncRunCall,CallParams } ] } ] } } ] } },

	OpNewLinkClause = { clause, Line, HeaderParams, [], [ S1, S2Link, S3 ] },

	OpLinkSpecForm = { attribute, Line, spec, { OpNewLinkId,
	   [ { type, Line, 'fun',
		   [ { type, Line, product, ConstructParamTypes }, _Result=PidType ]
		 } ] } },

	OpNewLinkInfo = #function_info{ name=OpNewLinkName,
									arity=OpArity,
									location=DefinitionLoc,
									line=Line,
									clauses=[ OpNewLinkClause ],
									spec={ DefinitionLoc, OpLinkSpecForm },
									callback=false,
									exported=[ ExportLocation ] },

	% Ensure not already defined (ex: by an unwary user):
	table:addNewEntries( [ { OpNewId, OpNewInfo },
						   { OpNewLinkId, OpNewLinkInfo } ], OperatorTable ).



% Adds the V4 operators, i.e. remote_new/N and remote_new_link/N by updating the
% specified operator table; they correspond roughly to the V1 ones, augmented
% with a node specification at the spawn call.
%
-spec add_v4_operators( wooper:classname(), arity(), ast_base:form_location(),
			ast_base:form_location(), boolean(), operator_table() ) ->
							  operator_table().
add_v4_operators( Classname, Arity, ExportLocation, DefinitionLoc, _IsDebugMode,
				  OperatorTable ) ->

	% Overall arity (with node()):
	OpArity = Arity,

	% For the actual parameters (without node()):
	ArgArity = OpArity - 1,

	%trace_utils:debug_fmt( "Adding {remote_new,remote_new_link}/~B.",
	%					   [ OpArity ] ),

	% Let's start with:
	OpNewName = remote_new,

	OpNewId = { OpNewName, OpArity },

	Line = 0,

	% Its definition is, if N=3 (hence OpArity=3):
	% ('S' standing for statement)
	%
	% remote_new( Node, A, B ) ->
	% [S1]  spawn( Node, fun() ->
	%		  wooper:construct_and_run( class_Foo, [ A, B ] )
	%                    end ).

	% First element must be the node parameter:
	HeaderParams =
		[ {var,Line,'Node'} | ast_generation:get_header_params( ArgArity ) ],

	RunCall = get_run_call( Line ),

	CallParams = [ {atom,Line,Classname},
				   ast_generation:list_variables( ArgArity ) ],

	OpNewClause = { clause, Line, HeaderParams, [],
					[{ call, Line, {atom,Line,spawn},
					   [ {var,Line,'Node'},
						 {'fun', Line,
						  { clauses,
							[ { clause, Line,[],[],
								[ { call, Line, RunCall, CallParams } ]
							  }]
						  }}]
					 }]},


	% Now the spec, which is:
	%
	% -spec remote_new( net_utils:atom_node_name(),
	%                   wooper:construction_parameter(),
	%                   wooper:construction_parameter() ) -> pid().

	NodeType = forge_node_type( Line ),

	ConstructParamTypes =
		[ NodeType | get_construction_types( ArgArity, Line ) ],

	PidType = forge_pid_type(),

	OpSpecForm = { attribute, Line, spec, { OpNewId,
	   [ { type, Line, 'fun',
		   [ { type, Line, product, ConstructParamTypes },
			 _Result=PidType ]
		 } ] } },

	OpNewInfo = #function_info{ name=OpNewName,
								arity=OpArity,
								location=DefinitionLoc,
								line=Line,
								clauses=[ OpNewClause ],
								spec={ DefinitionLoc, OpSpecForm },
								callback=false,
								exported=[ ExportLocation ] },


	% Next, roughly the same but with a link:

	OpNewLinkName = remote_new_link,

	OpNewLinkId = { OpNewLinkName, OpArity },

	OpNewLinkClause = { clause, Line, HeaderParams, [],
					[{ call, Line, {atom,Line,spawn_link},
					   [ {var,Line,'Node'},
						 {'fun', Line,
						  { clauses,
							[ { clause, Line,[],[],
								[ { call, Line, RunCall, CallParams } ]
							  }]
						  }}]
					 }]},

	OpLinkSpecForm = { attribute, Line, spec, { OpNewLinkId,
	   [ { type, Line, 'fun',
		   [ { type, Line, product, ConstructParamTypes },
			 _Result=PidType ]
		 } ] } },

	OpNewLinkInfo = #function_info{ name=OpNewLinkName,
									arity=OpArity,
									location=DefinitionLoc,
									line=Line,
									clauses=[ OpNewLinkClause ],
									spec={ DefinitionLoc, OpLinkSpecForm },
									callback=false,
									exported=[ ExportLocation ] },

	% Ensure not already defined (ex: by an unwary user):
	table:addNewEntries( [ { OpNewId, OpNewInfo },
						   { OpNewLinkId, OpNewLinkInfo } ], OperatorTable ).



% Adds the V5 operators, i.e. remote_synchronous_new/N and
% remote_synchronous_new_link/N, by updating the specified operator table; they
% correspond roughly to the V4 ones, augmented with a synchronous variant and a
% receive clause.
%
-spec add_v5_operators( wooper:classname(), arity(), ast_base:form_location(),
			ast_base:form_location(), boolean(), operator_table() ) ->
							  operator_table().
add_v5_operators( Classname, Arity, ExportLocation, DefinitionLoc, _IsDebugMode,
				  OperatorTable ) ->

	% Overall arity (with node()):
	OpArity = Arity,

	% For the actual parameters (without node()):
	ArgArity = OpArity - 1,

	%trace_utils:debug_fmt(
	%  "Adding {remote_synchronous_new,remote_synchronous_new_link}/~B.",
	%  [ OpArity ] ),

	% Let's start with:
	OpNewName = remote_synchronous_new,

	OpNewId = { OpNewName, OpArity },

	Line = 0,

	% Its definition is, if N=3 (hence OpArity=3):
	% ('S' standing for statement)
	%
	% remote_synchronous_new( Node, A, B ) ->
	%
	% [S1] CreatorPid = self(),
	% [S2] SpawnedPid = spawn( Node, fun() ->
	%					   wooper:construct_and_run_synchronous(
	%									 [ A, B ], CreatorPid )
	%								 end ),
	% [S3] receive
	%		  { spawn_successful, SpawnedPid } ->
	%			  SpawnedPid
	%	   end.

	S1 = { match, Line, {var,Line,'CreatorPid'},
		   { call, Line, {atom,Line,self}, [] } },

	SyncRunCall = get_sync_run_call( Line ),

	CallParams = [ {atom,Line,Classname},
				   ast_generation:list_variables( ArgArity ),
				   {var,Line,'CreatorPid'} ],

	S2 = { match, Line, {var,Line,'SpawnedPid'},
		   { call, Line, {atom,Line,spawn},
			 [ {var,Line,'Node'},
			   {'fun', Line,
				{ clauses,
				  [ { clause,Line,[],[],
					  [ { call,Line,SyncRunCall,CallParams },
						{var,Line,'CreatorPid'}
					  ] } ] } } ] } },

	S3 = get_receive( Line ),

	% First element must be the node parameter:
	HeaderParams =
		[ {var,Line,'Node'} | ast_generation:get_header_params( ArgArity ) ],

	OpNewClause = { clause, Line, HeaderParams, [],
					  [ S1, S2, S3 ] },


	% Now the spec, which is:
	%
	% -spec remote_synchronous_new( net_utils:atom_node_name(),
	%                   wooper:construction_parameter(),
	%                   wooper:construction_parameter() ) -> pid().

	NodeType = forge_node_type( Line ),

	ConstructParamTypes =
		[ NodeType | get_construction_types( ArgArity, Line ) ],

	PidType = forge_pid_type(),

	OpSpecForm = { attribute, Line, spec, { OpNewId,
	   [ { type, Line, 'fun',
		   [ { type, Line, product, ConstructParamTypes },
			 _Result=PidType ]
		 } ] } },

	OpNewInfo = #function_info{ name=OpNewName,
								arity=OpArity,
								location=DefinitionLoc,
								line=Line,
								clauses=[ OpNewClause ],
								spec={ DefinitionLoc, OpSpecForm },
								callback=false,
								exported=[ ExportLocation ] },


	% Next, roughly the same but with a link:

	OpNewLinkName = remote_synchronous_new_link,

	OpNewLinkId = { OpNewLinkName, OpArity },

	S2Link = { match, Line, {var,Line,'SpawnedPid'},
			   { call, Line, {atom,Line,spawn_link},
				 [ {var,Line,'Node'},
				   {'fun', Line,
					{ clauses,
					  [ { clause,Line,[],[],
						  [ { call,Line,SyncRunCall,CallParams },
							{var,Line,'CreatorPid'}
						  ] } ] } } ] } },

	OpNewLinkClause = { clause, Line, HeaderParams, [], [ S1, S2Link, S3 ] },


	OpLinkSpecForm = { attribute, Line, spec, { OpNewLinkId,
	   [ { type, Line, 'fun',
		   [ { type, Line, product, ConstructParamTypes },
			 _Result=PidType ]
		 } ] } },

	OpNewLinkInfo = #function_info{ name=OpNewLinkName,
									arity=OpArity,
									location=DefinitionLoc,
									line=Line,
									clauses=[ OpNewLinkClause ],
									spec={ DefinitionLoc, OpLinkSpecForm },
									callback=false,
									exported=[ ExportLocation ] },

	% Ensure not already defined (ex: by an unwary user):
	table:addNewEntries( [ { OpNewId, OpNewInfo },
						   { OpNewLinkId, OpNewLinkInfo } ], OperatorTable ).



% Adds the V6 operators, i.e. remote_synchronisable_new/N and
% remote_synchronisable_new_link/N, by updating the specified operator table;
% they correspond roughly to the V5 ones, except there is no integrated receive,
% as it is left at the hand of the user.
%
-spec add_v6_operators( wooper:classname(), arity(), ast_base:form_location(),
			ast_base:form_location(), boolean(), operator_table() ) ->
							  operator_table().
add_v6_operators( Classname, Arity, ExportLocation, DefinitionLoc, _IsDebugMode,
				  OperatorTable ) ->

	% Overall arity (with node()):
	OpArity = Arity,

	% For the actual parameters (without node()):
	ArgArity = OpArity - 1,

	%trace_utils:debug_fmt(
	%  "Adding {remote_synchronisable_new,remote_synchronisable_new_link}/~B.",
	%  [ OpArity ] ),

	% Let's start with:
	OpNewName = remote_synchronisable_new,

	OpNewId = { OpNewName, OpArity },

	Line = 0,

	% Its definition is, if N=3 (hence OpArity=3):
	% ('S' standing for statement)
	%
	% remote_synchronisable_new( Node, A, B ) ->
	%
	% [S1] CreatorPid = self(),
	% [S2] spawn( Node, fun() ->
	%					   wooper:construct_and_run_synchronous(
	%									 [ A, B ], CreatorPid )
	%	   end ).

	S1 = { match, Line, {var,Line,'CreatorPid'},
		   { call, Line, {atom,Line,self}, [] } },

	SyncRunCall = get_sync_run_call( Line ),

	CallParams = [ {atom,Line,Classname},
				   ast_generation:list_variables( ArgArity ),
				   {var,Line,'CreatorPid'} ],

	S2 = { call, Line, {atom,Line,spawn},
			 [ {var,Line,'Node'},
			   {'fun', Line,
				{ clauses,
				  [ { clause,Line,[],[],
					  [ { call,Line,SyncRunCall,CallParams },
						{var,Line,'CreatorPid'}
					  ] } ] } } ] },

	% First element must be the node parameter:
	HeaderParams = [ {var,Line,'Node'}
					 | ast_generation:get_header_params( ArgArity ) ],

	OpNewClause = { clause, Line, HeaderParams, [], [ S1, S2 ] },


	% Now the spec, which is:
	%
	% -spec remote_synchronisable_new( net_utils:atom_node_name(),
	%                   wooper:construction_parameter(),
	%                   wooper:construction_parameter() ) -> pid().

	NodeType = forge_node_type( Line ),

	ConstructParamTypes =
		[ NodeType | get_construction_types( ArgArity, Line ) ],

	PidType = forge_pid_type(),

	OpSpecForm = { attribute, Line, spec, { OpNewId,
	   [ { type, Line, 'fun',
		   [ { type, Line, product, ConstructParamTypes },
			 _Result=PidType ]
		 } ] } },

	OpNewInfo = #function_info{ name=OpNewName,
								arity=OpArity,
								location=DefinitionLoc,
								line=Line,
								clauses=[ OpNewClause ],
								spec={ DefinitionLoc, OpSpecForm },
								callback=false,
								exported=[ ExportLocation ] },


	% Next, roughly the same but with a link:

	OpNewLinkName = remote_synchronisable_new_link,

	OpNewLinkId = { OpNewLinkName, OpArity },

	S2Link = { call, Line, {atom,Line,spawn_link},
				 [ {var,Line,'Node'},
				   {'fun', Line,
					{ clauses,
					  [ { clause,Line,[],[],
						  [ { call,Line,SyncRunCall,CallParams },
							{var,Line,'CreatorPid'}
						  ] } ] } } ] },

	OpNewLinkClause = { clause, Line, HeaderParams, [], [ S1, S2Link ] },


	OpLinkSpecForm = { attribute, Line, spec, { OpNewLinkId,
	   [ { type, Line, 'fun',
		   [ { type, Line, product, ConstructParamTypes },
			 _Result=PidType ]
		 } ] } },

	OpNewLinkInfo = #function_info{ name=OpNewLinkName,
									arity=OpArity,
									location=DefinitionLoc,
									line=Line,
									clauses=[ OpNewLinkClause ],
									spec={ DefinitionLoc, OpLinkSpecForm },
									callback=false,
									exported=[ ExportLocation ] },

	% Ensure not already defined (ex: by an unwary user):
	table:addNewEntries( [ { OpNewId, OpNewInfo },
						   { OpNewLinkId, OpNewLinkInfo } ], OperatorTable ).



% Adds the V7 operators, i.e. remote_synchronous_timed_new/N and
% remote_synchronous_timed_new/N, by updating the specified operator table; they
% correspond roughly to a version of V5 with an additional after clause.
%
-spec add_v7_operators( wooper:classname(), arity(), ast_base:form_location(),
			ast_base:form_location(), boolean(), operator_table() ) ->
							  operator_table().
add_v7_operators( Classname, Arity, ExportLocation, DefinitionLoc, IsDebugMode,
				  OperatorTable ) ->

	% Overall arity (with node()):
	OpArity = Arity,

	% For the actual parameters (without node()):
	ArgArity = OpArity - 1,

	%trace_utils:debug_fmt( "Adding {remote_synchronous_timed_new,"
	%					   "remote_synchronous_timed_new_link}/~B.",
	%					   [ OpArity ] ),

	% Let's start with:
	OpNewName = remote_synchronous_timed_new,

	OpNewId = { OpNewName, OpArity },

	Line = 0,

	% Its definition is, if N=3 (hence OpArity=3):
	% ('S' standing for statement)
	%
	% remote_synchronous_timed_new( Node, A, B ) ->
	%
	% [S1] CreatorPid = self(),
	% [S2] SpawnedPid = spawn( Node, fun() ->
	%					   wooper:construct_and_run_synchronous(
	%									 [ A, B ], CreatorPid )
	%								 end ),
	% [S3] receive
	%		  { spawn_successful, SpawnedPid } ->
	%			  SpawnedPid
	%      after TimeOut ->
	%		  throw( { remote_synchronous_time_out, Node, Classname } )
	%      end.

	S1 = { match, Line, {var,Line,'CreatorPid'},
		   { call, Line, {atom,Line,self}, [] } },

	SyncRunCall = get_sync_run_call( Line ),

	CallParams = [ {atom,Line,Classname},
				   ast_generation:list_variables( ArgArity ),
				   {var,Line,'CreatorPid'} ],

	S2 = { match, Line, {var,Line,'SpawnedPid'},
		   { call, Line, {atom,Line,spawn},
			 [ {var,Line,'Node'},
			   {'fun', Line,
				{ clauses,
				  [ { clause,Line,[],[],
					  [ { call,Line,SyncRunCall,CallParams },
						{var,Line,'CreatorPid'}
					  ] } ] } } ] } },

	S3 = get_remote_receive_with_after( Classname, IsDebugMode, Line ),

	% First element must be the node parameter:
	HeaderParams =
		[ {var,Line,'Node'} | ast_generation:get_header_params( ArgArity ) ],

	OpNewClause = { clause, Line, HeaderParams, [], [ S1, S2, S3 ] },


	% Now the spec, which is:
	%
	% -spec remote_synchronous_timed_new( net_utils:atom_node_name(),
	%                   wooper:construction_parameter(),
	%                   wooper:construction_parameter() ) -> pid().

	NodeType = forge_node_type( Line ),

	ConstructParamTypes =
		[ NodeType | get_construction_types( ArgArity, Line ) ],

	PidType = forge_pid_type(),

	OpSpecForm = { attribute, Line, spec, { OpNewId,
	   [ { type, Line, 'fun',
		   [ { type, Line, product, ConstructParamTypes },
			 _Result=PidType ]
		 } ] } },

	OpNewInfo = #function_info{ name=OpNewName,
								arity=OpArity,
								location=DefinitionLoc,
								line=Line,
								clauses=[ OpNewClause ],
								spec={ DefinitionLoc, OpSpecForm },
								callback=false,
								exported=[ ExportLocation ] },


	% Next, roughly the same but with a link:

	OpNewLinkName = remote_synchronous_timed_new_link,

	OpNewLinkId = { OpNewLinkName, OpArity },

	S2Link = { match, Line, {var,Line,'SpawnedPid'},
			   { call, Line, {atom,Line,spawn_link},
				 [ {var,Line,'Node'},
				   {'fun', Line,
					{ clauses,
					  [ { clause,Line,[],[],
						  [ { call,Line,SyncRunCall,CallParams },
							{var,Line,'CreatorPid'}
						  ] } ] } } ] } },

	OpNewLinkClause = { clause, Line, HeaderParams, [], [ S1, S2Link, S3 ] },


	OpLinkSpecForm = { attribute, Line, spec, { OpNewLinkId,
	   [ { type, Line, 'fun',
		   [ { type, Line, product, ConstructParamTypes },
			 _Result=PidType ]
		 } ] } },

	OpNewLinkInfo = #function_info{ name=OpNewLinkName,
									arity=OpArity,
									location=DefinitionLoc,
									line=Line,
									clauses=[ OpNewLinkClause ],
									spec={ DefinitionLoc, OpLinkSpecForm },
									callback=false,
									exported=[ ExportLocation ] },

	% Ensure not already defined (ex: by an unwary user):
	table:addNewEntries( [ { OpNewId, OpNewInfo },
						   { OpNewLinkId, OpNewLinkInfo } ], OperatorTable ).



% Returns the form element corresponding to pid().
%
forge_pid_type() ->
	ast_type:forge_pid_type().


% Returns the form element corresponding to net_utils:atom_node_name().
%
forge_node_type( Line ) ->
	% Corresponds to node():
	ast_type:forge_remote_type( _ModuleName=net_utils, _TypeName=atom_node_name,
								_TypeVars=[], Line ).



% Returns the form element corresponding to wooper:construct_and_run/2.
%
-spec get_run_call( line() ) -> form_element().
get_run_call( Line ) ->
	{ remote, Line, {atom,Line,wooper}, {atom,Line,construct_and_run} }.



% Returns the form element corresponding to
% wooper:construct_and_run_synchronous/2.
%
-spec get_sync_run_call( line() ) -> form_element().
get_sync_run_call( Line ) ->
	{ remote, Line, {atom,Line,wooper},
	  {atom,Line,construct_and_run_synchronous} }.



% Returns the form element corresponding to:
%
% receive
%
%	{ spawn_successful, SpawnedPid } ->
%		SpawnedPid
%
% end
%
-spec get_receive( line() ) -> form_element().
get_receive( Line ) ->
	{ 'receive', Line, [
		{ clause, Line, [ {tuple,Line,[ {atom,Line,spawn_successful},
										{var,Line,'SpawnedPid'} ] } ],
		  [], [ {var,Line,'SpawnedPid'} ] } ] }.



% Returns the form element corresponding to:
%
% receive
%
%	{ spawn_successful, SpawnedPid } ->
%		SpawnedPid
%
% after 5000 ->
%
%	throw( { synchronous_time_out, ?MODULE } )
%
% end
%
-spec get_local_receive_with_after( basic_utils:module_name(),
							time_utils:time_out(), line() ) -> form_element().
get_local_receive_with_after( ModuleName, IsDebugMode, Line ) ->

	TimeOut = wooper:get_synchronous_time_out( IsDebugMode ),

	{ 'receive', Line, [
		{ clause, Line, [ {tuple,Line,[ {atom,Line,spawn_successful},
										{var,Line,'SpawnedPid'} ] } ],
		  [], [ {var,Line,'SpawnedPid'} ] } ],
	  {integer,Line,TimeOut},
	  [ { call, Line, {atom,Line,throw},
		   [ { tuple, Line,
			   [ {atom,Line,synchronous_time_out},
				 {atom,Line,ModuleName} ] } ] } ] }.



% Returns the form element corresponding to:
%
% receive
%
%	{ spawn_successful, SpawnedPid } ->
%		SpawnedPid
%
% afterTimeOut  ->
%
%	throw( { remote_synchronous_time_out, node(), ?MODULE } )
%
% end
%
-spec get_remote_receive_with_after( basic_utils:module_name(),
		   time_utils:time_out(), line() ) -> form_element().
get_remote_receive_with_after( ModuleName, IsDebugMode, Line ) ->

	TimeOut = wooper:get_synchronous_time_out( IsDebugMode ),

	{ 'receive', Line, [
		{ clause, Line, [ {tuple,Line,[ {atom,Line,spawn_successful},
										{var,Line,'SpawnedPid'} ] } ],
		  [], [ {var,Line,'SpawnedPid'} ] } ],
	  {integer,Line,TimeOut},
	  [ { call, Line, {atom,Line,throw},
		   [ { tuple, Line,
			   [ {atom,Line,remote_synchronous_time_out},
				 {var,Line,'Node'},
				 {atom,Line,ModuleName} ] } ] } ] }.




% Returns a list of adequate types for the specified number of construction
% parameters, i.e. of type wooper:construction_parameter().
%
-spec get_construction_types( basic_utils:count(), line() ) -> form_element().
get_construction_types( Count, Line ) ->
	Type = { remote_type, Line, [ {atom,Line,wooper},
								  {atom,Line,construction_parameter}, [] ] },
	lists:duplicate( Count, Type ).