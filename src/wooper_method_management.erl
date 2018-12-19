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


% Centralizes, on behalf of the WOOPER parse transform, the support for method
% management.
%
-module(wooper_method_management).


-export([ manage_methods/1, ensure_exported/2 ]).


% For the function_info record:
-include("ast_info.hrl").

% For the ast_transforms record:
-include("ast_transform.hrl").


% For the class_info record:
-include("wooper_info.hrl").


% The (WOOPER-level) nature of a given Erlang function.
%
-type function_nature() :: 'function' | 'request' | 'oneway' | 'static'.


% Shorthands:

-type line() :: ast_base:line().
-type function_info() :: ast_info:function_info().
-type marker_table() :: ast_info:section_marker_table().
-type function_table() :: ast_info:function_table().
-type compose_pair() :: wooper_parse_transform:compose_pair().
-type ast_expression() :: ast_expression:ast_expression().
-type transformation_state() :: ast_transform:transformation_state().



% A context used to better scan and interpret functions (of all sorts) within a
% class.
%
-record( scan_context, {

	% All the functions known of said class (before their dispatching in
	% methods):
	%
	function_table :: ast_info:function_table(),

	% The function identifiers of the WOOPER builtins:
	builtins :: [ ast_info:function_id() ]

} ).

-type scan_context() :: #scan_context{}.



% Extracts the methods found in the specified function table, transforms them,
% and interprets that information to update the specified class information.
%
% Returns an updated pair thereof.
%
-spec manage_methods( compose_pair() ) -> compose_pair().
manage_methods( { CompleteFunctionTable,
				  ClassInfo=#class_info{ requests=RequestTable,
										 oneways=OnewayTable,
										 statics=StaticTable,
										 markers=MarkerTable } } ) ->

	AllFunEntries = table:enumerate( CompleteFunctionTable ),

	% We do not want to analyse any WOOPER builtin function, as they will not
	% teach us anything about the class at hand, and are expected to be already
	% in a final form:
	%
	Builtins = wooper_info:get_wooper_builtins(),

	% Read-only convenience structure:
	ScanContext = #scan_context{ function_table=CompleteFunctionTable,
								 builtins=Builtins },

	{ NewFunctionTable, NewRequestTable, NewOnewayTable, NewStaticTable } =
		sort_out_functions( AllFunEntries, _FunctionTable=table:new(),
							RequestTable, OnewayTable, StaticTable,
							ScanContext ),

	ExportLoc = ast_info:get_default_export_function_location( MarkerTable ),

	% We take advantage of this step to ensure that all methods are exported:
	[ ExportedRequestTable, ExportedOnewayTable, ExportedStaticTable ] =
		[ ensure_all_exported_in( T, ExportLoc )
		  || T <- [ NewRequestTable, NewOnewayTable, NewStaticTable ] ],


	% Split as { Functions, Methods }:
	{ NewFunctionTable, ClassInfo#class_info{ requests=ExportedRequestTable,
											  oneways=ExportedOnewayTable,
											  statics=ExportedStaticTable } }.



% Transforms and categorises each of the specified functions according to its
% real nature (ex: a given Erlang function may actually be a WOOPER oneway).
%
sort_out_functions( _FunEntries=[], FunctionTable, RequestTable, OnewayTable,
					StaticTable, _ScanContext ) ->
	{ FunctionTable, RequestTable, OnewayTable, StaticTable };

%sort_out_functions( _FunEntries=[ { FunId={ FunName, Arity }, FunInfo } | T ],
sort_out_functions( _FunEntries=[ { FunId, FunInfo } | T ],
					FunctionTable, RequestTable, OnewayTable, StaticTable,
					ScanContext=#scan_context{ builtins=Builtins } ) ->

	case lists:member( FunId, Builtins ) of

		true ->

			% Skip any builtin, we currently just consider them as functions:
			%
			NewFunctionTable = table:addNewEntry( FunId, FunInfo,
												  FunctionTable ),

			sort_out_functions( T, NewFunctionTable, RequestTable, OnewayTable,
								StaticTable, ScanContext );

		false ->

			%trace_utils:debug_fmt( "Examining Erlang function ~s/~B",
			%					   [ FunName, Arity ] ),

			% Use the first clause to guess:
			FunNature = infer_function_nature( FunId, ScanContext ),

			%trace_utils:debug_fmt( "## ~s/~B is a ~s",
			%					   [ FunName, Arity, FunNature ] ),

			OriginalClauses = FunInfo#function_info.clauses,

			% Checks that the terminators in all leaves of all clauses are
			% consistent with the detected nature (this is an optional step, yet
			% it allows to better report errors):
			%
			check_terminators_in( OriginalClauses, FunNature ),

			% Then check and transform all clauses, for any given nature:
			NewClauses = transform_method_returns( OriginalClauses, FunNature ),

			NewFunInfo = FunInfo#function_info{ clauses=NewClauses },

			% Stores the result in the right category and recurses:
			case FunNature of

				function ->
					NewFunctionTable = table:addNewEntry( FunId, NewFunInfo,
														  FunctionTable ),
					sort_out_functions( T, NewFunctionTable, RequestTable,
										OnewayTable, StaticTable, ScanContext );

				request ->
					NewRequestTable = table:addNewEntry( FunId, NewFunInfo,
														 RequestTable ),
					sort_out_functions( T, FunctionTable, NewRequestTable,
										OnewayTable, StaticTable, ScanContext );

				oneway ->
					NewOnewayTable = table:addNewEntry( FunId, NewFunInfo,
														OnewayTable ),
					sort_out_functions( T, FunctionTable, RequestTable,
									NewOnewayTable, StaticTable, ScanContext );

				static ->
					NewStaticTable = table:addNewEntry( FunId, NewFunInfo,
														StaticTable ),
					sort_out_functions( T, FunctionTable, RequestTable,
									OnewayTable, NewStaticTable, ScanContext )

			end

	end;

sort_out_functions( _Functions=[ #function_info{ name=FunName,
												 arity=Arity } | _T ],
					_FunctionTable, _RequestTable, _OnewayTable, _StaticTable,
					_ScanContext ) ->

	trace_utils:error_fmt( "No clause found for ~s/~B; function exported "
						   "yet not defined?", [ FunName, Arity ] ),

	% Error raised directly, could be appended to the class_info.errors:
	wooper_internals:raise_error( { clauseless_function, { FunName, Arity } } ).



% Infers the nature of the corresponding function.
%
% We consider that all actual clauses of a method must explicitly terminate with
% a WOOPER method terminator (of course the same for all clauses), rather than
% calling an helper function that would use such a terminator (otherwise the
% methods could not be auto-detected, as there would be no way to determine
% whether said helper should be considered as a method or not).
%
-spec infer_function_nature( meta_utils:function_id(), scan_context() ) ->
								   function_nature().
infer_function_nature( FunId, ScanContext ) ->

	% For that, we rely on the analysis of the first call leaf of the first
	% clause:

	% Available by design:
	FunInfo = table:getEntry( FunId, ScanContext#scan_context.function_table ),

	% At least one clause per function:
	FirstClause = hd( FunInfo#function_info.clauses ),

	infer_function_nature_from_clause( FirstClause, ScanContext ).




% Infers the nature of the corresponding function, based on its specified
% clause.
%
-spec infer_function_nature_from_clause( meta_utils:clause_def(),
										 scan_context() ) -> function_nature().
infer_function_nature_from_clause( Clause, ScanContext ) ->

	%trace_utils:debug_fmt( " - examining nature of clause ~p", [ Clause ] ),

	Leaf = get_first_call_leaf_for_clause( Clause, ScanContext ),

	%trace_utils:debug_fmt( " - first call leaf found: ~p", [ Leaf ] ),

	case Leaf of

		% Request detected:
		{ wooper, return_state_result, 2 } ->
			request;

		E={ wooper, return_state_result, _Incorrect } ->
			wooper_internals:raise_error( { faulty_request_return, E } );


		% Oneway detected:
		{ wooper, return_state_only, 1 } ->
			oneway;

		E={ wooper, return_state_only, _Incorrect } ->
			wooper_internals:raise_error( { faulty_oneway_return, E } );


		% Static method detected:
		{ wooper, return_static, 1 } ->
			static;

		E={ wooper, return_static, _Incorrect } ->
			wooper_internals:raise_error( { faulty_static_return, E } );

		% Not a WOOPER method terminator, hence should be a simple function:
		{ _OtherModule, _FunctionName, _Arity } ->
			function;

		% Here these configurations denote with certainty a function:
		immediate_value ->
			function;

		local_call ->
			function;

		% Expected to be mostly expressions or remote calls:
		_ ->
			trace_utils:warning_fmt( "Call leaf ~p supposed denoting a "
									 "plain function.", [ Leaf ] ),
			function

	end.



% Returns the first (arbitrarily nested) leaf (terminal expression) of the
% specified function clause that is a remote call, as
% {ModuleName,FunctionName,Arity}, or returns other atoms to denote plain
% functions.
%
% Note: this used to be recursively done through all nested local function calls
% possibly ultimately ending with a WOOPER method terminator, but such cases are
% now disallowed (clearer, simpler, no need for explicit exports to avoid having
% an helper function be wrongly identified as methods).
%
-spec get_first_call_leaf_for_clause( meta_utils:clause_def(),
									  scan_context() ) -> maybe( mfa() ).
get_first_call_leaf_for_clause(
  _Clause={ clause, _Line, _Patterns, _Guards, Body }, ScanContext ) ->
	get_first_call_leaf_for_body( Body, ScanContext ).



% Only the last (possibly nested) expression stemming from a given body matters:
get_first_call_leaf_for_body( _ClauseBody=[ E ], ScanContext ) ->
	% This is the last expression, hence select it for scanning:
	get_first_call_leaf_for_expression( E, ScanContext );

get_first_call_leaf_for_body( _ClauseBody=[ _E | T ], ScanContext ) ->
	% Skip all non-last expressions:
	get_first_call_leaf_for_body( T, ScanContext ).




% Handles each possible branching construct of the specified expression.
%
% (anonymous mute variables correspond to line numbers)
%
% First, body-level:
get_first_call_leaf_for_expression(
  _Expr={ 'case', _, _Patterns, _Guards, Body }, ScanContext ) ->
	get_first_call_leaf_for_body( Body, ScanContext );

get_first_call_leaf_for_expression(
  _Expr={ 'catch', _, _Patterns, _Guards, Body }, ScanContext ) ->
	get_first_call_leaf_for_body( Body, ScanContext );

% Then, expression-level:
% Intercepts calls to the three WOOPER method terminators:
get_first_call_leaf_for_expression(
  _Expr={ 'call', _,
		  { remote, _, {atom,_,wooper}, {atom,_,return_state_result} },
		  Params }, _ScanContext ) ->
	{ wooper, return_state_result, length( Params ) };

get_first_call_leaf_for_expression(
  _Expr={ 'call', _,
		  { remote, _, {atom,_,wooper}, {atom,_,return_state_only} },
		  Params }, _ScanContext ) ->
	{ wooper, return_state_only, length( Params ) };

get_first_call_leaf_for_expression(
  _Expr={ 'call', _,
		  { remote, _, {atom,_,wooper}, {atom,_,return_static} },
		  Params }, _ScanContext ) ->
	{ wooper, return_static, length( Params ) };

% Terminating on another remote call, cannot say anything more here except that
% we expect immediate method terminators:
%
get_first_call_leaf_for_expression(
  _Expr={ 'call', _,
		  { remote, _, ModExpr, FunNameExpr }, Params }, _ScanContext ) ->
	{ ModExpr, FunNameExpr, length( Params ) };

% Terminating on an (immediate) local call, we used to recurse:
%get_first_call_leaf_for_expression(
%  _Expr={ 'call', _, _FunExpr={ atom, _, FunName }, Params }, ScanContext ) ->
%	LocalCallFunId = { FunName, length( Params ) },
%	infer_function_nature( LocalCallFunId, ScanContext );

% This is a local call that is not immediate:
%get_first_call_leaf_for_expression(
%  _Expr={ 'call', _, FunExpr, Params }, _ScanContext ) ->
%	wooper_internals:raise_error( { non_immediate_local_call, FunExpr,
%						   length( Params ) } );

% Module-local call; now we consider instead that this cannot denote a method,
% hence this must be a function:
%
get_first_call_leaf_for_expression(
  _Expr={ 'call', _, _FunExpr, _Params }, _ScanContext ) ->
	local_call;


% Try/catch expression, here with no case clause (try B catch Tc_1 ; ... ; Tc_k
% ...):
get_first_call_leaf_for_expression(
  _Expr={ 'try', _, Body, _CaseClauses=[], _CatchClauses, _AfterBody },
  ScanContext ) ->
	get_first_call_leaf_for_body( Body, ScanContext );


% Try/catch expression, here with at least one case clause (try B of Cc_1 ;
% ... ; Cc_k ...)
%
get_first_call_leaf_for_expression(
  _Expr={ 'try', _, _Body, _CaseClauses=[ C | _T ], _CatchClauses, _AfterBody },
  ScanContext ) ->
	% We rely on the first case clause:
	infer_function_nature_from_clause( C, ScanContext );

% Match expression: {match,LINE,Rep(P_1),Rep(P_2)}.
get_first_call_leaf_for_expression(
  _Expr={ 'match', _, FirstPattern, _SecondPattern },
  ScanContext ) ->
	% We rely on the first pattern:
	get_first_call_leaf_for_expression( FirstPattern, ScanContext );

% Receive expression: receive Cc_1 ; ... ; Cc_k end
get_first_call_leaf_for_expression( _Expr={ 'receive', _,
											_CaseClauses=[ C | _T ] },
									ScanContext ) ->
	% We rely on the first:
	get_first_call_leaf_for_case_clause( C, ScanContext );

% Immediate values:
get_first_call_leaf_for_expression( _Expr={ 'atom', _, _Atom },
									_ScanContext ) ->
	immediate_value;

get_first_call_leaf_for_expression( _Expr={ 'char', _, _Atom },
									_ScanContext ) ->
	immediate_value;

get_first_call_leaf_for_expression( _Expr={ 'float', _, _Atom },
									_ScanContext ) ->
	immediate_value;

get_first_call_leaf_for_expression( _Expr={ 'integer', _, _Atom },
									_ScanContext ) ->
	immediate_value;

get_first_call_leaf_for_expression( _Expr={ 'string', _, _Atom },
									_ScanContext ) ->
	immediate_value;

get_first_call_leaf_for_expression( Expr, _Acc ) ->
	wooper_internals:raise_error( { unhandled_expression, Expr } ).



% Handles (any kind of) case clause, {clause,LINE,[Rep(P)],[],Rep(B)}.
%
get_first_call_leaf_for_case_clause(
  { 'clause', _, _Patterns, _Guards, Body }, ScanContext ) ->
	get_first_call_leaf_for_body( Body, ScanContext ).



% Ensures that the specified function of the specified nature uses the expected
% terminators.
%
-spec check_terminators_in( [ meta_utils:clause_def() ], function_nature() ) ->
								  void().
check_terminators_in( _Clauses=[], _FunNature ) ->
	ok;

check_terminators_in( _Clauses=[ C | T ], FunNature ) ->
	check_terminators_in_clause( C, FunNature ),
	check_terminators_in( T, FunNature ).



% Ensures that the specified clause of the specified nature uses the
% corresponding terminators.
%
-spec check_terminators_in_clause( meta_utils:clause_def(),
								   function_nature() ) -> void().
check_terminators_in_clause( Clause, FunNature ) ->

	%trace_utils:debug_fmt( "- checking that the following clause corresponds "
	%					   "to a ~s:~n~p", [ FunNature, Clause ] ),

	AllLeaves = get_call_leaves_for_clause( Clause ),

	%trace_utils:debug_fmt( " - call leaves found: ~p", [ AllLeaves ] ),

	case list_utils:uniquify( AllLeaves ) of

		% At least one occurrence of, and no other, different element than:
		[ { wooper, return_state_result, 2 } ] ->
			case FunNature of

				request ->
					ok;

				_ ->
					wooper_internals:raise_error(
					  { inconsistent_function_terminators,
						{ detected_as, FunNature },
						{ found, return_state_result, 2 } } )

			end;

		[ E={ wooper, return_state_result, _Incorrect } ] ->
			wooper_internals:raise_error( { faulty_request_return, E } );

		[ { wooper, return_state_only, 1 } ] ->
			case FunNature of

				oneway ->
					ok;

				_ ->
					wooper_internals:raise_error(
					  { inconsistent_function_terminators,
						{ detected_as, FunNature },
						{ found, return_state_only, 1 } } )

			end;

		[ E={ wooper, return_state_only, _Incorrect } ] ->
			wooper_internals:raise_error( { faulty_oneway_return, E } );


		[ { wooper, return_static, 1 } ] ->
			case FunNature of

				static ->
					ok;

				_ ->
					wooper_internals:raise_error(
					  { inconsistent_function_terminators,
						{ detected_as, FunNature },
						{ found, return_static, 1 } } )

			end;

		[ E={ wooper, return_static, _Incorrect } ] ->
			wooper_internals:raise_error( { faulty_static_return, E } );

		Other ->
			case FunNature of

				function ->
					ok;

				_ ->
					wooper_internals:raise_error(
					  { inconsistent_function_terminators,
						{ detected_as, FunNature }, { found, Other } } )

			end

	end.


% Returns (recursively) all leaves (terminal expressions) of the specified
% function clause that are remote calls, as {ModuleName,FunctionName,Arity}.
%
% Examples of returned values: [], or [ {wooper,return_state_result,2},
% {wooper,return_state_result,2}, {lists,sort,1} ], etc.
%
-spec get_call_leaves_for_clause( meta_utils:clause_def() ) -> [ mfa() ].
get_call_leaves_for_clause( _Clause={ clause, _Line, _Patterns, _Guards,
									  Body } ) ->
	get_call_leaves_for_clause( Body, _Acc=[] ).


% Only the last expression of a given (possibly nested) clause matters:
get_call_leaves_for_clause( _ClauseBody=[ L ], Acc ) ->
	% This is the last expression, hence select it for scanning:
	get_call_leaves_for_expression( L, Acc );

get_call_leaves_for_clause( _ClauseBody=[ _E | T ], Acc ) ->
	% Skip non-last expressions:
	get_call_leaves_for_clause( T, Acc ).


% Returns (recursively) all leaves (terminal expressions) of the specified
% expression that are remote calls, as {ModuleName,FunctionName,Arity}.
%
% Handles each possible branching constructs.
%
% (anonymous mute variables correspond to lines)
%
-spec get_call_leaves_for_expression( ast_expression() ) -> [ mfa() ].
% First, body-level:
get_call_leaves_for_expression(
  _Expr={ 'case', _Line, _Patterns, _Guards, Body }, Acc ) ->
	get_call_leaves_for_clause( Body ) ++ Acc;

get_call_leaves_for_expression(
  _Expr={ 'catch', _Line, _Patterns, _Guards, Body }, Acc ) ->
	get_call_leaves_for_clause( Body ) ++ Acc;

% Then, expression-level:
% Intercepts calls to the 3 WOOPER method terminators:
get_call_leaves_for_expression(
  _Expr={ 'call', _Line,
		  { remote, _, {atom,_,wooper}, {atom,_,return_state_result} },
		  Params }, Acc ) ->
	[ { wooper, return_state_result, length( Params ) } | Acc ];

get_call_leaves_for_expression(
  _Expr={ 'call', _Line,
		  { remote, _, {atom,_,wooper}, {atom,_,return_state_only} },
		  Params }, Acc ) ->
	[ { wooper, return_state_only, length( Params ) } | Acc ];

get_call_leaves_for_expression(
  _Expr={ 'call', _Line,
		  { remote, _, {atom,_,wooper}, {atom,_,return_static} },
		  Params }, Acc ) ->
	[ { wooper, return_static, length( Params ) } | Acc ];

% Terminating on another remote call, cannot say anything more here:
get_call_leaves_for_expression(
  _Expr={ 'call', _Line, { remote, _, _ModExpr, _FunExpr },
		  _Params }, Acc ) ->
	Acc;

% Terminating on a local call, cannot say anything more here:
get_call_leaves_for_expression(
  _Expr={ 'call', _Line, _FunExpr, _Params }, Acc ) ->
	Acc;


get_call_leaves_for_expression(
  _Expr={ 'receive', _Line, CaseClauses }, Acc ) ->
	get_call_leaves_for_clause( CaseClauses ) ++ Acc;


% Immediate values, no information :
get_call_leaves_for_expression(
  _Expr={ 'atom', _Line, _Value }, Acc ) ->
	Acc;

get_call_leaves_for_expression(
  _Expr={ 'char', _Line, _Value }, Acc ) ->
	Acc;

get_call_leaves_for_expression(
  _Expr={ 'float', _Line, _Value }, Acc ) ->
	Acc;

get_call_leaves_for_expression(
  _Expr={ 'integer', _Line, _Value }, Acc ) ->
	Acc;

get_call_leaves_for_expression(
  _Expr={ 'string', _Line, _Value }, Acc ) ->
	Acc;

get_call_leaves_for_expression( Expr, _Acc ) ->
	wooper_internals:raise_error( { unhandled_expression, Expr, call_leaves } ).





% Transforms specified clauses according to their specified nature.
transform_method_returns( Clauses, FunNature ) ->
	[ transform_method_returns_in( C, FunNature ) || C <- Clauses ].



% Transforms specified clause according to the specified nature of its function.
%
transform_method_returns_in(
  _ClauseForm={ clause, Line, Patterns, Guards, Body }, FunNature ) ->

	%case Patterns of
	%
	%	[] ->
	%		ok;
	%
	%	_ ->
	%		trace_utils:debug_fmt( "- ignoring patterns ~p", [ Patterns ] )
	%
	%end,

	% Nothing to transform in very limited, BIF-based guards:
	%trace_utils:debug_fmt( " - ignoring guards ~p", [ Guards ] ),

	%trace_utils:debug_fmt( " - transforming, as '~s', following body:~n~p",
	%					   [ FunNature, Body ] ),

	NewBody = transform_body( Body, FunNature ),

	%trace_utils:debug_fmt( " - corresponding transformed body is:~n~p",
	%					   [ NewBody ] ),

	{ clause, Line, Patterns, Guards, NewBody }.



% Transforms specified (top-level) body of specified clause, according to the
% function nature.
%
transform_body( Body, _FunNature=function ) ->
	% Nothing to change in plain functions:
	Body;

% We have a method here; its transformation can only happen in the last
% top-level expression of the body, as we are only interested here in method
% terminators:
%
transform_body( _Body=[ LastExpression ], FunNature ) ->

	% We have to traverse recursively this final expression to handle all its
	% local leaves (recursing in nested calls is not needed here, as by
	% convention the method terminators should be local to the method body), in
	% order to transform all method terminators found.
	%
	% For that we can either follow exactly the AST legit structure that we can
	% foresee (typically based on http://erlang.org/doc/apps/erts/absform.html),
	% at the risk of rejecting correct code - should our traversal be not
	% perfect, or we can "blindly" rewrite calls for example corresponding to
	% wooper:return_state_result( S, R ) as { S, R } (and check we have no
	% incompatible method terminators).
	%
	% We preferred initially here the latter solution (lighter, simpler, safer),
	% but we have already a full logic (in Myriad's meta) to traverse and
	% transform, so we rely on it for the best, now that it has been enriched so
	% that it can maintain a transformation state.
	%
	% A combination of ast_transform:get_remote_call_transform_table/1 with
	% ast_expression:transform_expression/2 would be close to what we need, yet
	% we do not want to replace a *call* by another, we want it to be replaced
	% by a tuple creation, i.e. an expression. So:

	% Closure so that the callback transformer (whose signature must be
	% standard) is parametrised by the nature of the function that shall be
	% transformed:
	%
	CallTransformFun =
		fun( LineCall, CallFunctionRef, Params, TransformState ) ->
				transform_method_terminator( LineCall, CallFunctionRef, Params,
											 FunNature, TransformState )
		end,

	TransformTable = table:new( [ { call, CallTransformFun } ] ),

	% We create and drop after use our own transforms:
	MethodTerminatorTransforms = #ast_transforms{ expressions=TransformTable },

	% Replace the method terminators with the actual return code we want:
	{ NewExpression, _NewTransforms } =
		ast_expression:transform_expression( LastExpression,
											 MethodTerminatorTransforms ),

	[ NewExpression ];


% Select only the last top-level expression:
transform_body( _Body=[ E | T ], FunNature ) ->
	% One of the very few body-recursise cases:
	[ E | transform_body( T, FunNature ) ].



% Transformer so that calls corresponding to method terminators are "inlined",
% i.e. for example so that wooper:return_state_result( S, R ) are replaced with
% { S, R }.
%
% (anonymous mute variables correspond to line numbers)
%
-spec transform_method_terminator( line(),
		  ast_expression:function_ref_expression(),
		  ast_expression:params_expression(), function_nature(),
		  transformation_state() ) -> ast_expression().
% Rewrites only the right WOOPER functions, i.e. the method terminators:
% Correct case for requests:
transform_method_terminator( LineCall,
		_CallFunctionRef={ remote, _, {atom,_, wooper},
						   { atom, _, _FunName=return_state_result } },
		Params=[ _StateExpr, _ResExpr ],
		_FunNature=request,
		TransformState ) ->

	% So that wooper:return_state_result( S, R ) becomes simply { S, R }:
	NewExpr = { tuple, LineCall, Params },

	%trace_utils:debug_fmt( "Request terminator replaced with: ~p.~n",
	%					   [ NewExpr ] ),

	{ NewExpr, TransformState };


% Incorrect arity for requests:
transform_method_terminator( LineCall,
		_CallFunctionRef={ remote, _, {atom,_,wooper},
						   {atom,_,FunName=return_state_result} },
		Params,
		_FunNature=request,
		_TransformState ) ->
	wooper_internals:raise_error( { wrong_arity, { at, LineCall },
		{ wooper, FunName, { got, length( Params ) }, { expected, 2 } } } );


% Incorrect method terminator, as we have not a request here:
transform_method_terminator( LineCall,
		_CallFunctionRef={ remote, _, {atom,_,wooper},
						   {atom,_,FunName=return_state_result} },
		Params,
		FunNature,
		_TransformState ) ->
	wooper_internals:raise_error( { wrong_terminator, { at, LineCall },
		{ method_nature, FunNature }, { wooper, FunName, length( Params ) } } );


% Correct case for oneways:
transform_method_terminator( _LineCall,
		_CallFunctionRef={ remote, _, {atom,_,wooper},
						   {atom,_,_FunName=return_state_only} },
		_Params=[ StateExpr ],
		_FunNature=oneway,
		TransformState ) ->
	% wooper:return_state_only( S ) becomes S:
	{ StateExpr, TransformState };


% Incorrect arity for oneways:
transform_method_terminator( LineCall,
		_CallFunctionRef={ remote, _, {atom,_,wooper},
						   {atom,_,FunName=return_state_only} },
		Params,
		_FunNature=oneway,
		_TransformState ) ->
	wooper_internals:raise_error( { wrong_arity, { at, LineCall },
							 { wooper, FunName,
							   { got, length( Params ) }, { expected, 1 } } } );


% Incorrect method terminator, as we have not a oneway here:
transform_method_terminator( LineCall,
		_CallFunctionRef={ remote, _, {atom,_,wooper},
						   {atom,_,FunName=return_state_only} },
		Params,
		FunNature,
		_TransformState ) ->
	wooper_internals:raise_error( { wrong_terminator, { at, LineCall },
							 { method_nature, FunNature },
							 { wooper, FunName, length( Params ) } } );


% Correct case for static methods:
transform_method_terminator( _LineCall,
		_CallFunctionRef={ remote, _, {atom,_,wooper},
						   {atom,_,_FunName=return_static} },
		_Params=[ StateExpr ],
		_FunNature=static,
		TransformState ) ->
	% wooper:return_static( S ) becomes S:
	{ StateExpr, TransformState };


% Incorrect arity for static methods:
transform_method_terminator( LineCall,
		_CallFunctionRef={ remote, _, {atom,_,wooper},
						   {atom,_,FunName=return_static} },
		Params,
		_FunNature=static,
		_TransformState ) ->
	wooper_internals:raise_error( { wrong_arity, { at, LineCall },
							 { wooper, FunName,
							   { got, length( Params ) }, { expected, 1 } } } );

% Incorrect method terminator, as we have not a static method here:
transform_method_terminator( LineCall,
		_CallFunctionRef={ remote, _, {atom,_,wooper},
						   {atom,_,FunName=return_static} },
		Params,
		FunNature,
		_TransformState ) ->
	wooper_internals:raise_error( { wrong_terminator, {at,LineCall},
							 { method_nature, FunNature },
							 { wooper, FunName, length( Params ) } } );

% All other calls are to pass through, as they are:
transform_method_terminator( LineCall, CallFunctionRef, Params,
							 _FunNature, TransformState ) ->
	NewExpr = { call, LineCall, CallFunctionRef, Params },
	{ NewExpr, TransformState }.



% Intercept WOOPER calls:
%
% (anonymous mute variables are line numbers)
%
%% get_call_leaves_for_clause( _Clause={ 'call', _Line,
%% {remote,_,{atom,_,Module},{atom,_,Function}}, SubClause },
%%				Acc ) ->
%%	get_call_leaves_for_clause( SubClause ) ++ Acc;

%% get_call_leaves_for_clause( _Clause=E, _Acc ) ->
%%	throw( { unsupported_clause_element, E } ).


% Ensures that the specified information is exported, auto-exporting it if
% necessary.
%
% See also: ast_info:ensure_function_exported/4.
%
-spec ensure_exported( function_info(), marker_table() ) -> function_info().
ensure_exported( FunInfo=#function_info{ exported=[] }, MarkerTable ) ->

	%trace_utils:debug_fmt( "- auto-exporting ~s",
	%					 [ ast_info:function_info_to_string( FunInfo ) ] ),

	% Not exported yet, hence to export:
	ExportLoc = ast_info:get_default_export_function_location( MarkerTable ),
	FunInfo#function_info{ exported=[ ExportLoc ] };

% Already exported, thus nothing to do:
ensure_exported( FunInfo, _MarkerTable ) ->
	FunInfo.



% Ensures that all functions in specified function table are exported,
% auto-exporting them if necessary.
%
-spec ensure_all_exported_in( function_table(), ast_info:location() ) ->
									function_table().
ensure_all_exported_in( FunctionTable, ExportLoc ) ->
	table:mapOnValues( fun( FunInfo ) ->
							ensure_exported_at( FunInfo, ExportLoc )
					   end,
					   FunctionTable ).


% (helper)
ensure_exported_at( FunInfo=#function_info{ exported=[] }, ExportLoc ) ->
	FunInfo#function_info{ exported=[ ExportLoc ] };

ensure_exported_at( FunInfo, _ExportLoc ) ->
	FunInfo.
