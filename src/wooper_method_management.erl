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


-type function_info() :: ast_info:function_info().
-type marker_table() :: ast_info:section_marker_table().
-type function_table() :: ast_info:function_table().
-type compose_pair() :: wooper_parse_transform:compose_pair().



% Implementation notes:


% To determine whether a function is a method and, if yes, of what kind it is
% (request, oneway, etc.), we have to traverse recursively at least one of its
% clauses until finding at least one final expression in order to examine, check
% and possibly transform any method terminator found.
%
% Recursing in nested local calls is not needed here, as by convention the
% method terminators should be local to the method body.
%
% For that we can either follow exactly the AST legit structure that we can
% foresee (typically based on http://erlang.org/doc/apps/erts/absform.html), at
% the risk of rejecting correct code - should our traversal be not perfect, or
% we can "blindly" rewrite calls for example corresponding to
% wooper:return_state_result( S, R ) as { S, R } (and check we have no
% incompatible method terminators).
%
% We preferred initially here the latter solution (lighter, simpler, safer), but
% we had already a full logic (in Myriad's meta) to traverse and transform ASTs
% or part thereof, so we rely on it for the best, now that it has been enriched
% so that it can apply and maintain a transformation state.
%
% A combination of ast_transform:get_remote_call_transform_table/1 with
% ast_expression:transform_expression/2 would have been close to what we needed,
% yet we do not want to replace a *call* by another, we want it to be replaced
% by a tuple creation, i.e. an expression. Moreover we need to be stateful, to
% remember any past method terminator and check their are consistent with the
% newer ones being found.



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

	% We used to filter-out the WOOPER builtin functions (based on
	% wooper_info:get_wooper_builtins/0), as they would not teach WOOPER
	% anything about the class at hand, yet we want them to be correctly
	% identified by WOOPER (they are of different natures); moreover, not all of
	% them are already in a final form (ex: they still use method terminators),
	% so their processing shall not be skipped.

	{ NewFunctionTable, NewRequestTable, NewOnewayTable, NewStaticTable } =
		sort_out_functions( AllFunEntries, _FunctionTable=table:new(),
							RequestTable, OnewayTable, StaticTable ),

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
					StaticTable ) ->
	{ FunctionTable, RequestTable, OnewayTable, StaticTable };

%sort_out_functions( _FunEntries=[ { FunId={ FunName, Arity }, FunInfo } | T ],
sort_out_functions( _FunEntries=[ { FunId, FunInfo } | T ],
					FunctionTable, RequestTable, OnewayTable, StaticTable ) ->

	%trace_utils:debug_fmt( "sorting ~p", [ FunId ] ),


	%trace_utils:debug_fmt( "Examining Erlang function ~s/~B",
	%                       [ FunName, Arity ] ),

	OriginalClauses = FunInfo#function_info.clauses,

	% We used to infer the function nature based on its first clause, and then
	% to make a custom full traversal to transform method terminators. Now we
	% reuse the Myriad ast_transforms instead, and perform everything
	% (guessing/checking/transforming) in one pass:
	%
	{ NewClauses, FunNature } =
		manage_method_terminators( OriginalClauses, FunId ),

	NewFunInfo = FunInfo#function_info{ clauses=NewClauses },

	% Stores the result in the right category and recurses:
	case FunNature of

		function ->
			NewFunctionTable = table:addNewEntry( FunId, NewFunInfo,
												  FunctionTable ),
			sort_out_functions( T, NewFunctionTable, RequestTable,
								OnewayTable, StaticTable );

		request ->
			NewRequestTable = table:addNewEntry( FunId, NewFunInfo,
												 RequestTable ),
			sort_out_functions( T, FunctionTable, NewRequestTable,
								OnewayTable, StaticTable );

		oneway ->
			NewOnewayTable = table:addNewEntry( FunId, NewFunInfo,
												OnewayTable ),
			sort_out_functions( T, FunctionTable, RequestTable,
								NewOnewayTable, StaticTable );

		static ->
			NewStaticTable = table:addNewEntry( FunId, NewFunInfo,
												StaticTable ),
			sort_out_functions( T, FunctionTable, RequestTable,
								OnewayTable, NewStaticTable )

	end;

sort_out_functions( _Functions=[ #function_info{ name=FunName,
												 arity=Arity } | _T ],
					_FunctionTable, _RequestTable, _OnewayTable,
					_StaticTable ) ->

	trace_utils:error_fmt( "No clause found for ~s/~B; function exported "
						   "yet not defined?", [ FunName, Arity ] ),

	% Error raised directly, could be appended to the class_info.errors:
	wooper_internals:raise_error( { clauseless_function, { FunName, Arity } } ).



% Infers the nature of the corresponding function, ensures all method
% terminators correspond, and transform them appropriately, in one pass.
%
% We consider that all actual clauses of a method must explicitly terminate with
% a WOOPER method terminator (of course the same for all clauses), rather than
% calling an helper function that would use such a terminator (otherwise the
% nature of methods could not be auto-detected, as there would be no way to
% determine whether said helper should be considered as a method or not).
%
-spec manage_method_terminators( meta_utils:clause_def(),
								 meta_utils:function_id() ) ->
							{ meta_utils:clause_def(), function_nature() }.
manage_method_terminators( _Clauses=[], FunId ) ->
	wooper_internals:raise_error(
			  { function_exported_yet_not_defined, FunId } );

manage_method_terminators( Clauses, FunId ) ->

	% We define first the transformation functions in charge of the
	% guessing/checking/transforming of the method terminators.
	%
	% The body transform-fun will be used to skip all expressions of a body but
	% the last one, while the call transform-fun will be applied to call
	% expressions found there, as:
	%
	% { NewExpr, NewTransforms } = TransformFun( LineCall, FunctionRef,
	%                                            Params, Transforms )
	%
	% Transforms is an ast_transforms record that contains a
	% transformation_state field, which is of type maybe( function_nature() ):
	% it starts as 'undefined', then the first terminal call branch of the first
	% clause allows to detect and set the function nature, and afterwards all
	% other branches are checked against it, and transformed.

	% As empty bodies may happen (ex: 'receive' without an 'after'):
	BodyTransformFun = fun

		( _BodyExprs=[], Transforms ) ->
			{ _Exprs=[], Transforms };

		% Commented-out as the last expression is managed differently (we cannot
		% recurse easily), but the spirit remains:
		%( _BodyExprs=[ LastExpr ], Transforms ) ->
		%    ast_expression:transform_expressions( LastExpr, Transforms );

		% At least an element exists:
		( BodyExprs, Transforms ) -> % superfluous: when is_list( BodyExprs ) ->

			% Warning: we currently skip intermediate expressions as a whole (we
			% do not transform them at all, as currently WOOPER does not have
			% any need for that), but maybe in the future this will have to be
			% changed.
			%
			% We cannot use easily a Y-combinator here, as the signature of this
			% anonymous function is constrained:
			%{ [ Expr | SomeFun(T) ] }, Transforms }
			%
			% More efficient than list_utils:extract_last_element/2 and then
			% recreating the list:
			[ LastExpr | RevFirstExprs ] = lists:reverse( BodyExprs ),

			{ [ NewLastExpr ], NewTransforms } =
				ast_expression:transform_expression( LastExpr, Transforms ),

			NewExprs = lists:reverse( [ NewLastExpr | RevFirstExprs ] ),

			{ NewExprs, NewTransforms }

	end,

	% In charge of managing the method terminators:
	%
	% (anonymous mute variables correspond to line numbers)
	%
	CallTransformFun = fun

		% First, requests:

		% First (correct) request detection:
		( LineCall,
		  _FunctionRef={ remote, _, {atom,_,wooper},
						 {atom,_,return_state_result} },
		  Params=[ _StateExpr, _ResExpr ],
		  Transforms=#ast_transforms{
			transformation_state=S } )
							 when S =:= undefined orelse S =:= request ->

			%trace_utils:debug_fmt( "~s/~B detected as a request.",
			%					   pair:to_list( FunId ) ),

			% So that wooper:return_state_result( S, R ) becomes simply:
			% { S, R }:
			NewExpr = { tuple, LineCall, Params },
			NewTransforms = Transforms#ast_transforms{
							  transformation_state=request },
			{ [ NewExpr ], NewTransforms };


		% Faulty request arity:
		( LineCall,
		  _FunctionRef={ remote, _, {atom,_,wooper},
						 {atom,_,return_state_result} },
		  Params,
		  _Transforms ) when length( Params ) =/= 2 ->
			wooper_internals:raise_error( { wrong_arity, { line, LineCall },
			  { wooper, return_state_result, { got, length( Params ) },
				{ expected, 2 } }, FunId } );


		% Nature mismatch:
		( LineCall,
		  _FunctionRef={ remote, _, {atom,_,wooper},
						 {atom,_,return_state_result} },
		  _Params,
		  _Transforms=#ast_transforms{
			transformation_state=OtherNature } ) ->
			wooper_internals:raise_error( { method_terminator_mismatch,
				{ was, OtherNature }, { detected, request },
				FunId, { line, LineCall } } );



		% First (correct) oneway detection:
		( _LineCall,
		  _FunctionRef={ remote, _, {atom,_,wooper},
						 {atom,_,return_state_only} },
		  _Params=[ StateExpr ],
		  Transforms=#ast_transforms{
			transformation_state=S } )
							 when S =:= undefined orelse S =:= oneway ->

			%trace_utils:debug_fmt( "~s/~B detected as a oneway.",
			%					   pair:to_list( FunId ) ),

			% So that wooper:return_state_only( R ) becomes simply R:
			NewExpr = StateExpr,
			NewTransforms = Transforms#ast_transforms{
							  transformation_state=oneway },
			{ [ NewExpr ], NewTransforms };


		% Faulty oneway arity:
		( LineCall,
		  _FunctionRef={ remote, _, {atom,_,wooper},
						 {atom,_,return_state_only} },
		  Params=[ _State ],
		  _Transforms ) ->
			wooper_internals:raise_error( { wrong_arity, { line, LineCall },
			  { wooper, return_state_only, { got, length( Params ) },
				{ expected, 1 } }, FunId } );


		% Nature mismatch:
		( LineCall,
		  _FunctionRef={ remote, _, {atom,_,wooper},
						 {atom,_,return_state_only} },
		  Params,
		  _Transforms=#ast_transforms{
			transformation_state=OtherNature } ) when length( Params ) =/= 2 ->
			wooper_internals:raise_error( { method_terminator_mismatch,
				{ was, OtherNature }, { detected, oneway },
				FunId, { line, LineCall } } );



		% First (correct) static method detection:
		( _LineCall,
		  _FunctionRef={ remote, _, {atom,_,wooper},
						 {atom,_,return_static} },
		  _Params=[ ResultExpr ],
		  Transforms=#ast_transforms{
			transformation_state=S } )
							 when S =:= undefined orelse S =:= static ->

			%trace_utils:debug_fmt( "~s/~B detected as a static method.",
			%					   pair:to_list( FunId ) ),

			% So that wooper:return_static( R ) becomes simply R:
			NewExpr = ResultExpr,
			NewTransforms = Transforms#ast_transforms{
							  transformation_state=static },
			{ [ NewExpr ], NewTransforms };


		% Faulty static arity:
		( LineCall,
		  _FunctionRef={ remote, _, {atom,_,wooper},
						 {atom,_,return_static} },
		  Params,
		  _Transforms ) when length( Params ) =/= 2 ->
			wooper_internals:raise_error( { wrong_arity, { line, LineCall },
			  { wooper, return_static, { got, length( Params ) },
				{ expected, 1 }, FunId } } );


		% Nature mismatch:
		( LineCall,
		  _FunctionRef={ remote, _, {atom,_,wooper},
						 {atom,_,return_static} },
		  _Params,
		  _Transforms=#ast_transforms{
			transformation_state=OtherNature } ) ->
			wooper_internals:raise_error( { method_terminator_mismatch,
				{ was, OtherNature }, { detected, static },
				FunId, { line, LineCall } } );

		% All other calls are to pass through, as they are:
		( LineCall, FunctionRef, Params, Transforms ) ->
			SameExpr = { call, LineCall, FunctionRef, Params },
			{ [ SameExpr ], Transforms }

	end,

	TransformTable = table:new( [ { body, BodyTransformFun },
								  { call, CallTransformFun } ] ),

	Transforms = #ast_transforms{ transform_table=TransformTable,
								  transformation_state=undefined },

	%trace_utils:debug_fmt( "transforming now ~p.", [ FunId ] ),

	{ NewClauses, NewTransforms } =
		ast_clause:transform_function_clauses( Clauses, Transforms ),

	% Unless found different, a function is a (plain) function:
	FunNature = case NewTransforms#ast_transforms.transformation_state of

		undefined ->
			%trace_utils:debug_fmt( "~s/~B detected as a plain function.",
			%					   pair:to_list( FunId ) ),
			function;

		Other ->
			Other

	end,

	{ NewClauses, FunNature }.



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
