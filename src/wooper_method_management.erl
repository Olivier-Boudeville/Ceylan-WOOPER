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


% Centralizes, on behalf of the WOOPER parse transform, the support for method
% management.
%
-module(wooper_method_management).


-export([ manage_methods/1, ensure_exported/2, methods_to_functions/5 ]).


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
-type location() :: ast_base:form_location().
-type function_table() :: ast_info:function_table().

-type compose_pair() :: wooper_parse_transform:compose_pair().

-type request_table() :: wooper_info:request_info().
-type oneway_table() :: wooper_info:oneway_info().
-type static_table() :: wooper_info:static_info().
-type method_qualifiers() :: wooper:method_qualifiers().



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
				  ClassInfo=#class_info{ class={ Classname, _ClassForm },
										 requests=RequestTable,
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
							RequestTable, OnewayTable, StaticTable, Classname ),

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
					StaticTable, _Classname ) ->
	{ FunctionTable, RequestTable, OnewayTable, StaticTable };

%sort_out_functions( _FunEntries=[ { FunId={ FunName, Arity }, FunInfo } | T ],
sort_out_functions( _FunEntries=[ { FunId, FunInfo=#function_info{
											clauses=OriginalClauses,
											spec=Spec } } | T ],
					FunctionTable, RequestTable, OnewayTable, StaticTable,
					Classname ) ->

	%trace_utils:debug_fmt( "sorting ~p", [ FunId ] ),

	%trace_utils:debug_fmt( "Examining Erlang function ~s/~B",
	%                       [ FunName, Arity ] ),

	% We used to infer the function nature based on its first clause, and then
	% to make a custom full traversal to transform method terminators.
	%
	% Now we reuse the Myriad ast_transforms instead, and perform everything
	% (guessing/checking/transforming) in one pass:
	%
	{ NewClauses, FunNature, Qualifiers } =
		manage_method_terminators( OriginalClauses, FunId ),

	NewFunInfo = FunInfo#function_info{ clauses=NewClauses },

	% Stores the result in the right category and recurses:
	case FunNature of

		function ->
			check_spec( Spec, function, Qualifiers, Classname ),
			NewFunctionTable = table:addNewEntry( FunId, NewFunInfo,
												  FunctionTable ),
			sort_out_functions( T, NewFunctionTable, RequestTable,
								OnewayTable, StaticTable, Classname );

		request ->
			check_spec( Spec, request, Qualifiers, Classname ),
			check_state_argument( NewClauses, FunId ),
			RequestInfo = function_to_request_info( NewFunInfo ),
			NewRequestTable = table:addNewEntry( FunId, RequestInfo,
												 RequestTable ),
			sort_out_functions( T, FunctionTable, NewRequestTable,
								OnewayTable, StaticTable, Classname );

		oneway ->
			check_spec( Spec, oneway, Qualifiers, Classname ),
			check_state_argument( NewClauses, FunId ),
			OnewayInfo = function_to_oneway_info( NewFunInfo ),
			NewOnewayTable = table:addNewEntry( FunId, OnewayInfo,
												OnewayTable ),
			sort_out_functions( T, FunctionTable, RequestTable,
								NewOnewayTable, StaticTable, Classname );

		static ->
			check_spec( Spec, static, Qualifiers, Classname ),
			StaticInfo = function_to_static_info( NewFunInfo ),
			NewStaticTable = table:addNewEntry( FunId, StaticInfo,
												StaticTable ),
			sort_out_functions( T, FunctionTable, RequestTable,
								OnewayTable, NewStaticTable, Classname )

	end;

sort_out_functions( _Functions=[ #function_info{ name=FunName,
												 arity=Arity } | _T ],
					_FunctionTable, _RequestTable, _OnewayTable,
					_StaticTable, Classname ) ->

	trace_utils:error_fmt( "No clause found in ~s for ~s/~B; function exported "
						   "yet not defined?", [ Classname, FunName, Arity ] ),

	% Error raised directly, could be appended to the class_info.errors:
	wooper_internals:raise_error( { clauseless_function, { FunName, Arity },
									Classname } ).



% Checks that the request spec (if any) corresponds indeed to a request,
% i.e. rely on either the request_return/1 type or the request_const_return/1
% one.
%
% (helper)
%
-spec check_spec( maybe( ast_info:located_form() ), function_nature(),
				  method_qualifiers(), wooper:classname() ) -> void().
% Function specs are optional:
check_spec( _LocSpec=undefined, _FunNature, _Qualifiers, _Classname ) ->
	ok;

check_spec( _LocSpec={ _Loc,
					   { attribute, _, spec, { FunId, ClauseSpecs } } },
			FunNature, Qualifiers, Classname ) ->
	[ check_clause_spec( C, FunNature, Qualifiers, FunId, Classname )
	  || C <- ClauseSpecs ].




% Checks that request clause spec.
%
% (helper)


%% For oneways:

% Spec implies non-const request:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, _, request_return, [ _RType ] } ] },
	 _FunNature=request, Qualifiers, FunId, Classname ) ->
	case lists:member( const, Qualifiers ) of

		true ->
			wooper_internals:raise_error( { constness_request_mistmatch, FunId,
				{ clauses_tell, const }, { spec_uses, {request_return,1} },
				{ instead_of, {request_const_return,1} }, Classname } );

		false ->
			ok

	end;


% Spec implies const request:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, _, request_const_return, [ _RType ] } ] },
	 _FunNature=request, Qualifiers, FunId, Classname ) ->
	case lists:member( const, Qualifiers ) of

		true ->
			ok;

		false ->
			wooper_internals:raise_error( { constness_request_mistmatch, FunId,
				{ clauses_tell, non_const },
				{ uses_spec, {request_const_return,1} },
				{ instead_of, {request_return,1} }, Classname } )

	end;


% Spec implies request whereas is not:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, _, request_return, [ _RType ] } ] },
	 NonReqFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_error( { incorrect_return_type, FunId,
		{ clauses_tell, NonReqFunNature }, { uses, request_return },
									Classname } );


% Wrong arity for request_return/1:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, _, request_return, Types } ] },
	 _AnyFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_error( { wrong_request_return_arity,
			{ expected, 1 }, { got, length( Types ) }, FunId, Classname } );


% Wrong arity for request_const_return/1:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, _, request_const_return, Types } ] },
	 _AnyFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_error( { wrong_request_const_return_arity,
			{ expected, 1 }, { got, length( Types ) }, FunId, Classname } );


% Spec implies non-const request:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, _, request_return, [ _RType ] } ] },
	 _FunNature=request, Qualifiers, FunId, Classname ) ->
	case lists:member( const, Qualifiers ) of

		true ->
			wooper_internals:raise_error( { constness_request_mistmatch, FunId,
				{ clauses_tell, const }, { uses_spec, {request_return,1} },
				{ instead_of, {request_const_return,1} }, Classname } );

		false ->
			ok

	end;


% Spec implies const request:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, _, request_const_return, [ _RType ] } ] },
	 _FunNature=request, Qualifiers, FunId, Classname ) ->
	case lists:member( const, Qualifiers ) of

		true ->
			ok;

		false ->
			wooper_internals:raise_error( { constness_request_mistmatch, FunId,
				{ clauses_tell, non_const },
				{ uses_spec, {request_const_return,1} },
				{ instead_of, {request_return,1} }, Classname } )

	end;



%% For oneways:

% Spec implies non-const oneway:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, _, oneway_return, [] } ] },
	 _FunNature=oneway, Qualifiers, FunId, Classname ) ->
	case lists:member( const, Qualifiers ) of

		true ->
			wooper_internals:raise_error( { constness_oneway_mistmatch, FunId,
				{ clauses_tell, const }, { uses_spec, {oneway_return,0} },
				{ instead_of, {oneway_const_return,0} }, Classname } );

		false ->
			ok

	end;


% Spec implies const oneway:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, _, oneway_const_return, [] } ] },
	 _FunNature=oneway, Qualifiers, FunId, Classname ) ->
	case lists:member( const, Qualifiers ) of

		true ->
			ok;

		false ->
			wooper_internals:raise_error( { constness_oneway_mistmatch, FunId,
				{ clauses_tell, non_const },
				{ uses_spec, {oneway_const_return,0} },
				{ instead_of, {oneway_return,0} }, Classname } )

	end;


% Spec implies oneway whereas is not:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, _, oneway_return, [] } ] },
	 NonReqFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_error( { incorrect_return_type, FunId,
		{ clauses_tell, NonReqFunNature }, { uses, oneway_return },
									Classname } );


% Wrong arity for oneway_return/1:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, _, oneway_return, Types } ] },
	 _AnyFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_error( { wrong_oneway_return_arity,
			{ expected, 0 }, { got, length( Types ) }, FunId, Classname } );


% Wrong arity for oneway_const_return/1:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, _, oneway_const_return, Types } ] },
	 _AnyFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_error( { wrong_oneway_const_return_arity,
			{ expected, 0 }, { got, length( Types ) }, FunId, Classname } );


% Spec implies non-const oneway:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, _, oneway_return, [ _RType ] } ] },
	 _FunNature=oneway, Qualifiers, FunId, Classname ) ->
	case lists:member( const, Qualifiers ) of

		true ->
			wooper_internals:raise_error( { constness_oneway_mistmatch, FunId,
				{ clauses_tell, const }, { uses_spec, {oneway_return,1} },
				{ instead_of, {oneway_const_return,1} }, Classname } );

		false ->
			ok

	end;


% Spec implies const oneway:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, _, oneway_const_return, [ _RType ] } ] },
	 _FunNature=oneway, Qualifiers, FunId, Classname ) ->
	case lists:member( const, Qualifiers ) of

		true ->
			ok;

		false ->
			wooper_internals:raise_error( { constness_oneway_mistmatch, FunId,
				{ clauses_tell, non_const },
				{ uses_spec, {oneway_const_return,1} },
				{ instead_of, {oneway_return,1} }, Classname } )

	end;



%% For static methods:


% Spec implies static method:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, _, static_return, [ _RType ] } ] },
	 _FunNature=static, _Qualifiers, _FunId, _Classname ) ->
	ok;


% Spec implies static method whereas is not:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, _, static_return, [ _RType ] } ] },
	 NonReqFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_error( { incorrect_return_type, FunId,
		{ clauses_tell, NonReqFunNature }, { uses, static_return } },
								  Classname );


% Wrong arity for static_return/1:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, _, static_return, Types } ] },
	 _AnyFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_error( { wrong_static_return_arity,
			{ expected, 1 }, { got, length( Types ) }, FunId }, Classname );


%% For functions:

check_clause_spec( { type, _, 'fun',
					 _Seqs=[ _TypeProductForArgs, _ResultType ] },
				   _FunNature=function, _Qualifiers, _FunId, _Classname ) ->
	ok;


%% For unmatched spec returns:

% Presumably a rogue request (not using the right return type):
check_clause_spec( { type, _, 'fun',
					 _Seqs=[ _TypeProductForArgs, _ResultType ] },
				   _FunNature=request, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_error( { incorrect_return_type, FunId,
		{ clauses_tell, request }, { not_using, {request_return,1} },
									Classname } );

% Rogue oneway:
check_clause_spec( { type, _, 'fun',
					 _Seqs=[ _TypeProductForArgs, _ResultType ] },
				   _FunNature=oneway, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_error( { incorrect_return_type, FunId,
		{ clauses_tell, oneway }, { not_using, {oneway_return,0} },
									Classname } );
		%{ is, oneway }, { not_using, {oneway_return,0} } , Classname,
		%  ResultType } );

% Rogue static method:
check_clause_spec( { type, _, 'fun',
					 _Seqs=[ _TypeProductForArgs, _ResultType ] },
				   _FunNature=static, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_error( { incorrect_return_type, FunId,
		{ clauses_tell, static }, { not_using, {static_return,0} },
									Classname } );

check_clause_spec( _UnexpectedTypeForm, FunNature, _Qualifiers, FunId,
				   Classname ) ->
	wooper_internals:raise_error( { unexpected_return_type, FunId,
								  { clauses_tell, FunNature }, Classname } ).




% Checks that, in the specified clauses of specified function (corresponding to
% a request or a oneway), the first parameter is 'State' indeed.
%
% Note: enforces a very welcome convention, but also complies with the
% expression that the support for example of return_result_from_const/1
% introduces (ex: { var, LineCall, 'State'} added in the AST, hence the variable
% name.).
%
-spec check_state_argument( [ meta_utils:clause_def() ],
							ast_info:function_id() ) -> void().
check_state_argument( Clauses, FunId ) ->
	[ check_clause_for_state( C, FunId ) || C <- Clauses ].



% (helper)
%
check_clause_for_state(
  _Clause={ clause, _, _Params=[ {var,_,'State'} | _ ], [], _Body }, _FunId ) ->
	ok;

check_clause_for_state(
  _Clause={ clause, _, _Params=[ {var,Line,NonState} | _ ], [], _Body },
  FunId ) ->
	wooper_internals:raise_error( { non_state_initial_parameter, NonState,
									FunId, { line, Line } } );

% Should a non-var form be found, we were considering not halting the
% transformation (as the compiler would raise an error afterwards), however it
% would then report "variable 'State' is unbound", which is less clear than:
%
check_clause_for_state( _Clause, FunId ) ->
	wooper_internals:raise_error( { invalid_state_initial_parameter,
									FunId } ).



% Infers the nature of the corresponding function and any relevant method
% qualifier(s), ensures that all method terminators correspond, and transforms
% them appropriately, in one pass.
%
% We consider that no method is to be explicitly exported and that all actual
% clauses of a method must explicitly terminate with a WOOPER method terminator
% (of course the same for all clauses), rather than calling an helper function
% that would use such a terminator (otherwise the nature of methods could not be
% auto-detected, as there would be no way to determine whether said helper
% should be considered as a method or not).
%
-spec manage_method_terminators( meta_utils:clause_def(),
								 meta_utils:function_id() ) ->
		{ meta_utils:clause_def(), function_nature(), method_qualifiers() }.
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
	% transformation_state field, which contains a value of type:
	%       maybe( { function_nature(), method_qualifiers() }
	%
	% Indeed it starts as 'undefined', then the first terminal call branch of
	% the first clause allows to detect and set the function nature, and
	% afterwards all other branches are checked against it, and transformed.

	% As empty bodies may happen (ex: 'receive' without an 'after'):
	BodyTransformFun = fun

		( _BodyExprs=[], Transforms ) ->
			{ _Exprs=[], Transforms };

		% Commented-out as the last expression is managed differently (we cannot
		% recurse easily), but the spirit remains:
		%( _BodyExprs=[ LastExpr ], Transforms ) ->
		%    ast_expression:transform_expressions( LastExpr, Transforms );

		% Processes specifically each last expression of a body to be
		% transformed (and only them), as it is the place where we can guess the
		% nature of a function and possibly, if it is a method, at least some of
		% its qualifiers.
		%
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

	% In charge of detecting the method terminators and qualifiers:
	%
	% (anonymous mute variables correspond to line numbers)
	%
	CallTransformFun = fun

		% First, requests:

		% Better use the most precise return pseudo-function if this clause is
		% const (since returning the initial State):
		%
		( _LineCall,
		  _FunctionRef={ remote, _, {atom,_,wooper},
						 {atom,_,return_state_result} },
		  _Params=[ _StateExpr={ var, Line, 'State'}, _ResExpr ],
		  _Transforms ) ->
			wooper_internals:raise_error( { const_request_clause, FunId,
							{ shall_use, {return_result_from_const,1} },
							{ instead_of, {return_state_result,2} },
							{ line, Line } } );


		% First (correct, a priori non-const) request detection:
		( LineCall,
		  _FunctionRef={ remote, _, {atom,_,wooper},
						 {atom,_,return_state_result} },
		  Params=[ _StateExpr, _ResExpr ],
		  Transforms=#ast_transforms{ transformation_state=undefined } ) ->

			%trace_utils:debug_fmt( "~s/~B detected as a non-const request.",
			%					   pair:to_list( FunId ) ),

			% So that wooper:return_state_result( S, R ) becomes simply:
			% { S, R }:
			NewExpr = { tuple, LineCall, Params },
			NewTransforms = Transforms#ast_transforms{
							  transformation_state={ request, [] } },
			{ [ NewExpr ], NewTransforms };


		% Already detected as a request that is expected to be non-const,
		% checking qualifiers:
		%
		( LineCall,
		  _FunctionRef={ remote, _, {atom,_,wooper},
						 {atom,_,return_state_result} },
		  Params=[ _StateExpr, _ResExpr ],
		  Transforms=#ast_transforms{
			transformation_state={ request, Qualifiers } } ) ->

			case lists:member( const, Qualifiers ) of

				% Mistmatch:
				true ->
					wooper_internals:raise_error( { request_constness_mismatch,
							{ line, LineCall }, {detected_as,const},
							{ uses, {return_state_result,2} },
							{ instead_of, {return_result_from_const,1} } } );

				% Confirmed:
				false ->
					NewExpr = { tuple, LineCall, Params },
					{ [ NewExpr ], Transforms }

			end;


		% Faulty return_state_result/2 arity:
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
			transformation_state={ OtherNature, _Qualifiers } } ) ->
			wooper_internals:raise_error( { method_terminator_mismatch,
				{ was, OtherNature }, { detected, request },
				FunId, { line, LineCall } } );




		% First (correct, a priori const) request detection:
		( LineCall,
		  _FunctionRef={ remote, _, {atom,_,wooper},
						 {atom,_,return_result_from_const} },
		  Params=[ _ResExpr ],
		  Transforms=#ast_transforms{ transformation_state=undefined } ) ->

			%trace_utils:debug_fmt( "~s/~B detected as a const request.",
			%					   pair:to_list( FunId ) ),

			% So that wooper:return_result_from_const( R ) becomes simply:
			% { S, R }:
			NewExpr = { tuple, LineCall,
						[ { var, LineCall, 'State' } | Params ] },
			NewTransforms = Transforms#ast_transforms{
							  transformation_state={ request, [ const ] } },
			{ [ NewExpr ], NewTransforms };


		% Already detected as a request that is expected to be const, checking
		% qualifiers:
		%
		( LineCall,
		  _FunctionRef={ remote, _, {atom,_,wooper},
						 {atom,_,return_result_from_const} },
		  Params=[ _ResExpr ],
		  Transforms=#ast_transforms{
			transformation_state={ request, Qualifiers } } ) ->

			case lists:member( const, Qualifiers ) of

				% Confirmed:
				true ->
					NewExpr = { tuple, LineCall,
								[ { var, LineCall, 'State' } | Params ] },
					{ [ NewExpr ], Transforms };

				% Mistmatch:
				false ->
					wooper_internals:raise_error( { request_constness_mismatch,
							{ line, LineCall }, {detected_as,non_const},
							{ uses, {return_result_from_const,1} },
							{ instead_of, {return_state_result,2} } } )

			end;


		% Faulty return_result_from_const/1 arity:
		( LineCall,
		  _FunctionRef={ remote, _, {atom,_,wooper},
						 {atom,_,return_result_from_const} },
		  Params,
		  _Transforms ) when length( Params ) =/= 1 ->
			wooper_internals:raise_error( { wrong_arity, { line, LineCall },
			  { wooper, return_result_from_const, { got, length( Params ) },
				{ expected, 1 } }, FunId } );


		% Nature mismatch:
		( LineCall,
		  _FunctionRef={ remote, _, {atom,_,wooper},
						 {atom,_,return_result_from_const} },
		  _Params,
		  _Transforms=#ast_transforms{
			transformation_state={ OtherNature, _Qualifiers } } ) ->
			wooper_internals:raise_error( { method_terminator_mismatch,
				{ was, OtherNature }, { detected, request },
				FunId, { line, LineCall } } );



		% Second, oneways:

		% Better use the most precise return pseudo-function if this clause is
		% const (since returning the initial State):
		%
		( _LineCall,
		  _FunctionRef={ remote, _, {atom,_,wooper},
						 {atom,_,return_state_only} },
		  _Params=[ _StateExpr={ var, Line, 'State'} ],
		  _Transforms ) ->
			wooper_internals:raise_error( { const_oneway_clause, FunId,
							{ shall_use, {return_result_from_const,0} },
							{ instead_of, {return_state_only,1} },
							{ line, Line } } );

		% First (correct, a priori non-const) oneway detection:
		( _LineCall,
		  _FunctionRef={ remote, _, {atom,_,wooper},
						 {atom,_,return_state_only} },
		  _Params=[ StateExpr ],
		  Transforms=#ast_transforms{ transformation_state=undefined } ) ->

			%trace_utils:debug_fmt( "~s/~B detected as a non-const oneway.",
			%					   pair:to_list( FunId ) ),

			% So that wooper:return_state_only( R ) becomes simply R:
			NewExpr = StateExpr,
			NewTransforms = Transforms#ast_transforms{
							  transformation_state={ oneway, [] } },
			{ [ NewExpr ], NewTransforms };


		% Already detected as a oneway that is expected to be non-const,
		% checking qualifiers:
		%
		( LineCall,
		  _FunctionRef={ remote, _, {atom,_,wooper},
						 {atom,_,return_state_only} },
		  _Params=[ StateExpr ],
		  Transforms=#ast_transforms{
						transformation_state={ oneway, Qualifiers } } ) ->

			case lists:member( const, Qualifiers ) of

				% Mistmatch:
				true ->
					wooper_internals:raise_error( { oneway_constness_mismatch,
							{ line, LineCall }, {detected_as,const},
							{ uses, {return_state_only,1} },
							{ instead_of, {return_from_const,0} } } );

				% Confirmed:
				false ->
					NewExpr = StateExpr,
					{ [ NewExpr ], Transforms }

			end;


		% Faulty return_state_only/1 arity:
		( LineCall,
		  _FunctionRef={ remote, _, {atom,_,wooper},
						 {atom,_,return_state_only} },
		  Params,
		  _Transforms ) when length( Params ) =/= 1 ->
			wooper_internals:raise_error( { wrong_arity, { line, LineCall },
			  { wooper, return_state_only, { got, length( Params ) },
				{ expected, 1 } }, FunId } );


		% Nature mismatch:
		( LineCall,
		  _FunctionRef={ remote, _, {atom,_,wooper},
						 {atom,_,return_state_only} },
		  _Params,
		  _Transforms=#ast_transforms{
			transformation_state={ OtherNature, _Qualifiers } } ) ->
			wooper_internals:raise_error( { method_terminator_mismatch,
				{ was, OtherNature }, { detected, oneway },
				FunId, { line, LineCall } } );


		% First (correct, a priori const) oneway detection:
		( LineCall,
		  _FunctionRef={ remote, _, {atom,_,wooper},
						 {atom,_,return_from_const} },
		  _Params=[],
		  Transforms=#ast_transforms{ transformation_state=undefined } ) ->

			%trace_utils:debug_fmt( "~s/~B detected as a const oneway.",
			%					   pair:to_list( FunId ) ),

			% So that wooper:return_from_const() becomes simply S:
			NewExpr = { var, LineCall, 'State' },
			NewTransforms = Transforms#ast_transforms{
							  transformation_state={ oneway, [ const ] } },
			{ [ NewExpr ], NewTransforms };


		% Already detected as a oneway that is expected to be const, checking
		% qualifiers:
		%
		( LineCall,
		  _FunctionRef={ remote, _, {atom,_,wooper},
						 {atom,_,return_from_const} },
		  _Params=[],
		  Transforms=#ast_transforms{
						transformation_state={ oneway, Qualifiers } } ) ->

			case lists:member( const, Qualifiers ) of

				% Confirmed:
				true ->
					NewExpr = { var, LineCall, 'State' },
					{ [ NewExpr ], Transforms };

				% Mistmatch:
				false ->
					wooper_internals:raise_error( { oneway_constness_mismatch,
							{ line, LineCall }, {detected_as,non_const},
							{ uses, {return_from_const,0} },
							{ instead_of, {return_state_only,1} } } )

			end;


		% Faulty return_from_const/0 arity:
		( LineCall,
		  _FunctionRef={ remote, _, {atom,_,wooper},
						 {atom,_,return_state_only} },
		  Params,
		  _Transforms ) when Params =/= [] ->
			wooper_internals:raise_error( { wrong_arity, { line, LineCall },
			  { wooper, return_state_only, { got, length( Params ) },
				{ expected, 0 } }, FunId } );


		% Nature mismatch:
		( LineCall,
		  _FunctionRef={ remote, _, {atom,_,wooper},
						 {atom,_,return_from_const} },
		  _Params,
		  _Transforms=#ast_transforms{
			transformation_state={ OtherNature, _Qualifiers } } ) ->
			wooper_internals:raise_error( { method_terminator_mismatch,
				{ was, OtherNature }, { detected, oneway },
				FunId, { line, LineCall } } );


		% Third, static methods:

		% First (correct) static method detection:
		( _LineCall,
		  _FunctionRef={ remote, _, {atom,_,wooper},
						 {atom,_,return_static} },
		  _Params=[ ResultExpr ],
		  Transforms=#ast_transforms{ transformation_state=undefined } ) ->

			%trace_utils:debug_fmt( "~s/~B detected as a static method.",
			%					   pair:to_list( FunId ) ),

			% So that wooper:return_static( R ) becomes simply R:
			NewExpr = ResultExpr,
			NewTransforms = Transforms#ast_transforms{
							  transformation_state={ static, [] } },
			{ [ NewExpr ], NewTransforms };


		% Already detected as a static:
		%
		% (currently exactly the same clause as above, as qualifiers do not
		% matter for static:
		%
		( _LineCall,
		  _FunctionRef={ remote, _, {atom,_,wooper},
						 {atom,_,return_static} },
		  _Params=[ ResultExpr ],
		  Transforms=#ast_transforms{
						transformation_state={ static, _Qualifiers } } ) ->

			%trace_utils:debug_fmt( "~s/~B confirmed as a static method.",
			%					   pair:to_list( FunId ) ),

			% So that wooper:return_static( R ) becomes simply R:
			NewExpr = ResultExpr,
			NewTransforms = Transforms,
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
			transformation_state={ OtherNature, _Qualifiers } } ) ->
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
	{ FunNature, Qualifiers } =
			  case NewTransforms#ast_transforms.transformation_state of

		undefined ->
			%trace_utils:debug_fmt( "~s/~B detected as a plain function.",
			%					   pair:to_list( FunId ) ),
			{ function, _Qualifiers=[] };

		% Ex: { request, [ const ] }
		Other ->
			Other

	end,

	{ NewClauses, FunNature, Qualifiers }.



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




% Section for the conversion of information records.


% Converts specified (Myriad-level) function information into a (WOOPER-level)
% request information.
%
-spec function_to_request_info( function_info() ) -> request_info().
function_to_request_info( #function_info{ name=Name,
										  arity=Arity,
										  location=Loc,
										  line=Line,
										  clauses=Clauses,
										  spec=Spec,
										  callback=false,
										  exported=[] } ) ->
	#request_info{ name=Name,
				   arity=Arity,
				   %qualifiers=[]
				   location=Loc,
				   line=Line,
				   clauses=Clauses,
				   spec=Spec };


function_to_request_info( #function_info{ name=Name,
										  arity=Arity,
										  location=Loc,
										  line=Line,
										  clauses=Clauses,
										  spec=Spec,
										  callback=false
										  %exported
										} ) ->

	wooper_internals:notify_warning( [ text_utils:format(
		"~s/~B should not be explicitly exported, since requests "
		"are automatically exported.", [ Name, Arity ] ) ] ),

	#request_info{ name=Name,
				   arity=Arity,
				   %qualifiers=[]
				   location=Loc,
				   line=Line,
				   clauses=Clauses,
				   spec=Spec };


function_to_request_info( Other ) ->
	throw( { unexpected_function_info, Other, request } ).



% Converts specified (WOOPER-level) request information into a (Myriad-level)
% function information.
%
-spec request_to_function_info( request_info(), location() ) -> function_info().
request_to_function_info( #request_info{ name=Name,
										 arity=Arity,
										 % Unused: qualifiers
										 location=Loc,
										 line=Line,
										 clauses=Clauses,
										 spec=Spec },
						  Location ) ->
	#function_info{ name=Name,
					arity=Arity,
					location=Loc,
					line=Line,
					clauses=Clauses,
					spec=Spec,
					callback=false,
					exported=[ Location ] };

request_to_function_info( Other, _Location ) ->
	throw( { unexpected_request_info, Other, request } ).



% Converts specified (Myriad-level) function information into a (WOOPER-level)
% oneway information.
%
-spec function_to_oneway_info( function_info() ) -> oneway_info().
function_to_oneway_info( #function_info{ name=Name,
										 arity=Arity,
										 location=Loc,
										 line=Line,
										 clauses=Clauses,
										 spec=Spec,
										 callback=false,
										 exported=[] } ) ->
	#oneway_info{ name=Name,
				  arity=Arity,
				  %qualifiers=[]
				  location=Loc,
				  line=Line,
				  clauses=Clauses,
				  spec=Spec };


function_to_oneway_info( #function_info{ name=Name,
										 arity=Arity,
										 location=Loc,
										 line=Line,
										 clauses=Clauses,
										 spec=Spec,
										 callback=false
										 %exported
									   } ) ->

	wooper_internals:notify_warning( [ text_utils:format(
		"~s/~B should not be explicitly exported, since oneways "
		"are automatically exported.", [ Name, Arity ] ) ] ),

	#oneway_info{ name=Name,
				  arity=Arity,
				  %qualifiers=[]
				  location=Loc,
				  line=Line,
				  clauses=Clauses,
				  spec=Spec };


function_to_oneway_info( Other ) ->
	throw( { unexpected_function_info, Other, oneway } ).



% Converts specified (WOOPER-level) oneway information into a (Myriad-level)
% function information.
%
-spec oneway_to_function_info( oneway_info(), location() ) -> function_info().
oneway_to_function_info( #oneway_info{ name=Name,
									   arity=Arity,
									   % Unused: qualifiers
									   location=Loc,
									   line=Line,
									   clauses=Clauses,
									   spec=Spec },
						 Location ) ->
	#function_info{ name=Name,
					arity=Arity,
					location=Loc,
					line=Line,
					clauses=Clauses,
					spec=Spec,
					callback=false,
					exported=[ Location ] };

oneway_to_function_info( Other, _Location ) ->
	throw( { unexpected_oneway_info, Other, oneway } ).



% Converts specified (Myriad-level) function information into a (WOOPER-level)
% static information.
%
-spec function_to_static_info( function_info() ) -> static_info().
function_to_static_info( #function_info{ name=Name,
										 arity=Arity,
										 location=Loc,
										 line=Line,
										 clauses=Clauses,
										 spec=Spec,
										 callback=false,
										 exported=[] } ) ->
	#static_info{ name=Name,
				  arity=Arity,
				  %qualifiers=[]
				  location=Loc,
				  line=Line,
				  clauses=Clauses,
				  spec=Spec };


function_to_static_info( #function_info{ name=Name,
										 arity=Arity,
										 location=Loc,
										 line=Line,
										 clauses=Clauses,
										 spec=Spec,
										 callback=false
										 %exported
									   } ) ->

	wooper_internals:notify_warning( [ text_utils:format(
		"~s/~B should not be explicitly exported, since requests "
		"are automatically exported.", [ Name, Arity ] ) ] ),

	#static_info{ name=Name,
				  arity=Arity,
				  %qualifiers=[]
				  location=Loc,
				  line=Line,
				  clauses=Clauses,
				  spec=Spec };


function_to_static_info( Other ) ->
	throw( { unexpected_function_info, Other, static } ).



% Converts specified (WOOPER-level) static information into a (Myriad-level)
% function information.
%
-spec static_to_function_info( static_info(), location() ) ->
									 function_info().
static_to_function_info( #static_info{ name=Name,
									   arity=Arity,
									   % Unused: qualifiers
									   location=Loc,
									   line=Line,
									   clauses=Clauses,
									   spec=Spec },
						 Location ) ->
	#function_info{ name=Name,
					arity=Arity,
					location=Loc,
					line=Line,
					clauses=Clauses,
					spec=Spec,
					callback=false,
					exported=[ Location ] };

static_to_function_info( Other, _Location ) ->
	throw( { unexpected_static_info, Other, static } ).



% Transforms the methods in the specified tables into functions, and adds them
% in specified function table.
%
-spec methods_to_functions( request_table(), oneway_table(), static_table(),
							function_table(), marker_table() ) ->
								  function_table().
methods_to_functions( RequestTable, OnewayTable, StaticTable,
					  InitFunctionTable, MarkerTable ) ->

	% Methods shall not be exported, but their corresponding functions surely
	% should, otherwise they could be reported as unused:
	%
	ExportLoc = ast_info:get_default_export_function_location( MarkerTable ),

	RequestPairs = table:enumerate( RequestTable ),

	RequestAsFunPairs = [
		 { ReqId, request_to_function_info( ReqInfo, ExportLoc ) }
						  || { ReqId, ReqInfo } <- RequestPairs ],

	WithRequestsFunTable = table:addNewEntries( RequestAsFunPairs,
												InitFunctionTable ),


	OnewayPairs = table:enumerate( OnewayTable ),

	OnewayAsFunPairs = [
		 { OnwId, oneway_to_function_info( OnwInfo, ExportLoc ) }
						  || { OnwId, OnwInfo } <- OnewayPairs ],

	WithOnewaysFunTable = table:addNewEntries( OnewayAsFunPairs,
											   WithRequestsFunTable ),


	StaticPairs = table:enumerate( StaticTable ),

	StaticAsFunPairs = [
		 { StId, static_to_function_info( StInfo, ExportLoc ) }
						  || { StId, StInfo } <- StaticPairs ],

	table:addNewEntries( StaticAsFunPairs, WithOnewaysFunTable ).
