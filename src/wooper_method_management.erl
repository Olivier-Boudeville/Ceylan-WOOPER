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


-export([ manage_methods/1, body_transformer/2, call_transformer/4,
		  get_blank_transformation_state/0,
		  ensure_exported/2, ensure_exported_at/2, ensure_all_exported_in/2,
		  methods_to_functions/5,
		  check_spec/4, check_clause_spec/5, check_state_argument/3,
		  function_nature_to_string/1,

		  function_to_request_info/1, request_to_function_info/2,
		  function_to_oneway_info/1, oneway_to_function_info/2,
		  function_to_static_info/1, static_to_function_info/2 ]).


% For the function_info record:
-include("ast_info.hrl").

% For the ast_transforms record:
-include("ast_transform.hrl").


% For the class_info record:
-include("wooper_info.hrl").


% The (WOOPER-level) nature of a given Erlang function.
%
-type function_nature() :: 'constructor' | 'destructor'
						 | 'request' | 'oneway' | 'static' | 'function'.


% Shorthands:

-type function_info() :: ast_info:function_info().
-type marker_table() :: ast_info:section_marker_table().
-type location() :: ast_base:form_location().
-type function_table() :: ast_info:function_table().
-type line() :: ast_base:line().
-type ast_transforms() :: ast_transform:ast_transforms().
-type ast_body() :: ast_clause:ast_body().

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

	ExportLoc = ast_info:get_default_export_function_location( MarkerTable ),

	% FunctionTable starts from scratch as all functions are to be found in
	% AllFunEntries:
	%
	{ NewFunctionTable, NewRequestTable, NewOnewayTable, NewStaticTable } =
		sort_out_functions( AllFunEntries, _FunctionTable=table:new(),
			RequestTable, OnewayTable, StaticTable, Classname, ExportLoc ),


	% Split as { Functions, Methods }:
	{ NewFunctionTable, ClassInfo#class_info{ requests=NewRequestTable,
											  oneways=NewOnewayTable,
											  statics=NewStaticTable } }.



% Transforms and categorises each of the specified functions according to its
% real nature (ex: a given Erlang function may actually be a WOOPER oneway).
%
sort_out_functions( _FunEntries=[], FunctionTable, RequestTable, OnewayTable,
					StaticTable, _Classname, _ExportLoc ) ->
	{ FunctionTable, RequestTable, OnewayTable, StaticTable };

% Checks that all sorted functions have an actual implementation:
sort_out_functions( _FunEntries=[ { FunId, #function_info{
											  clauses=[],
											  spec=Spec } } | _T ],
					_FunctionTable, _RequestTable, _OnewayTable, _StaticTable,
					Classname, _ExportLoc ) when Spec =/= undefined ->
	wooper_internals:raise_usage_error(
	  "function ~s/~B has a type specification, yet has never been defined.",
	  pair:to_list( FunId ), Classname );

sort_out_functions( _FunEntries=[ { FunId, FunInfo=#function_info{
											clauses=OriginalClauses,
											spec=Spec } } | T ],
					FunctionTable, RequestTable, OnewayTable, StaticTable,
					Classname, ExportLoc ) ->

	%trace_utils:debug_fmt( "Examining Erlang function ~s/~B",
	%                       pair:to_list( FunId ) ),

	% We used to infer the function nature based on its first clause, and then
	% to make a custom full traversal to transform method terminators.
	%
	% Now we reuse the Myriad ast_transforms instead, and perform everything
	% (guessing/checking/transforming) in one pass:
	%
	{ NewClauses, FunNature, Qualifiers } =
		manage_method_terminators( OriginalClauses, FunId, Classname ),

	%trace_utils:debug_fmt( "~p is a ~s whose qualifiers are ~p.",
	%					   [ FunId, function_nature_to_string( FunNature ),
	%						 Qualifiers ] ),

	NewFunInfo = FunInfo#function_info{ clauses=NewClauses },

	% Stores the result in the right category and recurses:
	%
	% (we ensure here that methods are exported, while they are still
	% function_info records)
	%
	case FunNature of

		function ->
			check_spec( Spec, function, Qualifiers, Classname ),
			NewFunctionTable =
				table:addNewEntry( FunId, NewFunInfo, FunctionTable ),
			sort_out_functions( T, NewFunctionTable, RequestTable,
					OnewayTable, StaticTable, Classname, ExportLoc );

		request ->
			check_spec( Spec, request, Qualifiers, Classname ),
			check_state_argument( NewClauses, FunId, Classname ),
			ExportedFunInfo = ensure_exported_at( NewFunInfo, ExportLoc ),
			RequestInfo = function_to_request_info( ExportedFunInfo ),
			NewRequestTable = table:addNewEntry( FunId, RequestInfo,
												 RequestTable ),
			sort_out_functions( T, FunctionTable, NewRequestTable,
					OnewayTable, StaticTable, Classname, ExportLoc );

		oneway ->
			check_spec( Spec, oneway, Qualifiers, Classname ),
			check_state_argument( NewClauses, FunId, Classname ),
			ExportedFunInfo = ensure_exported_at( NewFunInfo, ExportLoc ),
			OnewayInfo = function_to_oneway_info( ExportedFunInfo ),
			NewOnewayTable = table:addNewEntry( FunId, OnewayInfo,
												OnewayTable ),
			sort_out_functions( T, FunctionTable, RequestTable,
					NewOnewayTable, StaticTable, Classname, ExportLoc );

		static ->
			check_spec( Spec, static, Qualifiers, Classname ),
			ExportedFunInfo = ensure_exported_at( NewFunInfo, ExportLoc ),
			StaticInfo = function_to_static_info( ExportedFunInfo ),
			NewStaticTable = table:addNewEntry( FunId, StaticInfo,
												StaticTable ),
			sort_out_functions( T, FunctionTable, RequestTable,
					OnewayTable, NewStaticTable, Classname, ExportLoc )

	end;

sort_out_functions( _Functions=[ #function_info{ name=FunName,
												 arity=Arity } | _T ],
					_FunctionTable, _RequestTable, _OnewayTable,
					_StaticTable, Classname, _ExportLoc ) ->

	% Error raised directly, could be appended to the class_info.errors:
	wooper_internals:raise_usage_error( "no clause found for ~s/~B; "
			"function exported yet not defined?",
			[ FunName, Arity ], Classname ).



% Checks that the method spec (if any) corresponds indeed to the right type of
% method, i.e. relies on the right method terminators.
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




% Checks the specified method clause spec.
%
% (helper)

%% For requests:

% Spec implies non-const request:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, request_return, [ _RType ] } ] },
	 _FunNature=request, Qualifiers, FunId, Classname ) ->

	case lists:member( const, Qualifiers ) of

		true ->
			wooper_internals:raise_usage_error( "the ~s/~B request has been "
				"detected as const, however its spec uses request_return/1 "
				"instead of const_request_return/1.", pair:to_list( FunId ),
				Classname, Line );

		false ->
			ok

	end;


% Spec implies const request:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, const_request_return, [ _RType ] } ] },
	 _FunNature=request, Qualifiers, FunId, Classname ) ->
	case lists:member( const, Qualifiers ) of

		true ->
			ok;

		false ->
			wooper_internals:raise_usage_error( "the ~s/~B request has been "
				"detected as non-const, however its spec uses "
				"const_request_return/1 instead of request_return/1.",
				pair:to_list( FunId ), Classname, Line )

	end;


% Spec implies (non-const) request whereas is not:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, request_return, [ _RType ] } ] },
	 NonReqFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "~s/~B has been detected as a ~s, "
		"not as a (non-const) request, however its spec uses request_return/1.",
		pair:to_list( FunId )
			++ [ function_nature_to_string( NonReqFunNature ) ],
		Classname, Line );


% Spec implies (const) request whereas is not:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, const_request_return, [ _RType ] } ] },
	 NonReqFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "~s/~B has been detected as a ~s, "
		"not as a (const) request, however its spec uses "
		"const_request_return/1.",
		pair:to_list( FunId )
			++ [ function_nature_to_string( NonReqFunNature ) ],
		Classname, Line );


% Wrong arity for request_return/1:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, request_return, Types } ] },
	 _AnyFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "~s/~B uses request_return/~B, "
		"which does not exist; its correct arity is 1.",
		pair:to_list( FunId ) ++ [ length( Types ) ], Classname, Line );


% Wrong arity for const_request_return/1:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, const_request_return, Types } ] },
	 _AnyFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "~s/~B uses const_request_return/~B, "
		"which does not exist; its correct arity is 1.",
		pair:to_list( FunId ) ++ [ length( Types ) ], Classname, Line );


% *_result used instead of *_return:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, const_request_result, _Types } ] },
	 _AnyFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "~s/~B uses the (unknown) "
		"const_request_result type: probably that "
		"const_request_return/1 was meant instead.",
		pair:to_list( FunId ), Classname, Line );


check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, request_result, _Types } ] },
	 _AnyFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "~s/~B uses the (unknown) "
		"request_result type: probably that "
		"request_return/1 was meant instead.",
		pair:to_list( FunId ), Classname, Line );



%% For oneways:

% Spec implies non-const oneway:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, oneway_return, [] } ] },
	 _FunNature=oneway, Qualifiers, FunId, Classname ) ->
	case lists:member( const, Qualifiers ) of

		true ->
			wooper_internals:raise_usage_error( "the ~s/~B oneway has been "
				"detected as const, however its spec uses oneway_return/0 "
				"instead of const_oneway_return/0.", pair:to_list( FunId ),
				Classname, Line );

		false ->
			ok

	end;


% Spec implies const oneway:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, const_oneway_return, [] } ] },
	 _FunNature=oneway, Qualifiers, FunId, Classname ) ->
	case lists:member( const, Qualifiers ) of

		true ->
			ok;

		false ->
			wooper_internals:raise_usage_error( "the ~s/~B oneway has been "
				"detected as non-const, however its spec uses "
				"const_oneway_return/0 instead of oneway_return/0.",
				pair:to_list( FunId ), Classname, Line )

	end;


% Spec implies (non-const) oneway whereas is not:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, oneway_return, [] } ] },
	 NonOnwFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "~s/~B has been detected as a ~s, "
		"not as a (non-const) oneway, however its spec uses oneway_return/0.",
		pair:to_list( FunId )
			++ [ function_nature_to_string( NonOnwFunNature ) ],
		Classname, Line );


% Spec implies (const) oneway whereas is not:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, const_oneway_return, [] } ] },
	 NonOnwFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "~s/~B has been detected as a ~s, "
		"not as a (const) oneway, however its spec uses const_oneway_return/0.",
		pair:to_list( FunId )
			++ [ function_nature_to_string( NonOnwFunNature ) ],
		Classname, Line );


% Wrong arity for oneway_return/0:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, oneway_return, Types } ] },
	 _AnyFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "~s/~B uses oneway_return/~B, "
		"which does not exist; its correct arity is 0.",
		[ length( Types ) | pair:to_list( FunId ) ], Classname, Line );


% Wrong arity for const_oneway_return/0:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, const_oneway_return, Types } ] },
	 _AnyFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "~s/~B uses const_oneway_return/~B, "
		"which does not exist; its correct arity is 0.",
		[ length( Types ) | pair:to_list( FunId ) ], Classname, Line );


% *_result used instead of *_return:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, const_oneway_result, _Types } ] },
	 _AnyFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "~s/~B uses the (unknown) "
		"const_oneway_result type: probably that "
		"const_oneway_return/0 was meant instead.",
		pair:to_list( FunId ), Classname, Line );


check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, oneway_result, _Types } ] },
	 _AnyFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "~s/~B uses the (unknown) "
		"oneway_result type: probably that "
		"oneway_return/0 was meant instead.",
		pair:to_list( FunId ), Classname, Line );



%% For static methods:


% Spec implies static method:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, _, static_return, [ _RType ] } ] },
	 _FunNature=static, _Qualifiers, _FunId, _Classname ) ->
	ok;


% Spec implies static method whereas is not:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, static_return, [ _RType ] } ] },
	 NonStatFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "~s/~B has been detected as a ~s, "
		"not as a static method, however its spec uses static_return/1.",
		pair:to_list( FunId )
			++ [ function_nature_to_string( NonStatFunNature ) ],
		Classname, Line );


% Wrong arity for static_return/1:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, static_return, Types } ] },
	 _AnyFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "~s/~B uses static_return/~B, "
		"which does not exist; its correct arity is 1.",
		[ length( Types ) | pair:to_list( FunId ) ], Classname, Line );


% *_result used instead of *_return:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, static_result, _Types } ] },
	 _AnyFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "~s/~B uses the (unknown) "
		"static_result type: probably that "
		"static_return/1 was meant instead.",
		pair:to_list( FunId ), Classname, Line );


%% For functions, nothing special to check:

check_clause_spec( { type, _, 'fun',
					 _Seqs=[ _TypeProductForArgs, _ResultType ] },
				   _FunNature=function, _Qualifiers, _FunId, _Classname ) ->
	ok;


%% For unmatched spec returns:

% Presumably a result spec wrongly qualified as wooper:S instead of S:
check_clause_spec( { type, Line, 'fun',
					 _Seqs=[ _TypeProductForArgs,
					 _ResultType={remote_type,_,[ {atom,_,wooper}, _, _ ] } ] },
				   _FunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "the type specification of ~s/~B "
		"is not expected to rely on a terminator prefixed with the wooper "
		"module (just remove 'wooper:').", pair:to_list( FunId ), Classname,
		Line );

% Presumably a rogue request (not using the right return type):
check_clause_spec( { type, Line, 'fun',
					 _Seqs=[ _TypeProductForArgs, _ResultType ] },
				   _FunNature=request, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "the clauses of ~s/~B indicate "
		"that this is a request, yet in the type specification no known "
		"request terminator is used.", pair:to_list( FunId ), Classname, Line );

% Rogue oneway:
check_clause_spec( { type, Line, 'fun',
					 _Seqs=[ _TypeProductForArgs, _ResultType ] },
				   _FunNature=oneway, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "the clauses of ~s/~B indicate "
		"that this is a oneway, yet in the type specification no known "
		"oneway terminator is used.", pair:to_list( FunId ), Classname,
		Line );


% Rogue static method:
check_clause_spec( { type, Line, 'fun',
					 _Seqs=[ _TypeProductForArgs, _ResultType ] },
				   _FunNature=static, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "the clauses of ~s/~B indicate "
		"that this is a static method, yet in the type specification no known "
		" static method terminator is used.", pair:to_list( FunId ), Classname,
		Line );


check_clause_spec( _UnexpectedTypeForm, FunNature, _Qualifiers, FunId,
				   Classname ) ->
	wooper_internals:raise_usage_error( "unexpected return type in the type "
		"specification of ~s/~B (which is detected as a ~s).",
		pair:to_list( FunId ) ++ [ function_nature_to_string( FunNature ) ],
		Classname, _Line=0 ).



% Returns a textual description of the specified function nature.
%
-spec function_nature_to_string( function_nature() ) -> text_utils:ustring().
function_nature_to_string( request ) ->
	"request";

function_nature_to_string( oneway ) ->
	"oneway";

function_nature_to_string( static ) ->
	"static method";

function_nature_to_string( function ) ->
	"plain function".



% Checks that, in the specified clauses of specified function (corresponding to
% a request or a oneway), the first parameter is 'State' indeed.
%
% Note: enforces a very welcome convention, but also complies with the
% expression that the support for example of const_return_result/1
% introduces (ex: { var, LineCall, 'State'} added in the AST, hence the enforced
% variable name).
%
-spec check_state_argument( [ meta_utils:clause_def() ],
					ast_info:function_id(), wooper:classname() ) -> void().
check_state_argument( Clauses, FunId, Classname ) ->
	[ check_clause_for_state( C, FunId, Classname ) || C <- Clauses ].



% (helper)
%
check_clause_for_state(
  _Clause={ clause, _, _Params=[ {var,_,'State'} | _ ], _Guards, _Body },
  _FunId, _Classname ) ->
	ok;

% Tolerated iff throwing afterwards:
check_clause_for_state(
  _Clause={ clause, _, _Params=[ {var,_,'_State'} | _ ], _Guards, _Body },
  _FunId, _Classname ) ->
	ok;

check_clause_for_state(
  _Clause={ clause, _, _Params=[ {var,Line,NonState} | _ ], _Guards, _Body },
  FunId, Classname ) ->
	wooper_internals:raise_usage_error( "the first parameter of this clause of "
		"method ~s/~B shall be named 'State', not '~s'.",
		pair:to_list( FunId ) ++ [ NonState ], Classname, Line );

% Should a non-var form be found, we were considering not halting the
% transformation (as the compiler would raise an error afterwards), however it
% would then report "variable 'State' is unbound", which is less clear than:
%
check_clause_for_state( _Clause, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "the first parameter of this clause of "
		"method ~s/~B shall be named 'State'.", pair:to_list( FunId ),
		Classname ).



% Infers the nature of the corresponding function and any relevant method
% qualifier(s), ensures that all method terminators correspond, and transforms
% them appropriately, in one pass.
%
% We consider that no method is to be explicitly exported and that all actual
% clauses of a method must explicitly terminate with a WOOPER method terminator
% (the same for all clauses, except regarding constness), rather than calling an
% helper function that would use such a terminator (otherwise the nature of
% methods could not be auto-detected, as there would be no way to determine
% whether said helper should be considered as a method or not).
%
-spec manage_method_terminators( meta_utils:clause_def(),
				   meta_utils:function_id(), wooper:classname() ) ->
		{ meta_utils:clause_def(), function_nature(), method_qualifiers() }.
manage_method_terminators( _Clauses=[], FunId, Classname ) ->
	wooper_internals:raise_usage_error(
	  "the function ~s/~B is exported yet not defined.", pair:to_list( FunId ),
	  Classname );

manage_method_terminators( Clauses, FunId, Classname ) ->

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
	% transformation_state field, which itself contains a value of type:
	%   { maybe( function_nature() ), method_qualifiers(),  } ).
	%
	% Indeed it starts as 'undefined', then the first terminal call branch of
	% the first clause allows to detect and set the function nature, and
	% afterwards all other branches are checked against it, and transformed.

	% Note that a member method is const iff all its clauses are const; so a
	% method is flagged initially as const depending on the constness of its
	% first clause, and loses any initial const status as soon as one clause
	% happens to be non-const (otherwise a non_const flag/qualifier would have
	% to be introduced).


	TransformTable = table:new( [ { body, fun body_transformer/2 },
								  { call, fun call_transformer/4 } ] ),

	Transforms = #ast_transforms{
					transformed_module_name=Classname,
					transform_table=TransformTable,
					transformed_function_identifier=FunId,
					transformation_state=get_blank_transformation_state() },

	%trace_utils:debug_fmt( "transforming now ~p.", [ FunId ] ),

	{ NewClauses, NewTransforms } =
		ast_clause:transform_function_clauses( Clauses, Transforms ),

	% Unless found different, a function is a (plain) function:
	{ FunNature, Qualifiers } =
			  case NewTransforms#ast_transforms.transformation_state of

		{ undefined, _, _WOOPERExportSet } ->
			%trace_utils:debug_fmt( "~s/~B detected as a plain function.",
			%					   pair:to_list( FunId ) ),
			{ function, _Qualifiers=[] };

		% Ex: { request, [ const ], _ }
		{ OtherNature, SomeQualifiers, _WOOPERExportSet } ->
			%trace_utils:debug_fmt( "~s/~B detected as: ~p (qualifiers: ~w)",
			%    pair:to_list( FunId ) ++ [ OtherNature, Qualifiers ] ),
			{ OtherNature, SomeQualifiers }

	end,

	{ NewClauses, FunNature, Qualifiers }.



% Returns a suitable, blank transformation state.
%
% Defined to be safely reused from various locations.
%
get_blank_transformation_state() ->
	{ _FunctionNature=undefined, _Qualifiers=[],
	  _ConstExportSet=wooper:get_exported_functions_set() }.





% Drives the AST transformation of a body: processes specifically each last
% expression of a body to be transformed (and only them), as it is the place
% where we can guess the nature of a function and possibly, if it is a method,
% at least some of its qualifiers.
%
% Used to be an anonymous function, yet now exported so that it can be re-used
% by upper layers.
%
-spec body_transformer( ast_body(), ast_transforms() ) ->
							  { ast_body(), ast_transforms() }.
% As empty bodies may happen (ex: 'receive' without an 'after'):
body_transformer( _BodyExprs=[], Transforms ) ->
	{ _Exprs=[], Transforms };


% Commented-out as the last expression is managed differently (we cannot recurse
% easily), but the spirit remains:
%
%body_transformer( _BodyExprs=[ LastExpr ], Transforms ) ->
%    ast_expression:transform_expressions( LastExpr, Transforms );


% At least an element exists here:
body_transformer( BodyExprs, Transforms ) -> % superfluous: when is_list( BodyExprs ) ->

	% Warning: we currently skip intermediate expressions as a whole (we do not
	% transform them at all, as currently WOOPER does not have any need for
	% that), but maybe in the future this will have to be changed.
	%
	% We cannot use easily a Y-combinator here, as the signature of this
	% anonymous function is constrained: { [ Expr | SomeFun(T) ] }, Transforms }
	%
	% More efficient than list_utils:extract_last_element/2 and then recreating
	% the list:
	[ LastExpr | RevFirstExprs ] = lists:reverse( BodyExprs ),

	{ [ NewLastExpr ], NewTransforms } =
		ast_expression:transform_expression( LastExpr, Transforms ),

	NewExprs = lists:reverse( [ NewLastExpr | RevFirstExprs ] ),

	{ NewExprs, NewTransforms }.





% In charge of detecting the method terminators and qualifiers, in a WOOPER
% context.
%
% (anonymous mute variables correspond to line numbers)
%
-spec call_transformer( line(), ast_expression:function_ref_expression(),
			ast_expression:params_expression(), ast_transforms() ) ->
					  { [ ast_expression:ast_expression() ], ast_transforms() }.

% First, requests:

% Better use the most precise return pseudo-function if this clause is const
% (since returning the initial State):
%
call_transformer( _LineCall, _FunctionRef={ remote, _, {atom,_,wooper},
											{atom,_,return_state_result} },
				  _Params=[ _StateExpr={ var, Line, 'State' }, _ResExpr ],
				  Transforms=#ast_transforms{
					transformed_function_identifier=FunId } ) ->
	wooper_internals:raise_usage_error( "this const clause of "
		"request ~s/~B shall use wooper:const_return_result/1 (instead "
		"of wooper:return_state_result/2).", pair:to_list( FunId ), Transforms,
		Line );

% First (correct, non-const) request detection:
call_transformer( LineCall,
		_FunctionRef={ remote, _, {atom,_,wooper},
						{atom,_,return_state_result} },
		Params=[ _StateExpr, _ResExpr ],
		Transforms=#ast_transforms{
			%transformed_function_identifier=FunId,
			transformation_state={ undefined, _, WOOPERExportSet } } ) ->

	%trace_utils:debug_fmt( "~s/~B detected as a non-const request.",
	%					   pair:to_list( FunId ) ),

	% So that wooper:return_state_result( S, R ) becomes simply { S, R }:
	NewExpr = { tuple, LineCall, Params },

	NewTransforms = Transforms#ast_transforms{
					  transformation_state={ request, [], WOOPERExportSet } },

	{ [ NewExpr ], NewTransforms };


% Already detected as a request, checking qualifiers:
call_transformer( LineCall, _FunctionRef={ remote, _, {atom,_,wooper},
										   {atom,_,return_state_result} },
				  Params=[ _StateExpr, _ResExpr ],
				  Transforms=#ast_transforms{
						%transformed_function_identifier=FunId,
						transformation_state={ request, Qualifiers,
											   WOOPERExportSet } } ) ->

	%trace_utils:debug_fmt( "~s/~B confirmed as a non-const request.",
	%					   pair:to_list( FunId ) ),

	% 'const' may or may not be still there, and will surely not:
	NewQualifiers = lists:delete( const, Qualifiers ),

	NewExpr = { tuple, LineCall, Params },

	NewTransforms = Transforms#ast_transforms{
					  transformation_state={ request, NewQualifiers,
											 WOOPERExportSet } },

	{ [ NewExpr ], NewTransforms };


% Faulty return_state_result/2 arity:
call_transformer( LineCall, _FunctionRef={ remote, _, {atom,_,wooper},
										   {atom,_,return_state_result} },
				  Params,
				  Transforms=#ast_transforms{
								transformed_function_identifier=FunId } )
  when length( Params ) =/= 2 ->
	wooper_internals:raise_usage_error( "wrong arity (~B) specified "
		"for wooper:return_state_result/2, for request ~s/~B.",
		[ length( Params ) | pair:to_list( FunId ) ], Transforms, LineCall );


% Nature mismatch:
call_transformer( LineCall, _FunctionRef={ remote, _, {atom,_,wooper},
										   {atom,_,return_state_result} },
				  _Params,
				  Transforms=#ast_transforms{
					transformed_function_identifier=FunId,
					transformation_state={ OtherNature, _Qualifiers,
										   _WOOPERExportSet } } ) ->
	wooper_internals:raise_usage_error( "method terminator mismatch "
		"for method ~s/~B: wooper:return_state_result/2 implies "
		"request, whereas was detected as a ~s.",
		pair:to_list( FunId ) ++ [ OtherNature ], Transforms, LineCall );


% First (correct, a priori const) request detection:
call_transformer( LineCall,
	  _FunctionRef={ remote, _, {atom,_,wooper}, {atom,_,const_return_result} },
	  Params=[ _ResExpr ],
	  Transforms=#ast_transforms{
			%transformed_function_identifier=FunId,
			transformation_state={ undefined, [], WOOPERExportSet } } ) ->

	%trace_utils:debug_fmt( "~s/~B detected as a const request.",
	%					   pair:to_list( FunId ) ),

	% So that wooper:const_return_result( R ) becomes simply { S, R }:
	NewExpr = { tuple, LineCall, [ { var, LineCall, 'State' } | Params ] },

	NewTransforms = Transforms#ast_transforms{
			  transformation_state={ request, [ const ], WOOPERExportSet } },

	{ [ NewExpr ], NewTransforms };


% Already detected as a request, this clause is const, this will not change
% overall constness status:
%
call_transformer( LineCall, _FunctionRef={ remote, _, {atom,_,wooper},
										   {atom,_,const_return_result} },
				  Params=[ _ResExpr ],
				  Transforms=#ast_transforms{
						transformation_state={ request, _Qualifiers,
											   _WOOPERExportSet } } ) ->

	NewExpr = { tuple, LineCall, [ { var, LineCall, 'State' } | Params ] },
	{ [ NewExpr ], Transforms };


% Faulty const_return_result/1 arity:
call_transformer( LineCall, _FunctionRef={ remote, _, {atom,_,wooper},
										   {atom,_,const_return_result} },
				  Params,
				  Transforms=#ast_transforms{
					transformed_function_identifier=FunId } )
  when length( Params ) =/= 1 ->
	wooper_internals:raise_usage_error( "wrong arity (~B) specified "
		"for wooper:const_return_result/1, for request ~s/~B.",
		[ length( Params ) | pair:to_list( FunId ) ], Transforms, LineCall );


% Nature mismatch:
call_transformer( LineCall, _FunctionRef={ remote, _, {atom,_,wooper},
										   {atom,_,const_return_result} },
				  _Params,
				  Transforms=#ast_transforms{
					transformed_function_identifier=FunId,
					transformation_state={ OtherNature, _Qualifiers,
										   _WOOPERExportSet } } ) ->
	wooper_internals:raise_usage_error( "method terminator mismatch "
		"for method ~s/~B: wooper:const_return_result/1 implies "
		"request, whereas was detected as a ~s.",
		pair:to_list( FunId ) ++ [ OtherNature ], Transforms, LineCall );



% Second, oneways:

% Better use the most precise return pseudo-function if this clause is const
% (since returning the initial State):
%
call_transformer( _LineCall, _FunctionRef={ remote, _, {atom,_,wooper},
											{atom,_,return_state} },
				  _Params=[ _StateExpr={ var, Line, 'State'} ],
				  Transforms=#ast_transforms{
					transformed_function_identifier=FunId } ) ->
	wooper_internals:raise_usage_error( "this const clause of oneway "
		"~s/~B shall use wooper:const_return/0 "
		"(instead of wooper:return_state/1).",
		pair:to_list( FunId ), Transforms, Line );


% First (correct, non-const) oneway detection:
call_transformer( _LineCall, _FunctionRef={ remote, _, {atom,_,wooper},
											{atom,_,return_state} },
				  _Params=[ StateExpr ],
				  Transforms=#ast_transforms{
					%transformed_function_identifier=FunId,
					transformation_state={ undefined, _Qualifiers,
										   WOOPERExportSet } } ) ->

	%trace_utils:debug_fmt( "~s/~B detected as a non-const oneway.",
	%					   pair:to_list( FunId ) ),

	% So that wooper:return_state( S ) becomes simply S:
	NewExpr = StateExpr,
	NewTransforms = Transforms#ast_transforms{
					  transformation_state={ oneway, [], WOOPERExportSet } },
	{ [ NewExpr ], NewTransforms };


% Already detected as a oneway, checking qualifiers:
call_transformer( _LineCall, _FunctionRef={ remote, _, {atom,_,wooper},
											{atom,_,return_state} },
				  _Params=[ StateExpr ],
				  Transforms=#ast_transforms{
						transformation_state={ oneway, Qualifiers,
											   WOOPERExportSet } } ) ->

	% 'const' may or may not be still there, and will surely not:
	NewQualifiers = lists:delete( const, Qualifiers ),

	NewExpr = StateExpr,

	NewTransforms = Transforms#ast_transforms{
					  transformation_state={ oneway, NewQualifiers,
											 WOOPERExportSet } },

	{ [ NewExpr ], NewTransforms };


% Faulty return_state/1 arity:
call_transformer( LineCall, _FunctionRef={ remote, _, {atom,_,wooper},
										   {atom,_,return_state} },
				  Params,
				  Transforms=#ast_transforms{
					transformed_function_identifier=FunId } )
  when length( Params ) =/= 1 ->
	wooper_internals:raise_usage_error( "wrong arity (~B) specified "
		"for wooper:return_state/1, for oneway ~s/~B.",
		[ length( Params ) | pair:to_list( FunId ) ], Transforms, LineCall );


% Nature mismatch:
call_transformer( LineCall, _FunctionRef={ remote, _, {atom,_,wooper},
										   {atom,_,return_state} },
				  _Params,
				  Transforms=#ast_transforms{
						transformed_function_identifier=FunId,
						transformation_state={ OtherNature, _Qualifiers,
											   _WOOPERExportSet } } ) ->
	wooper_internals:raise_usage_error( "method terminator mismatch "
		"for method ~s/~B: wooper:return_state/1 implies "
		"oneway, whereas was detected as a ~s.",
		pair:to_list( FunId ) ++ [ OtherNature ], Transforms, LineCall );


% First (correct, a priori const) oneway detection:
call_transformer( LineCall, _FunctionRef={ remote, _, {atom,_,wooper},
										   {atom,_,const_return} },
				  _Params=[],
				  Transforms=#ast_transforms{
						%transformed_function_identifier=FunId,
						transformation_state={ undefined, _,
											   WOOPERExportSet } } ) ->

	%trace_utils:debug_fmt( "~s/~B detected as a const oneway.",
	%					   pair:to_list( FunId ) ),

	% So that wooper:const_return() becomes simply S:
	NewExpr = { var, LineCall, 'State' },
	NewTransforms = Transforms#ast_transforms{
		  transformation_state={ oneway, [ const ], WOOPERExportSet } },
	{ [ NewExpr ], NewTransforms };


% Already detected as a oneway, this clause is const, this will not change
% overall constness status:
%
call_transformer( LineCall, _FunctionRef={ remote, _, {atom,_,wooper},
										   {atom,_,const_return} },
				  _Params=[],
				  Transforms=#ast_transforms{
						transformation_state={ oneway, _Qualifiers,
											   _WOOPERExportSet } } ) ->

	NewExpr = { var, LineCall, 'State' },

	{ [ NewExpr ], Transforms };


% Faulty const_return/0 arity:
call_transformer( LineCall, _FunctionRef={ remote, _, {atom,_,wooper},
										   {atom,_,const_return} },
				  Params,
				  Transforms=#ast_transforms{
								transformed_function_identifier=FunId } )
  when Params =/= [] ->
	wooper_internals:raise_usage_error( "wrong arity (~B) specified "
		"for wooper:const_return/0, for oneway ~s/~B.",
		[ length( Params ) | pair:to_list( FunId ) ], Transforms, LineCall );


% Nature mismatch:
call_transformer( LineCall, _FunctionRef={ remote, _, {atom,_,wooper},
										   {atom,_,const_return} },
				  _Params,
				  Transforms=#ast_transforms{
					transformed_function_identifier=FunId,
					transformation_state={ OtherNature, _Qualifiers,
										   _WOOPERExportSet } } ) ->
	wooper_internals:raise_usage_error( "method terminator mismatch "
		"for method ~s/~B: wooper:const_return/0 implies "
		"oneway, whereas was detected as a ~s.",
		pair:to_list( FunId ) ++ [ OtherNature ], Transforms, LineCall );



% Third, static methods:

% First (correct) static method detection:
call_transformer( _LineCall,
		  _FunctionRef={ remote, _, {atom,_,wooper}, {atom,_,return_static} },
		  _Params=[ ResultExpr ],
		  Transforms=#ast_transforms{
				%transformed_function_identifier=FunId,
				transformation_state={ undefined, _, WOOPERExportSet } } ) ->

	%trace_utils:debug_fmt( "~s/~B detected as a static method.",
	%					   pair:to_list( FunId ) ),

	% So that wooper:return_static( R ) becomes simply R:
	NewExpr = ResultExpr,
	NewTransforms = Transforms#ast_transforms{
					  transformation_state={ static, [], WOOPERExportSet } },
	{ [ NewExpr ], NewTransforms };


% Already detected as a static:
%
% (mostly the same clause as above, as qualifiers do not matter for static):
%
call_transformer( _LineCall,
		  _FunctionRef={ remote, _, {atom,_,wooper}, {atom,_,return_static} },
		  _Params=[ ResultExpr ],
		  Transforms=#ast_transforms{
				%transformed_function_identifier=FunId,
				transformation_state={ static, _Qualifiers,
									   _WOOPERExportSet } } ) ->

	%trace_utils:debug_fmt( "~s/~B confirmed as a static method.",
	%					   pair:to_list( FunId ) ),

	% So that wooper:return_static( R ) becomes simply R:
	NewExpr = ResultExpr,
	NewTransforms = Transforms,
	{ [ NewExpr ], NewTransforms };


% Faulty static arity:
call_transformer( LineCall,
		  _FunctionRef={ remote, _, {atom,_,wooper}, {atom,_,return_static} },
		  Params,
		  Transforms=#ast_transforms{ transformed_function_identifier=FunId } )
  when length( Params ) =/= 2 ->
	wooper_internals:raise_usage_error( "wrong arity (~B) specified "
		"for wooper:return_static/1, for static method ~s/~B.",
		[ length( Params ) | pair:to_list( FunId ) ], Transforms, LineCall );


% Nature mismatch:
call_transformer( LineCall,
		  _FunctionRef={ remote, _, {atom,_,wooper}, {atom,_,return_static} },
		  _Params,
		  Transforms=#ast_transforms{
						transformed_function_identifier=FunId,
						transformation_state={ OtherNature, _Qualifiers,
											   _WOOPERExportSet } } ) ->
	wooper_internals:raise_usage_error( "method terminator mismatch "
		"for method ~s/~B: wooper:return_static/1 implies "
		"static method, whereas was detected as a ~s.",
		pair:to_list( FunId ) ++ [ OtherNature ], Transforms, LineCall );


% The commented clause below cannot be kept, as a plain function may for example
% terminate with a call to wooper:execute_request/4, with is licit and should
% not be interpreted as an invalid method terminator:
%
% Invalid method terminator:
%call_transformer( LineCall, _FunctionRef={ remote, _, {atom,_,wooper},
%										   {atom,_,UnexpectedTerminator} },
%				  _Params,
%				  Transforms=#ast_transforms{
%								transformed_function_identifier=FunId } ) ->
%	wooper_internals:raise_usage_error( "invalid method terminator specified "
%		"for ~s/~B: wooper:~s does not exist (for any arity).",
%		pair:to_list( FunId ) ++ [ UnexpectedTerminator ], Transforms,
%		LineCall );


% So we selectively accept the WOOPER non-terminator functions, and reject the
% others, i.e. the ones that the wooper module does not export:
%
% (the purpose is to intercept any wrong method terminator that would be
% introduced by the user)
%
call_transformer( LineCall, FunctionRef={ remote, _, {atom,_,wooper},
										  {atom,_,FunctionName} },
				  Params,
				  Transforms=#ast_transforms{
						transformed_function_identifier=FunId,
						transformation_state={ _Nature, _Qualifiers,
											   WOOPERExportSet } } ) ->

	CallFunId = { FunctionName, length( Params ) },

	% So this call does not correspond to any known/expected method terminator;
	% let's check whether it is a legit WOOPER call (allowed as last expression
	% of a body) or a WOOPER-unknown one (hence most probably a faulty
	% terminator):
	%
	case set_utils:member( CallFunId, WOOPERExportSet ) of

		true ->
			% OK, terminating a body with a call to a function of the wooper
			% module is allowed (provided it is not a method but a plain
			% function), so we let this call pass through:

			%trace_utils:debug_fmt( "~s/~B detected as a plain function;~n"
			%	" - function ref is:~n~p~n - transform is:~n~p~n",
			%	pair:to_list( FunId ) ++ [ FunctionRef, Transforms ] ),

			SameExpr = { call, LineCall, FunctionRef, Params },
			{ [ SameExpr ], Transforms };

		false ->

			%trace_utils:debug_fmt( "Known functions exported by the wooper "
			%					   "module: ~s",
			%					   [ set_utils:to_string( WOOPERExportSet ) ] ),

			%trace_utils:debug_fmt( "Known functions exported by the wooper "
			%					   "module:~n  ~s",
			%					   [ table:toString( WOOPERExportSet ) ] ),

			wooper_internals:raise_usage_error( "invalid method terminator "
			  "specified for ~s/~B: wooper:~s/~B is neither a known terminator "
			  "nor an exported function.",
			  pair:to_list( FunId ) ++ pair:to_list( CallFunId ),
			  Transforms, LineCall )

	end;


% Finally, of course calls unrelated to WOOPER shall go through as well:
call_transformer( LineCall, FunctionRef, Params,
				  Transforms ) ->
				  %Transforms=#ast_transforms{
						%transformed_function_identifier=FunId,
						%transformation_state={ Nature, Qualifiers,
						%					   _WOOPERExportSet } } ) ->

	%trace_utils:debug_fmt( "Deducing that ~s/~B is a plain function "
	%					   "(nature: ~p, qualifiers: ~p)",
	%					   pair:to_list( FunId ) ++ [ Nature, Qualifiers ] ),

	SameExpr = { call, LineCall, FunctionRef, Params },
	{ [ SameExpr ], Transforms }.


% To help debugging any non-match:
%call_transformer( _LineCall, _FunctionRef, _Params, Transforms ) ->
%
%	trace_utils:debug_fmt( "Unexpected transforms:~n  ~s",
%				   [ ast_transform:ast_transforms_to_string( Transforms ) ] ),
%
%	throw( { unexpected_transforms, Transforms } ).



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

ensure_exported_at( FunInfo=#function_info{ name=Name,
											arity=Arity }, _ExportLoc ) ->

	wooper_internals:notify_warning( [ text_utils:format(
		"~s/~B should not be explicitly exported, since methods "
		"are automatically exported.", [ Name, Arity ] ) ] ),

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
