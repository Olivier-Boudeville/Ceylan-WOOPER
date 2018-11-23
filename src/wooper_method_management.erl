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


-export([ manage_methods/1 ]).


% For the function_info record:
-include("ast_info.hrl").

% For the class_info record:
-include("wooper_info.hrl").


% The (WOOPER-level) nature of a given Erlang function.
%
% Note that 'function' is a default; a later analysis may class a function into
% a more precise category (ex: if the actual returns are done only in helper
% functions).
%
-type function_nature() :: 'function' | 'request' | 'oneway' | 'static'.


% Shorthands:

%-type function_info() :: ast_info:function_info().
-type compose_pair() :: wooper_parse_transform:compose_pair().



% Extracts the methods found in the specified function table, transforms them,
% and interprets that information to update the specified class information.
%
% Returns an updated pair thereof.
%
-spec manage_methods( compose_pair() ) -> compose_pair().
manage_methods( { FunctionTable,
				  ClassInfo=#class_info{ requests=RequestTable,
										 oneways=OnewayTable,
										 statics=StaticTable } } ) ->

	AllFunctions = table:values( FunctionTable ),

	% We do not want to analyse any WOOPER builtin function, as they will not
	% teach us anything about the class at hand, and are expected to be already
	% in a final form:
	%
	Builtins = wooper_info:get_wooper_builtins(),


	{ NewHelperFunTable, NewRequestTable, NewOnewayTable, NewStaticTable } =
		sort_out_functions( AllFunctions, _FunctionTable=table:new(),
							RequestTable, OnewayTable, StaticTable,
							Builtins ),

	{ NewHelperFunTable, ClassInfo#class_info{ requests=NewRequestTable,
											   oneways=NewOnewayTable,
											   statics=NewStaticTable } }.



% Transforms and categorises each of the specified functions according to its
% real nature (ex: a given Erlang function may actually be a WOOPER oneway).
%
sort_out_functions( _Functions=[], FunctionTable, RequestTable, OnewayTable,
					StaticTable, _Builtins ) ->
	{ FunctionTable, RequestTable, OnewayTable, StaticTable };

% Each function supposed to have at least one clause:
sort_out_functions( _Functions=[
			F=#function_info{
				 name=FunName,
				 arity=Arity,
				 clauses=AllClauses=[ C | _OtherClauses ] } | T ],
			FunctionTable, RequestTable, OnewayTable, StaticTable, Builtins ) ->

	FunId = { FunName, Arity },

	case lists:member( FunId, Builtins ) of

		true ->
			% Skip any builtin, we currently just consider it as a function:
			NewFunctionTable = table:addNewEntry( FunId, F, FunctionTable ),
			sort_out_functions( T, NewFunctionTable, RequestTable, OnewayTable,
								StaticTable, Builtins );

		false ->

			trace_utils:debug_fmt( "Examining function ~s/~B",
								   [ FunName, Arity ] ),

			% Use the first clause to guess:
			FunNature = infer_function_nature_from( C ),

			% Then check and transform all clauses, for any given nature:
			NewClauses = transform_method_returns( AllClauses, FunNature ),

			NewFunInfo = F#function_info{ clauses=NewClauses },

			% Stores the result in the right category and recurses:
			case FunNature of

				function ->
					NewFunctionTable = table:addNewEntry( FunId, NewFunInfo,
														  FunctionTable ),
					sort_out_functions( T, NewFunctionTable, RequestTable,
										OnewayTable, StaticTable, Builtins );

				request ->
					NewRequestTable = table:addNewEntry( FunId, NewFunInfo,
														 RequestTable ),
					sort_out_functions( T, FunctionTable, NewRequestTable,
										OnewayTable, StaticTable, Builtins );

				oneway ->
					NewOnewayTable = table:addNewEntry( FunId, NewFunInfo,
														OnewayTable ),
					sort_out_functions( T, FunctionTable, RequestTable,
										NewOnewayTable, StaticTable, Builtins );

				static ->
					NewStaticTable = table:addNewEntry( FunId, NewFunInfo,
														StaticTable ),
					sort_out_functions( T, FunctionTable, RequestTable,
										OnewayTable, NewStaticTable, Builtins )

			end

	end;

sort_out_functions( _Functions=[ #function_info{ name=FunName,
												 arity=Arity } | _T ],
					_FunctionTable, _RequestTable, _OnewayTable, _StaticTable,
					_Builtins ) ->

	trace_utils:error_fmt( "No clause found for ~s/~B; function exported "
						   "yet not defined?", [ FunName, Arity ] ),

	ast_utils:raise_error( { clauseless_function, { FunName, Arity } } ).





% Infers the nature of the corresponding function, based on its specified
% clause.
%
-spec infer_function_nature_from( meta_utils:clause_def() ) ->
										function_nature().
infer_function_nature_from( Clause ) ->

	trace_utils:debug_fmt( " - examining nature of clause ~p", [ Clause ] ),

	AllLeaves = get_call_leaves_for_clause( Clause ),

	trace_utils:debug_fmt( " - call leaves found: ~p", [ AllLeaves ] ),

	case list_utils:uniquify( AllLeaves ) of

		% At least one occurrence of, and no other, different element than:
		[ { wooper, return_state_result, 2 } ] ->
			request;

		[ E={ wooper, return_state_result, _Incorrect } ] ->
			ast_utils:raise_error( { faulty_request_return, E } );

		[ { wooper, return_state_only, 1 } ] ->
			oneway;

		[ E={ wooper, return_state_only, _Incorrect } ] ->
			ast_utils:raise_error( { faulty_oneway_return, E } );


		[ { wooper, return_static, 1 } ] ->
			static;

		[ E={ wooper, return_static, _Incorrect } ] ->
			ast_utils:raise_error( { faulty_static_return, E } );

		_ ->
			function

	end.



% Transforms specified clauses according to their specified nature.
transform_method_returns( Clauses, FunNature ) ->
	[ transform_method_returns_in( C, FunNature ) || C <- Clauses ].



% Transforms specified clause according to its specified nature.
transform_method_returns_in( _ClauseForm={ clause, Line, Patterns, Guards,
										   Body }, FunNature ) ->

	%trace_utils:debug_fmt( " - ignoring patterns ~p", [ Patterns ] ),
	%trace_utils:debug_fmt( " - ignoring guards ~p", [ Guards ] ),

	trace_utils:debug_fmt( " - transforming, as '~s', following body:~n~p",
						   [ FunNature, Body ] ),

	NewBody = transform_body( Body, FunNature ),

	{ clause, Line, Patterns, Guards, NewBody }.



% Transforms specified body of specified clause, according to the function
% nature.
%
%transform_body( Body, _FunNature=function ) ->
transform_body( Body, _FunNature ) ->

	% Traverses recursively the body to obtain all leaves:
	%Leaves = get_all_leaves( Body ),

	%trace_utils:debug_fmt( "Leaves are: ~s",
	%					   [ text_utils:terms_to_string( Leaves ) ] ),

	Body.



% Returns (recursively) all leaves (terminal expressions) of specified function
% clause that are remote calls, as {ModuleName,FunctionName,Arity}.
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




% Handles each possible branching construct of the expression.
%
% (anonymous mute variables correspond to lines)
%
get_call_leaves_for_expression(
  _Expr={ 'case', _Line, _Patterns, _Guards, Body }, Acc ) ->
	get_call_leaves_for_clause( Body ) ++ Acc;

get_call_leaves_for_expression(
  _Expr={ 'catch', _Line, _Patterns, _Guards, Body }, Acc ) ->
	get_call_leaves_for_clause( Body ) ++ Acc;

% Intercept calls to WOOPER method terminators:
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

get_call_leaves_for_expression( Expr, _Acc ) ->
	ast_utils:raise_error( { unhandled_expression, Expr } ).



% Intercept WOOPER calls:
%
% (anonymous mute variables are line numbers)
%
%% get_call_leaves_for_clause( _Clause={ 'call', _Line, {remote,_,{atom,_,Module},{atom,_,Function}}, SubClause },
%%				Acc ) ->
%%	get_call_leaves_for_clause( SubClause ) ++ Acc;

%% get_call_leaves_for_clause( _Clause=E, _Acc ) ->
%%	throw( { unsupported_clause_element, E } ).



% Transforms the method returns:
%
% * for requests: wooper:return_state_result( S, R ) shall become { S, R }
% * for oneways:  wooper:return_state( S ) shall become S
% * for static methods: wooper:return_result( R ) shall be become R
%-spec transform_method_returns( class_info() ) -> class_info().
%transform_method_returns( #class_info{

%							} ) ->
