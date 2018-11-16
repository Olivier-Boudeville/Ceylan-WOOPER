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


% The (WOOPER-level) nature of a given Erlang function:
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

	% We do not want to analyse any WOOPER builtin, as they will not teach us
	% anything and are expected to be already in a final form:
	%
	Builtins = wooper_info:get_wooper_builtins(),

	AllFunctions = table:values( FunctionTable ),

	{ NewHelperFunTable, NewRequestTable, NewOnewayTable, NewStaticTable } =
		sort_out_functions( AllFunctions, _FunctionTable=table:new(),
							RequestTable, OnewayTable, StaticTable,
							Builtins ),

	{ NewHelperFunTable, ClassInfo#class_info{ requests=NewRequestTable,
											   oneways=NewOnewayTable,
											   statics=NewStaticTable } }.



% Transforms and categorises each function according to its real nature (ex:
% a given Erlang function may actually be a WOOPER oneway).
%
sort_out_functions( _Functions=[], FunctionTable, RequestTable, OnewayTable,
					StaticTable, _Builtins ) ->
	{ FunctionTable, RequestTable, OnewayTable, StaticTable };

% Each function supposed to have at least one clause:
sort_out_functions( _Functions=[
			F=#function_info{
				 name=FunName,
				 arity=Arity,
				 clauses=Clauses=[ C | _OtherClauses ] } | T ],
			FunctionTable, RequestTable, OnewayTable, StaticTable, Builtins ) ->

	FunId = { FunName, Arity },

	case lists:member( FunId, Builtins ) of

		true ->
			% Skip any builtin, we currently just consider it as a function:
			NewFunctionTable = table:addNewEntry( FunId, F, FunctionTable ),
			sort_out_functions( T, NewFunctionTable, RequestTable, OnewayTable,
								StaticTable, Builtins );

		false ->

			%trace_utils:debug_fmt( "Examining function ~s/~B",
			%                       [ FunName, Arity ] ),

			% Use the first clause to guess:
			FunNature = infer_function_nature_from( C ),

			% Then check and transform all clauses, for all natures:
			NewClauses = transform_method_returns( Clauses, FunNature ),

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

	end.



% Infers the nature of the corresponding function, based on its specified
% clause.
%
-spec infer_function_nature_from( meta_utils:clause_def() ) ->
										function_nature().
infer_function_nature_from( _Clause ) ->
	%trace_utils:debug_fmt( " - examining clause ~p", [ Clause ] ),
	function.


% Transforms specified clauses according to their specified nature.
transform_method_returns( Clauses, FunNature ) ->
	[ transform_method_returns_in( C, FunNature ) || C <- Clauses ].



% Transforms specified clause according to its specified nature.
transform_method_returns_in( _ClauseForm={ clause, Line, Patterns, Guards,
										   Body }, FunNature ) ->

	%trace_utils:debug_fmt( " - ignoring patterns ~p", [ Patterns ] ),
	%trace_utils:debug_fmt( " - ignoring guards ~p", [ Guards ] ),

	%trace_utils:debug_fmt( " - transforming, as '~s', following body:~n~p",
	%					   [ FunNature, Body ] ),

	NewBody = transform_body( Body, FunNature ),

	{ clause, Line, Patterns, Guards, NewBody }.



% Transforms specified body of specified clause, according to the function
% nature.
%
transform_body( Body, _FunNature=function ) ->
	Body.




% Transforms the method returns:
%
% * for requests: wooper:return_state_result( S, R ) shall become { S, R }
% * for oneways:  wooper:return_state( S ) shall become S
% * for static methods: wooper:return_result( R ) shall be become R
%-spec transform_method_returns( class_info() ) -> class_info().
%transform_method_returns( #class_info{

%							} ) ->
