% Copyright (C) 2018-2019 Olivier Boudeville
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
% Creation date: Friday, April 13, 2018
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]



% Gathering of internal helpers.
%
-module(wooper_internals).


-export([ raise_error/1, raise_error/2, raise_error/3, raise_error/4,
		  notify_warning/1, notify_warning/2 ]).


% For the ast_transforms record:
-include("ast_transform.hrl").



% To better report errors:
-define( origin_layer, "WOOPER" ).


% Raises a (compile-time, rather ad hoc) error when applying this parse
% transform, to stop the build on failure and report the actual error.
%
-spec raise_error( term() ) -> no_return().
raise_error( ErrorTerm ) ->
	raise_error( ErrorTerm, _Context=undefined ).



% Raises a (compile-time, rather ad hoc) error, with specified source context,
% when applying this parse transform, to stop the build on failure and report
% the actual error.
%
-spec raise_error( term(), ast_base:source_context() ) -> no_return().
raise_error( ErrorTerm, Context ) ->
	ast_utils:raise_error( ErrorTerm, Context, ?origin_layer ).



% Raises a (compile-time, rather ad hoc) error, with specified source context,
% when applying this parse transform, to stop the build on failure and report
% the actual error.
%
-spec raise_error( text_utils:ustring(), ast_transform:ast_transforms(),
				   ast_base:source_context(), ast_base:line() ) -> no_return().
raise_error( ErrorString, #ast_transforms{ transformed_module_name=ModName },
			 Line ) ->

	ExpectedModFile = text_utils:format( "~s.erl", [ ModName ] ),

	% Finally not used, as we do not need here to specify the layer or to print
	% a stacktrace:
	%
	%ast_utils:raise_error( ErrorString, _Context={ ExpectedModFile, Line },
	%					   ?origin_layer ).
	io:format( "~s:~B: ~s~n",
			   [ ExpectedModFile, Line, ErrorString ] ),

	% Almost the only way to stop the processing of the AST:
	halt( 6 ).



% Raises a (compile-time, rather ad hoc) error, with specified source context,
% when applying this parse transform, to stop the build on failure and report
% the actual error.
%
-spec raise_error( text_utils:format_string(), text_utils:format_values(),
				   ast_transforms(),
				   ast_base:source_context(), ast_base:line() ) -> no_return().
raise_error( ErrorFormatString, ErrorValues, Transforms, Line ) ->
	ErrorString = text_utils:format( ErrorFormatString, ErrorValues ),
	raise_error( ErrorString, Transforms, Line ).



% Notifies a (compile-time, rather ad hoc) warning, with no specific context,
% when applying this parse transform.
%
% Does not stop the build.
%
-spec notify_warning( [ term() ] ) -> basic_utils:void().
notify_warning( Elements ) ->
	notify_warning( Elements, _Context=undefined ).



% Notifies a (compile-time, rather ad hoc) warning, with specified context, when
% applying this parse transform.
%
% Does not stop the build.
%
-spec notify_warning( [ term() ], ast_base:form_context() ) ->
							basic_utils:void().
notify_warning( Elements, Context ) ->
	ast_utils:notify_warning( Elements, Context ).
