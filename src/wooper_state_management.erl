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


% Centralizes, on behalf of the WOOPER parse transform, the support for the
% state management, including instance attributes.
%
-module(wooper_state_management).


-export([ manage_attributes/1 ]).


% For the class_info record:
-include("wooper_info.hrl").



% Shorthands:

%-type form_element() :: ast_base:form_element().

-type class_info() :: wooper_info:class_info().



% Processes the class-specific attributes.
%
-spec manage_attributes( class_info() ) -> class_info().
manage_attributes( ClassInfo=#class_info{ parse_attributes=ParseAttrTable } ) ->

	% Has '-attributes(...).' been specified?
	case table:hasEntry( attributes, ParseAttrTable ) of

		true ->
			{ AttrDeclarations, ShrunkParseAttrTable } =
				table:extractEntry( attributes, ParseAttrTable ),

			trace_utils:debug_fmt( "Attribute declaration:~n~p",
								   [ AttrDeclarations ] ),


			ClassInfo#class_info{ parse_attributes=ShrunkParseAttrTable };


		false ->

			trace_utils:warning( "No attribute declaration found." ),
			ClassInfo

	end.
