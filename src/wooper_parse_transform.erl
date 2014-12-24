% Copyright (C) 2014 Olivier Boudeville
%
% This file is part of the WOOPER library.
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
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)
% Creation date: Wednesday, December 24, 2014




% Overall parse transform for the WOOPER layer.
%
%
-module(wooper_parse_transform).



% Implementation notes:
%
% Calls in turn the common parse transform, once WOOPER-level operations have
% been done.


-export([ parse_transform/2 ]).



% The parse transform itself, transforming the specified Abstract Format code
% into another one.
%
parse_transform( AST, Options ) ->

	%io:format( "  (applying parse transform '~p')~n", [ ?MODULE ] ),

	% We will be replacing here all

	%io:format( "Input AST:~n~p~n", [ AST ] ),

	%OutputAST = AST,
	OutputAST = common_parse_transform:parse_transform( AST, Options ),
%replace_table( AST ),


	%io:format( "~n~nOutput AST:~n~p~n", [ OutputAST ] ),

	OutputAST.
