% Copyright (C) 2018-2018 Olivier Boudeville
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
% Creation date: Friday, April 13, 2018
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]



% Centralisation of class-level information.
%
-module(wooper_info).


% For the attribute_info record:
-include("wooper_info.hrl").


% Shorthands:
-type attribute_name() :: wooper:attribute_name().


-type attribute_info() :: #attribute_info{}.


% Stores all class-level information (i.e. metadata) regarding attributes.
%
-type attribute_table() :: table:table( attribute_name(), attribute_info() ).


-export_type([ attribute_info/0, attribute_table/0 ]).