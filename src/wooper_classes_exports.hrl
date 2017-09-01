% Copyright (C) 2003-2017 Olivier Boudeville
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


% Modular WOOPER header gathering the class-related primitives (exports).


% These methods/functions are defined for all classes:
%
-export([
		 getClassName/1, get_superclasses/0, getSuperclasses/1
		]).



-ifdef(wooper_debug).

-export([ wooper_get_instance_description/1 ]).

-endif. % wooper_debug



% Shared code.
%
% All WOOPER classes should mention their superclasses and their WOOPER exports
% before the WOOPER header is included.

% Example:
% -module(class_Cat).
%
% -define( wooper_superclasses, [class_Mammal,class_ViviparousBeing] ).

% -define( wooper_public_method_export, hasWhiskers/1, canEat/2 ).
% -define( wooper_protected_method_export, canEat/2 ).
% -define( wooper_private_method_export, doSomething/4 ).
% -define( wooper_static_method_export, foo_bar/1 ).
%
% -define( wooper_construct_parameters, Age, Gender, FurColor ).
% -define( wooper_construct_export, new/3, new_link/3, construct/4, ... ).
% -include("wooper.hrl").
% [...]
% See also: class_Template.erl


% Note that, in this version of WOOPER, the method qualifiers (ex: regarding
% access rights, i.e. public, protected or private) are mere documentation
% elements (they are not checked nor enforced).


% wooper_method_export has been deprecated in favor of:
%
% - wooper_public_method_export
% - wooper_protected_method_export
% - wooper_private_method_export
% - wooper_static_method_export
%
% (could have been wooper_member_method_export)


-ifdef(wooper_method_export).
-export([ ?wooper_method_export ]).
-endif.

-ifdef(wooper_member_method_export).
-export([ ?wooper_member_method_export ]).
-endif.

-ifdef(wooper_public_method_export).
-export([ ?wooper_public_method_export ]).
-endif.


-ifdef(wooper_protected_method_export).
-export([ ?wooper_protected_method_export ]).
-endif.


-ifdef(wooper_private_method_export).
-export([ ?wooper_private_method_export ]).
-endif.

-ifdef(wooper_static_method_export).
-export([ ?wooper_static_method_export ]).
-endif.



% Must be defined, but an error message at their call should be clearer:
-ifdef(wooper_construct_export).
-export([ ?wooper_construct_export ]).
-endif.
