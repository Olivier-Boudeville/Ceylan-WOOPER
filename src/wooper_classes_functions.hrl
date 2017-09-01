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


% Modular WOOPER header gathering the class-related primitives (function
% definitions).


% Request returning the classname of the instance.
%
% Always accurate, in all constructors, methods and destructors.
%
% (const request)
%
-spec getClassName( wooper:state() ) -> request_return( class_name() ).
getClassName( State ) ->
	?wooper_return_state_result( State, State#state_holder.actual_class ).



% "Static method" (only a function) which returns the list of the superclasses
% for that class.
%
% Not to be called by the user, see get_superclasses/1 instead.
%
-spec get_superclasses() -> [ class_name() ].
get_superclasses() ->
	?wooper_superclasses.



% Method that returns the (direct) superclasses of the instance.
%
% Always accurate, in all constructors, methods and destructors.
%
% (const request)
%
-spec getSuperclasses( wooper:state() ) -> request_return( [ class_name() ] ).
getSuperclasses( State ) ->
	ActualModule = State#state_holder.actual_class,
	SuperClasses = apply( ActualModule, get_superclasses, [] ),
	%?wooper_return_state_result( State, ?wooper_superclasses ).
	?wooper_return_state_result( State, SuperClasses ).



-ifdef(wooper_debug).


% Returns a full textual description of this instance, including its state and
% virtual table.
%
% This is a method for debug purpose, only activated if wooper_debug is defined.
%
% (const request)
%
wooper_get_instance_description( State ) ->
	?wooper_return_state_result( State, wooper:instance_to_string( State ) ).


-endif. % wooper_debug
