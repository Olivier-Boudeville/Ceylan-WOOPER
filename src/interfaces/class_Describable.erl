% Copyright (C) 2008-2022 Olivier Boudeville
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
% Creation date: 2008.


% @doc Interface class implementing the Describable trait, so that instances
% supporting that trait are able to output a static (prebuilt) <b>textual
% description</b> of them.
%
% A class may implement this interface either through inheritance (if all
% instances of that class shall always have that trait) or through composition;
% in this last case, the interface class is not meant to be declared among the
% superclasses, but among the composed interfaces, and it is instead to be
% constructed and destructed explicitly by each instance deciding to support the
% corresponding trait.
%
% This interface provides also exported functions designed so that they can be
% applied to any WOOPER instance, whether or not it supports this trait.
%
% We recommend that all describable classe define their own
% to_string( wooper:state() ) -> ustring() helper function.
%
-module(class_Describable).


-define( class_description,
		 "Interface to be implemented by all instances able to output "
		 "their static textual description." ).


% No superclasses or composed interfaces.


% Declaration of the interface-specific attributes:
%
% (as it is a WOOPER builtin, they are all prefixed with 'wooper' and then the
% interface name)
%
-define( class_attributes, [

	{ wooper_describable_description, description(),
	  "description held by this instance" }

						   ] ).


-type user_description() :: ustring().
% A user-provided description of interest.

-type description() :: bin_string().
% The internal description of interest.

-type any_description() :: any_string().
% A description of interest, as any string.


-type describable_pid() :: pid().
% The PID of an instance implementing the describable interface.


-export_type([ user_description/0, description/0, any_description/0,
			   describable_pid/0 ]).


% Exported helper functions that can be applied to any WOOPER state:
-export([ is_describable/1, get_maybe_description/1, to_maybe_string/1 ]).


% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").



% Shorthands:

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().
-type any_string() :: text_utils:any_string().




% @doc Constructs a describable instance, based on the specified description.
-spec construct( wooper:state(), any_description() ) -> wooper:state().
construct( State, Description ) ->
	setAttribute( State, wooper_describable_description,
				  text_utils:ensure_binary( Description ) ).


% No destructor.



% Methods section.


% @doc Returns the description of this Describable.
-spec getDescription( wooper:state() ) -> const_request_return( description() ).
getDescription( State ) ->

	% Here the description may be dynamically updated, typically by calling a
	% to_string( wooper:state() ) -> ustring() helper function whose result
	% would be stored in the wooper_describable_description attribute and
	% returned by this method.

	% Another option is to return a static (class-level, not instance-level)
	% description.

	% Always defined by design:
	wooper:const_return_result( ?getAttr(wooper_describable_description) ).



% @doc Sets the description of this Describable.
-spec setDescription( wooper:state(), user_description() ) -> oneway_return().
setDescription( State, NewUserDescription ) ->
	NewBinDesc = text_utils:ensure_binary( NewUserDescription ),
	wooper:return_state(
		setAttribute( State, wooper_describable_description, NewBinDesc ) ).




% Section for helper functions (not methods).

% The following helper functions can be used in the context of any class,
% whether or not it implements this Describable interface.



% @doc Tells whether the corresponding instance implements the Describable
% interface.
%
% (exported helper)
%
-spec is_describable( wooper:state() ) -> boolean().
is_describable( State ) ->
	hasAttribute( State, wooper_describable_description ).



% @doc Returns any description available for the corresponding instance.
%
% This function is designed to apply to any WOOPER instance, whether it is a
% Describable one or not. This allows for more flexibility, and to rely on
% composition over inheritance if wanted.
%
% (exported helper)
%
-spec get_maybe_description( wooper:state() ) -> maybe( description() ).
get_maybe_description( State ) ->
	% Allowed, as the type of this attribute does not include the 'undefined'
	% atom:
	%
	?getMaybeAttr(wooper_describable_description).



% @doc Returns a textual element of description of the corresponding instance,
% should it implement the Describable interface.
%
% (exported helper)
%
-spec to_maybe_string( wooper:state() ) -> maybe( ustring() ).
to_maybe_string( State ) ->
	case ?getMaybeAttr(wooper_describable_description) of

		undefined ->
			undefined;

		Desc ->
			text_utils:format( "whose description is: '~ts'", [ Desc ] )

	end.
