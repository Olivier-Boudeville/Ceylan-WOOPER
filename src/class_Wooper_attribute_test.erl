% 
% Copyright (C) 2003-2013 Olivier Boudeville
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


-module(class_Wooper_attribute_test).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[]).


% Parameters taken by the constructor ('construct'). 
% There are no class-specific data needing to be set in the constructor here,
% thus wooper_construct_parameters is not defined (interesting test as such).


% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/0, new_link/0, 
	synchronous_new/0, synchronous_new_link/0,
	synchronous_timed_new/0, synchronous_timed_new_link/0,
	remote_new/1, remote_new_link/1, remote_synchronous_new/1,
	remote_synchronous_new_link/1, remote_synchronous_timed_new/1,
	remote_synchronous_timed_new_link/1, construct/1 ).



% Method declarations.
-define(wooper_method_export, test/1 ).



% Static method declarations.
-export([ run/0, crashing_examples/1 ]).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").

-define(Prefix,"--> ").



% Constructs a new test instance.
construct(State) ->
	% Class-specific attributes:
	setAttribute(State,test_attribute,true).



% Oneway test.
test(State) ->
	io:format( "   Testing attribute management.~n" ),
	
	true        = hasAttribute(State,test_attribute),
	false       = hasAttribute(State,non_existing),
	
	true        = ?getAttr(test_attribute),
	
	UnsetState  = removeAttribute(State,test_attribute),
	false       = hasAttribute(UnsetState,test_attribute),
	
	NewSetState = setAttribute(UnsetState,test_attribute,true),
	true        = getAttribute(NewSetState,test_attribute),
	
	MultiState  = setAttributes(NewSetState,[
		{test_attribute,false}, {another_attribute,42} ]),
	false       = getAttribute(MultiState,test_attribute),
	42          = getAttribute(MultiState,another_attribute),
	
	RevertState = toggleAttribute(MultiState,test_attribute),
	true        = getAttribute(RevertState,test_attribute),
	
	VoidState   = setAttribute(RevertState,test_list,[]),
	AppendState = appendToAttribute(VoidState,test_list, 7),
	AgainState  = appendToAttribute(AppendState,test_list, 8),
	[8,7]       = getAttribute(AgainState,test_list),
	
	DeleteState = deleteFromAttribute(AgainState,test_list,7),
	[8]         = getAttribute(DeleteState,test_list),
	
	PreAddState = setAttribute(DeleteState,test_add,1),
	AddState    = addToAttribute(PreAddState,test_add,10),
	11          = getAttribute(AddState,test_add),
	
	SubState    = subtractFromAttribute(AddState,test_add,5),
	6           = getAttribute(SubState,test_add),

	{PoppedState,8} = popFromAttribute(AgainState,test_list),
	{_,7}           = popFromAttribute(PoppedState,test_list),
	
	UndefState  = setAttribute(PoppedState,test_undef,undefined),
	%UndefState  = setAttribute(PoppedState,test_undef,not_undefined),
	
	not_crashing_examples(UndefState),
	%crashing_examples(UndefState),
	
	io:format( "   Successful ending of attribute management test.~n" ),	
	?wooper_return_state_result( SubState, test_ok ).



not_crashing_examples(State) ->
	NewState = removeAttribute(State,non_existing),
	OtherNewState = appendToAttribute(NewState,test_attribute,8),
	[8|true] = getAttribute(OtherNewState,test_attribute),
	not_crashing_test_undefined(State),
	not_crashing_test_hashtable(State),
	?wooper_return_state_result( OtherNewState, test_ok ).



% Usually operations are commented-out as we do not want to fail on purpose:
crashing_examples(State) ->
	%toggleAttribute(State,non_existing),
	% Not a boolean:
	%toggleAttribute(State,test_add),
	
	%addToAttribute(State,non_existing,4),
	% Not a number:
	%addToAttribute(State,test_attribute,4),
	
	%subtractFromAttribute(State,non_existing,4),
	% Not a number:
	%subtractFromAttribute(State,test_attribute,4),

	% Not a list:
	%deleteFromAttribute(State,test_attribute,7),
	
	crashing_test_undefined(State),

	%?getAttr(non_existing),
	?wooper_return_state_result( State, test_ok ).
	
	

% Function needed as the checkUndefined macro operates on 'State':
not_crashing_test_undefined(State) ->
	?checkUndefined(test_undef).
	
	
	
% Function needed as the checkUndefined macro operates on 'State':
crashing_test_undefined(State) ->
	?checkUndefined(unexisting_attribute).
	


not_crashing_test_hashtable(State) ->
	% Let's have an (empty) hashtable first:
	WithTableState = setAttribute( State, test_hashtable, hashtable:new() ),
	EntrySetState = addKeyValueToAttribute(WithTableState,test_hashtable,
		my_key, my_value ),
	% Check was registered indeed:
	ReadTable = getAttribute( EntrySetState, test_hashtable ),
	{value,my_value} = hashtable:lookupEntry( my_key, ReadTable ).
	
	
	
% Actual test.
% (static)
run() ->
	Tested = class_Wooper_attribute_test:new_link(),
	Tested ! {test,[],self()},
	receive
	
		{wooper_result,test_ok} ->
			io:format( ?Prefix "Test success.~n" ),
			erlang:halt();
			
		Other ->
			io:format( ?Prefix "Test failed: ~w.~n", [Other] ),
			throw(test_failed)
			
	end.
	
