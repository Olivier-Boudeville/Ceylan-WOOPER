% Copyright (C) 2003-2018 Olivier Boudeville
%
% This file is part of the Ceylan-WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]


% Basic testing of WOOPER attribute management.
%
-module(class_AttributeTester).


-include("test_facilities.hrl").


% Determines what are the mother classes of this class (if any):
-superclasses([]).



% Static method declarations.
-export([ crashing_examples/1 ]).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


-attributes([ test_attribute ]).


% Constructs a new test instance.
-spec construct( wooper:state() ) -> wooper:state().
construct( State ) ->

	% Class-specific attributes:
	setAttribute( State, test_attribute, true ).


% Request test.
%
-spec test( wooper:state() ) -> request_return( 'test_ok' ).
test( State ) ->

	test_facilities:display( "Testing attribute management." ),

	true        = hasAttribute( State, test_attribute ),
	false       = hasAttribute( State, non_existing),

	true        = ?getAttr( test_attribute ),

	UnsetState  = removeAttribute( State, test_attribute ),
	false       = hasAttribute( UnsetState, test_attribute ),

	NewSetState = setAttribute( UnsetState, test_attribute, true ),
	true        = getAttribute( NewSetState, test_attribute ),

	MultiState  = setAttributes( NewSetState,[
		{ test_attribute, false }, { another_attribute, 42 } ] ),
	false       = getAttribute( MultiState, test_attribute ),
	42          = getAttribute( MultiState, another_attribute ),


	RevertState = toggleAttribute(MultiState, test_attribute ),
	true        = getAttribute( RevertState, test_attribute ),

	VoidState   = setAttribute( RevertState, test_list,[]),
	AppendState = appendToAttribute(VoidState, test_list, 7),
	AgainState  = appendToAttribute( AppendState, test_list, 8),
	[8,7]       = getAttribute( AgainState, test_list),

	DeleteState = deleteFromAttribute( AgainState, test_list,7),
	[8]         = getAttribute( DeleteState, test_list),

	PreAddState = setAttribute( DeleteState, test_add,1),
	AddState    = addToAttribute( PreAddState, test_add,10),
	11          = getAttribute( AddState, test_add),

	SubState    = subtractFromAttribute( AddState, test_add,5),
	6           = getAttribute( SubState, test_add),

	{ PoppedState, 8 } = popFromAttribute( AgainState, test_list),
	{ _, 7 }           = popFromAttribute( PoppedState, test_list),

	UndefState = setAttribute( PoppedState, test_undef, undefined),
	%UndefState = setAttribute( PoppedState, test_undef, not_undefined),

	not_crashing_examples( UndefState ),
	%crashing_examples( UndefState ),

	test_facilities:display(
							"Successful ending of attribute management test." ),
	wooper:return_state_result( SubState, test_ok ).



-spec not_crashing_examples( wooper:state() ) -> request_return( test_ok ).
not_crashing_examples( State ) ->

	NewState = removeAttribute( State, non_existing ),

	OtherNewState = appendToAttribute( NewState, test_attribute, 8 ),

	[ 8 | true ] = getAttribute( OtherNewState, test_attribute ),
	not_crashing_test_undefined( State ),
	not_crashing_test_hashtable( State ),

	wooper:return_state_result( OtherNewState, test_ok ).



% Usually operations are commented-out as we do not want to fail on purpose:
-spec crashing_examples( wooper:state() ) -> request_return( test_ok ).
crashing_examples( State ) ->

	%toggleAttribute( State, non_existing ),
	% Not a boolean:
	%toggleAttribute( State, test_add ),

	%addToAttribute( State, non_existing, 4 ),
	% Not a number:
	%addToAttribute( State, test_attribute, 4 ),

	%subtractFromAttribute( State, non_existing, 4 ),
	% Not a number:
	%subtractFromAttribute( State, test_attribute, 4 ),

	% Not a list:
	%deleteFromAttribute( State, test_attribute, 7 ),

	crashing_test_undefined( State ),

	%?getAttr(non_existing),
	wooper:return_state_result( State, test_ok ).



% Function needed as the checkUndefined macro operates on 'State':
not_crashing_test_undefined( State ) ->
	?checkUndefined( test_undef ).



% Function needed as the checkUndefined macro operates on 'State':
crashing_test_undefined( State ) ->
	?checkUndefined( unexisting_attribute ).



-spec not_crashing_test_hashtable( wooper:state() ) -> 'test_ok'.
not_crashing_test_hashtable( State ) ->

	% Let's have an (empty) hashtable first:
	WithTableState = setAttribute( State, test_hashtable,
								   ?wooper_table_type:new() ),

	EntrySetState = addKeyValueToAttribute( WithTableState, test_hashtable,
		my_key, my_value ),

	% Check was registered indeed:
	ReadTable = getAttribute( EntrySetState, test_hashtable ),

	{ value, my_value } = ?wooper_table_type:lookupEntry( my_key,
														  ReadTable ),

	test_ok.



% Actual test.
-spec run() -> no_return().
run() ->

	test_facilities:display( "Running attribute test." ),

	Tested = class_AttributeTester:new_link(),

	Tested ! { test, [], self() },

	receive

		{ wooper_result, test_ok } ->
			test_facilities:display( "Test success." ),
			test_facilities:stop();

		Other ->
			test_facilities:fail( "Test failed: ~p.", [ Other ] )

	end.
