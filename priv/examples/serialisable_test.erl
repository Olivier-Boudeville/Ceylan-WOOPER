% Copyright (C) 2012-2022 Olivier Boudeville
%
% This file is part of the Ceylan-WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
% Creation date: 2012.


% @doc Testing the implementation of the <b>serialisation of WOOPER
% instances</b>, that is of the default implementation of the Serialisable
% interface.
%
-module(serialisable_test).


% For run/0 export and al:
-include_lib("myriad/include/test_facilities.hrl").



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	% Allows to support both OTP conventions and ad hoc, automatic ones:
	wooper_utils:start_for_test(),

	Age = 3,

	MyC = class_Cat:new_link( Age, female, sand, white ),

	MyC ! { toString, [], self() },

	receive

		{ wooper_result, FirstDescription } ->
			test_facilities:display( "Created following cat: ~ts~n" ,
									 [ FirstDescription ] )

	end,


	MyC ! { getAge, [], self() },
	receive

		{ wooper_result, Age } ->
			ok

	end,

	ActualUserData = none,

	% This is a do-nothing transformer, except that it outputs on the console
	% the attributes that it filters:
	%
	TextTransformer =
		fun( Entry={ AttributeName, AttributeValue },
			 _Acc={ AccEntries, AccUserData } ) ->

			test_facilities:display( " - text transformer: "
				"attribute name '~ts' is associated to value '~p'",
				[ AttributeName, AttributeValue ] ),

			% New accumulator:
			{ [ Entry | AccEntries ], AccUserData }

		end,

	MyC ! { serialise, [ TextTransformer, ActualUserData ], self() },

	CatSerialisation = receive

		{ wooper_result, { CatSerial, SerialUserData } } ->

			test_facilities:display( "Text transformer returned:~n"
				" - serialisation of ~ts:~n ~p~n"
				" - resulting user data: ~p~n",
				[ system_utils:interpret_byte_size(
					system_utils:get_size( CatSerial ) ), CatSerial,
				  SerialUserData ] ),

			CatSerial

	end,


	MyC ! { synchronous_delete, self() },
	receive

		{ deleted, MyC } ->
			ok

	end,


	test_facilities:display( "Testing also serialisation overridden method "
							 "(hooks), with a reptile." ),

	MyR = class_Reptile:new_link( 35, female ),

	MyR ! { serialise, [], self() },

	ReptileSerialisation = receive

		{ wooper_result, ReptSerial } ->

			test_facilities:display( "Default transformer returned a "
				"serialisation of ~ts:~n ~p~n",
				[ system_utils:interpret_byte_size(
					system_utils:get_size( ReptSerial ) ), ReptSerial ] ),

			ReptSerial

	end,


	test_facilities:display(
		"Recreating an instance corresponding to previous information." ),

	NewC = class_Serialisable:load_link( CatSerialisation ),

	NewC ! { toString, [], self() },

	receive

		{ wooper_result, SecondDescription } ->
			test_facilities:display( "Deserialised following cat: ~ts~n" ,
									 [ SecondDescription ] )

	end,

	NewC ! { getAge, [], self() },
	receive

		{ wooper_result, Age } ->
			ok

	end,

	NewC ! { synchronous_delete, self() },
	receive

		{ deleted, NewC } ->
			ok

	end,


	NewR = class_Serialisable:load_link( ReptileSerialisation ),

	NewR ! { synchronous_delete, self() },
	receive

		{ deleted, NewR } ->
			ok

	end,

	test_facilities:stop().
