% Unit tests for the MYCLASS class implementation.
% See the class_MYCLASS.erl tested module.

-module(class_MYCLASS_test).

-export([run/0]).

-define(Tested_module,class_MYCLASS).

-define(Prefix,"--> ").



% Comment out to be able to use the interpreter after the test:
-define(ExitAfterTest,).

-ifdef(ExitAfterTest).

testFinished() ->
	erlang:halt().
	
-else.

testFinished() ->
	io:format( "(interpreter still running)~n" ),
	test_success.
	
-endif.


testFailed(Reason) ->
	% For some reason erlang:error is unable to interpret strings as strings,
	% they are always output as unreadable list.
	io:format( "~n!!!! Test failed for module ~s, reason: ~s~n~n",
		[ ?Tested_module, Reason ] ),
	erlang:error( "Test failed" ).	
		

run() ->
	io:format( ?Prefix "Testing module ~s.~n", [ ?Tested_module ] ),
	io:format( ?Prefix "Debug mode: ~s.~n", 
		[ class_MYCLASS:is_wooper_debug() ] ),	
	
		
	% General tests.
	
	io:format( ?Prefix "Statically, class name is ~s, superclasses are ~w.~n", 
		[
			class_MYCLASS:get_class_name(), 
			class_MYCLASS:get_superclasses() ] ),
	MyC = class_MYCLASS:new(X,Y,Z),
	MyC ! {get_class_name,[],self()},
	receive
	
		{wooper_result,class_MYCLASS} ->
			io:format( ?Prefix 
				"After constructor, get_class_name returned 'class_MYCLASS' "
				"as expected.~n");
				
		{wooper_result,UnexpectedClass} -> 
			testFailed( io_lib:format( "wrong class: ~p",
				[ UnexpectedClass ] ) )
			
	end,
	MyC ! {get_superclasses,[],self()},
	receive
	
		{wooper_result, [class_X,class_Y]} ->
			io:format( ?Prefix 
				"After constructor, get_superclasses returned "
				"[class_X,class_Y] as expected.~n");
				
		{wooper_result,UnexpectedSuperclasses} -> 
			testFailed( io_lib:format( "wrong superclasses: ~p", 
				[ UnexpectedSuperclasses ] ) )
	
	end,
	
	
	% Tests related to XXXX.
	
	MyC ! {getX,[],self()},
	receive
	
		{wooper_result,X} ->
			io:format( ?Prefix getX returned X as expected.~n");

		{wooper_result,UnexpectedX} -> 
			testFailed( io_lib:format( "wrong X: ~p", 
				[ UnexpectedAge ] ) )
		
	end,
	
	case class_MYCLASS:is_wooper_debug() of 
		true ->
			MyC ! { wooper_get_instance_description,[], self() },
			receive
			
				{wooper_result,InspectString} ->
					io:format( "~s~n", [ InspectString ] )
			end ;		
		false ->
			ok	
	end,
	MyC ! delete,				
	io:format( ?Prefix "End of test for module ~s.~n", [ ?Tested_module ] ),
	testFinished().

