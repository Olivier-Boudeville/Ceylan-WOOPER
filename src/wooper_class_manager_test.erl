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


% Unit tests for the WOOPER class manager implementation.
% See the wooper_class_manager.erl tested module.

-module(wooper_class_manager_test).

-export([run/0]).

-define(Tested_module,wooper_class_manager).

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


% For WooperClassManagerName:
-include("wooper_class_manager.hrl").


run() ->
	io:format( ?Prefix "Testing module ~s.~n",  [ ?Tested_module ] ),
	io:format( ?Prefix "Spawning module ~s.~n", [ ?Tested_module ] ),
	spawn(?Tested_module,start,[self()]),	
	receive
			
		class_manager_registered ->
			io:format( ?Prefix "Requesting its state display.~n" ),
			?WooperClassManagerName ! display
			
	% 10-second time-out:
	after 10000	->
		error_logger:error_msg( "#### wooper_get_class_manager: unable to find "
			"class manager after 10s, test failed." )
				
	end,
	io:format( ?Prefix "Requesting it to stop.~n" ),
	?WooperClassManagerName ! stop,
	% Probably, if ExitAfterTest is set, the test will stop before the
	% manager itself.
	io:format( ?Prefix "End of test for module ~s.~n", [ ?Tested_module ] ),
	testFinished().

