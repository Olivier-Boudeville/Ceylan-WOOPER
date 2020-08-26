% Copyright (C) 2019-2020 Olivier Boudeville
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
% Creation date: Friday, July 19, 2019.


% Testing of WOOPER as an OTP active application, directly from within its code
% base (hence without needing to create a separate, mock-up test release for
% that).
%
-module(wooper_otp_application_test).


% For run/0 export and al:
-include_lib("myriad/include/test_facilities.hrl").


% Actual test:
test_wooper_application( OrderedAppNames ) ->

	test_facilities:display( "Starting the WOOPER OTP active application." ),
	otp_utils:start_applications( OrderedAppNames ),


	test_facilities:display( "WOOPER version: ~p.",
				 [ system_utils:get_application_version( wooper ) ] ),

	% To test also a WOOPER module:

	TestClassname = 'class_Tiger',

	test_facilities:display( "Class filename corresponding to '~s': '~s'.",
			 [ TestClassname, wooper:get_class_filename( TestClassname ) ] ),


	test_facilities:display( "Stopping the WOOPER application." ),
	otp_utils:stop_applications( OrderedAppNames ),

	test_facilities:display(
	  "Successful end of test of the WOOPER OTP application." ).



% Note that the wooper.app and myriad.app files will have to be found and used
% for this test to succeed: WOOPER and Myriad must be already available as
% prerequisite, fully-built OTP applications.
%
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	% Build root directory from which prerequisite applications may be found:
	BuildRootDir = "..",

	OrderedAppNames = [ myriad, wooper ],

	case otp_utils:prepare_for_test( OrderedAppNames, BuildRootDir ) of

		ready ->
			test_wooper_application( OrderedAppNames ) ;

		{ lacking_app, _App } ->
			% (a detailed warning message has been issued by
			% otp_utils:prepare_for_test/2)
			%
			ok

	end,

	test_facilities:stop().
