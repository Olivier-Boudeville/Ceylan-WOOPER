% Copyright (C) 2019-2019 Olivier Boudeville
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


% Testing of WOOPER as an OTP active application.
-module(wooper_otp_application_test).


% For run/0 export and al:
-include("test_facilities.hrl").


% Actual test:
test_wooper_application( WOOPEREBinPath, MyriadEBinPath ) ->

	code_utils:declare_beam_directories( [ WOOPEREBinPath, MyriadEBinPath ] ),

	test_facilities:display( "Starting the WOOPER application." ),

	% Was expecting starting dependencies would be automatic, apparently it is
	% not the case; moreover it visibly should be done before entering
	% wooper_app:start/2, so:
	%
	ok = application:start( myriad ),

	ok = application:start( wooper ),

	test_facilities:display( "WOOPER version: ~p.",
				 [ system_utils:get_application_version( wooper ) ] ),

	% To test also a WOOPER module:

	TestClassname = 'class_Tiger',

	test_facilities:display( "Class filename corresponding to '~s': '~s'.",
			 [ TestClassname, wooper:get_class_filename( TestClassname ) ] ),


	test_facilities:display( "Stopping the WOOPER application." ),
	ok = application:stop( wooper ),
	ok = application:stop( myriad ),

	test_facilities:display(
	  "Successful end of test of the WOOPER application." ).



% Note that the ebin application directory must be in the code path for the
% wooper.app file to be found and used, and for this test to succeed.
%
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	% Supposing here that the application is built, in the usual _build
	% directory, with the default rebar3 profile:
	%
	WOOPEREBinPath = "../_build/default/lib/wooper/ebin/",

	case file_utils:is_existing_directory_or_link( WOOPEREBinPath ) of

		true ->

			MyriadEBinPath =
				"../../Ceylan-Myriad/_build/default/lib/myriad/ebin/",

			case file_utils:is_existing_directory_or_link( WOOPEREBinPath ) of

				true ->
					test_wooper_application( WOOPEREBinPath, MyriadEBinPath ) ;

				false ->
					trace_utils:warning_fmt(
					  "No build directory found for the Myriad parent "
					  "application (searched for '~s'), stopping this test "
					  "(run beforehand 'make rebar3-application' at the root "
					  "of this source tree for a more relevant testing).",
					  [ MyriadEBinPath ] )

			end;


		false ->
			trace_utils:warning_fmt( "No build directory found for the WOOPER "
				"application (searched for '~s'), stopping this test "
				"(run beforehand 'make rebar3-compile' at the root of the "
				"source tree for a more relevant testing).",
				[ WOOPEREBinPath ] )

	end,

	test_facilities:stop().
