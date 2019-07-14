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
% Creation date: Thursday, July 11, 2019.
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]


% Module implementing the (active) application behaviour.
%
% Note that, thanks to the automatic creation of the class manager, WOOPER will
% still work flawlessly even if not specifically started.
%
-module(wooper_app).


% Implementing the (active, OTP) application behaviour:
-behaviour(application).


% Callbacks of the application behaviour:
-export([ start/2, stop/1 ]).



% Starts the WOOPER services.
start( Type, StartArgs ) ->

	trace_utils:debug_fmt( "Starting WOOPER application (type: ~w, "
						   "start arguments: ~w).", [ Type, StartArgs ] ),

	% Previously, no specific root supervisor was to launch, but:
	%wooper_class_manager:start().

	case wooper_sup:start_link() of

		R={ ok, _RootSupervisorPid } ->
			R;

		Other ->
			trace_utils:error_fmt( "The WOOPER root supervisor did not start "
								   "properly: ~w.", [ Other ] ),
			{ error, Other }

	end.



% Stops the WOOPER services.
stop( State ) ->

	trace_utils:debug_fmt( "Stopping WOOPER application (state: ~w).",
						   [ State ] ),

	% Previously (now managed by the root supervisor):
	% wooper_class_manager:stop().

	ok.
