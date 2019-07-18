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
% Creation date: Sunday, July 14, 2019.
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]


% Module implementing the root supervisor of WOOPER.
%
% In practice, it will supervise a single process, the one of the (singleton)
% WOOPER class manager.
%
-module(wooper_sup).


% Implementing the OTP supervisor behaviour:
-behaviour(supervisor).


% User API:
-export([ start_link/0 ]).


% Callback of the supervisor behaviour:
-export([ init/1 ]).


-define( supervisor_name, ?MODULE ).


% Starts and links the WOOPER root supervisor.
start_link() ->

	trace_utils:debug( "Starting WOOPER root supervisor." ),

	supervisor:start_link( { local, ?supervisor_name },
						   _Module=?MODULE, _Args=[] ).


% Callback to initialise this supervisor.
init( [] ) ->

	ChildManagerSpec = { wooper_class_manager_id,
		_Start={ _Mod=wooper_class_manager, _Fun=start_link, _Args=[] },
		_Restart=permanent,
		% 2-second termination allowed:
		_Shutdown=2000, worker,
		% Not to mention Myriad ones:
		_DepMods=[ wooper_class_manager, wooper ] },

	ChildrenSpec = [ ChildManagerSpec ],

	% No automatic restarts for a class manager that is never expected to fail:
	RestartStrategy = { one_for_one, _MaxRestarts=0, _WithinSeconds=1 },
	{ ok, { RestartStrategy, ChildrenSpec } }.