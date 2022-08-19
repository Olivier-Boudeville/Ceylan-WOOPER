% Copyright (C) 2022-2022 Olivier Boudeville
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
% Creation date: Tuesday, August 16, 2022.


% @doc Unit tests for the <b>Upgradable</b> class implementation.
%
% See the class_Upgradable module.
%
-module(class_Upgradable_test).


-export([ run/0 ]).


% For testing, one may force the compilation of the 1.2.4 version that way:
%
% $ /bin/rm -f class_TestUpgradable.beam; make class_TestUpgradable.beam
%       ERLANG_COMPILER_EXTRA_OPTS="-Denable_upgraded_test_class"


% Shorthands:

-type bin_string() :: text_utils:bin_string().
-type instance_pid() :: wooper:instance_pid().



% Tells whether the test module has old code.
-spec check_old_code() -> void().
check_old_code() ->
	test_facilities:display( "Has '~ts' old code? ~ts.",
		[ class_TestUpgradable,
		  erlang:check_old_code( class_TestUpgradableModule ) ] ).



% Returns a description of the specified instance, as the TestUpgradable class
% implements the Describable interface.
%
-spec get_description( instance_pid() ) -> bin_string().
get_description( IPid ) ->

	IPid ! { getDescription, [], self() },

	receive

		{ wooper_result, BinDesc } ->
			BinDesc

	end.



% @doc Runs the tests.
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	check_old_code(),

	TargetClass = class_TestUpgradable,

	% We do not want to inherit any updated, recompiled class from a prior test:
	ForceRecompilation = true,

	% Needed otherwise the update testing is meaningless:
	DoForceRecompilation = true,

	% Upgrade testing:

	% Initial version has no preprocessor define set; so a version 1.2.3 is
	% built:
	%
	ok = code_utils:recompile( TargetClass, _SrcBaseDir=".",
							   ForceRecompilation, _Define=[] ),

	test_facilities:display( "Creating a first test Upgradable." ),

	% We start with the base (1.2.3) implementation:
	AgentAPid = TargetClass:new_link( "Agent A", _Height=1.80 ),

	AgentAPid ! { getVersion, [], self() },

	InitialVersion = receive

		{ wooper_result, V1 } ->
			V1

	end,

	%test_facilities:display( "Initial version returned for A: '~ts'.",
	%   [ text_utils:version_to_string( InitialVersion ) ] ),

	{ 1, 2, 3 } = InitialVersion,

	% Here common to all updates and pattern-matched:
	ExtraData = upgradable_test,

	%test_facilities:display( "Initial description of A is: '~ts'.",
	%						 [ get_description( AgentAPid ) ] ),

	AgentBPid = TargetClass:new_link( "Agent B", 1.63 ),

	% This instance will intentionally not be updated:
	%AgentCPid = TargetClass:new_link( "Agent C", 1.71 ),


	% To test what happens if some instances (here, C) are not updated: the
	% class cannot then be soft-purged and, if KillAnyLingeringProcess is true,
	% C is killed and, as it is linked to this test process, the test fails:
	%
	InstancesToUpdate = [ AgentAPid, AgentBPid ],

	%AllInstances = list_utils:append_at_end( AgentCPid, InstancesToUpdate ),
	AllInstances = InstancesToUpdate,

	%InstancesToUpdate = [ AgentAPid, AgentBPid, AgentCPid ],
	%AllInstances = Instances,

	%test_facilities:display( "Initial description of B is: '~ts'.",
	%                         [ get_description( AgentBPid ) ] ),

	test_facilities:display( "Displaying the initial state description of "
		"all instances: ~ts",
		[ text_utils:strings_to_string( [ get_description( IPid )
					|| IPid <- AllInstances ] ) ] ),

	TargetVersion = { 1, 2, 4 },

	test_facilities:display( "This test process is the update controller here; "
		"requesting an upgrade of class '~ts' from version ~ts to version ~ts, "
		"starting by freezing instances ~w.",
		[ TargetClass, text_utils:version_to_string( InitialVersion ),
		  text_utils:version_to_string( TargetVersion ),
		  InstancesToUpdate ] ),

	UpFreezeInfos = class_Upgradable:freeze_instances( InstancesToUpdate,
							TargetVersion, ExtraData ),

	test_facilities:display( "Upgrade freeze information received: ~p.",
							 [ UpFreezeInfos ] ),

	%KillAnyLingeringProcess = true,
	KillAnyLingeringProcess = false,

	test_facilities:display( "Requesting now a first update, an upgrade, "
							 "of '~ts'.", [ TargetClass ] ),

	check_old_code(),

	class_Upgradable:update_class( TargetClass, DoForceRecompilation,
		_Defines=[ "enable_upgraded_test_class" ], KillAnyLingeringProcess ),

	test_facilities:display( "After the class upgrade, checking which "
		"instances (not specifically updated yet) are still alive: ~ts",
		[ text_utils:strings_to_string( [ text_utils:format( "instance ~w: ~ts",
			[ IPid, basic_utils:is_alive( IPid ) ] )
								|| IPid <- AllInstances ] ) ] ),

	check_old_code(),

	test_facilities:display( "Requesting a corresponding update of "
							 "the instances of '~ts'.", [ TargetClass ] ),

	case class_Upgradable:request_instances_to_update( InstancesToUpdate ) of

		{ UpSuccReports, _UpFailReports=[] } ->
			test_facilities:display( "All ~B instances successfully updated:~n"
				" ~p", [ length( UpSuccReports ), UpSuccReports ] );

		{ _UpSuccReports, UpFailReports } ->
			throw( { update_failed, UpFailReports } )

	end,

	check_old_code(),

	% Not AllInstances, as C did not upgraded its state and therefore the new
	% to_string/1 function would not find the new attribute ('age') that it
	% would expect:
	%
	test_facilities:display( "Displaying a state description of all updated "
		"(upgraded) instances: ~ts",
		[ text_utils:strings_to_string(
			[ get_description( IPid ) || IPid <- InstancesToUpdate ] ) ] ),



	% Downgrade testing:


	test_facilities:display( "Now requesting this time a downgrade of '~ts'.",
							 [ TargetClass ] ),

	DownFreezeInfos = class_Upgradable:freeze_instances( InstancesToUpdate,
							InitialVersion, ExtraData ),

	test_facilities:display( "Downgrade freeze information received: ~p.",
							 [ DownFreezeInfos ] ),

	check_old_code(),

	% No enable_upgraded_test_class define, so version 1.2.3:
	class_Upgradable:update_class( TargetClass, DoForceRecompilation,
								   _NoDefines=[], KillAnyLingeringProcess ),

	test_facilities:display( "After the class downgrade, checking which "
		"instances are still alive: ~ts",
		[ text_utils:strings_to_string( [ text_utils:format( "instance ~w: ~ts",
			[ IPid, basic_utils:is_alive( IPid ) ] )
								|| IPid <- AllInstances ] ) ] ),

	check_old_code(),

	test_facilities:display( "Requesting a corresponding update of "
							 "the instances of '~ts'.", [ TargetClass ] ),

	case class_Upgradable:request_instances_to_update( InstancesToUpdate ) of

		{ DownSuccReports, _DownFailReports=[] } ->
			test_facilities:display( "All ~B instances successfully updated:~n"
				" ~p", [ length( DownSuccReports ), DownSuccReports ] );

		{ _DownSuccReports, DownFailReports } ->
			throw( { update_failed, DownFailReports } )

	end,

	check_old_code(),

	% Not InstancesToUpdate, as even C, never updated, complies with the 1.2.3
	% conventions:
	%
	test_facilities:display( "Displaying a state description of all "
		"instances: ~ts",
		[ text_utils:strings_to_string(
			[ get_description( IPid ) || IPid <- AllInstances ] ) ] ),

	wooper:delete_synchronously_instances( AllInstances ),
	check_old_code(),

	test_facilities:stop().
