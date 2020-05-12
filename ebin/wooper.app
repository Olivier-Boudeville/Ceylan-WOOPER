% Description of the WOOPER OTP active application, typically used by rebar3.

% Note: if this file is named wooper.app, it is a *generated* file, whose source
% is conf/wooper.app.src.

% See also:
% - http://erlang.org/doc/man/app.html
% - https://learnyousomeerlang.com/building-otp-applications


{application, wooper,
 [{description, "Ceylan-WOOPER, a Wrapper for Object-Oriented Programming in Erlang, as an OTP active application here (see http://wooper.esperide.org)"},
  {vsn, "2.0.9"},

  % See wooper_class_manager.hrl:
  {registered, [wooper_class_manager]},

  % Regarding Myriad, see http://myriad.esperide.org/myriad.html#otp:
  % (however myriad is a library application, not an active one)
  {applications, [kernel, stdlib, myriad]},

  %{env,[]},

  % Flat hierarchy in ebin here:
  {modules, [wooper_method_management, wooper_class_manager, wooper_sup, wooper_instance_destruction, wooper, wooper_instance_construction, wooper_class_management, wooper_introspection, wooper_parse_utils, wooper_parse_transform, wooper_serialisation, wooper_app, wooper_state_management, wooper_info, wooper_internals, wooper_instance_proxy, wooper_utils]},

  {licenses, ["Ceylan-WOOPER is licensed by its author (Olivier Boudeville) under a disjunctive tri-license, giving you the choice of one of the three following sets of free software/open source licensing terms:
	- the Mozilla Public License (MPL), version 1.1 or later (very close to the former Erlang Public License, except aspects regarding Ericsson and/or the Swedish law)
	- the GNU General Public License (GPL), version 3.0 or later
	- the GNU Lesser General Public License (LGPL), version 3.0 or later"]},

  % Active application (yet would still work without thanks to automatic launch
  % of the class manager):
  %
  % (no specific relevant startup argument to specify here)
  %
  {mod, {wooper_app, []}},

  { links, [ {"Official website", "http://wooper.esperide.org" },
			 {"Github", "https://github.com/Olivier-Boudeville/Ceylan-WOOPER"} ]}

  %{exclude_files, []}

 ]}.