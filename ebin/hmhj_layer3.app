%%-*- mode: erlang -*-
{application, hmhj_layer3,
 [
  {description, "hmhj_layer3"},
  {vsn, "0.1"},
  {modules, [
             hmhj_layer3,
             hmhj_layer3_app,
             hmhj_layer3_sup,
             hmhj_layer3_resource,
	     hmhj_layer3_read,
	     hmhj_layer3_tools
            ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib,
                  crypto,
                  mochiweb,
                  webmachine,
		  hmhj_layer2
                 ]},
  {mod, { hmhj_layer3_app, []}},
  {env, []}
 ]}.
