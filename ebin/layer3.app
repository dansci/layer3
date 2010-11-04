%%-*- mode: erlang -*-
{application, layer3,
 [
  {description, "layer3"},
  {vsn, "1"},
  {modules, [
             layer3,
             layer3_app,
             layer3_sup,
             layer3_resource,
	     layer3_read,
	     layer3_tools
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
  {mod, { layer3_app, []}},
  {env, []}
 ]}.
