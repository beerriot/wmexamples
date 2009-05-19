{application, wmexamples,
 [{description, "wmexamples"},
  {vsn, "0.1"},
  {modules, [
    wmexamples,
    wmexamples_app,
    wmexamples_sup,
    wmexamples_deps,
    wmexamples_resource
  ]},
  {registered, []},
  {mod, {wmexamples_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
