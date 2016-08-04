{application, html_parser_app,
  [
    {description, "Simple Erlang Html Parser"},
    {vsn, "0.0.1"},
    {modules, [html_parser, tokenizer, tree_lookup]},
    {registered, [html_parser]},
    {applications, [kernel, stdlib]}
  ]
}.
