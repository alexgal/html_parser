-module(html_parser_tests).
-author("alexey").

-include_lib("eunit/include/eunit.hrl").

-import(tokenizer_tests, [test_html/0]).

flat_tree_test() ->
  Expected = [{tag,<<"head">>,undefined,[]}, {tag,<<"body">>,undefined,[]}],
  Actual = html_parser:parse(<<"<head></head><body></body>">>),
  Expected = Actual.

nested_tree_test() ->
  Expected = [{tag,<<"doctype">>,
    [{tag,<<"html">>,
      [{tag,<<"head">>,
        [{tag,<<"script">>,
          [{text,<<"alert(\"hello world!\");\n          ">>}],
          [{<<"type">>,[<<"text/javascript">>]}]}],
        []},
        {tag,<<"boby">>,
          [{tag,<<"div">>,
            [{tag,<<"span">>,
              [{text,<<"Some ">>},
                {tag,<<"br">>,undefined,[]},
                {text,<<"text ">>}],
              [{<<"id">>,[<<"span-id">>]}]}],
            [{<<"class">>,[<<"classa">>,<<"classb">>]}]}],
          []}],
      []}],
    []}],
    Actual = html_parser:parse(test_html()),
    Expected = Actual.

lookup_by_class_test() ->
  Expected = {ok,[{tag,<<"div">>,
    [{tag,<<"span">>,
      [{text,<<"Some ">>},
        {tag,<<"br">>,undefined,[]},
        {text,<<"text ">>}],
      [{<<"id">>,[<<"span-id">>]}]}],
    [{<<"class">>,[<<"classa">>,<<"classb">>]}]}]},
  Actual = tree_lookup:tag_with_classes([<<"classa">>],html_parser:parse(tokenizer_tests:test_html())),
  Expected = Actual.

lookup_miss_by_class_test() ->
  Expected = {not_found, []},
  Actual = tree_lookup:tag_with_classes([<<"classab">>],html_parser:parse(tokenizer_tests:test_html())),
  Expected = Actual.




