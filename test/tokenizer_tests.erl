-module(tokenizer_tests).
-author("alexey").

-include_lib("eunit/include/eunit.hrl").

-export([test_html/0]).

optional_singleton_tag_test() ->
    [{<<"div">>,singleton_tag,[]}] = tokenizer:tokenize(<<"<div/>">>).

html5_like_doctype_test() ->
  [{<<"!DOCTYPE">>,singleton_tag,[]}] = tokenizer:tokenize(<<"<!DOCTYPE >">>).

nested_structure_test() ->
  Expected = [{<<"doctype">>,opening_tag,[]},
    {<<"html">>,opening_tag,[]},
    {<<"head">>,opening_tag,[]},
    {<<"script">>,opening_tag,
      [{<<"type">>,[<<"text/javascript">>]}]},
    {text,<<"alert(\"hello world!\");\n          ">>},
    {<<"script">>,closing_tag},
    {<<"head">>,closing_tag},
    {<<"boby">>,opening_tag,[]},
    {<<"div">>,opening_tag,
      [{<<"class">>,[<<"classa">>,<<"classb">>]}]},
    {<<"span">>,opening_tag,[{<<"id">>,[<<"span-id">>]}]},
    {text,<<"Some ">>},
    {<<"br">>,singleton_tag,[]},
    {text,<<"text ">>},
    {<<"span">>,closing_tag},
    {<<"div">>,closing_tag},
    {<<"body">>,closing_tag},
    {<<"html">>,closing_tag}],
  Actual = tokenizer:tokenize(test_html()),
  Expected =:= Actual.

has_class_attributes_test() ->
  Stack = tokenizer:tokenize(test_html()),
  Expected = {value,{<<"div">>,opening_tag, [{<<"class">>,[<<"classa">>,<<"classb">>]}]}},
  Actual = lists:keysearch(<<"div">>, 1, Stack),
  Expected = Actual.

test_html() ->
  <<"<doctype >
      <html>
        <head>
          <script type=\"text/javascript\"> alert(\"hello world!\");
          </script>
        </head>
        <boby>
          <div class=\"classa classb\">
            <span id=\"span-id\"> Some <br /> text </span>
          </div>
        </body>
      </html>">>.
