-module(tokenizer).
-author("alexey").

%% API
-export([tokenize/1]).

-include("../include/html_parser.hrl").

-spec tokenize(binary())               -> [term(),...].
tokenize(Input)                        ->
  tokenize(Input, []).

-spec tokenize(binary(), [term(),...]) -> [term(),...].
tokenize(<<>>, Stack)                  ->
  Stack;
tokenize(B, Stack) -> % tag found
  {Terms, Remainder} = fetch_expr(B),
  Stack1 = push_to_stack(Stack, Terms),
  tokenize(Remainder, Stack1).

push_to_stack(Stack, Terms) ->
  Stack1 = Stack ++ Terms,
  Stack1.

-spec fetch_expr(binary()) -> [html_term(),...].
fetch_expr(Binary)         ->
  {Term, Remainder} = fetch_term(Binary),
  fetch_expr(Remainder, Term).


fetch_expr(Binary, PreviousTerm = {<<"script">>, opening_tag, _Attr}) ->
  {NextTerm, Remainder} = fetch_term(Binary),
  {Term1, Remainder1} =
    case NextTerm of
      {<<"script">>, closing_tag, _Attr} ->
        {NextTerm, Remainder};
      _Term ->
        fetch_script_contents(Binary, <<>>)
    end,
  {[PreviousTerm, Term1], Remainder1};
fetch_expr(Binary, {text, <<>>}) ->
  {Term, Remainder} = fetch_term(Binary),
  {[Term], Remainder};
fetch_expr(Binary, PreviousTerm) ->
  {[PreviousTerm], Binary}.

fetch_script_contents(Binary = <<"</script>", _T/binary>>, Acc) ->
  {{text, Acc}, Binary};
fetch_script_contents(<<H,T/binary>>, Acc) ->
  Acc1 = <<Acc/binary, H>>,
  fetch_script_contents(T, Acc1).


-spec fetch_term(binary()) -> html_term().
fetch_term(Binary = <<"<!--", _T/binary>>) ->
  fetch_comments(Binary, {comments, <<>>});
fetch_term(Binary = <<"<", _T/binary>>)    ->
  fetch_tag(Binary, {undefined, <<>>});
fetch_term(Binary)                         ->
  fetch_text(Binary, <<>>).

-spec fetch_comments(binary(), comments())            -> {comments(), Remainder} when
  Remainder :: binary().
fetch_comments(<<"<!--", T/binary>>, {comments, Acc}) ->
  Acc1 = <<Acc/binary, "<!--">>,
  fetch_comments(T, {comments, Acc1});
fetch_comments(<<"-->", R/binary>>, {comments, Acc})  ->
  {{comments, Acc}, R};
fetch_comments(<<H,T/binary>>, {comments, Acc})       ->
  Acc1 = <<Acc/binary, H>>,
  fetch_comments(T, {comments, Acc1}).

-spec fetch_text(binary(), text())       -> {text(), Remainder} when
  Remainder :: binary().
fetch_text(<<"\s", T/binary>>, <<>>)     ->
  fetch_text(T, <<>>);
fetch_text(<<"\n", T/binary>>, <<>>)     ->
  fetch_text(T, <<>>);
fetch_text(R = <<"<", _T/binary>>, Acc)  ->
  {{text, Acc}, R};
fetch_text(R = <<"</", _T/binary>>, Acc) ->
  {{text, Acc}, R};
fetch_text(<<>>, Acc)                    ->
  {{text, Acc}, <<>>};
fetch_text(<<H, T/binary>>, AccBinary)   ->
  Acc1 = <<AccBinary/binary, H>>,
  fetch_text(T, Acc1).


-spec fetch_tag(binary(), {tag_type(), tag_name()})     -> {tag(), Remainder} when
  Remainder :: binary().
fetch_tag(<<"</", T/binary>>, {_TagType, _AccBinary})   ->
  fetch_tag(T, {closing_tag, <<>>});
fetch_tag(<<"<", T/binary>>, {_TagType, _AccBinary})    ->
  fetch_tag(T, {opening_tag, <<>>});
fetch_tag(<<"/>", T/binary>>, {opening_tag, AccBinary}) ->
  TagType1 = ensure_tag_type(opening_tag, can_be_singleton_tag(AccBinary)),
  form_tag(AccBinary, TagType1, T);
fetch_tag(<<">", T/binary>>, {TagType, AccBinary})      ->
  TagType1 = ensure_tag_type(TagType, is_singleton_tag(AccBinary)),
  form_tag(AccBinary, TagType1, T);
fetch_tag(<<H, T/binary>>, {TagType, AccBinary})        ->
  AccBinary1 = <<AccBinary/binary, H>>,
  fetch_tag(T, {TagType, AccBinary1}).

form_tag(AccBinary, TagType = closing_tag, T) ->
  {TagName, _Remainder} = fetch_tag_name(AccBinary, <<>>),
  {{TagName, TagType}, T};
form_tag(AccBinary, TagType, T) ->
  {TagName, Attributes} = fetch_tag_details(AccBinary),
  {{TagName, TagType, Attributes}, T}.

-spec fetch_tag_details(binary()) -> {tag_name(), Attributes} when
  Attributes :: [attribute(),...].
fetch_tag_details(Binary) ->
  {TagName, Remainder} = fetch_tag_name(Binary, <<>>),
  {Attributes, _}      = fetch_attribute(Remainder, []),
  {TagName, Attributes}.


fetch_attribute(<<>>, Acc)               ->
  {Acc, <<>>};
fetch_attribute(<<"\s", T/binary>>, Acc) ->
  fetch_attribute(T, Acc);
fetch_attribute(Binary, Acc)             ->
  {Key, Remainder}   = fetch_attribute_key(Binary, <<>>),
  {Value, Remainder1} = fetch_attribute_values(Remainder, []),
  Acc1 = Acc ++ [{Key, Value}],
  fetch_attribute(Remainder1, Acc1).

fetch_attribute_key(<<>>, Acc) ->
  {Acc, <<>>};
fetch_attribute_key(<<"=\"", T/binary>>, Acc) ->
  {Acc,T};
fetch_attribute_key(<<"\s", T/binary>>, Acc)  ->
  fetch_attribute_key(T, Acc);
fetch_attribute_key(<<H, T/binary>>, Acc)     ->
  Acc1 = <<Acc/binary, H>>,
  fetch_attribute_key(T, Acc1).

fetch_attribute_values(<<>>, Acc)               ->
  {Acc, <<>>};
fetch_attribute_values(<<"\"", T/binary>>, Acc) ->
  {Acc, T};
fetch_attribute_values(Binary, Acc)             ->
  {Value, Remainder} = fetch_attribute_value(Binary, <<>>),
  Acc1 = Acc ++ [Value],
  fetch_attribute_values(Remainder, Acc1).

fetch_attribute_value(<<>>, Acc)                    ->
  {Acc, <<>>};
fetch_attribute_value(<<"\s", T/binary>>, Acc)      ->
  {Acc, T};
fetch_attribute_value(B = <<"\"", _T/binary>>, Acc) ->
  {Acc, B};
fetch_attribute_value(<<H, T/binary>>, Acc)         ->
  Acc1 = <<Acc/binary, H>>,
  fetch_attribute_value(T, Acc1).

fetch_tag_name(<<>>, Acc)               ->
  {Acc, <<>>};
fetch_tag_name(<<"\s", T/binary>>, Acc) ->
  {Acc, T};
fetch_tag_name(<<H, T/binary>>, Acc)    ->
  Acc1 = <<Acc/binary, H>>,
  fetch_tag_name(T, Acc1).

-spec ensure_tag_type(tag_type(), boolean()) -> tag_type().
ensure_tag_type(_TagType, true)              ->
  singleton_tag;
ensure_tag_type(TagType, false)              ->
  TagType.

can_be_singleton_tag(Tag) ->
  has_tag(Tag, ?POSSIBLY_NON_SINGLETONS) /= true.

is_singleton_tag(Tag) ->
  has_tag(Tag, ?SINGLETON_TAGS).

has_tag(Tag, List) ->
  {Tag1,_} = fetch_tag_name(Tag,<<>>),
  lists:any(fun(T) -> T =:= Tag1 end, List).
