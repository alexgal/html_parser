-module(html_parser).
-author("alexey").

%% API
-export([parse/1]).

-import(tokenizer, [tokenize/1]).

-include("../include/html_parser.hrl").

-spec parse(binary()) -> list().
parse(Input) ->
  build_tree(tokenize(Input)).

-spec build_tree([html_term(),...]) -> list().
build_tree(Stack) ->
  {Tree, _} = build_tree(Stack, []),
  Tree.

-spec build_tree([html_term(),...], list()) -> list().
build_tree([], Tree) ->
  {Tree, []};
build_tree([_Term = {TagName, opening_tag, Attributes}, _Term1 = {TagName1, closing_tag} | T],  Tree) when
  TagName =:= TagName1                                              ->
  Tag = #tag{name=TagName, attributes = Attributes},
  Tree1 = Tree ++ [Tag],
  build_tree(T, Tree1);
build_tree([_Term = {_TagName, closing_tag} | T], Tree)             ->
  {Tree, T};
build_tree([_Term = {TagName, opening_tag, Attributes} | T], Tree)  ->
  {Children, Remainder} = build_tree(T, []),
  Tag = #tag{name=TagName, children = Children, attributes = Attributes},
  Tree1 = Tree ++ [Tag],
  build_tree(Remainder, Tree1);
build_tree([_Term = {TagName, singleton_tag, Attributes}| T], Tree) ->
  Tag = #tag{name=TagName, attributes = Attributes},
  Tree1 = Tree ++ [Tag],
  build_tree(T, Tree1);
build_tree([_Term = {comments, Comments}| T], Tree)                 ->
  Tree1 = Tree ++ [{comments, Comments}],
  build_tree(T, Tree1);
build_tree([_Term = {text, Text}| T], Tree)                         ->
  Tree1 = Tree ++ [{text, Text}],
  build_tree(T, Tree1).
