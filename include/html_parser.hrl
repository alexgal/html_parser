-record(tag, {name       :: tag_name(),
  children   :: list(),
  attributes :: [attribute(),...]
}).

-type html_term()       :: tag() | text() | comments().
-type html_expr()       :: binary().

-type tag_name()        :: binary().
-type tag_type()        :: opening_tag | closing_tag | singleton_tag.
-type tag()             :: {tag_name(), tag_type(), [attribute(),...]} | {tag_name(), tag_type()}.
-type attribute_name()  :: binary().
-type attribute_value() :: binary().
-type attribute()       :: {attribute_name(), attribute_value()}.
-type comments()        :: {comments, binary()}.
-type text()            :: {text, binary()}.


-define(SINGLETON_TAGS, [<<"area">>,<<"base">>, <<"br">>,<<"col">>,<<"command">>,<<"embed">>,<<"hr">>,<<"img">>,
  <<"input">>, <<"link">>, <<"meta">>, <<"param">>, <<"source">>, <<"!DOCTYPE">>]).

%%  <<"div">> omitted from the list since it is quite often used is singleton tag : <div />
-define(POSSIBLY_NON_SINGLETONS, [<<"address">>, <<"article">>, <<"aside">>, <<"blockquote">>, <<"button">>,
  <<"center">>, <<"details">>, <<"dialog">>, <<"dir">>, <<"dl">>, <<"fieldset">>, <<"figcaption">>, <<"figure">>,
  <<"footer">>, <<"header">>, <<"hgroup">>, <<"listing">>, <<"main">>, <<"nav">>, <<"ol">>, <<"pre">>, <<"section">>,
  <<"summary">>, <<"ul">>]).
