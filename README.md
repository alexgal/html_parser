# html_parser
Erlang html parser. Very simple, good as example for studying the Erlang language. 

## Initial run 
```bash
erlc  -I include/ -o ebin/ src/*; erl -pa ebin/
```
##Usage Examples 
```erlang
Html = <<"<doctype >
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
      </html>">>,
Tree = html_parser:parse(Html),
tree_lookup:first_tag(<<"script">>, Tree), % will match {ok, matched tag}
tree_lookup:tag_with_classes([<<"classa">>], Tree), % will match {ok, [mathed tag]}
tree_lookup:tag_with_classes([<<"some-other-class">>], Tree). % will result in {not_found, []}
```

