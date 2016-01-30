%% @doc Top-level module doc.
%% @end
-module(edoc_example2).
-export([code_in_loose_text/0,
         g/0,
         p/0,
         pre/0,
         text1/0,
         text2/0,
         ul/0]).

-include_lib("docsh/include/pt_docsh.hrl").

%% @doc g() returns a more complex value,
%% while its documentation uses more complex markup.
%%
%% <dt>
%%     <dt>Why?</dt>
%%     <dd>To test the documentation extraction and formatting process.</dd>
%%
%%     <dt>Any other reason?</dt>
%%     <dd>Not really.</dd>
%% </dt>
%%
%% <ul>
%%     <li>Some</li>
%%     <li>Random</li>
%%     <li>Items</li>
%% </ul>
%%
%% <ol>
%%     <li>One</li>
%%     <li>Two</li>
%%     <li>
%%         <p>Three:</p>
%%         <ul>
%%          <li>a</li>
%%          <li>b</li>
%%          <li>c</li>
%%         </ul>
%%     </li>
%% </ol>
%% @end
-spec g() -> any().
g() ->
    Value = fun edoc_example:f/0,
    {some, [more, <<"complex">>, Value]}.

%% @doc <p>
%% Just
%%  a paragraph.
%%  </p>
%% @end
-spec p() -> ok.
p() -> ok.

%% @doc
%% ```
%%   pre
%%     formatted
%%       text
%% '''
%% @end
-spec pre() -> ok.
pre() -> ok.

%% @doc Some
%%  loose text,
%%     not a paragraph.
%% @end
-spec text1() -> ok.
text1() -> ok.

%% @doc Some
%%  loose text,
%%     not a paragraph.
%%
%% <p>A paragraph.</p>
%% @end
-spec text2() -> ok.
text2() -> ok.

%% @doc Fetch the internal state of an OTP process.
%% Calls `sys:get_state/2' directly in R16B01+, and fetches
%% it dynamically on older versions of OTP.
-spec code_in_loose_text() -> ok.
code_in_loose_text() -> ok.

%% @doc
%% <ul>
%%  <li>`Module' is any atom representing a module</li>
%%  <li>`Function' is any atom representing a function, or the wildcard
%%      <code>'_'</code></li>
%%  <li>`Args' is either the arity of a function (`0..255'), a wildcard
%%      pattern (<code>'_'</code>), a
%%      <a href="http://learnyousomeerlang.com/ets#you-have-been-selected">match specification</a>,
%%      or a function from a shell session that can be transformed into
%%      a match specification</li>
%% </ul>
%% @end
-spec ul() -> ok.
ul() -> ok.
