%% @doc Top-level module doc.
%% @end
-module(edoc_example2).
-export([g/0,
         p/0,
         pre/0]).

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
