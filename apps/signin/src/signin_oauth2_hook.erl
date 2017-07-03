-module(signin_oauth2_hook).


-export([
   allowed_methods/1,
   content_provided/1, 
   'GET'/3
]).

%%
allowed_methods(_Req) ->
   ['GET'].

%%
content_provided(_Req) ->
   [{text, html}].


%%
%% 
'GET'(_Type, _Req, {Uri, _Head, _Env}) ->
   signin:exchange(lens:get(lens:pair(<<"code">>), uri:q(Uri))),
   {ok, <<"
      <!DOCTYPE HTML>
      <html lang=\"en-US\">
         <head>
            <title>Authentication Successful.</title>
            <style>
            body 
            {
               font-family: monospace;
            }
            </style>
         </head>
         <body>
            <p>You are now authenticated with authorization server.</p>
            <p>The authentication flow has completed. You may close this window.</p>
         </body>
      </html>
   ">>}.
