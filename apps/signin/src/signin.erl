-module(signin).
-compile({parse_transform, monad}).

-export([
   main/1
]).

%%
%%
main(Args) ->
   {ok, {Opts, Files}} = getopt:parse(opts(), Args),
   case 
      lists:member(help, Opts) orelse
      lens:get(lens:pair(oauth2, undefined), Opts) =:= undefined orelse 
      lens:get(lens:pair(client, undefined), Opts) =:= undefined
   of
      true ->
         getopt:usage(opts(), escript:script_name(), ""),
         halt(0);
      _    ->
         main(Opts, Files)
   end.

%%
%%
opts() ->
   [
      {help,   $h,   "help",     undefined,  "Print usage"}
     ,{oauth2, $a,   "oauth2",   string,     "Authorization server"}
     ,{client, $c,   "client",   string,     "Identity of client application"}
   ].

%%
%%
main(Opts, _Files) ->
   start(
      opts:val(oauth2, undefined, Opts),
      opts:val(client, undefined, Opts)
   ).

start(OAuth2, Client) ->
   {ok, _} = application:ensure_all_started(lager), 
   lager:set_loglevel(lager_console_backend, critical),
   {ok, _} = application:ensure_all_started(restd),
   {ok, _} = restd_service_sup:start_link(signin, restd()),
   os:cmd(lists:flatten(io_lib:format("open '~s/authorize?response_type=code&client_id=~s'", [OAuth2, Client]))),
   erlang:register(signin, self()),
   receive
      {code, Code} ->
         exchange(OAuth2, Client, Code)
   after 300000 ->
      io:format("Service is not available~n"),
      halt(128)
   end.

%%
%%
restd() ->
   [
      {port,  "http://*:8421"}
     ,{route, [
         {"/hook",   signin_oauth2_hook}
      ]}
   ].

%%
%%
exchange(OAuth2, Client, Code) ->
   HttpIO = do([m_http ||
      _ /= new(scalar:s(io_lib:format("~s/oauth2/token", [OAuth2]))),
      _ /= x('POST'),
      _ /= h("Accept: */*"),
      _ /= h("Content-Type: application/x-www-form-urlencoded"),
      _ /= h("Connection: close"),
      _ /= d(scalar:s(io_lib:format("grant_type=authorization_code&code=~s&client_id=~s", [Code, Client]))),
      _ /= r(),
      _ =< jsx:decode(scalar:s(tl(_))),
      _ =< io:format("~s~n", [lens:get(lens:pair(<<"access_token">>), _)]),
      return(_)
   ]),
   HttpIO(#{}).

