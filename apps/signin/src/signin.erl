-module(signin).
-compile({parse_transform, monad}).

-export([
   main/1,
   exchange/1
]).

%%
%%
main(Args) ->
   {ok, {Opts, Files}} = getopt:parse(opts(), Args),
   case lists:member(help, Opts) of
      true ->
         getopt:usage(opts(), escript:script_name(), ""),
         halt(0);
      _    ->
         init(Opts, Files)
   end.

%%
%%
opts() ->
   [
      {help,   $h,   "help",     undefined,  "Print usage"}
     ,{config, $f,   "config",   string,     "Configuration file"}
     ,{oauth2, $a,   "oauth2",   string,     "Authorization server"}
     ,{client, $c,   "client",   string,     "Identity of client application"}
   ].


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
init(Opts, _Files) ->
   {ok, _} = application:ensure_all_started(restd),
   {ok, _} = restd_service_sup:start_link(signin, restd()),
   OAuth2  = opts:val(oauth2, Opts),
   Client  = opts:val(client, Opts),
   application:set_env(signin, oauth2, OAuth2),
   application:set_env(signin, client, Client),
   os:cmd(lists:flatten(io_lib:format("open '~s/authorize?response_type=code&client_id=~s'", [OAuth2, Client]))),
   timer:sleep(360000).

%%
%%
exchange(Code) ->
   {ok, OAuth2} = application:get_env(signin, oauth2),
   {ok, Client} = application:get_env(signin, client),
   HttpIO = do([m_http ||
      _ /= new(scalar:s(io_lib:format("~s/oauth2/token", [OAuth2]))),
      _ /= x('POST'),
      _ /= h("Accept: */*"),
      _ /= h("Content-Type: application/x-www-form-urlencoded"),
      _ /= h("Connection: close"),
      _ /= d(scalar:s(io_lib:format("grant_type=authorization_code&code=~s&client_id=~s", [Code, Client]))),
      _ /= r(),
      _ =< jsx:decode(scalar:s(tl(_))),
      _ =< io:format("=> ~p~n", [_]),
      return(_)
   ]),
   HttpIO(#{}).

