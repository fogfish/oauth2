-module(signin).
-compile({parse_transform, category}).

-export([
   main/1
]).

%%
%%
main(Args) ->
   {ok, {Opts, _Files}} = getopt:parse(opts(), Args),
   case 
      lists:member(help, Opts) orelse
      lens:get(lens:pair(oauth2, undefined), Opts) =:= undefined orelse 
      lens:get(lens:pair(client, undefined), Opts) =:= undefined orelse
      lens:get(lens:pair(access, undefined), Opts) =:= undefined orelse
      lens:get(lens:pair(secret, undefined), Opts) =:= undefined

   of
      true ->
         getopt:usage(opts(), escript:script_name(), ""),
         halt(0);
      _    ->
      {ok, _} = application:ensure_all_started(ssl),
      {ok, _} = application:ensure_all_started(knet),
      main(
         lens:get(lens:pair(oauth2), Opts),
         lens:get(lens:pair(client), Opts),
         lens:get(lens:pair(access), Opts),
         lens:get(lens:pair(secret), Opts)
      )
   end.

%%
%%
opts() ->
   [
      {help,      $h,   "help",     undefined,  "Print usage"}
   ,  {oauth2,    $a,   "oauth2",   string,     "Authorization server"}
   ,  {client,    $c,   "client",   string,     "Identity of client application"}
   ,  {access,    $u,   "username", string,     "Username"}
   ,  {secret,    $p,   "password", string,     "Password"}
   ].

%%
%%
main(OAuth2, Client, Access, Secret) ->
   case
      m_http:once(
         [m_state ||
            signin(OAuth2, Client, Access, Secret),
            exchange(OAuth2, Client, _)
         ]
      )
   of
      {ok, Token} ->
         file:write(standard_io, Token),
         halt(0);
      {error, Reason} ->
         file:write(standard_error, io_lib:format("Unable to fetch token ~p~n", [Reason])),
         halt(128)
   end.

signin(OAuth2, Client, Access, Secret) ->
   [m_http ||
      _ > "POST " ++ OAuth2 ++ "/oauth2/signin",
      _ > "Accept: */*",
      _ > "Connection: close",
      _ > "Content-Type: application/x-www-form-urlencoded",
      _ > #{
         access => Access, 
         secret => Secret, 
         response_type => <<"code">>, 
         client_id => Client, 
         state => <<"none">>
      },

      _ < 302,
    Url < "Location: _",
      _ < lens:c(lens:const(Url), lens_uri(fun uri:q/1), lens:pair(<<"code">>))
   ].

lens_uri(Accessor) ->
   fun(Fun, Uri) ->
      lens:fmap(fun(_) -> Uri end, Fun( Accessor(uri:new(Uri)) ))
   end.

exchange(OAuth2, Client, Code) ->
   [m_http ||
      _ > "POST " ++ OAuth2 ++ "/oauth2/token",
      _ > "Accept: */*",
      _ > "Connection: close",
      _ > "Content-Type: application/x-www-form-urlencoded",
      _ > #{
         grant_type => <<"authorization_code">>,
         client_id  => Client,
         code => Code
      },

      _ < 200,
      _ < "Content-Type: application/json",
      _ < lens:at(<<"access_token">>)
   ].
