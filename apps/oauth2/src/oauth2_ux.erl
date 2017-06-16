%% @doc
%%   build an oauth2 user experience
-module(oauth2_ux).
-compile({parse_transform, category}).

-export([
   signin/1,
   signup/1
]).

%%
%% render sign-in user experience
-spec signin([_]) -> {ok, binary()} | {error, _}.

signin(Env) ->
   build_ux(oauth2_signin, Env).

%%
%% render sign-up user experience
-spec signup([_]) -> {ok, binary()} | {error, _}.

signup(Env) ->
   build_ux(oauth2_signup, Env).

%%
%%
build_ux(Mod, Env) ->
   [either ||
      oauth2_req:issue_access_code(Env),
      fmap(build_ux_error(_)),
      build_ux_spec(_),
      Mod:ux(_)
   ].

build_ux_spec(Env) ->
   %% @todo: protect template from injections
   {ok, #{ux => [{scalar:atom(Key), Val} || {Key, Val} <- maps:to_list(Env)]}}.

build_ux_error(Env) ->
   %% @todo: move errors localization into template
   Lens = lens:map(<<"error">>, undefined),
   case lens:get(Lens, Env) of
      undefined ->
         Env;
      <<"unauthorized">> ->
         lens:put(Lens, <<"Invalid credentials.">>, Env);
      _ ->
         lens:put(Lens, <<"System error, try later.">>, Env)
   end.
