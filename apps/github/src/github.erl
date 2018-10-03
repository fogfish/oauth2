%%
%% @doc
%%   GitHub IAM
-module(github).
-compile({parse_transform, category}).

-export([
   auth_url/0,
   account/1
]).

%%
%% build auth url for service provider
-spec auth_url() -> uri:uri().

auth_url() ->
   [option ||
      Root   <- opts:val(oauth_url, undefined, github),
      Scope  <- opts:val(scopes, undefined, github),
      Client <- opts:val(access_key, github),
      uri:new(Root),
      uri:q([{client_id, Client}, {scope, Scope}], _),
      uri:s(_)
   ].

%%
%%
-spec account(_) -> datum:either(_).

account(Url) ->
   [either ||
      Client <- authenticate_oauth_client(Url),


      %% exchange code with github
      cats:optionT(badarg, uri:q(<<"code">>, undefined, Url)),
      authenticate_github_user(_),
      create_account(Client, _)
   ].

%%
authenticate_oauth_client(Url) ->
   [either ||
      %% OAuth2 implements federated accounts for GitHub users
      %% The feature is only available for public application
      %% OAuth2 state flag is used to communicate clientId across the session
      cats:optionT(badarg, uri:q(<<"state">>, undefined, Url)),
      oauth2_client:lookup(_),
      oauth2_client:is_public(_)
   ].

%%
authenticate_github_user(Code) ->
   case (request_github_api(Code))(#{}) of
      [{Access, undefined} | State] ->
         lager:warning("[github] unauthorized attempt from ~s", [Access]),
         [knet:close(Sock) || Sock <- maps:values(maps:without([req, ret, so], State))],
         {error, unauthorized};

      [{Access, Source} | State] ->
         lager:notice("[github] access to ~s is derived from ~s", [Access, Source]),
         [knet:close(Sock) || Sock <- maps:values(maps:without([req, ret, so], State))],         
         {ok, #{access => Access, master => scalar:s(opts:val(org, github))}}
   end.

request_github_api(Code) ->
   [m_state ||
      Token   <- github_access_token(Code),
      User    <- github_user_profile(Token),
      Access  <- allows_access_contributor(Token),
      cats:unit({User, Access})
   ].

allows_access_contributor(Token) ->
   fun(State0) ->
      case (github_user_orgs(Token))(State0) of
         [undefined | State1] ->
            (github_user_contribution(Token))(State1);

         [_ | _] = State1 ->
            State1
      end
   end.   

%%
%%
github_access_token(Code) ->
   [m_http ||
      cats:new( url(token_url) ),
      cats:so(#{active => true}),
      cats:method('POST'),
      cats:header("Connection", "keep-alive"),
      cats:header("Content-Type", "application/x-www-form-urlencoded"),
      cats:header("Transfer-Encoding", "chunked"),
      cats:payload( access_token_request(Code) ),
      cats:request(),
      cats:require(content, lens:at(<<"access_token">>))
   ].

%%
%%
github_user_profile(Token) ->
   [m_http ||
      cats:new( uri:segments([user], url(ghapi_url)) ),
      cats:so(#{active => true}),
      cats:method('GET'),
      cats:header("Connection", "keep-alive"),
      cats:header("Accept", "application/json"),
      cats:header("User-Agent", "knet"),
      cats:header("Authorization", <<"Bearer ", Token/binary>>),
      cats:request(),
      cats:require(content, lens:at(<<"login">>))
   ].

%%
%%
github_user_orgs(Token) ->
   [m_http ||
      cats:new( uri:segments([user, orgs], url(ghapi_url)) ),
      cats:so(#{active => true}),
      cats:method('GET'),
      cats:header("Connection", "keep-alive"),
      cats:header("Accept", "application/json"),
      cats:header("User-Agent", "knet"),
      cats:header("Authorization", <<"Bearer ", Token/binary>>),
      cats:request(),
      cats:require(content, 
         lens:c(
            lens:takewith(fun allowed_org/1, #{}), 
            lens:at(<<"login">>)
         )
      )
   ].

%%
%%
github_user_contribution(Token) ->
   [m_http ||
      cats:new( uri:q([{affiliation, collaborator}], uri:segments([user, repos], url(ghapi_url))) ),
      cats:so(#{active => true}),
      cats:method('GET'),
      cats:header("Connection", "keep-alive"),
      cats:header("Accept", "application/json"),
      cats:header("User-Agent", "knet"),
      cats:header("Authorization", <<"Bearer ", Token/binary>>),
      cats:request(),
      cats:require(content,
         lens:c(
            lens:takewith(fun allowed_contrib/1, #{}), 
            lens:at(<<"name">>)
         )         
      )
   ].

allowed_org(#{<<"login">> := Org}) ->
   Org =:= scalar:s(opts:val(org, github)).

allowed_contrib(#{<<"name">> := Name}) ->
   [identity ||
      scalar:s(opts:val(contrib, github)),
      binary:split(_, <<$,>>, [global]),
      lists:member(Name, _)
   ].

access_token_request(Code) ->
   #{
      client_id => opts:val(access_key, github),
      client_secret => opts:val(secret_key, github),
      code => Code
   }.

%%
%%
create_account(Client, #{access := Access, master := Master}) ->
   Secret = crypto:strong_rand_bytes(30),
   case permit:lookup(Access) of
      {error, not_found} ->
         %% token here
         permit:create(Access, Secret, #{
            <<"type">>   => <<"oauth2:account">>,
            <<"uid">>    => true,
            <<"master">> => Master
         });

      {ok, _} ->
         permit:update(Access, Secret, #{
            <<"type">>   => <<"oauth2:account">>,
            <<"uid">>    => true,
            <<"master">> => Master
         })
   end,
   case 
      oauth2_token:exchange_code(Access, Secret)
   of
      {ok, Code} ->
         redirect_uri([{code, Code}], Client);
      {error, _} = Error ->
         redirect_uri([Error], Client)
   end.


%%
%%
url(Key) ->
   uri:new(opts:val(Key, github)).

%%
redirect_uri(Status, Client) ->
   oauth2_client:redirect_uri(Client, Status).

