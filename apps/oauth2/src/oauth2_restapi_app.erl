-module(oauth2_restapi_app).
-compile({parse_transform, category}).

-export([
   allowed_methods/1,
   content_provided/1, 
   content_accepted/1,
   authorize/2,
   'GET'/3,
   'DELETE'/3,
   'POST'/3
]).

%%
allowed_methods(_Req) ->
   ['GET', 'POST'].

%%
content_provided(_Req) ->
   [{application, json}].

%%
content_accepted(_Req) ->
   [{application, 'x-www-form-urlencoded'}].

%%
authorize(_Mthd, {_Uri, Head, _Env}) ->
   %% @todo: make knet to accept {ok, _} | ok
   case access_token(Head) of
      undefined ->
         {error, unauthorized};
      Token ->
         case permit:validate(Token) of
            {ok, _} ->
               ok;
            {error, _} = Error ->
               Error
         end
   end.

%%
'GET'(_Type, _Req, {_Uri, Head, _Env}) ->
   [either ||
      permit:validate(access_token(Head)),
      fmap(lens:get(lens:map(<<"sub">>), _)),
      find_all_access(_),
      fmap(jsx:encode(_))
   ].

find_all_access(Master) ->
   pts:call(permit, Master, {match, <<"master">>, [{<<"master">>, Master}]}).

%%
'POST'(_Type, Req, {_Uri, Head, _Env}) ->
   [either ||
      fmap(permit_oauth2:decode(Req)),
      check_app_spec(_),
      register_app(access_token(Head), _)
   ].

check_app_spec(Req) ->
   [either ||
      fmap(maps:with([<<"redirect_uri">>, <<"type">>], Req)),
      check_app_type(Req),
      check_app_redirect_uri(_)
   ].

check_app_type(#{<<"type">> := <<"public">>} = Req) ->
   {ok, Req};
check_app_type(#{<<"type">> := <<"confidential">>} = Req) ->
   {ok, Req};
check_app_type(_) ->
   {error, invalid_type}.

check_app_redirect_uri(#{<<"redirect_uri">> := Redirect} = Req) ->
   [either ||
      fmap( uri:new(Redirect) ),
      is_defined(fun uri:schema/1, _),
      is_defined(fun uri:authority/1, _),
      is_defined(fun uri:path/1, _),
      is_not_defined(fun uri:q/1, _),
      is_not_defined(fun uri:anchor/1, _),
      fmap(Req)
   ].

is_defined(Fun, Uri) ->
   case Fun(Uri) of
      undefined ->
         {error, invalid_uri};
      _ ->
         {ok, Uri}
   end.

is_not_defined(Fun, Uri) ->
   case Fun(Uri) of
      undefined ->
         {ok, Uri};
      _ ->
         {error, invalid_uri}
   end.

register_app(Token, Spec) ->
   [either ||
      permit:pubkey(Token, [oauth2client]),
      % create_app(_, Spec),
      fmap(jsx:encode(_))
   ].

create_app(#{<<"access">> := Access}, Spec) ->
   pts:put(oauth2profile, Access, Spec#{<<"access">> => Access}).      

'DELETE'(_Type, _Req, {_Uri, Head, Env}) ->
   %% find user by id from token
   %% validate that user owns resource
   %% delete the resource
   ok.

%%
%%
access_token(Head) ->
   case lens:get(lens:pair('Authorization', undefined), Head) of
      <<"Bearer ", Token/binary>> ->
         Token;
      _ ->
         undefined
   end.
