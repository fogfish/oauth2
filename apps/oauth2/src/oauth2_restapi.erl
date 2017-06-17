%% @doc
%%   common restapi utility
-module(oauth2_restapi).
-compile({parse_transform, category}).

-export([
   access_token/1,
   validate_client_type/1,
   validate_redirect_uri/1
]).

%%
%%
-spec access_token(_) -> _ | undefined.

access_token(Head) ->
   case lens:get(lens:pair('Authorization', undefined), Head) of
      <<"Bearer ", Token/binary>> ->
         Token;
      _ ->
         undefined
   end.


%%
%% 2.1. Client Types
-spec validate_client_type(map()) -> {ok, map()} | {error, _}.

validate_client_type(#{<<"type">> := <<"public">>} = Req) ->
   {ok, Req};
validate_client_type(#{<<"type">> := <<"confidential">>} = Req) ->
   {ok, Req};
validate_client_type(_) ->
   {error, invalid_type}.

%%
%%
-spec validate_redirect_uri(map()) -> {ok, map()} | {error, _}.

validate_redirect_uri(#{<<"redirect_uri">> := Uri} = Req) ->
   [either ||
      fmap( uri:new(Uri) ),
      assert_some(fun uri:schema/1, _),
      assert_some(fun uri:authority/1, _),
      assert_some(fun uri:path/1, _),
      assert_none(fun uri:q/1, _),
      assert_none(fun uri:anchor/1, _),
      fmap(Req)
   ].


%%-----------------------------------------------------------------------------
%%
%% private
%%
%%-----------------------------------------------------------------------------


assert_some(Fun, Uri) ->
   case Fun(Uri) of
      undefined ->
         {error, invalid_uri};
      _ ->
         {ok, Uri}
   end.

assert_none(Fun, Uri) ->
   case Fun(Uri) of
      undefined ->
         {ok, Uri};
      _ ->
         {error, invalid_uri}
   end.
