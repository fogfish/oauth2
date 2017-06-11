-module(oauth2_restapi_app).
-compile({parse_transform, category}).

-export([
   allowed_methods/1,
   content_provided/1, 
   content_accepted/1,
   authorize/2,
   'POST'/3
]).

%%
allowed_methods(_Req) ->
   ['POST'].

%%
content_provided(_Req) ->
   [{application, json}].

%%
content_accepted(_Req) ->
   [{application, 'x-www-form-urlencoded'}].

%%
authorize(_Mthd, {_Uri, Head, _Env}) ->
   case lens:get(lens:pair('Authorization', undefined), Head) of
      <<"Bearer ", Token/binary>> ->
         case permit:validate(Token) of
            {ok, _} ->
               ok;
            {error, _} = Error ->
               Error
         end;
      _ ->
         {error, unauthorized}
   end.   

%%
'POST'(_Type, _Req, {_Uri, Head, _Env}) ->
   <<"Bearer ", Token/binary>> = lens:get(lens:pair('Authorization', undefined), Head),
   [either ||
      permit:pubkey(Token, [oauth2client]),
      fmap(jsx:encode(_))
   ].
