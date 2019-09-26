
%%
%% Authorization Request
%%
%% https://tools.ietf.org/html/rfc6749#section-4.1.1
-record(authorization, {
   response_type = undefined :: binary()
,  client_id     = undefined :: binary()
%% Note: disabled due to security issue
%%       redirect_uri is defined by client reg process 
%% redirect_uri  = undefined :: binary()
,  scope         = undefined :: binary()
,  state         = undefined :: binary()

%%
%% protocol extension to support UX
,  access        = undefined :: binary()
,  secret        = undefined :: binary()
}).
