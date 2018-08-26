%%
%%   Copyright 2017 Dmitry Kolesnikov, All Rights Reserved
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%
-module(oauth2_sup).
-behaviour(supervisor).
-include_lib("public_key/include/public_key.hrl").

-export([
   start_link/0, init/1
]).

-define(CHILD(Type, I),            {I,  {I, start_link,   []}, permanent, 5000, Type, dynamic}).
-define(CHILD(Type, I, Args),      {I,  {I, start_link, Args}, permanent, 5000, Type, dynamic}).
-define(CHILD(Type, ID, I, Args),  {ID, {I, start_link, Args}, permanent, 5000, Type, dynamic}).

%%-----------------------------------------------------------------------------
%%
%% supervisor
%%
%%-----------------------------------------------------------------------------

start_link() ->
   supervisor:start_link({local, ?MODULE}, ?MODULE, []).
   
init([]) ->   
   {ok,
      {
         {one_for_one, 4, 1800},
         [
            storage_pubkey(),
            % tls_config(),
            restapi()
         ]
      }
   }.


%%
%%
storage_pubkey() ->
   ?CHILD(supervisor, pts, storage_pubkey_spec()).

storage_pubkey_spec() ->
   Storage = opts:val(storage, "ephemeral://", oauth2),
   Backend = backend_pubkey_spec(uri:schema(uri:new(Storage))),   
   [permit,
      [
         'read-through',
         {factory, temporary},
         {entity,  {Backend, start_link, [Storage]}}
      ]
   ].

backend_pubkey_spec([ddb, _]) ->
   oauth2_ddb_pubkey;
backend_pubkey_spec(_) ->
   permit_pubkey_io.

%%
%%
restapi() ->
   restd:spec(
      oauth2_restapi:endpoints(), 
      [
         {port, opts:val(port, oauth2)}, 
         {backlog, 1024},
         {sock, so(uri:new(opts:val(port, oauth2)))}
      ]
   ).


so({uri, http, _}) ->
   [];

so({uri, https, _}) ->
   [
      {certfile, certificate()},
      {keyfile, private_key()}
   ].


certificate() ->
   File = filename:join([code:priv_dir(oauth2), "certificate.pem"]),
   {ok, Steam} = s3am:fetch(opts:val(tls_certificate, oauth2)),
   ok = file:write_file(File, stream:list(Steam)),
   File.


private_key() ->
   File = filename:join([code:priv_dir(oauth2), "private_key.pem"]),
   {ok, Steam} = s3am:fetch(opts:val(tls_private_key, oauth2)),
   ok = file:write_file(File, stream:list(Steam)),
   File.


