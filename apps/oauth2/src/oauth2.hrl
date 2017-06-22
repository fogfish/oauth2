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

%%
%% build-in oauth2 client identity 
-define(OAUTH2_UX,  <<"oauth2ux">>).

%%
%% build-in oauth profile callback
-define(OAUTH2_UX_CALLBACK,  <<"/widget/oauth2.html">>).
-define(OAUTH2_UX_ROOT,      <<"/oauth2/authorize">>).
-define(OAUTH2_UX_URL,       <<"/oauth2/authorize?response_type=code&client_id=oauth2ux">>).