%%------------------------------------------------------------------------------
%% Copyright 2013 epam.com
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%-----------------------------------------------------------------------------

%% @author Ihar Kukharchuk <Ihar_Kukharchuk@epam.com>
%% @copyright 2013 epam.com
%% @doc It is the routes for the REST interface.

-module(restservice_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([dispatch_rules/0]).

dispatch_rules() ->
    cowboy_router:compile([
        {'_', [
            {"/post/[:port]/[:ctrl_id]/[:ctrl_ip]/[:ctrl_port]/[:ctrl_protocol]/[:cf_id]/[:cf_id_logic]/[:cf_datapath_id]", post_handler, []},
            {"/getconfig", getconfig_handler, []},
	    {"/postconfig", postconfig_handler, []},
            {"/postjson" , postjson_handler, []},
	    {'_', notfound_handler, []}
        ]}
    ]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = dispatch_rules(),
    Port = 8008,
    {ok, _} = cowboy:start_http(http_listener, 100,
        [{port, Port}],
        [{env, [{dispatch, Dispatch}]}]
    ),
    restservice_sup:start_link().

stop(_State) ->
    ok.
