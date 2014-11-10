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
%% @doc Module for get_config implementation.


-module(getconfig_handler).
-behaviour(cowboy_http_handler).

%% Cowboy_http_handler callbacks
-export([
    init/3,
    handle/2,
    terminate/3
]).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req, State) ->
    {ok, C} = enetconf_client:connect("localhost", [{port, 1830}, {user, "linc"}, {password, "linc"}]),
    {ok, X} = enetconf_client:get_config(C, running),
    Body = [X],
    {ok, _} = enetconf_client:close_session(C),
    {ok, Req4} = cowboy_req:reply(200, [], Body, Req),
    {ok, Req4, State}.

terminate(_Reason, _Req, _State) ->
    ok.
