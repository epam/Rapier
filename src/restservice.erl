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
%% @doc It is the supervisor for the REST interface.

-module(restservice).

%% API
-export([
    start/0,
    stop/0,
    update_routes/0
]).

-define(APPS, [ssh, ranch, crypto, cowboy, restservice]).

update_routes() ->
    Routes = restservice_app:dispatch_rules(),
    cowboy:set_env(http_listener, dispatch, Routes).

%% ===================================================================
%% API functions
%% ===================================================================

start() ->
    ok = ensure_started(?APPS),
    ok = sync:go(),
    ok.

stop() ->
    sync:stop(),
    ok = stop_apps(lists:reverse(?APPS)).

%% ===================================================================
%% Internal functions
%% ===================================================================

stop_apps([]) -> ok;
stop_apps([App | Apps]) ->
    application:stop(App),
    stop_apps(Apps).

ensure_started([]) -> ok;

ensure_started([App | Apps] = All) ->
	{R, Msg} = case application:start(App) of
		ok -> {ok, "started"};
		{error, {already_started, App}} -> {ok, "already started"};
		{error, {not_started, Dependency}} -> {Dependency, "requires " ++ atom_to_list(Dependency)}
	end,
	io:format("~s ~s~n", [App, Msg]),
	case R of ok -> ensure_started(Apps); D -> ensure_started([D | All]) end.
