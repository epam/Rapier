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
%% @doc Module for post implementation.

-module(post_handler).
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
    {Bindings, _} = cowboy_req:bindings(Req),
    [{cf_datapath_id, Cf_datapath_id}, {cf_id_logic, Cf_id_logic}, {cf_id, Cf_id}, {ctrl_protocol, Ctrl_protocol}, {ctrl_port, Ctrl_port}, {ctrl_ip, Ctrl_ip}, {ctrl_id, Ctrl_id}, {port, Sshd_port}] = Bindings,

    Controller = {controller, [{id, [binary_to_list(Ctrl_id)]},
                       {'ip-address', [binary_to_list(Ctrl_ip)]},
		       {port, [binary_to_list(Ctrl_port)]}, {protocol, [binary_to_list(Ctrl_protocol)]}]}, 
    Config = {'capable-switch', [{xmlns, "urn:onf:of111:config:yang"}],
             [{id, [binary_to_list(Cf_id)]}, {'logical-switches',
             [{'switch', [{id, [binary_to_list(Cf_id_logic)]}, {'datapath-id', [binary_to_list(Cf_datapath_id)]},
             {enabled, ["true"]}, {controllers, [Controller]}]}]}]},

    {ok, C} = enetconf_client:connect("localhost", [{port, list_to_integer(binary_to_list(Sshd_port))}, {user, "linc"}, {password, "linc"}]),
    {ok, Y} = enetconf_client:edit_config(C, running, {xml, Config}),
    Body = [Y],
    {ok, _} = enetconf_client:close_session(C),
    {ok, Req2} = cowboy_req:reply(200, [], Body, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.
