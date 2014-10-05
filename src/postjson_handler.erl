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
%% @doc Module postjson_config implementation.

-module(postjson_handler).

-export([
    init/3,
    content_types_provided/2,
    json_provided/2,
    content_types_accepted/2,
    post_config/2,
    allowed_methods/2
]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"HEAD">>, <<"POST">>, <<"OPTIONS">>], Req, State}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, json_provided}
    ], Req, State}.

json_provided(Req, State) ->
    Body = <<"{\"json\":\"provided\"}">>,
    {Body, Req, State}.

content_types_accepted(Req, State) ->
    {[{'*', post_config}], Req, State }.

post_config(Req, State) ->
    {ok, Body, Req1} = cowboy_req:body(Req),
    {struct, Object} = mochijson:decode(Body),
    [{"sshd_port", Sshd_port}, {"controller_id", Ctrl_id}, {"controller_ip", Ctrl_ip}, {"controller_port", Ctrl_port}, {"controller_protocol", Ctrl_protocol}, {"capable_switch", Cf_id}, {"logical_switch", Cf_id_logic}, {"datapath", Cf_datapath_id}] = Object,
    Controller = {controller, [{id, [Ctrl_id]},
                       {'ip-address', [Ctrl_ip]},
			       {port, [Ctrl_port]}, {protocol, [Ctrl_protocol]}]},
    Config = {'capable-switch', [{xmlns, "urn:onf:of111:config:yang"}],
             [{id, [Cf_id]}, {'logical-switches',
             [{'switch', [{id, [Cf_id_logic]}, {'datapath-id', [Cf_datapath_id]},
			  {enabled, ["true"]}, {controllers, [Controller]}]}]}]},
    {ok, C} = enetconf_client:connect("localhost", [{port, list_to_integer(Sshd_port)}, {user, "linc"}, {password, "linc"}]),
    {ok, _} = enetconf_client:edit_config(C, running, {xml, Config}),
    {ok, _} = enetconf_client:close_session(C),
    Json = <<"{\"config\": \"was send\"}">>,
    Req3 = cowboy_req:set_resp_body( Json, Req1 ),
    { true, Req3, State }.
