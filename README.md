# Rapier

## What is Rapier?

This is REST service for OpenFlow switch configuration.

### Features

 * Basic get config from switch: [getconfig][of-c-d]
 * Basic post config to switch: [postconfig][of-c-d]
 * Variety post config to switch: [post][of-c-d]

## How to use it?

### Erlang

If you want to use Rapier you need to have an Erlang runtime installed on your
machine. Required version is R16B02.

### LINC-Switch

You can download&install switch from [LINC-Switch][ofs].

### Install Rapier

1. Make sure that you have started LINC-Switch (for example: localhost)
2. Clone this repo on your own machine
3. `# make`

### Start Rapier

Step by step (if required you must start ssh manually):

    # make run
    # ssh:start().

### Test Rapier

1. Make sure that you have started Rapier and LINC-Switch (for example: localhost)
2. Try to type `curl http://localhost:8008/getconfig`
3. Try to type `curl http://localhost:8008/postconfig` which will post standart config
   with such parameters:
   
   `Controller = {controller, [{id, ["Controller0"]},`
   `                           {'ip-address', ["127.0.0.1"]},`
   `                           {port, ["6633"]}, {protocol, ["tcp"]}]},`
   `Config = {'capable-switch', [{xmlns, "urn:onf:of111:config:yang"}],`
   `          [{id, ["CapableSwitch0"]}, {'logical-switches',`
   `          [{'switch', [{id, ["LogicalSwitch0"]}, {'datapath-id', ["11:11:11:11:11:11:11:11"]},`
   `           {enabled, ["true"]}, {controllers, [Controller]}]}]}]}.`

4. You can use `post` method with such template:
   `ip:port/post/ssh_port/controller/controller_ip/`
   `controller_port/protocol/capable_switch/logical_switch/datapath_id`.
   
   For example:
   `curl http://localhost:8008/post/1830/Controller0/127.0.0.1`
   `/6633/tcp/CapableSwitch0/LogicalSwitch0/21:11:11:11:11:11:11:11`

5. You can use `postjson` method to use JSON notation with such template:
   `{"sshd_port":"ssh_port", "controller_id":"controller", "controller_ip":"controller_ip",`
   `"controller_port":"controller_port", "controller_protocol":"protocol",`
   `"capable_switch":"capable_switch", "logical_switch":"logical_switch",`
   `"datapath":"datapatch_id"}' http://ip:port/postjson`

   For example:
   `curl -d '{"sshd_port":"1830", "controller_id":"Controller0", "controller_ip":"127.0.0.1",`
   `"controller_port":"6633", "controller_protocol":"tcp", "capable_switch":"CapableSwitch0",`
   `"logical_switch":"LogicalSwitch0", "datapath":"11:11:11:11:11:11:11:11"}' http://localhost:8008/postjson`  

## Development environment

1. "[Sync][sync]" - scans all the beam files and their corresponding source
   files and reloads or recompiles them respectively if necessary.
2. Makefile targets.

## Support

If you have any technical questions, problems or suggestions regarding Rapier
please send them to Ihar_Kukharchuk@epam.com or create an Issue. Thanks.

[ofs]: https://github.com/gentigr/LINC-Switch/
[of-c-d]: https://github.com/FlowForwarding/LINC-Switch/wiki/OF-Config-Demo
[sync]: https://github.com/mentels/sync
