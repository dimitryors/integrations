#Zabbix Connect#


an erlang application for transfering data from Zabbix to ATSD using API.

**Zabbix Connect** is built via [rebar][rebar]


## quickstart ##

#### to set and build the application ####

Add your Servers setting to **src/zbx_config.hrl** and build application

```bash
$ rebar get-deps
$ rebar compile
```

#### run application ####

Folder **json** include queries for the Zabbix API. Before you start add **json** folder to the root folder of application.
Add your Zabbix credentials to this query **json/get_auth.json** 

```erlang
application:load(zbx).
application:start(zbx).
application:stop(zbx).
```
