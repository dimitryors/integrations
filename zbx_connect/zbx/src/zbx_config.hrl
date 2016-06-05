-record(config, {
    running_interval = 15000                                             :: integer(),
    atsd_api_url = "http://5.8.180.71:8088/api/v1/series/insert"         :: string(),
    atsd_type = "application/json; charset=UTF-8"                        :: string(),
    atsd_auth = [{"Authorization","Basic Y29sbGVjdG9yOmNvbGxlY3Rvcg=="}] :: list(),
    zabbix_api_url = "http://5.8.180.68/zabbix/api_jsonrpc.php"          :: string(),
    zabbix_type = "application/json-rpc"                                 :: string(),
    zabbix_auth = []                                                     :: list()
}).