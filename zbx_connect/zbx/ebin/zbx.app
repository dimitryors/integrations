{application,zbx,
             [{description,"Zabbix Api Connector"},
              {vsn,"1"},
              {modules,[zbx_app,zbx_connect,zbx_sup]},
              {registered,[zbx_connect,zbx_sup]},
              {applications,[kernel,stdlib]},
              {mod,{zbx_app,[]}},
              {env,[]}]}.
