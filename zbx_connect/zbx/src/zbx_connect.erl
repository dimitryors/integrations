-module(zbx_connect).
-behaviour(gen_server).
-export([req_zabbix/0, start_link/0,request_zbx_api/1,json_config/0,http_api/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Import Config
-include_lib("zbx_config.hrl").

%% Types Declarations

-type	json_text()			::	binary().
-type	token()					::	json_text().
-type hosts_list()		::	[
														{
															Id 	:: binary(),
															Srv :: binary()
														},
														...].

-type items_list()		::	[
														{
															HostId 		:: binary(),
															Metric 		:: binary(),
															LastValue :: binary(),
															LastTs 		:: binary()
														},
														...].



-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%
% Call App
%%

-spec req_zabbix() -> any().

req_zabbix() ->
		gen_server:call(?MODULE, {req_zabbix}).

%%
% Initializing Program
%%

-spec init([]) -> {'ok',0}.

init([]) ->
	process_flag(trap_exit, true)
	,	Config = #config{}
	, io:format("~p starting~n" ,[?MODULE])
	, inets:start()
	, timer:apply_interval(Config#config.running_interval, ?MODULE, req_zabbix, [])
	,	{ok, 0}.

%%
%	Handling Request
%%

-spec handle_call(
									Call	::	{'req_zabbix'},
									From	::	any(),
									State	::	number()
	) -> {reply, Result	:: any(), NewState :: number()}.

handle_call({req_zabbix}, _From, N) ->
	  {ReqToken, ReqHosts, ReqItems} = json_config()
	, {ok, Token} = request_zbx_api({token, ReqToken})
	, {ok, Hosts} = request_zbx_api({hosts, ReqHosts, Token})
	, {ok, Items} = request_zbx_api({items, ReqItems, Token})
	, {ok, Result} = post_atsd_api(Hosts, Items)
	,	{reply, Result, N+1}.
	

handle_cast(_Msg, N) -> {noreply, N}.
handle_info(_Info, N) -> {noreply, N}.

terminate(_Reason, _N) ->
	io:format("~p stopping~n" ,[?MODULE]),
	ok.

code_change(_OldVsn, N, _Extra) ->
	{ok, N}.


%%
% Functions
%%

-spec http_api({
	Way :: atsd | zbx,
	Body :: json_text()
}) -> {	ok, Result :: any() } | { error, Reason :: any() }.


http_api({atsd, Body}) ->
		Config = #config{}
	, Method = post
	, URL = Config#config.atsd_api_url
	, Header = Config#config.atsd_auth
	, Type = Config#config.atsd_type
	, Body = Body
	, HTTPOptions = []
	, Options = []
	, httpc:request(Method, {URL, Header, Type, Body}, HTTPOptions, Options);
http_api({zbx, Body}) ->
		Config = #config{}
	, Method = post
	, URL = Config#config.zabbix_api_url
	, Header = Config#config.zabbix_auth
	, Type = Config#config.zabbix_type
	, Body = Body
	, HTTPOptions = []
	, Options = []
	, httpc:request(Method, {URL, Header, Type, Body}, HTTPOptions, Options).


%%
%	Get Config from Json
%%

-spec json_config() -> {
		Token		:: list(),
		Hosts		:: list(),
		Items		:: list()
	}
	|	ok.

% Find Json files in ./json directory
json_config() ->
		GetJsonFiles = filelib:wildcard("json/*.{json,JSON}")
	, JsonBin = [file:read_file(X) || X <- GetJsonFiles]
	, JsxJsonBin = [ jsx:decode(X, [stream]) || {ok,X} <- JsonBin]
	, IncompleateJsonFun = [ X || {incomplete, X} <- JsxJsonBin ]
	, json_config(IncompleateJsonFun, [], [], []).

% Put each method to it's own List
json_config([H | T],Token, Hosts, Items) ->
		Json = H(end_stream)

	,	case [Y || {X,Y} <- Json, X=:=<<"method">>] of
			[<<"user.login">>] ->
				json_config(T, [Json | Token], Hosts, Items)

	; 		[<<"host.get">>] ->
				json_config(T, Token, [Json | Hosts], Items)

	;		[<<"item.get">>] ->
				json_config(T, Token, Hosts, [Json | Items])

	;		_Any -> ok
		end;

% Return Lists with configurations
json_config([], Token, Hosts, Items) -> {Token, Hosts, Items}.


%%
%	Get Zabbix Data
%%

-spec request_zbx_api(
				{
					RequestedObject :: token,
					JsonBody				:: list()
				}
			|	{
					RequestedObject	::	hosts,
					JsonBody				:: 	list(),
					Token						::	token()
				}
			| {
					RequestedObject	::	items,
					JsonBody				:: 	list(),
					Token						::	token()
				}
	) ->
					{ok, Result :: json_text()}
				|	{ok, Result	:: hosts_list()}
				|	{ok, Result	:: items_list()}
				|	 ok.


% Request Zabbix for Token
request_zbx_api({token, [H|_T]}) ->
		RequestBody = jsx:encode(H)

	, case http_api({zbx, RequestBody}) of
			{ok, {{"HTTP/1.1",200,"OK"},_,ResZabbixRequest}} ->
					Result = list_to_binary(string:substr(ResZabbixRequest,28,32))
				,	{ok, Result}

	;		_Any -> ok
		end;

% Request Zabbix for Hosts List
request_zbx_api({hosts, [H|_T], Token}) ->
		MapJson = maps:from_list(H)
	,	MapToken = MapJson#{<<"auth">> := Token}
	,	RequestBody = jsx:encode(MapToken)

	, case http_api({zbx, RequestBody}) of
			{ok, {{"HTTP/1.1",200,"OK"},_,ResZabbixRequest}} ->
					Bin = list_to_binary(ResZabbixRequest)
				,	JsonBin = jsx:decode(Bin)
				, JsonResult = [Y || {X,Y} <- JsonBin, X=:=<<"result">>]
				, [JsonResult1] = JsonResult
				,	Result = [ {Y1,Y2} || [{_X1,Y1},{_X2,Y2}] <- JsonResult1]
				, {ok , Result}

	;	_Any -> ok
		end;

% Request Zabbix for Items
request_zbx_api({items, [H|_T], Token}) ->
		MapJson = maps:from_list(H)
	,	MapToken = MapJson#{<<"auth">> := Token}
	,	RequestBody = jsx:encode(MapToken)

	, case http_api({zbx, RequestBody}) of
			{ok, {{"HTTP/1.1",200,"OK"},_,ResZabbixRequest}} ->
					Bin = list_to_binary(ResZabbixRequest)
				,	JsonBin = jsx:decode(Bin)
				, [JsonResult] = [Y || {X,Y} <- JsonBin, X=:=<<"result">>]
				, Items = collect_items(JsonResult)
				, {ok , Items}

	;		_Any -> ok
		end.


-spec collect_items( ItemsList :: list() ) -> items_list().

%	Collect Items, return only required fields
collect_items(ItemsList) -> collect_items(ItemsList, []).
collect_items([H|T], Items) ->
		[Metric] = [ re:replace(Y, "\\s+", "", [global,{return,binary}]) || {X,Y} <- H, X=:=<<"key_">> ]
	,	[LastValue] = [ Y || {X,Y} <- H, X=:=<<"lastvalue">> ]
	,	[LastTs] = [ Y || {X,Y} <- H, X=:=<<"lastclock">> ]
	,	[HostId] = [ Y || {X,Y} <- H, X=:=<<"hostid">> ]
	,	collect_items(T, [{HostId, Metric, LastValue, LastTs} | Items]);
collect_items([], Items) -> Items.


%%
%	Send Metrics to Axibase Time Series Database (ATSD)
%%

-spec	post_atsd_api(
										Hosts		::	hosts_list(),
										Items		::	items_list()
	) -> {ok, Result :: [json_text(),...]}.


post_atsd_api(Hosts, Items) ->
		MetricList = [
			jsx:encode([[
				  {<<"entity">>,re:replace(Srv, "\\s+", "", [global,{return,binary}])}
				, {<<"metric">>, Metric}
				, {<<"data">>,[[{<<"t">>,integer_to_binary(binary_to_integer(LastTs)*1000)}
				, {<<"v">>,LastValue}]]}
			]])
			|| 
			{HostId, Metric, LastValue, LastTs} <- Items,
			{Id, Srv} <- Hosts, 
			Id =:= HostId
		]
		%%, {ok, Result} = pmap(fun(I) -> http_api({atsd, I}) end, MetricList)
		,	{ok, Result} = pmap(fun(I) -> I end, MetricList)
		,	{ok, Result}.


-spec	pmap(
		Fun			::	fun(),
		List		::	list()
) -> {ok, Result :: list()}.

% Spawn process for each element in List
pmap(Fun, List) ->
	S = self()
	, Ref = erlang:make_ref()
	, lists:foreach(fun(I) ->
		spawn( fun() -> do_f(S, Ref, Fun, I) end) end, List)
	, Result = gather(length(List), Ref, [])
	, {ok, Result}.

% Apply function for element and send result to Parent process
do_f(Parent, Ref, F, I) ->
	Parent ! {Ref, (catch F(I))}.

% Gathering result of processes in a List
gather(0, _, L) -> L;
gather(N, Ref, L) ->
	receive
		{Ref, Ret} -> gather(N-1, Ref, [Ret|L])
	end.