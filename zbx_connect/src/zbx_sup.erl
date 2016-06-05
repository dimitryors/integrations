%%%-------------------------------------------------------------------
%%% @author dmitriy_orshanskiy
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. май 2016 1:08
%%%-------------------------------------------------------------------
-module(zbx_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_in_shell_for_testing/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_in_shell_for_testing() ->
    {ok, Pid} = supervisor:start_link({local,?MODULE},
        ?MODULE, _Arg = []),
    unlink(Pid).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
        MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
        [ChildSpec :: supervisor:child_spec()]
    }} |
    ignore |
    {error, Reason :: term()}).

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 10000,
    Type = worker,

    AChild = {zbx_connect, {zbx_connect, start_link, []},
        Restart, Shutdown, Type, [zbx_connect]},

    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
