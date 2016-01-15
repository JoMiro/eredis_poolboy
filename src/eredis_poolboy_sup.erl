%%%-------------------------------------------------------------------
%% @doc eredis_poolboy top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(eredis_poolboy_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Pools = application:get_all_env(eredis_poolboy),
    Pools1 = proplists:delete(included_applications, Pools),
    PoolSpec = lists:map(
        fun ({PoolName, {PoolArgs, MysqlArgs}}) ->
            eredis_poolboy:child_spec(PoolName, PoolArgs, MysqlArgs)
        end,
        Pools1
    ),
    {ok, { {one_for_one, 10, 10}, PoolSpec} }.

%%====================================================================
%% Internal functions
%%====================================================================
