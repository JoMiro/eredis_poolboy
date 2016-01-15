%%%-------------------------------------------------------------------
%%% @author jiangxiaowei@lilith.sh
%%% @copyright (C) 2016, Lilith Games
%%% @doc
%%%
%%% @end
%%% Created : 15. 一月 2016 14:23
%%%-------------------------------------------------------------------
-module(eredis_poolboy).
-author("jiangxiaowei@lilith.sh").

%% API
-export([add_pool/3, checkin/2, checkout/1, child_spec/3]).
-export([q/2, q/3, qp/2, qp/3, q_noreply/2]).

%% @doc Adds a pool to the started mysql_poolboy application.
add_pool(PoolName, PoolArgs, MysqlArgs) ->
    %% We want strategy fifo as default instead of lifo.
    PoolSpec = child_spec(PoolName, PoolArgs, MysqlArgs),
    supervisor:start_child(eredis_poolboy_sup, PoolSpec).

%% @doc Returns a mysql connection to the given pool.
checkin(PoolName, Connection) ->
    poolboy:checkin(PoolName, Connection).

%% @doc Checks out a mysql connection from a given pool.
checkout(PoolName) ->
    poolboy:checkout(PoolName).

%% @doc Creates a supvervisor:child_spec. When the need to
%% supervise the pools in another way.
child_spec(PoolName, PoolArgs, MysqlArgs) ->
    PoolArgs1 = case proplists:is_defined(strategy, PoolArgs) of
                    true  ->
                        [{name, {local, PoolName}}, {worker_module, eredis} | PoolArgs];
                    false ->
                        %% Use fifo by default. MySQL closes unused connections after a certain time.
                        %% Fifo causes all connections to be regularily used which prevents them from
                        %% being closed.,
                        [{strategy, fifo}, {name, {local, PoolName}}, {worker_module, eredis} | PoolArgs]
                end,
    poolboy:child_spec(PoolName, PoolArgs1, MysqlArgs).

q(PoolName, Command) ->
    poolboy:transaction(PoolName, fun(RedisConn) ->
        eredis:q(RedisConn, Command)
    end).

q(PoolName, Command, TimeOut) ->
    poolboy:transaction(PoolName, fun(RedisConn) ->
        eredis:q(RedisConn, Command, TimeOut)
    end).

qp(PoolName, Command) ->
    poolboy:transaction(PoolName, fun(RedisConn) ->
        eredis:qp(RedisConn, Command)
    end).

qp(PoolName, Command, TimeOut) ->
    poolboy:transaction(PoolName, fun(RedisConn) ->
        eredis:qp(RedisConn, Command, TimeOut)
    end).

q_noreply(PoolName, Command) ->
    poolboy:transaction(PoolName, fun(RedisConn) ->
        eredis:q_noreply(RedisConn, Command)
    end).