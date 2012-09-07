-module(mcd_app).

-behaviour(application).
-export([start/2,stop/1]).

start(_Type, _StartArgs) ->
    mcd_sup:start_link().
    %% {ok, PeerAddresses} = application:get_env(mcd, peers),
    %% mcd_starter:start_link(mcdCluster, PeerAddresses).

stop(_State) ->
    ok.
