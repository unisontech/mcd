-module(mcd_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, PeerAddresses} = application:get_env(mcd, peers),
    {ok, 
     {{one_for_one, 10,10}, 
      [
       { mcdCluster, { mcd_starter, start_link, [mcdCluster, PeerAddresses]},
         permanent, infinity, supervisor, [mcd_starter] }
      ]}}.
