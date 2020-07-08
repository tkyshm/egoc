%%%-------------------------------------------------------------------
%% @doc egoc top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(egoc_sup).

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

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupFlags = #{
      strategy  => rest_for_one,
      intensity => 1000,
      period    => 3600
     },

    Spec = #{
      id       => 'id',
      start    => {'egoc_client', start_link, []},
      restart  => permanent,
      shutdown => 2000,
      type     => worker,
      modules  => ['egoc_client']
     },

    {ok, {SupFlags, [Spec]}}.
