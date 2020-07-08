%%%-------------------------------------------------------------------
%% @doc egoc public API
%% @end
%%%-------------------------------------------------------------------

-module(egoc).

-behaviour(application).

%% Application callbacks
-export([get_token/1]).
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

-type scopes() :: [binary()].

-spec get_token(scopes()) -> egoc_token:token(). 
get_token(Scopes) -> egoc_client:get_token(Scopes).

start(_StartType, _StartArgs) ->
    egoc_config:init(),
    egoc_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) -> ok.
