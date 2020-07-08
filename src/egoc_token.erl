-module(egoc_token).

-export([
         new/2,
         is_expired/1,
         access_token/1
        ]).

-record(token, {
    expires = 0 :: non_neg_integer(),
    type = undefined :: binary() | undefined,
    token = undefined :: binary() | undefined,
    scopes = [] :: [binary()],
    sub = undefined :: binary() | undefined
}).

-opaque token() :: #token{}.

-type scopes() :: [binary()].

-export_type([token/0]).

-spec new(scopes(), map()) -> token().
new(Scopes, Json) ->
    #token{
       scopes = Scopes,
       type = maps:get(<<"token_type">>, Json),
       token = maps:get(<<"access_token">>, Json),
       expires = os:system_time(seconds) + maps:get(<<"expires_in">>, Json),
       sub = undefined
    }.

-spec is_expired(token()) -> boolean().
is_expired(#token{expires = Expires}) -> do_is_expired(Expires, os:system_time(seconds)).

do_is_expired(TokenExpires, CurrentTime) when TokenExpires < CurrentTime -> true;
do_is_expired(_TokenExpires, _CurrentTime) -> false.

-spec access_token(token()) -> binary().
access_token(#token{ token = Token}) -> Token.
