-module(egoc_client).

-behaviour(gen_server).

-export([
         start_link/0,
         get_token/1
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_EXPIRES, 3600).

-type token_source() :: metadata | oauth_jwt.
-type scopes() :: [binary()].

-record(state, { 
          token = undefined :: egoc_token:token() | undefined,
          scopes = [] :: scopes()
         }).

-spec get_token(scopes()) -> egoc_token:token() | {error, any()}.
get_token(Scopes) -> gen_server:call(?MODULE, {get_token, Scopes}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) -> {ok, #state{}}.

handle_call({get_token, Scopes}, _From, #state{token=undefined} = State) ->
    case get_access_token(Scopes) of
        {ok, Token} ->
            {reply, Token, State#state{scopes=Scopes, token=Token}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;
handle_call({get_token, Sc}, _From, #state{scopes=OldSc} = State) when length(Sc) =/= length(OldSc) ->
    case get_access_token(Sc) of
        {ok, Token} ->
            {reply, Token, State#state{scopes=Sc, token=Token}};

        {error, Reason} ->
            {reply, {error, Reason}, State}

    end;
handle_call({get_token, Scopes}, _From, State) ->
    do_get_token(Scopes, State).

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_get_token(Scopes, #state{token=Token} = State) ->
    do_get_token(egoc_token:is_expired(Token), Scopes, State).

do_get_token(true, _OldScopes, #state{scopes=Scopes} = State) -> 
    case get_access_token(Scopes) of
        {ok, NewToken} -> 
            {reply, NewToken, State#state{token=NewToken}};

        {error, Reason} -> 
            {reply, {error, Reason}, State}

    end;
do_get_token(false, OldScopes, #state{token=Token, scopes=Scopes} = State) -> 
    case is_same_scopes(OldScopes, Scopes) of
        true ->
            {reply, Token, State};

        false ->
            case get_access_token(Scopes) of
                {ok, NewToken} ->
                    {reply, NewToken, State#state{token=NewToken}};

                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end.

is_same_scopes(Old, New) ->
    lists:all(fun(X) ->
        lists:any(fun(Y) -> X =:= Y end, Old)
    end, New).

-spec get_access_token(scopes()) -> {ok, egoc_token:token()} | {error, any()}.
get_access_token(Scopes) -> 
    get_access_token(Scopes, []).

-spec get_access_token(scopes(), list()) -> {ok, egoc_token:token()} | {error, any()}.
get_access_token(Scopes, Opts) when is_list(Scopes) and is_list(Opts) ->
    TokenSource = egoc_config:get(token_source),
    get_access_token(TokenSource, Scopes, Opts).

-spec get_access_token(token_source(), scopes(), list()) -> {ok, egoc_token:token()} | {error, any()}.
get_access_token(metadata, Scopes, _Opts) ->
    Headers = [{<<"Metadata-Flavor">>, <<"Google">>}],
    Account = egoc_config:get(metadata_account),
    Metadata = egoc_config:get(metadata_url),
    Endpoint = <<"/computeMetadata/v1/instance/service-accounts/">>,
    URL = <<Metadata/binary, Endpoint/binary, Account/binary, "/token">>,

    case hackney:request(get, URL, Headers, []) of
        {ok, _Status, _Headers, CRef} ->
            {ok, Body} = hackney:body(CRef),
            logger:debug("response: ~p", [Body]),

            Resp = jsone:decode(Body),
            {ok, egoc_token:new(Scopes, Resp)};

        {error, Reason} ->
            logger:warning("resp: ~p", [Reason]),
            {error, failed_to_get_access_token}

    end;

get_access_token(oauth_jwt, Scopes, Opts) ->
    Endpoint = egoc_config:get(endpoint),
    URL = <<Endpoint/binary, "/oauth2/v4/token">>,

    Body = 
    jsone:encode(#{
      assertion =>  gen_jwt_token(Scopes, Opts),
      grant_type => <<"urn:ietf:params:oauth:grant-type:jwt-bearer">>
     }),
    case hackney:request(post, URL, [{"content-type", <<"application/json">>}], Body, []) of
        {ok, _Status, _Headers, CRef} ->
            {ok, RespBody} = hackney:body(CRef),
            logger:debug("response: ~p", [RespBody]),

            Resp = jsone:decode(RespBody),
            {ok, egoc_token:new(Scopes, Resp)};

        {error, Reason} ->
            {error, Reason}
    end.

gen_jwt_token(Scopes, _Opts) ->
    PKey = egoc_config:get(private_key),
    JWK = jose_jwk:from_pem(PKey),

    CEmail = egoc_config:get(client_email),
    Now = os:system_time(seconds),

    Claims = 
    #{
      <<"iat">>    => Now,
      <<"exp">>    => Now + ?DEFAULT_EXPIRES,
      <<"iss">>    => CEmail,
      <<"sub">>    => CEmail,
      <<"aud">>    => <<"https://oauth2.googleapis.com/token">>,
      <<"scope">>  => join_scopes(Scopes)
     },

    JWS = #{
      <<"alg">> => <<"RS256">>,
      <<"typ">> => <<"JWT">>,
      <<"kid">> => egoc_config:get(private_key_id)
     },
    Signed = jose_jwt:sign(JWK, JWS, Claims),
    {_JWS, Token} = jose_jws:compact(Signed),

    Token.

join_scopes([Scope|Scopes]) -> join_scopes(Scope, Scopes).

join_scopes(Acc, []) -> Acc;
join_scopes(Acc, [Scope| Scopes]) ->
    join_scopes(<<Acc/binary, " ", Scope/binary>>, Scopes).

