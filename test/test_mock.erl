-module(test_mock).
-export([init/2, start_server/1, stop_server/0]).
-define(RESP_HEADERS, #{<<"content-type">> => <<"application/json">>}).

start_server(Config) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/oauth2/v4/token", test_mock, [Config]},
            {"/computeMetadata/v1/instance/service-accounts/default/token", test_mock, [Config]}
		]}
	]),
	{ok, _} = cowboy:start_clear(mock_server, [{port, 8001}], #{
		env => #{dispatch => Dispatch}
	}).

stop_server() ->
	cowboy:stop_listener(mock_server).


init(Req0, Opts) ->
	Method = cowboy_req:method(Req0),
	_HasBody = cowboy_req:has_body(Req0),
    Path = cowboy_req:path(Req0),
    logger:info("PPPPPPPPPPPPPPPPPPPPPPPPPPPPPP ~p",[Path]),
	{ok, handle_req(Method, Path, Req0, Opts), Opts}.

handle_req(<<"POST">>, <<"/oauth2/v4/token">>, Req, Opts) -> 
    cowboy_req:reply(200, ?RESP_HEADERS, make_test_resp(Opts), Req);

handle_req(<<"GET">>, <<"/computeMetadata/v1/instance/service-accounts/default/token">>, Req, Opts) -> 
    cowboy_req:reply(200, ?RESP_HEADERS, make_test_resp(Opts), Req).

make_test_resp([Opts]) ->
    Expires = maps:get(expires, Opts, 3600),
    jsone:encode(
      #{<<"token_type">> => <<"Bearer">>,
        <<"expires_in">> => Expires,
        <<"access_token">> => <<"1/8xbJqaOZXSUZbHLl5EOtu1pxz3fmmetKx9W8CV4t79M">>}).
