%%%-------------------------------------------------------------------
%%% @author tkyshm
%%% @copyright (C) 2020, tkyshm
%%% @doc
%%%
%%% @end
%%% Created : 2020-07-08 18:46:28.982975
%%%-------------------------------------------------------------------
-module(egoc_SUITE).

%% API
-export([all/0,
         suite/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         group/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% test cases
-export([
         t_get_token/1,
         t_get_expired_token/1,
         t_get_token_from_metadata/1
        ]).

% -include_lib("proper/include/proper.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

% -define(PROPTEST(M,F), true = proper:quickcheck(M:F())).

all() ->
    [
     {group, test}
    ].

suite() ->
    [{ct_hooks,[cth_surefire]}, {timetrap, {seconds, 30}}].

groups() ->
    [
     {test, [], [
        t_get_token,
        t_get_expired_token,
        t_get_token_from_metadata
     ]}
    ].

%%%===================================================================
%%% Overall setup/teardown
%%%===================================================================
init_per_suite(Config) ->
    logger:set_primary_config(level, debug),
    application:ensure_all_started(cowboy),
    Config.

end_per_suite(_Config) ->
    application:stop(cowboy),
    ok.

%%%===================================================================
%%% Group specific setup/teardown
%%%===================================================================
group(_Groupname) ->
    [].

init_per_group(_Groupname, Config) ->
    Config.

end_per_group(_Groupname, _Config) ->
    ok.


%%%===================================================================
%%% Testcase specific setup/teardown
%%%===================================================================
init_per_testcase(t_get_expired_token, Config) ->
    test_mock:start_server(#{expires => -1}),
    {ok, _} = application:ensure_all_started(egoc),
    Config;
init_per_testcase(t_get_token_from_metadata, Config) ->
    application:unset_env(egoc, file),
    egoc_config:init(),
    test_mock:start_server(#{expires => 3600}),
    {ok, _} = application:ensure_all_started(egoc),
    Config;
init_per_testcase(_TestCase, Config) ->
    test_mock:start_server(#{expires => 3600}),
    {ok, _} = application:ensure_all_started(egoc),
    Config.

end_per_testcase(_TestCase, _Config) ->
    application:stop(egoc),
    ok = test_mock:stop_server(),
    ok.

%%%===================================================================
%%% Individual Test Cases (from groups() definition)
%%%===================================================================

t_get_token(_Config) ->
    Token = egoc:get_token([<<"https://www.googleapis.com/auth/pubsub">>]),
    ?assert(egoc_token:access_token(Token) =:= <<"1/8xbJqaOZXSUZbHLl5EOtu1pxz3fmmetKx9W8CV4t79M">>),
    ?assert(egoc_token:is_expired(Token) =:= false).

t_get_expired_token(_Config) ->
    Token = egoc:get_token([<<"https://www.googleapis.com/auth/pubsub">>]),
    ?assert(egoc_token:access_token(Token) =:= <<"1/8xbJqaOZXSUZbHLl5EOtu1pxz3fmmetKx9W8CV4t79M">>),
    ?assert(egoc_token:is_expired(Token) =:= true).

t_get_token_from_metadata(_Config) ->
    Token = egoc:get_token([<<"https://www.googleapis.com/auth/pubsub">>]),
    ?assert(egoc_token:access_token(Token) =:= <<"1/8xbJqaOZXSUZbHLl5EOtu1pxz3fmmetKx9W8CV4t79M">>),
    ?assert(egoc_token:is_expired(Token) =:= false).
