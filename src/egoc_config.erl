-module(egoc_config).

-export([init/0, get/1]).

init() -> do_init(application:get_env(egoc, file)).

do_init({ok, Path}) ->
    try 
        {ok, Binary} = file:read_file(Path),
        Data = jsone:decode(Binary),

        Config = #{
          private_key => maps:get(<<"private_key">>, Data),
          private_key_id => maps:get(<<"private_key_id">>, Data),
          client_email => maps:get(<<"client_email">>, Data),
          client_id => maps:get(<<"client_id">>, Data),
          type => maps:get(<<"type">>, Data),
          project_id => maps:get(<<"project_id">>, Data),
          auth_uri =>  maps:get(<<"auth_uri">>, Data),
          token_uri => maps:get(<<"token_uri">>, Data),
          auth_provider_x509_cert_url => maps:get(<<"auth_provider_x509_cert_url">>, Data),
          client_x509_cert_url => maps:get(<<"client_x509_cert_url">>, Data),
          token_source => oauth_jwt,
          metadata_account => application:get_env(egoc, metadata_account, <<"default">>),
          metadata_url => application:get_env(egoc, metadata_url, <<"http://metadata.google.internal">>),
          endpoint => application:get_env(egoc, endpoint, <<"https://www.googleapis.com">>)
         },


        persistent_term:put(egoc_config, Config)
    of
        Result -> Result
    catch
        _:Reason:Stack ->
            logger:warning("failed to init egoc config: ~p", [Reason]),
            logger:debug("stacktrace: ~p", [Stack]),
            throw({error, failed_to_init_config})
    end;

do_init(undefined) ->
    Config = #{
      token_source => metadata,
      metadata_account => application:get_env(egoc, metadata_account, <<"default">>),
      metadata_url => application:get_env(egoc, metadata_url, <<"http://metadata.google.internal">>),
      endpoint => application:get_env(egoc, endpoint, <<"https://www.googleapis.com">>)
     },
    persistent_term:put(egoc_config, Config).

get(Key) -> maps:get(Key, persistent_term:get(egoc_config), undefined).

