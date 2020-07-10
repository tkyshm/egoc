egoc
=====

egoc is a simple library to generate and retrieve OAuth2 tokens for Google Cloud Services (GCP).

It can either do tokens using service account credential and from Google's metadata service on GCP.

## How to use

- configures
```erlang
% In case to use service account credential file (JSON file)
[
  {egoc, [
    {file, <<"./google-credential.json">>}
  ]}
].

% In case to get from Google's metadata service on GCP, you not have to set anything.
[
  {egoc, []}
].

```

- get oauth2 access token
```erlang
_> Token = egoc:get_token([<<"https://www.googleapis.com/auth/pubsub">>]).
_> egoc_token:access_token(Token).
_> egoc_token:is_expire(Token).
```

[google oauth2 scopes](https://developers.google.com/identity/protocols/oauth2/scopes)
