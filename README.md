# OpenId Connect implicit flow in Elm
The purpose of this repository is to implement OpenId Connect in elm.

## Background
I've been looking at the [truqu/elm-oauth2](https://package.elm-lang.org/packages/truqu/elm-oauth2/latest/OAuth) packeage as reference. This one is the only one I found that works out of the box on elm 0.19. Since this package deals explicitly with OAuth2 and my aim was to implement implement openId connect, which extends OAuth2 i ran into some issues:
- In the call to the providers /authorize endpoint the nonce value is required but not in OAuth. To conform to the openId spec the nonce needs to:
  1. A different string for each request and random enough to not be guessable
  2. The relaying party id supposed to hold on to the nonce until the browser is redirected back to it. At that time the provider is supposed to include a claim with the key "nonce" with a value that is identical to the value of the nonce parameter in the call to /authorize. The relaying party must validate that these values are identical.

  ## Notes

  To test the implementation I've been using an account with okta. A properly formed request to /authorize looks like this
  ```
  https://dev-987804.oktapreview.com/oauth2/default/v1/authorize
    ?client_id=0oahdv4gzpBZwPM6S0h7 (1)
    &redirect_uri=https%3A%2F%2Flocal.byappt (2)
    &response_type=id_token (3)
    &scope=openid profile(4)
    &state=state-296bc9a0-a2a2-4a57-be1a-d0e2fd9bb601 (5)
    &nonce=foo (6)
  ```
  1. The Id of the application I registered with okta
  2. The url the browser should be redirected to upon completed authentication with the provider. This url must be whitelisted at the provider per application.
  3. Can be either or both token or id_token.
  4. Scope must be openid
  5. The state value is similar to the nonce but repeated in the url of the call to redirect_url and should be verified there
  6. The nonce value as discussed above

  ## Todo

  The goal is to let the application complete the implicit flow with an openId connect provider. To do that we need to
  1. Redirect the browser to the providers /authorize endpoint with a properly formed query-string.
  2. Generate random strings for the nonce and state parameters and store them between redirect. Use ports and localstorage for this.
  3. Parse the url for an id_token and validate nonce and state. [see specs](https://openid.net/specs/openid-connect-core-1_0.html#ImplicitIDTValidation)

The response to the request above looks like this:
```
https://local.byappt/#id_token=ey..&state=someState

Contents of the id_token:
{
  "sub": "a string",
  "ver": "an int",
  "iss": "Url",
  "aud": "My application Id",
  "iat": "unix-timestamp",
  "exp": "unix-timestamp",
  "jti": "a string",
  "amr": [
    "pwd"
  ],
  "idp": "a string",
  "nonce": "someNonce",
  "preferred_username": "a string",
  "given_name": "a string",
  "family_name": "a string",
  "zoneinfo": "a string",
  "updated_at": unix-timestamp,
  "auth_time": unix-timestamp
}
```
