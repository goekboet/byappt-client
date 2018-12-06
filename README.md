# OpenId Connect implicit flow in Elm
The purpose of this repository is to implement OpenId Connect in elm.

## Background
I've been looking at the [truqu/elm-oauth2](https://package.elm-lang.org/packages/truqu/elm-oauth2/latest/OAuth) packeage as reference. This repository does not aim to be a elm package but rather a complete implementation of a user-agent implicit flow. This requires using ports and needs to be integrated into the architecture of the application it's providing authentication for.

## Goals

### Sign in
- Create well-formed redirect to oidc-provider
- Parse the response
- Verify that the response is to the signin-request we initiated. (1) Before redirect create an entry in session-storage using the value of the state-parameter as key. (2) Upon oidc response recall the state. If we have such an entry it contains the nonce-value we use to validate we got the right jwt.
- Using the kid-property of the jwt dynamically fetch the public key using the oidc-providers keys endpoint.
- Verifying that the jwt with the jwk.

### Signed in
- A public page with elements that are hidden if user is not signed in
- Subpages that requires that the user is signed in to get to. 



