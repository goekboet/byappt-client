(function () {
    function dec2hex(dec) { return ('0' + dec.toString(16)).substr(-2); }

    function generateId(len) {
        var arr = new Uint8Array((len || 40) / 2)
        window.crypto.getRandomValues(arr)

        return Array.from(arr, dec2hex).join('')
    }

    function newSession(port) {
        return function () {
            var session = {
                key: generateId(20),
                nonce: generateId(10)
            };

            sessionStorage.setItem(
                session.key,
                session.nonce
            );

            port.send(session);
        }
    }

    function getSession(port) {
        return function (key) {
            var session = sessionStorage.getItem(key);
            sessionStorage.removeItem(key);

            port.send(session);
        }
    }

    function verifyToken(port) {
        return function (v) {
            var key = KEYUTIL.getKey(v.jwk);
            if (KJUR.jws.JWS.verifyJWT(v.jwt, key, { alg: ["RS256"] })) {
                sessionStorage.setItem("byappt.token", v.jwt);
            }

            port.send(sessionStorage.getItem("byappt.token"));
        }
    }

    function deleteToken(port) {
        return function () {
            var token = sessionStorage.getItem("byappt.token");
            sessionStorage.removeItem("byappt.token");

            port.send(token);
        }
    }

    var fs = {
        oidcEndpoint: {
            clientId: "0oahdv4gzpBZwPM6S0h7",
            authRedirect: "https://local.byappt",
            auth: "https://dev-987804.oktapreview.com/oauth2/default/v1/authorize",
            keys: "https://dev-987804.oktapreview.com/oauth2/default/v1/keys",
            endSession: "https://dev-987804.oktapreview.com/oauth2/default/v1/logout"
        },
        token: sessionStorage.getItem("byappt.token")
    };

    var app = Elm.Main.init({ flags: fs });

    var initSession = app.ports.rememberSession;
    var sessionInit = newSession(app.ports.sessionRemebered);
    initSession.subscribe(sessionInit);

    var sessionGetter = app.ports.recallSession;
    var gotSession = getSession(app.ports.sessionRecalled);
    sessionGetter.subscribe(gotSession);

    var sessionVerification = app.ports.verifyToken;
    var sessionverified = verifyToken(app.ports.tokenVerified);
    sessionVerification.subscribe(sessionverified);

    var forgetToken = app.ports.forgetToken;
    var forgotToken = deleteToken(app.ports.forgotToken);
    forgetToken.subscribe(forgotToken);
})();