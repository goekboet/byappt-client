(function () {
    function dec2hex(dec) { return ('0' + dec.toString(16)).substr(-2); }

    function generateId(len) {
        var arr = new Uint8Array((len || 40) / 2)
        window.crypto.getRandomValues(arr)

        return Array.from(arr, dec2hex).join('')
    }

    var fs = {
        oidcEndpoint: {
            clientId: "0oahdv4gzpBZwPM6S0h7",
            authRedirect: "https://local.byappt",
            auth: "https://dev-987804.oktapreview.com/oauth2/default/v1/authorize",
        },
        bookingsAuth: sessionStorage.getItem("byappt.bookingsAuth")
    };

    var app = Elm.Main.init({ flags: fs });

    function setState(port) {
        return function ( v ) {
            var handle = generateId(20);

            sessionStorage.setItem(
                handle,
                JSON.stringify(v)
            );

            port.send(handle);
        }
    }

    app.ports.setState.subscribe(
        setState(app.ports.stateSet));

    function getState(port) {
        return function (key) {
            var session = sessionStorage.getItem(key);
            sessionStorage.removeItem(key);

            port.send(session);
        }
    }

    app.ports.getState.subscribe(
        getState(app.ports.gotState));

    function cacheAuthFgmt() {
        return function (fgmt) {
            sessionStorage.setItem(
                "byappt.bookingsAuth", 
                JSON.stringify(fgmt));
        }
    }

    app.ports.cacheAuthFgmt.subscribe(
        cacheAuthFgmt()
    )
})();