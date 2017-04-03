/* Social network logins and analytics */

console.log(" _____                                        ________" +
            "___ \n|  __ \\                                      |_" +
            "   ___   _|\n| |__) |___  _ __ ___   __ _ _ __   ___ _" +
            "__    | |   | |  \n|  _  // _ \\| '_ ` _ \\ / _` | '_ " +
            "\\ / __/ _ \\   | |   | |  \n| | \\ \\ (_) | | | | | |" +
            " (_| | | | | (_|  __/  _| |_ _| |_ \n|_|  \\_\\___/|_|" +
            " |_| |_|\\__,_|_| |_|\\___\\___| |___________|\n\nRoma" +
            "nce Ⅱ Game System");

window.romance = (function(){
    var gameState = {
        googleUser: false,
        facebookUser: false,
        oauthUser: false
    };
    window.gameState = gameState;
    return {
        googleLoggedInP: function() {
            return !! gameState.googleUser },
        facebookLoggedInP: function() {
            return !! gameState.facebookUser },
        oauthLoggedInP: function() {
            return !! gameState.oauthUser },
        loggedInP: function() {
            return (romance.googleLoggedInP() ||
                    romance.facebookLoggedInP() ||
                    romance.oauthLoggedInP()); },
        clearFacebookUser: function() {
            gameState.facebookUser = false },
        facebookUserRefusedSignIn: function () {
            ga('send', 'event', 'Facebook Sign-In', 'Refused', 'Refused');
        },
        userIsNoSignedInToFacebook: function() {
            ga('send', 'event', 'Facebook Sign-In', 'Not Signed In',
               'Not Signed In');
            romance.clearFacebookUser();
        },
        facebookSignInChanged: function (response) {
            console.log ('facebook-sign-in-changed');
            console.log (response);
            switch (response.status) {
            case 'connected':
                romance.finishFacebookSignIn ();
                break;
            case 'not_authorized':
                romance.facebookUserRefusedSignIn();
                break;
            default:
                romance.userIsNotSignedInToFacebook ();
            };
            romance.gameStatusUpdate();
        },
        checkLogInState: function () {
            FB.getLoginStatus(romance.facebookSignInChanged);
        },
        signInToFacebook: function() {
            ga('send','event','Facebook Sign-In','Start','Start');
            FB.login ({ 'scope': 'public_profile,email' },
                      romance.facebookSignInChanged );
        },
        facebookCantLoadMe: function(response) {
            console.log(response);
            romance.clearFacebookUser();
            romance.setHTML('facebook-name', '');
            romance.setSrc('facebook-photo', null);
            ga('send','event','Facebook Sign-In', 'Error',
               'After sign-in cannot load /me');
        },
        facebookGotMe: function(response) {
            var name = response.name_format;
            console.log("got successful FB login for: " +
                        name);
            console.log(response);
            romance.setHTML('facebook-name', name);
            romance.setSrc('facebook-photo',
                           response.picture.data.url);
            ga('send','event','Facebook Sign-In', 'Success',
               'Success');
            gameState.facebookUser = response;
        },
        finishFacebookSignIn: function () {
            FB.api(
                '/me',
                { 'fields': 'id,name_format,picture,email' },
                function (response) {
                    if (response.error) {
                        romance.facebookCantLoadMe(response);
                    } else {
                        romance.facebookGotMe(response);
                    }
                    romance.gameStatusUpdate();
                });},
        setHTML: function(id, html) {
            document.getElementById(id).innerHTML = html;
        },
        setSrc: function(id, src) {
            document.getElementById(id).src = src;
        },
        elementStyle: function(id) {
            return document.getElementById(id).style;
        },
        showOverlayLayer: function () {
            romance.elementStyle(gameState.overlayActive).display = 'block';
        },
        blockNone: function(flag) {
            if (flag) { return 'block' } else { return 'none' }
        },
        elementDisplayBlockP: function(id) {
            return (romance.elementStyle(id) == 'block');
        },
        setElementDisplayBlock: function(id, blockP) {
            romance.elementStyle(id).display =
                romance.blockNone(blockP);
        },
        hideLoginOverlay: function() {
            if ('none' != romance.elementStyle('login-overlay')) {
                gameState.overlayActive = 'game-welcome';
            }
            romance.setElementDisplayBlock('login-overlay', false);
        },
        showLoginOverlay: function() {
            ga('send','screenview', {'screenName': 'login-overlay'});
            romance.setElementDisplayBlock('login-overlay', true);
            romance.setElementDisplayBlock('game-welcome', false);
        },
        signInLayersToggle: function(service, signedInP) {
            romance.setElementDisplayBlock(
                service + '-signed-in',
                romance.blockNone(signedInP));
            romance.setElementDisplayBlock(
                service + '-not-signed-in',
                romance.blockNone(! signedInP));
        },
        updateLoginOverlay: function() {
            if (! romance.loggedInP() ) {
                console.log('user not signed in');
                gameState.overlayActive = 'login-overllay';
                romance.updateSignInLayers();
            }
            if (!! gameState.overlayActive) {
                romance.showOverlayLayer();
            } else {
                romance.hideLoginOverlay();
            }
        },
        updateSignInLayers: function() {
            romance.signInLayersToggle (
                'google',
                romance.googleLoggedInP());
            romance.signInLayersToggle (
                'facebook',
                romance.facebookLoggedInP());
        },
        updateOverlay: function() {
            var overlayActive = gameState.overlayActive;
            if (overlayActive) {
                romance.setActiveOverlay(overlayActive);
            } else {
                console.log('Game play mode active');
            }
        },
        notifyAboutMartian: function(connection) {
            alert("Martian message from connection " +
                  connection);
            console.log("Martian message from connection " +
                        connection);
            console.log(connection);
        },
        gibberishHandler: function(connection,relability,message) {
            alert("Gibberish message labeled “" + message.v + "”");
            console.log("Gibberish message label " + message.v);
            console.log(connection);
            console.log(reliability);
            console.log(message);
        },
        findMessageHandler: function(message) {
            return (message.v &&
                    romance.dispatchers[message.v] ) ||
                romance.gibberishHandler;
        },
        dispatchPeerMessage: function(connection,
                                      reliability,message) {
            if (window.serverInfo.peers[connection.id]) {
                ( romance.findMessageHandler(message)
                )(connection, reliability, message );
            } else {
                romance.notifyAboutMartian(connection);
            }
        },
        peerAcceptConnection: function(connection) {
            window.serverInfo.peers[connection.id] = connection;
            connection.ondisconnect = function () {
                delete window.serverInfo.peers[connection.id];
            }
            connection.onmessage = function (label,message) {
                romance.dispatchPeerMessage(connection,label,message);
            }
        },
        setChannelEvents: function(connection,channel,name) {
            channel.onmessage = function(event) {
                var data = JSON.parse(event.data);
                romance.dispatchPeerMessage(data);
            };
            channel.onopen = function() {
                channel.push = channel.send;
                channel.send = function (data) {
                    channel.push(JSON.stringify(data));
                };
            };
            channel.onerror = function(event) {
                console.error('Error in channel ' + name, JSON.stringify(event, null, '\t'));
            };
            channel.onclose = function(event) {
                if (gameState.selfPeers[name] == connection)
                { delete gameState.selfPeers[name]; }
            };
        },
        makeChannel: function (connection, name) {
            var channel = connection.createDataChannel(name);
            romance.setChannelEvents(connection,channel,name);
            gameState.selfPeers[name] = connection;
            return channel;
        },
        startFirstPeer: function () {
            var iceServers = {iceServers:[{url: 'stun:stun.l.google.com:19302'},
                                          {url: 'stun:23.21.150.121'}]};
            var firstPeer = new RTCPeerConnection(iceServers);
            firstPeer.onicecandidate = function(event) {
                if (event.candidate) {
                    romance.advertiseICE(event.candidate); }}
            firstPeer.ondatachannel = function(event) {
                romance.setChannelEvents(firstPeer,event.channel,'?FIXME?');
                romance.gossip({v:'i-am',nick:romance.currentPlayerNick()});
            };
            var gossipChannel = romance.MakeChannel(firstPeer, 'System/Gossip');
            firstPeer._$gossipChannel = gossipChannel;
            firstPeer.createOffer(function(sdp) {
                firstPeer.setLocalDescription(sdp);
                romance.advertiseOffer(sdp);
            });
        },
        gossip: function(datum) {
            window.serverInfo.peers.each(function(peer){
                var channel = peer._$gossipChannel;
                if (channel) {
                    channel.send(datum);
                }
            });
        },
        advertiseOffer: function(candidate) {
            var xhr = new XMLHttpRequest();
            romance.xhrSetUp(xhr, 'PUT', '/action/gossip');
            xhr.send('google-api-token=' + romance.googleAPIToken() + '&sdp=' + encodeURIComponent(sdp));
        },
        updatePeers: function() {
            if ( (! gameState.selfPeers) ||
                 0 == gameState.selfPeers.length) {
                romance.startFirstPeer();
            }
        },
        dispatchers: {
            alert: function(con,reli,message) {
                alert(message.text || '«no text»');
            }
        },
        gameStatusUpdate: function() {
            console.log('game-status-update');
            romance.updateLoginOverlay();
            romance.updateSignInLayers();

            if (gameState.playerInfo) {
                romance.updatePeers();
            } else {
                gameState.overlayActive = 'game-welcome';
                console.log('waiting for directory server reply');
                return;
            }
            romance.updateOverlay();
        },
        googleSignInFailed: function () {
            alert("Google sign-in failed. Please try again.");
            ga('send','event','Google Sign-In','Failed','Failed');
            romance.gameStatusUpdate();
        },
        googleSignInButton: function () {
            console.log('Google API loaded. Need a sign-in button?');
            romance.gameStatusUpdate();
            var auth = gapi.auth2.getAuthInstance();
            if(auth.isSignedIn) {
                console.log("Guess not! Let's fix the game state.");
                romance.gotGoogleSignIn(auth.currentUser.get());
            } else {
                console.log('Draw the sign-in button');
                return gapi.signin2.render(
                    'my-signin2',
                    {'scope': 'profile email',
                     'longtitle': true,
                     'theme': 'dark',
                     'onsuccess': romance.gotGoogleSignIn,
                     'onfailure': romance.googleSignInFailed });}},
        makeNewPlayer: function () {
            var nickname = 'Bubba';
            ga('send','event','Game Sign-In','Make New Player', nickname);
            return { 'nickname': nickname,
                     'hitPoints': 10,
                     'maxHitPoints': 10,
                     'inventory': [
                         { 'name': 'Blue Jeans',
                           'id': 0x1000,
                           'equip': 'pants',
                           'equippedP': true,
                           'durability': 1,
                           'repair': 1,
                           'maxRepair': 1 } ] };
        },
        gotUserSignin: function(response) {
            console.log("Sign in to game mesh now; received directory");
            if (response) { console.log(response); }
            ga('send', 'event', 'Game Sign-In', 'Join Mesh', 'Join Mesh');
            var rando = Math.floor(Math.random(10000));
            gameState.playerInfo = { 'id': response.playerID,
                                     'nickname': response.nickname };
            romance.gameStatusUpdate();
        },
        googleAPIToken: function () {
            return ( gameState.googleUser &&
                     gameState.googleUser.getAuthResponse('id_token') );
        },
        signInToGame: function () {
            console.log("Sign in to game mesh now; get directory");
            var xhr = new XMLHttpRequest();
            ga('send','event','Game Sign-In','Start','Start');
            romance.xhrSetUp(xhr, 'PUT', '/action/gossip', romance.gotUserSignin);
            xhr.send('google-api-token=' + romance.googleAPIToken());
        },
        quitFromGame: function() {
            var xhr = new XMLHttpRequest();
            ga('send','event','Game Sign-In','Sign Out','Sign Out');
            romance.xhrSetUpPost(xhr, '/action/quit', function(response) {
                gameState.playerInfo = false;
                romance.gameStatusUpdate ();
            });
            xhr.send('google-api-token=' + romance.googleAPIToken());
        },
        quitFromGoogle: function() {
            ga('send','event','Google Sign-In','Sign Out','Sign Out');
            gapi.auth2.getAuthInstance().signOut(function () {
                gameState.googleUser = false;
                romance.gameStatusUpdate();
            });
        },
        quitFromFacebook: function () {
            ga('send','event','Facebook Sign-In','Sign Out','Sign Out');
            FB.logout();
        },
        disconnectMeshLink: function (link) {
        },
        quitFromMesh: function() {
            if (! gameState.meshLinks) { return; }
            ga('send','event','Mesh','Disconnect','Disconnecting');
            gameState.meshLinks.each(romance.disconnectMeshLink);
        },
        quitWorker: function() {},
        quit: function () {
            if (! ( (!! gameState.playerInfo) ||
                    (romance.loggedInP ()) ) ) {
                romance.gameStatusUpdate();
            }
            if (!! gameState.playerInfo) { romance.quitFromGame(); }
            if (!! gameState.googleUser) { romance.quitFromGoogle(); }
            if (!! gameState.facebookUser) {
                romance.quitFromFacebook(); }
            if (!! gameState.meshLinks) { romance.quitFromMesh(); }
            if (!! gameState.worker) { romance.quitWorker(); }
        },
        gotGoogleSignIn: function(user) {
            console.log("Got Google sign-in", user);
            var wantSignInP = false;
            if (! romance.loggedInP ()) {
                wantSignInP = true;
            }
            gameState.googleUser = user;
            if (wantSignInP) {
                romance.signInToGame();
            }
            romance.gameStatusUpdate();
        },
        networkError: function(condition) {
            alert("A network error occurred.\n"+
                  "Check Internet connection.");
            console.log("network error");
            console.log(condition);
        },
        xhrReportErrors: function(xhr) {
            xhr.onerror = romance.networkError;
        },
        serverURL: function (suffix) {
            return window.serverInfo.url + suffix;
        },
        xhrSetUp: function(xhr,verb,uri,onLoad) {
            xhr.open(verb, romance.serverURL(uri));
            xhr.setRequestHeader('Accept', 'application/json');
            xhr.setRequestHeader('Content-Type',
                                 'application/x-www-form-urlencoded');
            xhr.onload = onLoad;
            romance.xhrReportErrors(xhr);
        },
        currentPlayerID: function() {
            return gameState.playerInfo && gameState.playerInfo.id;
        },
        currentPlayerNick: function() {
            return gameState.playerInfo &&
                gameState.playerInfo.nickname;
        }
    };
})();

if ( ! window.romance ) {
    console.error("Could not load Romance Ⅱ module");
    alert("Unable to load the game software. Perhaps you need to update your web browser to the latest Firefox?");
}

console.log("Main library of functions has " + (Object.keys(romance).length) + " members.");

/* Facebook API */
window.fbAsyncInit = function() {
    FB.init({ appId: '1265378050154137',
              xfbml: true,
              version: 'v2.7'});
};

// (function(d,s,id){
//     var js, fjs = d.getElementsByTagName(s)[0];
//     if (d.getElementById(id)) {return;}
//     js = d.createElement(s);
//     js.id = id;
//     js.src = "//connect.facebook.net/en_US/sdk.js";
//     fjs.parentNode.insertBefore(js,fjs);
// } (document,'script','facebook-jssdk'));

(function(d,s,id){
    var js, fjs = d.getElementsByTagName(s)[0];
    if(d.getElementById(id))return;
    js=d.createElement(s);
    js.id = id;
    js.src = "//connect.facebook.net/en_US/sdk.js#xfbml=1&version=v2.7&appId=1265378050154137";
    fjs.parentNode.insertBefore(js,fjs);
})(document,'script','facebook-jssdk');

/* Google Analytics */
(function(){
    var googleAnalytics = window['ga'] || function() {
        (ga['q'] = ga['q'] || []).push(arguments) };
    ga = function() {
        console.log.apply(console.log, ["Analytics:"].concat(arguments));
        googleAnalytics.apply(googleAnalytics, arguments);
    };
    ga["l"] = +new Date;
    ga('create', 'UA-80917352-1', 'auto');
    ga('set', 'transport', 'beacon');
    ga('set','appName','tootstest');
    ga('set','appVersion','0.0.2');
    if (romance.currentPlayerID()) {
        ga('set', 'userId', romance.currentPlayerID());
    }
    ga('require', 'linkid');
    ga('require', 'eventTracker');
    ga('require', 'outboundLinkTracker');
    ga('require', 'urlChangeTracker');
    ga('send', 'pageview');
})();

window.googleSignInButton = romance.googleSignInButton;
window.gotGoogleSignIn = romance.gotGoogleSignIn;
window.gotFacebookSignin = romance.gotFacebookSignin;
