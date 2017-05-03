/* Social network logins and analytics */

window.r2note = function (kind,event,detail,size) {
    console.info("Event: Kind (" + kind + ") event (" + event + ") detail (" + detail +
                 (size ? (") size (" + size + ")") : ")"));
    ga('send','event',kind,event,detail,size);
    Rollbar.info(kind + "; " + event + "; " + detail +
                 (size ? "; " + size : ""),
                 { kind: kind, event: event, detail: detail, size: size });
    if (FB) {
        var fbParams = {};
        fbParams[ FB.AppEvents.ParameterNames.DESCRIPTION ] = detail;
        FB.AppEvents.logEvent(kind + " _ " + event,
                              size, fbParams);
    }
}

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
        oauthUser: false,
        peers: []
    };
    var gameAnalyticsKeys = {
        host: 'https://sandbox-api.gameanalytics.com/',
        gameKey: '5c6bcb5402204249437fb5a7a80a4959', // sandbox
        secretKey: '16813a12f718bc5c620f56944e1abc3ea13ccbac'
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
            r2note('Facebook Sign-In', 'Refused', 'Refused');
        },
        userIsNoSignedInToFacebook: function() {
            r2note('Facebook Sign-In', 'Not Signed In',
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
            FB &&
                FB.getLoginStatus(romance.facebookSignInChanged);
        },
        signInToFacebook: function() {
            if (! FB) {
                throw "No FB object (at sign-in time)";
            }
            r2note ('Facebook Sign-In','Start','Start');
            FB.login ({ 'scope': 'public_profile,email' },
                      romance.facebookSignInChanged );
        },
        facebookCantLoadMe: function(response) {
            console.log(response);
            romance.clearFacebookUser();
            romance.setHTML('facebook-name', '');
            romance.setSrc('facebook-photo', null);
            r2note ('Facebook Sign-In', 'Error',
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
            r2note ('Facebook Sign-In', 'Success',
                    'Success');
            gameState.facebookUser = response;
        },
        finishFacebookSignIn: function () {
            if (! FB) {
                throw "FB object gone (finishing sign-in)";
            }
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
            el = document.getElementById(id);
            return el && el.style;
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
        setActiveOverlay: function (overlay) {
            gameState.overlayActive = overlay;
            for (layer in {'login-overlay':0,
                           'game-welcome':0
                          }) {
                romance.setElementDisplayBlock(layer,
                                               (layer == overlay));
            }
            romance.setElementDisplayBlock('login-overlay-coppa',
                                           ('login-overlay' == overlay))
            // romance.setElementDisplayBlock ("overlay", gameState.overlayActive);
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
                signedInP);
            romance.setElementDisplayBlock(
                service + '-not-signed-in',
                (! signedInP));
        },
        updateLoginOverlay: function() {
            if (! romance.loggedInP() ) {
                console.log('user not signed in');
                gameState.overlayActive = 'login-overlay';
                romance.updateSignInLayers();
            }
            if (gameState.overlayActive) {
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
            if (gameState.peers[connection.id]) {
                ( romance.findMessageHandler(message)
                )(connection, reliability, message );
            } else {
                romance.notifyAboutMartian(connection);
            }
        },
        peerAcceptConnection: function(connection) {
            gameState.peers[connection.id] = connection;
            connection.ondisconnect = function () {
                delete gameState.peers[connection.id];
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
                if (gameState.peers[name] == connection)
                { delete gameState.peers[name]; }
            };
        },
        makeChannel: function (connection, name) {
            var channel = connection.createDataChannel(name);
            romance.setChannelEvents(connection,channel,name);
            gameState.peers[name] = connection;
            return channel;
        },
        advertiseSDP: function(description) {
            r2note ('Game Sign-In','SDP Offer','Advertise');
            var xhr = new XMLHttpRequest();
            console.log("SDP offer: ", description);
            romance.xhrSetUp(xhr, 'PUT', '/action/gossip',
                             romance.gotUserSignin);
            xhr.send('google-api-token=' + romance.googleAPIToken() +
                     '&sdp=' +
                     encodeURIComponent(JSON.stringify(description)),
                     romance.checkForAnswer);
        },
        startFirstPeer: function () {
            if (gameState.peers['first']) {
                throw "Already have firstPeer";
            }
            var firstPeer = new RTCPeerConnection(serverInfo.iceServers);
            firstPeer.onicecandidate = function(event) {
                if (event.candidate) {
                    console.log("Candidate in ",event);
                    firstPeer.addIceCandidate(event.candidate).
                        then(function() {
                            console.log("Candidate added");
                        }, function(e) {
                            console.error("Could not add candidate", e);
                        }); }
                else { console.error ("No candidate in ", event) }}
            firstPeer.ondatachannel = function(event) {
                romance.setChannelEvents(firstPeer,event.channel,'?FIXME?');
                romance.gossip({v:'i-am',nick:romance.currentPlayerNick()});
            };
            var gossipChannel = romance.makeChannel(firstPeer, 'System/Gossip');
            firstPeer._$gossipChannel = gossipChannel;
            firstPeer.createOffer().then(function(sd) {
                firstPeer.setLocalDescription(sd);
                romance.advertiseSDP(sd);});
            console.log("Starting first peer; waiting for ICE candidates");
            gameState.peers['first'] = firstPeer;
            return firstPeer;
        },
        checkForAnswer: function (update) {
            var response = update.currentTarget.response;
            if (response && response.answer) {
                var answer =  JSON.parse(decodeURIComponent(answer.replace(/\+/g,'%20')));
                var rendezvous = gameState.peers["Rendezvous"];
                rendezvous.setRemoteDescription(answer);
                gameState.peers[gameState.peers.length + 1] = rendezvous;
                gameState.peers["Rendezvous"] = null;
                updatePeers();
            }
        },
        acceptPeerOffer: function (peerSDP) {
            var newPeer = new RTCPeerConnection(serverInfo.iceServers);
            newPeer.ondatachannel = function(event) {
                romance.setChannelEvents(firstPeer,event.channel,'?FIXME?');
                romance.gossip({v:'i-am',nick:romance.currentPlayerNick()});
            };
            var gossipChannel = romance.makeChannel(newPeer, 'System/Gossip');
            newPeer._$gossipChannel = gossipChannel;
            newPeer.setRemoteDescription(peerSDP);
            console.log("Trying to reach peer through offer received");
            newPeer.createAnswer().then (
                function(sd) {
                    newPeer.setLocalDescription(sd);
                    gameState.peers[sd] = newPeer;
                    var xhr = new XMLHttpRequest();
                    console.log("SDP answer: ", sd);
                    romance.xhrSetUp(xhr, 'POST', '/action/gossip/answer' );
                    xhr.send('google-api-token=' + romance.googleAPIToken() +
                             '&offeror=' + (encodeURIComponent(JSON.stringify(peerSDP))) +
                             "&answer=" +
                             encodeURIComponent(JSON.stringify(sd.toJSON())));
                }
            );
        },
        gossip: function(datum) {
            Object.keys(gameState.peers).forEach(function(key) {
                var peer = gameState.peers[key];
                if ('_$gossipChannel' in peer) {
                    peer._$gossipChannel.send(datum);
                }
            });
        },
        updatePeers: function() {
            if ( (! gameState.peers) ||
                 0 == gameState.peers.length) {
                gameState.peers = {};
                romance.startFirstPeer();
            }
            else {
                var connected = 0;
                Object.keys(gameState.peers).forEach(function(peer) {
                    if (! peer._$gossipChannel) {
                        console.warn("Peer has no Gossip channel", peer);
                    } else {
                        if (peer._$gossipChannel.readyState == 'open')
                        { ++connected; }
                    }
                });
                if (connected < 1) { // FIXME: MinPeers
                    romance.startFirstPeer();
                }
            }
            setTimeout(1000, romance.updatePeers);
        },
        dispatchers: {
            alert: function(con,reli,message) {
                alert(message.text || '«no text»');
            }
        },
        gameStatusUpdate: function() {
            console.log('game-status-update');
            romance.updateSignInLayers();
            romance.updateLoginOverlay();

            if (gameState.playerInfo) {
                romance.updatePeers();
            } else {
                romance.setActiveOverlay('game-welcome');
                console.log('not signed in to mesh yet');
                return;
            }
            romance.updateOverlay();
        },
        googleSignInFailed: function () {
            alert("Google sign-in failed. Please try again.");
            r2note ('Google Sign-In','Failed','Failed');
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
            r2note ('Game Sign-In','Make New Player', nickname);
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
        gotUserSignin: function(update) {
            console.log("update", update);
            var response = JSON.parse(update.currentTarget.response);
            if (!response) {
                console.error("No response to sign-in yet");
                return;
            }
            console.log("Sign in to game mesh now; received user sign-in");
            console.log(response);
            r2note('Game Sign-In', 'Join Mesh', 'Join Mesh');
            romance.setPlayerInfo ({ 'id': response.id,
                                     'nickname': response.nickname });
            if (response.offers.length > 0) {
                console.log("Got peer offer × " +
                            response.offers.length);
                response.offers.forEach(function(sdp) {
                    var remote = JSON.parse(decodeURIComponent(sdp.replace(/\+/g,'%20')));
                    console.log("remote = ",remote);
                    romance.acceptPeerOffer(remote);
                });
            }
            romance.gameStatusUpdate();
        },
        updatePlayerAnalytics: function () {
            if (gameState.playerInfo.id) {
                ga('set','userId', gameState.playerInfo.id);
                Rollbar.configure({
                    payload: { id: gameState.playerInfo.id,
                               username: gameState.playerInfo.nickname,
                               email: (gameState.playerInfo.nickname ?
                                       gameState.playerInfo.nickname :
                                       gameState.playerInfo.id) +
                               '@players.tootsville.org' }});
                if (FB) {
                    FB.AppEvents.setUserID(""+gameState.playerInfo.id);
                }
            } else {
                ga('set','userId', null);
                Rollbar.configure({
                    payload: { id: null,
                               username: null,
                               email: null }});
                if (FB) {
                    FB.AppEvents.clearUserID();
                }
            }
            if (gameState.playerInfo.nickname) {
                if (FB) {
                    FB.AppEvents.updateUserProperties(
                        { nickname: gameState.playerInfo.nickname });
                }
            } else {
                if (FB) {
                    FB.AppEvents.updateUserProperties(
                        { nickname: null });
                }
            }

        },
        setPlayerInfo: function (hash) {
            var changedP = false;
            if (!gameState.playerInfo) {
                gameState.playerInfo = {}; }
            if (hash.id) {
                gameState.playerInfo.id = hash.id;
                changedP = true;
            }
            if (hash.nickname) {
                gameState.playerInfo.nickname = hash.nickname;
                changedP = true;
            }
            if (changedP) { romance.updatePlayerAnalytics(); }
        },
        googleAPIToken: function () {
            return ( gameState.googleUser &&
                     gameState.googleUser.getAuthResponse('id_token').access_token );
        },
        signInToGame: function () {
            console.log("Sign in to game mesh now; gather peers");
            romance.updatePeers();
            r2note ('Game Sign-In','Start','Start');
        },
        quitFromGame: function() {
            var xhr = new XMLHttpRequest();
            r2note ('Game Sign-In','Sign Out','Sign Out');
            romance.xhrSetUpPost(xhr, '/action/quit', function(response) {
                gameState.playerInfo = false;
                romance.gameStatusUpdate ();
            });
            xhr.send('google-api-token=' + romance.googleAPIToken());
        },
        quitFromGoogle: function() {
            r2note ('Google Sign-In','Sign Out','Sign Out');
            gapi.auth2.getAuthInstance().signOut(function () {
                gameState.googleUser = false;
                romance.gameStatusUpdate();
            });
        },
        quitFromFacebook: function () {
            r2note ('Facebook Sign-In','Sign Out','Sign Out');
            FB.logout();
        },
        disconnectMeshLink: function (link) {
        },
        quitFromMesh: function() {
            if (! gameState.meshLinks) { return; }
            r2note ('Mesh','Disconnect','Disconnecting');
            gameState.meshLinks.each(romance.disconnectMeshLink);
        },
        quitWorker: function() {},
        quit: function () {
            if (! ( (!! gameState.playerInfo) ||
                    (romance.loggedInP ()) ) ) {
                romance.gameStatusUpdate();
            }
            if (gameState.playerInfo) { romance.quitFromGame(); }
            if (gameState.googleUser) { romance.quitFromGoogle(); }
            if (gameState.facebookUser) {
                romance.quitFromFacebook(); }
            if (gameState.meshLinks) { romance.quitFromMesh(); }
            if (gameState.worker) { romance.quitWorker(); }
        },
        gotGoogleSignIn: function(user) {
            r2note ('Google Sign-In','Sign In','Success');
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
            console.error("network error", condition);
        },
        xhrReportErrors: function(xhr) {
            alert("Having trouble reaching the sign-in servers.\n"+
                  "Perhaps network problems or servers too busy?");
            console.error("XHR error", xhr);
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
            xhr.onerror = romance.xhrReportErrors;
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

function toArray(arrayLikeObject) {
    array = [];
    for (i = 0;
         i < arrayLikeObject.length;
         ++i) {
        array = array.concat(arrayLikeObject[i]) }
    return array
}

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
    window['ga'] = window['ga'] || function() {
        (ga['q'] = ga['q'] || []).push(arguments) };
    ga["l"] = +new Date;
    ga('create', 'UA-80917352-1', 'auto');
    ga('set', 'transport', 'beacon');
    ga('set','appName','tootstest');
    ga('set','appVersion','0.3.0');
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

FB.getLoginStatus(function(response) {
    // Check login status on load, and if the user is
    // already logged in, go directly to the welcome message.
    if (response.status == 'connected') {
        romance.gotFacebookSignin(response);
    }
});
