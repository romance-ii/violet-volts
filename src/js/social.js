/* Social network logins and analytics */

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
} (document,'script','facebook-jssdk'));

/* Google Analytics */
function(){
    ga = window['ga'] || function() {
        (ga['q'] = ga['q'] || []).push(arguments)
    };
    ga["l"] = +new Date;
    ga('create', 'UA-80917352-1', 'auto');
    ga('set', 'transport', 'beacon');
    ga('set','appName','tootstest');
    ga('set','appVersion','0.0.2');
    /*      if (gameState.playerInfo) {
            ga('set', 'userId', gameState.playerInfo.playerID);
            } */
    ga('require', 'linkid');
    ga('require', 'eventTracker');
    ga('require', 'outboundLinkTracker');
    ga('require', 'urlChangeTracker');
    ga('send', 'pageview');
}();

function(){
    var gameState = {
        googleUser: false,
        facebookUser: false,
        oauthUser: false
    };
    window.romance = {
        googleLoggedInP: function() { return !! gameState.googleUser },
        facebookLoggedInP: function() { return !! gameState.facebookUser },
        oauthLoggedInP: function() { return !! gameState.oauthUser },
        loggedInP: function() { return (romance.googleLoggedInP() || romance.facebookLoggedInP() || romance.oauthLoggedInP()); },
        clearFacebookUser: function() { gameState.facebookUser = false },
        facebookUserRefusedSignIn: function () {
            ga('send', 'event', 'Facebook Sign-In', 'Refused', 'Refused');
        },
        userIsNoSignedInToFacebook: function() {
            ga('send', 'event', 'Facebook Sign-In', 'Not Signed In', 'Not Signed In');
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
            gameStatusUpdate();
        },
        checkLogInState: function () {
            FB.getLoginStatus(romance.facebookSignInChanged);
        },
        signInToFacebook: function() {
            ga('send','event','Facebook Sign-In','Start','Start');
            FB.login ({ 'scope': 'public_profile,email' },
                      romance.facebookSignInChanged );
        },
        finishFacebookSignIn: function () {
            FB.api('/me', { 'fields': 'id,name_format,picture,email' },
                   function (response) {
                       if (response.error) {
                           console.log(response);
                           romance.clearFacebookUser();
                           romance.setHTML('facebook-name', '');
                           romance.setSrc('facebook-photo', null);
                           ga('send','event','Facebook Sign-In', 'Error', 'After sign-in cannot load /me');
                       } else {
                           var name = response.name_format;
                           console.log("got successful FB login for: " + name);
                           console.log(response);
                           romance.setHTML('facebook-name', name);
                           romance.setSrc('facebook-photo', response.picture.data.url);
                           ga('send','event','Facebook Sign-In', 'Success', 'Success');
                           gameState.facebookUser = response;
                       }
                       romance.gameStatusUpdate();
                   });
        },
        setHTML: function(id, html) {
            getElementById(id).innerHTML = html;
        },
        setSrc: function(id, src) {
            getElementById(id).src = src;
        },
        elementStyle: function(id) {
            return getElementById(id).style;
        },
        showOverlayLayer: function () {
            romance.elementStyle('overlay').display = 'block';
        },
        blockNone: function(flag) {
            if (flag) { return 'block' } else { return 'none' }
        },
        elementDisplayBlockP: function(id) {
            return (romance.elementStyle(id) == 'block');
        },
        setElementDisplayBlock: function(id, blockP) {
            romance.elementStyle(id).display = romance.blockNone(blockP);
        },
        hideLoginOverlay: function() {
            romance.setElementDisplayBlock('login-overlay', false); 
        },
        showLoginOverlay: function() {
            ga('send','screenview', {'screenName': 'login-overlay'});
            romance.setElementDisplayBlock('login-overlay', true);
            romance.setElementDisplayBlock('game-welcome', false);
        },
        signInLayersToggle: function(service, signedInP) {
            romance.setElementDisplayBlock(service + '-signed-in', romance.blockNone(signedInP));
            romance.setElementDisplayBlock(service + '-not-signed-in', romance.blockNone(! signedInP));
        },
        gameStatusUpdate: function() {
            console.log('game-status-update');
            if ( (! romance.loggedInP()) || (!! gameState.overlayActive) ) {
                romance.showOverlayLayer();
                if (! romance.loggedInP() ) {
                    console.log('user not signed in');
                }
                if (! romance.elementDisplayBlockP ('login-overlay')) {
                    romance.showLoginOverlay ();
                }
            } else {
                romance.hideLoginOverlay();
            }

            romance.signInLayersToggle ('google', romance.googleUserLoggedInP());
            romance.signInLayersToggle ('facebook', romance.facebookUserLoggedInP());

            if (! gameState.playerInfo ) {
                console.log('waiting for game server reply');
                return;
            }
            var overlayActive = gameState.overlayActive;
            if (overlayActive) {
                romance.setActiveOverlay(overlayActive);
            } else {
                console.log('Game play mode active');
            }
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
                return gapi.signin2.render('my-signin2', {'scope': 'profile email',
                                                          'longtitle': true,
                                                          'theme': 'dark',
                                                          'onsuccess': romance.gotGoogleSignIn,
                                                          'onfailure': romance.googleSignInFailed });
            }
        },
        makeNewPlayer: function () {
            var nickname = 'Bubba';
            ga('send','event','Game Sign-In','Make New Player', nickname);
            return { 'nickname': nickname,
                     'hitPoints': 10,
                     'maxHitPoints': 10,
                     'inventory': [ { 'name': 'Blue Jeans',
                                      'id': 0x1000,
                                      'equip': 'pants',
                                      'equippedP': true,
                                      'durability': 1,
                                      'repair': 1,
                                      'maxRepair': 1 } ] };
        },
        signInToMesh: function(response) {
            console.log("Sign in to game mesh now; received directory");
            if (response) { console.log(response); }
            ga('send', 'event', 'Game Sign-In', 'Join Mesh', 'Join Mesh');
            var rando = Math.floor(Math.random(10000));
            gameState.playerInfo = { 'id': rando, 'nickname': 'Testing ' + rando };
            romance.gameStatusUpdate();
        },
        googleAPIToken: function () {
            return ( gameState.googleUser && gameState.googleUser.getAuthResponse('id_token') );
        },
        signInToGame: function () {
            console.log("Sign in to game mesh now; get directory");
            var xhr = new XMLHttpRequest();
            ga('send','event','Game Sign-In','Start','Start');
            romance.xhrSetUpPost(xhr, '/gossip', romance.signInToMesh,);
            xhr.send('google-api-token=' + romance.googleAPIToken());
        },
        quitFromGame: function() {
            var xhr = new XMLHttpRequest();
            ga('send','event','Game Sign-In','Sign Out','Sign Out');
            romance.xhrSetUpPost(xhr, '/quit', function(response) {
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
        }
        quitFromMesh: function() {
            if (! gameState.meshLinks) { return; }
            ga('send','event','Mesh','Disconnect','Disconnecting');
            gameState.meshLinks.each(romance.disconnectMeshLink);
        },
        quitWorker: function() {},
        quit: function () {
            if (! ( (!! gameState.playerInfo) || (romance.loggedInP ()) ) ) {
                romance.gameStatusUpdate();
            }
            if (!! gameState.playerInfo) { romance.quitFromGame(); }
            if (!! gameState.googleUser) { romance.quitFromGoogle(); }
            if (!! gameState.facebookUser) { romance.quitFromFacebook(); }
            if (!! gameState.meshLinks) { romance.quitFromMesh(); }
            if (!! gameState.worker) { romance.quitWorker(); }
        },
        gotGoogleSignIn: function() {
            console.log("Got Google sign-in");
        },
        networkError: function(condition) {
            alert("A network error occurred. Is the network disconnected?");
            console.log("network error");
            console.log(condition);
        },
        xhrReportErrors: function(xhr) {
            xhr.onerror = romance.networkError;
        },
        serverURL: function (suffix) {
            return window.serverInfo.url + suffix;
        },
        xhrSetUpPost: function(xhr,uri,onLoad) {
            xhr.open('PUT', romance.serverURL(uri));
            xhr.setRequestHeader('Accept': 'application/json');
            xhr.setRequestHeader('Content-Type': 'application/x-www-form-urlencoded');
            xhr.onload = onLoad;
            romance.xhrReportErrors(xhr);
        }
    };
}();
