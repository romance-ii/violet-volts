/* Social network logins and analytics */

/* Facebook API */
window.fbAsyncInit = function() {
    FB.init({ appId: '1265378050154137',
              xfbml: true,
              version: 'v2.7'});
};

(function(d,s,id){
    var js, fjs = d.getElementsByTagName(s)[0];
    if (d.getElementById(id)) {return;}
    js = d.createElement(s);
    js.id = id;
    js.src = "//connect.facebook.net/en_US/sdk.js";
    fjs.parentNode.insertBefore(js,fjs);
} (document,'script','facebook-jssdk'));


(function(d,s,id){
    var js, fjs = d.getElementsByTagName(s)[0];
    if(d.getElementById(id))return;
    js=d.createElement(s);
    js.id = id;
    js.src = "//connect.facebook.net/en_US/sdk.js#xfbml=1&version=v2.7&appId=1265378050154137";
    fjs.parentNode.insertBefore(js,fjs);
} (document,'script','facebook-jssdk'));

/* Google Analytics */
ga = window['ga'] || function() {
    (ga['q'] = ga['q'] || []).push(arguments)
};
ga["l"] = +new Date;
ga('create', 'UA-80917352-1', 'auto');
ga('set', 'transport', 'beacon');
ga('set','appName','tootstest');
ga('set','appVersion','0.0.1');
/*      if (gameState.playerInfo) {
        ga('set', 'userId', gameState.playerInfo.playerID);
        } */
ga('require', 'linkid');
ga('require', 'eventTracker');
ga('require', 'outboundLinkTracker');
ga('require', 'urlChangeTracker');
ga('send', 'pageview');
