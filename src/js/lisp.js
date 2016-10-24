/*
 * Functions to allow JavaScript to call into Lisp functions.
 */

window.call_lisp = function (symbol) {
    var nameParts = symbol.split(":");
    var packageName = nameParts[0];
    var symbolName = nameParts[2] || nameParts[1];
    return jscl.packages[packageName].symbols[symbolName].
        fvalue.apply(jscl,
                     arguments.slice(1).map(jscl.internals.js_to_lisp));
}

window.curry_lisp = function (symbol) {
    var nameParts = symbol.split(":");
    var packageName = nameParts[0];
    var symbolName = nameParts[2] || nameParts[1];
    var fun = jscl.packages[packageName].symbols[symbolName].fvalue;
    var curryArgs = arguments.slice(1).map(jscl.internals.js_to_lisp);
    return function () {
        return fun.apply(jscl,
                         curryArgs.concat(arguments.map(jscl.internals.js_to_lisp)));
    }
}

(function(){
    var nextMethod = window.onerror;
    window.onerror = function (message, source, line, column, error) {
        console.error("Unhandled error: " + message +
                      "\n\tSource: " + (source || '?') +
                      "\n\tLine: " + (line || '?') + "\tColumn: " + (column || '?'));
        if (error) { console.error(error) }
        console.log("Reporting home…");
        var xhr = new XMLHttpRequest();
        xhr.open('POST', "/tootstest/zomg", true);
        xhr.setRequestHeader('Content-Type', 'application/json; charset=utf-8');
        xhr.send(JSON.stringify({
            "condition": "window.onerror",
            "catch": "gazonga",
            "product": "tootstest/0.2",
            "navigator": {	"appCodeName": appName.appCodeName,
                              "appName": navigator.appName,
                              "appVersion": navigator.appVersion,
                              "buildID": navigator.buildID,
                              "connection": navigator.connection &&
                              { "type": navigator.connection.type,
                                "downlinkMax": navigator.connection.downlinkMax },
                              "hardwareConcurrency": navigator.hardwareConcurrency,
                              "javaEnabled": navigator.javaEnabled,
                              "language": navigator.language,
                              "languages": navigator.languages,
                              "mimeTypes": navigator.mimeTypes,
                              "onLine": navigator.onLine,
                              "oscpu": navigator.oscpu,
                              "platform": navigator.platform,
                              "product": navigator.product,
                              "userAgent": navigator.userAgent,
                              "cookieEnabled": navigator.cookieEnabled,
                              "doNotTrack": navigator.doNotTrack,
                              "BrowserID": navigator.id,
                              "productSub": navigator.productSub,
                              "vendor": navigator.vendor,
                              "vendorSub": navigator.vendorSub,
                              "standalone", navigator.standalone },
            "message": message,
            "source": source,
            "line": line,
            "column": column,
            "error": {	"name": (error && error.name),
                              "message": (error && error.message),
                              "fileName": (error && error.fileName),
                              "lineNumber": (error && error.lineNumber),
                              "columnNumber": (error && error.columnNumber),
                              "stack": (error && error.stack) }}));
        if (nextMethod) {
            nextMethod(message, source, line, column, error);
        }
    }
})()
