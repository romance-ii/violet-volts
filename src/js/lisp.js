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

