/* jshint -W027, unused:false */

'use strict';

function idris_reverseStr(s) {
    var r = '';
    for (var i = s.length - 1; i >= 0; i -= 1) {
        r += s[i];
    }
    return r;
}

function idris_writeStr(s) {
    console.log(s);
}

function idris_error(s) {
    throw s;
}
