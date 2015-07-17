/* jshint unused:false */

'use strict';

var _R; // Return register

var _S   = []; // Stack frame data
var _SP  = 0;  // Beginning of current stack frame
var _SQ  = 0;  // End of current stack frame
var _SR  = 0;  // Current stack frame reference
var _PSP = []; // Beginnings of previous stack frames

var _A  = []; // Array frame data
var _AP = 0;  // Beginning of current array frame


function idris_pushFrame() {
  _PSP[_SR] = _SP;
  _SP = _SQ;
  _SR += 1;
  for (var i = 0; i < arguments.length; i += 1, _SQ += 1) {
    _S[_SQ] = arguments[i];
  }
}


function idris_popFrame() {
  _SQ = _SP;
  _SR -= 1;
  _SP = _PSP[_SR];
}


function idris_makeArray() {
  _R = _AP;
  // var log = '' + _AP;
  for (var i = 0; i < arguments.length; i += 1, _AP += 1) {
    _A[_AP] = arguments[i];
    // log += ' ' + arguments[i];
  }
  // console.log(log);
}


function idris_error(s) {
  throw s;
}


function idris_writeStr(s) {
  console.log(s);
}
