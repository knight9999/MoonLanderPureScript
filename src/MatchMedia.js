/* global exports */
"use strict";

exports.getMatchMedia = function (str) {
  return function () {
    return window.matchMedia(str);
  };
};

exports.matches = function (mm) {
	return function () {
		return mm.matches;
	};
};

exports.addListener = function (callback) {
  return function (mm) {
    return function () {
      mm.addListener(function (mql) {
        callback(mql);
      });
    }
  }
};

exports.callListener = function (callback) {
  return function (mm) {
    return function () {
      callback(mm);
    }
  }
};