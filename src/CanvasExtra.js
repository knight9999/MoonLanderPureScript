"use strict";

exports.getFillStyle = function (ctx) {
  return function () {
    return ctx.fillStyle;
  }
};