// https://github.com/GlenDC/trixel/blob/refactor/src/Native/GlueRandom.js

var _tpict$adder$Native_GlueRandom = (function() {
  function randomInt(min, max) {
    return Math.floor(Math.random() * (max - min + 1)) + min;
  }

  return {
    randomInt: F2(randomInt)
  };
})();
