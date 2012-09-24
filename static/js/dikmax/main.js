goog.provide('dikmax.main');

goog.require('dikmax.App');
goog.require('goog.debug.ErrorHandler');


/**
 * Main function.
 */
dikmax.main = function() {
  var app = new dikmax.App();
  app.init();
};

goog.exportSymbol('main', dikmax.main);
