// This file was automatically generated from Templates.soy.
// Please don't edit this file by hand.

goog.provide('dikmax.Templates');

goog.require('soy');
goog.require('soy.StringBuilder');


/**
 * @param {Object.<string, *>=} opt_data
 * @param {soy.StringBuilder=} opt_sb
 * @return {string}
 * @notypecheck
 */
dikmax.Templates.codeWrapper = function(opt_data, opt_sb) {
  var output = opt_sb || new soy.StringBuilder();
  var lineList3 = opt_data.lines;
  var lineListLen3 = lineList3.length;
  for (var lineIndex3 = 0; lineIndex3 < lineListLen3; lineIndex3++) {
    var lineData3 = lineList3[lineIndex3];
    output.append((! (lineIndex3 == 0)) ? '\n' : '', '<span class="line">', lineData3, '</span>');
  }
  return opt_sb ? '' : output.toString();
};


/**
 * @param {Object.<string, *>=} opt_data
 * @param {soy.StringBuilder=} opt_sb
 * @return {string}
 * @notypecheck
 */
dikmax.Templates.footnote = function(opt_data, opt_sb) {
  var output = opt_sb || new soy.StringBuilder();
  output.append('<div><div class="arrow"></div><div class="popover-inner"><h3 class="popover-title"></h3><div class="popover-content"></div></div></div>');
  return opt_sb ? '' : output.toString();
};


/**
 * @param {Object.<string, *>=} opt_data
 * @param {soy.StringBuilder=} opt_sb
 * @return {string}
 * @notypecheck
 */
dikmax.Templates.movies = function(opt_data, opt_sb) {
  var output = opt_sb || new soy.StringBuilder();
  output.append('<span class="wrap">');
  var movieList16 = opt_data.movies;
  var movieListLen16 = movieList16.length;
  for (var movieIndex16 = 0; movieIndex16 < movieListLen16; movieIndex16++) {
    var movieData16 = movieList16[movieIndex16];
    output.append('<span class="item"><span class="item-header">', soy.$$escapeHtml(movieData16.date), ' <br><a href="', soy.$$escapeHtml(movieData16.link), '">', soy.$$escapeHtml(movieData16.title), '</a>', (movieData16.rating) ? '<br><span class="rating" title="(' + soy.$$escapeHtml(movieData16.rating) + '/10)"><span class="rate-' + soy.$$escapeHtml(movieData16.rating) + '"></span></span>' : '', '</span>', (movieData16.image) ? '<span class="item-footer"><a href="' + soy.$$escapeHtml(movieData16.link) + '"><img src="' + soy.$$escapeHtml(movieData16.image) + '" title="' + soy.$$escapeHtml(movieData16.title) + '"></a></span>' : '', '</span>');
  }
  output.append('</span>');
  return opt_sb ? '' : output.toString();
};


/**
 * @param {Object.<string, *>=} opt_data
 * @param {soy.StringBuilder=} opt_sb
 * @return {string}
 * @notypecheck
 */
dikmax.Templates.vaultContainers = function(opt_data, opt_sb) {
  var output = opt_sb || new soy.StringBuilder();
  var containerList45 = opt_data.containers;
  var containerListLen45 = containerList45.length;
  for (var containerIndex45 = 0; containerIndex45 < containerListLen45; containerIndex45++) {
    var containerData45 = containerList45[containerIndex45];
    output.append('<tr><td><a class="container-link" href="#', soy.$$escapeHtml(containerData45['name']), '">', soy.$$escapeHtml(containerData45['name']), '</a></td></tr>');
  }
  return opt_sb ? '' : output.toString();
};


/**
 * @param {Object.<string, *>=} opt_data
 * @param {soy.StringBuilder=} opt_sb
 * @return {string}
 * @notypecheck
 */
dikmax.Templates.vaultFiles = function(opt_data, opt_sb) {
  var output = opt_sb || new soy.StringBuilder();
  var fileList53 = opt_data.files;
  var fileListLen53 = fileList53.length;
  for (var fileIndex53 = 0; fileIndex53 < fileListLen53; fileIndex53++) {
    var fileData53 = fileList53[fileIndex53];
    output.append('<tr><td><a href="', soy.$$escapeHtml(opt_data.cdnUri), '/', soy.$$escapeHtml(fileData53['name']), '" target="_blank">', soy.$$escapeHtml(fileData53['name']), '</a></td><td>', soy.$$escapeHtml(fileData53['bytes']), '</td><td>', soy.$$escapeHtml(fileData53['content_type']), '</td><td>', soy.$$escapeHtml(fileData53['last_modified']), '</td></tr>');
  }
  return opt_sb ? '' : output.toString();
};
