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
    output.append((! (lineIndex3 == 0)) ? '\n' : '', '<span class="line" data-linenum="', soy.$$escapeHtml(lineIndex3 + 1), '">', (lineData3 == '') ? '&nbsp;' : lineData3, '</span>');
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
dikmax.Templates.codeTooltip = function(opt_data, opt_sb) {
  var output = opt_sb || new soy.StringBuilder();
  output.append('<div><div class="tooltip-arrow"></div><div class="tooltip-inner">Tooltip on left</div></div>');
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
  var movieList24 = opt_data.movies;
  var movieListLen24 = movieList24.length;
  for (var movieIndex24 = 0; movieIndex24 < movieListLen24; movieIndex24++) {
    var movieData24 = movieList24[movieIndex24];
    output.append('<span class="item"><span class="item-header">', soy.$$escapeHtml(movieData24.date), ' <br><a href="', soy.$$escapeHtml(movieData24.link), '">', soy.$$escapeHtml(movieData24.title), '</a>', (movieData24.rating) ? '<br><span class="rating" title="(' + soy.$$escapeHtml(movieData24.rating) + '/10)"><span class="rate-' + soy.$$escapeHtml(movieData24.rating) + '"></span></span>' : '', '</span>', (movieData24.image) ? '<span class="item-footer"><a href="' + soy.$$escapeHtml(movieData24.link) + '"><img src="' + soy.$$escapeHtml(movieData24.image) + '" title="' + soy.$$escapeHtml(movieData24.title) + '"></a></span>' : '', '</span>');
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
  var containerList53 = opt_data.containers;
  var containerListLen53 = containerList53.length;
  for (var containerIndex53 = 0; containerIndex53 < containerListLen53; containerIndex53++) {
    var containerData53 = containerList53[containerIndex53];
    output.append('<tr><td><a class="container-link" href="#', soy.$$escapeHtml(containerData53['name']), '">', soy.$$escapeHtml(containerData53['name']), '</a></td></tr>');
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
  var fileList61 = opt_data.files;
  var fileListLen61 = fileList61.length;
  for (var fileIndex61 = 0; fileIndex61 < fileListLen61; fileIndex61++) {
    var fileData61 = fileList61[fileIndex61];
    output.append('<tr><td><a href="', soy.$$escapeHtml(opt_data.cdnUri), '/', soy.$$escapeHtml(fileData61['name']), '" target="_blank">', soy.$$escapeHtml(fileData61['name']), '</a></td><td>', soy.$$escapeHtml(fileData61['bytes']), '</td><td>', soy.$$escapeHtml(fileData61['content_type']), '</td><td>', soy.$$escapeHtml(fileData61['last_modified']), '</td></tr>');
  }
  return opt_sb ? '' : output.toString();
};
