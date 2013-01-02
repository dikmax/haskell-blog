// This file was automatically generated from Templates.soy.
// Please don't edit this file by hand.

goog.provide('dikmax.Templates');

goog.require('soy');
goog.require('soydata');


/**
 * @param {Object.<string, *>=} opt_data
 * @param {(null|undefined)=} opt_ignored
 * @return {string}
 * @notypecheck
 */
dikmax.Templates.codeWrapper = function(opt_data, opt_ignored) {
  var output = '';
  var lineList3 = opt_data.lines;
  var lineListLen3 = lineList3.length;
  for (var lineIndex3 = 0; lineIndex3 < lineListLen3; lineIndex3++) {
    var lineData3 = lineList3[lineIndex3];
    output += ((! (lineIndex3 == 0)) ? '\n' : '') + '<span class="line" data-linenum="' + soy.$$escapeHtml(lineIndex3 + 1) + '">' + ((lineData3 == '') ? '&nbsp;' : soy.$$filterNoAutoescape(lineData3)) + '</span>';
  }
  return output;
};


/**
 * @param {Object.<string, *>=} opt_data
 * @param {(null|undefined)=} opt_ignored
 * @return {string}
 * @notypecheck
 */
dikmax.Templates.footnote = function(opt_data, opt_ignored) {
  return '<div><div class="arrow"></div><div class="popover-inner"><h3 class="popover-title"></h3><div class="popover-content"></div></div></div>';
};


/**
 * @param {Object.<string, *>=} opt_data
 * @param {(null|undefined)=} opt_ignored
 * @return {string}
 * @notypecheck
 */
dikmax.Templates.codeTooltip = function(opt_data, opt_ignored) {
  return '<div><div class="tooltip-arrow"></div><div class="tooltip-inner">Tooltip on left</div></div>';
};


/**
 * @param {Object.<string, *>=} opt_data
 * @param {(null|undefined)=} opt_ignored
 * @return {string}
 * @notypecheck
 */
dikmax.Templates.movies = function(opt_data, opt_ignored) {
  var output = '<span class="wrap">';
  var movieList24 = opt_data.movies;
  var movieListLen24 = movieList24.length;
  for (var movieIndex24 = 0; movieIndex24 < movieListLen24; movieIndex24++) {
    var movieData24 = movieList24[movieIndex24];
    output += '<span class="item"><span class="item-header">' + soy.$$escapeHtml(movieData24.date) + ' <br><a href="' + soy.$$escapeHtml(movieData24.link) + '">' + soy.$$escapeHtml(movieData24.title) + '</a>' + ((movieData24.rating) ? '<br><span class="rating" title="(' + soy.$$escapeHtml(movieData24.rating) + '/10)"><span class="rate-' + soy.$$escapeHtml(movieData24.rating) + '"></span></span>' : '') + '</span>' + ((movieData24.image) ? '<span class="item-footer"><a href="' + soy.$$escapeHtml(movieData24.link) + '"><img src="' + soy.$$escapeHtml(movieData24.image) + '" title="' + soy.$$escapeHtml(movieData24.title) + '"></a></span>' : '') + '</span>';
  }
  output += '</span>';
  return output;
};


/**
 * @param {Object.<string, *>=} opt_data
 * @param {(null|undefined)=} opt_ignored
 * @return {string}
 * @notypecheck
 */
dikmax.Templates.vaultPostsList = function(opt_data, opt_ignored) {
  var output = '';
  var postList53 = opt_data.posts;
  var postListLen53 = postList53.length;
  for (var postIndex53 = 0; postIndex53 < postListLen53; postIndex53++) {
    var postData53 = postList53[postIndex53];
    output += '<tr data-rowid=\'' + soy.$$escapeHtml(postData53.id) + '\' data-url=\'' + soy.$$escapeHtml(postData53.url) + '\'><td>' + soy.$$escapeHtml(postData53['localeDate']) + '</td><td>' + ((postData53['special']) ? '<i class=\'icon-ok-circle\'></i>' : (postData53['published']) ? '<i class=\'icon-ok\'></i>' : '') + '</td> <!-- TODO --><td>' + soy.$$escapeHtml(postData53['title']) + '<div><p>';
    var tagList69 = postData53['tags'];
    var tagListLen69 = tagList69.length;
    for (var tagIndex69 = 0; tagIndex69 < tagListLen69; tagIndex69++) {
      var tagData69 = tagList69[tagIndex69];
      output += '<a href=\'/tag/' + soy.$$escapeHtml(tagData69) + '\' class=\'vault-tag label\'>' + soy.$$escapeHtml(tagData69) + '</a> ';
    }
    output += '</p></div></td><td class=\'actions\'><a href="/post/' + soy.$$escapeHtml(postData53['url']) + '" class=\'action action-view\' title=\'Посмотреть\'></a><span class=\'action action-delete\' title=\'Удалить\'></span></td></tr>';
  }
  return output;
};


/**
 * @param {Object.<string, *>=} opt_data
 * @param {(null|undefined)=} opt_ignored
 * @return {string}
 * @notypecheck
 */
dikmax.Templates.vaultContainers = function(opt_data, opt_ignored) {
  var output = '';
  var containerList81 = opt_data.containers;
  var containerListLen81 = containerList81.length;
  for (var containerIndex81 = 0; containerIndex81 < containerListLen81; containerIndex81++) {
    var containerData81 = containerList81[containerIndex81];
    output += '<tr><td><a class="container-link" href="#' + soy.$$escapeHtml(containerData81['name']) + '">' + soy.$$escapeHtml(containerData81['name']) + '</a></td></tr>';
  }
  return output;
};


/**
 * @param {Object.<string, *>=} opt_data
 * @param {(null|undefined)=} opt_ignored
 * @return {string}
 * @notypecheck
 */
dikmax.Templates.vaultContainersLoading = function(opt_data, opt_ignored) {
  return '<tr><td>Загружаем список контейнеров...</td></tr>';
};


/**
 * @param {Object.<string, *>=} opt_data
 * @param {(null|undefined)=} opt_ignored
 * @return {string}
 * @notypecheck
 */
dikmax.Templates.vaultFiles = function(opt_data, opt_ignored) {
  var output = '';
  var fileList91 = opt_data.files;
  var fileListLen91 = fileList91.length;
  for (var fileIndex91 = 0; fileIndex91 < fileListLen91; fileIndex91++) {
    var fileData91 = fileList91[fileIndex91];
    output += '<tr><td><a href="' + soy.$$escapeHtml(opt_data.cdnUri) + '/' + soy.$$escapeHtml(fileData91['name']) + '" target="_blank">' + soy.$$escapeHtml(fileData91['name']) + '</a></td><td>' + soy.$$escapeHtml(fileData91['bytes']) + '</td><td>' + soy.$$escapeHtml(fileData91['content_type']) + '</td><td>' + soy.$$escapeHtml(fileData91['last_modified']) + '</td></tr>';
  }
  return output;
};


/**
 * @param {Object.<string, *>=} opt_data
 * @param {(null|undefined)=} opt_ignored
 * @return {string}
 * @notypecheck
 */
dikmax.Templates.vaultFilesLoading = function(opt_data, opt_ignored) {
  return '<tr><td colspan="4">Загружаем список файлов...</td></tr>';
};
