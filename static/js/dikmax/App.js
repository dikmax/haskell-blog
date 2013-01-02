goog.provide('dikmax.App');

goog.require('dikmax.CodeSaxHandler');
goog.require('dikmax.CodeTooltip');
goog.require('dikmax.FootnotePopover');
goog.require('dikmax.Templates');
goog.require('goog.Timer');
goog.require('goog.Uri.QueryData');
goog.require('goog.array');
goog.require('goog.async.Delay');
goog.require('goog.date');
goog.require('goog.dom');
goog.require('goog.dom.classes');
goog.require('goog.dom.forms');
goog.require('goog.events');
goog.require('goog.fx.dom');
goog.require('goog.math.Box');
goog.require('goog.net.IframeIo');
goog.require('goog.net.XhrIo');
goog.require('goog.soy');
goog.require('goog.string');
goog.require('goog.string.StringBuffer');
goog.require('goog.string.html.HtmlParser');
goog.require('goog.ui.HoverCard');
goog.require('goog.userAgent');
goog.require('hljs');


/** @define {boolean} Don't include frontend part. */
var EXCLUDE_FRONT = false;


/** @define {boolean} Don't include vault part. */
var EXCLUDE_VAULT = false;



/**
 * @constructor
 */
dikmax.App = function() {
};


/**
 * @return {boolean} Is it a mobile browser?
 */
dikmax.App.isMobile = function() {
  return goog.dom.classes.has(document.body, 'mobile');
};


/**
 * Inits application
 */
dikmax.App.prototype.init = function() {
  this.topNavBar_();

  var pathname = document.location.pathname;
  if (!EXCLUDE_VAULT && goog.string.startsWith(pathname, '/vault/edit')) {
    this.setupDuplicateUrlChecker_();
    this.setupRenderer_();
    this.renderVaultPreview_();
  } else if (!EXCLUDE_VAULT &&
      goog.string.startsWith(pathname, '/vault/files')) {
    this.setupFileManager_();
  } else if (!EXCLUDE_VAULT && goog.string.startsWith(pathname, '/vault')) {
    this.vaultEventHandlers_();
  } else if (!EXCLUDE_FRONT) {
    this.updateCommentsText_();
    this.updateCodeListings_();
    this.inlineFootnotes_();
    this.transformLatestMovies_();
    this.setupKeyboardNavigation_();
  }
};


/**
 * @type {goog.Timer}
 * @private
 */
dikmax.App.prototype.timer_ = null;


/**
 * @type {?{length: number}}
 * @private
 */
dikmax.App.prototype.commentsElements_ = null;


/**
 * Comments processing.
 *
 * @private
 */
dikmax.App.prototype.updateCommentsText_ = function() {
  // Replace text in comments lint to proper Russian
  var elements = goog.dom.getElementsByTagNameAndClass('span', 'post-comments');
  if (!elements.length) {
    return;
  }
  this.commentsElements_ = elements;
  this.timer_ = new goog.Timer(100);
  goog.events.listen(this.timer_, goog.Timer.TICK,
      this.checkIsCommentsLoaded_, false, this);
  this.timer_.start();
};


/**
 * @private
 */
dikmax.App.prototype.checkIsCommentsLoaded_ = function() {
  if (goog.string.startsWith(goog.dom.getTextContent(
      this.commentsElements_[0]), 'Считаем')) {
    return;
  }
  this.timer_.stop();
  goog.array.forEach(this.commentsElements_, this.changeCommentText_, this);
};


/**
 * @param {Element} item Element to update.
 * @private
 */
dikmax.App.prototype.changeCommentText_ = function(item) {
  var links = goog.dom.getElementsByTagNameAndClass('a', null, item);
  if (goog.array.isEmpty(links)) {
    return;
  }
  var text = goog.dom.getTextContent(links[0]);
  var match = text.match(/^(\d+) комментариев/);
  if (match) {
    var count = Number(match[1]);
    if (Math.round(count % 100 / 10) != 1) {
      var count10 = count % 10;
      if (count10 == 1) {
        goog.dom.setTextContent(links[0], count + ' комментарий');
      } else if (count10 >= 2 && count10 <= 4) {
        goog.dom.setTextContent(links[0], count + ' комментария');
      }
    }
  }
};

// Code processing


/**
 * @private
 */
dikmax.App.prototype.updateCodeListings_ = function() {
  if (this.highlightBlocks_()) {
    new dikmax.CodeTooltip();
  }
};


/**
 * @private
 * @return {boolean} Is there any blocks on page?
 */
dikmax.App.prototype.highlightBlocks_ = function() {
  /** @type {{length: number}} */
  var blocks = goog.dom.getElementsByTagNameAndClass('code', 'sourceCode');
  blocks = goog.array.filter(blocks, function(item) {
    return item.parentNode instanceof HTMLPreElement;
  });
  if (blocks.length) {
    var parser = new goog.string.html.HtmlParser();
    var handler = new dikmax.CodeSaxHandler();
    goog.array.forEach(blocks, function(block) {
      hljs.highlightBlock(block);

      parser.parse(handler, block.innerHTML);
      goog.soy.renderElement(block, dikmax.Templates.codeWrapper,
          {lines: handler.getLines()});
      goog.dom.classes.add(block, 'highlighted');
    });
  }

  return blocks.length > 0;
};


/**
 * @private
 */
dikmax.App.prototype.inlineFootnotes_ = function() {
  new dikmax.FootnotePopover();
};


/**
 * @private
 */
dikmax.App.prototype.topNavBar_ = function() {
  var toggleButton = goog.dom.getElementByClass('topnavbar-collapse-toggle');
  var collapsibleBlock =
      goog.dom.getElementByClass('topnavbar-collapsible-block');
  var collapsibleContent =
      goog.dom.getElementByClass('topnavbar-collapsible-content');

  var themesToggle = goog.dom.getElementByClass('themes-box-toggle');
  var themesBox = goog.dom.getElementByClass('themes-box', themesToggle);
  var themesLink = goog.dom.getElementsByTagNameAndClass(
      'a', null, themesToggle)[0];
  var themesButton = goog.dom.getElementByClass('themes-button', themesToggle);
  var themesVisible = false;

  // Collapse/expand navbar on smallscreen devices
  var visible = false;
  /**
     * @param {!number} oldHeight Old navbar height.
     * @param {!number} newHeight New navbar height.
     */
  var resizeNavBar = function(oldHeight, newHeight) {
    var transition;
    if (goog.style.transition.isSupported()) {
      transition = new goog.fx.css3.Transition(
          collapsibleBlock, 0.35,
          {'height': oldHeight + 'px'}, {'height': newHeight + 'px'},
          {
            property: 'height',
            duration: 0.35,
            timing: 'ease',
            delay: 0
          }
          );
    } else {
      transition = new goog.fx.dom.ResizeHeight(collapsibleBlock,
          oldHeight, newHeight, 350);
    }

    transition.play();
  };
  goog.events.listen(toggleButton, goog.events.EventType.CLICK, function() {
    var height = goog.style.getSize(collapsibleContent).height;
    if (visible) {
      resizeNavBar(height, 0);
      if (themesVisible) {
        themesVisible = false;
        themesBox.style.display = 'none';
      }
    } else {
      resizeNavBar(0, height);
    }
    visible = !visible;
  });

  goog.events.listen(themesLink, goog.events.EventType.CLICK, function() {
    var screenWidth = goog.dom.getViewportSize().width;
    if (screenWidth < 980) {
      var oldHeight = goog.style.getSize(collapsibleContent).height;
      themesVisible = !themesVisible;
      if (themesVisible) {
        themesBox.style.display = 'block';
      } else {
        themesBox.style.display = 'none';
      }
      var newHeight = goog.style.getSize(collapsibleContent).height;
      resizeNavBar(oldHeight, newHeight);
    } else {
      themesVisible = true;
      themesBox.style.display = 'block';
    }
  });

  goog.events.listen(themesButton, goog.events.EventType.CLICK, function() {
    themesVisible = false;
    themesBox.style.display = 'none';
  });
  goog.events.listen(document.body, goog.events.EventType.CLICK,
      function(event) {
        if (themesVisible) {
          var screenWidth = goog.dom.getViewportSize().width;
          if (screenWidth >= 980) {
            var target = event.target;
            if (!goog.dom.getAncestorByClass(target, 'tags-wrapper')) {
              themesVisible = false;
              themesBox.style.display = 'none';
            }
          }
        }
        return true;
      }, true
  );

  // On resize
  goog.events.listen(window, goog.events.EventType.RESIZE, function() {
    if (visible) {
      var screenWidth = goog.dom.getViewportSize().width;
      if (screenWidth < 980) {
        var newHeight = goog.style.getSize(collapsibleContent).height;
        var oldHeight = goog.style.getSize(collapsibleBlock).height;
        if (oldHeight !== newHeight) {
          collapsibleBlock.style.height = newHeight + 'px';
        }
      } else {
        collapsibleBlock.style.height = '';
      }
    }
    return true;
  });

};


/**
 * @private
 */
dikmax.App.prototype.transformLatestMovies_ = function() {
  var moviesEl = goog.dom.getElementByClass('latest-movies');
  if (!moviesEl) {
    return;
  }

  var listItems = goog.dom.getElementsByTagNameAndClass('li', null, moviesEl);
  var movies = [];
  goog.array.forEach(listItems, function(item) {
    if (item.childNodes.length !== 3) {
      return;
    }
    var date = item.childNodes[0].data;
    var titleLink = item.childNodes[1];
    var link = titleLink.getAttribute('href');
    var title = goog.dom.getTextContent(titleLink);
    var image = titleLink.getAttribute('title');
    var rating = item.childNodes[2].data.match(/\((\d+)\/10\)/);
    movies.push({
      date: date,
      link: link,
      rating: rating ? Number(rating[1]) : null,
      image: image,
      title: title
    });
  });

  goog.dom.getElementByClass('post-body', moviesEl).
      innerHTML = dikmax.Templates.movies({movies: movies});
};

// Vault transforms


/**
 * @private
 */
dikmax.App.prototype.vaultEventHandlers_ = function() {
  var postsList = goog.dom.getElementByClass('vault-posts-list');

  var posts = [];
  goog.net.XhrIo.send('/vault/postslist', function(e) {
    /** @type {goog.net.XhrIo} */
    var request = e.target;
    if (request.isSuccess()) {
      posts = request.getResponseJson()['result'];
      var publishedCount = 0;
      for (var i = 0; i < posts.length; ++i) {
        var dateTime = goog.date.fromIsoString(posts[i]['date']);
        posts[i]['date'] = dateTime;
        posts[i]['localeDate'] =
            goog.string.padNumber(dateTime.getDate(), 2) + '.' +
            goog.string.padNumber(dateTime.getMonth() + 1, 2) + '.' +
            goog.string.padNumber(dateTime.getFullYear(), 4) + ' ' +
            goog.string.padNumber(dateTime.getHours(), 2) + ':' +
            goog.string.padNumber(dateTime.getMinutes(), 2) + ':' +
            goog.string.padNumber(dateTime.getSeconds(), 2);
        posts[i]['index'] =
            [posts[i]['title'], posts[i]['localeDate'],
              posts[i]['tags'].join(' ')].join(' ').toLocaleLowerCase();
        if (posts[i]['published']) {
          ++publishedCount;
        }
      }
      goog.dom.getElementsByTagNameAndClass('tbody', null, postsList)[0].
          innerHTML = dikmax.Templates.vaultPostsList({posts: posts});

      goog.dom.getElementByClass('vault-posts-count').innerHTML =
          'Записей: ' + posts.length;
      goog.dom.getElementByClass('vault-published-count').innerHTML =
          'Опубликовано: ' + publishedCount;
      goog.dom.getElementByClass('vault-filtered-count').innerHTML =
          'Показано: ' + posts.length;
    }
  });

  var listFilter = goog.dom.getElementByClass('vault-posts-list-filter');

  goog.events.listen(postsList, goog.events.EventType.CLICK,
      function(event) {
        var target = event.target;
        var row = goog.dom.getAncestorByTagNameAndClass(target, 'tr');
        var rowId = row.getAttribute('data-rowid');
        if (goog.dom.getAncestorByClass(target, 'actions')) {
          if (goog.dom.classes.has(target, 'action-delete')) {
            // Delete
            var title = rowId;
            for (var i = 0; i < posts.length; ++i) {
              if (posts[i]['id'] == rowId) {
                title = posts[i]['title'];
                break;
              }
            }
            if (confirm('Действительно удалить запись "' + title +
                        '"?')) {

              document.location = '/vault/delete/' + rowId;
            }
          }
        } else if (goog.dom.classes.has(target, 'vault-tag')) {
          // Tag click
          if (listFilter.value == '') {
            listFilter.value = target.innerHTML;
          } else {
            listFilter.value += ' ' + target.innerHTML;
          }
          delay.start();
          event.preventDefault();
        } else {
          // Edit
          if (!rowId) {
            return false;
          }
          document.location = '/vault/edit/' + rowId;
        }
      }
  );

  // Processing filter
  var delay = new goog.async.Delay(function() {
    var value = listFilter.value.toLocaleLowerCase();
    var words = value.split(' ').filter(function(item) {
      return item !== '';
    });
    var result;
    if (!words.length) {
      result = posts;
    } else if (words.length === 1) {
      var word = words[0];
      result = posts.filter(function(item) {
        return item['index'].indexOf(word) !== -1;
      });
    } else {
      result = posts.filter(function(item) {
        for (var i = 0; i < words.length; ++i) {
          if (item['index'].indexOf(words[i]) === -1) {
            return false;
          }
        }

        return true;
      });
    }

    goog.dom.getElementsByTagNameAndClass('tbody', null, postsList)[0].
        innerHTML = dikmax.Templates.vaultPostsList({posts: result});
    goog.dom.getElementByClass('vault-filtered-count').innerHTML =
        'Показано: ' + result.length;
  }, 100);

  goog.events.listen(listFilter,
      [goog.events.EventType.CHANGE,
       goog.events.EventType.KEYPRESS,
       goog.events.EventType.KEYUP],
      function() {
        delay.start();
      }
  );
};


/**
 * @private
 */
dikmax.App.prototype.setupDuplicateUrlChecker_ = function() {
  var postUrl = document.getElementById('post-url');
  var postIdValue = goog.dom.forms.getValue(
      document.getElementById('post-id')
      );
  // TODO add timeout checks
  goog.events.listen(postUrl,
      goog.events.EventType.CHANGE, function() {
        var value = goog.dom.forms.getValue(postUrl);
        goog.net.XhrIo.send('/vault/checkurl', function(e) {
          /** @type {goog.net.XhrIo} */
          var request = e.target;
          if (request.isSuccess()) {
            var responseJson = request.getResponseJson();
            goog.dom.classes.enable(
                goog.dom.getAncestorByClass(postUrl, 'control-group'),
                'error', !responseJson.result);
          } else {
            console.log(request);
          }
        }, 'POST', goog.Uri.QueryData.createFromMap({
          'id': postIdValue,
          'url': value
        }).toString());
      }
  );
};


/**
 * @param {string=} opt_formData Data to render.
 * @private
 */
dikmax.App.prototype.renderVaultPreview_ = function(opt_formData) {
  if (!goog.isString(opt_formData)) {
    /** @type {HTMLFormElement} */
    var form = /** @type {HTMLFormElement} */
        (goog.dom.getElementByClass('post-form'));
    opt_formData = goog.dom.forms.getFormDataString(form);
  }

  var me = this;
  goog.net.XhrIo.send('/vault/renderpost', function(e) {
    /** @type {goog.net.XhrIo} */
    var request = e.target;
    if (request.isSuccess()) {
      goog.dom.getElementByClass('render-area').innerHTML =
          request.getResponseText();
      me.highlightBlocks_();
    }
  }, 'POST', opt_formData);
};


/**
 * @private
 */
dikmax.App.prototype.setupRenderer_ = function() {
  new dikmax.CodeTooltip();

  goog.events.listen(goog.dom.getElementByClass('btn-refresh'),
      goog.events.EventType.CLICK, function(e) {
        this.renderVaultPreview_();
        e.preventDefault();
      }, true, this);

  // TODO remove error on cancel button click

  /** @type {HTMLFormElement} */
  var form = /** @type {HTMLFormElement} */
      (goog.dom.getElementByClass('post-form'));

  if (!dikmax.App.isMobile()) {
    var checkTimer = new goog.Timer(500);
    var formData = goog.dom.forms.getFormDataString(form);
    var newFormData = '';
    var delay = new goog.async.Delay(this.renderVaultPreview_, 2000, this);
    goog.events.listen(checkTimer, goog.Timer.TICK, function() {
      newFormData = goog.dom.forms.getFormDataString(form);
      if (formData != newFormData) {
        formData = newFormData;
        delay.start();
      }
    });
    checkTimer.start();
  }
};


/**
 * @private
 */
dikmax.App.prototype.setupFileManager_ = function() {
  var showError = function(object) {
    if (object['message']) {
      alert('Произошла ошибка: ' + object['message']);
    } else {
      alert('Произошла неизвестная ошибка.');
    }
  };

  var containersList = goog.dom.getElementByClass('containers-list');
  var filesList = goog.dom.getElementByClass('files-list');

  var currentContainer = null;
  var containers = {};

  // Load containers list
  goog.dom.getElementsByTagNameAndClass('tbody', null,
      containersList)[0].innerHTML =
      dikmax.Templates.vaultContainersLoading();

  goog.net.XhrIo.send('/vault/fileshandler', function(e) {
    /** @type {goog.net.XhrIo} */
    var request = e.target;
    if (request.isSuccess()) {
      var data = request.getResponseJson();
      if (!data['success']) {
        showError(data);
        return;
      }
      if (data['result']) {
        goog.array.forEach(data['result'], function(item) {
          containers[item.name] = item;
        });
        goog.dom.getElementsByTagNameAndClass('tbody', null,
            containersList)[0].innerHTML =
            dikmax.Templates.vaultContainers({
              containers: data['result']
            });
      }
    }
  }, 'POST', 'action=getContainersList');

  var updateFilesList = function() {
    goog.dom.getElementsByTagNameAndClass('tbody', null,
        filesList)[0].innerHTML = dikmax.Templates.vaultFilesLoading();

    goog.net.XhrIo.send('/vault/fileshandler', function(e) {
      /** @type {goog.net.XhrIo} */
      var request = e.target;
      if (request.isSuccess()) {
        var data = request.getResponseJson();
        if (!data['success']) {
          showError(data);
          return;
        }
        if (data['result']) {
          goog.dom.getElementsByTagNameAndClass('tbody', null,
              filesList)[0].innerHTML =
              dikmax.Templates.vaultFiles({
                files: data['result'],
                cdnUri: containers[currentContainer]['cdn_uri']
              });
        }
      }
    }, 'POST', goog.Uri.QueryData.createFromMap({
      'action': 'getContainerItems',
      'container': currentContainer
    }).toString());
  };

  // Handle container select
  goog.events.listen(containersList, goog.events.EventType.CLICK,
      function(e) {
        var target = e.target;
        if (!(target instanceof HTMLAnchorElement)) {
          var children = goog.dom.getElementsByTagNameAndClass(
              'a', 'container-link', target
              );
          if (!children) {
            e.preventDefault();
            return;
          }
          target = children[0];
        }
        currentContainer = target.getAttribute('href').substring(1);

        goog.style.showElement(
            goog.dom.getElementByClass('files-panel'), true
        );
        goog.dom.forms.setValue(
            document.getElementById('file-container'),
            currentContainer
        );

        updateFilesList();
      }
  );

  var fileUpload = document.getElementById('file-upload');
  goog.events.listen(fileUpload,
      goog.events.EventType.CHANGE,
      function(e) {
        var filename = e.target.files[0].name;
        goog.dom.forms.setValue(document.getElementById('file-name'),
            filename);
      }
  );

  goog.events.listen(document.getElementById('file-button'),
      goog.events.EventType.CLICK,
      function() {
        if (!document.createEvent) {
          alert('С IE не работаем.');
        }

        var event = document.createEvent('MouseEvents');
        event.initMouseEvent('click', true, true, window,
            0, 0, 0, 0, 0, false, false, false, false, 0, null);

        fileUpload.dispatchEvent(event);
      }
  );

  goog.events.listen(document.getElementById('file-submit'),
      goog.events.EventType.CLICK,
      function(e) {
        var io = new goog.net.IframeIo();
        goog.events.listenOnce(io, goog.net.EventType.COMPLETE,
            function(e) {
              /** @type {goog.net.IframeIo} */
              var request = e.target;
              if (request.isSuccess()) {
                var data = request.getResponseJson();
                if (!data['success']) {
                  showError(data);
                  return;
                }

                goog.dom.forms.setValue(document.getElementById('file-name'),
                    '');
                goog.dom.forms.setValue(document.getElementById('file-upload'),
                    '');

                updateFilesList();
              }
            }
        );
        io.sendFromForm(/** @type {HTMLFormElement} */
            (goog.dom.getElementByClass('upload-form')));
        e.preventDefault();
      }
  );
};


/**
 * @private
 */
dikmax.App.prototype.setupKeyboardNavigation_ = function() {
  goog.events.listen(document, goog.events.EventType.KEYDOWN, function(e) {
    if (e.ctrlKey) {
      var link;
      if (e.keyCode === 37) {
        // Previous page link
        link = goog.dom.getElementByClass('previous');
      } else if (e.keyCode === 39) {
        // Next page link
        link = goog.dom.getElementByClass('next');
      }
      if (link && goog.dom.getAncestorByClass(link, 'pager')) {
        var anchor = goog.dom.getElementsByTagNameAndClass('a', null, link);
        if (anchor.length) {
          document.location = anchor[0].getAttribute('href');
        }
      }
    }
  });
};
