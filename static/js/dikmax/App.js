goog.provide('dikmax.App');

goog.require('dikmax.FootnotePopover');
goog.require('dikmax.Templates');
goog.require('goog.Timer');
goog.require('goog.Uri.QueryData');
goog.require('goog.array');
goog.require('goog.async.Delay');
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
goog.require('goog.ui.HoverCard');
goog.require('goog.userAgent');

/**
 * @constructor
 */
dikmax.App = function() {
};

/**
 * Inits application
 */
dikmax.App.prototype.init = function() {
    hljs.initHighlighting();

    this.topNavBar_();

    var pathname = document.location.pathname;
    if (goog.string.startsWith(pathname, '/vault/edit')) {
        this.setupDuplicateUrlChecker_();
        this.setupRenderer_();
        this.renderVaultPreview_();
    } else if (goog.string.startsWith(pathname, '/vault/files')) {
        this.setupFileManager_();
    } else if (goog.string.startsWith(pathname, '/vault')) {
        this.vaultEventHandlers_();
        this.showVaultStatistics_();
    } else {
        this.updateCommentsText_();
        this.updateCodeListings_();
        this.inlineFootnotes_();
        this.transformLatestMovies_();
    }
};


// Comments processing

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
 * @private
 */
dikmax.App.prototype.updateCommentsText_ = function() {
    // Replace text in comments lint to proper Russian
    var elements = goog.dom.getElementsByTagNameAndClass('p', 'post-comments');
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
 * @param {Element} item
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
    var code = goog.dom.getElementsByTagNameAndClass('code');

    code = goog.array.filter(code, function(item) {
        return item.parentNode instanceof HTMLPreElement;
    });
    goog.array.forEach(code, function(item) {
        /** @type {string} */
        var html = item.innerHTML;
        var lines = html.split('\n');
        goog.soy.renderElement(item, dikmax.Templates.codeWrapper,
            {lines: lines});
        goog.dom.classes.add(item, 'highlighted');
    });
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
    var themesLink = goog.dom.getElementsByTagNameAndClass('a', null, themesToggle)[0];
    var themesButton = goog.dom.getElementByClass('themes-button', themesToggle);
    var themesVisible = false;

    // Collapse/expand navbar on smallscreen devices
    var visible = false;
    /**
     * @param {!number} oldHeight
     * @param {!number} newHeight
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
    goog.events.listen(postsList, goog.events.EventType.CLICK,
        function(event) {
            var target = event.target;
            var row = goog.dom.getAncestorByTagNameAndClass(target, 'tr');
            if (goog.dom.getAncestorByClass(target, 'actions')) {
                if (goog.dom.classes.has(target, 'action-delete')) {
                    // Delete
                    var title = goog.dom.getTextContent(
                        row.childNodes[2].childNodes[0]
                    );
                    if (confirm('Действительно удалить запись "' + title +
                        '"?')) {

                        document.location = '/vault/delete/' +
                            row.getAttribute('data-rowid');
                    }
                } else if (goog.dom.classes.has(target, 'action-view')) {
                    // View
                    document.location = '/post/' +
                        row.getAttribute('data-url');
                }
            } else {
                // Edit
                document.location = '/vault/edit/' +
                    row.getAttribute('data-rowid');
            }
        }
    );
};

/**
 * @private
 */
dikmax.App.prototype.showVaultStatistics_ = function() {
    var postsList = goog.dom.getElementByClass('vault-posts-list');
    if (!postsList) {
        return;
    }

    var rows = goog.dom.getElementsByTagNameAndClass('tr', null, postsList);
    var totalCount = 0;
    var publishedCount = 0;
    goog.array.forEach(rows, function(row) {
        var rowId = row.getAttribute('data-rowid');
        if (!rowId) {
            return;
        }
        // TODO bootstrap-styled hints
        goog.dom.getElementByClass('action-view', row).setAttribute('title',
            'Посмотреть');
        goog.dom.getElementByClass('action-delete', row).setAttribute('title',
            'Удалить');

        ++totalCount;
        if (goog.dom.getTextContent(row.childNodes[1]) === '+') {
            ++publishedCount;
        }
    });

    goog.dom.getElementByClass('vault-posts-count').innerHTML =
        'Записей: ' + totalCount;
    goog.dom.getElementByClass('vault-published-count').innerHTML =
        'Опубликовано: ' + publishedCount;
};

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
 * @param {string=} formData
 * @private
 */
dikmax.App.prototype.renderVaultPreview_ = function(formData) {
    if (!goog.isString(formData)) {
        /** @type {HTMLFormElement} */
        var form = /** @type {HTMLFormElement} */
            (goog.dom.getElementByClass('post-form'));
        formData = goog.dom.forms.getFormDataString(form);
    }

    goog.net.XhrIo.send('/vault/renderpost', function(e) {
        /** @type {goog.net.XhrIo} */
        var request = e.target;
        if (request.isSuccess()) {
            goog.dom.getElementByClass('render-area').innerHTML =
                request.getResponseText();
        }
    }, 'POST', formData);
};

/**
 * @private
 */
dikmax.App.prototype.setupRenderer_ = function() {
    goog.events.listen(goog.dom.getElementByClass('btn-refresh'),
        goog.events.EventType.CLICK, function(e) {
            this.renderVaultPreview_();
            e.preventDefault();
        }, true, this);

    // TODO remove error on cancel button click

    /** @type {HTMLFormElement} */
    var form = /** @type {HTMLFormElement} */
        (goog.dom.getElementByClass('post-form'));

    if (!goog.userAgent.MOBILE) {
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
                    goog.style.showElement(
                        goog.dom.getElementByClass('files-panel'), true
                    );
                    goog.dom.forms.setValue(
                        document.getElementById('file-container'),
                        currentContainer
                    );
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

                        updateFilesList();
                    }
                    console.log(e);
                }
            );
            io.sendFromForm(/** @type {HTMLFormElement} */
                (goog.dom.getElementByClass('upload-form')));
            e.preventDefault();
        }
    );
};
