goog.provide('dikmax.FootnotePopover');

goog.require('dikmax.FootnotePosition');
goog.require('dikmax.Templates');
goog.require('goog.dom.classes');
goog.require('goog.events');
goog.require('goog.fx.css3');
goog.require('goog.fx.dom');
goog.require('goog.positioning');
goog.require('goog.soy');
goog.require('goog.style.transition');
goog.require('goog.ui.HoverCard');



/**
 * @constructor
 * @extends {goog.ui.HoverCard}
 */
dikmax.FootnotePopover = function() {
  goog.base(this, function(el) {
    return goog.dom.classes.has(el, 'note-link');
  }, false);

  this.footnotes = {};
  this.initData_();

  var footnoteElement = goog.soy.renderAsElement(dikmax.Templates.footnote);
  this.setElement(footnoteElement);
  footnoteElement.style.display = 'none';
  this.setPinnedCorner(goog.positioning.Corner.TOP_RIGHT);
  this.className = 'popover fade top in';

  this.setCursorTracking(true);
  this.setHideDelayMs(500);

  if (goog.style.transition.isSupported()) {
    this.setTransition(goog.fx.css3.fadeIn(footnoteElement, 0.15),
        goog.fx.css3.fadeOut(footnoteElement, 0.15));
  } else {
    this.setTransition(new goog.fx.dom.FadeIn(footnoteElement, 150),
        new goog.fx.dom.FadeOut(footnoteElement, 150));
  }

  goog.events.listen(this, goog.ui.HoverCard.EventType.TRIGGER,
      function(event) {
        this.setPosition(new dikmax.FootnotePosition(event.anchor));
        var id = event.anchor.getAttribute('id');
        goog.dom.getElementByClass('popover-content', footnoteElement).
            innerHTML = this.footnotes[id].content;
        goog.dom.getElementByClass('popover-title', footnoteElement).
            innerHTML = 'Примечание ' + this.footnotes[id].number;
        return true;
      }, false, this
  );
};
goog.inherits(dikmax.FootnotePopover, goog.ui.HoverCard);


/**
 * @type {Object.<string, {number: string, content: string}>}
 */
dikmax.FootnotePopover.prototype.footnotes = null;


/**
 * @override
 */
dikmax.FootnotePopover.prototype.showPopupElement = function() {
  var el = this.getElement();
  el.style.visibility = 'visible';
  el.style.display = 'block';
};


/**
 * @private
 */
dikmax.FootnotePopover.prototype.initData_ = function() {
  goog.array.forEach(goog.dom.getElementsByClass('footnotes'), function(item) {
    goog.array.forEach(goog.dom.getElementsByTagNameAndClass('li', null, item),
        function(item) {
          var forId = item.getAttribute('data-for');
          if (!forId) {
            return;
          }
          var el = goog.dom.getElement(forId);
          this.footnotes[forId] = {
            number: goog.dom.getTextContent(el),
            content: item.innerHTML
          };
        }, this
    );
    goog.dom.removeNode(item);
  }, this);
};
