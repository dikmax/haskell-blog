goog.provide('dikmax.FootnotePosition');

goog.require('goog.positioning');
goog.require('goog.positioning.AbstractPosition');
goog.require('goog.style');



/**
 * @param {Element} trigger Element which triggers footnote.
 * @constructor
 * @extends {goog.positioning.AbstractPosition}
 */
dikmax.FootnotePosition = function(trigger) {
  /**
     * @type {Element}
     */
  this.trigger = trigger;
  goog.base(this);
};
goog.inherits(dikmax.FootnotePosition, goog.positioning.AbstractPosition);


/**
 * @inheritDoc
 */
dikmax.FootnotePosition.prototype.reposition = function(movableElement) {
  /** @type {goog.math.Rect} */
  var movableBounds = goog.style.getBounds(movableElement);
  /** @type {goog.math.Rect} */
  var anchorBounds = goog.style.getBounds(this.trigger);
  goog.positioning.positionAtAnchor(this.trigger,
      goog.positioning.Corner.TOP_LEFT,
      movableElement,
      goog.positioning.Corner.BOTTOM_LEFT,
      undefined,
      new goog.math.Box(0, 0, 10,
          (anchorBounds.width - movableBounds.width) / 2));
};
