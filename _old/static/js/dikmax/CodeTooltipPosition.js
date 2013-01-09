goog.provide('dikmax.CodeTooltipPosition');

goog.require('goog.positioning');
goog.require('goog.positioning.AbstractPosition');
goog.require('goog.style');



/**
 * @param {Element} trigger Element which triggers tooltip.
 * @constructor
 * @extends {goog.positioning.AbstractPosition}
 */
dikmax.CodeTooltipPosition = function(trigger) {
  /**
     * @type {Element}
     */
  this.trigger = trigger;
  goog.base(this);
};
goog.inherits(dikmax.CodeTooltipPosition, goog.positioning.AbstractPosition);


/**
 * @inheritDoc
 */
dikmax.CodeTooltipPosition.prototype.reposition = function(movableElement) {
  goog.positioning.positionAtAnchor(this.trigger,
      goog.positioning.Corner.TOP_LEFT,
      movableElement,
      goog.positioning.Corner.TOP_RIGHT,
      undefined,
      new goog.math.Box(-9, 0, 0, 0));
};
