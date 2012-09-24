goog.provide('dikmax.CodeTooltip');

goog.require('dikmax.CodeTooltipPosition');
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
dikmax.CodeTooltip = function() {
  goog.base(this, function(el) {
    return goog.dom.classes.has(el, 'line') &&
        el.getAttribute('data-linenum');

  }, false);

  var tooltipElement = goog.soy.renderAsElement(dikmax.Templates.codeTooltip);
  this.setElement(tooltipElement);
  tooltipElement.style.display = 'none';
  this.setPinnedCorner(goog.positioning.Corner.TOP_RIGHT);
  this.className = 'tooltip fade left in';

  this.setCursorTracking(true);
  this.setHideDelayMs(500);

  if (goog.style.transition.isSupported()) {
    this.setTransition(goog.fx.css3.fadeIn(tooltipElement, 0.15),
        goog.fx.css3.fadeOut(tooltipElement, 0.15));
  } else {
    this.setTransition(new goog.fx.dom.FadeIn(tooltipElement, 150),
        new goog.fx.dom.FadeOut(tooltipElement, 150));
  }

  goog.events.listen(this, goog.ui.HoverCard.EventType.TRIGGER,
      function(event) {
        this.setPosition(new dikmax.CodeTooltipPosition(event.anchor));
        var lineNum = event.anchor.getAttribute('data-linenum');
        goog.dom.getElementByClass('tooltip-inner', tooltipElement).
            innerHTML = '#' + lineNum;
        return true;
      }, false, this
  );
};
goog.inherits(dikmax.CodeTooltip, goog.ui.HoverCard);


/**
 * @override
 */
dikmax.CodeTooltip.prototype.showPopupElement = function() {
  var el = this.getElement();
  el.style.visibility = 'visible';
  el.style.display = 'block';
};
