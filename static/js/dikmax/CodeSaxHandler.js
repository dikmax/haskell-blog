goog.provide('dikmax.CodeSaxHandler');

goog.require('goog.array');
goog.require('goog.string.StringBuffer');
goog.require('goog.string.html.HtmlSaxHandler');



/**
 * @constructor
 * @extends {goog.string.html.HtmlSaxHandler}
 */
dikmax.CodeSaxHandler = function() {

};
goog.inherits(dikmax.CodeSaxHandler, goog.string.html.HtmlSaxHandler);


/**
 * @type {Array.<string>}
 * @private
 */
dikmax.CodeSaxHandler.prototype.lines_ = null;


/**
 * @type {goog.string.StringBuffer}
 * @private
 */
dikmax.CodeSaxHandler.prototype.currentLine_ = null;


/**
 * @type {Array.<{tag: string, attributes: string}>}
 * @private
 */
dikmax.CodeSaxHandler.prototype.stack_ = null;


/**
 * @inheritDoc
 */
dikmax.CodeSaxHandler.prototype.cdata = function(text) {
  // Ignoring
};


/**
 * @inheritDoc
 */
dikmax.CodeSaxHandler.prototype.endTag = function(name) {
  this.stack_.pop();
  this.currentLine_.append('</', name, '>');
};


/**
 * @inheritDoc
 */
dikmax.CodeSaxHandler.prototype.pcdata = function(text) {
  if (goog.string.contains(text, '\n')) {
    var lines = text.split('\n');
    var last = lines.length - 1;
    goog.array.forEach(lines, function(line, i) {
      if (i != 0) {
        goog.array.forEach(this.stack_, function(item) {
          this.currentLine_.append(
              '<', item.tag, item.attributes, '>'
          );
        }, this);
      }
      this.currentLine_.append(line);
      if (i != last) {
        goog.array.forEachRight(this.stack_, function(item) {
          this.currentLine_.append(
              '</', item.tag, '>'
          );
        }, this);
        this.lines_.push(this.currentLine_.toString());
        this.currentLine_ = new goog.string.StringBuffer();
      }
    }, this);
  } else {
    this.currentLine_.append(text);
  }
};


/**
 * @inheritDoc
 */
dikmax.CodeSaxHandler.prototype.rcdata = function(text) {
  // Ignoring
};


/**
 * @inheritDoc
 */
dikmax.CodeSaxHandler.prototype.startTag = function(name, attrs) {
  /** @type {goog.string.StringBuffer} */
  var attributes = new goog.string.StringBuffer();
  for (var i = 0; i < attrs.length; i += 2) {
    attributes.append(' ', attrs[i], '="',
        goog.string.htmlEscape(attrs[i + 1], false), '"');
  }
  var attributesString = attributes.toString();
  this.stack_.push({
    tag: name,
    attributes: attributesString
  });
  this.currentLine_.append('<', name, attributesString, '>');
  //console.dir(attrs[1]);
};


/**
 * @inheritDoc
 */
dikmax.CodeSaxHandler.prototype.startDoc = function() {
  this.lines_ = [];
  this.currentLine_ = new goog.string.StringBuffer();
  this.stack_ = [];
};


/**
 * @inheritDoc
 */
dikmax.CodeSaxHandler.prototype.endDoc = function() {
  var str = this.currentLine_.toString();
  if (str) {
    this.lines_.push(str);
  }
};


/**
 * @return {Array.<string>} Returns array with lines of code block.
 */
dikmax.CodeSaxHandler.prototype.getLines = function() {
  return this.lines_;
};
