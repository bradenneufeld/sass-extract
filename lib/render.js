'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.render = render;
exports.renderSync = renderSync;

var _extract = require('./extract');

var _sass = require('sass');

var _sass2 = _interopRequireDefault(_sass);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

/**
 * Render with node-sass using provided compile options and augment variable extraction
 */
function render() {
  var compileOptions = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : {};
  var extractOptions = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {};

  var sass = require('sass');
  var rendered = sass.renderSync(compileOptions);
  rendered.vars = (0, _extract.extractSync)(rendered, { compileOptions: compileOptions, extractOptions: extractOptions });
  return rendered;
}

/**
 * Render synchronously with node-sass using provided compile options and augment variable extraction
 */
function renderSync() {
  var compileOptions = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : {};
  var extractOptions = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {};

  var sass = require('sass');
  var rendered = sass.renderSync(compileOptions);
  rendered.vars = (0, _extract.extractSync)(rendered, { compileOptions: compileOptions, extractOptions: extractOptions });
  return rendered;
}