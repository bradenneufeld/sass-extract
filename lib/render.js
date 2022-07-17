'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.render = render;
exports.renderSync = renderSync;

var _extract = require('./extract');

var _util = require('./util');

/**
 * Render with node-sass using provided compile options and augment variable extraction
 */
async function render() {
  var compileOptions = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : {};
  var extractOptions = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {};

  var sass = require('sass');
  return new Promise(function (resolve) {
    //console.log("promisedcompileOptions", compileOptions)
    sass.render(compileOptions, function (except, rendered) {
      //console.log("renderedEND", rendered)
      return (0, _extract.extract)(rendered, { compileOptions: compileOptions, extractOptions: extractOptions }).then(function (vars) {
        //console.log("extractEND", vars)
        rendered.vars = vars;
        resolve(rendered);
      });
    });
  });
}

/**
 * Render synchronously with node-sass using provided compile options and augment variable extraction
 */
function renderSync() {
  var compileOptions = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : {};
  var extractOptions = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {};

  //console.log("renderSync")
  var sass = require('sass');
  //console.log("renderedcompileOptions", compileOptions)
  compileOptions.file = (0, _util.makeAbsolute)((0, _util.normalizePath)(compileOptions.file));
  //console.log("renderedcompileOptions2", compileOptions)
  var rendered = sass.renderSync(compileOptions);
  rendered.vars = (0, _extract.extractSync)(rendered, { compileOptions: compileOptions, extractOptions: extractOptions });
  //console.log("extractSync")
  return rendered;
}