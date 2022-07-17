import { extract, extractSync } from './extract';
import { normalizePath, makeAbsolute } from './util';

/**
 * Render with node-sass using provided compile options and augment variable extraction
 */
export async function render(compileOptions = {}, extractOptions = {}) {
  const sass = require('sass');
  return new Promise((resolve) => {
    //console.log("promisedcompileOptions", compileOptions)
    sass.render(compileOptions, (except, rendered) => {
      //console.log("renderedEND", rendered)
      return extract(rendered, {compileOptions, extractOptions})
        .then(vars => {
          //console.log("extractEND", vars)
          rendered.vars = vars;
          resolve(rendered);
        });
    })
  });
}

/**
 * Render synchronously with node-sass using provided compile options and augment variable extraction
 */
export function renderSync(compileOptions = {}, extractOptions = {}) {
  //console.log("renderSync")
  const sass = require('sass');
  //console.log("renderedcompileOptions", compileOptions)
  compileOptions.file = makeAbsolute(normalizePath(compileOptions.file));
  //console.log("renderedcompileOptions2", compileOptions)
  const rendered = sass.renderSync(compileOptions);
  rendered.vars = extractSync(rendered, { compileOptions, extractOptions })
  //console.log("extractSync")
  return rendered;
}
