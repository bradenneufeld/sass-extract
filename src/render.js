import { extract, extractSync } from './extract';

/**
 * Render with node-sass using provided compile options and augment variable extraction
 */
export function render(compileOptions = {}, extractOptions = {}) {
  const sass = require('sass');
  return sass.render(compileOptions, function(err, rendered) {
    return extract(rendered, { compileOptions, extractOptions })
    .then(vars => {
      rendered.vars = vars;
      return rendered;
    });
  });
}

/**
 * Render synchronously with node-sass using provided compile options and augment variable extraction
 */
export function renderSync(compileOptions = {}, extractOptions = {}) {
  const sass = require('sass');
  const rendered = sass.renderSync(compileOptions);
  rendered.vars = extractSync(rendered, { compileOptions, extractOptions })
  return rendered;
}
