import { extract, extractSync } from './extract';

/**
 * Render with node-sass using provided compile options and augment variable extraction
 */
export async function render(compileOptions = {}, extractOptions = {}) {
  return renderSync(compileOptions, extractOptions);
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
