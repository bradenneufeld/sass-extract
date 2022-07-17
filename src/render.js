import { extract, extractSync } from './extract';
import sass from "sass";

/**
 * Render with node-sass using provided compile options and augment variable extraction
 */
export function render(compileOptions = {}, extractOptions = {}) {
  const sass = require('sass');
  const rendered = sass.renderSync(compileOptions);
  rendered.vars = extractSync(rendered, { compileOptions, extractOptions })
  return rendered;
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
