import { createStructuredValue } from './struct';

const FN_PREFIX = '___SV_INJECT';
const FN_PREFIX_IMPLICIT_GLOBAL = 'IG';
const FN_PREFIX_EXPLICIT_GLOBAL = 'EG';
const FN_PREFIX_DEPENDENT_GLOBAL = 'DG';
const FN_SUFFIX_VALUE = 'VALUE';

/**
 * Create injection function and source for a file, category, declaration and result handler
 */
function createInjection(fileId, categoryPrefix, declaration, idx, declarationResultHandler, sass) {
  const fnName = `${FN_PREFIX}_${fileId}_${categoryPrefix}_${declaration.declarationClean}_${idx}`;
  const fnSignature = `${fnName}(${declaration.declaration})`;

  const injectedFunction = function(inputSassValue) {
    var sassValue = inputSassValue;
    if ( Array.isArray(sassValue) && sassValue.length === 1) sassValue = inputSassValue[0];
    const value = createStructuredValue(sassValue, sass);
    declarationResultHandler(declaration, value, sassValue);
    return sassValue;
  };

  let injectedCode = `@if global_variable_exists('${declaration.declarationClean}') { 
    $${fnName}: ${fnName}(${declaration.declaration}); 
  }\n`

  return { fnName, fnSignature, injectedFunction, injectedCode };
}

/**
 * Create injection functions for extraction variable values
 * Returns injection sass source and the injected functions
 * Declaration result handlers will be called with the extracted value of each declaration
 * Provided file id will be used to ensure unique function names per file
 */
export function injectExtractionFunctions(fileId, declarations, dependentDeclarations, { globalDeclarationResultHandler }, sass) {
  let injectedData = ``;
  const injectedFunctions = {};

  // Create injections for implicit global variables
  declarations.implicitGlobals.forEach((declaration, idx) => {
    const { fnName, fnSignature, injectedFunction, injectedCode } = createInjection(fileId, FN_PREFIX_IMPLICIT_GLOBAL, declaration, idx, globalDeclarationResultHandler, sass);
    injectedFunctions[fnSignature] = injectedFunction;
    injectedData += injectedCode;
  });

  // Create injections for explicit global variables
  declarations.explicitGlobals.forEach((declaration, idx) => {
    const { fnName, fnSignature, injectedFunction, injectedCode } = createInjection(fileId, FN_PREFIX_EXPLICIT_GLOBAL, declaration, idx, globalDeclarationResultHandler, sass);
    injectedFunctions[fnSignature] = injectedFunction;
    injectedData += injectedCode;
  });

  dependentDeclarations.forEach(({ declaration, decFileId }, idx) => {
    // Do not add dependent injection if the declaration is in the current file
    // It will already be added by explicits
    if(decFileId === fileId) { return; }
    const { fnName, fnSignature, injectedFunction, injectedCode } = createInjection(fileId, FN_PREFIX_DEPENDENT_GLOBAL, declaration, idx, globalDeclarationResultHandler, sass);
    injectedFunctions[fnSignature] = injectedFunction;
    injectedData += injectedCode;
  });  

  return { injectedData, injectedFunctions };
}
