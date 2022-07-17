import { injectExtractionFunctions } from './inject';
import { parseDeclarations } from './parse';
import { Pluggable } from './pluggable';

/**
 * Get a string id for a filename
 */
function getFileId(filename) {
  return new Buffer.from(filename).toString('base64').replace(/=/g, '');
}

function parseFile(filename, data) {
  //console.log("parseFile", data)
  return parseDeclarations(data);
}

function getDependentDeclarations(filename, declarations) {
  const fileId = getFileId(filename);
  const dependentDeclarations = [];

  declarations.explicitGlobals.forEach(declaration => {
    if(Object.keys(declaration.deps).length > 0) {
      dependentDeclarations.push({ filename, declaration, decFileId: fileId });
    }
  });

  return dependentDeclarations;
}

/**
 * Process a single sass files to get declarations, injected source and functions
 */
function processFile(idx, count, filename, data, parsedDeclarations, pluggable, sass) {
  const declarations = parsedDeclarations.files[filename];
  // Inject dependent declaration extraction to last file
  const dependentDeclarations = idx === count - 1 ? parsedDeclarations.dependentDeclarations : [];
  const variables = { global: {} };

  const globalDeclarationResultHandler = (declaration, value, sassValue) => {
    //console.log("globalDeclarationResultHandler");
    //console.trace();
    if(!variables.global[declaration.declaration]) {
      variables.global[declaration.declaration] = [];
    }
    const variableValue = pluggable.run(Pluggable.POST_VALUE, { value, sassValue, sass }).value;
    variables.global[declaration.declaration].push({ declaration, value: variableValue });
  }

  const fileId = getFileId(filename);
  const injection = injectExtractionFunctions(fileId, declarations, dependentDeclarations, { globalDeclarationResultHandler }, sass);
  //console.log("OBJECTION", injection);
  const injectedData = `${data}\n\n${injection.injectedData}`;
  const injectedFunctions = injection.injectedFunctions;

  return {
    fileId,
    declarations,
    variables,
    injectedData,
    injectedFunctions,
  };
}

export function parseFiles(files) {
  //console.log("parseFiles")
  const parsedDeclarations = {
    files: {},
    dependentDeclarations: [],
  };

  Object.keys(files).map(filename => {
    //console.log("filename", filename)
    const fileDeclarations = parseFile(filename, files[filename]);
    //console.log("parseFile", fileDeclarations)
    parsedDeclarations.files[filename] = fileDeclarations;
    parsedDeclarations.dependentDeclarations.push(...getDependentDeclarations(filename, fileDeclarations))
  });
  //console.log("OBJECYSK")

  return parsedDeclarations;
}

/**
 * Process a set of sass files to get declarations, injected source and functions
 * Files are provided in a map of filename -> key entries
 */
export function processFiles(orderedFiles, files, parsedDeclarations, pluggable, sass) {
  const extractions = {};

  orderedFiles.forEach((filename, idx) => {
    extractions[filename] = processFile(idx, orderedFiles.length, filename, files[filename], parsedDeclarations, pluggable, sass);
  });

  return extractions;
}
