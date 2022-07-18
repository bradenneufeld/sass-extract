'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.extract = extract;
exports.extractSync = extractSync;

var _util = require('./util');

var _load = require('./load');

var _process = require('./process');

var _importer = require('./importer');

var _pluggable = require('./pluggable');

var _sass = require('sass');

var _sass2 = _interopRequireDefault(_sass);

var _nodeUrl = require('node:url');

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _toConsumableArray(arr) { if (Array.isArray(arr)) { for (var i = 0, arr2 = Array(arr.length); i < arr.length; i++) { arr2[i] = arr[i]; } return arr2; } else { return Array.from(arr); } }

var path = require('path');

/**
 * Get rendered stats required for extraction
 */
function getRenderedStats(rendered, compileOptions) {
  return {
    entryFilename: (0, _util.normalizePath)((0, _util.makeAbsolute)(rendered.stats.entry)),
    includedFiles: rendered.stats.includedFiles.map(function (f) {
      return (0, _util.normalizePath)((0, _util.makeAbsolute)(f));
    }),
    includedPaths: (compileOptions.includePaths || []).map(_util.normalizePath)
  };
}

/**
 * Make the compilation option for the extraction rendering
 * Set the data to be rendered to the injected source
 * Add compilation functions and custom importer for injected sources
 */
function makeExtractionCompileOptions(compileOptions, entryFilename, extractions, importer) {
  var extractionCompileOptions = Object.assign({}, compileOptions);
  var extractionFunctions = {};

  // Copy all extraction function for each file into one object for compilation
  Object.keys(extractions).forEach(function (extractionKey) {
    Object.assign(extractionFunctions, extractions[extractionKey].injectedFunctions);
  });

  extractionCompileOptions.functions = Object.assign(extractionFunctions, compileOptions.functions);
  extractionCompileOptions.data = extractions[entryFilename].injectedData;
  if (!makeExtractionCompileOptions.imported) {
    extractionCompileOptions.importer = importer;
  }

  return extractionCompileOptions;
}

/**
 * Compile extracted variables per file into a complete result object
 */
function compileExtractionResult(orderedFiles, extractions) {
  // the extractions here should already contain global vars, but they don't for eui
  var extractedVariables = { global: {} };

  orderedFiles.map(function (filename) {
    var globalFileVariables = extractions[filename].variables.global;

    Object.keys(globalFileVariables).map(function (variableKey) {
      globalFileVariables[variableKey].forEach(function (extractedVariable) {
        var variable = extractedVariables.global[variableKey];
        var currentVariableSources = [];
        var currentVariableDeclarations = [];

        if (variable) {
          currentVariableSources = variable.sources;
          currentVariableDeclarations = variable.declarations;
        }

        var hasOnlyDefaults = currentVariableDeclarations.every(function (declaration) {
          return declaration.flags.default;
        });
        var currentIsDefault = extractedVariable.declaration.flags.default;

        if (currentVariableDeclarations.length === 0 || !currentIsDefault || hasOnlyDefaults) {
          variable = extractedVariables.global[variableKey] = Object.assign({}, extractedVariable.value);
        }
        variable.sources = currentVariableSources.indexOf(filename) < 0 ? [].concat(_toConsumableArray(currentVariableSources), [filename]) : currentVariableSources;
        variable.declarations = [].concat(_toConsumableArray(currentVariableDeclarations), [{
          expression: extractedVariable.declaration.expression,
          flags: extractedVariable.declaration.flags,
          in: filename,
          position: extractedVariable.declaration.position
        }]);
      });
    });
  });

  return extractedVariables;
}

/**
 * Extract the variables from already rendered sass file(s)
 * Returns the extracted variables
 */
function extract(rendered) {
  var _ref = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {},
      _ref$compileOptions = _ref.compileOptions,
      compileOptions = _ref$compileOptions === undefined ? {} : _ref$compileOptions,
      _ref$extractOptions = _ref.extractOptions,
      extractOptions = _ref$extractOptions === undefined ? {} : _ref$extractOptions;

  var pluggable = new _pluggable.Pluggable(extractOptions.plugins).init();
  var sass = require('sass');

  var _getRenderedStats = getRenderedStats(rendered, compileOptions),
      entryFilename = _getRenderedStats.entryFilename,
      includedFiles = _getRenderedStats.includedFiles,
      includedPaths = _getRenderedStats.includedPaths;

  return (0, _load.loadCompiledFiles)(includedFiles, entryFilename, compileOptions.data).then(function (_ref2) {
    var compiledFiles = _ref2.compiledFiles,
        orderedFiles = _ref2.orderedFiles;

    var parsedDeclarations = (0, _process.parseFiles)(compiledFiles);
    var extractions = (0, _process.processFiles)(orderedFiles, compiledFiles, parsedDeclarations, pluggable, sass);
    var importer = (0, _importer.makeImporter)(extractions, includedFiles, includedPaths, compileOptions.importer);
    var extractionCompileOptions = makeExtractionCompileOptions(compileOptions, entryFilename, extractions, importer);
    var importer2 = {
      findFileUrl: function findFileUrl(url) {
        try {
          var newURL = new URL(url, 'file:' + path.parse(entryFilename).dir + '/');
          return newURL;
        } catch (error) {
          console.log(error);
        }
        return new URL(url);
      }
    };

    var importer3 = {
      canonicalize: function canonicalize(url) {
        console.log("canonicalize", url)
        var extension = '';
        if (!url.endsWith('.scss')) {
          extension = '.scss';
        }
        var newURL = new URL(url + extension, 'file:' + path.parse(entryFilename).dir + '/');
        console.log("newURL", newURL)
        return newURL;
      },
      load: function load(canonicalUrl) {
        return {
          contents: extractions[canonicalUrl.pathname].injectedData,
          syntax: 'scss'
        };
      }
    };
    return sass.compileStringAsync(extractionCompileOptions.data, {
      functions: extractionCompileOptions.functions,
      importers: [importer3],
      importer: importer3
    }).then(function () {
      return pluggable.run(_pluggable.Pluggable.POST_EXTRACT, compileExtractionResult(orderedFiles, extractions));
    });
  });
}

/**
 * Synchronously extract the variables from already rendered sass file(s)
 * Returns the extracted variables
 */
function extractSync(rendered) {
  var _ref3 = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {},
      _ref3$compileOptions = _ref3.compileOptions,
      compileOptions = _ref3$compileOptions === undefined ? {} : _ref3$compileOptions,
      _ref3$extractOptions = _ref3.extractOptions,
      extractOptions = _ref3$extractOptions === undefined ? {} : _ref3$extractOptions;

  var pluggable = new _pluggable.Pluggable(extractOptions.plugins).init();
  var sass = require('sass');

  var _getRenderedStats2 = getRenderedStats(rendered, compileOptions),
      entryFilename = _getRenderedStats2.entryFilename,
      includedFiles = _getRenderedStats2.includedFiles,
      includedPaths = _getRenderedStats2.includedPaths;

  var _loadCompiledFilesSyn = (0, _load.loadCompiledFilesSync)(includedFiles, entryFilename, compileOptions.data),
      compiledFiles = _loadCompiledFilesSyn.compiledFiles,
      orderedFiles = _loadCompiledFilesSyn.orderedFiles;

  var parsedDeclarations = (0, _process.parseFiles)(compiledFiles);
  //console.log("parsedDeclarations", parsedDeclarations)
  var extractions = (0, _process.processFiles)(orderedFiles, compiledFiles, parsedDeclarations, pluggable, sass);
  // should not have globals here
  var importer = (0, _importer.makeSyncImporter)(extractions, includedFiles, includedPaths, compileOptions.importer);
  //console.log("importer", importer)
  var extractionCompileOptions = makeExtractionCompileOptions(compileOptions, entryFilename, extractions, importer);

  var importer3 = {
    canonicalize: function canonicalize(url) {
      var extension = '';
      if (!url.endsWith('.scss')) {
        extension = '.scss';
      }
      var newURL = new URL(url + extension, 'file:' + path.parse(entryFilename).dir + '/');
      return newURL;
    },
    load: function load(canonicalUrl) {
      return {
        contents: extractions[canonicalUrl.pathname].injectedData,
        syntax: 'scss'
      };
    }
  };
  try {
    sass.compileString(extractionCompileOptions.data, {
      functions: extractionCompileOptions.functions,
      importers: [importer3],
      importer: importer3
    });
  } catch (e) {
    console.log(e);
  }

  return pluggable.run(_pluggable.Pluggable.POST_EXTRACT, compileExtractionResult(orderedFiles, extractions));
}
