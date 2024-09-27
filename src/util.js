import path from 'path';

const NORMALIZED_PATH_SEPARATOR = '/';
const PLATFORM_PATH_SEPARATOR = path.sep;

/**
 * Normalize path across platforms
 */
export function normalizePath(path) {
  return path.split(PLATFORM_PATH_SEPARATOR).join(NORMALIZED_PATH_SEPARATOR);
}

/**
 * Make a potentially relative path absolute relative to cwd
 */
export function makeAbsolute(maybeRelativePath) {
  if(path.isAbsolute(maybeRelativePath)) {
    return maybeRelativePath;
  } else {
    return path.posix.join(process.cwd(), maybeRelativePath);
  }
}

/**
 * Convert a color value 0-255 to hex 00-FF
 */
export function toColorHex(value) {
  let colorHex = Math.round(value).toString(16);

  if(colorHex.length < 2) {
    colorHex = `0${colorHex}`;
  }

  return colorHex;
}

/**
 * Returns the Sass implementation based on the `extractOptions`. Resolves the implementation in the following order: `compileOptions.implementation` || `Node Sass` || `Dart Sass`
 */
export function getSassImplementation(compileOptions = {}) {
  const implementation = compileOptions.implementation || require('sass');

  if(!implementation.info || !['dart-sass'].includes(implementation.info.split('\t')[0])) {
    throw new Error('The given Sass implementation is invalid. Should be `sass`.')
  }

  return implementation;
}

/**
 * The constructor of Dart Sass' Booleans and Null values do not match any of the constructors in `sass.types` in Dart Sass.
 */
export function getConstructor (sassValue, sass) {
  //console.log("sassValue", sassValue)
  //console.log("sassValue unit", sassValue['_single_unit$_unit'])
  //console.log("sassValue value", sassValue['_number1$_value'])
  //console.log("keys", Object.keys(sassValue))
  //console.log("constr", sassValue.constructor)
  for (var key in sassValue) {
    //console.log("SKEY", key)
  }
  //console.log("get$runtimeType", sassValue['get$runtimeType']()._rti._primary)
  //console.log("get$runtimeType keys", Object.keys(sassValue['get$runtimeType']()))
  //console.log("isSingleUnitSassNumber0", sassValue['$isSingleUnitSassNumber0'](sassValue))
  //console.log("get$runtimeType", sassValue['get$runtimeType']())
  //console.log("getConstructor sass unit1", sassValue['_single_unit$_unit'])
  //console.log("getConstructor sass value1", sassValue.toString())
  if ('get$runtimeType' in sassValue) {
    if (sassValue['get$runtimeType']()._rti._primary === 'SingleUnitSassNumber0' ||
      sassValue['get$runtimeType']()._rti._primary === 'UnitlessSassNumber0') {
      //console.log("ISNUMBER")
      return sass.types.Number;
    }
    if (sassValue['get$runtimeType']()._rti._primary === 'SassColor0') {
      //console.log("ISCOLOR")
      return sass.types.Color;
    }
    if (sassValue['get$runtimeType']()._rti._primary === 'SassList0') {
      //console.log("ISLIST")
      return sass.types.List;
    }
    if (sassValue['get$runtimeType']()._rti._primary === 'SassString0') {
      //console.log("ISSTR")
      return sass.types.String;
    }
    if (sassValue['get$runtimeType']()._rti._primary === 'SassMap0') {
      //console.log("ISMAP")
      return sass.types.Map;
    }
  }


  //console.log("getConstructor sass unit1", sassValue['_single_unit$_unit'])
  //console.log("getConstructor sass unit2", Object.keys(sassValue['_single_unit$_unit']))
  //console.log("getConstructor sass unit3", sassValue.getUnit())
  switch (sassValue.constructor) {
    case sass.types.Boolean.TRUE.constructor:
    case sass.types.Boolean.FALSE.constructor: // Both TRUE and FALSE have the same constructor, but for clarity's sake
      return sass.types.Boolean;

    case sass.types.Null.NULL.constructor:
      return sass.types.Null;

    default:
      return sassValue.constructor;
  }
}

/**
 * Returns the constructor name of the given Sass value type.
 * Until 1.2.5, Dart Sass did not report the constructor name in a human readable format, this is why we need to use this helper.
 */
export function getConstructorName (sassValue, sass) {
  //console.log("sass types", sass.types)
  if ( Array.isArray(sassValue) && sassValue.length === 1) sassValue = sassValue[0];
  switch(getConstructor(sassValue, sass)) {
    case sass.types.String:
      return 'SassString';

    case sass.types.Boolean:
      return 'SassBoolean';

    case sass.types.Number:
      return 'SassNumber';

    case sass.types.Color:
      return 'SassColor';

    case sass.types.Null:
      return 'SassNull';

    case sass.types.List:
      return 'SassList';

    case sass.types.Map:
      return 'SassMap';

    default:
      throw new Error(`Unsupported sass constructor '${sassValue.constructor.name}'`)
  }
}
