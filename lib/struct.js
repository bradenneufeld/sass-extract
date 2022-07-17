'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.createStructuredValue = createStructuredValue;

var _util = require('./util');

var _serialize = require('./serialize');

/**
 * Transform a sassValue into a structured value based on the value type
 */
function makeValue(inputSassValue, sass) {
  var sassValue = inputSassValue;
  if (Array.isArray(sassValue) && sassValue.length === 1) sassValue = inputSassValue[0];
  switch ((0, _util.getConstructor)(sassValue, sass)) {
    case sass.types.String:
      if ('_string0$_text' in sassValue) {
        return { value: sassValue['_string0$_text'] };
      } else return { value: sassValue.getValue() };
    case sass.types.Boolean:
      return { value: sassValue.getValue() };

    case sass.types.Number:
      if (sassValue.hasOwnProperty('_single_unit$_unit')) {
        return { value: sassValue['_number1$_value'], unit: sassValue['_single_unit$_unit'] };
      } else if (sassValue.hasOwnProperty('_number1$_value')) {
        return { value: sassValue['_number1$_value'] };
      }
      return { value: sassValue.getValue(), unit: sassValue.getUnit() };

    case sass.types.Color:
      if (sassValue.hasOwnProperty('_color1$_red')) {
        var _r = Math.round(sassValue['_color1$_red']);
        var _g = Math.round(sassValue['_color1$_green']);
        var _b = Math.round(sassValue['_color1$_blue']);
        var _a = Math.round(sassValue['_color1$_alpha']);
        return {
          value: {
            r: _r, g: _g, b: _b,
            a: _a,
            hex: '#' + (0, _util.toColorHex)(_r) + (0, _util.toColorHex)(_g) + (0, _util.toColorHex)(_b)
          }
        };
      }
      var r = Math.round(sassValue.getR());
      var g = Math.round(sassValue.getG());
      var b = Math.round(sassValue.getB());
      var a = sassValue.getA();
      return {
        value: {
          r: r, g: g, b: b,
          a: a,
          hex: '#' + (0, _util.toColorHex)(r) + (0, _util.toColorHex)(g) + (0, _util.toColorHex)(b)
        }
      };

    case sass.types.Null:
      return { value: null };

    case sass.types.List:
      if ('$isSassList0' in sassValue) {
        var _listLength = sassValue['get$lengthAsList']();
        var list = sassValue['get$asList']();
        var _listValue = [];
        for (var i = 0; i < _listLength; i++) {
          _listValue.push(createStructuredValue(list[i], sass));
        }
        return { value: _listValue, separator: sassValue['_list1$_separator'].separator };
      }
      var listLength = sassValue.getLength();
      var listValue = [];
      for (var _i = 0; _i < listLength; _i++) {
        listValue.push(createStructuredValue(sassValue.getValue(_i), sass));
      }
      return { value: listValue, separator: sassValue.getSeparator() ? ',' : ' ' };

    case sass.types.Map:
      if ('_map0$_contents' in sassValue) {
        var _mapLength = sassValue['get$lengthAsList']();
        var mapContents = sassValue['get$asList']();
        for (var key in mapContents[0]) {
          //console.log("OKEY", key)
        }
        var _mapValue = {};
        for (var _i2 = 0; _i2 < _mapLength; _i2++) {
          var _key = mapContents[_i2]['_list1$_contents'][0];
          var value = mapContents[_i2]['_list1$_contents'][1];
          // Serialize map keys of arbitrary type for extracted struct
          var serializedKey = (0, _serialize.serialize)(_key, false, sass);
          _mapValue[serializedKey] = createStructuredValue(value, sass);
        }
        return { value: _mapValue };
      }

      var mapLength = sassValue.getLength();
      var mapValue = {};
      for (var _i3 = 0; _i3 < mapLength; _i3++) {
        // Serialize map keys of arbitrary type for extracted struct
        var _serializedKey = (0, _serialize.serialize)(sassValue.getKey(_i3), false, sass);
        mapValue[_serializedKey] = createStructuredValue(sassValue.getValue(_i3), sass);
      }
      return { value: mapValue };

    default:
      throw new Error('Unsupported sass variable type \'' + sassValue.constructor.name + '\'');
  };
};

/**
 * Create a structured value definition from a sassValue object
 */
function createStructuredValue(sassValue, sass) {
  //console.log("createStructuredValue",sassValue )
  var value = Object.assign({
    type: (0, _util.getConstructorName)(sassValue, sass)
  }, makeValue(sassValue, sass));
  //console.log("value", value)
  return value;
};