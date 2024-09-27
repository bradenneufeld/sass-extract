'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.serialize = serialize;

var _util = require('./util');

var _parseColor = require('parse-color');

var _parseColor2 = _interopRequireDefault(_parseColor);

var _struct = require('./struct');

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

/**
 * Serialize a given sass color into a color name like 'white', an rgba(r,g,b,a), or #000000 string
 * based on the color provided
 */
function serializeColor(sassColor) {
  if (sassValue.hasOwnProperty('_color1$_red')) {
    var _r = Math.round(sassValue['_color1$_red']);
    var _g = Math.round(sassValue['_color1$_green']);
    var _b = Math.round(sassValue['_color1$_blue']);
    var a = Math.round(sassValue['_color1$_alpha']);
  } else {
    var _alpha = Math.round(sassColor.getA() * 100) / 100;
    var _r2 = Math.round(sassColor.getR());
    var _g2 = Math.round(sassColor.getG());
    var _b2 = Math.round(sassColor.getB());
  }

  if (alpha < 0.999) {
    return 'rgba(' + r + ',' + g + ',' + b + ',' + alpha + ')';
  } else {
    var hex = '#' + (0, _util.toColorHex)(r) + (0, _util.toColorHex)(g) + (0, _util.toColorHex)(b);
    var parsedColor = (0, _parseColor2.default)(hex);
    if (parsedColor.keyword != null) {
      return parsedColor.keyword;
    } else {
      return hex;
    }
  }
}

/**
 * Transform a SassValue into a serialized string
 */
function serializeValue(sassValue, isInList, sass) {
  //console.log("serializeValue", sassValue)
  //console.log("sassValue type", typeof sassValue)
  switch ((0, _util.getConstructor)(sassValue, sass)) {
    case sass.types.String:
      if ('_string0$_text' in sassValue) {
        return sassValue['_string0$_text'];
      }
      return '' + sassValue.getValue();
    case sass.types.Boolean:
      return '' + sassValue.getValue();

    case sass.types.Number:
      if (sassValue.hasOwnProperty('_single_unit$_unit')) {
        return '' + sassValue['_number1$_value'] + sassValue['_single_unit$_unit'];
      } else if (sassValue.hasOwnProperty('_number1$_value')) {
        return sassValue['_number1$_value'];
      }
      return '' + sassValue.getValue() + sassValue.getUnit();

    case sass.types.Color:
      return serializeColor(sassValue);

    case sass.types.Null:
      return 'null';

    case sass.types.List:
      if ('$isSassList0' in sassValue) {
        var _listLength = sassValue['get$lengthAsList']();
        var list = sassValue['get$asList']();
        var _listElement = [];
        for (var i = 0; i < _listLength; i++) {
          _listElement.push(serialize(list[i], true, sass));
        }
        if (isInList) {
          return '(' + _listElement.join(sassValue['_list1$_separator'].separator) + ')';
        } else {
          return '' + _listElement.join(sassValue['_list1$_separator'].separator);
        }
      }
      var listLength = sassValue.getLength();
      var listElement = [];
      var hasSeparator = sassValue.getSeparator();
      for (var _i = 0; _i < listLength; _i++) {
        listElement.push(serialize(sassValue.getValue(_i), true, sass));
      }
      // Make sure nested lists are serialized with surrounding parenthesis
      if (isInList) {
        return '(' + listElement.join(hasSeparator ? ',' : ' ') + ')';
      } else {
        return '' + listElement.join(hasSeparator ? ',' : ' ');
      }

    case sass.types.Map:
      if ('_map0$_contents' in sassValue) {
        var _mapLength = sassValue['get$lengthAsList']();
        var mapContents = sassValue['get$asList']();
        for (var key in mapContents[0]) {
          //console.log("OKEY", key)
        }
        var _mapValue = {};
        for (var _i2 = 0; _i2 < _mapLength; _i2++) {
          var rawkey = mapContents[_i2]['_list1$_contents'][0];
          var rawvalue = mapContents[_i2]['_list1$_contents'][1];
          var _key = serialize(rawkey, false, sass);
          var value = serialize(rawvalue, false, sass);
          _mapValue[_key] = value;
        }
        var _serializedMapValues = Object.keys(_mapValue).map(function (key) {
          return key + ': ' + _mapValue[key];
        });
        return '(' + _serializedMapValues + ')';
      }
      var mapLength = sassValue.getLength();
      var mapValue = {};
      for (var _i3 = 0; _i3 < mapLength; _i3++) {
        var _key2 = serialize(sassValue.getKey(_i3), false, sass);
        var _value = serialize(sassValue.getValue(_i3), false, sass);
        mapValue[_key2] = _value;
      }
      var serializedMapValues = Object.keys(mapValue).map(function (key) {
        return key + ': ' + mapValue[key];
      });
      return '(' + serializedMapValues + ')';

    default:
      throw new Error('Unsupported sass variable type \'' + sassValue.constructor.name + '\'');
  };
};

/**
 * Create a serialized string from a sassValue object
 */
function serialize(sassValue, isInList, sass) {
  return serializeValue(sassValue, isInList, sass);
};