import { getConstructor, toColorHex } from './util';
import parseColor from 'parse-color';
import {createStructuredValue} from "./struct";

/**
 * Serialize a given sass color into a color name like 'white', an rgba(r,g,b,a), or #000000 string
 * based on the color provided
 */
function serializeColor(sassColor) {
  if (sassValue.hasOwnProperty('_color1$_red')) {
    const r = Math.round(sassValue['_color1$_red']);
    const g = Math.round(sassValue[`_color1$_green`]);
    const b = Math.round(sassValue['_color1$_blue']);
    const a = Math.round(sassValue['_color1$_alpha']);
  }
  else {
    const alpha = Math.round(sassColor.getA() * 100) / 100;
    const r = Math.round(sassColor.getR());
    const g = Math.round(sassColor.getG());
    const b = Math.round(sassColor.getB());
  }

  if(alpha < 0.999) {
    return `rgba(${r},${g},${b},${alpha})`;
  } else {
    const hex = `#${toColorHex(r)}${toColorHex(g)}${toColorHex(b)}`;
    const parsedColor = parseColor(hex);
    if(parsedColor.keyword != null) {
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
  switch(getConstructor(sassValue, sass)) {
    case sass.types.String:
      if ('_string0$_text' in sassValue) {
        return sassValue['_string0$_text'];
      }
      return `${sassValue.getValue()}`;
    case sass.types.Boolean:
      return `${sassValue.getValue()}`;

    case sass.types.Number:
      if (sassValue.hasOwnProperty('_single_unit$_unit')) {
        return `${sassValue['_number1$_value']}${sassValue['_single_unit$_unit']}`;
      }
      else if (sassValue.hasOwnProperty('_number1$_value')) {
        return sassValue['_number1$_value'];
      }
      return `${sassValue.getValue()}${sassValue.getUnit()}`;

    case sass.types.Color:
      return serializeColor(sassValue);

    case sass.types.Null: 
      return `null`;

    case sass.types.List:
      if ('$isSassList0' in sassValue) {
        const listLength = sassValue['get$lengthAsList']();
        const list = sassValue['get$asList']();
        const listElement = [];
        for(let i = 0; i < listLength; i++) {
          listElement.push(serialize(list[i], true, sass));
        }
        if(isInList) {
          return `(${listElement.join(sassValue['_list1$_separator'].separator)})`;
        } else {
          return `${listElement.join(sassValue['_list1$_separator'].separator)}`;
        }
      }
      const listLength = sassValue.getLength();
      const listElement = [];
      const hasSeparator = sassValue.getSeparator();
      for(let i = 0; i < listLength; i++) {
        listElement.push(serialize(sassValue.getValue(i), true, sass));
      }
      // Make sure nested lists are serialized with surrounding parenthesis
      if(isInList) {
        return `(${listElement.join(hasSeparator ? ',' : ' ')})`;
      } else {
        return `${listElement.join(hasSeparator ? ',' : ' ')}`;
      }

    case sass.types.Map:
      if ('_map0$_contents' in sassValue) {
        const mapLength = sassValue['get$lengthAsList']();
        const mapContents = sassValue['get$asList']();
        for (var key in mapContents[0]) {
          //console.log("OKEY", key)
        }
        const mapValue = {};
        for(let i = 0; i < mapLength; i++) {
          const rawkey = mapContents[i]['_list1$_contents'][0];
          const rawvalue = mapContents[i]['_list1$_contents'][1];
          const key = serialize(rawkey, false, sass);
          const value = serialize(rawvalue, false, sass);
          mapValue[key] = value;
        }
        const serializedMapValues = Object.keys(mapValue).map(key => `${key}: ${mapValue[key]}`);
        return `(${serializedMapValues})`;
      }
      const mapLength = sassValue.getLength();
      const mapValue = {};
      for(let i = 0; i < mapLength; i++) {
        const key = serialize(sassValue.getKey(i), false, sass);
        const value = serialize(sassValue.getValue(i), false, sass);
        mapValue[key] = value;
      }
      const serializedMapValues = Object.keys(mapValue).map(key => `${key}: ${mapValue[key]}`);
      return `(${serializedMapValues})`;

    default:
      throw new Error(`Unsupported sass variable type '${sassValue.constructor.name}'`)
  };
};

/**
 * Create a serialized string from a sassValue object
 */
export function serialize(sassValue, isInList, sass) {
  return serializeValue(sassValue, isInList, sass);
};
