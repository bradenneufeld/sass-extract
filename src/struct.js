import { getConstructor, getConstructorName, toColorHex } from './util';
import { serialize } from './serialize';

/**
 * Transform a sassValue into a structured value based on the value type
 */
function makeValue(inputSassValue, sass) {
  var sassValue = inputSassValue;
  if ( Array.isArray(sassValue) && sassValue.length === 1) sassValue = inputSassValue[0];
  switch(getConstructor(sassValue, sass)) {
    case sass.types.String:
      if ('_string0$_text' in sassValue) {
        return { value: sassValue['_string0$_text'] };
      }
      else return { value: sassValue.getValue() };
    case sass.types.Boolean:
      return { value: sassValue.getValue() };

    case sass.types.Number:
      if (sassValue.hasOwnProperty('_single_unit$_unit')) {
        return { value: sassValue['_number1$_value'], unit: sassValue['_single_unit$_unit'] };
      }
      else if (sassValue.hasOwnProperty('_number1$_value')) {
        return { value: sassValue['_number1$_value'] };
      }
      return { value: sassValue.getValue(), unit: sassValue.getUnit() };

    case sass.types.Color:
      if (sassValue.hasOwnProperty('_color1$_red')) {
        const r = Math.round(sassValue['_color1$_red']);
        const g = Math.round(sassValue[`_color1$_green`]);
        const b = Math.round(sassValue['_color1$_blue']);
        const a = Math.round(sassValue['_color1$_alpha']);
        return {
          value: {
            r, g, b,
            a: a,
            hex: `#${toColorHex(r)}${toColorHex(g)}${toColorHex(b)}`
          },
        };
      }
        const r = Math.round(sassValue.getR());
        const g = Math.round(sassValue.getG());
        const b = Math.round(sassValue.getB());
        const a = sassValue.getA();
      return {
        value: {
          r, g, b,
          a: a,
          hex: `#${toColorHex(r)}${toColorHex(g)}${toColorHex(b)}`
        },
      };

    case sass.types.Null:
      return { value: null };

    case sass.types.List:
      if ('$isSassList0' in sassValue) {
        const listLength = sassValue['get$lengthAsList']();
        const list = sassValue['get$asList']();
        const listValue = [];
        for(let i = 0; i < listLength; i++) {
          listValue.push(createStructuredValue(list[i], sass));
        }
        return { value: listValue, separator: sassValue['_list1$_separator'].separator };
      }
      const listLength = sassValue.getLength();
      const listValue = [];
      for(let i = 0; i < listLength; i++) {
        listValue.push(createStructuredValue(sassValue.getValue(i), sass));
      }
      return { value: listValue, separator: sassValue.getSeparator() ? ',' : ' ' };

    case sass.types.Map:
      if ('_map0$_contents' in sassValue) {
        const mapLength = sassValue['get$lengthAsList']();
        const mapContents = sassValue['get$asList']();
        for (var key in mapContents[0]) {
          //console.log("OKEY", key)
        }
        const mapValue = {};
        for(let i = 0; i < mapLength; i++) {
          const key = mapContents[i]['_list1$_contents'][0];
          const value = mapContents[i]['_list1$_contents'][1];
          // Serialize map keys of arbitrary type for extracted struct
          const serializedKey = serialize(key, false, sass);
          mapValue[serializedKey] = createStructuredValue(value, sass);
        }
        return { value: mapValue };
      }

      const mapLength = sassValue.getLength();
      const mapValue = {};
      for(let i = 0; i < mapLength; i++) {
        // Serialize map keys of arbitrary type for extracted struct
        const serializedKey = serialize(sassValue.getKey(i), false, sass);
        mapValue[serializedKey] = createStructuredValue(sassValue.getValue(i), sass);
      }
      return { value: mapValue };

    default:
      throw new Error(`Unsupported sass variable type '${sassValue.constructor.name}'`)
  };
};

/**
 * Create a structured value definition from a sassValue object
 */
export function createStructuredValue(sassValue, sass) {
  //console.log("createStructuredValue",sassValue )
  const value = Object.assign({
    type: getConstructorName(sassValue, sass),
  }, makeValue(sassValue, sass));
  //console.log("value", value)
  return value;
};
