const { expect } = require('chai');
const { getSassImplementation } = require('../src/util');

describe('getSassImplementation', () => {
  it('should use `sass` by default', () => {
    const implementation = getSassImplementation();

    expect(implementation.info.split('\t')[0]).to.equal('dart-sass');
  });

  it('should use the given implementation, based on `extractOptions`', () => {
    const dartSass = getSassImplementation({ implementation: require('sass') });

    expect(dartSass.info.split('\t')[0]).to.equal('dart-sass');
  });

  it('should throw error if the given implementation is neither `node-sass`, nor `sass`', () => {
    expect(() => getSassImplementation({ implementation: require('path') })).to.throw(Error);
  });
});
