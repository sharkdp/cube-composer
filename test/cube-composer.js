'use strict';

require('should');
var R = require('ramda');
var C = require('../js/cube-composer.js');

describe('clone', function(){
    it('takes x to [x, x]', function(){
        C.clone(1).should.eql([1, 1]);
    });
});

describe('map2D', function(){
    it('should map the given function over the second dimension', function(){
        C.map2D(R.inc)([[1, 2], [3, 4, 5]]).should.eql([[2, 3], [4, 5, 6]]);
    });

    it('should handle empty lists correctly', function(){
        C.map2D(R.inc)([[1, 2], []]).should.eql([[2, 3], []]);
    });

    it('may create 3D arrays', function(){
        C.map2D(C.clone)([[1], [2, 3]]).should.eql([[[1, 1]], [[2, 2], [3, 3]]]);
    });
});

describe('filter2D', function(){
    it('should filter the lists in the second dimension', function(){
        C.filter2D(R.gt(R.__, 2))([[1, 2], [1, 3, 4]]).should.eql([[], [3, 4]]);
    });
});

describe('wrap', function(){
    it('should lift the array into the second dimension', function(){
        C.wrap([1, 2]).should.eql([[1], [2]]);
    });
});

describe('flattenDeep', function(){
    it('flattens the array deeply and calls wrap to return a 2D array', function(){
        C.flattenDeep([[], [1], [[1]], [[1, 2]], [[[[1]]]]]).should.eql([[1], [1], [1], [2], [1]]);
    });
});

describe('flatten2D', function(){
    it('flattens everything starting from the second dimension', function(){
        C.flatten2D([[], [1], [[1]], [[1, 2]], [[[[1]]]]]).should.eql([[], [1], [1], [1, 2], [1]]);
    });
});

describe('removeNil', function(){
    it('clears out empty lists in the 2D array', function(){
        C.removeNil([[1], [], [2, 3]]).should.eql([[1], [2, 3]]);
    });
});

describe('cleanup', function(){
    it('is just a composition: clearNil . flatten2D', function(){
        C.cleanup([[1], [], [2, [3, 4]]]).should.eql([[1], [2, 3, 4]]);
    });
});

describe('transformationSteps', function(){
    var first = [[0], [1, 2]];

    it('returns a list of the intermediate results, starting with the initial array', function(){
        var f1 = C.map2D(C.clone);
        var f2 = C.filter2D(R.eq(1));

        var second = C.cleanup(f1(first));
        var third = C.cleanup(f2(second));

        C.transformationSteps(first, [f1, f2]).should.eql([first, second, third]);
    });
});
