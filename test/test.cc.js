/* jshint globalstrict: true */

'use strict';

describe('clone', function(){
    it('takes x to [x, x]', function(){
        CC.clone(1).should.eql([1, 1]);
    });
});

describe('map2D', function(){
    it('should map the given function over the second dimension', function(){
        CC.map2D(R.inc)([[1, 2], [3, 4, 5]]).should.eql([[2, 3], [4, 5, 6]]);
    });

    it('should handle empty lists correctly', function(){
        CC.map2D(R.inc)([[1, 2], []]).should.eql([[2, 3], []]);
    });

    it('may create 3D arrays', function(){
        CC.map2D(CC.clone)([[1], [2, 3]]).should.eql([[[1, 1]], [[2, 2], [3, 3]]]);
    });
});

describe('filter2D', function(){
    it('should filter the lists in the second dimension', function(){
        CC.filter2D(R.gt(R.__, 2))([[1, 2], [1, 3, 4]]).should.eql([[], [3, 4]]);
    });
});

describe('wrap', function(){
    it('should lift the array into the second dimension', function(){
        CC.wrap([1, 2]).should.eql([[1], [2]]);
    });
});

describe('flattenDeep', function(){
    it('flattens the array deeply and calls wrap to return a 2D array', function(){
        CC.flattenDeep([[], [1], [[1]], [[1, 2]], [[[[1]]]]]).should.eql([[1], [1], [1], [2], [1]]);
    });
});

describe('flatten2D', function(){
    it('flattens everything starting from the second dimension', function(){
        CC.flatten2D([[], [1], [[1]], [[1, 2]], [[[[1]]]]]).should.eql([[], [1], [1], [1, 2], [1]]);
    });
});

describe('removeNil', function(){
    it('clears out empty lists in the 2D array', function(){
        CC.removeNil([[1], [], [2, 3]]).should.eql([[1], [2, 3]]);
    });
});

describe('cleanup', function(){
    it('is just a composition: clearNil . flatten2D', function(){
        CC.cleanup([[1], [], [2, [3, 4]]]).should.eql([[1], [2, 3, 4]]);
    });
});

describe('transformationSteps', function(){
    var first = [[0], [1, 2]];

    it('returns a list of the intermediate results, starting with the initial array', function(){
        var f1 = CC.map2D(CC.clone);
        var f2 = CC.filter2D(R.eq(1));

        var second = CC.cleanup(f1(first));
        var third = CC.cleanup(f2(second));

        CC.transformationSteps(first, [f1, f2]).should.eql([first, second, third]);
    });
});
