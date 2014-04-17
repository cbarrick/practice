'use strict';

var Set = require('collections/set');


// Sum of the multiples of 3 and 5 less than 1000
// ---
// Solution: p1(3, 5, 1000) === 233168

var p1 = module.exports = function (multiple1, multiple2, cap) {
	var a = multiple1;
	var b = multiple2;
	var sum = 0;
	var cache = new Set();

	for (var i = a, j = b; i < cap || j < cap; i += a, j += b) {
		if (i < cap && !cache.has(i)) {
			sum += i;
			cache.add(i);
		}
		if (j < cap && !cache.has(j)) {
			sum += j;
			cache.add(j);
		}
	}

	return sum;
}
