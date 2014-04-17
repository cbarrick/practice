'use strict';


// Sum of the even Fibonacci numbers less than four million
// Every third number is even.
// Very fast: no floats, no exponents, no mods, and the loop takes only 11 iterations.
// ---
// Solution: p2(4000000) === 4613732

var p2 = module.exports = function (cap) {
	var sum = 0;
	var a = 1;
	var b = 1;
	var next;

	while (a < cap && b < cap) {
		next = (a + b);
		sum += next;
		a += b * 2;
		b += next * 2;
	}

	return sum;
}
