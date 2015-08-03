// My solution is to click a corner and fill mines from the opposite corner. I.E:
// *****
// ****.
// ***..
// **...
// *...c

var readline = require('readline');


// Line buffer for stdin
var rl = readline.createInterface({
	input: process.stdin,
	output: process.stdout
});


var getConfiguration(r, c, m) {

	// Build empty board
	var board = [];
	for (var i = 0; i < r; i++) {
		var row = [];
		for (var j = 0; j < c; j++) {
			row.push('.');
		}
		board.push('row');
	}

	// TODO: finish
}
