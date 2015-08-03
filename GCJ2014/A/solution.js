var readline = require('readline');


// Line buffer for stdin
var rl = readline.createInterface({
	input: process.stdin,
	output: process.stdout
});


// The number of cases (first line of input) and count of how many we've done
var numCases = 0;
var caseCount = 0;


// The index and content of the row being processed
var rowIndex = -1;
var row = [];


// The most recent match and number of matches found
var match = null;
var matches = 0;

var counter = 0;


// States
var readState = 0; // 0 - get number of cases
				   // 1 - read first answer
				   // 2 - read first board
				   // 3 - read second answer
				   // 4 - read second board


// Transition function
var transition = function (line) {
	switch (readState) {

		case 1:
		case 3:
			rowIndex = parseInt(line);
			readState += 1;
			break;

		case 2:
			counter += 1;
			rowIndex -= 1;
			if (rowIndex === 0) {
				row = [];
				line.split(' ').forEach(function (n) {
					row.push(parseInt(n))
				})
			}
			if (counter === 4) {
				counter = 0;
				readState = 3;
			}
			break;

		case 4:
			counter += 1;
			rowIndex -= 1;
			if (rowIndex === 0) {
				matches = 0;
				caseCount += 1
				line.split(' ').forEach(function (n) {
					n = parseInt(n);
					if (row.indexOf(n) >= 0) {
						match = n;
						matches++;
					}
				});
				if (matches === 0) {
					console.log("Case #" + caseCount + ": Volunteer cheated!");
				} else if (matches === 1) {
					console.log("Case #" + caseCount + ":", match);
				} else {
					console.log("Case #" + caseCount + ": Bad magician!");
				}
			}
			if (counter === 4) {
				counter = 0;
				readState = 1;
				if (caseCount === numCases) rl.close();
			}
			break;

		case 0:
		default:
			numCases = parseInt(line);
			readState = 1;
	}
}


// Gets the party started
rl.on('line', transition);
