// NOTE: The accepted solution considers cookies as being granted continuously
// as in: at 1 cookie per second, you have 1.5 cookies after 1.5 seconds.
//
// My original solution considered cookies as being dispersed in whole units,
// as in: at 1 cookie per second, you have 1 cookie after 1.5 seconds.
// This caused my answers to be off in many cases, but I argue that my original is the better way
// of thinking about the problem, since cookies are not fluids.

var readline = require('readline');


// Line buffer for stdin
var rl = readline.createInterface({
	input: process.stdin,
	output: process.stdout
});


// The number of cases (first line of input) and count of how many we've done
var numCases = 0;
var caseCount = 0;


// The solution
var getTime = function (c, f, x) {
	var cookies = 0;
	var time = 0;
	var rate = 2;
	var best = x / rate;

	while (cookies < x) {
		cookies += c;
		time += c / rate;
		var time_with_farm = time + x / (rate + f);
		if (time_with_farm < best) {
			best = time_with_farm;
			rate += f;
			cookies -= c;
		}
	}

	return best;
}


// States
var readState = 0; // 0 - get number of cases
                   // 1 - handle case


// Transition function
var transition = function (line) {
	switch (readState) {
	case 1:
		caseCount += 1;
		var args = line.split(' ');
		args = args.map(parseFloat);
		console.log('Case #' + caseCount + ': ' + getTime(args[0], args[1], args[2]));
		break;
	case 0:
	default:
		numCases = parseInt(line);
		readState = 1;
	}
}


// Gets the party started
rl.on('line', transition);
