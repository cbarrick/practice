use std::env;
use std::process;
use std::time::Instant;

/// Get the arguments from argv.
fn args() -> Vec<usize> {
    let mut argv = env::args();
    let mut args = Vec::with_capacity(argv.len());

    // Discard the first arg, the name of the command.
    let name = argv.next().unwrap();

    // Parse each arg as a usize.
    for arg in argv {
        let n = match arg.parse() {
            Err(_) => {
                println!("Argument must be a non-negative integer.");
                println!("Usage: {} <N> [<N> ...]", name);
                process::exit(1);
            }
            Ok(n) => n,
        };
        args.push(n);
    }

    // Must contain at least one argument.
    if args.len() == 0 {
        println!("No argument provided.");
        println!("Usage: {} <N> [<N> ...]", name);
        process::exit(1);
    }

    args
}

/// Make the memo.
fn make_memo(max: usize) -> Vec<u128> {
    let mut memo = vec![0; max+1];
    memo[0] = 1;
    memo
}

/// A memoized implementation of the partition function.
fn part_memoized(n: usize, memo: &mut [u128]) -> u128 {
    if memo[n] != 0 {
        return memo[n];
    }

    for i in 1..=n {
        let mut p = 0;
        let mut k = 1;
        let mut a = 1;
        let mut b = 3;
        loop {
            if i < k { break };
            p += memo[i-k]; // plus
            k += a;
            a += 1;

            if i < k { break };
            p += memo[i-k]; // plus
            k += b;
            b += 2;

            if i < k { break };
            p -= memo[i-k]; // minus
            k += a;
            a += 1;

            if i < k { break };
            p -= memo[i-k]; // minus
            k += b;
            b += 2;
        }
        memo[i] = p;
    }

    memo[n]
}

/// Entry point.
fn main() {
    // Get the list of inputs to compute.
    let args = args();

    // Do the computation. Only this part is timed.
    let timer = Instant::now();
    let max = args.iter().copied().fold(0, usize::max);
    let mut memo = make_memo(max);
    part_memoized(max, &mut memo);
    let duration = timer.elapsed();

    // Print the output.
    for n in args {
        println!("p({})\t = {}", n, memo[n]);
    }
    println!("Total time: {:.6}s", duration.as_secs_f64());
}
