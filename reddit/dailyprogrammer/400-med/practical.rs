fn divisors_of(n: u32) -> Vec<u32> {
    let sqrt = (n as f64).sqrt() as u32;
    let mut divisors = Vec::with_capacity((sqrt * 2) as usize);
    for x in 1..=sqrt {
        let d = n / x;
        if d * x == n {
            divisors.push(x);
            if d != sqrt {
                divisors.push(d);
            }
        }
    }
    divisors.sort_unstable();
    divisors
}

fn is_practical(n: u32) -> bool {
    let divisors = divisors_of(n);
    for mut m in 1..n {
        for x in divisors.iter().rev().copied() {
            if x <= m {
                m -= x;
            }
        }
        if m != 0 {
            return false;
        }
    }
    true
}

fn main() {
    let sum: u32 = (1..=10_000).filter(|n| is_practical(*n)).sum();
    println!("challenge = {}", sum);
}
