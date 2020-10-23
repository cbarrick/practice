fn main() {
    for kind in &["Baby", "Daddy", "Mommy", "Grandpa", "Grandma"] {
        for _ in 0..3 { println!("{} shark doo doo doo doo doo doo", kind); }
        println!("{} shark!", kind);
    }
}
