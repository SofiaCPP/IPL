/*
 * The following function returns the first answer.
 * It takes no arguments and returns an unsigned integer.
 */
fn answer1() -> usize {
    return 42; // answer to the Ultimate Question of Life, the Universe, and Everything
}

/*
 * The following function returns the second answer.
 * It takes no arguments and returns a floating-point number.
 */
fn answer2() -> f64 {
    return -4.2;
}

/*
 * The following function returns the third answer.
 * It takes no arguments and returns a single character.
 */
fn answer3() -> char {
    return 'E';
}

fn main() {
    println!("The \"First Answer\" is: '{}'", answer1());
    println!("The \"Second Answer\" is: '{}'", answer2());
    println!("The \"Third Answer\" is: '{}'", answer3());
}
