use crispy::interpreter;
use std::io;

pub fn main() {
    let mut stdin = io::stdin().lock();
    interpreter(&mut stdin);
}
