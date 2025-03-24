mod translate;
mod translate_old;

fn main() {
    let src = include_str!("prospero.vm");

    println!("{src}");
}
