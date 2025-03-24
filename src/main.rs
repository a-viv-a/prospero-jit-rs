use std::{fs::File, io::Write};

use itertools::Itertools;
use translate::translate;

mod translate;

fn main() {
    let src = include_str!("prospero.vm");
    let dim: usize = 1024;

    // this might be especially evil...
    let step = 2f64 / dim as f64;

    let xy_fn = translate(src);
    let bytes = (0..dim)
        .cartesian_product(0..dim)
        .map(|(x_step, y_step)| (-1f64 + step * x_step as f64, -1f64 + step * y_step as f64))
        .map(|(x, y)| {
            let b = xy_fn(x, y);
            if b != 0 {
                true
            } else {
                false
            }
        })
        .map(|b| match b {
            true => 255u8,
            false => 0u8,
        })
        .collect::<Vec<_>>();

    let mut out = File::create("out.ppm").unwrap();
    write!(out, "P5\n{dim} {dim}\n255\n").unwrap();
    out.write(&bytes[..]).unwrap();
}
