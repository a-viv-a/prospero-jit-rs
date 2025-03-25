use std::{fs::File, io::Write};

use bitvec::{prelude::*, view::BitView};
use itertools::Itertools;
use rayon::prelude::*;

use translate::{translate, SIMD_WIDTH};

mod translate;

fn main() {
    let src = include_str!("prospero.vm");
    let dim: usize = 1024;

    // this might be especially evil...
    let step = 2f32 / dim as f32;

    let xy_fn = translate(src, step);
    let bytes = (0..dim)
        .flat_map(|y_step| {
            // this ugly code lets us step 8 xes at once since each call computes 8 x values
            (0..(dim / SIMD_WIDTH))
                .rev()
                .map(move |x_step| (x_step * SIMD_WIDTH, y_step))
        })
        .rev()
        .map(|(x_step, y_step)| (-1f32 + step * x_step as f32, -1f32 + step * y_step as f32))
        .collect::<Vec<_>>()
        .into_par_iter()
        .flat_map(|(x, y)| {
            let mask = xy_fn(x, y) as u8;
            mask.view_bits::<Lsb0>()
                .into_iter()
                .take(4)
                .map(|b| match *b {
                    true => 255u8,
                    false => 0u8,
                })
                .collect_vec()
        })
        .collect::<Vec<_>>();

    let mut out = File::create("out.ppm").unwrap();
    write!(out, "P5\n{dim} {dim}\n255\n").unwrap();
    out.write(&bytes[..]).unwrap();
}
