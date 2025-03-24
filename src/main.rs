use itertools::Itertools;
use translate::translate;

mod translate;
// mod translate_old;

fn main() {
    let src = include_str!("prospero.vm");
    let dim: usize = 1024;

    // this might be especially evil...
    let step = 1f64 / dim as f64;

    let xy_fn = translate(src);
    (0..dim)
        .cartesian_product(0..dim)
        .map(|(x_step, y_step)| {
            dbg!(x_step, y_step);
            (-1f64 + step * x_step as f64, -1f64 + step * y_step as f64)
        })
        .map(|(x, y)| {
            let b = xy_fn(x, y);
            if b != 0 {
                true
            } else {
                false
            }
        });
}
