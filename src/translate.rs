use std::{collections::HashMap, mem};

use cranelift::{
    codegen::ir::UserFuncName,
    jit::{JITBuilder, JITModule},
    module::{default_libcall_names, Linkage, Module},
    prelude::{
        settings, types, AbiParam, FloatCC, FunctionBuilder, FunctionBuilderContext, InstBuilder,
        Type,
    },
};
use itertools::Itertools;

fn variable_index(v: &str) -> usize {
    usize::from_str_radix(&v[1..], 16).expect("passed a valid variable")
}

pub const SIMD_WIDTH: usize = 4;

/// params are (x_base, y) -> i8
pub fn translate(src: &str, x_stepsize: f32) -> extern "C" fn(f32, f32) -> i8 {
    let flag_builder = settings::builder();
    let isa_builder = cranelift::native::builder().unwrap_or_else(|msg| {
        panic!("host machine is not supported: {msg}");
    });
    let isa = isa_builder
        .finish(settings::Flags::new(flag_builder))
        .unwrap();

    let mut module = JITModule::new(JITBuilder::with_isa(isa, default_libcall_names()));
    let mut ctx = module.make_context();

    let mut sig = module.make_signature();
    sig.params.push(AbiParam::new(types::F32));
    sig.params.push(AbiParam::new(types::F32));
    sig.returns.push(AbiParam::new(types::I8));

    let mut fn_builder_ctx = FunctionBuilderContext::new();
    let func = module
        .declare_function("prospero_calc", Linkage::Local, &sig)
        .unwrap();

    ctx.func.signature = sig;
    ctx.func.name = UserFuncName::user(0, func.as_u32());

    let mut builder = FunctionBuilder::new(&mut ctx.func, &mut fn_builder_ctx);
    let block = builder.create_block();
    builder.append_block_params_for_function_params(block);
    builder.seal_block(block);

    builder.switch_to_block(block);
    // lots of perf related comments but i don't expect this to dominate runtime so probably whatev

    // TODO: check if unwrap cost matters (it doesn't shut up)
    // NOTE: we iterate the lines twice... probably better than creating a lines vec?
    let mut kv = Vec::with_capacity(src.lines().count());
    // WARN: skip 1 isn't really handling comments... might regret this?
    for line in src.lines().skip(1) {
        let mut tokens = line.split_ascii_whitespace();
        let key = variable_index(tokens.next().unwrap());
        let op = tokens.next().unwrap();

        // WARN: this is bad! relying on ordering of instruction variables...
        kv.push(match op {
            "const" => {
                let float_str = tokens.next().unwrap();
                let value = builder.ins().f32const(float_str.parse::<f32>().unwrap());
                // create an approp vector with all lanes having this value
                builder.ins().splat(types::F32X4, value)
            }
            "var-x" => {
                let x_base = builder.block_params(block)[0];
                let x_vec_base = builder.ins().splat(types::F32X4, x_base);

                // WARN: probably bugbear live here
                let incrementing = builder.func.dfg.constants.insert(
                    (0..SIMD_WIDTH)
                        .map(|n| n as f32 * x_stepsize)
                        .flat_map(|n| n.to_ne_bytes())
                        .collect(),
                );

                let step_vec = builder.ins().vconst(types::F32X4, incrementing);
                builder.ins().fadd(step_vec, x_vec_base)
            }
            "var-y" => {
                let y_val = builder.block_params(block)[1];
                builder.ins().splat(types::F32X4, y_val)
            }
            // TODO: does compiler optimize the double match away?
            "neg" | "square" | "sqrt" => {
                let val = kv[variable_index(tokens.next().unwrap())];
                match op {
                    "neg" => builder.ins().fneg(val),
                    "square" => builder.ins().fmul(val, val),
                    // TODO: can we get away with something faster?
                    "sqrt" => builder.ins().sqrt(val),
                    _ => unreachable!(),
                }
            }
            "add" | "sub" | "mul" | "div" | "min" | "max" => {
                let a = kv[variable_index(tokens.next().unwrap())];
                let b = kv[variable_index(tokens.next().unwrap())];
                match op {
                    "add" => builder.ins().fadd(a, b),
                    "sub" => builder.ins().fsub(a, b),
                    "mul" => builder.ins().fmul(a, b),
                    "div" => builder.ins().fdiv(a, b),
                    "max" => builder.ins().fmax(a, b),
                    "min" => builder.ins().fmin(a, b),
                    _ => unreachable!(),
                }
            }
            _ => panic!("unhandled opcode {op}"),
        });
        debug_assert_eq!(key, kv.len() - 1);
    }

    // TODO: single instruction with const?
    let zero = builder.ins().f32const(0f32);
    let zero_vec = builder.ins().splat(types::F32X4, zero);
    let signs_vec = builder
        .ins()
        .fcmp(FloatCC::LessThan, *kv.last().unwrap(), zero_vec);
    let signs = builder.ins().vhigh_bits(types::I8, signs_vec);
    builder.ins().return_(&[signs]);

    builder.finalize();
    // println!("{}", ctx.func.display());
    // match module.define_function(func, &mut ctx) {
    //     Err(e) => eprintln!("{e:#?}"),
    //     Ok(()) => {}
    // }
    module.define_function(func, &mut ctx).unwrap();
    module.finalize_definitions().unwrap();
    let code = module.get_finalized_function(func);

    unsafe { mem::transmute::<_, extern "C" fn(f32, f32) -> i8>(code) }
}
