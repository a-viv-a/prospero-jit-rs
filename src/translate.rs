use std::{collections::HashMap, mem};

use cranelift::{
    codegen::ir::UserFuncName, jit::{JITBuilder, JITModule}, module::{default_libcall_names, Linkage, Module}, prelude::{settings, types, AbiParam, FunctionBuilder, FunctionBuilderContext}
};

const FT = types::F64;
type FT = f64;

pub fn translate(src: &str) -> extern "C" fn(FT, FT) -> i8 {
    let mut flag_builder = settings::builder();
    let isa_builder = cranelift::native::builder().unwrap_or_else(|msg| {
        panic!("host machine is not supported: {msg}");
    });
    let isa = isa_builder
        .finish(settings::Flags::new(flag_builder))
        .unwrap();

    let mut module = JITModule::new(JITBuilder::with_isa(isa, default_libcall_names()));
    let mut ctx = module.make_context();

    let mut sig = module.make_signature();
    sig.params.push(AbiParam::new(FT));
    sig.params.push(AbiParam::new(FT));
    sig.returns.push(AbiParam::new(types::I8));

    let mut fn_builder_ctx = FunctionBuilderContext::new();
    let mut func = module
        .declare_function("prospero_calc", Linkage::Local, &sig)
        .unwrap();

    ctx.func.signature = sig;
    ctx.func.name = UserFuncName::user(0, func.as_u32());

    let mut builder = FunctionBuilder::new(&mut ctx.func, &mut fn_builder_ctx);
    let block = builder.create_block();
    builder.seal_block(block);

    builder.switch_to_block(block);

    // TODO: faster hash?
    let kv = HashMap::new();
    for line in src.lines() {
        let tokens = line.split_ascii_whitespace();
        let key = tokens.next().unwrap();
        let op = tokens.next().unwrap();

        match op {
            "const" => kv.set,
        }
    }

    builder.finalize();
    module.define_function(func, &mut ctx);
    module.finalize_definitions().unwrap();
    let code = module.get_finalized_function(func);
    
    unsafe { mem::transmute::<_, extern "C" fn(FT, FT) -> i8>(code) }
}
