use std::{fmt::Display, mem};

use crate::raise::{NumTy, Ty};

pub enum DogF {
    I64(Box<dyn Fn() -> i64>),
    Bool(Box<dyn Fn() -> bool>),
}

impl DogF {
    pub unsafe fn from_typed_cptr(cptr: *const u8, ty: Ty) -> Self {
        match ty {
            Ty::Num(num_ty) => match num_ty {
                NumTy::I64 => {
                    let f = mem::transmute::<_, extern "C" fn() -> i64>(cptr);
                    Self::I64(Box::new(move || f()))
                }
                NumTy::Infer => todo!(),
            },
            Ty::Bool => {
                let f = mem::transmute::<_, extern "C" fn() -> i8>(cptr);
                Self::Bool(Box::new(move || {
                    let v = f();
                    // canonical bools are 1 or 0
                    assert!(v == 1 || v == 0, "'{v}' isn't canonical");
                    if v == 1 {
                        true
                    } else {
                        false
                    }
                }))
            }
            Ty::Unit => todo!(),
            Ty::Infer => todo!(),
        }
    }

    pub fn call(&self) -> DogV {
        macro_rules! call_match {
            ($($name:ident,)+) => {
                match self {
                    $(
                        Self::$name(f) => DogV::$name(f()),
                    )+
                }
            };
        }

        call_match![I64, Bool,]
    }
}

#[derive(Clone, Debug)]
pub enum DogV {
    I64(i64),
    Bool(bool),
}

impl Display for DogV {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        macro_rules! write_match {
            ($($name:ident,)+) => {
                match self {
                    $(
                        Self::$name(v) => write!(f, "{v}"),
                    )+
                }
            };
        }

        write_match![I64, Bool,]
    }
}

macro_rules! from_match {
    ($($name:ident <=> $type:ty,)+) => {
        $(
            impl From<$type> for DogV {
                fn from(value: $type) -> Self {
                    Self::$name(value)
                }
            }

            impl TryFrom<DogV> for $type {
                type Error = ();
                fn try_from(value: DogV) -> Result<$type, ()> {
                    match value {
                        DogV::$name(v) => Ok(v),
                        _ => Err(())
                    }
                }
            }
        )+
    };
}

from_match! {
    I64 <=> i64,
    Bool <=> bool,
}
