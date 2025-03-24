use std::{
    cell::RefCell,
    fmt::{Display, Pointer},
    iter,
    rc::Rc,
};

use edit_distance::edit_distance;
use lrlex::DefaultLexerTypes;
use lrpar::{NonStreamingLexer, Span};
use miette::{miette, Error, Result};

use itertools::{Either, Itertools};

use crate::{
    dogwood_y::{Expr, Literal, Op, POp, Spanning},
    label,
    stackhashmap::StackHashMap,
};

trait Unify: Sized {
    fn unify(&self, other: &Self) -> Option<Self>;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NumTy {
    I64,
    Infer,
}

impl NumTy {
    pub fn repr(&self) -> Option<cranelift::prelude::Type> {
        use cranelift::prelude::types;

        match self {
            Self::I64 => Some(types::I64),
            Self::Infer => None,
        }
    }
}

impl Display for NumTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NumTy::I64 => write!(f, "i64"),
            NumTy::Infer => write!(f, "infer"),
        }
    }
}

impl Unify for NumTy {
    fn unify(&self, other: &NumTy) -> Option<NumTy> {
        match (self, other) {
            (ty, NumTy::Infer) => Some(*ty),
            (NumTy::Infer, ty) => Some(*ty),

            (l, r) if l == r => Some(*l),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Ty {
    Num(NumTy),
    Bool,
    /// The type of "nothing", an empty tuple
    Unit,
    Infer,
}

impl Ty {
    pub fn repr(&self) -> Option<cranelift::prelude::Type> {
        use cranelift::prelude::types;

        match self {
            Ty::Num(num_ty) => num_ty.repr(),
            Ty::Bool => Some(types::I8),
            Ty::Unit => None,
            Ty::Infer => None,
        }
    }
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool => write!(f, "bool"),
            Self::Num(n) => write!(f, "num_{n}"),
            Self::Unit => write!(f, "()"),
            Self::Infer => write!(f, "_"),
        }
    }
}

impl Unify for Ty {
    fn unify(&self, other: &Ty) -> Option<Ty> {
        match (self, other) {
            (Ty::Num(l), Ty::Num(r)) => Some(Ty::Num(l.unify(r)?)),

            (ty, Ty::Infer) => Some(*ty),
            (Ty::Infer, ty) => Some(*ty),

            (l, r) if l == r => Some(*l),
            _ => None,
        }
    }
}

fn unify_all<T>(of: &[&T], base: Option<Ty>, help: &str) -> Result<Ty>
where
    T: Spanning + Tyable {
    of.iter()
        .try_fold(base.unwrap_or(Ty::Infer), |a, b| a.ty().unify(&b.ty()).ok_or_else(|| miette! {
            labels = of.iter().map(|e| label!(e.ty() => e.span())).collect::<Vec<_>>(),
            help = help,
            "can't unify these types with {}",
            base.map(|t| format!("{t}")).unwrap_or("each other".to_string())
        }))    
}

pub trait Tyable {
    fn ty(&self) -> Ty;
}

#[derive(Clone, Debug)]
pub struct BlockNode {
    span: Span,
    pub exprs: Vec<Node>,
    ty: Ty,
}

// TODO: move inside node
#[derive(Clone, Debug)]
pub struct CondNode {
    span: Span,
    pub cond: Box<Node>,
    pub then_node: BlockNode,
    pub else_node: Option<BlockNode>,
}

#[derive(Clone, Debug)]
pub struct Ident {
    pub span: Span,
    pub id: usize,
}

#[derive(Clone, Debug)]
pub enum Node {
    // TODO: remove this Ty! its gross
    Ident(Ty, Ident),
    Prefix {
        span: Span,
        ty: Ty,
        op: POp,
        node: Box<Node>,
    },
    Infix {
        span: Span,
        ty: Ty,
        lhs: Box<Node>,
        op: Op,
        rhs: Box<Node>,
    },
    Let {
        span: Span,
        ident: Ident,
        val: Box<Node>,
    },
    Assign {
        span: Span,
        ident: Ident,
        val: Box<Node>,
    },
    While {
        span: Span,
        cond: Box<Node>,
        then: BlockNode,
    },
    Block(BlockNode),
    Cond(CondNode),
    Lit(LitNode),
}

#[derive(Clone, Debug)]
pub enum LitNode {
    Bool(Span),
    Num(Span, NumTy),
}

impl Spanning for Ident {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl Spanning for Node {
    fn span(&self) -> &Span {
        match self {
            Node::Ident(_, ident) => ident.span(),
            Node::Infix { span, .. } | Node::Prefix { span, .. } | Node::Let { span, .. } | Node::Assign { span, .. } | Node::While { span, .. } => span,
            Node::Block(block_node) => block_node.span(),
            Node::Cond(cond_node) => cond_node.span(),
            Node::Lit(lit_node) => lit_node.span(),
        }
    }
}

impl Spanning for LitNode {
    fn span(&self) -> &Span {
        match self {
            LitNode::Bool(span) => span,
            LitNode::Num(span, _) => span,
        }
    }
}

impl Spanning for BlockNode {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl Spanning for CondNode {
    fn span(&self) -> &Span {
        &self.span
    }
}

macro_rules! parse_as {
	($(($fn_name:ident, $literal:ident, $type:ty), )+) => {
	    $(
    		pub fn $fn_name(&self, lexer: &dyn NonStreamingLexer<DefaultLexerTypes<u32>>) -> miette::Result<$type> {
    			use crate::label;
    			use miette::miette;
    			match self {
    			    // TODO: ensure num type is compatible!
    				Self::$literal(span, ..) => lexer
    					.span_str(*span)
    					.parse::<$type>()
    					.map_err(|e| miette!(
    						labels = vec![
    							label!(format!("tried to represent this value as a {}", stringify!($type)) => span)
    						],
    						"can't represent this {} family literal as {}: {e}",
    						self.ty(),
    						stringify!($type)
    					)),
    				lit => Err(miette!(
    					labels = vec![
    						label!(format!("this parsed as {}", lit.ty()) => self.span())
    					],
    					"incorrect type assumption during translation, attempted to represent {} literal `{}`",
    					lit.ty(),
    					stringify!($fn_name)
    				))
    			}
    		}
    	)+

    	// pub fn as_num_ty(&self, lexer: &dyn NonStreamingLexer<DefaultLexerTypes<u32>>) -> miette::Result<i64> {
    	//     match self {
    	//         Self::Num(span, ty) => {
    	//             match ty {
    	//                 NumTy::I64 => self.as_i64(lexer)
    	//             }
    	//         }
    	//     }
    	// }
    };
}

impl LitNode {
    parse_as! {
        (as_u64, Num, u64),
        (as_i64, Num, i64),
        (as_bool, Bool, bool),
    }
}

impl Ident {
    pub fn as_rpn(&self, lexer: &dyn NonStreamingLexer<DefaultLexerTypes<u32>>) -> String {
        format!("{}`{}`", self.id, lexer.span_str(self.span))
    }

    pub fn var(&self) -> cranelift::prelude::Variable {
        <cranelift::prelude::Variable as cranelift::prelude::EntityRef>::new(self.id)
    }
}

impl Node {
    pub fn as_rpn(&self, lexer: &dyn NonStreamingLexer<DefaultLexerTypes<u32>>) -> String {
        let repr = match self {
            Node::Infix { lhs, op, rhs, .. } => {
                format!("{} {} {op}", lhs.as_rpn(lexer), rhs.as_rpn(lexer))
            }
            Node::Prefix { node, op, .. } => {
                format!("{} {op}", node.as_rpn(lexer))
            }
            Node::Block(block_node) => block_node.as_rpn(lexer),
            Node::Let { ident, val, .. } => {
                format!("let {} = {}", ident.as_rpn(lexer), val.as_rpn(lexer))
            }
            Node::Assign { ident, val, .. } => {
                format!("{} = {}", ident.as_rpn(lexer), val.as_rpn(lexer))
            }
            Node::While { cond, then, .. } => {
                format!("while {} {}", cond.as_rpn(lexer), then.as_rpn(lexer))
            }
            Node::Cond(cond_node) => {
                format!(
                    "if {} {} else {}",
                    cond_node.cond.as_rpn(lexer),
                    cond_node.then_node.as_rpn(lexer),
                    cond_node
                        .else_node
                        .as_ref()
                        .map(|e| e.as_rpn(lexer))
                        .unwrap_or_else(|| "()".to_string())
                )
            }
            Node::Ident(_, ident) => ident.as_rpn(lexer),
            Node::Lit(LitNode::Bool(span)) | Node::Lit(LitNode::Num(span, _)) => {
                lexer.span_str(*span).to_string()
            }
        };
        format!("({repr} :{})", self.ty())
    }
}

impl BlockNode {
    pub fn as_rpn(&self, lexer: &dyn NonStreamingLexer<DefaultLexerTypes<u32>>) -> String {
        format!(
            "{{\n{}{}\n}}",
            self.exprs
                .iter()
                .map(|e| e.as_rpn(lexer))
                .collect::<Vec<_>>()
                .join(";\n"),
            if self.ty() == Ty::Unit { ";" } else { "" }
        )
    }
}

impl Tyable for Ty {
    fn ty(&self) -> Ty {
        *self
    }
}

impl Tyable for Node {
    fn ty(&self) -> Ty {
        match self {
            Node::Ident(ty, _) => *ty,
            Node::Infix { ty, .. } => *ty,
            Node::Prefix { ty, .. } => *ty,
            Node::Let { .. } | Node::Assign { .. } | Node::While { .. } => Ty::Unit,
            Node::Block(block) => block.ty(),
            Node::Cond(cond) => cond.ty(),
            Node::Lit(lit) => lit.ty(),
        }
    }
}

impl Tyable for LitNode {
    fn ty(&self) -> Ty {
        match self {
            LitNode::Bool(_) => Ty::Bool,
            LitNode::Num(_, num_ty) => Ty::Num(*num_ty),
        }
    }
}

impl Tyable for BlockNode {
    fn ty(&self) -> Ty {
        self.ty
    }
}

impl Tyable for CondNode {
    fn ty(&self) -> Ty {
        self.then_node
            .ty()
            .unify(&self.else_node.as_ref().map(|n| n.ty()).unwrap_or(Ty::Unit))
            .unwrap_or(Ty::Unit)
    }
}

// type Scope = Rc<RefCell<StackHashMap<&'input str, (usize, Ty)>>>;

macro_rules! must_be {
    ($t:ident, $n:expr) => {
        match $n {
            Node::$t(e) => e,
            _ => panic!(
                "must be was incorrect: {} was not {}",
                stringify!($n),
                stringify!($t)
            ),
        }
    };
}

pub fn raise_expr<'input>(
    lexer: &dyn NonStreamingLexer<'input, DefaultLexerTypes<u32>>,
    expr: Expr,
    scope: &mut StackHashMap<&'input str, (usize, Ty)>,
    nth: &mut usize,
) -> Result<Node> {
    match expr {
        Expr::Literal(Literal::Boolean(span)) => Ok(Node::Lit(LitNode::Bool(span))),
        // TODO: mark as infer!
        Expr::Literal(Literal::Integer(span)) => Ok(Node::Lit(LitNode::Num(span, NumTy::I64))),
        Expr::Infix { span, lhs, op, rhs } => {
            let lhn = raise_expr(lexer, *lhs, scope, nth)?;
            let rhn = raise_expr(lexer, *rhs, scope, nth)?;

            match op {
                Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Pow | Op::Mod => {
                    if let Some(ty) = lhn.ty().unify(&rhn.ty()) {
                        Ok(Node::Infix {
                            span,
                            ty,
                            lhs: Box::new(lhn),
                            op,
                            rhs: Box::new(rhn),
                        })
                    } else {
                        Err(miette! {
                            labels = vec![
                                label!(lhn.ty() => lhn.span()),
                                label!(rhn.ty() => rhn.span()),
                            ],
                            help = "infix numerical operators requires both operands be the same type",
                            "can't unify these types"
                        })
                    }
                }

                Op::Eq | Op::Ne => {
                    lhn.ty().unify(&rhn.ty()).ok_or_else(|| miette! {
                            labels = vec![
                                label!(lhn.ty() => lhn.span()),
                                label!(rhn.ty() => rhn.span()),
                            ],
                            help = "infix equality operators requires both operands be the same type",
                            "can't unify these types"
                        })?;
                        
                    Ok(Node::Infix {
                        span,
                        ty: Ty::Bool,
                        lhs: Box::new(lhn),
                        op,
                        rhs: Box::new(rhn),
                    })
                }

                Op::Gt | Op::Lt => {
                    lhn.ty().unify(&rhn.ty()).and_then(|t| t.unify(&Ty::Num(NumTy::Infer))).ok_or_else(|| miette! {
                            labels = vec![
                                label!(lhn.ty() => lhn.span()),
                                label!(rhn.ty() => rhn.span()),
                            ],
                            help = "infix numerical comparison operators requires both operands be the same type and unify with a numerical type",
                            "can't unify these types as numericals"
                        })?;
                        
                    Ok(Node::Infix {
                        span,
                        ty: Ty::Bool,
                        lhs: Box::new(lhn),
                        op,
                        rhs: Box::new(rhn),
                    })
                }

                Op::And | Op::Or => {
                    if let Some(ty) = lhn.ty().unify(&rhn.ty()).and_then(|t| t.unify(&Ty::Bool)) {
                        Ok(Node::Infix {
                            span,
                            ty,
                            lhs: Box::new(lhn),
                            op,
                            rhs: Box::new(rhn),
                        })
                    } else {
                        Err(miette! {
                            labels = vec![
                                label!(lhn.ty() => lhn.span()),
                                label!(rhn.ty() => rhn.span()),
                            ],
                            help = "infix logical operators requires both operands be the same type and unify with boolean",
                            "can't unify these types with bool"
                        })
                    }
                }
            }
        }
        Expr::Prefix {
            span,
            op,
            expr
        } => {
            match op {
                POp::Neg => {
                    let node = raise_expr(lexer, *expr, scope, nth)?;
                    // TODO:: NumTy::InferSigned / unsigned?
                    let ty = unify_all(&[&node], Some(Ty::Num(NumTy::Infer)), "prefix negation requires it's target unify with number")?;

                    Ok(Node::Prefix { span, ty, op, node: Box::new(node) })
                },
                POp::Not => {
                    // TODO: bitwise not...
                    let node = raise_expr(lexer, *expr, scope, nth)?;
                    let ty = unify_all(&[&node], Some(Ty::Bool), "prefix logical not requires it's target unify with boolean")?;

                    Ok(Node::Prefix { span, ty, op, node: Box::new(node) })
                },
            }
        }
        Expr::LetExpr {
            span: let_span,
            ident,
            val,
            ..
        } => {
            let val = raise_expr(lexer, *val, scope, nth)?;
            let ident_str = ident.as_str(lexer);

            *nth += 1;
            let id = *nth;

            scope.insert(ident_str, (id, val.ty()));

            let ident = Ident {
                span: ident.span,
                id,
            };

            Ok(Node::Let {
                span: let_span,
                ident,
                val: Box::new(val),
            })
        }
        Expr::AssignExpr {
            span: assign_span,
            ident,
            val,
            ..
        } => {
            // TODO: dedupe the error handling logic
            let val = raise_expr(lexer, *val, scope, nth)?;
            let ident_str = ident.as_str(lexer);

            let (id, ty) = scope.get(&ident_str).ok_or_else(|| {
                miette! {
                    labels = vec![
                        label!("variable" => ident.span()),
                        label!("with this value" => val.span())
                    ],
                    help = if let Some(similar) = scope.get_close_keys(|(k, _)| {
                        let dist = edit_distance(k, ident_str);
                        if dist > 5 { None } else { Some(dist) }
                    }, 10).next() {
                        format!("did you mean `{similar}`?")
                    } else {
                        "check if you intended to use a variable that isn't in scope".to_string()
                    },
                    "can't assign to variable that hasn't been declared"
                }
            })?;

            ty.unify(&val.ty()).ok_or_else(|| {
                let base_help = format!("did you intend to shadow `{ident_str}` with type {}? use let instead of assign", val.ty());
                miette! {
                    labels = vec![
                        label!(format!("expr of type {}", val.ty()) => val.span()),
                        label!(format!("variable of type {ty}") => ident.span())
                    ],
                    help = if let Some(correct_type) = scope.get_close_keys(|(k, v)| {
                        v.1.unify(&val.ty())?;
                        
                        let dist = edit_distance(k, ident_str);
                        if dist > 10 { None } else { Some(dist) }
                    }, 10).next() {
                        format!("did you mean to write `{correct_type}` instead of `{ident_str}`? it has a type that is compatible with {}.\nif not, {base_help}", val.ty())
                    } else {
                        base_help
                    },
                    "assignment value must unify with variable type"
                }
            })?;

            let ident = Ident {
                span: ident.span,
                id: *id,
            };

            Ok(Node::Assign {
                span: assign_span,
                ident,
                val: Box::new(val),
            })
        }
        Expr::Ident(ident) => {
            let ident_str = ident.as_str(lexer);
            let (id, ty) = scope.get(&ident_str).ok_or_else(|| {
                miette! {
                    labels = vec![
                        label!("variable" => ident.span()),
                    ],
                    help = if let Some(similar) = scope.get_close_keys(|(k, _)| {
                        let dist = edit_distance(k, ident_str);
                        if dist > 5 { None } else { Some(dist) }
                    }, 10).next() {
                        format!("did you mean `{similar}`?")
                    } else {
                        "check if you intended to use a variable that isn't in scope".to_string()
                    },
                    "can't use variable before it has been declared"
                }
            })?;

            Ok(Node::Ident(
                *ty,
                Ident {
                    span: ident.span,
                    id: *id,
                },
            ))
        }
        Expr::BlockExpr(block_expr) => {
            // TODO: clean this up
            scope.scope(|scope| {
                let (exprs, ty) = if let Some(retval) = block_expr.retval {
                    let (exprs, errs) = raise_exprs(
                        lexer,
                        block_expr.stmts.into_iter().chain(iter::once(retval)),
                        scope,
                        nth,
                    );
                    if !errs.is_empty() {
                        return Err(errs.into_iter().next().unwrap());
                    }
                    let ty = exprs.last().unwrap().ty();
                    (exprs, ty)
                } else {
                    let (exprs, errs) = raise_exprs(lexer, block_expr.stmts.into_iter(), scope, nth);

                    if !errs.is_empty() {
                        return Err(errs.into_iter().next().unwrap());
                    }
                    (exprs, Ty::Unit)
                };

                Ok(Node::Block(BlockNode {
                    span: block_expr.span,
                    exprs,
                    ty,
                }))
            })
        }
        Expr::CondExpr(cond_expr) => {
            // in the future, when there is a means by which to do assignment in a condition,
            // create a new scope to contain it
            let cond = Box::new(raise_expr(lexer, cond_expr.cond, scope, nth)?);
            assert!(cond.ty().unify(&Ty::Bool).is_some());
            let then_node = must_be!(
                Block,
                raise_expr(
                    lexer,
                    Expr::BlockExpr(Box::new(cond_expr.then_br)),
                    scope,
                    nth,
                )?
            );
            let else_node = if let Some(else_br) = cond_expr.else_br {
                Some(must_be!(
                    Block,
                    raise_expr(lexer, Expr::BlockExpr(Box::new(else_br)), scope, nth)?
                ))
            } else {
                None
            };

            Ok(Node::Cond(CondNode {
                span: cond_expr.span,
                cond,
                then_node,
                else_node,
            }))
        }
        Expr::WhileExpr { span, cond, then } => {
            let cond = Box::new(raise_expr(lexer, *cond, scope, nth)?);
            assert!(cond.ty().unify(&Ty::Bool).is_some());
            let then_node = must_be!(
                Block,
                raise_expr(
                    lexer,
                    Expr::BlockExpr(then),
                    scope,
                    nth,
                )?
            );

            Ok(Node::While {
                span,
                cond,
                then: then_node,
            })
        }

    }
}

pub fn raise_exprs<'input>(
    lexer: &dyn NonStreamingLexer<'input, DefaultLexerTypes<u32>>,
    exprs: impl Iterator<Item = Expr>,
    scope: &mut StackHashMap<&'input str, (usize, Ty)>,
    nth: &mut usize,
) -> (Vec<Node>, Vec<Error>) {
    let mut nodes = vec![];
    let mut errs = vec![];
    for expr in exprs {
        match raise_expr(lexer, expr, scope, nth) {
            Ok(node) => nodes.push(node),
            Err(err) => errs.push(err),
        }
    }
    (nodes, errs)
}
