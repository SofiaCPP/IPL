use paste::paste;

use core::fmt::Debug;
use std::any::Any;
use std::rc::Rc;

macro_rules! define_expression {
    ($name: ident$(, $element: ident: $ty: ty)* $(,)?) => {
        #[derive(Debug, PartialEq, Clone)]
        pub struct $name {
            $(pub $element: $ty),*
        }

        impl $name {
            pub fn new($($element: $ty),*) -> Self {
                $name {
                    $($element),*
                }
            }
        }

        impl Expression for $name {
            fn accept(&self, visitor: &mut dyn Visitor) {
                paste! {
                    visitor.[<visit_ $name:snake>](self);
                }
            }

            fn as_any(&self) -> &dyn Any {
                self
            }

            fn equals(&self, other: &dyn Expression) -> bool {
                other
                    .as_any()
                    .downcast_ref::<$name>()
                    .map_or(false, |a| self == a)
            }

            fn to_debug_string(&self) -> String {
                format!("{:?}", self)
            }
        }

    }
}

macro_rules! define_all_expressions {
    ($(($name: ident, $($element: ident: $ty: ty),*)),*) => {
        $(
            define_expression!($name, $($element: $ty),*);
        )*

        pub trait Visitor {
            paste! {
                $(
                    fn [<visit_ $name:snake>](&mut self, [<$name:snake>]: &$name);
                )*
            }
        }
    }
}

pub trait Expression {
    fn accept(&self, visitor: &mut dyn Visitor);
    fn as_any(&self) -> &dyn Any;
    fn equals(&self, _: &dyn Expression) -> bool;
    fn to_debug_string(&self) -> String;
}

impl Debug for dyn Expression {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}", self.to_debug_string())
    }
}

impl PartialEq for dyn Expression {
    fn eq(&self, other: &Self) -> bool {
        self.equals(other)
    }
}

define_all_expressions! {
    (FunctionDeclaration, name: String, args: Vec<String>, body: Rc<TopLevelExpressions>),
    (TopLevelExpressions, statements: Vec<Rc<dyn Expression>>),
    (VariableDeclaration, name: String, initial_value: Option<f64>)
}
