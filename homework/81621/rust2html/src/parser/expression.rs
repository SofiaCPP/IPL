use paste::paste;

use std::rc::Rc;

macro_rules! define_expression {
    ($name: ident$(, $element: ident: $ty: ty)* $(,)?) => {
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
            fn accept(&self, visitor: Box<&mut dyn Visitor>) {
                paste! {
                    visitor.[<visit_ $name:snake>](self);
                }
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
    fn accept(&self, visitor: Box<&mut dyn Visitor>);
}

//define_expression!(BodyExpression, statement: Vec<Rc<dyn Expression>>);
//define_expression!(FunctionExpression, name: String, args: Vec<String>,);

define_all_expressions! {
    (BodyExpression, statement: Vec<Rc<dyn Expression>>),
    (FunctionExpression, name: String, args: Vec<String>)
}
