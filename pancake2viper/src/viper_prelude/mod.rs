pub mod bitvector;
pub mod ext_calls;
pub mod heap;
pub mod shared_mem;
pub mod utils;

use bitvector::create_bv_domain;
pub use heap::HeapHelper;
use shared_mem::create_shared_mem_methods;
use utils::{bound_bits_function, bound_function, Utils};
use viper::{AstFactory, Domain, Field, Function, Method};

use crate::{ir::Model, utils::EncodeOptions};

pub fn create_viper_prelude(
    ast: AstFactory,
    model: Model,
    options: EncodeOptions,
) -> (Vec<Domain>, Vec<Field>, Vec<Method>, Vec<Function>) {
    if !options.include_prelude {
        return (vec![], vec![], vec![], vec![]);
    }
    let heap = HeapHelper::new(ast);
    let field1 = ast.field("pan", ast.int_type());
    let field2 = ast.field("shared", ast.int_type());
    let utils = Utils::new(ast, heap.get_type(), model);
    let domains = vec![create_bv_domain(ast)];
    let fields = vec![field1, field2];
    let methods = create_shared_mem_methods(ast, &utils);
    (
        domains,
        fields,
        methods,
        [8, 16, 32, 64]
            .into_iter()
            .map(|bits| bound_bits_function(ast, bits))
            .chain(std::iter::once(bound_function(ast, &utils, options)))
            .collect(),
    )
}
