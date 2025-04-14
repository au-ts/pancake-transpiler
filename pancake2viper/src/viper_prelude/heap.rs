use viper::{AstFactory, Expr, Field, Predicate, Type};

use crate::utils::ViperUtils;

#[derive(Clone, Copy)]
pub struct HeapHelper<'a> {
    ast: AstFactory<'a>,
}

pub enum MemType {
    Pancake,
    Shared,
}

impl<'a> HeapHelper<'a> {
    pub fn new(ast: AstFactory<'a>) -> Self {
        Self { ast }
    }

    /// Encodes the following function application to access an element of an Heap
    /// ```viper
    /// slot(array, idx)
    /// ```
    fn idx_f(&self, heap: Expr, idx: Expr) -> Expr<'a> {
        self.ast.seq_index(heap,idx)
    }

    /// Encodes the following function application to get the length of an Heap
    /// ```viper
    /// alen(array)
    /// ```
    pub fn len_f(&self, heap: Expr) -> Expr<'a> {
        self.ast.seq_length(heap)
    }

    /// Encodes array accesses of an Heap
    /// ```viper
    /// slot(array, idx).heap_elem
    /// ```
    pub fn access(&self, heap: Expr, idx: Expr, mem: MemType) -> Expr<'a> {
        match mem {
            MemType::Pancake => self.ast.field_access(self.idx_f(heap, idx), self.field_internal()),
            MemType::Shared => self.ast.field_access(self.idx_f(heap, idx), self.field_shared()),
        }
    }

    /// The type of an Heap
    pub fn get_type(&self) -> Type<'a> {
        self.ast.seq_type(self.ast.ref_type())
    }

    /// The internal Pancake memory
    pub fn field_internal(&self) -> Field<'a> {
        self.ast.field("pan", self.ast.int_type())
    }

    /// Shared memory
    pub fn field_shared(&self) -> Field<'a> {
        self.ast.field("shared", self.ast.int_type())
    }

    /// Encodes the following predicate for slice access of an Heap
    /// ```viper
    /// predicate slice_acc(src: Heap, idx: Int, length: Int) {
    ///     forall j: Int :: 0 <= idx <= j < idx + length <= alen(src) ==> acc(slot(src, j).heap_elem)
    /// }
    /// ```
    pub fn slice_acc_def(&self, biw_size: i64) -> Predicate<'a> {
        let ast = self.ast;
        let (src_decl, src) = ast.new_var("src", self.get_type());
        let (idx_decl, idx) = ast.new_var("idx", ast.int_type());
        let (length_decl, length) = ast.new_var("length", ast.int_type());
        let (perm_decl, perm) = ast.new_var("perm", ast.perm_type());
        self.ast.predicate(
            "slice_acc",
            &[src_decl, idx_decl, length_decl, perm_decl],
            Some(self.heap_acc_expr(src, idx, length, perm, biw_size)),
        )
    }

    /// Encodes the following predicate for full access of an Heap
    /// ```viper
    /// predicate slice_acc(src: Heap, l: Int, h: Int) {
    ///     forall j: Int ::0 <= j < |heap| ==> acc(heap[j].heap_elem)
    /// }
    /// ```
    pub fn full_acc_def(&self, biw_size: i64) -> Predicate<'a> {
        let ast = self.ast;
        let (src_decl, src) = ast.new_var("src", self.get_type());
        let (perm_decl, perm) = ast.new_var("perm", ast.perm_type());
        let l = ast.zero();
        let h = self.len_f(src);
        self.ast.predicate(
            "slice_acc",
            &[src_decl, perm_decl],
            Some(self.heap_acc_expr(src, l, h, perm, biw_size)),
        )
    }

    /// Encodes the following expression for permissions of an Heap (slice)
    /// ```viper
    ///     forall j: Int :: 0 <= low <= j < upper <= |heap| ==> acc(heap[j].heap_elem)
    /// ```
    pub fn heap_acc_expr(&self, heap: Expr, low: Expr, upper: Expr, perm: Expr, biw_size: i64) -> Expr<'a> {
        let ast: AstFactory<'a> = self.ast;
        let (j_decl, j) = ast.new_var("j", ast.int_type());
        let zero = ast.zero();
        let limit = self.len_f(heap);

        let i0 = ast.le_cmp(zero, low);
        let ij = ast.le_cmp(low, j);
        let jl = ast.lt_cmp(j, limit);
        let lu = ast.le_cmp(limit, upper);
        let bytes_in_word = ast.int_lit(biw_size);
        let align = ast.eq_cmp(ast.module(j, bytes_in_word), ast.int_lit(0));
        let guard = ast.and(ast.and(ast.and(i0, ij), ast.and(jl, lu)), align);

        let access = ast.field_access_predicate(self.access(heap, j, MemType::Pancake), perm);

        ast.forall(&[j_decl], &[], ast.implies(guard, access))
    }

    /// Sequence injective assumption on heap variable
    /// ```viper
    ///     forall i: Int, j: Int :: 
    /// 0 <= i < |heap| && 0 <= j < |heap| && i != j ==> heap[i] != heap[j]
    /// ```
    pub fn heap_injective(&self, heap: Expr) -> Expr<'a> {
        let ast = self.ast;
        let heap_len = ast.seq_length(heap);
        let (i_decl, i) = ast.new_var("i", ast.int_type());
        let (j_decl, j) = ast.new_var("j", ast.int_type());
        let guard_i = ast.and(ast.le_cmp(ast.zero(), i),
            ast.lt_cmp(i, heap_len));
        let guard_j = ast.and(ast.le_cmp(ast.zero(), j),
            ast.lt_cmp(j, heap_len));
        let guard = ast.and(ast.and(guard_i, guard_j),
            ast.ne_cmp(i, j));
        let diff = ast.ne_cmp(
            ast.seq_index(heap, i),
        ast.seq_index(heap, j));
        ast.forall(
            &[i_decl, j_decl], 
            &[], 
            ast.implies(guard, diff))
    }
}
