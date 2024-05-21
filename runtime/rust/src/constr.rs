// Constructor access and manipulation

use super::defs::*;
use super::memory::*;

pub const CONSTR_HEADER_SIZE: usize = 2;

impl Memory {
    pub fn alloc_constr(self: &mut Memory, tag: Word, args: &[Word]) -> Pointer {
        let p = self.alloc(args.len() + CONSTR_HEADER_SIZE);
        self.set_constr_tag(p, tag);
        self.set_constr_nargs(p, args.len());
        self.set_constr_args(p, args);
        p
    }

    pub fn get_constr_tag(self: &Memory, ptr: Pointer) -> Word {
        self[word_to_usize(ptr)]
    }

    pub fn get_constr_nargs(self: &Memory, ptr: Pointer) -> usize {
        word_to_usize(self[word_to_usize(ptr) + 1])
    }

    pub fn get_constr_arg(self: &Memory, ptr: Pointer, idx: Word) -> Word {
        self[word_to_usize(ptr) + CONSTR_HEADER_SIZE + word_to_usize(idx)]
    }

    pub fn get_constr_args(self: &Memory, ptr: Pointer) -> &[Word] {
        let i = word_to_usize(ptr) + CONSTR_HEADER_SIZE;
        let nargs = self.get_constr_nargs(ptr);
        &self[i..i + nargs]
    }

    pub fn mut_constr_args(self: &mut Memory, ptr: Pointer) -> &mut [Word] {
        let i = word_to_usize(ptr) + CONSTR_HEADER_SIZE;
        let nargs = self.get_constr_nargs(ptr);
        &mut self[i..i + nargs]
    }

    pub fn set_constr_tag(self: &mut Memory, ptr: Pointer, tag: Word) {
        self[word_to_usize(ptr)] = tag
    }

    pub fn set_constr_nargs(self: &mut Memory, ptr: Pointer, nfields: usize) {
        self[word_to_usize(ptr) + 1] = usize_to_word(nfields)
    }

    pub fn set_constr_arg(self: &mut Memory, ptr: Pointer, idx: Word, x: Word) {
        self[word_to_usize(ptr) + CONSTR_HEADER_SIZE + word_to_usize(idx)] = x
    }

    pub fn set_constr_args(self: &mut Memory, ptr: Pointer, args: &[Word]) {
        self.mut_constr_args(ptr).copy_from_slice(args);
    }
}
