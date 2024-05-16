// Closure manipulation

use super::defs::*;
use super::memory::*;

pub const CLOSURE_HEADER_SIZE: usize = 3;

impl Memory {
    pub fn alloc_closure(self: &mut Memory, fid: Word, args: &[Word], largs: usize) -> Pointer {
        let p = self.alloc(args.len() + CLOSURE_HEADER_SIZE);
        self.set_closure_fid(p, fid);
        self.set_closure_nargs(p, args.len());
        self.set_closure_largs(p, largs);
        self.set_closure_args(p, args);
        p
    }

    pub fn extend_closure(self: &mut Memory, ptr: Pointer, args: &[Word]) -> Pointer {
        let p = self.alloc(args.len() + CLOSURE_HEADER_SIZE);
        self.set_closure_fid(p, self.get_closure_fid(ptr));
        self.set_closure_nargs(p, self.get_closure_nargs(ptr) + args.len());
        self.set_closure_largs(p, self.get_closure_largs(ptr) - args.len());
        let i1 = word_to_usize(ptr) + CLOSURE_HEADER_SIZE;
        let nargs = self.get_closure_nargs(ptr);
        let i2 = word_to_usize(p) + CLOSURE_HEADER_SIZE;
        for i in 0..nargs - 1 {
            self[i2 + i] = self[i1 + i]
        }
        self[i2 + nargs..i2 + nargs + args.len() - 1].copy_from_slice(args);
        p
    }

    // Function id
    pub fn get_closure_fid(self: &Memory, ptr: Pointer) -> Word {
        self[word_to_usize(ptr)]
    }

    // The number of arguments stored in the closure
    pub fn get_closure_nargs(self: &Memory, ptr: Pointer) -> usize {
        word_to_usize(self[word_to_usize(ptr) + 1])
    }

    // The number of arguments remaining for a call to the function
    pub fn get_closure_largs(self: &Memory, ptr: Pointer) -> usize {
        word_to_usize(self[word_to_usize(ptr) + 2])
    }

    pub fn get_closure_arg(self: &Memory, ptr: Pointer, idx: Word) -> Word {
        self[word_to_usize(ptr) + CLOSURE_HEADER_SIZE + word_to_usize(idx)]
    }

    pub fn get_closure_args(self: &mut Memory, ptr: Pointer) -> &mut [Word] {
        let i = word_to_usize(ptr) + CLOSURE_HEADER_SIZE;
        let nargs = self.get_closure_nargs(ptr);
        &mut self[i..i + nargs - 1]
    }

    pub fn set_closure_fid(self: &mut Memory, ptr: Pointer, uid: Word) {
        self[word_to_usize(ptr)] = uid
    }

    pub fn set_closure_nargs(self: &mut Memory, ptr: Pointer, nargs: usize) {
        self[word_to_usize(ptr) + 1] = usize_to_word(nargs)
    }

    pub fn set_closure_largs(self: &mut Memory, ptr: Pointer, largs: usize) {
        self[word_to_usize(ptr) + 1] = usize_to_word(largs)
    }

    pub fn set_closure_arg(self: &mut Memory, ptr: Pointer, idx: Word, x: Word) {
        self[word_to_usize(ptr) + CLOSURE_HEADER_SIZE + word_to_usize(idx)] = x
    }

    pub fn set_closure_args(self: &mut Memory, ptr: Pointer, args: &[Word]) {
        self.get_closure_args(ptr).copy_from_slice(args);
    }
}
