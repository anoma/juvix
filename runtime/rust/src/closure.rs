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
        let nargs = self.get_closure_nargs(ptr);
        let p = self.alloc(nargs + args.len() + CLOSURE_HEADER_SIZE);
        self.set_closure_fid(p, self.get_closure_fid(ptr));
        self.set_closure_nargs(p, self.get_closure_nargs(ptr) + args.len());
        self.set_closure_largs(p, self.get_closure_largs(ptr) - args.len());
        let i1 = word_to_usize(ptr) + CLOSURE_HEADER_SIZE;
        let i2 = word_to_usize(p) + CLOSURE_HEADER_SIZE;
        for i in 0..nargs {
            self[i2 + i] = self[i1 + i]
        }
        self[i2 + nargs..i2 + nargs + args.len()].copy_from_slice(args);
        p
    }

    // Assigns stored closure args plus the provided closure args (`cargs`) to the
    // argument space (`args`). Returns the function id to call.
    pub fn call_closure(self: &Memory, cl: Word, cargs: &[Word]) -> (Word, Vec<Word>) {
        let mut args = Vec::from(self.get_closure_args(cl));
        args.extend(cargs);
        return (self.get_closure_fid(cl), args);
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

    pub fn get_closure_args(self: &Memory, ptr: Pointer) -> &[Word] {
        let i = word_to_usize(ptr) + CLOSURE_HEADER_SIZE;
        let nargs = self.get_closure_nargs(ptr);
        &self[i..i + nargs]
    }

    pub fn mut_closure_args(self: &mut Memory, ptr: Pointer) -> &mut [Word] {
        let i = word_to_usize(ptr) + CLOSURE_HEADER_SIZE;
        let nargs = self.get_closure_nargs(ptr);
        &mut self[i..i + nargs]
    }

    pub fn set_closure_fid(self: &mut Memory, ptr: Pointer, uid: Word) {
        self[word_to_usize(ptr)] = uid
    }

    pub fn set_closure_nargs(self: &mut Memory, ptr: Pointer, nargs: usize) {
        self[word_to_usize(ptr) + 1] = usize_to_word(nargs)
    }

    pub fn set_closure_largs(self: &mut Memory, ptr: Pointer, largs: usize) {
        self[word_to_usize(ptr) + 2] = usize_to_word(largs)
    }

    pub fn set_closure_arg(self: &mut Memory, ptr: Pointer, idx: Word, x: Word) {
        self[word_to_usize(ptr) + CLOSURE_HEADER_SIZE + word_to_usize(idx)] = x
    }

    pub fn set_closure_args(self: &mut Memory, ptr: Pointer, args: &[Word]) {
        self.mut_closure_args(ptr).copy_from_slice(args);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_closures() {
        let mut mem = Memory::new();
        for fid in 0..10 {
            for k in 0..100 {
                let args: Vec<Word> = (0..k).collect();
                let largs = (k / 2) as usize;
                let cl = mem.alloc_closure(fid, &args, largs);
                assert_eq!(mem.get_closure_fid(cl), fid);
                assert_eq!(mem.get_closure_nargs(cl), args.len());
                assert_eq!(mem.get_closure_largs(cl), largs);
                assert_eq!(mem.get_closure_args(cl), args);
                mem.set_closure_fid(cl, fid + 1);
                assert_eq!(mem.get_closure_fid(cl), fid + 1);
                if k > 0 {
                    mem.set_closure_arg(cl, k / 2, 789);
                    assert_eq!(mem.get_closure_arg(cl, k / 2), 789);
                }
                if largs > 1 {
                    let n: Word = k + largs as Word - 1;
                    let args1: Vec<Word> = (k..n).collect();
                    let cl1 = mem.extend_closure(cl, &args1);
                    let mut args2: Vec<Word> = (0..n).collect();
                    if k > 0 {
                        args2[(k / 2) as usize] = 789;
                    }
                    assert_eq!(mem.get_closure_args(cl1), args2);
                    assert_eq!(mem.get_closure_nargs(cl1), args2.len());
                    assert_eq!(mem.get_closure_largs(cl1), 1);
                }
            }
        }
    }
}
