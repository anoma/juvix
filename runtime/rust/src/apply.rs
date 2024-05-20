// Closure application

use super::defs::*;
use super::memory::*;

pub enum AppResult {
    // Call(fid)
    Call(Word, Vec<Word>),
    // Return(result_closure)
    Return(Word),
    // Continue(fid, args_left)
    Continue(Word, Vec<Word>, Vec<Word>),
}

impl Memory {
    pub fn apply(self: &mut Memory, cl: Word, cargs: &[Word]) -> AppResult {
        let argsnum = self.get_closure_largs(cl);
        if argsnum == cargs.len() {
            let (fid, args) = self.call_closure(cl, cargs);
            AppResult::Call(fid, args)
        } else if argsnum > cargs.len() {
            AppResult::Return(self.extend_closure(cl, cargs))
        } else {
            let (fid, args) = self.call_closure(cl, &cargs[0..argsnum]);
            AppResult::Continue(fid, args, Vec::from(&cargs[argsnum..cargs.len()]))
        }
    }
}

#[macro_export]
macro_rules! tapply {
    ($lab:lifetime, $program:ident, $mem:ident, $fid:ident, $args:expr, $cl0:expr, $cargs0:expr) => {
        let mut cl = $cl0;
        let mut cargs = $cargs0;
        loop {
            match $mem.apply( cl, &cargs) {
                apply::AppResult::Call(fid1, args1) => {
                    $fid = fid1;
                    $args = args1;
                    continue $lab;
                }
                apply::AppResult::Return(r) => break $lab r,
                apply::AppResult::Continue(fid1, args1, cargs1) => {
                    cl = $program($mem, fid1, args1);
                    cargs = cargs1;
                }
            }
        }
    };
}

#[macro_export]
macro_rules! apply {
    ($program:ident, $mem:ident, $cl0:expr, $cargs0:expr) => {{
        let mut cl = $cl0;
        let mut cargs = $cargs0;
        loop {
            match $mem.apply(cl, &cargs) {
                apply::AppResult::Call(fid, args) => {
                    break $program($mem, fid, args);
                }
                apply::AppResult::Return(r) => break r,
                apply::AppResult::Continue(fid1, args1, cargs1) => {
                    cl = $program($mem, fid1, args1);
                    cargs = cargs1;
                }
            }
        }
    }};
}
