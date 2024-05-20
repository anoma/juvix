#[macro_use]
pub mod apply;
pub mod closure;
pub mod constr;
pub mod defs;
pub mod equality;
pub mod integer;
pub mod memory;

#[cfg(test)]
mod tests {
    use super::apply;
    use super::defs::*;
    use super::equality::*;
    use super::integer::*;
    use super::memory::*;

    #[allow(unused_mut)]
    fn program_fib(mut fid: Word, args: Vec<Word>) -> Word {
        const FUN_FIB: Word = 0;
        loop {
            match fid {
                FUN_FIB => {
                    #[allow(unused_mut)]
                    let mut tmp1: Word;
                    #[allow(unused_mut)]
                    let mut tmp2: Word;
                    if word_to_bool(smallint_le(args[0], make_smallint(1))) {
                        break args[0];
                    } else {
                        tmp1 = program_fib(FUN_FIB, vec![smallint_sub(args[0], make_smallint(1))]);
                        tmp2 = program_fib(FUN_FIB, vec![smallint_sub(args[0], make_smallint(2))]);
                        break (smallint_add(tmp1, tmp2));
                    }
                }
                _ => panic!("unknown function id"),
            }
        }
    }

    fn program_itfib(mut fid: Word, mut args: Vec<Word>) -> Word {
        const FUN_ITFIB: Word = 0;
        const FUN_ITFIB_GO: Word = 1;
        loop {
            match fid {
                FUN_ITFIB => {
                    args = vec![args[0], 0, 1];
                    fid = FUN_ITFIB_GO;
                    continue;
                }
                FUN_ITFIB_GO => {
                    if juvix_equal(args[0], make_smallint(0)) {
                        break args[1];
                    } else {
                        args = vec![
                            smallint_sub(args[0], make_smallint(1)),
                            args[2],
                            smallint_add(args[1], args[2]),
                        ];
                        fid = FUN_ITFIB_GO;
                        continue;
                    }
                }
                _ => panic!("unknown function id"),
            }
        }
    }

    fn program_closure_call(mem: &mut Memory, mut fid: Word, mut args: Vec<Word>) -> Word {
        const FUN_MAIN: Word = 0;
        const FUN_CALCULATE: Word = 1;
        const FUN_APPLY_1: Word = 2;
        loop {
            match fid {
                FUN_MAIN => {
                    args[0] =
                        mem.alloc_closure(FUN_CALCULATE, &[make_smallint(5), make_smallint(3)], 1);
                    fid = FUN_APPLY_1;
                    continue;
                }
                FUN_CALCULATE => {
                    #[allow(unused_mut)]
                    let mut tmp1: Word;
                    tmp1 = smallint_mul(args[2], args[1]);
                    tmp1 = smallint_add(args[0], tmp1);
                    return tmp1;
                }
                FUN_APPLY_1 => {
                    (fid, args) = mem.call_closure(args[0], &[make_smallint(2)]);
                    continue;
                }
                _ => panic!("unknown function id"),
            }
        }
    }

    fn program_sk(mem: &mut Memory, mut fid: Word, mut args: Vec<Word>) -> Word {
        const FUN_MAIN: Word = 0;
        const FUN_S: Word = 1;
        const FUN_K: Word = 2;
        const FUN_I: Word = 3;
        'program: loop {
            match fid {
                FUN_MAIN => {
                    let id = program_sk(mem, FUN_I, vec![]);
                    let x = apply!(
                        program_sk,
                        mem,
                        id,
                        vec![id, id, id, id, id, id, make_smallint(1)]
                    );
                    let y = apply!(
                        program_sk,
                        mem,
                        id,
                        vec![id, id, id, id, id, id, id, id, id, make_smallint(1)]
                    );
                    let z = apply!(
                        program_sk,
                        mem,
                        id,
                        vec![id, id, id, id, id, id, id, id, id, id, id, make_smallint(1)]
                    );
                    let tmp1 = smallint_add(x, y);
                    break smallint_add(tmp1, z);
                }
                FUN_S => {
                    let xz = apply!(program_sk, mem, args[0], vec![args[2]]);
                    let yz = apply!(program_sk, mem, args[1], vec![args[2]]);
                    tapply!('program, program_sk, mem, fid, args, xz, vec![yz]);
                }
                FUN_K => {
                    break args[0];
                }
                FUN_I => {
                    let k = mem.alloc_closure(FUN_K, &[], 2);
                    break mem.alloc_closure(FUN_S, &[k, k], 1);
                }
                _ => panic!("unknown function id"),
            }
        }
    }

    #[allow(unused_mut)]
    fn program_lists(mem: &mut Memory, mut fid: Word, mut args: Vec<Word>) -> Word {
        const FUN_MAIN: Word = 0;
        const FUN_MAP: Word = 1;
        const FUN_ADD_ONE: Word = 2;
        const FUN_SUM: Word = 3;
        const FUN_GEN: Word = 4;
        const TAG_NIL: Word = 0;
        const TAG_CONS: Word = 1;
        loop {
            match fid {
                FUN_MAIN => {
                    let lst1 = program_lists(mem, FUN_GEN, vec![make_smallint(1000)]);
                    let inc = mem.alloc_closure(FUN_ADD_ONE, &[], 1);
                    let lst2 = program_lists(mem, FUN_MAP, vec![inc, lst1]);
                    return program_lists(mem, FUN_SUM, vec![lst2]);
                }
                FUN_MAP => match mem.get_constr_tag(args[1]) {
                    TAG_NIL => {
                        return args[1];
                    }
                    TAG_CONS => {
                        let h = apply!(
                            program_lists,
                            mem,
                            args[0],
                            vec![mem.get_constr_arg(args[1], 0)]
                        );
                        let t = program_lists(
                            mem,
                            FUN_MAP,
                            vec![args[0], mem.get_constr_arg(args[1], 1)],
                        );
                        return mem.alloc_constr(TAG_CONS, &[h, t]);
                    }
                    _ => panic!("unknown constructor tag"),
                },
                FUN_ADD_ONE => {
                    return smallint_add(args[0], make_smallint(1));
                }
                FUN_SUM => match mem.get_constr_tag(args[0]) {
                    TAG_NIL => return make_smallint(0),
                    TAG_CONS => {
                        let s = program_lists(mem, FUN_SUM, vec![mem.get_constr_arg(args[0], 1)]);
                        return smallint_add(s, mem.get_constr_arg(args[0], 0));
                    }
                    _ => panic!("unknown constructor tag"),
                },
                FUN_GEN => {
                    if args[0] == make_smallint(0) {
                        return mem.alloc_constr(TAG_NIL, &[]);
                    } else {
                        let t = program_lists(
                            mem,
                            FUN_GEN,
                            vec![smallint_sub(args[0], make_smallint(1))],
                        );
                        return mem.alloc_constr(TAG_CONS, &[args[0], t]);
                    }
                }
                _ => panic!("unknown function id"),
            }
        }
    }

    #[test]
    fn test_fib() {
        let result = program_fib(0, vec![11]);
        assert_eq!(result, 89);
    }

    #[test]
    fn test_itfib() {
        let result = program_itfib(0, vec![11, 0, 0]);
        assert_eq!(result, 89);
    }

    #[test]
    fn test_closure_call() {
        let result = program_closure_call(&mut Memory::new(), 0, vec![0, 0, 0]);
        assert_eq!(result, 11);
    }

    #[test]
    fn test_sk() {
        let result = program_sk(&mut Memory::new(), 0, vec![]);
        assert_eq!(result, 3);
    }

    #[test]
    fn test_lists() {
        let result = program_lists(&mut Memory::new(), 0, vec![]);
        assert_eq!(result, 501500);
    }
}
