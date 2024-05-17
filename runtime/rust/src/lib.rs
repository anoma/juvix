pub mod closure;
pub mod constr;
pub mod defs;
pub mod equality;
pub mod integer;
pub mod memory;
pub mod stack;

#[cfg(test)]
mod tests {
    use super::defs::*;
    use super::equality::*;
    use super::integer::*;

    fn program_fib(fid: Word, args: &[Word]) -> Word {
        const FUN_FIB: Word = 0;
        #[allow(unused_mut)]
        let mut tmp1: Word;
        #[allow(unused_mut)]
        let mut tmp2: Word;
        loop {
            match fid {
                FUN_FIB => {
                    if word_to_bool(smallint_le(args[0], make_smallint(1))) {
                        break args[0];
                    } else {
                        tmp1 = program_fib(0, &[smallint_sub(args[0], make_smallint(1))]);
                        tmp2 = program_fib(0, &[smallint_sub(args[0], make_smallint(2))]);
                        break (smallint_add(tmp1, tmp2));
                    }
                }
                _ => panic!("unknown function id"),
            }
        }
    }

    fn program_itfib(arg_fid: Word, args: &mut [Word]) -> Word {
        #[allow(unused_mut)]
        let mut fid = arg_fid;
        const FUN_ITFIB: Word = 0;
        const FUN_ITFIB_GO: Word = 1;
        #[allow(unused_mut)]
        let mut tmp1: Word;
        loop {
            match fid {
                FUN_ITFIB => {
                    args[1] = 0;
                    args[2] = 1;
                    fid = FUN_ITFIB_GO;
                    continue;
                }
                FUN_ITFIB_GO => {
                    if juvix_equal(args[0], make_smallint(0)) {
                        break args[1];
                    } else {
                        tmp1 = args[1];
                        args[1] = args[2];
                        args[2] = smallint_add(args[1], tmp1);
                        args[0] = smallint_sub(args[0], make_smallint(1));
                        fid = FUN_ITFIB_GO;
                        continue;
                    }
                }
                _ => panic!("unknown function id"),
            }
        }
    }

    #[test]
    fn test_fib() {
        let result = program_fib(0, &[11]);
        assert_eq!(result, 89);
    }

    #[test]
    fn test_itfib() {
        let result = program_itfib(0, &mut [11, 0, 0]);
        assert_eq!(result, 89);
    }
}
