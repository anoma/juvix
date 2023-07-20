/////////////////////////////////////////////////////////
// VampIR runtime VM primitives
/////////////////////////////////////////////////////////

def hd (x:_) = x;

def zeros n = iter n (fun xs {0:xs}) [];

def map_rec f x acc = (f x):acc;
def map f xs = fold xs (map_rec f) [];

def sum xs = fold xs (fun x y {x + y}) 0;

def zipWith_rec f x z (y:ys) = (f x y):(z ys);
def zipWith f xs = fold xs (zipWith_rec f) (fun x {[]});

/////////////////////////////////////////////////////////

def Cst x = x * 2;
def Reg x = x * 2 + 1;

def decomp2 x = {
    def a = fresh (x \ 2);
    def b = fresh (x % 2);
    isBool b;
    x = 2 * a + b;
    (a, b)
};

def read_rec n x step acc idx = step (acc + x * equal idx n) (idx + 1);
def read mem n = fold mem (read_rec n) (fun acc idx {acc}) 0 0;

def write_rec n v x step idx = {
    def e = equal idx n;
    (v * e + (1 - e) * x):(step (idx + 1))
};
def write mem n v = fold mem (write_rec n v) (fun idx {[]}) 0;

def read_val mem val = {
    def (v, isReg) = decomp2 val;
    if isReg (read mem v) v
};

def read_code_rec n (x1, x2, x3, x4) step (acc1, acc2, acc3, acc4) idx = {
    def e = equal idx n;
    step (acc1 + x1 * e, acc2 + x2 * e, acc3 + x3 * e, acc4 + x4 * e) (idx + 1)
};
def read_code mem n = fold mem (read_code_rec n) (fun acc idx {acc}) (0, 0, 0, 0) 0;

/////////////////////////////////////////////////////////

// add dest, val1, val2
def OpIntAdd = 0;
// sub dest, val1, val2
def OpIntSub = 1;
// mul dest, val1, val2
def OpIntMul = 2;
// div dest, val1, val2
def OpIntDiv = 3;
// mod dest, val1, val2
def OpIntMod = 4;
// lt dest, val1, val2
def OpIntLt = 5;
// eq dest, val1, val2
def OpIntEq = 6;
// load dest, src, offset
def OpLoad = 7;
// store dest, offset, val
def OpStore = 8;
// move dest, val, 0
def OpMove = 9;
// halt 0, 0, 0
def OpHalt = 10;
// alloc dest, val, 0
def OpAlloc = 11;
// push 0, val, 0
def OpPush = 12;
// pop dest, 0, 0
def OpPop = 13;
// jump 0, val, 0
def OpJump = 14;
// jumpz reg, val, 0
def OpJumpOnZero = 15;

/////////////////////////////////////////////////////////

def exec_add reg val1 val2 (pc, sp, hp, regs, stack, heap) = {
    def v1 = read_val regs val1;
    def v2 = read_val regs val2;
    (pc + 1, sp, hp, write regs reg (v1 + v2), stack, heap)
};

def exec_sub reg val1 val2 (pc, sp, hp, regs, stack, heap) = {
    def v1 = read_val regs val1;
    def v2 = read_val regs val2;
    (pc + 1, sp, hp, write regs reg (v1 - v2), stack, heap)
};

def exec_mul reg val1 val2 (pc, sp, hp, regs, stack, heap) = {
    def v1 = read_val regs val1;
    def v2 = read_val regs val2;
    (pc + 1, sp, hp, write regs reg (v1 * v2), stack, heap)
};

def exec_div reg val1 val2 (pc, sp, hp, regs, stack, heap) = {
    def v1 = read_val regs val1;
    def v2 = read_val regs val2;
    (pc + 1, sp, hp, write regs reg (div v1 v2), stack, heap)
};

def exec_mod reg val1 val2 (pc, sp, hp, regs, stack, heap) = {
    def v1 = read_val regs val1;
    def v2 = read_val regs val2;
    (pc + 1, sp, hp, write regs reg (rem v1 v2), stack, heap)
};

def exec_lt reg val1 val2 (pc, sp, hp, regs, stack, heap) = {
    def v1 = read_val regs val1;
    def v2 = read_val regs val2;
    (pc + 1, sp, hp, write regs reg (lessThan v1 v2), stack, heap)
};

def exec_eq reg val1 val2 (pc, sp, hp, regs, stack, heap) = {
    def v1 = read_val regs val1;
    def v2 = read_val regs val2;
    (pc + 1, sp, hp, write regs reg (equal v1 v2), stack, heap)
};

def exec_load dest src off (pc, sp, hp, regs, stack, heap) = {
    def addr = read regs src;
    def v = read heap (addr + off);
    (pc + 1, sp, hp, write regs dest v, stack, heap)
};

def exec_store dest off val (pc, sp, hp, regs, stack, heap) = {
    def v = read_val regs val;
    def addr = read regs dest;
    (pc + 1, sp, hp, regs, stack, write heap (addr + off) v)
};

def exec_move reg val _ (pc, sp, hp, regs, stack, heap) = {
    def v = read_val regs val;
    (pc + 1, sp, hp, write regs reg v, stack, heap)
};

def exec_halt reg val1 val2 (pc, sp, hp, regs, stack, heap) = {
    (pc, sp, hp, regs, stack, heap)
};

def exec_alloc reg val _ (pc, sp, hp, regs, stack, heap) = {
    def v = read_val regs val;
    (pc + 1, sp, hp + v, write regs reg hp, stack, heap)
};

def exec_push _ val _ (pc, sp, hp, regs, stack, heap) = {
    def v = read_val regs val;
    (pc + 1, sp + 1, hp, regs, write stack sp v, heap)
};

def exec_pop reg _ _ (pc, sp, hp, regs, stack, heap) = {
    def v = read stack (sp - 1);
    (pc + 1, sp - 1, hp, write regs reg v, stack, heap)
};

def exec_jump _ val _ (pc, sp, hp, regs, stack, heap) = {
    def addr = read_val regs val;
    (addr, sp, hp, regs, stack, heap)
};

def exec_jumpz reg val _ (pc, sp, hp, regs, stack, heap) = {
    def addr = read_val regs val;
    def v = read regs reg;
    (if (isZero v) addr (pc + 1), sp, hp, regs, stack, heap)
};

def opcodes = ( (OpIntAdd, exec_add):
                (OpIntSub, exec_sub):
                (OpIntMul, exec_mul):
//                (OpIntDiv, exec_div):
//                (OpIntMod, exec_mod):
                (OpIntLt, exec_lt):
                (OpIntEq, exec_eq):
                (OpLoad, exec_load):
                (OpStore, exec_store):
                (OpMove, exec_move):
                (OpHalt, exec_halt):
                (OpAlloc, exec_alloc):
                (OpPush, exec_push):
                (OpPop, exec_pop):
                (OpJump, exec_jump):
                (OpJumpOnZero, exec_jumpz):
                []);

def zeroState = (0, 0, 0, zeros regsNum, zeros stackSize, zeros heapSize);

def exec_rec op reg val1 val2 state (opcode, f) step (apc, asp, ahp, aregs, astack, aheap) = {
    def e = equal opcode op;
    def (pc, sp, hp, regs, stack, heap) = f reg val1 val2 state;
    step (
        apc + e * pc,
        asp + e * sp,
        ahp + e * hp,
        zipWith (fun x1 x2 {x1 + e * x2}) aregs regs,
        zipWith (fun x1 x2 {x1 + e * x2}) astack stack,
        zipWith (fun x1 x2 {x1 + e * x2}) aheap heap
    )
};

def exec op reg val1 val2 state = fold opcodes (exec_rec op reg val1 val2 state) (fun acc {acc}) zeroState;

def run_rec code state = {
    def (pc, sp, hp, regs, stack, heap) = state;
    def (op, reg, val1, val2) = read_code code pc;
    exec op reg val1 val2 state
};

def run n code = {
    def (_, _, _, regs, _, _) = iter n (run_rec code) (0, 0, 1, zeros regsNum, zeros stackSize, zeros heapSize);
    hd regs
};
