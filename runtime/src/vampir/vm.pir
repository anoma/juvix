
def hd (x:_) = x;

def zeros n = iter n (fun xs {0:xs}) [];

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
def write mem n v = fold mem (write_rec n v) (fun idx {[]}) 0 0;

def read_val mem val = {
    def (v, isReg) = decomp2 val;
    if isReg (read mem v) v
};

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
def OpEq = 6;
// load dest, src, offset
def OpLoad = 7;
// store dest, offset, val
def OpStore = 8;
// move dest, val, 0
def OpMove = 9;
// halt 0, 0, 0
def OpHalt = 10;
// alloc dest, num, 0
def OpAlloc = 11;
// push src, 0, 0
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
    def addr = read_val regs src;
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

def exec_alloc reg num _ (pc, sp, hp, regs, stack, heap) = {
    (pc + 1, sp, hp + num, write regs reg hp, stack, heap)
};

def exec_push reg _ _ (pc, sp, hp, regs, stack, heap) = {
    def v = read regs reg;
    (pc + 1, sp + 1, hp, regs, write stack sp v, heap)
};

def exec_pop reg _ _ (pc, sp, hp, regs, stack, heap) = {
    def v = read stack sp;
    (pc + 1, sp - 1, hp, write regs reg v, stack, heap)
};

def exec_jump _ val _ (pc, sp, hp, regs, stack, heap) = {
    def addr = read_val regs val;
    (addr, sp, hp, regs, stack, heap)
};

def exec_jumpz reg val _ (pc, sp, hp, regs, stack, heap) = {
    def addr = read_val regs val;
    def v = read regs reg;
    (if (isZero v) addr pc, sp, hp, regs, stack, heap)
};

def opcodes = ((OpIntAdd, exec_add):(OpIntSub, exec_sub):(OpIntMul, exec_mul):(OpIntDiv, exec_div):(OpIntMod, exec_mod):(OpIntLt, exec_lt):(OpIntEq, exec_eq):(OpIntLoad, exec_load):(OpIntStore, exec_store):(OpIntMove, exec_move):(OpIntHalt, exec_halt):(OpIntAlloc, exec_alloc):(OpIntPush, exec_push):(OpIntPop, exec_pop):(OpIntJump, exec_jump):(OpIntJumpOnZero, exec_jumpz):[]);

def run_rec code (pc, sp, hp, hp, regs, stack, heap) = {
    def (op, reg, val1, val2) = read code pc;
    switch opcodes reg val1 val2 (pc, sp, hp, hp, regs, stack, heap)
};

def run n code = {
    def (_, _, regs, _, _) = iter n (run_rec code) (0, 0, 0, zeros regsNum, zeros stackSize, zeros heapSize);
    hd regs
};
