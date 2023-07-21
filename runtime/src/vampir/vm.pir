/////////////////////////////////////////////////////////
// VampIR runtime VM primitives
/////////////////////////////////////////////////////////

def memSize = stackSize + heapSize;

/////////////////////////////////////////////////////////

def hd (x:_) = x;
def tl (_:xs) = xs;

def zeros n = iter n (fun xs {0:xs}) [];

def sum xs = fold xs (fun x y {x + y}) 0;

/////////////////////////////////////////////////////////

def Cst x = x * 2;
def Reg x = (x + 2) * 2 + 1;
def Sp = 0 * 2 + 1;
def Hp = 1 * 2 + 1;

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
// jump 0, val, 0
def OpJump = 14;
// jumpz reg, val, 0
def OpJumpOnZero = 15;

/////////////////////////////////////////////////////////

def exec_add reg val1 val2 (pc, regs, mem) = {
    (pc + 1, val1 + val2, memSize)
};

def exec_sub reg val1 val2 (pc, regs, mem) = {
    (pc + 1, val1 - val2, memSize)
};

def exec_mul reg val1 val2 (pc, regs, mem) = {
    (pc + 1, val1 * val2, memSize)
};

def exec_div reg val1 val2 (pc, regs, mem) = {
    (pc + 1, div val1 val2, memSize)
};

def exec_mod reg val1 val2 (pc, regs, mem) = {
    (pc + 1, rem val1 val2, memSize)
};

def exec_lt reg val1 val2 (pc, regs, mem) = {
    (pc + 1, lessThan val1 val2, memSize)
};

def exec_eq reg val1 val2 (pc, regs, mem) = {
    (pc + 1, equal val1 val2, memSize)
};

def exec_load reg addr off (pc, regs, mem) = {
    def v = read mem (addr + off);
    (pc + 1, v, memSize)
};

def exec_move reg val _ (pc, regs, mem) = {
    (pc + 1, val, memSize)
};

def exec_halt reg val1 val2 (pc, regs, mem) = {
    (pc, 0, memSize)
};

def exec_jumpz reg val addr (pc, regs, mem) = {
    (if (isZero val) addr (pc + 1), 0, memSize)
};

def exec_store dest off val (pc, regs, mem) = {
    def addr = read regs dest;
    (pc + 1, addr, addr + off)
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
                (OpJumpOnZero, exec_jumpz):
                []);

def exec_rec op reg val1 val2 state (opcode, f) step (apc, av, aaddr) = {
    def e = equal opcode op;
    def (pc, v, addr) = f reg val1 val2 state;
    step (
        apc + e * pc,
        av + e * v,
        aaddr + e * addr
    )
};

def exec op reg val1 val2 (pc, regs, mem) = {
    def v1 = read_val regs val1;
    def v2 = read_val regs val2;
    def (pc, v, addr) = fold opcodes (exec_rec op reg v1 v2 (pc, regs, mem)) (fun acc {acc}) (0, 0, 0);
    (pc, write regs reg v, write mem addr v)
};

def run_rec code state = {
    def (pc, regs, mem) = state;
    def (op, reg, val1, val2) = read_code code pc;
    exec op reg val1 val2 state
};

def run n code = {
    def (_, regs, _) = iter n (run_rec code) (0, write (zeros regsNum) Hp stackSize, zeros memSize);
    hd (tl (tl regs))
};
