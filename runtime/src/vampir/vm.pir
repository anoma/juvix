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

def LReg x = x * 2;
def LMem x = x * 2 + 1;

def Cst x = x * 3;
def Reg x = x * 3 + 1;
def Mem x = x * 3 + 2;

def decomp2 x = {
    def a = fresh (x \ 2);
    def b = fresh (x % 2);
    isBool b;
    x = 2 * a + b;
    (a, b)
};

def decomp3 x = {
    def a = fresh (x \ 3);
    def b = fresh (x % 3);
    b * (b - 1) * (b - 2) = 0;
    x = 3 * a + b;
    (a, b)
};

def read_rec n x step acc idx = step (acc + x * equal idx n) (idx + 1);
def read mem n = fold mem (read_rec n) (fun acc idx {acc}) 0 0;

def write_rec n v x step idx = {
    def e = equal idx n;
    (v * e + (1 - e) * x):(step (idx + 1))
};
def write mem n v = fold mem (write_rec n v) (fun idx {[]}) 0;

def read_val regs mem val = {
    def (v, r) = decomp3 val;
    def rv = read regs v;
    if (isZero r)
        v
        (if (r - 1) (read mem rv) rv)
};

def read_code_rec n (x1, x2, x3, x4) step (acc1, acc2, acc3, acc4) idx = {
    def e = equal idx n;
    step (acc1 + x1 * e, acc2 + x2 * e, acc3 + x3 * e, acc4 + x4 * e) (idx + 1)
};
def read_code mem n = fold mem (read_code_rec n) (fun acc idx {acc}) (0, 0, 0, 0) 0;

def write_reg regs reg val = {
    def (r0, isMem) = decomp2 reg;
    def r = if isMem regsNum r0;
    write regs r val
};

def write_mem regs mem reg val = {
    def (r0, isMem) = decomp2 reg;
    def addr = if isMem (read regs r0) memSize;
    write mem addr val
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
def OpIntEq = 6;
// jumpz 0, reg, val
def OpJumpOnZero = 7;
// halt 0, 0, 0
def OpHalt = 8;

/////////////////////////////////////////////////////////

def exec_add val1 val2 pc = {
    (pc + 1, val1 + val2)
};

def exec_sub val1 val2 pc = {
    (pc + 1, val1 - val2)
};

def exec_mul val1 val2 pc = {
    (pc + 1, val1 * val2)
};

def exec_div val1 val2 pc = {
    (pc + 1, div val1 val2)
};

def exec_mod val1 val2 pc = {
    (pc + 1, rem val1 val2)
};

def exec_lt val1 val2 pc = {
    (pc + 1, lessThan val1 val2)
};

def exec_eq val1 val2 pc = {
    (pc + 1, equal val1 val2)
};

def exec_move val _ pc = {
    (pc + 1, val)
};

def exec_jumpz val addr pc = {
    (if (isZero val) addr (pc + 1), 0)
};

def exec_halt val1 val2 pc = {
    (pc, 0)
};

def opcodes = ( (OpIntAdd, exec_add):
                (OpIntSub, exec_sub):
                (OpIntMul, exec_mul):
//                (OpIntDiv, exec_div):
//                (OpIntMod, exec_mod):
                (OpIntLt, exec_lt):
                (OpIntEq, exec_eq):
                (OpJumpOnZero, exec_jumpz):
                (OpHalt, exec_halt):
                []);

def exec_rec op reg val1 val2 pc0 (opcode, f) step (apc, av) = {
    def e = equal opcode op;
    def (pc, v) = f val1 val2 pc0;
    step (
        apc + e * pc,
        av + e * v
    )
};

def exec op reg val1 val2 (pc, regs, mem) = {
    def v1 = read_val regs mem val1;
    def v2 = read_val regs mem val2;
    def (pc, v) = fold opcodes (exec_rec op reg v1 v2 pc) (fun acc {acc}) (0, 0);
    (pc, write_reg regs reg v, write_mem regs mem reg v)
};

def run_rec code state = {
    def (pc, regs, mem) = state;
    def (op, reg, val1, val2) = read_code code pc;
    exec op reg val1 val2 state
};

def run n code = {
    def (_, regs, _) = iter n (run_rec code) (0, write (zeros regsNum) 1 stackSize, zeros memSize);
    hd (tl (tl regs))
};
