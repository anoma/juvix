////////////////////////////////////////////////
// VampIR runtime for Juvix (unsafe version)
////////////////////////////////////////////////

def fst (x, y) = x;
def snd (x, y) = y;

def isBool x = (x * (x - 1) = 0);

def msb_rec x = {
    def x0 = fresh (x%2); isBool x0;
    def x1 = fresh (x\2);
    x = x0 + 2*x1;
    x1
};

def msb n x = iter (n - 1) msb_rec x;

def isNegative x = 1 - msb integerBits (x + 2^(integerBits - 1));
def isNegativeD x = 1 - msb (2*integerBits) (x + 2^(2*integerBits - 1));

def isZero x = {
    def xi = fresh (1 | x);
    def y = 1 - xi * x;
    x * y = 0;
    y
};

def divRem a b = {
    def q = fresh (a\b);
    def r = fresh (a%b);
    isNegative r = 0;
    isNegativeD (r - b) = 1;
    a = b * q + r; (q, r)
};

def number x = x + 0;
def fail = 0;

def add x y = x + y;
def sub x y = x - y;
def mul x y = x * y;

def equal x y = isZero (x - y);

def if b x y = b * x + (1 - b) * y;

def lessThan x y = isNegativeD (x - y);
def lessOrEqual x y = lessThan x (y + 1);

def div x y = fst (divRem x y);
def rem x y = snd (divRem x y);

////////////////////////////////////////////////
