/////////////////////////////////////////////////////////
// VampIR runtime standard library
/////////////////////////////////////////////////////////

def isBool x = (x * (x - 1) = 0);

def msb_rec x = {
    def x0 = fresh (x%2); isBool x0;
    def x1 = fresh (x\2);
    x = x0 + 2*x1;
    x1
};

def msb n x = {
    def b = iter (n - 1) msb_rec x;
    isBool b;
    b
};

def isNegativeN n x = 1 - msb n (x + 2^(n - 1));
def isNegative x = isNegativeN integerBits x;

def add x y = x + y;
def sub x y = x - y;
def mul x y = x * y;

def isZero x = {
    def xi = fresh (1 | x);
    x * (1 - xi * x) = 0;
    1 - xi * x
};
def equal x y = isZero (x - y);

def if b x y = b * x + (1 - b) * y;

def lessThan x y = isNegativeN (integerBits + 1) (x - y);
def lessOrEqual x y = lessThan x (y + 1);

def divRem a b = {
    def q = fresh (a\b);
    def r = fresh (a%b);
    isNegative r = 0;
    lessThan r b = 1;
    a = b * q + r;
    (q, r)
};

def fst (x, y) = x;
def snd (x, y) = y;

def div x y = fst (divRem x y);
def rem x y = snd (divRem x y);

/////////////////////////////////////////////////////////
