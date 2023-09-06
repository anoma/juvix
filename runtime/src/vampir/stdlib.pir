// VampIR runtime for Juvix (safe version)

def fst (x, y) = x;
def snd (x, y) = y;

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

def range n x = msb n x;

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

def number x = (x + 0, 1);
def fail = (0, 0);

def add (x, e1) (y, e2) = {
    range integerBits (x + y);
    (x + y, e1 * e2)
};
def sub (x, e1) (y, e2) = {
    range integerBits (x - y);
    (x - y, e1 * e2)
};
def mul (x, e1) (y, e2) = {
    range integerBits (x * y);
    (x * y, e1 * e2)
};

def equal (x, e1) (y, e2) = (isZero (x - y), e1 * e2);

def if (b, e) (x, e1) (y, e2) = (b * x + (1 - b) * y, e * e1 * e2);

def lessThan (x, e1) (y, e2) = (isNegativeD (x - y), e1 * e2);
def lessOrEqual (x, e1) (y, e2) = (isNegativeD (x - y - 1), e1 * e2);

def div (x, e1) (y, e2) = (fst (divRem x y), e1 * e2);
def rem (x, e1) (y, e2) = (snd (divRem x y), e1 * e2);
