////////////////////////////////////////////////
// VampIR runtime for Juvix (safe version)
////////////////////////////////////////////////

def fst (x, y) = x;
def snd (x, y) = y;

def isBool x = (x * (x - 1) = 0);

def isZero x = {
    def xi = fresh (1 | x);
    def y = 1 - xi * x;
    x * y = 0;
    y
};

def msb_rec x = {
    def x0 = fresh (x%2); isBool x0;
    def x1 = fresh (x\2);
    x = x0 + 2*x1;
    x1
};

def msb n x = iter (n - 1) msb_rec x;

def isNegative x = {
    def b = msb integerBits (x + 2^(integerBits - 1));
    def c = 1 - b;
    (c, isZero (b * c))
};
def isNegativeD x = {
    def b = msb (2*integerBits) (x + 2^(2*integerBits - 1));
    def c = 1 - b;
    (c, isZero (b * c))
};

def range_check x = {
    def b = msb integerBits (x + 2^(integerBits - 1));
    isZero (b * (b - 1))
};

def divRem a b = {
    def q = fresh (a\b);
    def r = fresh (a%b);
    def (r1, e1) = isNegative r;
    def (r2, e2) = isNegative (r - b);
    r1 = 0;
    r2 = 1;
    a = b * q + r;
    (q, r, e1 * e2)
};

def number x = (x + 0, 1);
def fail = (0, 0);

def add (x, e1) (y, e2) = {
    (x + y, e1 * e2 * range_check (x + y))
};
def sub (x, e1) (y, e2) = {
    (x - y, e1 * e2 * range_check (x - y))
};
def mul (x, e1) (y, e2) = {
    (x * y, e1 * e2 * range_check (x * y))
};

def equal (x, e1) (y, e2) = (isZero (x - y), e1 * e2);

def if (b, e) (x, e1) (y, e2) = (b * x + (1 - b) * y, e * (b * e1 + (1 - b) * e2));

def lessThan (x, e1) (y, e2) = {
    def (z, e) = isNegativeD (x - y);
    (z, e1 * e2 * e)
};
def lessOrEqual (x, e1) (y, e2) = {
    def (z, e) = isNegativeD (x - y - 1);
    (z, e1 * e2 * e)
};

def div (x, e1) (y, e2) = {
    def (z, _, e) = (divRem x y);
    (z, e1 * e2 * e)
};
def rem (x, e1) (y, e2) = {
    def (_, z, e) = divRem x y;
    (z, e1 * e2 * e)
};

////////////////////////////////////////////////
