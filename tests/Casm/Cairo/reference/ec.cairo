%builtins output ec_op

from starkware.cairo.common.cairo_builtins import EcOpBuiltin
from starkware.cairo.common.ec_point import EcPoint
from starkware.cairo.common.ec import StarkCurve

func ec_double(p: EcPoint) -> (r: EcPoint) {
    // (0, 0), which represents the point at infinity, is the only point with y = 0.
    if (p.y == 0) {
        return (r=p);
    }
    tempvar slope = (3 * p.x * p.x + StarkCurve.ALPHA) / (2 * p.y);
    tempvar r_x = slope * slope - p.x - p.x;
    return (r=EcPoint(x=r_x, y=slope * (p.x - r_x) - p.y));
}

func ec_op{ec_op_ptr: EcOpBuiltin*}(p: EcPoint, m: felt, q: EcPoint) -> (r: EcPoint) {
    assert ec_op_ptr.p = p;
    assert ec_op_ptr.q = q;
    assert ec_op_ptr.m = m;
    let r: EcPoint = ec_op_ptr.r;
    let ec_op_ptr = ec_op_ptr + EcOpBuiltin.SIZE;
    return (r=r);
}

func main{output_ptr, ec_op_ptr: EcOpBuiltin*}() {
  let p = EcPoint(x = 874739451078007766457464989774322083649278607533249481151382481072868806602, y = 152666792071518830868575557812948353041420400780739481342941381225525861407);
  let m = 9;
  let (q) = ec_double(p);
  let (r) = ec_op(p, m, q);
  assert [output_ptr] = r.x + r.y;
  let output_ptr = output_ptr + 1;
  return ();
}
