//! Special mathematical functions
//!
//! Port of fspec.pas and fmath.pas special functions.
//! Includes Gamma, incomplete Gamma, Beta, incomplete Beta, and error functions.

use super::constants::*;
use std::f64::consts::PI;

/// Table of factorials for n = 0..33
const FACT_ARRAY: [f64; 34] = [
    1.0,
    1.0,
    2.0,
    6.0,
    24.0,
    120.0,
    720.0,
    5040.0,
    40320.0,
    362880.0,
    3628800.0,
    39916800.0,
    479001600.0,
    6227020800.0,
    87178291200.0,
    1307674368000.0,
    20922789888000.0,
    355687428096000.0,
    6402373705728000.0,
    121645100408832000.0,
    2432902008176640000.0,
    51090942171709440000.0,
    1124000727777607680000.0,
    25852016738884976640000.0,
    620448401733239439360000.0,
    15511210043330985984000000.0,
    403291461126605635584000000.0,
    10888869450418352160768000000.0,
    304888344611713860501504000000.0,
    8841761993739701954543616000000.0,
    265252859812191058636308480000000.0,
    8222838654177922817725562880000000.0,
    263130836933693530167218012160000000.0,
    8683317618811886495518194401280000000.0,
];

/// Safe exponential function with overflow/underflow protection
#[inline]
pub fn expo(x: f64) -> f64 {
    if x < MINLOG {
        0.0
    } else if x > MAXLOG {
        MAXNUM
    } else {
        x.exp()
    }
}

/// Safe natural logarithm with domain checking
#[inline]
pub fn log(x: f64) -> f64 {
    if x < 0.0 {
        -MAXNUM
    } else if x == 0.0 {
        -MAXNUM
    } else {
        x.ln()
    }
}

/// Power function with special case handling
pub fn power(x: f64, y: f64) -> f64 {
    if x == 0.0 {
        if y == 0.0 {
            1.0 // 0^0 = 1 by convention
        } else if y > 0.0 {
            0.0
        } else {
            MAXNUM // 0^(-y) = infinity
        }
    } else if y == 0.0 {
        1.0
    } else if y == 1.0 {
        x
    } else if x < 0.0 {
        // For negative x, only integer powers are valid
        let y_int = y.trunc();
        if (y - y_int).abs() < 1e-10 {
            int_power(x, y_int as i64)
        } else {
            0.0 // Domain error
        }
    } else {
        let y_ln_x = y * x.ln();
        if y_ln_x < MINLOG {
            0.0
        } else if y_ln_x > MAXLOG {
            MAXNUM
        } else {
            y_ln_x.exp()
        }
    }
}

/// Integer power by repeated multiplication (Legendre's algorithm)
fn int_power(x: f64, n: i64) -> f64 {
    if n == 0 {
        return 1.0;
    }
    if x == 0.0 {
        return if n > 0 { 0.0 } else { MAXNUM };
    }

    let mut result = 1.0;
    let mut base = if n < 0 { 1.0 / x } else { x };
    let mut exp = n.abs();

    while exp > 0 {
        if exp & 1 == 1 {
            result *= base;
        }
        base *= base;
        exp >>= 1;
    }

    result
}

/// Sign of Gamma function
pub fn sgn_gamma(x: f64) -> i32 {
    if x > 0.0 {
        1
    } else if (x.abs().trunc() as i64) % 2 == 1 {
        1
    } else {
        -1
    }
}

/// Stirling's formula for Gamma function
/// Gamma(x) = sqrt(2*Pi) * x^(x-0.5) * exp(-x) * (1 + 1/x * P(1/x))
fn stirf(x: f64) -> f64 {
    const STIR: [f64; 9] = [
        7.147391378143610789273e-4,
        -2.363848809501759061727e-5,
        -5.950237554056330156018e-4,
        6.989332260623193171870e-5,
        7.840334842744753003862e-4,
        -2.294719747873185405699e-4,
        -2.681327161876304418288e-3,
        3.472222222230075327854e-3,
        8.333333333333331800504e-2,
    ];

    let w = 1.0 / x;
    let p = if x > 1024.0 {
        let mut p = 6.97281375836585777429e-5 * w + 7.84039221720066627474e-4;
        p = p * w - 2.29472093621399176955e-4;
        p = p * w - 2.68132716049382716049e-3;
        p = p * w + 3.47222222222222222222e-3;
        p * w + 8.33333333333333333333e-2
    } else {
        pol_evl(w, &STIR)
    };

    SQRT2PI * ((x - 0.5) * x.ln() - x).exp() * (1.0 + w * p)
}

/// Gamma function for small values
fn gam_small(x1: f64, z: f64) -> f64 {
    const S: [f64; 9] = [
        -1.193945051381510095614e-3,
        7.220599478036909672331e-3,
        -9.622023360406271645744e-3,
        -4.219773360705915470089e-2,
        1.665386113720805206758e-1,
        -4.200263503403344054473e-2,
        -6.558780715202540684668e-1,
        5.772156649015328608253e-1,
        1.000000000000000000000e0,
    ];

    const SN: [f64; 9] = [
        1.133374167243894382010e-3,
        7.220837261893170325704e-3,
        9.621911155035976733706e-3,
        -4.219773343731191721664e-2,
        -1.665386113944413519335e-1,
        -4.200263503402112910504e-2,
        6.558780715202536547116e-1,
        5.772156649015328608727e-1,
        -1.000000000000000000000e0,
    ];

    if x1 == 0.0 {
        return MAXNUM;
    }

    let (x_abs, p) = if x1 < 0.0 {
        (-x1, pol_evl(-x1, &SN))
    } else {
        (x1, pol_evl(x1, &S))
    };

    z / (x_abs * p)
}

/// Evaluate polynomial with coefficients in reverse order
fn pol_evl(x: f64, coef: &[f64]) -> f64 {
    let mut ans = coef[0];
    for &c in &coef[1..] {
        ans = ans * x + c;
    }
    ans
}

/// Evaluate polynomial with leading coefficient = 1
fn p1_evl(x: f64, coef: &[f64]) -> f64 {
    let mut ans = x + coef[0];
    for &c in &coef[1..] {
        ans = ans * x + c;
    }
    ans
}

/// Gamma function
pub fn gamma(x: f64) -> f64 {
    const P: [f64; 8] = [
        4.212760487471622013093e-5,
        4.542931960608009155600e-4,
        4.092666828394035500949e-3,
        2.385363243461108252554e-2,
        1.113062816019361559013e-1,
        3.629515436640239168939e-1,
        8.378004301573126728826e-1,
        1.000000000000000000009e0,
    ];

    const Q: [f64; 9] = [
        -1.397148517476170440917e-5,
        2.346584059160635244282e-4,
        -1.237799246653152231188e-3,
        -7.955933682494738320586e-4,
        2.773706565840072979165e-2,
        -4.633887671244534213831e-2,
        -2.243510905670329164562e-1,
        4.150160950588455434583e-1,
        9.999999999999999999908e-1,
    ];

    let sgn_gam = sgn_gamma(x);

    // Check for singularities and overflow
    if x == 0.0 || (x < 0.0 && x.fract() == 0.0) {
        return sgn_gam as f64 * MAXNUM;
    }

    if x > MAXGAM {
        return MAXNUM;
    }

    let a = x.abs();
    if a > 13.0 {
        if x < 0.0 {
            let n = a.trunc() as i64;
            let mut z = a - n as f64;
            if z > 0.5 {
                z = a - (n + 1) as f64;
            }
            z = (a * (PI * z).sin()).abs() * stirf(a);
            if z <= PI / MAXNUM {
                return sgn_gam as f64 * MAXNUM;
            }
            sgn_gam as f64 * (PI / z)
        } else {
            sgn_gam as f64 * stirf(x)
        }
    } else {
        let mut z = 1.0;
        let mut x1 = x;

        while x1 >= 3.0 {
            x1 -= 1.0;
            z *= x1;
        }

        while x1 < -0.03125 {
            z /= x1;
            x1 += 1.0;
        }

        if x1 <= 0.03125 {
            gam_small(x1, z)
        } else {
            while x1 < 2.0 {
                z /= x1;
                x1 += 1.0;
            }

            if x1 == 2.0 || x1 == 3.0 {
                z
            } else {
                let x1 = x1 - 2.0;
                z * pol_evl(x1, &P) / pol_evl(x1, &Q)
            }
        }
    }
}

/// Stirling's formula for log Gamma (for x >= 13)
fn stirf_l(x: f64) -> f64 {
    const P: [f64; 7] = [
        4.885026142432270781165e-3,
        -1.880801938119376907179e-3,
        8.412723297322498080632e-4,
        -5.952345851765688514613e-4,
        7.936507795855070755671e-4,
        -2.777777777750349603440e-3,
        8.333333333333331447505e-2,
    ];

    let mut q = x.ln() * (x - 0.5) - x;
    q += LNSQRT2PI;

    if x > 1.0e10 {
        q
    } else {
        let w = 1.0 / (x * x);
        q + pol_evl(w, &P) / x
    }
}

/// Natural logarithm of Gamma function
pub fn ln_gamma(x: f64) -> f64 {
    const P: [f64; 7] = [
        -2.163690827643812857640e3,
        -8.723871522843511459790e4,
        -1.104326814691464261197e6,
        -6.111225012005214299996e6,
        -1.625568062543700591014e7,
        -2.003937418103815175475e7,
        -8.875666783650703802159e6,
    ];

    const Q: [f64; 7] = [
        -5.139481484435370143617e2,
        -3.403570840534304670537e4,
        -6.227441164066219501697e5,
        -4.814940379411882186630e6,
        -1.785433287045078156959e7,
        -3.138646407656182662088e7,
        -2.099336717757895876142e7,
    ];

    let sgn_gam = sgn_gamma(x);

    // Check for singularities
    if x == 0.0 || (x < 0.0 && x.fract() == 0.0) {
        return MAXNUM;
    }

    if x > MAXLGM {
        return MAXNUM;
    }

    let a = x.abs();
    if a > 34.0 {
        if x < 0.0 {
            let n = a.trunc() as i64;
            let mut z = a - n as f64;
            if z > 0.5 {
                z = (n + 1) as f64 - a;
            }
            z = a * (PI * z).sin();
            if z == 0.0 {
                return sgn_gam as f64 * MAXNUM;
            }
            LNPI - z.ln() - stirf_l(a)
        } else {
            stirf_l(x)
        }
    } else if x < 13.0 {
        let mut z = 1.0;
        let mut x1 = x;

        while x1 >= 3.0 {
            x1 -= 1.0;
            z *= x1;
        }

        while x1 < 2.0 {
            if x1.abs() <= 0.03125 {
                return gam_small(x1, z).abs().ln();
            }
            z /= x1;
            x1 += 1.0;
        }

        let z = if z < 0.0 { -z } else { z };
        if x1 == 2.0 {
            z.ln()
        } else {
            let x1 = x1 - 2.0;
            x1 * pol_evl(x1, &P) / p1_evl(x1, &Q) + z.ln()
        }
    } else {
        stirf_l(x)
    }
}

/// Incomplete Gamma function (regularized lower)
/// Returns P(a, x) = integral from 0 to x of t^(a-1) * e^(-t) dt / Gamma(a)
pub fn igamma(a: f64, x: f64) -> f64 {
    if x <= 0.0 || a <= 0.0 {
        return 0.0;
    }

    if x > 1.0 && x > a {
        return 1.0 - jgamma(a, x);
    }

    let ax = a * x.ln() - x - ln_gamma(a);
    if ax < MINLOG {
        return 0.0;
    }

    let ax = ax.exp();

    // Power series
    let mut r = a;
    let mut c = 1.0;
    let mut ans = 1.0;

    loop {
        r += 1.0;
        c *= x / r;
        ans += c;
        if c / ans <= MACHEP {
            break;
        }
    }

    ans * ax / a
}

/// Complement of incomplete Gamma function
/// Returns Q(a, x) = 1 - P(a, x)
pub fn jgamma(a: f64, x: f64) -> f64 {
    if x <= 0.0 || a <= 0.0 {
        return 1.0;
    }

    if x < 1.0 || x < a {
        return 1.0 - igamma(a, x);
    }

    let ax = a * x.ln() - x - ln_gamma(a);
    if ax < MINLOG {
        return 0.0;
    }

    let ax = ax.exp();

    // Continued fraction
    let mut y = 1.0 - a;
    let mut z = x + y + 1.0;
    let mut c = 0.0;
    let mut pkm2 = 1.0;
    let mut qkm2 = x;
    let mut pkm1 = x + 1.0;
    let mut qkm1 = z * x;
    let mut ans = pkm1 / qkm1;

    loop {
        c += 1.0;
        y += 1.0;
        z += 2.0;
        let yc = y * c;
        let pk = pkm1 * z - pkm2 * yc;
        let qk = qkm1 * z - qkm2 * yc;

        let (t, new_ans) = if qk != 0.0 {
            let r = pk / qk;
            let t = ((ans - r) / r).abs();
            (t, r)
        } else {
            (1.0, ans)
        };
        ans = new_ans;

        pkm2 = pkm1;
        pkm1 = pk;
        qkm2 = qkm1;
        qkm1 = qk;

        if pk.abs() > BIG {
            pkm2 *= BIGINV;
            pkm1 *= BIGINV;
            qkm2 *= BIGINV;
            qkm1 *= BIGINV;
        }

        if t <= MACHEP {
            break;
        }
    }

    ans * ax
}

/// Factorial function
pub fn fact(n: i32) -> f64 {
    if n < 0 {
        1.0 // Domain error, return default
    } else if n > MAXFAC {
        MAXNUM
    } else if (n as usize) < FACT_ARRAY.len() {
        FACT_ARRAY[n as usize]
    } else {
        gamma((n + 1) as f64)
    }
}

/// Binomial coefficient C(n, k)
pub fn binomial(n: i32, k: i32) -> f64 {
    if k < 0 {
        0.0
    } else if k == 0 || k == n {
        1.0
    } else if k == 1 || k == n - 1 {
        n as f64
    } else {
        let k = if k > n - k { n - k } else { k };
        let n1 = n + 1;
        let mut prod = n as f64;
        for i in 2..=k {
            prod *= (n1 - i) as f64 / i as f64;
        }
        (0.5 + prod).trunc()
    }
}

/// Beta function B(x, y) = Gamma(x) * Gamma(y) / Gamma(x + y)
pub fn beta(x: f64, y: f64) -> f64 {
    let sgn_beta = sgn_gamma(x) * sgn_gamma(y) * sgn_gamma(x + y);
    let lxy = ln_gamma(x + y);
    let lx = ln_gamma(x);
    let ly = ln_gamma(y);
    sgn_beta as f64 * (lx + ly - lxy).exp()
}

/// Incomplete Beta function
pub fn ibeta(a: f64, b: f64, x: f64) -> f64 {
    if a <= 0.0 || b <= 0.0 || x < 0.0 {
        return 0.0;
    }

    if x > 1.0 {
        return 1.0;
    }

    if x == 0.0 || x == 1.0 {
        return x;
    }

    let mut flag = false;

    // Check if power series is appropriate
    if b * x <= 1.0 && x <= 0.95 {
        return pseries(a, b, x);
    }

    let w = 1.0 - x;

    // Reverse a and b if x is greater than the mean
    let (a1, b1, xc, x1) = if x > a / (a + b) {
        flag = true;
        (b, a, x, w)
    } else {
        (a, b, w, x)
    };

    if flag && b1 * x1 <= 1.0 && x1 <= 0.95 {
        let t = pseries(a1, b1, x1);
        return if t <= MACHEP { 1.0 - MACHEP } else { 1.0 - t };
    }

    // Choose expansion for optimal convergence
    let y = x1 * (a1 + b1 - 2.0) - (a1 - 1.0);
    let w = if y < 0.0 {
        cfrac1(a1, b1, x1)
    } else {
        cfrac2(a1, b1, x1) / xc
    };

    // Multiply by the factor x^a * (1-x)^b * Gamma(a+b) / (a * Gamma(a) * Gamma(b))
    let y = a1 * x1.ln();
    let t_ln = b1 * xc.ln();

    let t = if a1 + b1 < MAXGAM && y.abs() < MAXLOG && t_ln.abs() < MAXLOG {
        let mut t = xc.powf(b1);
        t *= x1.powf(a1);
        t /= a1;
        t *= w;
        t * gamma(a1 + b1) / (gamma(a1) * gamma(b1))
    } else {
        let y = y + t_ln + ln_gamma(a1 + b1) - ln_gamma(a1) - ln_gamma(b1) + (w / a1).ln();
        if y < MINLOG {
            0.0
        } else {
            y.exp()
        }
    };

    if flag {
        if t <= MACHEP {
            1.0 - MACHEP
        } else {
            1.0 - t
        }
    } else {
        t
    }
}

/// Power series for incomplete beta
fn pseries(a: f64, b: f64, x: f64) -> f64 {
    let ai = 1.0 / a;
    let u = (1.0 - b) * x;
    let mut v = u / (a + 1.0);
    let t1 = v;
    let mut t = u;
    let mut n = 2;
    let mut s = 0.0;
    let z = MACHEP * ai;

    while v.abs() > z {
        let u = ((n as f64) - b) * x / (n as f64);
        t *= u;
        v = t / (a + n as f64);
        s += v;
        n += 1;
    }
    s += t1;
    s += ai;

    let u = a * x.ln();
    if a + b < MAXGAM && u.abs() < MAXLOG {
        let t = gamma(a + b) / (gamma(a) * gamma(b));
        s * t * x.powf(a)
    } else {
        let t = ln_gamma(a + b) - ln_gamma(a) - ln_gamma(b) + u + s.ln();
        if t < MINLOG {
            0.0
        } else {
            t.exp()
        }
    }
}

/// Continued fraction expansion #1 for incomplete beta
fn cfrac1(a: f64, b: f64, x: f64) -> f64 {
    let mut k1 = a;
    let mut k2 = a + b;
    let mut k3 = a;
    let mut k4 = a + 1.0;
    let mut k5 = 1.0;
    let mut k6 = b - 1.0;
    let mut k7 = k4;
    let mut k8 = a + 2.0;

    let mut pkm2 = 0.0;
    let mut qkm2 = 1.0;
    let mut pkm1 = 1.0;
    let mut qkm1 = 1.0;
    let mut ans = 1.0;
    let mut r = 1.0;
    let thresh = 3.0 * MACHEP;

    for _ in 0..400 {
        let xk = -(x * k1 * k2) / (k3 * k4);
        let pk = pkm1 + pkm2 * xk;
        let qk = qkm1 + qkm2 * xk;
        pkm2 = pkm1;
        pkm1 = pk;
        qkm2 = qkm1;
        qkm1 = qk;

        let xk = (x * k5 * k6) / (k7 * k8);
        let pk = pkm1 + pkm2 * xk;
        let qk = qkm1 + qkm2 * xk;
        pkm2 = pkm1;
        pkm1 = pk;
        qkm2 = qkm1;
        qkm1 = qk;

        if qk != 0.0 {
            r = pk / qk;
        }

        let t = if r != 0.0 {
            let t = ((ans - r) / r).abs();
            ans = r;
            t
        } else {
            1.0
        };

        if t < thresh {
            return ans;
        }

        k1 += 1.0;
        k2 += 1.0;
        k3 += 2.0;
        k4 += 2.0;
        k5 += 1.0;
        k6 -= 1.0;
        k7 += 2.0;
        k8 += 2.0;

        if (qk.abs() + pk.abs()) > BIG {
            pkm2 *= BIGINV;
            pkm1 *= BIGINV;
            qkm2 *= BIGINV;
            qkm1 *= BIGINV;
        }

        if qk.abs() < BIGINV || pk.abs() < BIGINV {
            pkm2 *= BIG;
            pkm1 *= BIG;
            qkm2 *= BIG;
            qkm1 *= BIG;
        }
    }

    ans
}

/// Continued fraction expansion #2 for incomplete beta
fn cfrac2(a: f64, b: f64, x: f64) -> f64 {
    let mut k1 = a;
    let mut k2 = b - 1.0;
    let mut k3 = a;
    let mut k4 = a + 1.0;
    let mut k5 = 1.0;
    let mut k6 = a + b;
    let mut k7 = a + 1.0;
    let mut k8 = a + 2.0;

    let mut pkm2 = 0.0;
    let mut qkm2 = 1.0;
    let mut pkm1 = 1.0;
    let mut qkm1 = 1.0;
    let z = x / (1.0 - x);
    let mut ans = 1.0;
    let mut r = 1.0;
    let thresh = 3.0 * MACHEP;

    for _ in 0..400 {
        let xk = -(z * k1 * k2) / (k3 * k4);
        let pk = pkm1 + pkm2 * xk;
        let qk = qkm1 + qkm2 * xk;
        pkm2 = pkm1;
        pkm1 = pk;
        qkm2 = qkm1;
        qkm1 = qk;

        let xk = (z * k5 * k6) / (k7 * k8);
        let pk = pkm1 + pkm2 * xk;
        let qk = qkm1 + qkm2 * xk;
        pkm2 = pkm1;
        pkm1 = pk;
        qkm2 = qkm1;
        qkm1 = qk;

        if qk != 0.0 {
            r = pk / qk;
        }

        let t = if r != 0.0 {
            let t = ((ans - r) / r).abs();
            ans = r;
            t
        } else {
            1.0
        };

        if t < thresh {
            return ans;
        }

        k1 += 1.0;
        k2 -= 1.0;
        k3 += 2.0;
        k4 += 2.0;
        k5 += 1.0;
        k6 += 1.0;
        k7 += 2.0;
        k8 += 2.0;

        if (qk.abs() + pk.abs()) > BIG {
            pkm2 *= BIGINV;
            pkm1 *= BIGINV;
            qkm2 *= BIGINV;
            qkm1 *= BIGINV;
        }

        if qk.abs() < BIGINV || pk.abs() < BIGINV {
            pkm2 *= BIG;
            pkm1 *= BIG;
            qkm2 *= BIG;
            qkm1 *= BIG;
        }
    }

    ans
}

/// Error function (erf)
pub fn erf(x: f64) -> f64 {
    if x < 0.0 {
        -igamma(0.5, x * x)
    } else {
        igamma(0.5, x * x)
    }
}

/// Complementary error function (erfc)
pub fn erfc(x: f64) -> f64 {
    if x < 0.0 {
        1.0 + igamma(0.5, x * x)
    } else {
        jgamma(0.5, x * x)
    }
}

/// Rational approximation of error function for x > 0 (from Pascal code)
pub fn erfrapos(x: f64) -> f64 {
    if x > 3.5 {
        1.0 - MINUS
    } else {
        let t = 1.0 / (1.0 + P_ERFRA * x);
        1.0 - (A1_ERFRA * t + A2_ERFRA * t * t + A3_ERFRA * t * t * t) * (-x * x).exp()
    }
}

/// Error function using rational approximation
pub fn erfra(x: f64) -> f64 {
    if x > 0.0 {
        erfrapos(x)
    } else {
        -erfrapos(-x)
    }
}

/// Poisson probability mass function P(X = k)
pub fn p_poisson(mu: f64, k: i32) -> f64 {
    if mu <= 0.0 || k < 0 {
        0.0
    } else if k == 0 {
        expo(-mu)
    } else {
        let mut p = mu;
        for i in 2..=k {
            p *= mu / (i as f64);
        }
        expo(-mu) * p
    }
}

/// Poisson cumulative distribution function P(X <= k)
pub fn f_poisson(mu: f64, k: i32) -> f64 {
    if mu <= 0.0 || k < 0 {
        0.0
    } else if k == 0 {
        expo(-mu)
    } else {
        jgamma((k + 1) as f64, mu)
    }
}

/// Standard normal probability density function
pub fn dnorm(x: f64) -> f64 {
    INVSQRT2PI * expo(-0.5 * x * x)
}

/// Standard normal cumulative distribution function
pub fn fnorm(x: f64) -> f64 {
    0.5 * (1.0 + erf(x * SQRT2DIV2))
}

/// Two-tailed standard normal probability P(|U| >= |x|)
pub fn pnorm(x: f64) -> f64 {
    let a = x.abs();
    if a == 0.0 {
        1.0
    } else if a < 1.0 {
        1.0 - erf(a * SQRT2DIV2)
    } else {
        erfc(a * SQRT2DIV2)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_gamma_positive_integers() {
        // Gamma(n) = (n-1)! for positive integers
        assert!((gamma(1.0) - 1.0).abs() < 1e-10);
        assert!((gamma(2.0) - 1.0).abs() < 1e-10);
        assert!((gamma(3.0) - 2.0).abs() < 1e-10);
        assert!((gamma(4.0) - 6.0).abs() < 1e-10);
        assert!((gamma(5.0) - 24.0).abs() < 1e-10);
    }

    #[test]
    fn test_gamma_half() {
        // Gamma(0.5) = sqrt(pi)
        assert!((gamma(0.5) - SQRTPI).abs() < 1e-10);
    }

    #[test]
    fn test_ln_gamma() {
        assert!((ln_gamma(1.0) - 0.0).abs() < 1e-10);
        assert!((ln_gamma(5.0) - 24.0_f64.ln()).abs() < 1e-10);
    }

    #[test]
    fn test_fact() {
        assert_eq!(fact(0), 1.0);
        assert_eq!(fact(1), 1.0);
        assert_eq!(fact(5), 120.0);
        assert_eq!(fact(10), 3628800.0);
    }

    #[test]
    fn test_binomial() {
        assert_eq!(binomial(5, 0), 1.0);
        assert_eq!(binomial(5, 1), 5.0);
        assert_eq!(binomial(5, 2), 10.0);
        assert_eq!(binomial(5, 5), 1.0);
        assert_eq!(binomial(10, 3), 120.0);
    }

    #[test]
    fn test_erf() {
        // erf(0) = 0
        assert!(erf(0.0).abs() < 1e-10);
        // erf is odd function
        assert!((erf(1.0) + erf(-1.0)).abs() < 1e-10);
        // erf approaches 1 for large positive x
        assert!((erf(3.0) - 1.0).abs() < 0.01);
    }

    #[test]
    fn test_igamma_jgamma_complement() {
        // igamma + jgamma should equal 1
        for a in [0.5, 1.0, 2.0, 5.0] {
            for x in [0.1, 1.0, 2.0, 5.0] {
                let sum = igamma(a, x) + jgamma(a, x);
                assert!((sum - 1.0).abs() < 1e-8, "a={}, x={}, sum={}", a, x, sum);
            }
        }
    }
}
