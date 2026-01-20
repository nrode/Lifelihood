//! Mathematical constants
//!
//! Port of constants from fmath.pas

/// Natural logarithm of 2
pub const LN2: f64 = 0.693_147_180_559_945_3;

/// Natural logarithm of 10
pub const LN10: f64 = 2.302_585_092_994_046;

/// Natural logarithm of PI
pub const LNPI: f64 = 1.144_729_885_849_400_2;

/// 1 / ln(2)
pub const INVLN2: f64 = 1.442_695_040_888_963_4;

/// 1 / ln(10)
pub const INVLN10: f64 = 0.434_294_481_903_251_83;

/// 2 * PI
pub const TWOPI: f64 = 6.283_185_307_179_586_5;

/// 1 / (2 * PI)
pub const UNSURTWOPI: f64 = 0.159_154_943_091_895_35;

/// PI / 2
pub const PIDIV2: f64 = 1.570_796_326_794_896_6;

/// sqrt(PI)
pub const SQRTPI: f64 = 1.772_453_850_905_516;

/// sqrt(2 * PI)
pub const SQRT2PI: f64 = 2.506_628_274_631_000_5;

/// 1 / sqrt(2 * PI)
pub const INVSQRT2PI: f64 = 0.398_942_280_401_432_7;

/// ln(sqrt(2 * PI))
pub const LNSQRT2PI: f64 = 0.918_938_533_204_672_7;

/// ln(2 * PI) / 2
pub const LN2PIDIV2: f64 = 0.918_938_533_204_672_7;

/// sqrt(2)
pub const SQRT2: f64 = 1.414_213_562_373_095;

/// sqrt(2) / 2
pub const SQRT2DIV2: f64 = 0.707_106_781_186_547_5;

/// Golden ratio (1 + sqrt(5)) / 2
pub const GOLD: f64 = 1.618_033_988_749_895;

/// 2 - GOLD
pub const CGOLD: f64 = 0.381_966_011_250_105_15;

// Machine-dependent constants for double precision (64-bit float)

/// Machine epsilon: 2^(-52)
pub const MACHEP: f64 = 2.220_446_049_250_313e-16;

/// Maximum floating point number: 2^1024
pub const MAXNUM: f64 = 1.797_693_134_862_315_7e308;

/// Minimum floating point number: 2^(-1022)
pub const MINNUM: f64 = 2.225_073_858_507_201_4e-308;

/// Maximum argument for exp()
pub const MAXLOG: f64 = 709.782_712_893_384;

/// Minimum argument for exp()
pub const MINLOG: f64 = -708.396_418_532_264_1;

/// Maximum argument for factorial
pub const MAXFAC: i32 = 170;

/// Maximum argument for gamma
pub const MAXGAM: f64 = 171.624_376_956_302;

/// Maximum argument for ln_gamma
pub const MAXLGM: f64 = 2.556_348e305;

/// Small constant for numerical stability
pub const MINUS: f64 = 1e-35;

/// Constants for error function rational approximation
pub const P_ERFRA: f64 = 0.470_47;
pub const A1_ERFRA: f64 = 0.348_024_2;
pub const A2_ERFRA: f64 = -0.095_879_8;
pub const A3_ERFRA: f64 = 0.747_855_6;

/// Theta = sqrt(2 * PI)
pub const THETA: f64 = 2.506_628_274_631;

/// sqrt(2)
pub const RACINE2: f64 = 1.414_213_562;

// Constants used by IGamma and IBeta
pub const BIG: f64 = 9.223_372_036_854_775_8e18;
pub const BIGINV: f64 = 1.084_202_172_485_504_4e-19;

/// Number of possible parameters
pub const NBPARPOSS: usize = 20;

/// Number of mortality parameters
pub const NBPARMORT: usize = 5;

/// Number of maturity parameters
pub const NBPARMAT: usize = 3;

/// Number of reproduction parameters
pub const NBPARPONTE: usize = 12;

/// Initial values for parameter intercepts (in unconstrained/delinked space)
/// Port of `intinit` array from Unit2.pas:1744-1765
///
/// These values provide reasonable starting points for optimization that avoid
/// extreme regions of parameter space.
///
/// Index mapping:
/// 0: expt_death, 1: survival_param2, 2: ratio_expt_death, 3: prob_death,
/// 4: sex_ratio, 5: expt_maturity, 6: maturity_param2, 7: ratio_expt_maturity,
/// 8: expt_reproduction, 9: reproduction_param2, 10: n_offspring,
/// 11: increase_death_hazard, 12: tof_reduction_date, 13: increase_tof_n_offspring,
/// 14: lin_decrease_hazard, 15: quad_senescence, 16: quad_decrease_hazard,
/// 17: quad_change_n_offspring, 18: tof_n_offspring, 19: fitness
pub const INTINIT: [f64; 20] = [
    -0.37,  // expt_death
    -2.23,  // survival_param2
    0.0,    // ratio_expt_death
    0.0,    // prob_death
    0.0,    // sex_ratio
    -1.96,  // expt_maturity
    -3.45,  // maturity_param2
    0.0,    // ratio_expt_maturity
    -3.036, // expt_reproduction
    -3.35,  // reproduction_param2
    -2.6,   // n_offspring
    0.0,    // increase_death_hazard
    0.0,    // tof_reduction_date
    0.0,    // increase_tof_n_offspring
    0.0,    // lin_decrease_hazard
    0.0,    // quad_senescence
    0.0,    // quad_decrease_hazard
    0.0,    // quad_change_n_offspring
    0.0,    // tof_n_offspring
    -4.0,   // fitness
];
