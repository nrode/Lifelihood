### Data Flow

```
R (execute_bin.R)
    │
    ├── Writes: input_data.txt (life history data + model spec)
    ├── Writes: param_bounds.txt (parameter boundaries)
    ├── Writes: continuous_var.txt (optional)
    │
    ▼
Pascal Binary (stdin: space-separated arguments)
    │
    ├── Reads input files
    ├── Parses model configuration
    ├── Runs simulated annealing optimization
    ├── Computes Hessian (optional)
    ├── Runs MCMC sampling (optional)
    │
    ▼
    Writes: input_data.out
    │
    ▼
R (read_output.R, parsers.R)
    │
    └── Parses output file sections
```

### Command Line Arguments (24 positional args via stdin)

```
1.  path_input_data      - Path to data file
2.  path_param_bounds    - Path to parameter bounds file
3.  group_by_group       - Boolean (TRUE/FALSE)
4.  MCMC                 - Number of MCMC samples (0 = disabled)
5.  interval             - Interval between MCMC samples
6.  se.fit               - Compute standard errors (TRUE/FALSE)
7.  saveprobevent        - Save event probabilities (TRUE/FALSE)
8.  fitness              - Fitness reparametrization (TRUE/FALSE)
9.  r                    - Intrinsic rate value
10. seed1-seed4          - Random seeds (4 values)
14. ratiomax             - Max ratio for clutch size senescence
15. tc                   - Critical time for juvenile mortality
16. tinf                 - Maximum censoring time
17. sub_interval         - Sub-interval for integration
18. path_continuous_var  - Path to continuous variables file (or "NULL")
19. ntr                  - Number of temperature reductions
20. nst                  - Number of steps per temperature
21. To                   - Initial temperature
22. Tf                   - Final temperature
23. climbrate            - Cooling rate
24. precision            - Convergence precision
```

## Input/Output File Formats

### Input Data File Format

```
*******data struct****
matclutch true|false
covar1 covar2 ...           # covariate names (or "none")
n1 n2 ...                   # number of levels per covariate
****modele******
wei|exp|gam|lgn wei|exp|gam|lgn wei|exp|gam|lgn   # mortality, maturity, reproduction distributions
0|-1 0|-1 ...               # model terms for each of 20 parameters (20 lines)
*******data*********
<cov1> <cov2> sex <t1> <t2> <sex_val> mat <t1> <t2> [<clutch_size>] [pon <t1> <t2> <size>]... mor <t1> <t2>
...
```

**Model term encoding:**

- `-1` = not fitted
- `0` = intercept only
- `1,2,3` = covariate effect (factor)
- `12,13,23` = interaction between covariates

### Parameter Bounds File Format

```
param_name min_bound max_bound
expt_death 1 200
survival_param2 0.001 30
...
```

### Output File Format

```
---------------------------

datafile= <path>
seed1= X seed2= Y seed3= Z seed4= W
#parameters= N
Likelihood_max= -XXX.XXXXXX
int_param_name X.XXXXXXXX SE.XXXXXXXX
eff_param_name_covar1 X.XXXXXXXX SE.XXXXXXXX
...

inverse of Hessian Matrix [or "(!INVALID)"]
X.XX X.XX X.XX ...
...

MCMCsamples                    # (if MCMC > 0)
LL X.XX X.XX X.XX ...
param1 X.XX X.XX X.XX ...
...

Parameter_Range_Table
expt_death 1.00000000 200.00000000
...
ratiomax X
tinf_(right_censoring) X
tc_(juvenile_period_length) X
```

---

## Rust Project Structure

```
src/
   ├── main.rs               # Entry point, argument parsing
   ├── lib.rs                # Library root
   ├── io/
   │   ├── mod.rs
   │   ├── input.rs          # Data file parsing
   │   ├── config.rs         # Parameter bounds parsing
   │   ├── output.rs         # Output file writing
   │   └── continuous_var.rs # Continuous variables parsing
   ├── model/
   │   ├── mod.rs
   │   ├── types.rs          # Data structures (Group, Individual, Event, etc.)
   │   ├── distributions.rs  # Survival functions (Weibull, Exponential, LogNormal, Gamma)
   │   ├── likelihood.rs     # Likelihood computation
   │   ├── events.rs         # Event probability calculations
   │   └── link.rs           # Link/delink functions
   ├── optim/
   │   ├── mod.rs
   │   ├── metropolis.rs     # Simulated annealing optimizer
   │   ├── mcmc.rs           # MCMC sampling (promenade)
   │   └── hessian.rs        # Hessian computation & inversion
   ├── math/
   │   ├── mod.rs
   │   ├── special.rs        # Gamma, incomplete gamma, beta, erf
   │   ├── integration.rs    # Romberg integration
   │   └── matrix.rs         # Matrix operations (Gauss-Jordan)
   └── rng/
       ├── mod.rs
       └── marsaglia.rs      # Marsaglia PRNG (exact port for reproducibility)
```

## Module Breakdown

### 1. `rng::marsaglia` - Random Number Generator

**Pascal source:** `Alea.pas`

Port the exact Marsaglia algorithm to ensure reproducible results with same seeds.

```rust
pub struct Marsaglia {
    u: [f64; 97],
    c: f64,
    cd: f64,
    cm: f64,
    ip: usize,
    jp: usize,
}

impl Marsaglia {
    pub fn new(seed1: i32, seed2: i32, seed3: i32, seed4: i32) -> Self;
    pub fn next(&mut self) -> f64;  // Returns uniform [0,1)
}
```

### 2. `math::special` - Special Mathematical Functions

**Pascal source:** `fspec.pas`, `fmath.pas`

Use the `statrs` crate for most functions, but verify numerical equivalence:

- `gamma(x)` - Gamma function
- `ln_gamma(x)` - Log gamma
- `igamma(a, x)` - Incomplete gamma (regularized)
- `jgamma(a, x)` - Complement of incomplete gamma
- `erf(x)` - Error function
- `erfc(x)` - Complementary error function

**Alternative:** Port Pascal implementations directly if `statrs` gives different results.

### 3. `math::integration` - Numerical Integration

**Pascal source:** `mathromb.pas`, embedded `romb` in `Unit2.pas`

```rust
pub fn romberg<F>(f: F, lower: f64, upper: f64, tol: f64) -> f64
where
    F: Fn(f64) -> f64;
```

### 4. `math::matrix` - Matrix Operations

**Pascal source:** `adaptGaussJordan` in `Unit2.pas`

```rust
pub fn gauss_jordan_invert(matrix: &mut Vec<Vec<f64>>) -> Result<f64, MatrixError>;
```

### 5. `model::types` - Data Structures

**Pascal source:** `Unit2.pas` type definitions

```rust
pub struct Event {
    pub name: EventType,  // Sex, Mat, Pon, Mor, Nop
    pub t1: f64,
    pub t2: f64,
    pub debut: f64,
    pub fin: f64,
    pub tp: i32,          // clutch size or sex indicator
}

pub struct LifeHistory {
    pub events: Vec<Event>,
    pub nb_ponte: i32,
}

pub struct Individual {
    pub life_histories: Vec<LifeHistory>,
    pub covariates: Vec<f64>,
}

pub struct SurvivalFunction {
    pub name: DistributionType,  // Wei, Exp, Lgn, Gam
    pub vp: Vec<f64>,            // parameters
}

pub struct Group {
    pub individuals: Vec<Individual>,
    pub mortality: SurvivalFunction,
    pub maturity: SurvivalFunction,
    pub reproduction: SurvivalFunction,
    pub param_indices: Vec<Vec<ModelParamInst>>,
}

pub struct FunctionDescriptor {
    pub variables: Vec<VarInfo>,
    pub param_descriptors: Vec<ParamDescriptor>,
    pub best_result: f64,
    pub current_result: f64,
}
```

### 6. `model::distributions` - Survival Functions

**Pascal source:** `surv`, `survp`, `survtotpon` in `Unit2.pas`

```rust
pub fn survival(x: f64, dist: &SurvivalFunction, sex: i32, tinf: f64) -> f64;
pub fn survival_with_juvenile(x: f64, dist: &SurvivalFunction, sex: i32, tc: f64) -> f64;
pub fn survival_with_senescence(...) -> f64;
```

### 7. `model::events` - Event Probabilities

**Pascal source:** `probevent`, `censored_mort`, `censored_mat` in `Unit2.pas`

```rust
pub fn prob_event(event: &Event, mort: &SurvivalFunction, mat: &SurvivalFunction,
                  pon: &SurvivalFunction, integrale: f64, sex: i32,
                  hv: &LifeHistory, params: &GlobalParams) -> f64;
```

### 8. `model::likelihood` - Likelihood Computation

**Pascal source:** `f` function in `Unit2.pas`

```rust
pub fn compute_likelihood(fd: &mut FunctionDescriptor, groups: &mut [Group],
                         params: &GlobalParams) -> f64;
```

### 9. `optim::metropolis` - Simulated Annealing

**Pascal source:** `Metropolise`, `automatic_met` in `Unit2.pas`

```rust
pub struct MetropolisParams {
    pub temp0: f64,
    pub tempf: f64,
    pub ntr: i32,
    pub nst: i32,
    pub climbrate: f64,
    pub precision: f64,
    // ...
}

pub fn optimize(fd: &mut FunctionDescriptor, params: &mut MetropolisParams,
                groups: &mut [Group], rng: &mut Marsaglia, global: &GlobalParams) -> f64;
```

### 10. `optim::mcmc` - MCMC Sampling

**Pascal source:** `promenade` in `Unit2.pas`

```rust
pub fn mcmc_sample(fd: &mut FunctionDescriptor, params: &MetropolisParams,
                   groups: &mut [Group], rng: &mut Marsaglia,
                   n_samples: i32, interval: i32, global: &GlobalParams);
```

### 11. `optim::hessian` - Hessian & Standard Errors

**Pascal source:** `Hessian`, `calcSE` in `Unit2.pas`

```rust
pub fn compute_hessian(fd: &mut FunctionDescriptor, groups: &mut [Group],
                       global: &GlobalParams) -> Vec<Vec<f64>>;
pub fn compute_standard_errors(fd: &mut FunctionDescriptor, hessian: &mut Vec<Vec<f64>>) -> bool;
```

### 12. `io::input` - Input File Parsing

**Pascal source:** `readata` in `Unit1.pas`

```rust
pub fn parse_data_file(path: &str) -> Result<(Vec<Group>, ModelConfig, Vec<Covariate>), ParseError>;
```

### 13. `io::config` - Parameter Bounds Parsing

**Pascal source:** `read_custom` in `Unit1.pas`

```rust
pub fn parse_bounds_file(path: &str) -> Result<Vec<ParamDescriptor>, ParseError>;
```

### 14. `io::output` - Output File Writing

**Pascal source:** `printout_FD`, `writeparamdescript` in `Unit2.pas`

```rust
pub fn write_output(path: &str, fd: &FunctionDescriptor, config: &OutputConfig) -> io::Result<()>;
```

## Rust Environment Setup

### Prerequisites

1. **Install Rust** (via rustup):

   ```bash
   curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
   source $HOME/.cargo/env
   ```

2. **Verify installation**:

   ```bash
   rustc --version  # Should be 1.70+
   cargo --version
   ```

### Development Workflow

```bash
# Build debug version
cargo build

# Build release version
cargo build --release

# Run tests
cargo test

# Format code
cargo fmt

# Lint
cargo clippy
```

## Notes about previous code using Pascal instead of Rust

### Numerical Precision

The Pascal code uses `double` (64-bit float). Rust's `f64` is equivalent. However, pay attention to:

- Order of operations (floating-point is not associative)
- Edge cases near 0 or infinity
- The `minus` constant (`1e-35`) used for numerical stability

### Global State

Pascal uses global variables extensively. In Rust, prefer:

- Passing state explicitly through function parameters
- Using structs to bundle related state
- Consider `Rc<RefCell<T>>` if truly needed (avoid if possible)

### Error Handling

Pascal uses exceptions sparingly. In Rust:

- Use `Result<T, E>` for recoverable errors
- Use `Option<T>` for optional values
- Panic only for truly unrecoverable situations

### Performance

The Rust version should be at least as fast as Pascal. Key optimizations:

- Use `--release` builds
- Profile with `cargo flamegraph`
- Consider SIMD for likelihood loops (future optimization)
