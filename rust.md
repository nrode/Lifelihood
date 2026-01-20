# Pascal to Rust Migration Plan for Lifelihood

This document outlines the plan to rewrite the Pascal computational core of the `lifelihood` R package in Rust, while maintaining the existing file-based interface between R and the compiled binary.

## Table of Contents

1. [Overview](#overview)
2. [Current Architecture](#current-architecture)
3. [Input/Output File Formats](#inputoutput-file-formats)
4. [Rust Project Structure](#rust-project-structure)
5. [Module Breakdown](#module-breakdown)
6. [Implementation Plan](#implementation-plan)
7. [Build System Changes](#build-system-changes)
8. [Testing Strategy](#testing-strategy)
9. [Rust Environment Setup](#rust-environment-setup)

---

## Overview

### Goals

- Replace Pascal source code (`source/*.pas`, `source/*.lpr`) with equivalent Rust code
- Maintain identical file-based I/O interface (R writes text files → binary reads/processes → binary writes output files)
- Produce cross-platform binaries: `lifelihood-linux`, `lifelihood-macos`, `lifelihood-windows.exe`
- Preserve all existing functionality: likelihood computation, simulated annealing optimization, MCMC sampling, Hessian computation

### Non-Goals

- Changing the R-binary interface (keep using `system()` calls with stdin arguments)
- Adding new features during migration
- Using Rust-R bindings (extendr) - keeping file-based approach

---

## Current Architecture

### Pascal Source Files

| File             | Purpose                                                                                                                       | Lines (approx) |
| ---------------- | ----------------------------------------------------------------------------------------------------------------------------- | -------------- |
| `lifelihood.lpr` | Main entry point, argument parsing, orchestration                                                                             | 120            |
| `Unit1.pas`      | Data reading (`readata`), config parsing (`read_custom`)                                                                      | 320            |
| `Unit2.pas`      | Core algorithms: likelihood (`f`), optimization (`Metropolise`, `automatic_met`), MCMC (`promenade`), Hessian, output writing | 2300           |
| `Alea.pas`       | Marsaglia random number generator                                                                                             | 70             |
| `fmath.pas`      | Math utilities: exp, log, power, complex numbers                                                                              | ~800           |
| `fspec.pas`      | Special functions: Gamma, incomplete Gamma, Beta, error function, distributions                                               | ~600           |
| `mathromb.pas`   | Romberg numerical integration                                                                                                 | 160            |

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

---

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
source/
├── rust/                          # New Rust project root
│   ├── Cargo.toml
│   ├── src/
│   │   ├── main.rs               # Entry point, argument parsing
│   │   ├── lib.rs                # Library root
│   │   ├── io/
│   │   │   ├── mod.rs
│   │   │   ├── input.rs          # Data file parsing
│   │   │   ├── config.rs         # Parameter bounds parsing
│   │   │   ├── output.rs         # Output file writing
│   │   │   └── continuous_var.rs # Continuous variables parsing
│   │   ├── model/
│   │   │   ├── mod.rs
│   │   │   ├── types.rs          # Data structures (Group, Individual, Event, etc.)
│   │   │   ├── distributions.rs  # Survival functions (Weibull, Exponential, LogNormal, Gamma)
│   │   │   ├── likelihood.rs     # Likelihood computation
│   │   │   ├── events.rs         # Event probability calculations
│   │   │   └── link.rs           # Link/delink functions
│   │   ├── optim/
│   │   │   ├── mod.rs
│   │   │   ├── metropolis.rs     # Simulated annealing optimizer
│   │   │   ├── mcmc.rs           # MCMC sampling (promenade)
│   │   │   └── hessian.rs        # Hessian computation & inversion
│   │   ├── math/
│   │   │   ├── mod.rs
│   │   │   ├── special.rs        # Gamma, incomplete gamma, beta, erf
│   │   │   ├── integration.rs    # Romberg integration
│   │   │   └── matrix.rs         # Matrix operations (Gauss-Jordan)
│   │   └── rng/
│   │       ├── mod.rs
│   │       └── marsaglia.rs      # Marsaglia PRNG (exact port for reproducibility)
│   └── tests/
│       ├── integration_tests.rs
│       └── fixtures/             # Test input/output files
├── *.pas                         # Old Pascal files (to be removed after migration)
└── rust.md                       # This file
```

---

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

---

## Implementation Plan

### Phase 1: Foundation (Week 1-2)

1. **Set up Rust project structure**
   - Initialize Cargo project
   - Define module structure
   - Add dependencies (`statrs`, `thiserror`, etc.)

2. **Port RNG (`rng::marsaglia`)**
   - Exact port of Marsaglia algorithm
   - Unit tests comparing Pascal output with Rust output

3. **Port special functions (`math::special`)**
   - Use `statrs` where possible
   - Port Pascal implementations where needed for numerical consistency
   - Extensive unit tests

### Phase 2: Data Structures & I/O (Week 3-4)

4. **Define data types (`model::types`)**
   - Port all Pascal record types to Rust structs
   - Implement `Default`, `Clone`, `Debug` traits

5. **Implement input parsing (`io::input`, `io::config`)**
   - Parse data file with life history records
   - Parse parameter bounds file
   - Handle continuous variables file

6. **Implement output writing (`io::output`)**
   - Match Pascal output format exactly
   - Include all sections: seeds, likelihood, effects, Hessian, MCMC, ranges

### Phase 3: Core Algorithms (Week 5-7)

7. **Port survival functions (`model::distributions`)**
   - Weibull, Exponential, LogNormal, Gamma
   - Include male ratio, juvenile mortality, senescence effects

8. **Port event probability (`model::events`)**
   - `probevent` for each event type
   - Truncated Poisson for clutch sizes

9. **Port likelihood function (`model::likelihood`)**
   - Main `f` function
   - Link/delink transformations
   - Group/individual/event loops

10. **Port numerical integration (`math::integration`)**
    - Romberg integration for fitness computation

### Phase 4: Optimization (Week 8-9)

11. **Port Metropolis algorithm (`optim::metropolis`)**
    - Simulated annealing with adaptive cooling
    - Local search refinement

12. **Port MCMC sampling (`optim::mcmc`)**
    - `promenade` function
    - Sample storage

13. **Port Hessian computation (`optim::hessian`)**
    - Numerical differentiation
    - Gauss-Jordan matrix inversion

### Phase 5: Integration & Testing (Week 10-11)

14. **Implement main entry point**
    - Argument parsing (24 positional args from stdin)
    - Orchestration of full workflow

15. **Integration testing**
    - Run Rust binary with same inputs as Pascal
    - Compare outputs numerically
    - Regression tests with real datasets

### Phase 6: Build System & Deployment (Week 12)

16. **Update Makefile**
    - Cross-compilation targets
    - Update binary names and paths

17. **Documentation**
    - Update CLAUDE.md
    - README updates

---

## Build System Changes

### New Makefile Targets

```makefile
RUST_DIR := $(SRC_DIR)/rust

.PHONY: rust-macos rust-linux rust-windows

rust-macos:
	cd $(RUST_DIR) && cargo build --release
	cp $(RUST_DIR)/target/release/lifelihood $(BUILD_DIR)/lifelihood-macos

rust-linux:
	cd $(RUST_DIR) && cross build --release --target x86_64-unknown-linux-gnu
	cp $(RUST_DIR)/target/x86_64-unknown-linux-gnu/release/lifelihood $(BUILD_DIR)/lifelihood-linux

rust-windows:
	cd $(RUST_DIR) && cross build --release --target x86_64-pc-windows-gnu
	cp $(RUST_DIR)/target/x86_64-pc-windows-gnu/release/lifelihood.exe $(BUILD_DIR)/lifelihood-windows.exe
```

### Dependencies

- `cross` for cross-compilation: `cargo install cross`
- Docker for Linux builds (already used)

---

## Testing Strategy

### Unit Tests

Each module should have unit tests comparing Rust output with known Pascal output:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_marsaglia_sequence() {
        let mut rng = Marsaglia::new(12345, 67890, 11111, 22222);
        // Compare with Pascal output for same seeds
        assert!((rng.next() - 0.123456789).abs() < 1e-10);
    }

    #[test]
    fn test_weibull_survival() {
        // Known values from Pascal
    }
}
```

### Integration Tests

1. **Golden file tests**: Run both Pascal and Rust binaries with identical inputs, compare outputs
2. **Numerical tolerance**: Allow small floating-point differences (< 1e-8 relative)
3. **Real dataset tests**: Use actual `lifelihood` datasets from tests/testthat/

### R Package Tests

After migration, all existing R tests should pass:

```r
devtools::test()  # Should show PASS 94
```

---

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

3. **Install cross-compilation tools**:

   ```bash
   cargo install cross
   ```

4. **Install additional targets** (optional, cross handles this):
   ```bash
   rustup target add x86_64-unknown-linux-gnu
   rustup target add x86_64-pc-windows-gnu
   ```

### Project Initialization

```bash
cd source
cargo new rust --name lifelihood
cd rust
```

### Recommended Dependencies (Cargo.toml)

```toml
[package]
name = "lifelihood"
version = "0.1.0"
edition = "2021"

[dependencies]
statrs = "0.16"          # Statistical functions
thiserror = "1.0"        # Error handling
anyhow = "1.0"           # Error propagation

[profile.release]
lto = true               # Link-time optimization
codegen-units = 1        # Better optimization
panic = "abort"          # Smaller binary
```

### Development Workflow

```bash
# Build debug version
cargo build

# Build release version
cargo build --release

# Run tests
cargo test

# Run with arguments (simulating R call)
echo "path/to/data.txt path/to/bounds.txt FALSE 0 1 TRUE FALSE FALSE 0 1234 5678 9012 3456 2 0 1000 0.1 NULL 100 20 10 0 1 0.0001" | cargo run --release

# Format code
cargo fmt

# Lint
cargo clippy
```

### Cross-Compilation

```bash
# Linux (from macOS)
cross build --release --target x86_64-unknown-linux-gnu

# Windows (from macOS/Linux)
cross build --release --target x86_64-pc-windows-gnu
```

---

## Migration Checklist

- [ ] Phase 1: Foundation
  - [ ] Cargo project setup
  - [ ] Marsaglia RNG port
  - [ ] Special functions port
  - [ ] Unit tests passing

- [ ] Phase 2: Data & I/O
  - [ ] Type definitions
  - [ ] Input file parser
  - [ ] Config file parser
  - [ ] Output file writer
  - [ ] Unit tests passing

- [ ] Phase 3: Core Algorithms
  - [ ] Survival functions
  - [ ] Event probabilities
  - [ ] Likelihood computation
  - [ ] Romberg integration
  - [ ] Unit tests passing

- [ ] Phase 4: Optimization
  - [ ] Metropolis algorithm
  - [ ] MCMC sampling
  - [ ] Hessian computation
  - [ ] Unit tests passing

- [ ] Phase 5: Integration
  - [ ] Main entry point
  - [ ] Integration tests passing
  - [ ] R package tests passing

- [ ] Phase 6: Deployment
  - [ ] Makefile updated
  - [ ] Cross-compilation working
  - [ ] Documentation updated
  - [ ] Old Pascal files removed

---

## Notes

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
