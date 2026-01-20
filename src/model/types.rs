//! Data type definitions
//!
//! Port of type definitions from Unit2.pas

use crate::math::constants::{NBPARMAT, NBPARMORT, NBPARPONTE, NBPARPOSS};

/// Event type enumeration
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EventType {
    /// Sex determination event
    Sex,
    /// Maturity event
    Mat,
    /// Reproduction (ponte) event
    Pon,
    /// Mortality event
    Mor,
    /// Non-reproduction event
    Nop,
}

impl EventType {
    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "sex" => Some(EventType::Sex),
            "mat" => Some(EventType::Mat),
            "pon" => Some(EventType::Pon),
            "mor" => Some(EventType::Mor),
            "nop" => Some(EventType::Nop),
            _ => None,
        }
    }

    pub fn as_str(&self) -> &'static str {
        match self {
            EventType::Sex => "sex",
            EventType::Mat => "mat",
            EventType::Pon => "pon",
            EventType::Mor => "mor",
            EventType::Nop => "nop",
        }
    }
}

/// Distribution type for survival functions
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DistributionType {
    /// Exponential distribution
    Exp,
    /// Weibull distribution
    Wei,
    /// Log-normal distribution
    Lgn,
    /// Gamma distribution
    Gam,
}

impl DistributionType {
    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "exp" => Some(DistributionType::Exp),
            "wei" => Some(DistributionType::Wei),
            "lgn" => Some(DistributionType::Lgn),
            "gam" => Some(DistributionType::Gam),
            _ => None,
        }
    }

    pub fn as_str(&self) -> &'static str {
        match self {
            DistributionType::Exp => "exp",
            DistributionType::Wei => "wei",
            DistributionType::Lgn => "lgn",
            DistributionType::Gam => "gam",
        }
    }
}

/// An event in an individual's life history
#[derive(Debug, Clone)]
pub struct Event {
    /// Type of event
    pub event_type: EventType,
    /// Lower bound of event time interval
    pub t1: f64,
    /// Upper bound of event time interval
    pub t2: f64,
    /// Computed start time
    pub debut: f64,
    /// Computed end time
    pub fin: f64,
    /// Type parameter (clutch size for pon, sex indicator for sex)
    pub tp: i32,
}

impl Event {
    pub fn new(event_type: EventType, t1: f64, t2: f64) -> Self {
        Event {
            event_type,
            t1,
            t2,
            debut: t1,
            fin: t2,
            tp: 0,
        }
    }

    pub fn with_tp(mut self, tp: i32) -> Self {
        self.tp = tp;
        self
    }
}

/// A life history containing a sequence of events
#[derive(Debug, Clone)]
pub struct LifeHistory {
    /// Events in this life history
    pub events: Vec<Event>,
    /// Number of reproduction events
    pub nb_ponte: i32,
}

impl LifeHistory {
    pub fn new() -> Self {
        LifeHistory {
            events: Vec::new(),
            nb_ponte: 0,
        }
    }

    pub fn nb_event(&self) -> usize {
        self.events.len()
    }
}

impl Default for LifeHistory {
    fn default() -> Self {
        Self::new()
    }
}

/// Covariate information
#[derive(Debug, Clone)]
pub struct CovarInfo {
    /// Type: 0 = factor, 1 = continuous
    pub typ: i32,
    /// Number of levels (for factors)
    pub lev: i32,
    /// Covariate name
    pub name: String,
    /// Values for continuous covariate
    pub valcont: Vec<f64>,
}

impl CovarInfo {
    pub fn new(name: String, typ: i32, lev: i32) -> Self {
        CovarInfo {
            typ,
            lev,
            name,
            valcont: Vec::new(),
        }
    }

    pub fn is_factor(&self) -> bool {
        self.typ == 0
    }

    pub fn is_continuous(&self) -> bool {
        self.typ == 1
    }
}

/// Individual information
#[derive(Debug, Clone)]
pub struct Individual {
    /// Life histories for this individual
    pub life_histories: Vec<LifeHistory>,
    /// Covariate values
    pub covariates: Vec<f64>,
}

impl Individual {
    pub fn new() -> Self {
        Individual {
            life_histories: Vec::new(),
            covariates: Vec::new(),
        }
    }

    pub fn nb_hv(&self) -> usize {
        self.life_histories.len()
    }
}

impl Default for Individual {
    fn default() -> Self {
        Self::new()
    }
}

/// Survival function specification
#[derive(Debug, Clone)]
pub struct SurvivalFunction {
    /// Distribution type name
    pub name: DistributionType,
    /// Distribution parameters
    pub vp: Vec<f64>,
}

impl SurvivalFunction {
    pub fn new(name: DistributionType, n_params: usize) -> Self {
        SurvivalFunction {
            name,
            vp: vec![0.0; n_params],
        }
    }

    pub fn exponential() -> Self {
        Self::new(DistributionType::Exp, NBPARMORT)
    }

    pub fn weibull() -> Self {
        Self::new(DistributionType::Wei, NBPARMORT)
    }

    pub fn log_normal() -> Self {
        Self::new(DistributionType::Lgn, NBPARMORT)
    }

    pub fn gamma() -> Self {
        Self::new(DistributionType::Gam, NBPARMORT)
    }
}

/// Model parameter specification
#[derive(Debug, Clone)]
pub struct ModelParam {
    /// Number of terms
    pub nb_terms: usize,
    /// Term indices
    pub term: Vec<i32>,
    /// Term covariate indices (first covariate)
    pub term_cov0: Vec<i32>,
    /// Term covariate indices (second covariate for interactions)
    pub term_cov1: Vec<i32>,
    /// First variable index for each term
    pub first_vi: Vec<usize>,
    /// Term names
    pub name_term: Vec<String>,
}

impl ModelParam {
    pub fn new() -> Self {
        ModelParam {
            nb_terms: 0,
            term: Vec::new(),
            term_cov0: Vec::new(),
            term_cov1: Vec::new(),
            first_vi: Vec::new(),
            name_term: Vec::new(),
        }
    }
}

impl Default for ModelParam {
    fn default() -> Self {
        Self::new()
    }
}

/// Model parameter instance (for a specific group)
#[derive(Debug, Clone)]
pub struct ModelParamInst {
    /// Number of terms
    pub nb_terms: usize,
    /// Pointer to variable indices
    pub po: Vec<usize>,
    /// Parameter values
    pub valpo: Vec<f64>,
}

impl ModelParamInst {
    pub fn new() -> Self {
        ModelParamInst {
            nb_terms: 0,
            po: Vec::new(),
            valpo: Vec::new(),
        }
    }
}

impl Default for ModelParamInst {
    fn default() -> Self {
        Self::new()
    }
}

/// Group information
#[derive(Debug, Clone)]
pub struct Group {
    /// Individuals in this group
    pub individuals: Vec<Individual>,
    /// Whether this group is used
    pub use_flag: bool,
    /// Mortality survival function
    pub mort: SurvivalFunction,
    /// Maturity survival function
    pub mat: SurvivalFunction,
    /// Maturity without ponte (for fitness calculation)
    pub mat_sans_pon: SurvivalFunction,
    /// Reproduction survival function
    pub ponte: SurvivalFunction,
    /// Parameter instances for each parameter type
    pub param: Vec<ModelParamInst>,
}

impl Group {
    pub fn new(
        mort_dist: DistributionType,
        mat_dist: DistributionType,
        pon_dist: DistributionType,
    ) -> Self {
        let mort = SurvivalFunction::new(mort_dist, NBPARMORT);
        let mat = SurvivalFunction::new(mat_dist, NBPARMAT);
        let mat_sans_pon = SurvivalFunction::new(mat_dist, NBPARMAT);
        let ponte = SurvivalFunction::new(pon_dist, NBPARPONTE);

        let mut param = Vec::with_capacity(NBPARPOSS);
        for _ in 0..NBPARPOSS {
            param.push(ModelParamInst::new());
        }

        Group {
            individuals: Vec::new(),
            use_flag: false,
            mort,
            mat,
            mat_sans_pon,
            ponte,
            param,
        }
    }

    pub fn nb_ind(&self) -> usize {
        self.individuals.len()
    }
}

/// Variable information for optimization
#[derive(Debug, Clone)]
pub struct VarInfo {
    /// Minimum bound
    pub min_bound: f64,
    /// Maximum bound
    pub max_bound: f64,
    /// Current value
    pub value: f64,
    /// Best value found
    pub best_value: f64,
    /// Step size for optimization
    pub step: f64,
    /// Standard error
    pub se: f64,
    /// MCMC samples
    pub sample: Vec<f64>,
    /// Variable name
    pub name: String,
    /// Variable type
    pub typ: i32,
    /// Location index
    pub loc: i32,
}

impl VarInfo {
    pub fn new(name: String, min_bound: f64, max_bound: f64) -> Self {
        VarInfo {
            min_bound,
            max_bound,
            value: 0.0,
            best_value: 0.0,
            step: 1.0,
            se: 0.0,
            sample: Vec::new(),
            name,
            typ: 0,
            loc: 0,
        }
    }
}

/// Parameter descriptor
#[derive(Debug, Clone)]
pub struct ParamDescriptor {
    /// Minimum bound
    pub min_bound: f64,
    /// Maximum bound
    pub max_bound: f64,
    /// Current value
    pub value: f64,
    /// Parameter name
    pub name: String,
}

impl ParamDescriptor {
    pub fn new(name: String, min_bound: f64, max_bound: f64) -> Self {
        ParamDescriptor {
            min_bound,
            max_bound,
            value: 0.0,
            name,
        }
    }
}

/// Function descriptor for optimization
#[derive(Debug, Clone)]
pub struct FunctionDescriptor {
    /// Current result (likelihood)
    pub curr_result: f64,
    /// Best result found
    pub best_result: f64,
    /// Mean result (for MCMC)
    pub mean_result: f64,
    /// Variance of result
    pub var_result: f64,
    /// Variable information
    pub var_info: Vec<VarInfo>,
    /// Likelihood samples (for MCMC)
    pub sample: Vec<f64>,
    /// Number of variables
    pub number_of_variables: usize,
    /// Parameter descriptors
    pub param_descript: Vec<ParamDescriptor>,
}

impl FunctionDescriptor {
    pub fn new() -> Self {
        let mut param_descript = Vec::with_capacity(NBPARPOSS);
        for _ in 0..NBPARPOSS {
            param_descript.push(ParamDescriptor::new(String::new(), 0.0, 1.0));
        }

        FunctionDescriptor {
            curr_result: 0.0,
            best_result: f64::NEG_INFINITY,
            mean_result: 0.0,
            var_result: 0.0,
            var_info: Vec::new(),
            sample: Vec::new(),
            number_of_variables: 0,
            param_descript,
        }
    }
}

impl Default for FunctionDescriptor {
    fn default() -> Self {
        Self::new()
    }
}

/// Metropolis algorithm parameters
#[derive(Debug, Clone)]
pub struct MetropolisParams {
    /// Initial temperature
    pub temp0: f64,
    /// Final temperature
    pub tempf: f64,
    /// Step increase factor
    pub bgeup: f64,
    /// Step decrease factor
    pub bgedown: f64,
    /// Target temperature ratio
    pub t_t: f64,
    /// Base temperature ratio
    pub t0: f64,
    /// Cooling/climbing rate
    pub climbrate: f64,
    /// Target acceptance probability
    pub pacc: f64,
    /// Convergence precision
    pub precision: f64,
    /// Number of temperature reductions
    pub ntr: i32,
    /// Number of steps per temperature
    pub nst: i32,
    /// Maximum repetitions
    pub maxrep: i32,
}

impl MetropolisParams {
    pub fn new() -> Self {
        MetropolisParams {
            temp0: 10.0,
            tempf: 0.0,
            bgeup: 1.0 / 0.98,
            bgedown: 0.99,
            t_t: 4.0,
            t0: 2.0,
            climbrate: 1.0,
            pacc: 0.0,
            precision: 0.0001,
            ntr: 100,
            nst: 20,
            maxrep: 40,
        }
    }
}

impl Default for MetropolisParams {
    fn default() -> Self {
        Self::new()
    }
}

/// Global model state
#[derive(Debug, Clone)]
pub struct ModelState {
    /// Model parameters for each parameter type
    pub modele: Vec<ModelParam>,
    /// Number of covariates
    pub nbcov: usize,
    /// Covariate information
    pub covar: Vec<CovarInfo>,
    /// Groups
    pub groups: Vec<Group>,
    /// Number of groups
    pub nb_group: usize,
    /// Whether maturity and first clutch are combined
    pub matclutch: bool,
    /// Critical time for juvenile mortality
    pub tc: f64,
    /// Maximum censoring time
    pub tinf: f64,
    /// Sub-interval for integration
    pub sub_interval: f64,
    /// Ratio max for clutch size senescence
    pub ratiomax: f64,
    /// Intrinsic rate value
    pub r: f64,
    /// Whether fitness reparametrization is used
    pub fitness_repar: bool,
    /// Random seeds
    pub saved_seed: [i32; 4],
}

impl ModelState {
    pub fn new() -> Self {
        let mut modele = Vec::with_capacity(NBPARPOSS);
        for _ in 0..NBPARPOSS {
            modele.push(ModelParam::new());
        }

        ModelState {
            modele,
            nbcov: 0,
            covar: Vec::new(),
            groups: Vec::new(),
            nb_group: 0,
            matclutch: false,
            tc: 0.0,
            tinf: 1000.0,
            sub_interval: 0.1,
            ratiomax: 2.0,
            r: 0.0,
            fitness_repar: false,
            saved_seed: [0; 4],
        }
    }

    /// Get group index from covariate values
    pub fn get_group(&self, cov_values: &[i32]) -> usize {
        match self.nbcov {
            0 => 0,
            1 => cov_values[0] as usize,
            2 => {
                let lev2 = self.covar[1].lev as usize;
                cov_values[0] as usize * lev2 + cov_values[1] as usize
            }
            3 => {
                let lev2 = self.covar[1].lev as usize;
                let lev3 = self.covar[2].lev as usize;
                cov_values[0] as usize * (lev2 * lev3)
                    + cov_values[1] as usize * lev3
                    + cov_values[2] as usize
            }
            _ => 0,
        }
    }

    /// Get covariate value for a group
    pub fn get_cov(&self, group: usize, covnum: usize) -> usize {
        let help = if covnum < self.nbcov + 1 {
            covnum
        } else {
            covnum - self.nbcov
        };

        match self.nbcov {
            0 => 0,
            1 => group,
            2 => {
                let lev2 = self.covar[1].lev as usize;
                match help {
                    1 => group / lev2,
                    2 => group % lev2,
                    _ => 0,
                }
            }
            3 => {
                let lev2 = self.covar[1].lev as usize;
                let lev3 = self.covar[2].lev as usize;
                let a = group / (lev2 * lev3);
                match help {
                    1 => a,
                    2 => (group - a * (lev2 * lev3)) / lev3,
                    3 => (group - a * (lev2 * lev3)) % lev3,
                    _ => 0,
                }
            }
            _ => 0,
        }
    }
}

impl Default for ModelState {
    fn default() -> Self {
        Self::new()
    }
}
