//! Marsaglia random number generator
//!
//! This is an exact port of the Pascal implementation from Alea.pas to ensure
//! reproducible results with the same seeds.
//!
//! Reference: Marsaglia et al, Statistics and Probability Letters 9, 35-39

/// Marsaglia PRNG state
///
/// Implements the Marsaglia random number generator algorithm for producing
/// uniform random numbers in [0, 1).
#[derive(Debug, Clone)]
pub struct Marsaglia {
    u: [f64; 97],
    c: f64,
    cd: f64,
    cm: f64,
    ip: usize,
    jp: usize,
}

impl Marsaglia {
    /// Create a new Marsaglia RNG with the given seeds.
    ///
    /// # Arguments
    ///
    /// * `seed1` - First seed value
    /// * `seed2` - Second seed value
    /// * `seed3` - Third seed value
    /// * `seed4` - Fourth seed value
    ///
    /// # Returns
    ///
    /// A new Marsaglia RNG instance initialized with the given seeds.
    pub fn new(mut iu: i32, mut ju: i32, mut ku: i32, mut lu: i32) -> Self {
        let mut u = [0.0f64; 97];

        // Initialize the u array (indices 0-96, corresponding to Pascal's 1-97)
        for ii in 0..97 {
            let mut su = 0.0f64;
            let mut tu = 0.5f64;

            for _jj in 0..24 {
                let mu = (((iu * ju) % 179) * ku) % 179;
                iu = ju;
                ju = ku;
                ku = mu;
                lu = (53 * lu + 1) % 169;

                if ((lu * mu) % 64) >= 32 {
                    su += tu;
                }
                tu *= 0.5;
            }
            u[ii] = su;
        }

        let c = 362436.0 / 16777216.0;
        let cd = 7654321.0 / 16777216.0;
        let cm = 16777213.0 / 16777216.0;
        let ip = 96; // 97 - 1 in 0-indexed
        let jp = 32; // 33 - 1 in 0-indexed

        Marsaglia {
            u,
            c,
            cd,
            cm,
            ip,
            jp,
        }
    }

    /// Generate the next random number in [0, 1).
    ///
    /// This method advances the RNG state and returns a uniform random number.
    pub fn next(&mut self) -> f64 {
        let mut uni = self.u[self.ip] - self.u[self.jp];
        if uni < 0.0 {
            uni += 1.0;
        }
        self.u[self.ip] = uni;

        // Decrement ip with wrap-around (0-indexed: 96 -> 0 when it would go to -1)
        if self.ip == 0 {
            self.ip = 96;
        } else {
            self.ip -= 1;
        }

        // Decrement jp with wrap-around
        if self.jp == 0 {
            self.jp = 96;
        } else {
            self.jp -= 1;
        }

        self.c -= self.cd;
        if self.c < 0.0 {
            self.c += self.cm;
        }

        uni -= self.c;
        if uni < 0.0 {
            uni += 1.0;
        }

        uni
    }

    /// Generate a standard normal random variate using Box-Muller transform.
    pub fn next_normal(&mut self) -> f64 {
        let u1 = self.next();
        let u2 = self.next();

        // Box-Muller transform
        (-2.0 * u1.ln()).sqrt() * (2.0 * std::f64::consts::PI * u2).cos()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_marsaglia_initialization() {
        let rng = Marsaglia::new(12345, 67890, 11111, 22222);
        // Just ensure it initializes without panic
        assert_eq!(rng.ip, 96);
        assert_eq!(rng.jp, 32);
    }

    #[test]
    fn test_marsaglia_generates_valid_range() {
        let mut rng = Marsaglia::new(1, 2, 3, 4);
        for _ in 0..1000 {
            let val = rng.next();
            assert!(val >= 0.0 && val < 1.0, "Value {} out of range [0, 1)", val);
        }
    }

    #[test]
    fn test_marsaglia_reproducibility() {
        let mut rng1 = Marsaglia::new(12345, 67890, 11111, 22222);
        let mut rng2 = Marsaglia::new(12345, 67890, 11111, 22222);

        for _ in 0..100 {
            assert_eq!(rng1.next(), rng2.next());
        }
    }

    #[test]
    fn test_different_seeds_different_sequences() {
        let mut rng1 = Marsaglia::new(1, 2, 3, 4);
        let mut rng2 = Marsaglia::new(5, 6, 7, 8);

        // Sequences should differ
        let seq1: Vec<f64> = (0..10).map(|_| rng1.next()).collect();
        let seq2: Vec<f64> = (0..10).map(|_| rng2.next()).collect();

        assert_ne!(seq1, seq2);
    }
}
