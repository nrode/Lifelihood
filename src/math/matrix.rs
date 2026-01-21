//! Matrix operations
//!
//! Port of Gauss-Jordan matrix inversion from Unit2.pas

use thiserror::Error;

#[derive(Error, Debug)]
pub enum MatrixError {
    #[error("Matrix is singular")]
    Singular,
    #[error("Matrix is not square")]
    NotSquare,
    #[error("Matrix dimension mismatch")]
    DimensionMismatch,
}

/// Gauss-Jordan matrix inversion with pivoting
///
/// Inverts the matrix in-place and returns the determinant.
///
/// # Arguments
///
/// * `matrix` - The matrix to invert (modified in-place)
///
/// # Returns
///
/// The determinant of the original matrix, or an error if singular.
pub fn gauss_jordan_invert(matrix: &mut Vec<Vec<f64>>) -> Result<f64, MatrixError> {
    let n = matrix.len();
    if n == 0 {
        return Ok(1.0);
    }

    // Check square
    for row in matrix.iter() {
        if row.len() != n {
            return Err(MatrixError::NotSquare);
        }
    }

    // Create augmented matrix [A | I]
    let mut aug = vec![vec![0.0; 2 * n]; n];
    for i in 0..n {
        for j in 0..n {
            aug[i][j] = matrix[i][j];
        }
        aug[i][n + i] = 1.0;
    }

    let mut det = 1.0;

    // Forward elimination with partial pivoting
    for col in 0..n {
        // Find pivot
        let mut max_row = col;
        let mut max_val = aug[col][col].abs();
        for row in (col + 1)..n {
            if aug[row][col].abs() > max_val {
                max_val = aug[row][col].abs();
                max_row = row;
            }
        }

        // Check for singularity
        if max_val < 1e-15 {
            return Err(MatrixError::Singular);
        }

        // Swap rows if necessary
        if max_row != col {
            aug.swap(col, max_row);
            det = -det;
        }

        // Scale pivot row
        let pivot = aug[col][col];
        det *= pivot;
        for j in 0..(2 * n) {
            aug[col][j] /= pivot;
        }

        // Eliminate column
        for row in 0..n {
            if row != col {
                let factor = aug[row][col];
                for j in 0..(2 * n) {
                    aug[row][j] -= factor * aug[col][j];
                }
            }
        }
    }

    // Copy inverse back to matrix
    for i in 0..n {
        for j in 0..n {
            matrix[i][j] = aug[i][n + j];
        }
    }

    Ok(det)
}

/// Multiply two matrices
pub fn matrix_multiply(a: &[Vec<f64>], b: &[Vec<f64>]) -> Result<Vec<Vec<f64>>, MatrixError> {
    if a.is_empty() || b.is_empty() {
        return Ok(vec![]);
    }

    let m = a.len();
    let n = a[0].len();
    let p = b[0].len();

    if n != b.len() {
        return Err(MatrixError::DimensionMismatch);
    }

    let mut result = vec![vec![0.0; p]; m];
    for i in 0..m {
        for j in 0..p {
            for k in 0..n {
                result[i][j] += a[i][k] * b[k][j];
            }
        }
    }

    Ok(result)
}

/// Compute matrix transpose
pub fn transpose(matrix: &[Vec<f64>]) -> Vec<Vec<f64>> {
    if matrix.is_empty() {
        return vec![];
    }

    let m = matrix.len();
    let n = matrix[0].len();
    let mut result = vec![vec![0.0; m]; n];

    for i in 0..m {
        for j in 0..n {
            result[j][i] = matrix[i][j];
        }
    }

    result
}

/// Compute dot product of two vectors
pub fn dot_product(a: &[f64], b: &[f64]) -> f64 {
    a.iter().zip(b.iter()).map(|(x, y)| x * y).sum()
}

/// Create identity matrix
pub fn identity(n: usize) -> Vec<Vec<f64>> {
    let mut result = vec![vec![0.0; n]; n];
    for i in 0..n {
        result[i][i] = 1.0;
    }
    result
}

/// Extract diagonal elements
pub fn diagonal(matrix: &[Vec<f64>]) -> Vec<f64> {
    let n = matrix.len().min(matrix.get(0).map_or(0, |r| r.len()));
    (0..n).map(|i| matrix[i][i]).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_gauss_jordan_2x2() {
        let mut matrix = vec![vec![4.0, 7.0], vec![2.0, 6.0]];

        let det = gauss_jordan_invert(&mut matrix).unwrap();

        // Check determinant (4*6 - 7*2 = 10)
        assert!((det - 10.0).abs() < 1e-10);

        // Check inverse (A^-1 = 1/10 * [[6, -7], [-2, 4]])
        assert!((matrix[0][0] - 0.6).abs() < 1e-10);
        assert!((matrix[0][1] - -0.7).abs() < 1e-10);
        assert!((matrix[1][0] - -0.2).abs() < 1e-10);
        assert!((matrix[1][1] - 0.4).abs() < 1e-10);
    }

    #[test]
    fn test_gauss_jordan_identity() {
        let mut matrix = vec![vec![1.0, 0.0], vec![0.0, 1.0]];

        let det = gauss_jordan_invert(&mut matrix).unwrap();

        assert!((det - 1.0).abs() < 1e-10);
        assert!((matrix[0][0] - 1.0).abs() < 1e-10);
        assert!((matrix[1][1] - 1.0).abs() < 1e-10);
    }

    #[test]
    fn test_gauss_jordan_singular() {
        let mut matrix = vec![vec![1.0, 2.0], vec![2.0, 4.0]];

        let result = gauss_jordan_invert(&mut matrix);
        assert!(result.is_err());
    }

    #[test]
    fn test_matrix_multiply() {
        let a = vec![vec![1.0, 2.0], vec![3.0, 4.0]];
        let b = vec![vec![5.0, 6.0], vec![7.0, 8.0]];

        let c = matrix_multiply(&a, &b).unwrap();

        // [[1*5+2*7, 1*6+2*8], [3*5+4*7, 3*6+4*8]] = [[19, 22], [43, 50]]
        assert_eq!(c[0][0], 19.0);
        assert_eq!(c[0][1], 22.0);
        assert_eq!(c[1][0], 43.0);
        assert_eq!(c[1][1], 50.0);
    }

    #[test]
    fn test_transpose() {
        let matrix = vec![vec![1.0, 2.0, 3.0], vec![4.0, 5.0, 6.0]];

        let t = transpose(&matrix);

        assert_eq!(t.len(), 3);
        assert_eq!(t[0].len(), 2);
        assert_eq!(t[0][0], 1.0);
        assert_eq!(t[0][1], 4.0);
        assert_eq!(t[2][1], 6.0);
    }
}
