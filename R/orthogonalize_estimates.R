#' Orthogonalize estimates and covariance matrix
#'
#' @param estimates A numeric vector of estimates
#' @param covariance A numeric matrix whose number of rows and columns is equal
#' to the length of \code{estimates}
#'
#' @return a \code{list} containing the orthogonalized estimates and covariance
#' matrix.
#'
#' @export
#'
#' @examples
#' # To be added

orthogonalize_estimates <-
  function(
    estimates,
    covariance
  ){
    k <- length(estimates)

    projection <-
      matrix(data = covariance[k, k], nrow = k, ncol = k) -
      kronecker(
        X = matrix(data = as.vector(c(covariance[1:k-1, k], 0)), nrow = 1),
        Y = matrix(data = 1, nrow = k)
      ) -
      t(
        kronecker(
          Y = matrix(data = 1, nrow = k),
          X = matrix(data = as.vector(c(covariance[1:k-1, k], 0)), nrow = 1)
        )
      ) +
      rbind(cbind(covariance[1:(k-1), 1:(k-1)], 0), 0)

    # A is the Cholesky decomposition of covMatrixProjection
    A <- chol(projection)
    # Atilde is a kx(k-1) matrix with the first k-1 columns of A
    Atilde = A[, -k]
    # Ak is a k x 1 vector equal to the kth column of A
    Ak = A[, k]

    # W equals the vector # (theta_k-theta_1, ..., theta_k-theta_{k-1}, theta_k)
    W = c(estimates[k] - estimates[1:k-1], estimates[k])

    # scale are the orthogonalizing 'coefficients'
    scale <-
      solve(A)%*%(diag(k) - Atilde%*%solve(t(Atilde)%*%Atilde)%*%t(Atilde))%*%Ak

    # updatedEstimate is the updated/orthogonalized estimate at analysis k
    estimate_orthogonal <- t(scale)%*%W

    # updatedVariance is the variance of the
    # updated/orthogonalized estimate at analysis k
    variance_orthogonal <- t(scale)%*%t(A)%*%A%*%scale

    return(
      list(
        estimate_orthogonal = estimate_orthogonal,
        covariance_orthogonal = variance_orthogonal
      )
    )
  }
