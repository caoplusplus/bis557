#' Fit a ridge regression model
#'
#' @description This function passes parameters to the ridge regression.
#' @param formula a formula
#' @param lambda a ridge regression parameter
#' @param data a data.frame
#' @return A ridge regression object
#' @examples
#' fit_ridge <- ridge_reg(Sepal.Length ~. -1, 2.5, iris)
#' summary(fit_ridge)
#' @export
ridge_reg <- function(formula, lambda, data) {
  rownames(data) <- NULL
  x <- model.matrix(formula, data)
  y <- as.matrix(data[, as.character(formula)[2]], ncol = 1)
  y <- y[as.numeric(rownames(x)),, drop = FALSE]
  
  svd_obj <- svd(x)
  U <- svd_obj$u
  V <- svd_obj$v
  svals <- svd_obj$d
  D <- diag(svals / (svals^2 + lambda))
  beta <- V %*% D %*% t(U) %*% y
  
  rownames(beta) <- colnames(x)
  ret <- list(coefficients = beta, lambda = lambda)
  class(ret) <- "ridge reg"
  ret
}