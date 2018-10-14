#' Fit a linear model
#'
#' @description This function passes parameters to the lm function.
#' @param formula a formula
#' @param data a data.frame
#' @return An lm object
#' @examples
#' fit <- linear_model(Sepal.Length ~., iris)
#' summary(fit)
#' @export
linear_model <- function(formula, data) {
  rownames(data) <- NULL
  x <- model.matrix(formula, data)
  y <- as.matrix(data[, as.character(formula)[2]], ncol = 1)
  y <- y[as.numeric(rownames(x)),, drop = FALSE]
  cl <- match.call()
  
  n <- dim(data)[1]
  p <- dim(data)[2]
  udv <- svd(x)
  if (n < p)
  {
    x.inv <- udv$v[1:n,] %*% diag(1 / udv$d) %*% t(udv$u)
    pseudo.inv <- rbind((x.inv %*% y), as.vector(rep(NA, p - n)))
  }
  else
  {
    x.inv <- udv$v %*% diag(1 / udv$d) %*% t(udv$u)
    pseudo.inv <- x.inv %*% y
  }
  
  rownames(pseudo.inv) <- colnames(x)
  x.pseudo.inv <- list(coefficients = pseudo.inv)
  class(x.pseudo.inv) <- "lm"
  x.pseudo.inv$call <- cl
  x.pseudo.inv
}