
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
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  y <- model.response(mf, "numeric")
  x <- model.matrix(formula, data)
  udv <- svd(x)
  x.inv <- udv$v %*% diag(1 / udv$d) %*% t(udv$u)
  pseudo.inv <- x.inv %*% y
  attr(pseudo.inv, "names") <- colnames(x)
  x.pseudo.inv <- list(coefficients = pseudo.inv)
  x.pseudo.inv
}
