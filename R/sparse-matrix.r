#' create the class 'sparse.matrix'
#'
#' @description create sparse matrix class using sparse.matrix
#' @param i row indices of elements in a sparse matrix
#' @param j col indices of elements in a sparse matrix
#' @param x value of the sparse matrix corresponding to i, j
#' @param dims dimensions of the sparse matrix
#' @return A sparse.matrix object
#' @export
sparse.matrix <- function(i, j, x, dims) {
  if (missing(dims)) {
    a <- data.frame(i = i, j = j , x = x)
    attr(a, "dims") <- c(max(i), max(j))
  } 
  else {
    a<- data.frame(i = i, j = j, x = x)
    attr(a, "dims") <- dims
  }
  class(a) <- c("sparse.matrix", class(a))
  a
}


#' create the add function
#'
#' @description add operation defined in sparse matrix class
#' @param a a sparse.matrix object
#' @param b a sparse.matrix object
#' @return a sparse.matrix object
#' @export
`+.sparse.matrix` <- function(a, b) {
  if (attr(a, "dims")[1] != attr(b, "dims")[1] || attr(a, "dims")[2] != attr(b, "dims")[2]) {
    stop("should not add these two matrices.")
  }
  c <- merge(a, b, by = c("i", "j"), all = TRUE, suffixes = c("1", "2"))
  c$x1[is.na(c$x1)] <- 0
  c$x2[is.na(c$x2)] <- 0
  c$x <- c$x1 + c$x2
  c <- c[order(c$j),]
  attr(c, "dims") <- attr(a, "dims")
  sparse.matrix(c$i, c$j, c$x, attr(c, "dims"))
}


#' create the transpose function
#'
#' @description transpose operation defined in sparse matrix class
#' @param x a sparse.matrix object
#' @return a sparse.matrix object
#' @export
`t.sparse.matrix` <- function(x) {
  a <- x
  attr(a, "dims") <- attr(x, "dims")
  sparse.matrix(a$j, a$i, a$x, c(attr(a, "dims")[2], attr(a, "dims")[1]))
}


`%*%.default` <- .Primitive("%*%")

`%*%` <- function(a, ...) {
  UseMethod("%*%", a)
}


#' create the multiply function
#'
#' @description multiplication operation defined in sparse matrix class
#' @param a a sparse.matrix object
#' @param b a sparse.matrix object
#' @return a sparse.matrix object
#' @export
`%*%.sparse.matrix` <- function(a, b) {
  if (attr(a, "dims")[2] != attr(b, "dims")[1]) {
    stop("should not multiply these two matrices.")
  }
  
  cc <- merge(a, t(b), by = c("j"), all = TRUE, suffixes = c("1", "2"))
  cc <- na.omit(cc)
  cc$x <- cc$x1 * cc$x2
  cc <- cc[, c("i1", "i2", "x")]
  d <- unique(cc[, -3])
  e <- tapply(cc$x, cc[, -3], sum)
  c <- cbind(d, as.vector(t(e)))
  
  names(c) <- c("i", "j", "x")
  c <- c[order(c$j),]
  attr(c, "dims")[1] <- attr(a, "dims")[1]
  attr(c, "dims")[2] <- attr(b, "dims")[2]
  
  sparse.matrix(c$i, c$j, c$x, attr(c, "dims"))
}