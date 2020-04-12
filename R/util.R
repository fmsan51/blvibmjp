#' Convert non-integer to integer
#'
#' Convert non-integer to integer. \cr
#' The output will be an integer which is obtained by [ceiling()] or [floor()] and the mean of the output of multiple trials equals to the input.
#' For example, if input is 1.7, the output will be 1 by 30% or 2 by 70%.
#'
#' @param number numeric vector.
#'
#' @examples
#' set.seed(1)
#' res <- blvibmjp:::integerize(rep(pi, 100))
#' table(res)
#'
#' @return numerc vector of the same length with the input.
integerize <- function(number) {
  floor(number) + (runif(length(number)) < number %% 1)
}


#' Test if a numeric vector is consisted of whole numbers or not
#'
#' Test if all the components of a numeric vector is whole number or not.
#'
#' @param number numeric vector.
#'
#' @examples
#' blvibmjp:::is.wholenumber(3L)  # TRUE
#' blvibmjp:::is.wholenumber(3)  # TRUE
#' blvibmjp:::is.wholenumber(pi)  # FALSE
#'
#' @return A logical value.
is.wholenumber <- function(number) {
  all(number %% 1 == 0)
}


#' sample() from x rather than 1:x
#'
#' Modified version of [sample()] to sample from `x` rather than `1:x` when `x` has length 1 and is numeric.
#'
#' See "sample()'s surprise" and "safer version" section in the example section in the help of [sample()] to understand why this function is necessary.
#'
#' @param x,... See [sample()].
#'
#' @examples
#' sample(9)  # length 9
#' blvibmjp:::resample(9)  # length 1
resample <- function(x, ...) {
  x[sample.int(length(x), ...)]
}


#' Days per month (365 / 12)
days_per_month <- (365 / 12)

