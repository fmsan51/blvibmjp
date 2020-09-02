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
#' @param na.rm logical. Passed to `na.rm` of [all()].
#'
#' @examples
#' blvibmjp:::is.wholenumbers(3L)  # TRUE
#' blvibmjp:::is.wholenumbers(3)  # TRUE
#' blvibmjp:::is.wholenumbers(pi)  # FALSE
#'
#' @return A logical value.
is.wholenumbers <- function(number, na.rm = F) {
  all(number %% 1 == 0, na.rm = na.rm)
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


#' Remove NA from a vector
#' @param x A vector
remove_na <- function(x) {
  x[!is.na(x)]
}


#' 97.5% quantile of normal distribution
q975 <- qnorm(0.975)


#' Likelihood function of a coefficient for infection by insects
#'
#' A function used to estimate coefficient for risks_inf_insects in [calc_param()].
#'
#' @param coef,risks_inf_insects,prob_seroconv_insects See the source code of [calc_param()] for detail.
est_coef_inf_insects <- function(coef,
                                 risks_inf_insects, prob_seroconv_insects) {
  prob <- 1 - prod(1 - risks_inf_insects * coef / 10000)
  # / 10000 to prevent estimates become 0 when coef is small
  return(abs(prob_seroconv_insects - prob))
}

