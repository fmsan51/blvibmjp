#' Make a named list
#'
#' Construct a named list whose elements' names are same with the inputs.
#'
#' @param ... Inputs.
#'
#' @return A list consisted of inputs and having same names with inputs.
#' @examples
#' a <- "a"
#' b <- "B"
#' c <- 3
#' \dontrun{
#' vars_to_named_list(a, b, c)  # list(a = "a", b = "B", c = 3)
#' }
vars_to_named_list <- function(...) {
  vars <- quos_auto_name(enquos(...))
  list <- eval_tidy(expr(list(!!!vars)))
  return(list)
}
# TODO: いらんかも
# TODO: スピード的にボトルネックになりそう


#' Update a list if an input is not `NULL`
#'
#' Overwrite components of a list if inputs are not `NULL`.
#'
#' @param list A list to be updated.
#' @param ... Inputs to update.
#'
#' @return A list whose components which has same names with non-`NULL` inputs are overwritten.
#'
#' @examples
#' a <- "A"
#' b <- NULL
#' l <- list(a = "a", b = "B", c = 3)
#' \dontrun{
#' update_if_input_not_null(l, a, b)  # list(a = "A", b = "B", c = 3)
#' }
update_if_input_not_null <- function(list, ...) {
  input_list <- as.list(...)
  input_name <- as.character(ensyms(...))
  for (i in input_name) {
    if (!is.null(input_list[[i]])) {
      list[[i]] <- input_list[[i]]
    }
  }
}
# TODO:いらんかも

