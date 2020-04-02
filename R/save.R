#' Export a cow_table or the list of parameters to a file.
#'
#' @param cows See [cow_table].
#' @param param The parameter list used in the simulation. For more detail, see [calc_param()].
#' @param filename The file name to an output file.
#' @param i The number of months from the start of the simulation.
#' @param subdir Specify the path to the directory from "output" folder if you want to output a file into a different directory.
#' @param ext The file extension of an output file.
#'
#' @rdname save_file
#' @return An output file at the specified location (`save_to_csv()` and `save_param_txt()`) or a character string indicates a file path (eg. "subdir/filename001.csv") (`construct_filepath()`).
save_to_csv <- function(cows, filename, i, subdir = ".") {
  file <- construct_filepath(filename, i, subdir)
  fwrite(cows, file)
}
# TODO: ここあとでテストかく


#' @rdname save_file
save_param_txt <- function(param, filename, i, subdir = ".") {
  sink(file = construct_filepath(filename, i, subdir, ".txt"))
  str(param, vec.len = Inf, give.attr = F, give.head = F,
      indent.str = "", comp.str = "", no.list = T)
  sink(file = NULL)
  invisible(NULL)
}


#' @rdname save_file
construct_filepath <- function(filename, i = NULL, subdir = ".", ext = ".csv") {
  if (is.null(i)) {
    filenameXX <- filename
  } else {
    filenameXX <- paste0(filename, formatC(i, width = 2, flag = "0"))
    #TODO: i を 4桁に
  }
  path <- paste0(gsub("[\\/]*$", "/", subdir), filenameXX, ext)

  return(path)
}
# TODO: test

