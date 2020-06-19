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
  invisible(NULL)
}


#' @rdname save_file
save_param_txt <- function(param, filename, i, subdir = ".") {
  sink(file = construct_filepath(filename, i, subdir, ".txt"))
  str(param, vec.len = Inf, give.attr = F,
      indent.str = "", comp.str = "", no.list = T)
  sink(file = NULL)
  invisible(NULL)
}


#' @rdname save_file
construct_filepath <- function(filename, i = NULL, subdir = ".", ext = ".csv") {
  if (is.null(i)) {
    filenameXXX <- filename
  } else {
    filenameXXX <- paste0(filename, formatC(i, width = 3, flag = "0"))
  }
  path <- paste0(gsub("[\\/]*$", "/", subdir), filenameXXX, ext)

  return(path)
}


#' Read parameters from a txt file
#'
#' Read parameters from a txt file which contains parameter information (param_simulationXXX.txt) and return a list consisted of the parameter values in the file.
#'
#' @param path A path of a txt file which contains parameter information.
#' @export
read_param <- function(path) {
  par <- readLines(path)

  var_val <- str_split(par, " *:( |(?=Classes |List of|'))", simplify = T)
  var <- sub("^ *\\.\\.\\$ *", "", var_val[, 1])
  depth2 <- var_val[, 1] != var

  type_val <- str_split(var_val[, 2], "(?<=logi|int|num|chr) ", simplify = T)
  type_wo0 <- sub("\\(0\\)", "", type_val[, 1])
  is_0len <- type_val[, 1] != type_wo0
  type <- sub("( of |:\\t).+$", "", type_wo0)

  len <- as.integer(
    str_extract(type_val[, 1], "(\\d+(?= variables)|(?<=List of )\\d+)")
    )
  is_list <- !is.na(len)

  raw_val <- sub("^\\[.+?\\] ", "", type_val[, 2])
  val_num_lgl <- strsplit(raw_val, " ")
  val <- strsplit(str_extract(raw_val, '(?<=^").+(?="$)'), '" "')

  val[is.na(val)] <- val_num_lgl[is.na(val)]
  names(val) <- var

  # Convert type
  for (i in seq_along(val)) {
    if (is_0len[i]) {
      val[[i]] <- logical(0)
    }

    i_type <- type[i]
    if (i_type == "logi") {
      val[[i]] <- as.logical(val[[i]])
    } else if (i_type == "int") {
      val[[i]] <- as.integer(val[[i]])
    } else if (i_type == "num") {
      val[[i]] <- as.numeric(val[[i]])
    }
  }

  # Make list/data.frame/data.table
  for (i in seq_along(val)) {
    if (!is_list[i]) next()

    i_list <- val[seq_len(len[i]) + i]
    if (type[i] == "List") {
      val[[i]] <- i_list
    } else if (type[i] == "'data.frame'") {
      val[[i]] <- as.data.frame(i_list)
    } else {
      val[[i]] <- as.data.table(i_list)
    }
  }

  res <- val[!depth2]
  return(res)
}

