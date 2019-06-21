.onUnload <- function (libpath) {
  library.dynam.unload("blvibmjp", libpath)
}

if (getRversion() >= "3.1.0") {
  utils::globalVariables(
    c("."))
}
