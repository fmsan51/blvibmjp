#include <Rcpp.h>
#include <boost/format.hpp>
using namespace Rcpp;

// [[Rcpp::depends(BH)]]
// [[Rcpp::plugins("cpp11")]]

Environment dt_env = Environment::namespace_env("data.table");
Function dt_subset = dt_env["[.data.table"];
Function df_subset("[.data.frame");

//' Increment age column by 1
//'
//' @param cows cow data table
//' @export
// [[Rcpp::export]]
List rcpp_add_1_to_age(List cows) {
  NumericVector age = cows["age"];
  age = age + 1;
  return cows;
}

//' Add the month indicator
//'
//' @param cows cow data table
//' @param i numeric. The index of month.
//' @export
// [[Rcpp::export]]
List rcpp_set_i_month(List cows, double i) {
  NumericVector i_month = cows["i_month"];
  i_month = i_month + 1;
  return cows;
}

//' Extract owned cows
//'
//' @param cows cow data frame
//' @export
// [[Rcpp::export]]
DataFrame rcpp_extract_owned_cows(DataFrame cows) {
  LogicalVector is_owned = cows["is_owned"];
  LogicalVector owned_cows = is_owned | is_na(is_owned);
  cows = df_subset(cows, owned_cows, R_MissingArg);
  return cows;
}

// https://stackoverflow.com/a/12971535
const char kPathSeparator =
#ifdef _WIN32
  '\\';
#else
  '/';
#endif

//' Construct filepath to output a file
//'
//' @param filename character. アウトプットファイル名.
//' @param i numeric. The file name will be like filename01.csv.
//' @param subdir character. A file is saved into data/output/subdir.
//' @param ext character. Extension of the output file.
//' @return character. data/output/subdir/filenameiext. eg. data/output/your_sub_directory/filename01.csv
//' @export
// [[Rcpp::export]]
String rcpp_construct_filepath(std::string filename,
                               Nullable<Rcpp::NumericVector> i = R_NilValue,
                               Nullable<Rcpp::String> subdir = R_NilValue,
                               std::string ext = ".csv") {
  std::string filenameXX = filename;
  if (i.isNotNull()) {
    NumericVector XX(i);
    if (XX.length() != 1) stop("Length of i must be one.");
    filenameXX += (boost::format("%02d") % XX).str();
  }
  filenameXX += ext;

  std::string path = "data";
  path += kPathSeparator;
  path += "output";
  path += kPathSeparator;
  if (subdir.isNull()) {
    path += filenameXX;
  } else {
    std::string c_subdir = as<std::string>(subdir);
    path += c_subdir;
    path += kPathSeparator;
    path += filenameXX;
  }
  return path;
}



