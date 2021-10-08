# From jdtools R package
# https://github.com/jdtrat/jdtools/blob/bd7e7baf7522b8c225b1beb41fea65cad85855fd/R/func_strings-extract.R#L72
#
extract_after <- function(full_string, after_string, trim_spaces = TRUE) {

  reg_pattern <- paste0("(?<=", after_string, ").*")

  out <- regmatches(full_string, gregexpr(pattern = reg_pattern,
                                          text = full_string,
                                          perl = T))[[1]]

  if (trim_spaces) {
    out <- trimws(out)
  }

  return(out)
}
