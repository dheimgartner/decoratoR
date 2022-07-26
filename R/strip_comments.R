strip_comments <- function(path)
{
  file <- paste(readLines(path, warn = FALSE), collapse = "\n")
  exprs <- rlang::parse_exprs(file)
  out_list <- purrr::map(exprs, rlang::expr_deparse)
  out_list$sep <- "\n"
  out_file <- do.call(paste, args = out_list)
  file_name <- stringr::str_remove(path, ".R$")
  file_name <- paste0(file_name, "_noc.R")
  writeLines(out_file, con = file_name, sep = "\n")

  invisible(out_file)
}



debugonce(strip_comments)


strip_comments("inst/scripts/advancedR.R")
