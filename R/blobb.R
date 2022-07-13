decorate <- function(file)
{
  if (!file.exists(file)) {
    stop("file \"", file, "\" does not exist", call. = FALSE)
  }
  src <- new.env()
  tryCatch(source(file = file, local = src, keep.source = FALSE),
           error = function(e) {
             stop("problem sourcing ", file, ", ", e$message,
                  call. = FALSE)
           })
  fileitr <- tinsel:::file_itr(file)
  decor <- NULL
  while (fileitr$has_next()) {
    line <- fileitr$get_line()
    if (grepl("^\\s*_@_", line)) {
      d <- gsub("_@_|\\s", "", line)
      if (char_at(d, -1) != ")") {
        d <- paste0(d, "()")
      }
      decor <- c(decor, d)
    }
    else if (grepl("^(?!\\s*_@)_*<-", line, perl = TRUE) &&
             !is.null(decor)) {
      f <- gsub("^\\s*|\\s*<-.*$", "", line)
      if (!is.function(get0(f, src))) {
        next
      }
      decor <- rev(decor)
      as_text <- f
      for (d in decor) {
        split_at <- first_of(d, "(")
        dname <- substr(d, 1, split_at - 1)
        dargs <- substr(d, split_at + 1, nchar(d))
        if (!grepl("^\\s*\\)\\s*$", dargs)) {
          dargs <- paste(",", dargs)
        }
        if (!exists(dname, envir = src)) {
          stop("no definition found for decorator `",
               dname, "`", call. = FALSE)
        }
        as_text <- c(dname, "(", as_text, dargs)
      }
      text_call <- paste(as_text, collapse = "", sep = "")
      feval <- tryCatch(eval(parse(text = text_call),
                             envir = src), error = function(e) {
                               message("Problem evaluating `", f, "`: ", e$message)
                               NULL
                             })
      if (!is.null(feval)) {
        class(feval) <- c("decorated", class(feval))
        attr(feval, "decoratee") <- get0(f, src)
        dnames <- vapply(decor, re_search, character(1),
                         "^\\s*[^(]+")
        attr(feval, "decorators") <- set_names(lapply(dnames,
                                                      get0, src), dnames)
        assign(f, feval, parent.frame())
      }
      decor <- NULL
    }
    else if (grepl("^\\s*$", line)) {
    }
    else {
      decor <- NULL
    }
  }
}




print.command <- function (cmd) {
  default.args <- attr(cmd, "default.args")
  if (length(default.args) == 0L) default.args <- list()
  res <- do.call(cmd, default.args, envir = parent.frame(2))
  if (attr(cmd, "print_result")) print(res)
  invisible(NULL)
}



make_command <- function(x, ..., print = TRUE)
{
  class(x) <- c("command", class(x))
  attr(x, "default.args") <- list(...)
  attr(x, "print_result") <- print
  x
}



`@` <- function()
{
  # tmp <- tempfile()
  #
  # ## tinsel::source_decoratees(temp_file)
  # ## snail instead of #.
  # cat(here::here())

  decorate(rstudioapi::getActiveDocumentContext()$path)
}


`_@_` <- make_command(`@`)


## snail!
# `_@_`







decorator <- function(f)
{
  force(f)
  wrapper <- function(...)
  {
    cat("Hello from decorator\n")
    f(...)
  }

  return(wrapper)
}




# `_@_`decorator
# crazy_func <- function()
# {
#   cat("crazy\n")
# }
#
# crazy_func()

