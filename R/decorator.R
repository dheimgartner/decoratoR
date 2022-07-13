`%@%` <- function(decorator, to_decorate)
{
  f_name <- as.character(substitute(to_decorate))
  decorated <- decorator(to_decorate)
  assign(f_name, decorated, envir = environment(to_decorate))
}



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



foo <- function(p = "hello from foo")
{
  cat(p, "\n")
}


decorator %@% foo
foo()

foo <- decorator(foo)
foo()


## what @ should do:
## take the function decorator, execute next function definition,
## do function <- decorator(function)
#' @decorator
#' foo <- function(p = "hello from foo")
#' {
#'   cat(p, "\n")
#' }
