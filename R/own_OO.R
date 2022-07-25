methods <- list(
  can_swim = function(self, can = TRUE)
  {
    cat(glue::glue("{self$name} can swim\n"))
  }
)



parse_self <- function(self, methods)
{
  self <- enexpr(self)
  meths <- purrr::map(methods, expr(...)  ## hmm...
}



myObject <- list(
  new = function(name, ...)
  {
    dots <- list(name = name, ...)
    parsed_methods <- parse_self(dots, myObject_methods)
    myobject <- append(dots, myObject_methods)
    class(myobject) <- "myObject"
    return(myobject)
  }
)



mo <- myObject$new("blobb")
mo
mo$can_swim()
