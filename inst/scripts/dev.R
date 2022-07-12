decorator <- function(f)
{
  g <- f
  wrapper <- function(...)
  {
    message("Hello from wrapper")
    g(...)
  }

  return(wrapper)
}



test <- function(p = "Hello from test")
{
  cat(p, "\n")
}

test()


decorated <- decorator(test)
decorated()
decorated("blobb")


test <- decorator(test)
test()

test %>%
  decorator() -> test
test()


## write an operator that does the following
# @decorator
# test

`%decorate%` <- function(decorator, to_decorate)
{
  decorated <- decorator(to_decorate)
  return(decorated)
}

foo <- decorator %decorate% test
foo()
foo("blobb")

`%decoratoR%` <- function(decorator, to_decorate)
{
  decorated <- decorator(to_decorate)
  assign("foo", decorated, envir = environment(to_decorate))
}

decorator %decoratoR% test
foo()
foo("blobb")


`%@%` <- function(decorator, to_decorate)
{
  fun_name <- as.character(substitute(to_decorate))
  decorated <- decorator(to_decorate)
  assign(fun_name, decorated, envir = environment(to_decorate))
}

decorator %@% test

test()
test("bla")


