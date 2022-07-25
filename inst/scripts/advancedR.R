## Functional Programming
## https://adv-r.hadley.nz/fp.html

## solutions:
## https://advanced-r-solutions.rbind.io/index.html




## Functionals
## take other functions as argument
randomise <- function(f) f(runif(1e3))
hist(runif(1e6))
randomise(mean)



library(purrr)

## replicate map
simple_map <- function(x, f, ...)
{
  out <- vector("list", length(x))
  for(i in seq_along(x))
  {
    out[[i]] <- f(x[[i]], ...)
  }
  out
}

times_n <- function(x, n) n*x
simple_map(1:3, times_n, n = 6)

## also f.ex. maxLik which takes a likelihood function as argument







## Function factories
## functions that create functions

library(rlang)  ## peek inside R


power1 <- function(exp)
{
  function(x)
  {
    x^exp
  }
}

exp <- 2
square <- power1(exp)
exp <- 3
square(10)



## stateful functions
## <<- modifies bindings in the enclosing env
## nice but use with moderation...
new_counter <- function()
{
  i <- 0

  function()
  {
    i <<- i + 1  ## <<-
    i
  }
}

counter_one <- new_counter()
counter_one()
counter_one()
counter_two <- new_counter()
counter_two()
counter_two()

## another function factory example
bootstrap <- function(df, var)
{
  n <- nrow(df)
  force(var)

  function()
  {
    col <- df[[var]]
    col[sample(n, replace = TRUE)]
  }
}


boot_mtcars <- bootstrap(mtcars, "mpg")
boot_mtcars()
boot_mtcars()




## maximum likelihood estimation
## very neat example
lprob_poission <- function(lambda, x)
{
  n <- length(x)
  (log(lambda) * sum(x)) - (n * lambda) - sum(lfactorial(x))
}

x1 <- c(41, 30, 31, 38, 29, 24, 30, 29, 31, 38)

lprob_poission(10, x1)
lprob_poission(20, x1)
lprob_poission(30, x1)

## but lambda is unknown and has to be estimated...
## The likelihood is the probability function seen through this lens:
## we want to find the lambda that makes the observed x the most likely.

ll_poisson <- function(x)
{
  n <- length(x)
  force(x)

  function(lambda)
  {
    lprob_poission(lambda, x)
  }
}

## even better: precomputations: any term that only involves x can be computed
## once in the factory
ll_poisson_precomp <- function(x)
{
  n <- length(x)
  sum_x <- sum(x)
  c <- sum(lfactorial(x))

  function(lambda)
  {
    log(lambda) * sum_x - n * lambda -c
  }


}

ll1 <- ll_poisson(x1)
ll1(10)
ll1(20)
ll1(30)

ll2 <- ll_poisson_precomp(x1)
ll2(10)
ll2(20)
ll2(30)

rbenchmark::benchmark(
  optimise(ll1, c(0, 100), maximum = TRUE),
  optimise(ll2, c(0, 100), maximum = TRUE)
)






## Function operators
## take function as input and return function (decorators)

count_calls <- function(f)
{
  force(f)
  i <- 0

  wrapper <- function(..., calls = FALSE)
  {
    if(calls) return(i)
    i <<- i + 1
    f(...)
  }


  return(wrapper)
}

sum_w_counter <- count_calls(sum)
sum_w_counter(c(1, 1))
sum_w_counter(c(1, 2))
sum_w_counter(c(1, 3))
sum_w_counter(calls = TRUE)
sum_w_counter(c(1, 4))
sum_w_counter(calls = TRUE)









## Metaprogramming
## https://adv-r.hadley.nz/metaprogramming.html

## metaprogramming is inspecting and modifying expressions of the abstract syntax tree
## @daniehei: it's not code, it's the idea of code...


## Big picture
## code is data (tree like structure -> abstract syntax tree)
library(rlang)
library(lobstr)  ## visualize r data structures with trees




## code is data
rlang::expr(mean(x, na.rm = TRUE))

capture_it <- function(x)
{
  rlang::enexpr(x)
}

capture_it(a + b + c)

## inspect and modify expressions (similar to list)
f <- expr(f(x = 1, y = 2))
f$z <- 3
f
f[[2]] <- NULL
f
f[[1]]




## code is a tree
## almost any programming language -> abstract syntax tree (AST)
## R is unusual in that you can actually inspect and manipulate this tree
lobstr::ast(f(a, "b"))
lobstr::ast(1 + 2 * 3)





## code can generate code
## code to create new trees
rlang::call2("f", 1, 2, 3) ==
  rlang::expr(f(1, 2, 3))
rlang::call2("+", 1, rlang::call2("*", 2, 3))

## !!x inserts the code tree in x into the expression, i.e. inserts expression into other expression
## building complex trees with help of simpler trees
xx <- expr(x + x)
yy <- expr(y + y)
expr(!!xx / !!yy)





## run code
## needs expression and environment, which tells R what the symbols in the
## expression mean
base::eval(expr(x + y), env(x = 1, y = 10))


## customising evaluation with functions
## override the behaviour of existing functions in an encapsulated way...
string_math <- function(x)
{
  e <- env(
    rlang::caller_env(),
    `+` = function(x, y) paste0(x, y),  ## originally +
    `*` = function(x, y) strrep(x, y)
  )

  eval(enexpr(x), e)
}

name <- "Daniel"
string_math("Hello " @ name)  ## wow! -> could be used for likelihood(...) and inside likelihood call stuff like random_coef(...)
string_math(("x" * 2 + "-y") * 3)






## customizing evaluation with data
df <- data.frame(x = 1:5, y = sample(5))
eval(expr(x + y), df) ## some pitfalls
rlang::eval_tidy(expr(x + y), df)

## wrap up in function
with2 <- function(df, expr) {
  eval_tidy(enexpr(expr), df)
}

with2(df, x + y)





## quosures
## quosures bundle and expression with an environment
## -> use enquo instead of enexpr (whenever you use data masking, i.e. when you refer
## to vars in a dataframe but treat vars as individual objects - not df$var)







## Expressions
## separating the description of the action from the action itself
## focus of the chapter: understand the data structures that underlie expressions
## mastering this knowledge will allow you to inspect and modify captured code, and to generate code with code
## constants, symbols and calls (which are data structures) are collectively known as expressions (@daniehei i.e. data structures that underly the AST)
y <- x * 10
#> Error: object 'x' not found

z <- rlang::expr(y <- x * 10)
z
x <- 4
eval(z)
y

View(expr(f(x, "y", 1)))
lobstr::ast(f(x, "y", 1))
lobstr::ast(f(g(1, 2), h(3, 4, i())))
lobstr::ast(y <- x * 10)


## expressions:
## constants
## symbols
## calls
## pairlists
## empty symbols



## parsing and grammar
## the process by which a computer lanugage takes a string and constructs an
## expression is called parsing and is governed by a set of rules known
## as grammar


## parsing and deparsing
x <- 10
x1 <- "y <- x + 10"
x1

x2 <- rlang::parse_expr(x1)
x2
eval(x2)
y



## walking AST with recursive functions
expr_type <- function(x)
{
  if (rlang::is_syntactic_literal(x))
  {
    "constant"
  }
  else if (is.symbol(x))
  {
    "symbol"
  }
  else if (is.call(x))
  {
    "call"
  }
  else if (is.pairlist(x))
  {
    "pairlist"
  }
  else
  {
    typeof(x)
  }
}



expr_type(expr("a"))
expr_type(expr(x))
expr_type(expr(f(1, 2)))



switch_expr <- function(x, ...)
{
  switch(expr_type(x),
         ...,
         stop("Don't know how to handle type ", typeof(x), call. = FALSE))
}



recurse_call <- function(x)
{
  switch_expr(x,
              ## base cases
              symbol = ,
              constant = ,

              ## recursive cases
              call = ,
              pairlist =
  )
}


## function uses logical abbreviation?
logical_abbr_rec <- function(x)
{
  switch_expr(x,
              constant = FALSE,
              symbol = as_string(x) %in% c("F", "T")
  )
}



logical_abbr <- function(x)
{
  logical_abbr_rec(enexpr(x))
}


logical_abbr(T)
logical_abbr(TRUE)

## listing all variables created by assignment
# .
# .
# .
# .
# .


## pairlists
## function arguments
## can be treated like regular lists
f <- expr(function(x, y = 10) x + y)
args <- f[[2]]
args
typeof(args)



## missing arguments
## only need to care about missing symbols if you're programatically creating
## functions with missing arguments...
rlang::is_missing(expr())


## expression vectors
## ~just a list of expressions






## Quasiquotation
## https://adv-r.hadley.nz/quasiquotation.html
## quasiquotation != quosures
## quoting and unquoting!
## where quotation is the act of capturing an unevaluated expression,
## unquotiation is the ability to selectively evaluate parts of an otherwise
## quoted expressioin. together this is called quasiquotation.
## quasiquotation makes it easy to create functions that combine code written by
## the function's author with code written by the function's user (language of LL!).

## motivation
ccement <- function(...)
{
  elems <- rlang::ensyms(...)
  purrr::map(elems, rlang::as_string) %>%  ## as_string cast symbol to string
    as.vector(mode = "character")
}


ccement(hello,
        world,
        how,
        is,
        it,
        going)




## !! called "unquote" (injection operator)


x <- "hello"
rlang::is_expression(x)


## distinction between quoted and evaluated arguments is important:
## an evaluated argument obeys R's usual evaluation rules
## a quoted argument is captured by the function and is processed in some custom way!
## -> non standard evaluation NSE

## ccement quotes all its arguments -> symbols



## quoting
## capturing an expression without evaluating it
## there are 4 important quoting functions:
## 1. expr() captures its argument exactly as provided (without whitespace and comments)
## however:
f1 <- function(x) rlang::expr(x)
f1(a + b + c)

## 2. enexpr() captures what the caller supplied to the function by looking at the internal
## promise object that powers lazy evaluation

## 3. enexprs() capture all arguments in ...
f <- function(...) rlang::enexprs(...)
f(x = 1, y = 2, z = x * y)
test <- f(x = 1, y = 2, z = x * y)
rlang::is_expression(test$z)

## 4. exprs() make a list of expressions
## useful interactively...
rlang::exprs(x = x ^ 2, y = y ^ 3, z ?? .)  ## can also capture stuff that does not make sense...



## In short, use enexpr() and enexprs() to capture the expressions supplied as arguments by the user.
## Use expr() and exprs() to capture expressions that you supply.



## capturing symbols
## if you want to allow the user to specify a variable name (symbol) and not
## an arbitrary expression (although symbols are expressions...)
x <- rlang::sym(x)
rlang::ensym(x)
rlang::ensyms()
f <- function(...)
{
  rlang::ensyms(...)
}



## with base R
## each rlang function described above has a base R equivalent
## base equivalents do not support unquoting (discussed further below)
## -> makes them quoting funcs rather than quasiquoting funcs

## 1. expr() -> quote()
## 2. enexpr() -> substitute()
## 3. exprs() -> alist()
## 4. enexprs() -> substitute() [not documented!]
f <- function(...) as.list(substitute(...()))

## further (but discussed later):
## bquote() -> form of quasiquotation
## ~ formula -> quoting function that also captures the environment




## substitution
## substitute captures unevaluated expressions but does also substitution
f <- function(x) substitute(x * 2)
f(a + b + c)

## if you want to use subsitute for substitution use the second argument (env)
## for substitution
substitute(x * y * z, list(x = 10, y = quote(a + b)))





## unquoting
## unquoting allows you to selectively evaluate parts of the expression
## (allows you to merge ASTs using a template AST)
## base R functions use other techniques to do stuff similar to unquoting

## selectively evaluate code inside expr(): expr(!!x) is equivalent to x
## later: eval(expr(x)) is also equivalent to x

## @daniehei: read carefully ONLY inside expressions!
y <- quote(variable)
!!y
## but
expr(!!y)


## unquoting one argument
## !! unquotes a single argument in a function: it takes a single expression,
## evaluates it and inlines the result in the AST (i.e. !! introduces a placeholder
## in the AST...)
x <- expr(-1)
expr(f(!!x, y))

## also works with symbols and constants
a <- sym("y")
b <- 1
expr(f(!!a, !!b))

## if RHS of !! is a function call - it will evaluate it and substitute the result




## unquoting a function
## only challenge expr(!!f(x, y)) unquotes the result of f(x, y) so you need an
## extra pair of parentheses
f <- expr(foo)
expr((!!f)(x, y))
## equivalent to
call2(f, expr(x), expr(y))


## unquoting missing arguments
arg <- rlang::missing_arg()
expr(foo(!!arg, !!arg))
#> Error in enexpr(expr) : object 'arg' not found
expr(foo(!!maybe_missing(arg)))




## unquoting special form
get_from_iris <- function(x)
{
  y <- enexpr(x)
  z <- expr(`$`(iris, !!y))
  eval(z)
}


## @daniehei not really related...
filter_iris <- function(var, filt)
{
  var <- enexpr(var)
  filt <- enexpr(filt)
  filt <- rlang::as_string(filt)
  z <<- expr(dplyr::filter(iris, !!var == filt))
  eval(z)
}

filter_iris(Species, virginica)




## unquoting many args
## !!! takes a list of expressions and inserts them at the location of the !!!
xs <- exprs(1, a, -b)
ys <- set_names(xs, c("a", "b", "c"))
expr(f(!!!ys, d = 4))




## non-standard ASTs
## ASTs that contain components that are not expressions





## non-quoting
## one baseR funciton that implements quasiquotation: bquote()
## uses .() for unquoting; however rarely used...
xyz <- bquote((x + y + z))
rlang::is_expression(xyz)
bquote(-.(xyz) / 2)

zyx <- rlang::expr(x + y + z)
bquote(-.(zyx) / 2)

## baseR uses non-quoting techniques, i.e. selectively turns off quoting rather
## than using unquoting in order to allow indirect specification.

## @daniehei The above terms are simply talking about interfaces where a name to be used is
## captured from the source code the user typed, and thus does not need quote marks.


## 4 basic forms of that concept:
## 1. pair of quoting ($) and non-quoting ([[]]) functions
`$`(mtcars, cyl)  ## second argument is quoted
## three other quoting functions closely related to $: subset(), transform(), with()

## 2. a pair of quoting and non-quoting arguments: e.g. rm(..., list) where ...
## allows you to provide bare variable names or a character vector via the list argument

## 3. an argument that controls whether a different argument is quoting or non-quoting
## e.g. library()
library(rlang)
pkg <- "rlang"
library(pkg)
#> Error in library(pkg) : there is no package called ‘pkg’
library(pkg, character.only = TRUE)


## @daniehei: library -> if the arg (pkgname) is not a character it is quoted
## -> as.character(substitute(pkgname))


## 4. quoting if evaluation fails
help(var)
var <- "mean"
help(var)
var <- 10
help(var)

## @daniehei if a function quotes an argument, that is, you can specify the arg as
## source without quoting. unquoting = selectively evaluate code!


## KEEP IN MIND: quoting and unquoting selectively capture the idea of code without
## evaluating it, and actually selectively evaluate part of code




## ... (dot-dot-dot)
## read again 19 Quasiquotation Outline
dfs <- list(
  a = data.frame(x = 1, y = 2),
  b = data.frame(x = 3, y = 4)
)

dplyr::bind_rows(!!!dfs)
rbind(exprs(!!!dfs))  ## does not work (because rbind does not understand lists...)

rbind2 <- function(...)
{
  ldfs <- rlang::list2(...)
  rdf <- purrr::map_df(ldfs, rbind)
  rdf
}

rbind2(!!!dfs)
rbind2(dfs)

rbind3 <- function(...)
{
  dots <- list(...)
  rdf <- purrr::map_df(dots, rbind)
  rdf
}

rbind3(dfs)

## closely related to *args and **kwargs!

var <- "x"
val <- c(1, 2, 3)
tibble::tibble(!!var := val)

## we need := because R's grammar does not allow expressions as argument names...


set_attr <- function(.x, ...)
{
  attr <- rlang::list2(...)
  attributes(.x) <- attr
  .x
}

attrs <- list(x = 1, y = 2)
attr_name <- "z"

1:10 %>%
  set_attr(w = 0, !!!attrs, !!attr_name := 3) %>%
  str()


## compare with
set_attr <- function(.x, ...)
{
  attr <- list(...)
  attributes(.x) <- attr
  .x
}

1:10 %>%
  set_attr(w = 0) %>%   ## owrks
  # set_attr(attrs)  ## Error... : attributes must be named!
  set_attr(!!attr_name := 3) ## Error... (also without :=)

?rlang::`dyn-dots`  ## -> injects names




## use rlang::exec() if you want to use this technique with a function that does
## not have tidy dots
## directly
exec("mean", x = 1:10, na.rm = TRUE)
## indirectly
args <- list(x = 1:10, na.rm = TRUE, trim = 0.1)
exec("mean", !!!args)
## mixed...



## base R
## do.call: first argument gives a function to call, second argument is a list
## of arguments to be passed to the function
do.call("rbind", dfs)
## can also solve the second kind of problem (:=)
var <- "x"
val <- c(1, 2, 3)
args <- list(val)
names(args) <- var
do.call("data.frame", args)









## Evaluation
## proceed: https://adv-r.hadley.nz/evaluation.html




## Translating R code



