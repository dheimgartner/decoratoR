## Functional Programming
## https://adv-r.hadley.nz/fp.html




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
## proceed: https://adv-r.hadley.nz/quasiquotation.html







## Evaluation




## Translating R code



