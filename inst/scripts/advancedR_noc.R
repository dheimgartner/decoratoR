randomise <- (function(f) f(runif(1000)))
hist(runif(1e+06))
randomise(mean)
library(purrr)
simple_map <- (function(x, f, ...) {
times_n <- (function(x, n) n * x)
simple_map(1:3, times_n, n = 6)
library(rlang)
power1 <- (function(exp) {
exp <- 2
square <- power1(exp)
exp <- 3
square(10)
new_counter <- (function() {
counter_one <- new_counter()
counter_one()
counter_one()
counter_two <- new_counter()
counter_two()
counter_two()
bootstrap <- (function(df, var) {
boot_mtcars <- bootstrap(mtcars, "mpg")
boot_mtcars()
boot_mtcars()
lprob_poission <- (function(lambda, x) {
x1 <- c(41, 30, 31, 38, 29, 24, 30, 29, 31, 38)
lprob_poission(10, x1)
lprob_poission(20, x1)
lprob_poission(30, x1)
ll_poisson <- (function(x) {
ll_poisson_precomp <- (function(x) {
ll1 <- ll_poisson(x1)
ll1(10)
ll1(20)
ll1(30)
ll2 <- ll_poisson_precomp(x1)
ll2(10)
ll2(20)
ll2(30)
rbenchmark::benchmark(optimise(ll1, c(0, 100), maximum = TRUE), optimise(ll2, c(0, 100), maximum = TRUE))
count_calls <- (function(f) {
sum_w_counter <- count_calls(sum)
sum_w_counter(c(1, 1))
sum_w_counter(c(1, 2))
sum_w_counter(c(1, 3))
sum_w_counter(calls = TRUE)
sum_w_counter(c(1, 4))
sum_w_counter(calls = TRUE)
library(rlang)
library(lobstr)
rlang::expr(mean(x, na.rm = TRUE))
capture_it <- (function(x) {
capture_it(a + b + c)
f <- expr(f(x = 1, y = 2))
f$z <- 3
f
f[[2]] <- NULL
f
f[[1]]
lobstr::ast(f(a, "b"))
lobstr::ast(1 + 2 * 3)
rlang::call2("f", 1, 2, 3) == rlang::expr(f(1, 2, 3))
rlang::call2("+", 1, rlang::call2("*", 2, 3))
xx <- expr(x + x)
yy <- expr(y + y)
expr(!!xx / (!!yy))
base::eval(expr(x + y), env(x = 1, y = 10))
string_math <- (function(x) {
name <- "Daniel"
string_math("Hello "@name)
string_math(("x" * 2 + "-y") * 3)
df <- data.frame(x = 1:5, y = sample(5))
eval(expr(x + y), df)
rlang::eval_tidy(expr(x + y), df)
with2 <- (function(df, expr) {
with2(df, x + y)
y <- x * 10
z <- rlang::expr(y <- x * 10)
z
x <- 4
eval(z)
y
View(expr(f(x, "y", 1)))
lobstr::ast(f(x, "y", 1))
lobstr::ast(f(g(1, 2), h(3, 4, i())))
lobstr::ast(y <- x * 10)
x <- 10
x1 <- "y <- x + 10"
x1
x2 <- rlang::parse_expr(x1)
x2
eval(x2)
y
expr_type <- (function(x) {
expr_type(expr("a"))
expr_type(expr(x))
expr_type(expr(f(1, 2)))
switch_expr <- (function(x, ...) {
recurse_call <- (function(x) {
logical_abbr_rec <- (function(x) {
logical_abbr <- (function(x) {
logical_abbr(T)
logical_abbr(TRUE)
f <- expr(function(x, y = 10) x + y)
args <- f[[2]]
args
typeof(args)
rlang::is_missing(expr())
ccement <- (function(...) {
ccement(hello, world, how, is, it, going)
x <- "hello"
rlang::is_expression(x)
f1 <- (function(x) rlang::expr(x))
f1(a + b + c)
f <- (function(...) rlang::enexprs(...))
f(x = 1, y = 2, z = x * y)
test <- f(x = 1, y = 2, z = x * y)
rlang::is_expression(test$z)
rlang::exprs(x = x^2, y = y^3, z ? (?.))
x <- rlang::sym(x)
rlang::ensym(x)
rlang::ensyms()
f <- (function(...) {
f <- (function(...) as.list(substitute(...())))
f <- (function(x) substitute(x * 2))
f(a + b + c)
substitute(x * y * z, list(x = 10, y = quote(a + b)))
y <- quote(variable)
!!y
expr(!!y)
x <- expr(-1)
expr(f(!!x, y))
a <- sym("y")
b <- 1
expr(f(!!a, !!b))
f <- expr(foo)
expr((!!f)(x, y))
call2(f, expr(x), expr(y))
arg <- rlang::missing_arg()
expr(foo(!!arg, !!arg))
expr(foo(!!maybe_missing(arg)))
get_from_iris <- (function(x) {
filter_iris <- (function(var, filt) {
filter_iris(Species, virginica)
xs <- exprs(1, a, -b)
ys <- set_names(xs, c("a", "b", "c"))
expr(f(!!!ys, d = 4))
xyz <- bquote((x + y + z))
rlang::is_expression(xyz)
bquote(-.(xyz) / 2)
zyx <- rlang::expr(x + y + z)
bquote(-.(zyx) / 2)
mtcars$cyl
library(rlang)
pkg <- "rlang"
library(pkg)
library(pkg, character.only = TRUE)
help(var)
var <- "mean"
help(var)
var <- 10
help(var)
dfs <- list(a = data.frame(x = 1, y = 2), b = data.frame(x = 3, y = 4))
dplyr::bind_rows(!!!dfs)
rbind(exprs(!!!dfs))
rbind2 <- (function(...) {
rbind2(!!!dfs)
rbind2(dfs)
rbind3 <- (function(...) {
rbind3(dfs)
var <- "x"
val <- c(1, 2, 3)
tibble::tibble(!!var := val)
set_attr <- (function(.x, ...) {
attrs <- list(x = 1, y = 2)
attr_name <- "z"
1:10 %>% set_attr(w = 0, !!!attrs, !!attr_name := 3) %>% str()
set_attr <- (function(.x, ...) {
1:10 %>% set_attr(w = 0) %>% set_attr(!!attr_name := 3)
?rlang::`dyn-dots`
exec("mean", x = 1:10, na.rm = TRUE)
args <- list(x = 1:10, na.rm = TRUE, trim = 0.1)
exec("mean", !!!args)
do.call("rbind", dfs)
var <- "x"
val <- c(1, 2, 3)
args <- list(val)
names(args) <- var
do.call("data.frame", args)
eval(expr(print(x + 1)), env(x = 1000))
source2 <- (function(path, env = caller_env()) {
randomise <- (function(f) f(runif(1000)))
hist(runif(1e+06))
randomise(mean)
library(purrr)
  out <- vector("list", length(x))
times_n <- (function(x, n) n * x)
simple_map(1:3, times_n, n = 6)
library(rlang)
  function(x) {
exp <- 2
square <- power1(exp)
exp <- 3
square(10)
  i <- 0
counter_one <- new_counter()
counter_one()
counter_one()
counter_two <- new_counter()
counter_two()
counter_two()
  n <- nrow(df)
boot_mtcars <- bootstrap(mtcars, "mpg")
boot_mtcars()
boot_mtcars()
  n <- length(x)
x1 <- c(41, 30, 31, 38, 29, 24, 30, 29, 31, 38)
lprob_poission(10, x1)
lprob_poission(20, x1)
lprob_poission(30, x1)
  n <- length(x)
  n <- length(x)
ll1 <- ll_poisson(x1)
ll1(10)
ll1(20)
ll1(30)
ll2 <- ll_poisson_precomp(x1)
ll2(10)
ll2(20)
ll2(30)
rbenchmark::benchmark(optimise(ll1, c(0, 100), maximum = TRUE), optimise(ll2, c(0, 100), maximum = TRUE))
  force(f)
sum_w_counter <- count_calls(sum)
sum_w_counter(c(1, 1))
sum_w_counter(c(1, 2))
sum_w_counter(c(1, 3))
sum_w_counter(calls = TRUE)
sum_w_counter(c(1, 4))
sum_w_counter(calls = TRUE)
library(rlang)
library(lobstr)
rlang::expr(mean(x, na.rm = TRUE))
  rlang::enexpr(x)
capture_it(a + b + c)
f <- expr(f(x = 1, y = 2))
f$z <- 3
f
f[[2]] <- NULL
f
f[[1]]
lobstr::ast(f(a, "b"))
lobstr::ast(1 + 2 * 3)
rlang::call2("f", 1, 2, 3) == rlang::expr(f(1, 2, 3))
rlang::call2("+", 1, rlang::call2("*", 2, 3))
xx <- expr(x + x)
yy <- expr(y + y)
expr(!!xx / (!!yy))
base::eval(expr(x + y), env(x = 1, y = 10))
  e <- env(rlang::caller_env(), `+` = function(x, y) paste0(x, y), `*` = function(x, y) strrep(x, y))
name <- "Daniel"
string_math("Hello "@name)
string_math(("x" * 2 + "-y") * 3)
df <- data.frame(x = 1:5, y = sample(5))
eval(expr(x + y), df)
rlang::eval_tidy(expr(x + y), df)
  eval_tidy(enexpr(expr), df)
with2(df, x + y)
y <- x * 10
z <- rlang::expr(y <- x * 10)
z
x <- 4
eval(z)
y
View(expr(f(x, "y", 1)))
lobstr::ast(f(x, "y", 1))
lobstr::ast(f(g(1, 2), h(3, 4, i())))
lobstr::ast(y <- x * 10)
x <- 10
x1 <- "y <- x + 10"
x1
x2 <- rlang::parse_expr(x1)
x2
eval(x2)
y
  if (rlang::is_syntactic_literal(x)) {
expr_type(expr("a"))
expr_type(expr(x))
expr_type(expr(f(1, 2)))
  switch(expr_type(x), ..., stop("Don't know how to handle type ", typeof(x), call. = FALSE))
  switch_expr(x, symbol = , constant = , call = , pairlist = )
  switch_expr(x, constant = FALSE, symbol = as_string(x) %in% c("F", "T"))
  logical_abbr_rec(enexpr(x))
logical_abbr(T)
logical_abbr(TRUE)
f <- expr(function(x, y = 10) x + y)
args <- f[[2]]
args
typeof(args)
rlang::is_missing(expr())
  elems <- rlang::ensyms(...)
ccement(hello, world, how, is, it, going)
x <- "hello"
rlang::is_expression(x)
f1 <- (function(x) rlang::expr(x))
f1(a + b + c)
f <- (function(...) rlang::enexprs(...))
f(x = 1, y = 2, z = x * y)
test <- f(x = 1, y = 2, z = x * y)
rlang::is_expression(test$z)
rlang::exprs(x = x^2, y = y^3, z ? (?.))
x <- rlang::sym(x)
rlang::ensym(x)
rlang::ensyms()
  rlang::ensyms(...)
f <- (function(...) as.list(substitute(...())))
f <- (function(x) substitute(x * 2))
f(a + b + c)
substitute(x * y * z, list(x = 10, y = quote(a + b)))
y <- quote(variable)
!!y
expr(!!y)
x <- expr(-1)
expr(f(!!x, y))
a <- sym("y")
b <- 1
expr(f(!!a, !!b))
f <- expr(foo)
expr((!!f)(x, y))
call2(f, expr(x), expr(y))
arg <- rlang::missing_arg()
expr(foo(!!arg, !!arg))
expr(foo(!!maybe_missing(arg)))
  y <- enexpr(x)
  var <- enexpr(var)
filter_iris(Species, virginica)
xs <- exprs(1, a, -b)
ys <- set_names(xs, c("a", "b", "c"))
expr(f(!!!ys, d = 4))
xyz <- bquote((x + y + z))
rlang::is_expression(xyz)
bquote(-.(xyz) / 2)
zyx <- rlang::expr(x + y + z)
bquote(-.(zyx) / 2)
mtcars$cyl
library(rlang)
pkg <- "rlang"
library(pkg)
library(pkg, character.only = TRUE)
help(var)
var <- "mean"
help(var)
var <- 10
help(var)
dfs <- list(a = data.frame(x = 1, y = 2), b = data.frame(x = 3, y = 4))
dplyr::bind_rows(!!!dfs)
rbind(exprs(!!!dfs))
  ldfs <- rlang::list2(...)
rbind2(!!!dfs)
rbind2(dfs)
  dots <- list(...)
rbind3(dfs)
var <- "x"
val <- c(1, 2, 3)
tibble::tibble(!!var := val)
  attr <- rlang::list2(...)
attrs <- list(x = 1, y = 2)
attr_name <- "z"
1:10 %>% set_attr(w = 0, !!!attrs, !!attr_name := 3) %>% str()
  attr <- list(...)
1:10 %>% set_attr(w = 0) %>% set_attr(!!attr_name := 3)
?rlang::`dyn-dots`
exec("mean", x = 1:10, na.rm = TRUE)
args <- list(x = 1:10, na.rm = TRUE, trim = 0.1)
exec("mean", !!!args)
do.call("rbind", dfs)
var <- "x"
val <- c(1, 2, 3)
args <- list(val)
names(args) <- var
do.call("data.frame", args)
eval(expr(print(x + 1)), env(x = 1000))
  file <- paste(readLines(path, warn = FALSE), collapse = "\n")
randomise <- (function(f) f(runif(1000)))
hist(runif(1e+06))
randomise(mean)
library(purrr)
  for (i in seq_along(x)) {
times_n <- (function(x, n) n * x)
simple_map(1:3, times_n, n = 6)
library(rlang)
    x^exp
exp <- 2
square <- power1(exp)
exp <- 3
square(10)
  function() {
counter_one <- new_counter()
counter_one()
counter_one()
counter_two <- new_counter()
counter_two()
counter_two()
  force(var)
boot_mtcars <- bootstrap(mtcars, "mpg")
boot_mtcars()
boot_mtcars()
  (log(lambda) * sum(x)) - (n * lambda) - sum(lfactorial(x))
x1 <- c(41, 30, 31, 38, 29, 24, 30, 29, 31, 38)
lprob_poission(10, x1)
lprob_poission(20, x1)
lprob_poission(30, x1)
  force(x)
  sum_x <- sum(x)
ll1 <- ll_poisson(x1)
ll1(10)
ll1(20)
ll1(30)
ll2 <- ll_poisson_precomp(x1)
ll2(10)
ll2(20)
ll2(30)
rbenchmark::benchmark(optimise(ll1, c(0, 100), maximum = TRUE), optimise(ll2, c(0, 100), maximum = TRUE))
  i <- 0
sum_w_counter <- count_calls(sum)
sum_w_counter(c(1, 1))
sum_w_counter(c(1, 2))
sum_w_counter(c(1, 3))
sum_w_counter(calls = TRUE)
sum_w_counter(c(1, 4))
sum_w_counter(calls = TRUE)
library(rlang)
library(lobstr)
rlang::expr(mean(x, na.rm = TRUE))
})
capture_it(a + b + c)
f <- expr(f(x = 1, y = 2))
f$z <- 3
f
f[[2]] <- NULL
f
f[[1]]
lobstr::ast(f(a, "b"))
lobstr::ast(1 + 2 * 3)
rlang::call2("f", 1, 2, 3) == rlang::expr(f(1, 2, 3))
rlang::call2("+", 1, rlang::call2("*", 2, 3))
xx <- expr(x + x)
yy <- expr(y + y)
expr(!!xx / (!!yy))
base::eval(expr(x + y), env(x = 1, y = 10))
  eval(enexpr(x), e)
name <- "Daniel"
string_math("Hello "@name)
string_math(("x" * 2 + "-y") * 3)
df <- data.frame(x = 1:5, y = sample(5))
eval(expr(x + y), df)
rlang::eval_tidy(expr(x + y), df)
})
with2(df, x + y)
y <- x * 10
z <- rlang::expr(y <- x * 10)
z
x <- 4
eval(z)
y
View(expr(f(x, "y", 1)))
lobstr::ast(f(x, "y", 1))
lobstr::ast(f(g(1, 2), h(3, 4, i())))
lobstr::ast(y <- x * 10)
x <- 10
x1 <- "y <- x + 10"
x1
x2 <- rlang::parse_expr(x1)
x2
eval(x2)
y
    "constant"
expr_type(expr("a"))
expr_type(expr(x))
expr_type(expr(f(1, 2)))
})
})
})
})
logical_abbr(T)
logical_abbr(TRUE)
f <- expr(function(x, y = 10) x + y)
args <- f[[2]]
args
typeof(args)
rlang::is_missing(expr())
  purrr::map(elems, rlang::as_string) %>% as.vector(mode = "character")
ccement(hello, world, how, is, it, going)
x <- "hello"
rlang::is_expression(x)
f1 <- (function(x) rlang::expr(x))
f1(a + b + c)
f <- (function(...) rlang::enexprs(...))
f(x = 1, y = 2, z = x * y)
test <- f(x = 1, y = 2, z = x * y)
rlang::is_expression(test$z)
rlang::exprs(x = x^2, y = y^3, z ? (?.))
x <- rlang::sym(x)
rlang::ensym(x)
rlang::ensyms()
})
f <- (function(...) as.list(substitute(...())))
f <- (function(x) substitute(x * 2))
f(a + b + c)
substitute(x * y * z, list(x = 10, y = quote(a + b)))
y <- quote(variable)
!!y
expr(!!y)
x <- expr(-1)
expr(f(!!x, y))
a <- sym("y")
b <- 1
expr(f(!!a, !!b))
f <- expr(foo)
expr((!!f)(x, y))
call2(f, expr(x), expr(y))
arg <- rlang::missing_arg()
expr(foo(!!arg, !!arg))
expr(foo(!!maybe_missing(arg)))
  z <- expr(iris$(!!y))
  filt <- enexpr(filt)
filter_iris(Species, virginica)
xs <- exprs(1, a, -b)
ys <- set_names(xs, c("a", "b", "c"))
expr(f(!!!ys, d = 4))
xyz <- bquote((x + y + z))
rlang::is_expression(xyz)
bquote(-.(xyz) / 2)
zyx <- rlang::expr(x + y + z)
bquote(-.(zyx) / 2)
mtcars$cyl
library(rlang)
pkg <- "rlang"
library(pkg)
library(pkg, character.only = TRUE)
help(var)
var <- "mean"
help(var)
var <- 10
help(var)
dfs <- list(a = data.frame(x = 1, y = 2), b = data.frame(x = 3, y = 4))
dplyr::bind_rows(!!!dfs)
rbind(exprs(!!!dfs))
  rdf <- purrr::map_df(ldfs, rbind)
rbind2(!!!dfs)
rbind2(dfs)
  rdf <- purrr::map_df(dots, rbind)
rbind3(dfs)
var <- "x"
val <- c(1, 2, 3)
tibble::tibble(!!var := val)
  attributes(.x) <- attr
attrs <- list(x = 1, y = 2)
attr_name <- "z"
1:10 %>% set_attr(w = 0, !!!attrs, !!attr_name := 3) %>% str()
  attributes(.x) <- attr
1:10 %>% set_attr(w = 0) %>% set_attr(!!attr_name := 3)
?rlang::`dyn-dots`
exec("mean", x = 1:10, na.rm = TRUE)
args <- list(x = 1:10, na.rm = TRUE, trim = 0.1)
exec("mean", !!!args)
do.call("rbind", dfs)
var <- "x"
val <- c(1, 2, 3)
args <- list(val)
names(args) <- var
do.call("data.frame", args)
eval(expr(print(x + 1)), env(x = 1000))
  exprs <- rlang::parse_exprs(file)
randomise <- (function(f) f(runif(1000)))
hist(runif(1e+06))
randomise(mean)
library(purrr)
    out[[i]] <- f(x[[i]], ...)
times_n <- (function(x, n) n * x)
simple_map(1:3, times_n, n = 6)
library(rlang)
  }
exp <- 2
square <- power1(exp)
exp <- 3
square(10)
    i <<- i + 1
counter_one <- new_counter()
counter_one()
counter_one()
counter_two <- new_counter()
counter_two()
counter_two()
  function() {
boot_mtcars <- bootstrap(mtcars, "mpg")
boot_mtcars()
boot_mtcars()
})
x1 <- c(41, 30, 31, 38, 29, 24, 30, 29, 31, 38)
lprob_poission(10, x1)
lprob_poission(20, x1)
lprob_poission(30, x1)
  function(lambda) {
  c <- sum(lfactorial(x))
ll1 <- ll_poisson(x1)
ll1(10)
ll1(20)
ll1(30)
ll2 <- ll_poisson_precomp(x1)
ll2(10)
ll2(20)
ll2(30)
rbenchmark::benchmark(optimise(ll1, c(0, 100), maximum = TRUE), optimise(ll2, c(0, 100), maximum = TRUE))
  wrapper <- (function(..., calls = FALSE) {
sum_w_counter <- count_calls(sum)
sum_w_counter(c(1, 1))
sum_w_counter(c(1, 2))
sum_w_counter(c(1, 3))
sum_w_counter(calls = TRUE)
sum_w_counter(c(1, 4))
sum_w_counter(calls = TRUE)
library(rlang)
library(lobstr)
rlang::expr(mean(x, na.rm = TRUE))
capture_it <- (function(x) {
capture_it(a + b + c)
f <- expr(f(x = 1, y = 2))
f$z <- 3
f
f[[2]] <- NULL
f
f[[1]]
lobstr::ast(f(a, "b"))
lobstr::ast(1 + 2 * 3)
rlang::call2("f", 1, 2, 3) == rlang::expr(f(1, 2, 3))
rlang::call2("+", 1, rlang::call2("*", 2, 3))
xx <- expr(x + x)
yy <- expr(y + y)
expr(!!xx / (!!yy))
base::eval(expr(x + y), env(x = 1, y = 10))
})
name <- "Daniel"
string_math("Hello "@name)
string_math(("x" * 2 + "-y") * 3)
df <- data.frame(x = 1:5, y = sample(5))
eval(expr(x + y), df)
rlang::eval_tidy(expr(x + y), df)
with2 <- (function(df, expr) {
with2(df, x + y)
y <- x * 10
z <- rlang::expr(y <- x * 10)
z
x <- 4
eval(z)
y
View(expr(f(x, "y", 1)))
lobstr::ast(f(x, "y", 1))
lobstr::ast(f(g(1, 2), h(3, 4, i())))
lobstr::ast(y <- x * 10)
x <- 10
x1 <- "y <- x + 10"
x1
x2 <- rlang::parse_expr(x1)
x2
eval(x2)
y
  } else if (is.symbol(x)) {
expr_type(expr("a"))
expr_type(expr(x))
expr_type(expr(f(1, 2)))
switch_expr <- (function(x, ...) {
recurse_call <- (function(x) {
logical_abbr_rec <- (function(x) {
logical_abbr <- (function(x) {
logical_abbr(T)
logical_abbr(TRUE)
f <- expr(function(x, y = 10) x + y)
args <- f[[2]]
args
typeof(args)
rlang::is_missing(expr())
})
ccement(hello, world, how, is, it, going)
x <- "hello"
rlang::is_expression(x)
f1 <- (function(x) rlang::expr(x))
f1(a + b + c)
f <- (function(...) rlang::enexprs(...))
f(x = 1, y = 2, z = x * y)
test <- f(x = 1, y = 2, z = x * y)
rlang::is_expression(test$z)
rlang::exprs(x = x^2, y = y^3, z ? (?.))
x <- rlang::sym(x)
rlang::ensym(x)
rlang::ensyms()
f <- (function(...) {
f <- (function(...) as.list(substitute(...())))
f <- (function(x) substitute(x * 2))
f(a + b + c)
substitute(x * y * z, list(x = 10, y = quote(a + b)))
y <- quote(variable)
!!y
expr(!!y)
x <- expr(-1)
expr(f(!!x, y))
a <- sym("y")
b <- 1
expr(f(!!a, !!b))
f <- expr(foo)
expr((!!f)(x, y))
call2(f, expr(x), expr(y))
arg <- rlang::missing_arg()
expr(foo(!!arg, !!arg))
expr(foo(!!maybe_missing(arg)))
  eval(z)
  filt <- rlang::as_string(filt)
filter_iris(Species, virginica)
xs <- exprs(1, a, -b)
ys <- set_names(xs, c("a", "b", "c"))
expr(f(!!!ys, d = 4))
xyz <- bquote((x + y + z))
rlang::is_expression(xyz)
bquote(-.(xyz) / 2)
zyx <- rlang::expr(x + y + z)
bquote(-.(zyx) / 2)
mtcars$cyl
library(rlang)
pkg <- "rlang"
library(pkg)
library(pkg, character.only = TRUE)
help(var)
var <- "mean"
help(var)
var <- 10
help(var)
dfs <- list(a = data.frame(x = 1, y = 2), b = data.frame(x = 3, y = 4))
dplyr::bind_rows(!!!dfs)
rbind(exprs(!!!dfs))
  rdf
rbind2(!!!dfs)
rbind2(dfs)
  rdf
rbind3(dfs)
var <- "x"
val <- c(1, 2, 3)
tibble::tibble(!!var := val)
  .x
attrs <- list(x = 1, y = 2)
attr_name <- "z"
1:10 %>% set_attr(w = 0, !!!attrs, !!attr_name := 3) %>% str()
  .x
1:10 %>% set_attr(w = 0) %>% set_attr(!!attr_name := 3)
?rlang::`dyn-dots`
exec("mean", x = 1:10, na.rm = TRUE)
args <- list(x = 1:10, na.rm = TRUE, trim = 0.1)
exec("mean", !!!args)
do.call("rbind", dfs)
var <- "x"
val <- c(1, 2, 3)
args <- list(val)
names(args) <- var
do.call("data.frame", args)
eval(expr(print(x + 1)), env(x = 1000))
  res <- NULL
randomise <- (function(f) f(runif(1000)))
hist(runif(1e+06))
randomise(mean)
library(purrr)
  }
times_n <- (function(x, n) n * x)
simple_map(1:3, times_n, n = 6)
library(rlang)
})
exp <- 2
square <- power1(exp)
exp <- 3
square(10)
    i
counter_one <- new_counter()
counter_one()
counter_one()
counter_two <- new_counter()
counter_two()
counter_two()
    col <- df[[var]]
boot_mtcars <- bootstrap(mtcars, "mpg")
boot_mtcars()
boot_mtcars()
lprob_poission <- (function(lambda, x) {
x1 <- c(41, 30, 31, 38, 29, 24, 30, 29, 31, 38)
lprob_poission(10, x1)
lprob_poission(20, x1)
lprob_poission(30, x1)
    lprob_poission(lambda, x)
  function(lambda) {
ll1 <- ll_poisson(x1)
ll1(10)
ll1(20)
ll1(30)
ll2 <- ll_poisson_precomp(x1)
ll2(10)
ll2(20)
ll2(30)
rbenchmark::benchmark(optimise(ll1, c(0, 100), maximum = TRUE), optimise(ll2, c(0, 100), maximum = TRUE))
    if (calls) return(i)
sum_w_counter <- count_calls(sum)
sum_w_counter(c(1, 1))
sum_w_counter(c(1, 2))
sum_w_counter(c(1, 3))
sum_w_counter(calls = TRUE)
sum_w_counter(c(1, 4))
sum_w_counter(calls = TRUE)
library(rlang)
library(lobstr)
rlang::expr(mean(x, na.rm = TRUE))
  rlang::enexpr(x)
capture_it(a + b + c)
f <- expr(f(x = 1, y = 2))
f$z <- 3
f
f[[2]] <- NULL
f
f[[1]]
lobstr::ast(f(a, "b"))
lobstr::ast(1 + 2 * 3)
rlang::call2("f", 1, 2, 3) == rlang::expr(f(1, 2, 3))
rlang::call2("+", 1, rlang::call2("*", 2, 3))
xx <- expr(x + x)
yy <- expr(y + y)
expr(!!xx / (!!yy))
base::eval(expr(x + y), env(x = 1, y = 10))
string_math <- (function(x) {
name <- "Daniel"
string_math("Hello "@name)
string_math(("x" * 2 + "-y") * 3)
df <- data.frame(x = 1:5, y = sample(5))
eval(expr(x + y), df)
rlang::eval_tidy(expr(x + y), df)
  eval_tidy(enexpr(expr), df)
with2(df, x + y)
y <- x * 10
z <- rlang::expr(y <- x * 10)
z
x <- 4
eval(z)
y
View(expr(f(x, "y", 1)))
lobstr::ast(f(x, "y", 1))
lobstr::ast(f(g(1, 2), h(3, 4, i())))
lobstr::ast(y <- x * 10)
x <- 10
x1 <- "y <- x + 10"
x1
x2 <- rlang::parse_expr(x1)
x2
eval(x2)
y
    "symbol"
expr_type(expr("a"))
expr_type(expr(x))
expr_type(expr(f(1, 2)))
  switch(expr_type(x), ..., stop("Don't know how to handle type ", typeof(x), call. = FALSE))
  switch_expr(x, symbol = , constant = , call = , pairlist = )
  switch_expr(x, constant = FALSE, symbol = as_string(x) %in% c("F", "T"))
  logical_abbr_rec(enexpr(x))
logical_abbr(T)
logical_abbr(TRUE)
f <- expr(function(x, y = 10) x + y)
args <- f[[2]]
args
typeof(args)
rlang::is_missing(expr())
ccement <- (function(...) {
ccement(hello, world, how, is, it, going)
x <- "hello"
rlang::is_expression(x)
f1 <- (function(x) rlang::expr(x))
f1(a + b + c)
f <- (function(...) rlang::enexprs(...))
f(x = 1, y = 2, z = x * y)
test <- f(x = 1, y = 2, z = x * y)
rlang::is_expression(test$z)
rlang::exprs(x = x^2, y = y^3, z ? (?.))
x <- rlang::sym(x)
rlang::ensym(x)
rlang::ensyms()
  rlang::ensyms(...)
f <- (function(...) as.list(substitute(...())))
f <- (function(x) substitute(x * 2))
f(a + b + c)
substitute(x * y * z, list(x = 10, y = quote(a + b)))
y <- quote(variable)
!!y
expr(!!y)
x <- expr(-1)
expr(f(!!x, y))
a <- sym("y")
b <- 1
expr(f(!!a, !!b))
f <- expr(foo)
expr((!!f)(x, y))
call2(f, expr(x), expr(y))
arg <- rlang::missing_arg()
expr(foo(!!arg, !!arg))
expr(foo(!!maybe_missing(arg)))
})
  z <<- expr(dplyr::filter(iris, !!var == filt))
filter_iris(Species, virginica)
xs <- exprs(1, a, -b)
ys <- set_names(xs, c("a", "b", "c"))
expr(f(!!!ys, d = 4))
xyz <- bquote((x + y + z))
rlang::is_expression(xyz)
bquote(-.(xyz) / 2)
zyx <- rlang::expr(x + y + z)
bquote(-.(zyx) / 2)
mtcars$cyl
library(rlang)
pkg <- "rlang"
library(pkg)
library(pkg, character.only = TRUE)
help(var)
var <- "mean"
help(var)
var <- 10
help(var)
dfs <- list(a = data.frame(x = 1, y = 2), b = data.frame(x = 3, y = 4))
dplyr::bind_rows(!!!dfs)
rbind(exprs(!!!dfs))
})
rbind2(!!!dfs)
rbind2(dfs)
})
rbind3(dfs)
var <- "x"
val <- c(1, 2, 3)
tibble::tibble(!!var := val)
})
attrs <- list(x = 1, y = 2)
attr_name <- "z"
1:10 %>% set_attr(w = 0, !!!attrs, !!attr_name := 3) %>% str()
})
1:10 %>% set_attr(w = 0) %>% set_attr(!!attr_name := 3)
?rlang::`dyn-dots`
exec("mean", x = 1:10, na.rm = TRUE)
args <- list(x = 1:10, na.rm = TRUE, trim = 0.1)
exec("mean", !!!args)
do.call("rbind", dfs)
var <- "x"
val <- c(1, 2, 3)
args <- list(val)
names(args) <- var
do.call("data.frame", args)
eval(expr(print(x + 1)), env(x = 1000))
  for (i in seq_along(exprs)) {
randomise <- (function(f) f(runif(1000)))
hist(runif(1e+06))
randomise(mean)
library(purrr)
  out
times_n <- (function(x, n) n * x)
simple_map(1:3, times_n, n = 6)
library(rlang)
power1 <- (function(exp) {
exp <- 2
square <- power1(exp)
exp <- 3
square(10)
  }
counter_one <- new_counter()
counter_one()
counter_one()
counter_two <- new_counter()
counter_two()
counter_two()
    col[sample(n, replace = TRUE)]
boot_mtcars <- bootstrap(mtcars, "mpg")
boot_mtcars()
boot_mtcars()
  n <- length(x)
x1 <- c(41, 30, 31, 38, 29, 24, 30, 29, 31, 38)
lprob_poission(10, x1)
lprob_poission(20, x1)
lprob_poission(30, x1)
  }
    log(lambda) * sum_x - n * lambda - c
ll1 <- ll_poisson(x1)
ll1(10)
ll1(20)
ll1(30)
ll2 <- ll_poisson_precomp(x1)
ll2(10)
ll2(20)
ll2(30)
rbenchmark::benchmark(optimise(ll1, c(0, 100), maximum = TRUE), optimise(ll2, c(0, 100), maximum = TRUE))
    i <<- i + 1
sum_w_counter <- count_calls(sum)
sum_w_counter(c(1, 1))
sum_w_counter(c(1, 2))
sum_w_counter(c(1, 3))
sum_w_counter(calls = TRUE)
sum_w_counter(c(1, 4))
sum_w_counter(calls = TRUE)
library(rlang)
library(lobstr)
rlang::expr(mean(x, na.rm = TRUE))
})
capture_it(a + b + c)
f <- expr(f(x = 1, y = 2))
f$z <- 3
f
f[[2]] <- NULL
f
f[[1]]
lobstr::ast(f(a, "b"))
lobstr::ast(1 + 2 * 3)
rlang::call2("f", 1, 2, 3) == rlang::expr(f(1, 2, 3))
rlang::call2("+", 1, rlang::call2("*", 2, 3))
xx <- expr(x + x)
yy <- expr(y + y)
expr(!!xx / (!!yy))
base::eval(expr(x + y), env(x = 1, y = 10))
  e <- env(rlang::caller_env(), `+` = function(x, y) paste0(x, y), `*` = function(x, y) strrep(x, y))
name <- "Daniel"
string_math("Hello "@name)
string_math(("x" * 2 + "-y") * 3)
df <- data.frame(x = 1:5, y = sample(5))
eval(expr(x + y), df)
rlang::eval_tidy(expr(x + y), df)
})
with2(df, x + y)
y <- x * 10
z <- rlang::expr(y <- x * 10)
z
x <- 4
eval(z)
y
View(expr(f(x, "y", 1)))
lobstr::ast(f(x, "y", 1))
lobstr::ast(f(g(1, 2), h(3, 4, i())))
lobstr::ast(y <- x * 10)
x <- 10
x1 <- "y <- x + 10"
x1
x2 <- rlang::parse_expr(x1)
x2
eval(x2)
y
  } else if (is.call(x)) {
expr_type(expr("a"))
expr_type(expr(x))
expr_type(expr(f(1, 2)))
})
})
})
})
logical_abbr(T)
logical_abbr(TRUE)
f <- expr(function(x, y = 10) x + y)
args <- f[[2]]
args
typeof(args)
rlang::is_missing(expr())
  elems <- rlang::ensyms(...)
ccement(hello, world, how, is, it, going)
x <- "hello"
rlang::is_expression(x)
f1 <- (function(x) rlang::expr(x))
f1(a + b + c)
f <- (function(...) rlang::enexprs(...))
f(x = 1, y = 2, z = x * y)
test <- f(x = 1, y = 2, z = x * y)
rlang::is_expression(test$z)
rlang::exprs(x = x^2, y = y^3, z ? (?.))
x <- rlang::sym(x)
rlang::ensym(x)
rlang::ensyms()
})
f <- (function(...) as.list(substitute(...())))
f <- (function(x) substitute(x * 2))
f(a + b + c)
substitute(x * y * z, list(x = 10, y = quote(a + b)))
y <- quote(variable)
!!y
expr(!!y)
x <- expr(-1)
expr(f(!!x, y))
a <- sym("y")
b <- 1
expr(f(!!a, !!b))
f <- expr(foo)
expr((!!f)(x, y))
call2(f, expr(x), expr(y))
arg <- rlang::missing_arg()
expr(foo(!!arg, !!arg))
expr(foo(!!maybe_missing(arg)))
get_from_iris <- (function(x) {
  eval(z)
filter_iris(Species, virginica)
xs <- exprs(1, a, -b)
ys <- set_names(xs, c("a", "b", "c"))
expr(f(!!!ys, d = 4))
xyz <- bquote((x + y + z))
rlang::is_expression(xyz)
bquote(-.(xyz) / 2)
zyx <- rlang::expr(x + y + z)
bquote(-.(zyx) / 2)
mtcars$cyl
library(rlang)
pkg <- "rlang"
library(pkg)
library(pkg, character.only = TRUE)
help(var)
var <- "mean"
help(var)
var <- 10
help(var)
dfs <- list(a = data.frame(x = 1, y = 2), b = data.frame(x = 3, y = 4))
dplyr::bind_rows(!!!dfs)
rbind(exprs(!!!dfs))
rbind2 <- (function(...) {
rbind2(!!!dfs)
rbind2(dfs)
rbind3 <- (function(...) {
rbind3(dfs)
var <- "x"
val <- c(1, 2, 3)
tibble::tibble(!!var := val)
set_attr <- (function(.x, ...) {
attrs <- list(x = 1, y = 2)
attr_name <- "z"
1:10 %>% set_attr(w = 0, !!!attrs, !!attr_name := 3) %>% str()
set_attr <- (function(.x, ...) {
1:10 %>% set_attr(w = 0) %>% set_attr(!!attr_name := 3)
?rlang::`dyn-dots`
exec("mean", x = 1:10, na.rm = TRUE)
args <- list(x = 1:10, na.rm = TRUE, trim = 0.1)
exec("mean", !!!args)
do.call("rbind", dfs)
var <- "x"
val <- c(1, 2, 3)
args <- list(val)
names(args) <- var
do.call("data.frame", args)
eval(expr(print(x + 1)), env(x = 1000))
    res <- eval(exprs[[i]], env)
randomise <- (function(f) f(runif(1000)))
hist(runif(1e+06))
randomise(mean)
library(purrr)
})
times_n <- (function(x, n) n * x)
simple_map(1:3, times_n, n = 6)
library(rlang)
  function(x) {
exp <- 2
square <- power1(exp)
exp <- 3
square(10)
})
counter_one <- new_counter()
counter_one()
counter_one()
counter_two <- new_counter()
counter_two()
counter_two()
  }
boot_mtcars <- bootstrap(mtcars, "mpg")
boot_mtcars()
boot_mtcars()
  (log(lambda) * sum(x)) - (n * lambda) - sum(lfactorial(x))
x1 <- c(41, 30, 31, 38, 29, 24, 30, 29, 31, 38)
lprob_poission(10, x1)
lprob_poission(20, x1)
lprob_poission(30, x1)
})
  }
ll1 <- ll_poisson(x1)
ll1(10)
ll1(20)
ll1(30)
ll2 <- ll_poisson_precomp(x1)
ll2(10)
ll2(20)
ll2(30)
rbenchmark::benchmark(optimise(ll1, c(0, 100), maximum = TRUE), optimise(ll2, c(0, 100), maximum = TRUE))
    f(...)
sum_w_counter <- count_calls(sum)
sum_w_counter(c(1, 1))
sum_w_counter(c(1, 2))
sum_w_counter(c(1, 3))
sum_w_counter(calls = TRUE)
sum_w_counter(c(1, 4))
sum_w_counter(calls = TRUE)
library(rlang)
library(lobstr)
rlang::expr(mean(x, na.rm = TRUE))
capture_it <- (function(x) {
capture_it(a + b + c)
f <- expr(f(x = 1, y = 2))
f$z <- 3
f
f[[2]] <- NULL
f
f[[1]]
lobstr::ast(f(a, "b"))
lobstr::ast(1 + 2 * 3)
rlang::call2("f", 1, 2, 3) == rlang::expr(f(1, 2, 3))
rlang::call2("+", 1, rlang::call2("*", 2, 3))
xx <- expr(x + x)
yy <- expr(y + y)
expr(!!xx / (!!yy))
base::eval(expr(x + y), env(x = 1, y = 10))
  eval(enexpr(x), e)
name <- "Daniel"
string_math("Hello "@name)
string_math(("x" * 2 + "-y") * 3)
df <- data.frame(x = 1:5, y = sample(5))
eval(expr(x + y), df)
rlang::eval_tidy(expr(x + y), df)
with2 <- (function(df, expr) {
with2(df, x + y)
y <- x * 10
z <- rlang::expr(y <- x * 10)
z
x <- 4
eval(z)
y
View(expr(f(x, "y", 1)))
lobstr::ast(f(x, "y", 1))
lobstr::ast(f(g(1, 2), h(3, 4, i())))
lobstr::ast(y <- x * 10)
x <- 10
x1 <- "y <- x + 10"
x1
x2 <- rlang::parse_expr(x1)
x2
eval(x2)
y
    "call"
expr_type(expr("a"))
expr_type(expr(x))
expr_type(expr(f(1, 2)))
switch_expr <- (function(x, ...) {
recurse_call <- (function(x) {
logical_abbr_rec <- (function(x) {
logical_abbr <- (function(x) {
logical_abbr(T)
logical_abbr(TRUE)
f <- expr(function(x, y = 10) x + y)
args <- f[[2]]
args
typeof(args)
rlang::is_missing(expr())
  purrr::map(elems, rlang::as_string) %>% as.vector(mode = "character")
ccement(hello, world, how, is, it, going)
x <- "hello"
rlang::is_expression(x)
f1 <- (function(x) rlang::expr(x))
f1(a + b + c)
f <- (function(...) rlang::enexprs(...))
f(x = 1, y = 2, z = x * y)
test <- f(x = 1, y = 2, z = x * y)
rlang::is_expression(test$z)
rlang::exprs(x = x^2, y = y^3, z ? (?.))
x <- rlang::sym(x)
rlang::ensym(x)
rlang::ensyms()
f <- (function(...) {
f <- (function(...) as.list(substitute(...())))
f <- (function(x) substitute(x * 2))
f(a + b + c)
substitute(x * y * z, list(x = 10, y = quote(a + b)))
y <- quote(variable)
!!y
expr(!!y)
x <- expr(-1)
expr(f(!!x, y))
a <- sym("y")
b <- 1
expr(f(!!a, !!b))
f <- expr(foo)
expr((!!f)(x, y))
call2(f, expr(x), expr(y))
arg <- rlang::missing_arg()
expr(foo(!!arg, !!arg))
expr(foo(!!maybe_missing(arg)))
  y <- enexpr(x)
})
filter_iris(Species, virginica)
xs <- exprs(1, a, -b)
ys <- set_names(xs, c("a", "b", "c"))
expr(f(!!!ys, d = 4))
xyz <- bquote((x + y + z))
rlang::is_expression(xyz)
bquote(-.(xyz) / 2)
zyx <- rlang::expr(x + y + z)
bquote(-.(zyx) / 2)
mtcars$cyl
library(rlang)
pkg <- "rlang"
library(pkg)
library(pkg, character.only = TRUE)
help(var)
var <- "mean"
help(var)
var <- 10
help(var)
dfs <- list(a = data.frame(x = 1, y = 2), b = data.frame(x = 3, y = 4))
dplyr::bind_rows(!!!dfs)
rbind(exprs(!!!dfs))
  ldfs <- rlang::list2(...)
rbind2(!!!dfs)
rbind2(dfs)
  dots <- list(...)
rbind3(dfs)
var <- "x"
val <- c(1, 2, 3)
tibble::tibble(!!var := val)
  attr <- rlang::list2(...)
attrs <- list(x = 1, y = 2)
attr_name <- "z"
1:10 %>% set_attr(w = 0, !!!attrs, !!attr_name := 3) %>% str()
  attr <- list(...)
1:10 %>% set_attr(w = 0) %>% set_attr(!!attr_name := 3)
?rlang::`dyn-dots`
exec("mean", x = 1:10, na.rm = TRUE)
args <- list(x = 1:10, na.rm = TRUE, trim = 0.1)
exec("mean", !!!args)
do.call("rbind", dfs)
var <- "x"
val <- c(1, 2, 3)
args <- list(val)
names(args) <- var
do.call("data.frame", args)
eval(expr(print(x + 1)), env(x = 1000))
  }
randomise <- (function(f) f(runif(1000)))
hist(runif(1e+06))
randomise(mean)
library(purrr)
simple_map <- (function(x, f, ...) {
times_n <- (function(x, n) n * x)
simple_map(1:3, times_n, n = 6)
library(rlang)
    x^exp
exp <- 2
square <- power1(exp)
exp <- 3
square(10)
new_counter <- (function() {
counter_one <- new_counter()
counter_one()
counter_one()
counter_two <- new_counter()
counter_two()
counter_two()
})
boot_mtcars <- bootstrap(mtcars, "mpg")
boot_mtcars()
boot_mtcars()
})
x1 <- c(41, 30, 31, 38, 29, 24, 30, 29, 31, 38)
lprob_poission(10, x1)
lprob_poission(20, x1)
lprob_poission(30, x1)
ll_poisson <- (function(x) {
})
ll1 <- ll_poisson(x1)
ll1(10)
ll1(20)
ll1(30)
ll2 <- ll_poisson_precomp(x1)
ll2(10)
ll2(20)
ll2(30)
rbenchmark::benchmark(optimise(ll1, c(0, 100), maximum = TRUE), optimise(ll2, c(0, 100), maximum = TRUE))
  })
sum_w_counter <- count_calls(sum)
sum_w_counter(c(1, 1))
sum_w_counter(c(1, 2))
sum_w_counter(c(1, 3))
sum_w_counter(calls = TRUE)
sum_w_counter(c(1, 4))
sum_w_counter(calls = TRUE)
library(rlang)
library(lobstr)
rlang::expr(mean(x, na.rm = TRUE))
  rlang::enexpr(x)
capture_it(a + b + c)
f <- expr(f(x = 1, y = 2))
f$z <- 3
f
f[[2]] <- NULL
f
f[[1]]
lobstr::ast(f(a, "b"))
lobstr::ast(1 + 2 * 3)
rlang::call2("f", 1, 2, 3) == rlang::expr(f(1, 2, 3))
rlang::call2("+", 1, rlang::call2("*", 2, 3))
xx <- expr(x + x)
yy <- expr(y + y)
expr(!!xx / (!!yy))
base::eval(expr(x + y), env(x = 1, y = 10))
})
name <- "Daniel"
string_math("Hello "@name)
string_math(("x" * 2 + "-y") * 3)
df <- data.frame(x = 1:5, y = sample(5))
eval(expr(x + y), df)
rlang::eval_tidy(expr(x + y), df)
  eval_tidy(enexpr(expr), df)
with2(df, x + y)
y <- x * 10
z <- rlang::expr(y <- x * 10)
z
x <- 4
eval(z)
y
View(expr(f(x, "y", 1)))
lobstr::ast(f(x, "y", 1))
lobstr::ast(f(g(1, 2), h(3, 4, i())))
lobstr::ast(y <- x * 10)
x <- 10
x1 <- "y <- x + 10"
x1
x2 <- rlang::parse_expr(x1)
x2
eval(x2)
y
  } else if (is.pairlist(x)) {
expr_type(expr("a"))
expr_type(expr(x))
expr_type(expr(f(1, 2)))
  switch(expr_type(x), ..., stop("Don't know how to handle type ", typeof(x), call. = FALSE))
  switch_expr(x, symbol = , constant = , call = , pairlist = )
  switch_expr(x, constant = FALSE, symbol = as_string(x) %in% c("F", "T"))
  logical_abbr_rec(enexpr(x))
logical_abbr(T)
logical_abbr(TRUE)
f <- expr(function(x, y = 10) x + y)
args <- f[[2]]
args
typeof(args)
rlang::is_missing(expr())
})
ccement(hello, world, how, is, it, going)
x <- "hello"
rlang::is_expression(x)
f1 <- (function(x) rlang::expr(x))
f1(a + b + c)
f <- (function(...) rlang::enexprs(...))
f(x = 1, y = 2, z = x * y)
test <- f(x = 1, y = 2, z = x * y)
rlang::is_expression(test$z)
rlang::exprs(x = x^2, y = y^3, z ? (?.))
x <- rlang::sym(x)
rlang::ensym(x)
rlang::ensyms()
  rlang::ensyms(...)
f <- (function(...) as.list(substitute(...())))
f <- (function(x) substitute(x * 2))
f(a + b + c)
substitute(x * y * z, list(x = 10, y = quote(a + b)))
y <- quote(variable)
!!y
expr(!!y)
x <- expr(-1)
expr(f(!!x, y))
a <- sym("y")
b <- 1
expr(f(!!a, !!b))
f <- expr(foo)
expr((!!f)(x, y))
call2(f, expr(x), expr(y))
arg <- rlang::missing_arg()
expr(foo(!!arg, !!arg))
expr(foo(!!maybe_missing(arg)))
  z <- expr(iris$(!!y))
filter_iris <- (function(var, filt) {
filter_iris(Species, virginica)
xs <- exprs(1, a, -b)
ys <- set_names(xs, c("a", "b", "c"))
expr(f(!!!ys, d = 4))
xyz <- bquote((x + y + z))
rlang::is_expression(xyz)
bquote(-.(xyz) / 2)
zyx <- rlang::expr(x + y + z)
bquote(-.(zyx) / 2)
mtcars$cyl
library(rlang)
pkg <- "rlang"
library(pkg)
library(pkg, character.only = TRUE)
help(var)
var <- "mean"
help(var)
var <- 10
help(var)
dfs <- list(a = data.frame(x = 1, y = 2), b = data.frame(x = 3, y = 4))
dplyr::bind_rows(!!!dfs)
rbind(exprs(!!!dfs))
  rdf <- purrr::map_df(ldfs, rbind)
rbind2(!!!dfs)
rbind2(dfs)
  rdf <- purrr::map_df(dots, rbind)
rbind3(dfs)
var <- "x"
val <- c(1, 2, 3)
tibble::tibble(!!var := val)
  attributes(.x) <- attr
attrs <- list(x = 1, y = 2)
attr_name <- "z"
1:10 %>% set_attr(w = 0, !!!attrs, !!attr_name := 3) %>% str()
  attributes(.x) <- attr
1:10 %>% set_attr(w = 0) %>% set_attr(!!attr_name := 3)
?rlang::`dyn-dots`
exec("mean", x = 1:10, na.rm = TRUE)
args <- list(x = 1:10, na.rm = TRUE, trim = 0.1)
exec("mean", !!!args)
do.call("rbind", dfs)
var <- "x"
val <- c(1, 2, 3)
args <- list(val)
names(args) <- var
do.call("data.frame", args)
eval(expr(print(x + 1)), env(x = 1000))
  invisible(res)
randomise <- (function(f) f(runif(1000)))
hist(runif(1e+06))
randomise(mean)
library(purrr)
  out <- vector("list", length(x))
times_n <- (function(x, n) n * x)
simple_map(1:3, times_n, n = 6)
library(rlang)
  }
exp <- 2
square <- power1(exp)
exp <- 3
square(10)
  i <- 0
counter_one <- new_counter()
counter_one()
counter_one()
counter_two <- new_counter()
counter_two()
counter_two()
bootstrap <- (function(df, var) {
boot_mtcars <- bootstrap(mtcars, "mpg")
boot_mtcars()
boot_mtcars()
lprob_poission <- (function(lambda, x) {
x1 <- c(41, 30, 31, 38, 29, 24, 30, 29, 31, 38)
lprob_poission(10, x1)
lprob_poission(20, x1)
lprob_poission(30, x1)
  n <- length(x)
ll_poisson_precomp <- (function(x) {
ll1 <- ll_poisson(x1)
ll1(10)
ll1(20)
ll1(30)
ll2 <- ll_poisson_precomp(x1)
ll2(10)
ll2(20)
ll2(30)
rbenchmark::benchmark(optimise(ll1, c(0, 100), maximum = TRUE), optimise(ll2, c(0, 100), maximum = TRUE))
  return(wrapper)
sum_w_counter <- count_calls(sum)
sum_w_counter(c(1, 1))
sum_w_counter(c(1, 2))
sum_w_counter(c(1, 3))
sum_w_counter(calls = TRUE)
sum_w_counter(c(1, 4))
sum_w_counter(calls = TRUE)
library(rlang)
library(lobstr)
rlang::expr(mean(x, na.rm = TRUE))
})
capture_it(a + b + c)
f <- expr(f(x = 1, y = 2))
f$z <- 3
f
f[[2]] <- NULL
f
f[[1]]
lobstr::ast(f(a, "b"))
lobstr::ast(1 + 2 * 3)
rlang::call2("f", 1, 2, 3) == rlang::expr(f(1, 2, 3))
rlang::call2("+", 1, rlang::call2("*", 2, 3))
xx <- expr(x + x)
yy <- expr(y + y)
expr(!!xx / (!!yy))
base::eval(expr(x + y), env(x = 1, y = 10))
string_math <- (function(x) {
name <- "Daniel"
string_math("Hello "@name)
string_math(("x" * 2 + "-y") * 3)
df <- data.frame(x = 1:5, y = sample(5))
eval(expr(x + y), df)
rlang::eval_tidy(expr(x + y), df)
})
with2(df, x + y)
y <- x * 10
z <- rlang::expr(y <- x * 10)
z
x <- 4
eval(z)
y
View(expr(f(x, "y", 1)))
lobstr::ast(f(x, "y", 1))
lobstr::ast(f(g(1, 2), h(3, 4, i())))
lobstr::ast(y <- x * 10)
x <- 10
x1 <- "y <- x + 10"
x1
x2 <- rlang::parse_expr(x1)
x2
eval(x2)
y
    "pairlist"
expr_type(expr("a"))
expr_type(expr(x))
expr_type(expr(f(1, 2)))
})
})
})
})
logical_abbr(T)
logical_abbr(TRUE)
f <- expr(function(x, y = 10) x + y)
args <- f[[2]]
args
typeof(args)
rlang::is_missing(expr())
ccement <- (function(...) {
ccement(hello, world, how, is, it, going)
x <- "hello"
rlang::is_expression(x)
f1 <- (function(x) rlang::expr(x))
f1(a + b + c)
f <- (function(...) rlang::enexprs(...))
f(x = 1, y = 2, z = x * y)
test <- f(x = 1, y = 2, z = x * y)
rlang::is_expression(test$z)
rlang::exprs(x = x^2, y = y^3, z ? (?.))
x <- rlang::sym(x)
rlang::ensym(x)
rlang::ensyms()
})
f <- (function(...) as.list(substitute(...())))
f <- (function(x) substitute(x * 2))
f(a + b + c)
substitute(x * y * z, list(x = 10, y = quote(a + b)))
y <- quote(variable)
!!y
expr(!!y)
x <- expr(-1)
expr(f(!!x, y))
a <- sym("y")
b <- 1
expr(f(!!a, !!b))
f <- expr(foo)
expr((!!f)(x, y))
call2(f, expr(x), expr(y))
arg <- rlang::missing_arg()
expr(foo(!!arg, !!arg))
expr(foo(!!maybe_missing(arg)))
  eval(z)
  var <- enexpr(var)
filter_iris(Species, virginica)
xs <- exprs(1, a, -b)
ys <- set_names(xs, c("a", "b", "c"))
expr(f(!!!ys, d = 4))
xyz <- bquote((x + y + z))
rlang::is_expression(xyz)
bquote(-.(xyz) / 2)
zyx <- rlang::expr(x + y + z)
bquote(-.(zyx) / 2)
mtcars$cyl
library(rlang)
pkg <- "rlang"
library(pkg)
library(pkg, character.only = TRUE)
help(var)
var <- "mean"
help(var)
var <- 10
help(var)
dfs <- list(a = data.frame(x = 1, y = 2), b = data.frame(x = 3, y = 4))
dplyr::bind_rows(!!!dfs)
rbind(exprs(!!!dfs))
  rdf
rbind2(!!!dfs)
rbind2(dfs)
  rdf
rbind3(dfs)
var <- "x"
val <- c(1, 2, 3)
tibble::tibble(!!var := val)
  .x
attrs <- list(x = 1, y = 2)
attr_name <- "z"
1:10 %>% set_attr(w = 0, !!!attrs, !!attr_name := 3) %>% str()
  .x
1:10 %>% set_attr(w = 0) %>% set_attr(!!attr_name := 3)
?rlang::`dyn-dots`
exec("mean", x = 1:10, na.rm = TRUE)
args <- list(x = 1:10, na.rm = TRUE, trim = 0.1)
exec("mean", !!!args)
do.call("rbind", dfs)
var <- "x"
val <- c(1, 2, 3)
args <- list(val)
names(args) <- var
do.call("data.frame", args)
eval(expr(print(x + 1)), env(x = 1000))
})
randomise <- (function(f) f(runif(1000)))
hist(runif(1e+06))
randomise(mean)
library(purrr)
  for (i in seq_along(x)) {
times_n <- (function(x, n) n * x)
simple_map(1:3, times_n, n = 6)
library(rlang)
})
exp <- 2
square <- power1(exp)
exp <- 3
square(10)
  function() {
counter_one <- new_counter()
counter_one()
counter_one()
counter_two <- new_counter()
counter_two()
counter_two()
  n <- nrow(df)
boot_mtcars <- bootstrap(mtcars, "mpg")
boot_mtcars()
boot_mtcars()
  n <- length(x)
x1 <- c(41, 30, 31, 38, 29, 24, 30, 29, 31, 38)
lprob_poission(10, x1)
lprob_poission(20, x1)
lprob_poission(30, x1)
  force(x)
  n <- length(x)
ll1 <- ll_poisson(x1)
ll1(10)
ll1(20)
ll1(30)
ll2 <- ll_poisson_precomp(x1)
ll2(10)
ll2(20)
ll2(30)
rbenchmark::benchmark(optimise(ll1, c(0, 100), maximum = TRUE), optimise(ll2, c(0, 100), maximum = TRUE))
})
sum_w_counter <- count_calls(sum)
sum_w_counter(c(1, 1))
sum_w_counter(c(1, 2))
sum_w_counter(c(1, 3))
sum_w_counter(calls = TRUE)
sum_w_counter(c(1, 4))
sum_w_counter(calls = TRUE)
library(rlang)
library(lobstr)
rlang::expr(mean(x, na.rm = TRUE))
capture_it <- (function(x) {
capture_it(a + b + c)
f <- expr(f(x = 1, y = 2))
f$z <- 3
f
f[[2]] <- NULL
f
f[[1]]
lobstr::ast(f(a, "b"))
lobstr::ast(1 + 2 * 3)
rlang::call2("f", 1, 2, 3) == rlang::expr(f(1, 2, 3))
rlang::call2("+", 1, rlang::call2("*", 2, 3))
xx <- expr(x + x)
yy <- expr(y + y)
expr(!!xx / (!!yy))
base::eval(expr(x + y), env(x = 1, y = 10))
  e <- env(rlang::caller_env(), `+` = function(x, y) paste0(x, y), `*` = function(x, y) strrep(x, y))
name <- "Daniel"
string_math("Hello "@name)
string_math(("x" * 2 + "-y") * 3)
df <- data.frame(x = 1:5, y = sample(5))
eval(expr(x + y), df)
rlang::eval_tidy(expr(x + y), df)
with2 <- (function(df, expr) {
with2(df, x + y)
y <- x * 10
z <- rlang::expr(y <- x * 10)
z
x <- 4
eval(z)
y
View(expr(f(x, "y", 1)))
lobstr::ast(f(x, "y", 1))
lobstr::ast(f(g(1, 2), h(3, 4, i())))
lobstr::ast(y <- x * 10)
x <- 10
x1 <- "y <- x + 10"
x1
x2 <- rlang::parse_expr(x1)
x2
eval(x2)
y
  } else {
expr_type(expr("a"))
expr_type(expr(x))
expr_type(expr(f(1, 2)))
switch_expr <- (function(x, ...) {
recurse_call <- (function(x) {
logical_abbr_rec <- (function(x) {
logical_abbr <- (function(x) {
logical_abbr(T)
logical_abbr(TRUE)
f <- expr(function(x, y = 10) x + y)
args <- f[[2]]
args
typeof(args)
rlang::is_missing(expr())
  elems <- rlang::ensyms(...)
ccement(hello, world, how, is, it, going)
x <- "hello"
rlang::is_expression(x)
f1 <- (function(x) rlang::expr(x))
f1(a + b + c)
f <- (function(...) rlang::enexprs(...))
f(x = 1, y = 2, z = x * y)
test <- f(x = 1, y = 2, z = x * y)
rlang::is_expression(test$z)
rlang::exprs(x = x^2, y = y^3, z ? (?.))
x <- rlang::sym(x)
rlang::ensym(x)
rlang::ensyms()
f <- (function(...) {
f <- (function(...) as.list(substitute(...())))
f <- (function(x) substitute(x * 2))
f(a + b + c)
substitute(x * y * z, list(x = 10, y = quote(a + b)))
y <- quote(variable)
!!y
expr(!!y)
x <- expr(-1)
expr(f(!!x, y))
a <- sym("y")
b <- 1
expr(f(!!a, !!b))
f <- expr(foo)
expr((!!f)(x, y))
call2(f, expr(x), expr(y))
arg <- rlang::missing_arg()
expr(foo(!!arg, !!arg))
expr(foo(!!maybe_missing(arg)))
})
  filt <- enexpr(filt)
filter_iris(Species, virginica)
xs <- exprs(1, a, -b)
ys <- set_names(xs, c("a", "b", "c"))
expr(f(!!!ys, d = 4))
xyz <- bquote((x + y + z))
rlang::is_expression(xyz)
bquote(-.(xyz) / 2)
zyx <- rlang::expr(x + y + z)
bquote(-.(zyx) / 2)
mtcars$cyl
library(rlang)
pkg <- "rlang"
library(pkg)
library(pkg, character.only = TRUE)
help(var)
var <- "mean"
help(var)
var <- 10
help(var)
dfs <- list(a = data.frame(x = 1, y = 2), b = data.frame(x = 3, y = 4))
dplyr::bind_rows(!!!dfs)
rbind(exprs(!!!dfs))
})
rbind2(!!!dfs)
rbind2(dfs)
})
rbind3(dfs)
var <- "x"
val <- c(1, 2, 3)
tibble::tibble(!!var := val)
})
attrs <- list(x = 1, y = 2)
attr_name <- "z"
1:10 %>% set_attr(w = 0, !!!attrs, !!attr_name := 3) %>% str()
})
1:10 %>% set_attr(w = 0) %>% set_attr(!!attr_name := 3)
?rlang::`dyn-dots`
exec("mean", x = 1:10, na.rm = TRUE)
args <- list(x = 1:10, na.rm = TRUE, trim = 0.1)
exec("mean", !!!args)
do.call("rbind", dfs)
var <- "x"
val <- c(1, 2, 3)
args <- list(val)
names(args) <- var
do.call("data.frame", args)
eval(expr(print(x + 1)), env(x = 1000))
source2 <- (function(path, env = caller_env()) {
randomise <- (function(f) f(runif(1000)))
hist(runif(1e+06))
randomise(mean)
library(purrr)
    out[[i]] <- f(x[[i]], ...)
times_n <- (function(x, n) n * x)
simple_map(1:3, times_n, n = 6)
library(rlang)
power1 <- (function(exp) {
exp <- 2
square <- power1(exp)
exp <- 3
square(10)
    i <<- i + 1
counter_one <- new_counter()
counter_one()
counter_one()
counter_two <- new_counter()
counter_two()
counter_two()
  force(var)
boot_mtcars <- bootstrap(mtcars, "mpg")
boot_mtcars()
boot_mtcars()
  (log(lambda) * sum(x)) - (n * lambda) - sum(lfactorial(x))
x1 <- c(41, 30, 31, 38, 29, 24, 30, 29, 31, 38)
lprob_poission(10, x1)
lprob_poission(20, x1)
lprob_poission(30, x1)
  function(lambda) {
  sum_x <- sum(x)
ll1 <- ll_poisson(x1)
ll1(10)
ll1(20)
ll1(30)
ll2 <- ll_poisson_precomp(x1)
ll2(10)
ll2(20)
ll2(30)
rbenchmark::benchmark(optimise(ll1, c(0, 100), maximum = TRUE), optimise(ll2, c(0, 100), maximum = TRUE))
count_calls <- (function(f) {
sum_w_counter <- count_calls(sum)
sum_w_counter(c(1, 1))
sum_w_counter(c(1, 2))
sum_w_counter(c(1, 3))
sum_w_counter(calls = TRUE)
sum_w_counter(c(1, 4))
sum_w_counter(calls = TRUE)
library(rlang)
library(lobstr)
rlang::expr(mean(x, na.rm = TRUE))
  rlang::enexpr(x)
capture_it(a + b + c)
f <- expr(f(x = 1, y = 2))
f$z <- 3
f
f[[2]] <- NULL
f
f[[1]]
lobstr::ast(f(a, "b"))
lobstr::ast(1 + 2 * 3)
rlang::call2("f", 1, 2, 3) == rlang::expr(f(1, 2, 3))
rlang::call2("+", 1, rlang::call2("*", 2, 3))
xx <- expr(x + x)
yy <- expr(y + y)
expr(!!xx / (!!yy))
base::eval(expr(x + y), env(x = 1, y = 10))
  eval(enexpr(x), e)
name <- "Daniel"
string_math("Hello "@name)
string_math(("x" * 2 + "-y") * 3)
df <- data.frame(x = 1:5, y = sample(5))
eval(expr(x + y), df)
rlang::eval_tidy(expr(x + y), df)
  eval_tidy(enexpr(expr), df)
with2(df, x + y)
y <- x * 10
z <- rlang::expr(y <- x * 10)
z
x <- 4
eval(z)
y
View(expr(f(x, "y", 1)))
lobstr::ast(f(x, "y", 1))
lobstr::ast(f(g(1, 2), h(3, 4, i())))
lobstr::ast(y <- x * 10)
x <- 10
x1 <- "y <- x + 10"
x1
x2 <- rlang::parse_expr(x1)
x2
eval(x2)
y
    typeof(x)
expr_type(expr("a"))
expr_type(expr(x))
expr_type(expr(f(1, 2)))
  switch(expr_type(x), ..., stop("Don't know how to handle type ", typeof(x), call. = FALSE))
  switch_expr(x, symbol = , constant = , call = , pairlist = )
  switch_expr(x, constant = FALSE, symbol = as_string(x) %in% c("F", "T"))
  logical_abbr_rec(enexpr(x))
logical_abbr(T)
logical_abbr(TRUE)
f <- expr(function(x, y = 10) x + y)
args <- f[[2]]
args
typeof(args)
rlang::is_missing(expr())
  purrr::map(elems, rlang::as_string) %>% as.vector(mode = "character")
ccement(hello, world, how, is, it, going)
x <- "hello"
rlang::is_expression(x)
f1 <- (function(x) rlang::expr(x))
f1(a + b + c)
f <- (function(...) rlang::enexprs(...))
f(x = 1, y = 2, z = x * y)
test <- f(x = 1, y = 2, z = x * y)
rlang::is_expression(test$z)
rlang::exprs(x = x^2, y = y^3, z ? (?.))
x <- rlang::sym(x)
rlang::ensym(x)
rlang::ensyms()
  rlang::ensyms(...)
f <- (function(...) as.list(substitute(...())))
f <- (function(x) substitute(x * 2))
f(a + b + c)
substitute(x * y * z, list(x = 10, y = quote(a + b)))
y <- quote(variable)
!!y
expr(!!y)
x <- expr(-1)
expr(f(!!x, y))
a <- sym("y")
b <- 1
expr(f(!!a, !!b))
f <- expr(foo)
expr((!!f)(x, y))
call2(f, expr(x), expr(y))
arg <- rlang::missing_arg()
expr(foo(!!arg, !!arg))
expr(foo(!!maybe_missing(arg)))
get_from_iris <- (function(x) {
  filt <- rlang::as_string(filt)
filter_iris(Species, virginica)
xs <- exprs(1, a, -b)
ys <- set_names(xs, c("a", "b", "c"))
expr(f(!!!ys, d = 4))
xyz <- bquote((x + y + z))
rlang::is_expression(xyz)
bquote(-.(xyz) / 2)
zyx <- rlang::expr(x + y + z)
bquote(-.(zyx) / 2)
mtcars$cyl
library(rlang)
pkg <- "rlang"
library(pkg)
library(pkg, character.only = TRUE)
help(var)
var <- "mean"
help(var)
var <- 10
help(var)
dfs <- list(a = data.frame(x = 1, y = 2), b = data.frame(x = 3, y = 4))
dplyr::bind_rows(!!!dfs)
rbind(exprs(!!!dfs))
rbind2 <- (function(...) {
rbind2(!!!dfs)
rbind2(dfs)
rbind3 <- (function(...) {
rbind3(dfs)
var <- "x"
val <- c(1, 2, 3)
tibble::tibble(!!var := val)
set_attr <- (function(.x, ...) {
attrs <- list(x = 1, y = 2)
attr_name <- "z"
1:10 %>% set_attr(w = 0, !!!attrs, !!attr_name := 3) %>% str()
set_attr <- (function(.x, ...) {
1:10 %>% set_attr(w = 0) %>% set_attr(!!attr_name := 3)
?rlang::`dyn-dots`
exec("mean", x = 1:10, na.rm = TRUE)
args <- list(x = 1:10, na.rm = TRUE, trim = 0.1)
exec("mean", !!!args)
do.call("rbind", dfs)
var <- "x"
val <- c(1, 2, 3)
args <- list(val)
names(args) <- var
do.call("data.frame", args)
eval(expr(print(x + 1)), env(x = 1000))
  file <- paste(readLines(path, warn = FALSE), collapse = "\n")
randomise <- (function(f) f(runif(1000)))
hist(runif(1e+06))
randomise(mean)
library(purrr)
  }
times_n <- (function(x, n) n * x)
simple_map(1:3, times_n, n = 6)
library(rlang)
  function(x) {
exp <- 2
square <- power1(exp)
exp <- 3
square(10)
    i
counter_one <- new_counter()
counter_one()
counter_one()
counter_two <- new_counter()
counter_two()
counter_two()
  function() {
boot_mtcars <- bootstrap(mtcars, "mpg")
boot_mtcars()
boot_mtcars()
})
x1 <- c(41, 30, 31, 38, 29, 24, 30, 29, 31, 38)
lprob_poission(10, x1)
lprob_poission(20, x1)
lprob_poission(30, x1)
    lprob_poission(lambda, x)
  c <- sum(lfactorial(x))
ll1 <- ll_poisson(x1)
ll1(10)
ll1(20)
ll1(30)
ll2 <- ll_poisson_precomp(x1)
ll2(10)
ll2(20)
ll2(30)
rbenchmark::benchmark(optimise(ll1, c(0, 100), maximum = TRUE), optimise(ll2, c(0, 100), maximum = TRUE))
  force(f)
sum_w_counter <- count_calls(sum)
sum_w_counter(c(1, 1))
sum_w_counter(c(1, 2))
sum_w_counter(c(1, 3))
sum_w_counter(calls = TRUE)
sum_w_counter(c(1, 4))
sum_w_counter(calls = TRUE)
library(rlang)
library(lobstr)
rlang::expr(mean(x, na.rm = TRUE))
})
capture_it(a + b + c)
f <- expr(f(x = 1, y = 2))
f$z <- 3
f
f[[2]] <- NULL
f
f[[1]]
lobstr::ast(f(a, "b"))
lobstr::ast(1 + 2 * 3)
rlang::call2("f", 1, 2, 3) == rlang::expr(f(1, 2, 3))
rlang::call2("+", 1, rlang::call2("*", 2, 3))
xx <- expr(x + x)
yy <- expr(y + y)
expr(!!xx / (!!yy))
base::eval(expr(x + y), env(x = 1, y = 10))
})
name <- "Daniel"
string_math("Hello "@name)
string_math(("x" * 2 + "-y") * 3)
df <- data.frame(x = 1:5, y = sample(5))
eval(expr(x + y), df)
rlang::eval_tidy(expr(x + y), df)
})
with2(df, x + y)
y <- x * 10
z <- rlang::expr(y <- x * 10)
z
x <- 4
eval(z)
y
View(expr(f(x, "y", 1)))
lobstr::ast(f(x, "y", 1))
lobstr::ast(f(g(1, 2), h(3, 4, i())))
lobstr::ast(y <- x * 10)
x <- 10
x1 <- "y <- x + 10"
x1
x2 <- rlang::parse_expr(x1)
x2
eval(x2)
y
  }
expr_type(expr("a"))
expr_type(expr(x))
expr_type(expr(f(1, 2)))
})
})
})
})
logical_abbr(T)
logical_abbr(TRUE)
f <- expr(function(x, y = 10) x + y)
args <- f[[2]]
args
typeof(args)
rlang::is_missing(expr())
})
ccement(hello, world, how, is, it, going)
x <- "hello"
rlang::is_expression(x)
f1 <- (function(x) rlang::expr(x))
f1(a + b + c)
f <- (function(...) rlang::enexprs(...))
f(x = 1, y = 2, z = x * y)
test <- f(x = 1, y = 2, z = x * y)
rlang::is_expression(test$z)
rlang::exprs(x = x^2, y = y^3, z ? (?.))
x <- rlang::sym(x)
rlang::ensym(x)
rlang::ensyms()
})
f <- (function(...) as.list(substitute(...())))
f <- (function(x) substitute(x * 2))
f(a + b + c)
substitute(x * y * z, list(x = 10, y = quote(a + b)))
y <- quote(variable)
!!y
expr(!!y)
x <- expr(-1)
expr(f(!!x, y))
a <- sym("y")
b <- 1
expr(f(!!a, !!b))
f <- expr(foo)
expr((!!f)(x, y))
call2(f, expr(x), expr(y))
arg <- rlang::missing_arg()
expr(foo(!!arg, !!arg))
expr(foo(!!maybe_missing(arg)))
  y <- enexpr(x)
  z <<- expr(dplyr::filter(iris, !!var == filt))
filter_iris(Species, virginica)
xs <- exprs(1, a, -b)
ys <- set_names(xs, c("a", "b", "c"))
expr(f(!!!ys, d = 4))
xyz <- bquote((x + y + z))
rlang::is_expression(xyz)
bquote(-.(xyz) / 2)
zyx <- rlang::expr(x + y + z)
bquote(-.(zyx) / 2)
mtcars$cyl
library(rlang)
pkg <- "rlang"
library(pkg)
library(pkg, character.only = TRUE)
help(var)
var <- "mean"
help(var)
var <- 10
help(var)
dfs <- list(a = data.frame(x = 1, y = 2), b = data.frame(x = 3, y = 4))
dplyr::bind_rows(!!!dfs)
rbind(exprs(!!!dfs))
  ldfs <- rlang::list2(...)
rbind2(!!!dfs)
rbind2(dfs)
  dots <- list(...)
rbind3(dfs)
var <- "x"
val <- c(1, 2, 3)
tibble::tibble(!!var := val)
  attr <- rlang::list2(...)
attrs <- list(x = 1, y = 2)
attr_name <- "z"
1:10 %>% set_attr(w = 0, !!!attrs, !!attr_name := 3) %>% str()
  attr <- list(...)
1:10 %>% set_attr(w = 0) %>% set_attr(!!attr_name := 3)
?rlang::`dyn-dots`
exec("mean", x = 1:10, na.rm = TRUE)
args <- list(x = 1:10, na.rm = TRUE, trim = 0.1)
exec("mean", !!!args)
do.call("rbind", dfs)
var <- "x"
val <- c(1, 2, 3)
args <- list(val)
names(args) <- var
do.call("data.frame", args)
eval(expr(print(x + 1)), env(x = 1000))
  exprs <- rlang::parse_exprs(file)
randomise <- (function(f) f(runif(1000)))
hist(runif(1e+06))
randomise(mean)
library(purrr)
  out
times_n <- (function(x, n) n * x)
simple_map(1:3, times_n, n = 6)
library(rlang)
    x^exp
exp <- 2
square <- power1(exp)
exp <- 3
square(10)
  }
counter_one <- new_counter()
counter_one()
counter_one()
counter_two <- new_counter()
counter_two()
counter_two()
    col <- df[[var]]
boot_mtcars <- bootstrap(mtcars, "mpg")
boot_mtcars()
boot_mtcars()
lprob_poission <- (function(lambda, x) {
x1 <- c(41, 30, 31, 38, 29, 24, 30, 29, 31, 38)
lprob_poission(10, x1)
lprob_poission(20, x1)
lprob_poission(30, x1)
  }
  function(lambda) {
ll1 <- ll_poisson(x1)
ll1(10)
ll1(20)
ll1(30)
ll2 <- ll_poisson_precomp(x1)
ll2(10)
ll2(20)
ll2(30)
rbenchmark::benchmark(optimise(ll1, c(0, 100), maximum = TRUE), optimise(ll2, c(0, 100), maximum = TRUE))
  i <- 0
sum_w_counter <- count_calls(sum)
sum_w_counter(c(1, 1))
sum_w_counter(c(1, 2))
sum_w_counter(c(1, 3))
sum_w_counter(calls = TRUE)
sum_w_counter(c(1, 4))
sum_w_counter(calls = TRUE)
library(rlang)
library(lobstr)
rlang::expr(mean(x, na.rm = TRUE))
capture_it <- (function(x) {
capture_it(a + b + c)
f <- expr(f(x = 1, y = 2))
f$z <- 3
f
f[[2]] <- NULL
f
f[[1]]
lobstr::ast(f(a, "b"))
lobstr::ast(1 + 2 * 3)
rlang::call2("f", 1, 2, 3) == rlang::expr(f(1, 2, 3))
rlang::call2("+", 1, rlang::call2("*", 2, 3))
xx <- expr(x + x)
yy <- expr(y + y)
expr(!!xx / (!!yy))
base::eval(expr(x + y), env(x = 1, y = 10))
string_math <- (function(x) {
name <- "Daniel"
string_math("Hello "@name)
string_math(("x" * 2 + "-y") * 3)
df <- data.frame(x = 1:5, y = sample(5))
eval(expr(x + y), df)
rlang::eval_tidy(expr(x + y), df)
with2 <- (function(df, expr) {
with2(df, x + y)
y <- x * 10
z <- rlang::expr(y <- x * 10)
z
x <- 4
eval(z)
y
View(expr(f(x, "y", 1)))
lobstr::ast(f(x, "y", 1))
lobstr::ast(f(g(1, 2), h(3, 4, i())))
lobstr::ast(y <- x * 10)
x <- 10
x1 <- "y <- x + 10"
x1
x2 <- rlang::parse_expr(x1)
x2
eval(x2)
y
})
expr_type(expr("a"))
expr_type(expr(x))
expr_type(expr(f(1, 2)))
switch_expr <- (function(x, ...) {
recurse_call <- (function(x) {
logical_abbr_rec <- (function(x) {
logical_abbr <- (function(x) {
logical_abbr(T)
logical_abbr(TRUE)
f <- expr(function(x, y = 10) x + y)
args <- f[[2]]
args
typeof(args)
rlang::is_missing(expr())
ccement <- (function(...) {
ccement(hello, world, how, is, it, going)
x <- "hello"
rlang::is_expression(x)
f1 <- (function(x) rlang::expr(x))
f1(a + b + c)
f <- (function(...) rlang::enexprs(...))
f(x = 1, y = 2, z = x * y)
test <- f(x = 1, y = 2, z = x * y)
rlang::is_expression(test$z)
rlang::exprs(x = x^2, y = y^3, z ? (?.))
x <- rlang::sym(x)
rlang::ensym(x)
rlang::ensyms()
f <- (function(...) {
f <- (function(...) as.list(substitute(...())))
f <- (function(x) substitute(x * 2))
f(a + b + c)
substitute(x * y * z, list(x = 10, y = quote(a + b)))
y <- quote(variable)
!!y
expr(!!y)
x <- expr(-1)
expr(f(!!x, y))
a <- sym("y")
b <- 1
expr(f(!!a, !!b))
f <- expr(foo)
expr((!!f)(x, y))
call2(f, expr(x), expr(y))
arg <- rlang::missing_arg()
expr(foo(!!arg, !!arg))
expr(foo(!!maybe_missing(arg)))
  z <- expr(iris$(!!y))
  eval(z)
filter_iris(Species, virginica)
xs <- exprs(1, a, -b)
ys <- set_names(xs, c("a", "b", "c"))
expr(f(!!!ys, d = 4))
xyz <- bquote((x + y + z))
rlang::is_expression(xyz)
bquote(-.(xyz) / 2)
zyx <- rlang::expr(x + y + z)
bquote(-.(zyx) / 2)
mtcars$cyl
library(rlang)
pkg <- "rlang"
library(pkg)
library(pkg, character.only = TRUE)
help(var)
var <- "mean"
help(var)
var <- 10
help(var)
dfs <- list(a = data.frame(x = 1, y = 2), b = data.frame(x = 3, y = 4))
dplyr::bind_rows(!!!dfs)
rbind(exprs(!!!dfs))
  rdf <- purrr::map_df(ldfs, rbind)
rbind2(!!!dfs)
rbind2(dfs)
  rdf <- purrr::map_df(dots, rbind)
rbind3(dfs)
var <- "x"
val <- c(1, 2, 3)
tibble::tibble(!!var := val)
  attributes(.x) <- attr
attrs <- list(x = 1, y = 2)
attr_name <- "z"
1:10 %>% set_attr(w = 0, !!!attrs, !!attr_name := 3) %>% str()
  attributes(.x) <- attr
1:10 %>% set_attr(w = 0) %>% set_attr(!!attr_name := 3)
?rlang::`dyn-dots`
exec("mean", x = 1:10, na.rm = TRUE)
args <- list(x = 1:10, na.rm = TRUE, trim = 0.1)
exec("mean", !!!args)
do.call("rbind", dfs)
var <- "x"
val <- c(1, 2, 3)
args <- list(val)
names(args) <- var
do.call("data.frame", args)
eval(expr(print(x + 1)), env(x = 1000))
  res <- NULL
