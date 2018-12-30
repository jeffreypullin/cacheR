#' Utility to evalulate text as an expression
#'
#' Evaluate text as an expression in a given environment
#'
#' @param text a string which can be evaluated as an expression
#' @param env an environment
#'
#' @return The result of running \code{text} as an expression in \code{env}
#'
#' @importFrom rlang parse_expr
#' @importFrom rlang eval_bare
eval_text <- function(text, env){
  expr <- rlang::parse_expr(text)
  res <- rlang::eval_bare(expr, env = env)
  res
}

#' Utility to split a string and trim all whitespace
#'
#' Split a string and trim all the resulting substrings
#'
#' @param string a string
#' @param pattern a string which string can be split on
#' @param n number of substring string should be split into
#'
#' @return A character vector of the substring of \code{string} split on
#' \code{pattern} with all whitespace removed (from both sides)
#'
#' @importFrom rlang parse_expr
#' @importFrom rlang eval_bare
str_split_trim <- function(string, pattern, n = Inf){
  split <- stringr::str_split(string, pattern, n = n)
  out <- stringr::str_trim(split[[1]])
  out
}

