#' Parse a fucntion call into it's constituent parts
#'
#' Parse a function call/assignment into text pieces - function name, argument
#' names and variable name as well as the environment it has been called in
#'
#' @importFrom rlang quo_get_env
#' @importFrom rlang quo_get_expr
#' @importFrom rlang expr_text
#'
#' @param q function call/assignment of the form f <- g()
#'
#' @return List of: enviroment the call was made and (all as text) the call
#' itself, name of the variable being assigned to, name of the function, name
#' of the arguments the function takes
parse_call <- function(q){

  # get the enviroment
  env <- rlang::quo_get_env(q)

  # get the expression and convert to text
  expr <- rlang::quo_get_expr(q)
  text <- rlang::expr_text(expr)

  # break the call up into it's different elements
  call_elements <- parse_text(text)

  # return the elements and the environment
  out <- c(env = env, call_elements)
  out
}

#' Parse the text of the function call/assignment into it's constituent parts
#'
#' Parses the text of a function call, breaking it into the constituent parts
#' needed to test whether the call has changed
#'
#' @importFrom stringr str_detect
#' @importFrom stringr str_sub
#' @importFrom stringr str_length
#'
#' @param text the text of the function call/assignment
#'
#' @return A list of (all as text) the whole call, the function name, the
#' variable name, the names of the arguments to the function
parse_text <- function(text){

  # check the expr is of a valid form
  if (!stringr::str_detect(text, "<-")) {
    stop("Call must be of the form bar <- foo().",
          "Assignment with = is currently not supported")
  }

  # split into variable and function call
  split_expr <- str_split_trim(text, "<-")
  var <- split_expr[[1]]
  f_call <- split_expr[[2]]

  # split the function into name and arguments
  split_f <- str_split_trim(f_call, "\\(", n = 2)
  f_name <- split_f[[1]]
  # remove the last bracket to clean the arguements
  f_args <- stringr::str_sub(split_f[[2]], 1, stringr::str_length(split_f[[2]])-1)

  # break into individual arguments seperating on ,
  f_args <- str_split_trim(f_args, "(?<!\\(),(?![\\w\\s]*[\\)])")
  # if the argument is of the form arg=var split it and keep var
  for (i in 1:length(f_args)){
    if (stringr::str_detect(f_args[[i]], "=")){
      f_args[[i]] <- str_split_trim(f_args[[i]], "=", n = 2)[[2]]
    }
  }
  out <- list(call = text, v_name = var, f_name = f_name, f_args = f_args)

  out
}
