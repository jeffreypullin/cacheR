#' Cache a function call
#'
#' Provides a convinient method to cache the results of slowing running
#' functions. \code{cache} will detect if the inputs to a function or the
#' function itself changes and will automatically rerun the function and
#' re-cache in this instance.
#'
#' @importFrom rlang enquo
#'
#' @param input expression of the form g <- f()
#' @param cache_dir directory which the cached files should be saved in
#'
#' @details The function itself it's arguments are parsed from the function
#' call and run using rlang. The results are cached and compared to the parmeters
#' each time the function is run. The caching using savind in external rds files.
#' In the cache directory each cache will have a subdirectory named 'g' (the name)
#' of the object being assigned to. In this subdirectory will be two files:
#' 'call.rds' which contains the function and parameters and 'object.rds' which
#' contains the object created by the slow running computation.
#'
#' @examples
#' \dontrun{
#' # define a 'slow running' function with external parameters
#' N <- 100
#' K <- 42
#' simulate <- function(n, k){
#'   Sys.sleep(10)
#'   rnorm(n) + k
#' }
#'
#' # the results of this function will be cached
#' # the function will only be run:
#' # 1. the first time the function is run
#' # 2. if the parameters (N, K) are changed and the function is rerun
#'
#' cache(x <- simulate(N, K))
#' }
cache <- function(input, cache_dir = getOption("cache_dir")){

  # TODO add support for multiple caches

  q <- rlang::enquo(input)
  call <- parse_call(q)

  # we always need to evaluate the parts of the call
  f_name <- eval_text(call$f_name, env = call$env)
  f_args <- unlist(lapply(call$f_args, function(x) eval_text(x, env = call$env)))
  parts <- list(f_name, f_args)

  if (!file.exists(file.path(cache_dir, call$v_name))){
    # Case 1: A cache does not exit

    # evaluate the whole call (SLOW)
    comp <- eval_text(call$call, env = call$env)

    # create a cache directory and one for this computation
    dir.create(cache_dir)
    dir.create(file.path(cache_dir, call$v_name))

    # save the objects
    saveRDS(comp, file.path(cache_dir, call$v_name, "object.rds"))
    saveRDS(parts, file.path(cache_dir, call$v_name, "call.rds"))

    # assign to the desired value value as well
    assign(call$v_name, comp, envir = call$env)

  } else {
    # Case 2: A cache exists
    prev_parts <- parts
    curr_parts <- readRDS(file.path(cache_dir, call$v_name, "call.rds"))

    if (identical(prev_parts, curr_parts)) {
      # Case 2a: The call is the same as the one used to create the cache
      # No need to overwrite parts.rds

      # Load the desired object and assign it
      comp <- readRDS(file.path(cache_dir, call$v_name, "object.rds"))
      assign(call$v_name, comp, envir = call$env)

    } else {
      # Case 2b: The call is not the same
      # TODO: remove minor code duplication

      # evaluate the whole call (SLOW)
      comp <- eval_text(call$call, env = call$env)

      saveRDS(comp, file.path(cache_dir, call$v_name, "object.rds"))
      saveRDS(parts, file.path(cache_dir, call$v_name, "call.rds"))

      # assign to the desired value value as well
      assign(call$v_name, comp, envir = call$env)
    }
  }
}
