## get starting point

build_starting_pop <- function(pars=NULL){
  if (is.null(pars)){
    pars <- yoder_defaults()
  }
  start_pars <- get_parameters(pars)
  start_pops <- get_starting_pop(start_pars)
  out <- list(pars=start_pars, pops=start_pops,
              fit=NA, part=NA)
  class(out) <- c("list", "metapop")
  out
}

print.metapop <- function(x){
  if (!inherits(x, "metapop")){
    stop("object needs to be of class 'metapop'")
  }
  print(paste0("There are ", length(x$pops), " populations in this community"))
}