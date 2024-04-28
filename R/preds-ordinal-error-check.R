
predsErrorCheckF <- function(model, at, centrality){

  # make sure it's the right model #
  
  if(!all(c('stanreg', 'polr') %in% class(model))){
    stop("Only models fit using 'stan_polr' are supported!")
  }
  
  # make sure the link function is supported #
  
  if(!model$method %in% c("logistic", "probit", "cloglog")){
    stop('The link function of your model is not supported!')
  }

  # check that the names for the at values are correct #

  if(!all(names(at) %in% names(model$model))){
    stop("The names for the at values don't match up with the names in the model data!")
  }
  
  # check that the centrality measure is supported #
  
  if(!(centrality %in% c('mean', 'median'))){
    stop("Centrality options are 'mean' or 'median'!")
  }

}
