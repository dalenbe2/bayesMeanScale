
predsErrorCheckF <- function(model, at){

  # check that the 'at' argument is specified #

  if(is.null(at)){
    stop("You have to specify 'at' values!")
  }

  # make sure model is a stanreg object and of a supported exponential family #

  if(!('stanreg' %in% class(model))){
    stop("The model must be a 'stanreg' object!")
  }

  if(!(model$family$family %in% c('beta', 'binomial', 'gaussian', 'Gamma', 'poisson', 'neg_binomial_2'))){
    stop("The model must be in a supported exponential family!")
  }

  # check that the names for the at values are correct #

  if(!all(names(at) %in% names(model$model))){
    stop("The names for the at values don't match up with the names in the model data!")
  }

}
