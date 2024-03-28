

bayesOrdinalPredsF <- function(model, n_draws=1000, ci=.95, digits=3, at=NULL){

  # set dplyr option #

  options(dplyr.summarise.inform = FALSE)

  # check that the 'at' argument is specified #

  if(is.null(at)){
    stop("You have to specify 'at' values!")
  }

  # check the class and link of the model #

  if(!('stanreg' %in% class(model) & 'polr' %in% class(model) & model$family=='logistic')){
    stop("This function only works for ordinal logistic regression models fit by 'stan_polr' from the 'rstanarm' package.")
  }

  # get the model data #

  atValues  <- expand.grid(at) %>%
    as_tibble()

  modelDataOrg <- model.frame(formula=formula(model), data=model$data)

  modelData <- modelDataOrg %>%
    as_tibble(.) %>%
    select(., -c(names(atValues))) %>%
    cross_join(., atValues)

  newData <- levelsPrepF(modelData, atValues, modelDataOrg)

  dataCheckF(newData, modelDataOrg)

  # get the ordered outcomes for the response variable #

  yOutcomes <- levels(model$y)

  # get the predictions #

  set.seed(500)

  predTable <- meanPredOrdinalF(model, new_data=newData, draws=n_draws, y_outcomes=yOutcomes, at_values=atValues, digits=digits, ci=ci)

  # output #

  return(list(predTable = predTable))

}

