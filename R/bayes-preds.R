
bayesPredsF <- function(model, n_draws=2000, ci=.95, hdi_interval=TRUE, digits=4, at=NULL, at_means=FALSE){

  predsErrorCheckF(model = model,
                   at    = at)

  # modify the model formula if there's an offset #

  formulaNoOffsets <- modifyFormulaF(model=model)

  # get the model data #
  
  if(!is.null(model$offset)){
    
    modelDataOrg <- model$data %>%
      cbind(offset=model$offset)
    
  } else{
    
    modelDataOrg <- model$data
    
  }

  # tack on the grouping variables #

  atValues  <- expand.grid(at)
  atVars    <- names(atValues)

  modelData <- modelDataOrg %>%
    na.omit() %>%
    .[, !(colnames(.) %in% atVars), drop=F] %>%
    merge(atValues, all=T) %>%
    setDT()

  # do some checks for data integrity #

  newData <- levelsPrepF(data          = modelData,
                         at            = atValues,
                         original_data = modelDataOrg)

  dataCheckF(new_data    = newData,
             model_frame = modelDataOrg)

  # get the predictions #

  preds <- meanPredF(model,
                     new_data    = newData,
                     at          = at,
                     draws       = n_draws,
                     new_formula = formulaNoOffsets,
                     at_means    = at_means)

  # get the results table #

  predTable <- predTableF(preds        = preds,
                          model_data   = modelData,
                          at_vars      = atVars,
                          at_values    = atValues,
                          hdi_interval = hdi_interval,
                          digits       = digits,
                          ci           = ci,
                          at_means     = at_means)

  # output #

  return(as.data.frame(predTable))

}

