
bayesCountPredsF <- function(model, counts, n_draws=2000, ci=.95, hdi_interval=TRUE, centrality='mean', digits=4, at=NULL, at_means=FALSE){

  countPredsErrorCheckF(model      = model,
                        counts     = counts,
                        at         = at,
                        centrality = centrality)

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

  preds <- meanCountPredF(model,
                          new_data    = newData,
                          counts      = counts,
                          at          = at,
                          draws       = n_draws,
                          new_formula = formulaNoOffsets,
                          at_means    = at_means)

  # get the results table #

  predTable <- countPredTableF(preds        = preds,
                               model_data   = modelData,
                               counts       = counts,
                               at_vars      = atVars,
                               at_values    = atValues,
                               hdi_interval = hdi_interval,
                               centrality   = centrality,
                               digits       = digits,
                               ci           = ci,
                               at_means     = at_means)

  # output #

  predList <- structure(list(predDraws = preds,
                             predTable = as.data.frame(predTable)),
                        class = c("bayes_mean_scale_pred", "list"))
  
  
  return(predList)
  
}
