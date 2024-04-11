
meanCountPredF <- function(model, new_data, counts, at, draws, new_formula, at_means){

  # make the new model matrix #

  modelMatrix   <- model.matrix(new_formula, data=new_data)

  # set aside the offset if there is one #

  if(!is.null(model$offset)){
    modelOffset <- new_data$offset
  }

  # get the draws from the joint posterior #

  modelDrawsOrg <- as.data.table(model)

  # check that new model matrix doesn't have any columns that aren't in joint posterior #

  if(!all(dimnames(modelMatrix)[[2]] %in% names(modelDrawsOrg))){
    stop("Something is wrong with the model matrix!")
  }

  # make sure to only get joint posterior columns that match up with model matrix #

  betaDraws <- modelDrawsOrg %>%
    .[, .SD, .SDcols = dimnames(modelMatrix)[[2]]] %>%
    as.matrix()

  # make sure model matrix lines up with draws matrix #

  if(at_means==F){

    modelMatrixNew <- modelMatrix %>%
      as.data.table() %>%
      .[, .SD, .SDcols = dimnames(betaDraws)[[2]]] %>%
      as.matrix()

  }
  
  if(at_means==T & !is.null(at)){

    atVars <- names(expand.grid(at))

    atVarsNew <- paste0(atVars, "_new")
    setnames(new_data, old=names(new_data[, ..atVars]), new=atVarsNew)

    modelMatrixNew <- modelMatrix %>%
      as.data.table() %>%
      .[, .SD, .SDcols = dimnames(betaDraws)[[2]]] %>%
      cbind(new_data[, ..atVarsNew]) %>%
      .[, lapply(.SD, mean), by=atVarsNew] %>%
      .[, !..atVarsNew] %>%
      as.matrix()

    setnames(new_data, old=names(new_data[, ..atVarsNew]), new=atVars)

  }

  if(at_means==T & is.null(at)){

    modelMatrixNew <- modelMatrix %>%
      as.data.table() %>%
      .[, .SD, .SDcols = dimnames(betaDraws)[[2]]] %>%
      .[, lapply(.SD, mean)] %>%
      as.matrix()

  }
  
  # get a sample from the joint posterior #

  set.seed(500)

  betaSamples <- sample(1:nrow(betaDraws), size=draws, replace=T)

  # compute the linear predictor #

  if(!is.null(model$offset)){

    Z <- (modelMatrixNew %*% t(betaDraws[betaSamples,])) + modelOffset

  } else{

    Z <- modelMatrixNew %*% t(betaDraws[betaSamples,])

  }
  
  # get the draws for the reciprocal dispersion parameter for negative binomial #
  
  if(model$family$family=="neg_binomial_2"){
    
    dispersion <- 1 / modelDrawsOrg[betaSamples,]$reciprocal_dispersion
    dispersionMatrix <- as.matrix(t(replicate(draws, dispersion)))
    
  }

  # apply the inverse link function #

  if(model$family$link=="log"){
    meanPreds <- exp(Z)
  }

  if(model$family$link=='identity'){
    meanPreds <- Z
  }
  
  if(model$family$link=='sqrt'){
    meanPreds <- Z^2
  }

  # get probabilities for counts #
  
  meanCountPreds <- data.frame()
  
  if(model$family$family=="poisson"){
    
    for(i in 1:length(counts)){
      
      tempPreds      <- dpois(counts[[i]], lambda=meanPreds)
      meanCountPreds <- rbind(meanCountPreds, data.frame(tempPreds, count=counts[[i]]))
      
    }
    
  } 
  
  if(model$family$family=="neg_binomial_2"){
    
    for(i in 1:length(counts)){
      
      tempPreds      <- dnbinom(counts[[i]], size=dispersionMatrix, mu=meanPreds)
      meanCountPreds <- rbind(meanCountPreds, data.frame(tempPreds, count=counts[[i]]))
      
    }
    
  }
  
  # output #

  return(meanCountPreds)

}
