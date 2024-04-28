
meanPredOrdinalF <- function(model, new_data, at, draws, y_outcomes, new_formula, at_means){

  # initialize data frame to hold prediction information in #

  meanPredsBig <- data.frame()

  for(j in 1:nrow(at_values)){

  # make the new model matrix #

  atValueNames <- names(at_values)

  newData <- new_data %>%
    setDT(key = names(at_values)) %>%
    .[at_values[j, ], nomatch=NULL]

  modelMatrix   <- model.matrix(formula(model), data=newData)

  # get the draws from the joint posterior #

  modelDrawsOrg <- as.data.frame(model) %>% setDT()
  thresholds    <- names(modelDrawsOrg[, .SD, .SDcols=patterns("\\|")])

  # initialize the array to store the linear predictors #

  linearPredictors <- data.table()

  for(i in thresholds){

    # initialize model matrix and draws for one threshold at a time #

    modelMatrixNew <- modelMatrix[,-1]

    modelDrawsNew  <- modelDrawsOrg %>%
      .[, .SD, .SDcols=!patterns("\\|")]

    # check that new model matrix doesn't have any columns that aren't in joint posterior #

    if(!all(dimnames(modelMatrixNew)[[2]] %in% names(modelDrawsNew))){
      stop("Something is wrong with the model matrix!")
    }

    # make sure to only get joint posterior columns that match up with model matrix #

    modelMatrixNames <- dimnames(modelMatrixNew)[[2]]

    betaDraws <- modelDrawsNew %>%
      .[, .SD, .SDcols=modelMatrixNames] %>%
      as.matrix()

    # make sure model matrix lines up with draws matrix #

    betaDrawsNames <- dimnames(betaDraws)[[2]]

    modelMatrix <- modelMatrixNew %>%
      as.data.table() %>%
      .[, .SD, .SDcols=betaDrawsNames] %>%
      as.matrix()

    # get a sample from the joint posterior #

    set.seed(500)

    betaSamples <- sample(1:nrow(betaDraws), size=draws, replace=T)

    intercept <- modelDrawsOrg %>%
      .[, .SD, .SDcols=i] %>%
      as.matrix() %>%
      .[betaSamples,]

    # compute the linear predictor #

    Z <- (modelMatrix %*% t(betaDraws[betaSamples,]))*-1 + intercept

    # store the linear predictor in a data frame #

    linearPredictors <- rbind(linearPredictors, data.table(Z, threshold=i, observation=1:dim(Z)[[1]]))

  }

  # get the probabilities for each outcome #

  thresholdData <- tibble(
    threshold   = unique(linearPredictors$threshold),
    which_logit = sub("\\|.*", "", threshold)
  ) %>%
    setDT()

  measureVars <- names(select(linearPredictors, -threshold, -observation))

  tempData <- linearPredictors %>%
    setDT() %>%
    melt(measure.vars  = measureVars,
         variable.name = 'draw',
         value.name    = 'z') %>%
    .[, logit_z := exp(z) / (1 + exp(z)), key='threshold'] %>%
    .[thresholdData] %>%
    .[order(observation, draw)]

  # remove some data that's no longer necessary, to free up some memory #

  rm(linearPredictors, modelMatrix, modelDrawsOrg, modelMatrixNew, modelDrawsNew, betaDraws)

  # initialize the data frame for the probabilities #

  probData <- tempData %>%
    unique(., by=c('observation', 'draw')) %>%
    .[, .(observation, draw)]

  probData <- probData[, -c('observation', 'draw')]

  for(i in 1:length(y_outcomes)){

    if(i==1){

      probData[, y_outcomes[[i]]] <- tempData %>%
        .[which_logit==y_outcomes[[i]], logit_z]

    } else{
      if(i==length(y_outcomes)){

        probData[, y_outcomes[[i]]] <- tempData %>%
          .[, 1 - logit_z[which_logit==y_outcomes[[i-1]]]]

      } else{

        probData[, y_outcomes[[i]]] <- tempData %>%
          .[, logit_z[which_logit==y_outcomes[[i]]] - logit_z[which_logit==y_outcomes[[i-1]]]]

      }
    }


  }

  varNames <- c('observation', 'draw')

  meanPreds <- tempData %>%
    unique(., by=c('observation', 'draw')) %>%
    .[, ..varNames] %>%
    cbind(at_values[j,], probData) %>%
    melt(measure.vars  = y_outcomes,
         variable.name = 'which_prob',
         value.name    = 'prob') %>%
    .[, .(avg_prob = mean(prob)), by=c('which_prob', 'observation', names(at_values))] %>%
    .[, .(mean      = round(mean(avg_prob), digits=digits),
          median    = round(median(avg_prob), digits=digits),
          ci_level  = paste0(ci*100, "%"),
          hpd_lower = round(hdi(avg_prob, ci=ci)$CI_low, digits=digits),
          hpd_upper = round(hdi(avg_prob, ci=ci)$CI_high, digits=digits)), by=c('which_prob', names(at_values))]

  meanPredsBig <- bind_rows(meanPredsBig, meanPreds)

  rm(probData, tempData)

  }

  # output #

  return(meanPredsBig)

  }

