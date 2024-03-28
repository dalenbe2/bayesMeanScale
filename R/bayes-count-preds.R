


bayesCountPredsF <- function(model, n_draws=2000, ci=.95, digits=3, at=NULL, outcomes=NULL, model_offset=NULL){

  # set dplyr option #

  options(dplyr.summarise.inform = FALSE)

  # check that the 'at' argument is specified #

  if(is.null(at)){
    stop("You have to specify 'at' values!")
  }

  # check the class and exponential family of the model #

  if(!('stanreg' %in% class(model) & (model$family$family %in% c("poisson", "neg_binomial_2")))){
    stop("This function is only for Poisson and negative binomial models fit by 'rstanarm'.")
  }

  # check that the outcomes have been specified #

  if(is.null(outcomes)){
    stop("You have to specify which outcomes you want the probabilities for!")
  }

  # check that the offset has been specified if there's one in the model #

  if(!is.null(model$offset) & is.null(model_offset)){
    stop("You need to specify the model offset!")
  }

  # get the model data #

  atValues  <- expand.grid(at)

  modelData <- model.frame(formula=formula(model), data=model$data) %>%
    as_tibble(.) %>%
    select(., -c(names(atValues))) %>%
    full_join(atValues, by=character())

  # get the predictions #

  set.seed(500)

  predsRaw <- posterior_predict(model, newdata=modelData, ndraws=n_draws, offset=model_offset) %>%
    t(.) %>%
    as_tibble(., .name_repair='minimal') %>%
    cbind(., atValues)

  predsNew <- data.frame()

  for(i in 1:length(outcomes)){

    predsTemp <- predsRaw %>%
      group_by_at(names(atValues)) %>%
      summarize_all(~ mean(.==outcomes[i])) %>%
      gather(contains("Var."),
             key   = "which_pred",
             value = "pred") %>%
      mutate(which_pred = outcomes[i])

    predsNew  <- bind_rows(predsNew, predsTemp)

  }

  # make the table #

  predTable <- predsNew %>%
    group_by_at(c(names(atValues), 'which_pred')) %>%
    summarize(mean        = round(mean(pred), digits=digits),
              median      = round(median(pred), digits=digits),
              ci_level    = paste(paste(ci*100, "%", sep="")),
              hpd_lower   = round(hdi(pred, ci=ci)$CI_low, digits=digits),
              hpd_upper   = round(hdi(pred, ci=ci)$CI_high, digits=digits)) %>%
    as.data.frame(.)

  # output #

  return(list(predTable = predTable))

}

