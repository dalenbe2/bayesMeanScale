

bayesOrdinalAndCountMargEffF <- function(model, n_draws=2000, response_outcome, marginal_effect, start_value, end_value, ci=.95, digits=2, at=NULL, graph_title="", sub_title=""){

  ## Check the class of the model ##

  if(!(class(model)[1]=="stanreg" & (class(model)[2]=="polr") | (model$family$family %in% c("poisson", "neg_binomial_2")))){
    stop("This function only works for ordinal or count model fit by 'rstanarm'.")
  }

  ## Set dplyr option ##

  options(dplyr.summarise.inform = FALSE)

  ## Get the model data ##

  if(!is.null(at)){

    atValues                    <- expand.grid(at)
    startData <- endData        <- model.frame(formula=formula(model), data=model$data)
    startData[,marginal_effect] <- start_value
    startData                   <- cross_join(select(startData, -c(names(atValues))), atValues)
    endData[,marginal_effect]   <- end_value
    endData                     <- cross_join(select(endData, -c(names(atValues))), atValues)

  } else{

    startData <- endData        <- model.frame(formula=formula(model), data=model$data)
    startData[,marginal_effect] <- start_value
    endData[,marginal_effect]   <- end_value

  }

  ## Get the predictions ##

  set.seed(500)

  if(!is.null(at)){

  predStart <- posterior_predict(model, newdata=startData, draws=n_draws) %>%
    t(.) %>%
    as.data.frame(.) %>%
    cbind(., atValues) %>%
    group_by_at(names(atValues)) %>%
    summarize_all(~ mean(.==response_outcome)) %>%
    gather(-names(atValues),
           key   = "which_pred",
           value = "pred")

  predEnd   <- posterior_predict(model, newdata=endData, draws=n_draws) %>%
    t(.) %>%
    as.data.frame(.) %>%
    cbind(., atValues) %>%
    group_by_at(names(atValues)) %>%
    summarize_all(~ mean(.==response_outcome)) %>%
    gather(-names(atValues),
           key   = "which_pred",
           value = "pred")

  } else{

    predStart <- posterior_predict(model, newdata=startData, draws=n_draws) %>%
      t(.) %>%
      as.data.frame(.) %>%
      summarize_all(~ mean(.==response_outcome)) %>%
      t(.)

    predEnd   <- posterior_predict(model, newdata=endData, draws=n_draws) %>%
      t(.) %>%
      as.data.frame(.) %>%
      summarize_all(~ mean(.==response_outcome)) %>%
      t(.)

  }

  ## Get the difference in predicted means ##

  if(!is.null(at)){

    predDiff <- predStart %>%
      cbind(pred_end=predEnd$pred) %>%
      mutate(diff = pred - pred_end)

  } else{

    predDiff <- predStart - predEnd %>%
      as.data.frame(.) %>%
      set_names(., "diff")

  }

  ## Make the marginal effects and predicted probabilities tables ##

  if(!is.null(at)){

    diffTable <- predDiff %>%
      group_by_at(names(atValues)) %>%
      summarize(response_outcomes = response_outcome,
                marg_effect       = marginal_effect,
                start_val         = start_value,
                end_val           = end_value,
                mean              = round(mean(diff), digits=digits),
                median            = round(median(diff), digits=digits),
                ci_level          = paste(paste(ci*100, "%", sep="")),
                hpd_lower         = round(hdi(diff, ci=ci)$CI_low, digits=digits),
                hpd_upper         = round(hdi(diff, ci=ci)$CI_high, digits=digits),
                pd                = round(as.numeric(p_direction(diff)), digits=digits)) %>%
      as.data.frame(.)

  } else{

    diffTable <- data.frame(response_outcome = response_outcome,
                            marg_effect      = marginal_effect,
                            start_val        = start_value,
                            end_val          = end_value,
                            mean             = round(mean(predDiff$diff), digits=digits),
                            median           = round(median(predDiff$diff), digits=digits),
                            ci_level         = paste(paste(ci*100, "%", sep="")),
                            hpd_lower        = round(hdi(predDiff$diff, ci=ci)$CI_low, digits=digits),
                            hpd_upper        = round(hdi(predDiff$diff, ci=ci)$CI_high, digits=digits),
                            pd               = round(as.numeric(p_direction(predDiff$diff)), digits=digits))

  }

  ## Output ##

  return(list(diffMatrix = predDiff,
              diffTable  = diffTable))


}



