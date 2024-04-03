
margCompareErrorCheckF <- function(marg_list){

  if(!inherits(marg_list, 'bayes.marg')){
    stop("The 'marg_list' argument must have class 'bayes.marg'!")
  }

  if(nrow(unique(subset(marg_list$diffDraws, select=-diff)))==1){
    stop("There is only 1 marginal effect, so nothing to compare to!")
  }

}
