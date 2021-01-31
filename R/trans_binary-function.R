#' Transform medical history by binarizing day interval
#'
#' This function transforms each medical history from day interval to binary
#' one. If there is no record, then the value is 0; otherwise, the value is 1.
#'
#' @param tidy_set A TidySet (i.e. ExpressionSet) containing the visits of 
#' subjects in outcome dataset, paid by any payment systems. This TidySet also 
#' accomodates outcome dataset. This is an output of 
#' \code{compile_mh_outcome()}.
#' @param verbose Verbosity, a logical indicating whether progress should be
#' shown.
#'
#' @return A TidySet (i.e. ExpressionSet) containing the visits of subjects in
#' medical history and the outcome datasets. Since a code may be encountered
#' before \code{db_start_date}, causing the value returns NA to denote censored
#' data, that medical history is transformed to 0. But, if the interval is 0,
#' the value returns 1 because this means the record exists.
#'
#' @keywords medical history, binarization of day interval
#'
#' @export
#'
#' @examples
#'
#' ## Create input example
#' data(medhistdata)
#' 
#' ## Transform medical history by binarizing day interval
#' mh_binary=trans_binary(medhistdata)

trans_binary=function(tidy_set,verbose=T){
  
  if(!verbose) pblapply=lapply
  
  if(verbose) cat('Convert the day intervals into the binarized ones\n')
  mh_binary=
    tidy_set %>%
    exprs() %>%
    t() %>%
    as.data.frame() %>%
    pblapply(X=seq(ncol(.)),Y=.,function(X,Y){
      data.frame(enc=Y[[X]]) %>%
        mutate(enc=ifelse(is.na(enc),0,1)) %>%
        setNames(colnames(Y)[X])
    }) %>%
    do.call(cbind,.) %>%
    t()
  
  tidy_set %>%
    `exprs<-`(mh_binary)
  
}
