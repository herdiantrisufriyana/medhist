#' Extract medical history without zero variance
#'
#' This function makes a list of medical histories without zero variance.
#'
#' @param tidy_set A TidySet (i.e. ExpressionSet) containing the visits of subjects in
#' outcome dataset, paid by any payment systems. This TidySet also
#' accomodates outcome dataset. This is an output of 
#' \code{compile_mh_outcome()}.
#' @param sd_cutoff Cutoff of standard deviation, a non-negative numeric that is
#' used for cutoff. If standard deviation of a medical history metric is more
#' than the cutoff, then that medical history is accounted to the list.
#'
#' @return Medical history list, a data frame with rows for unique medical
#' history and columns of \code{key} and \code{sd_value}. Missing value or NA
#' is not accounted when computing the standard deviation.
#'
#' @keywords medical history, zero variance
#'
#' @export
#'
#' @examples
#'
#' ## Create input example
#' data(medhistdata)
#' 
#' ## Extract medical history without zero variance
#' zv_remover=extract_nzv_mh(medhistdata)

extract_nzv_mh=function(tidy_set,sd_cutoff=0){
  
  tidy_set %>%
    exprs() %>%
    t() %>%
    as.data.frame() %>%
    gather() %>%
    group_by(key) %>%
    summarize(sd_value=sd(value,na.rm=T),.groups='drop') %>%
    filter(sd_value>sd_cutoff)
  
}
