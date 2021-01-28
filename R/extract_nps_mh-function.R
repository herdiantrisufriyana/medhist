#' Extract medical history without perfect separation
#'
#' This function makes a list of medical histories without perfect separation.
#' This means a medical history per outcome have non-zero variance.
#'
#' @param tidy_set A TidySet (i.e. ExpressionSet) containing the visits of subjects in
#' outcome dataset, paid by any payment systems. This TidySet also
#' accomodates outcome dataset. This is an output of 
#' \code{compile_mh_outcome()}.
#' @param sd_cutoff Cutoff of standard deviation, a non-negative numeric that is
#' used for cutoff. If standard deviation of a medical history metric for an
#' outcome is more than the cutoff for that outcome, either non-event or event,
#' then that medical history is accounted to the list.
#'
#' @return Medical history list, a data frame with rows for unique medical
#' history and columns of \code{key}, \code{nonevent} and \code{event}. The last
#' two columns are standard deviation of the medical history for non-event and
#' event. Missing value or NA is not accounted when computing the standard
#' deviation.
#'
#' @keywords medical history, perfect separation
#'
#' @export
#'
#' @examples
#'
#' ## Create input example
#' data(medhistdata)
#' 
#' ## Extract medical history without perfect separation
#' ps_remover=extract_nps_mh(medhistdata)

extract_nps_mh=function(tidy_set,sd_cutoff=0){
  
  tidy_set %>%
    exprs() %>%
    t() %>%
    as.data.frame() %>%
    cbind(select(pData(tidy_set),'outcome')) %>%
    select(outcome,everything()) %>%
    gather(key,value,-outcome) %>%
    group_by(key,outcome) %>%
    summarize(sd_value=sd(value,na.rm=T),.groups='drop') %>%
    arrange(factor(key,unique(key)),outcome) %>%
    spread(outcome,sd_value) %>%
    setNames(str_remove_all(names(.),'-')) %>%
    filter(nonevent>0 & event>0)
  
}
