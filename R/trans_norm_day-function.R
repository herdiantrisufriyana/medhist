#' Transform medical history by normalizing day up to maximum database days
#'
#' This function transforms each medical history from day interval to normalized
#' one based on maximum period of database, inversely. For example, if maximum
#' period is 730 days, and a code medical history is encountered at 365 days
#' at the latest up to a visit by a subject, then this function will normalize
#' this number into (730-365)/730=0.5. Therefore, normalized values of 0 and 1
#' are respectively day intervals of 730 and 0. Missing value or NA day interval
#' is also normalized to 0.
#'
#' @param mh_table Target population data, a data frame with rows for visits
#' and standardized columns (please see Details below). This is an output of
#' \code{extract_medical_history()}.
#' @param day_cutoff Cutoff of the maximum days, a non-negative numeric that is
#' used for cutoff. If day interval of a medical is more than the cutoff, then
#' that medical history is transformed to 0. Missing value or NA is also
#' transformed to 0.
#' @param verbose Verbosity, a logical indicating whether progress should be
#' shown.
#'
#' @return A TidySet (i.e. ExpressionSet) containing the visits of subjects in
#' medical history and the outcome datasets. Since a code may be encountered
#' before \code{db_start_date}, causing the value returns NA to denote censored
#' data, that medical history is transformed to 0.
#'
#' @keywords medical history, normalization of day interval
#'
#' @export
#'
#' @examples
#'
#' ## Create input example
#' data(medhistdata)
#' 
#' ## Transform medical history by normalizing day up to maximum database days
#' mh_norm_day=trans_norm_day(medhistdata,730)

trans_norm_day=function(tidy_set,day_cutoff,verbose=T){
  
  if(!verbose) pblapply=lapply
  
  if(verbose) cat('Convert the day intervals into the normalized ones\n')
  mh_norm_day=
    tidy_set %>%
    exprs() %>%
    t() %>%
    as.data.frame() %>%
    pblapply(X=seq(ncol(.)),Y=.,function(X,Y){
      data.frame(
          time=
            (day_cutoff-sapply(Y[[X]],function(x)min(x,day_cutoff)))
            /day_cutoff
        ) %>%
        mutate(time=ifelse(is.na(time),0,time)) %>%
        setNames(colnames(Y)[X])
    }) %>%
    do.call(cbind,.) %>%
    t()
  
  tidy_set %>%
    `exprs<-`(mh_norm_day)
  
}
