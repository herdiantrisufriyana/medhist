#' Transform medical history into a rate by utilizing Kaplan-Meier estimator
#'
#' This function transforms each medical history from day interval to a rate by
#' utilizing Kaplan-Meier estimator in time-to-event analysis. Intuition behind
#' this procedure is that a medical history may be absent because a patient do
#' not visit a healthcare provider although there is a symptom indicating a
#' medical condition. Another situation is a patient visit another provider;
#' thus, a healthcare provider do not have record of a medical history, while
#' the patient do not report (e.g. forget) that medical history to the
#' physician. Therefore, the transformed value is the probability of a code
#' not encountered before t that denotes a time when at least a code encountered
#' once. A censored code encounter is treated as 'not encountered'. This
#' probability is also computed across healthcare providers.
#' 
#' @param tidy_set A TidySet (i.e. ExpressionSet) containing the visits of subjects in
#' outcome dataset, paid by any payment systems. This TidySet also accomodates
#' outcome dataset. This is an output of  \code{compile_mh_outcome()}. Only 
#' non-zero variance medical histories are taken. This is applied automatically.
#' No need to use \code{extract_nzv_mh()} before plugging data for this
#' function. It does not matter if \code{extract_nps_mh()} is applied, since 
#' non-perfect separation medical histories are also non-zero variance ones, 
#' conceivably.
#' @param hist_rate Historical rate data, a data frame with three columns which
#' are \code{mh} for medical history, \code{time} for days before the visit, and
#' \code{estimate} for historical rate for each medical history at the time.
#' @param interpolation Interpolation method, a character of interpolation
#' method. This accepts following input: 'none', 'linear', 'spline', and
#' 'stine'. If no interpolation is expected, then set to 'none'; thus, the same
#' rate is applied as the later one. This will show a stair-step appearance in
#' a typical Kaplan-Meier plot. Instead of a flat line among the times of the
#' estimates, interpolation may be applied by linear, spline, and Stineman
#' approximation.
#' @param verbose Verbosity, a logical indicating whether progress should be
#' shown.
#'
#' @return A TidySet (i.e. ExpressionSet) containing the visits of subjects in
#' medical history and the outcome datasets. Since a code may be encountered 
#' before \code{db_start_date}, the value returns NA to denote censored data. 
#' This is accounted for Kaplan-Meier estimation.
#'
#' @keywords medical history, historical rate, Kaplan-Meier estimator,
#' time-to-event analysis
#'
#' @export
#'
#' @examples
#'
#' ## Create input example
#' data(medhistdata)
#' 
#' ## Transform medical history by normalizing day up to maximum database days
#' mh_hist_rate=trans_hist_rate(medhistdata)

trans_hist_rate=function(tidy_set,hist_rate=NULL,interpolation=NULL,verbose=T){
  
  if(!verbose) pblapply=lapply
  
  key=extract_nzv_mh(tidy_set)$key
  
  if(is.null(hist_rate)){
    
    if(is.null(interpolation)) interpolation='none'
    
    if(verbose){
      cat('Compute historical rate using time-to-event analysis ')
      if(interpolation=='none'){
        cat('without interpolation\n')
      }else{
        cat('with interpolation\n')
      }
    }
    input=
      tidy_set %>%
      exprs() %>%
      t() %>%
      as.data.frame() %>%
      cbind(pData(protocolData(tidy_set))) %>%
      select_at(c(key,'subject_id')) %>%
      gather(mh,time,-subject_id) %>%
      group_by(mh) %>%
      do(tidy(survfit(Surv(time)~mh,data=.,id=subject_id),data=.)) %>%
      select(mh,time,estimate)
    
    hist_rate=
      input %>%
      pblapply(X=.$mh %>% .[!duplicated(.)],Y=.,function(X,Y){
        Z=filter(Y,mh==X)
        
        K=Z %>%
          right_join(
            data.frame(
              mh=X
              ,time=seq(min(Z$time),max(Z$time))
            )
            ,by=c('mh','time')
          ) %>%
          arrange(time)
        if(interpolation!='none'){
          mutate(K,estimate=na_interpolation(estimate,option=interpolation))
        }else{
          fill(K,estimate)
        }
      }) %>%
      do.call(rbind,.)
    
  }
  
  if(verbose) cat('Convert the day intervals into the historical rates\n')
  km_table=
    tidy_set %>%
    exprs() %>%
    t() %>%
    as.data.frame() %>%
    pblapply(X=seq(ncol(.)),Y=.,Z=hist_rate,function(X,Y,Z){
      data.frame(
        mh=colnames(Y)[X]
        ,time=Y[[X]]
      ) %>%
        left_join(Z,by=c('mh','time')) %>%
        select(estimate) %>%
        mutate(estimate=ifelse(is.na(estimate),0,estimate)) %>%
        setNames(colnames(Y)[X])
    }) %>%
    do.call(cbind,.) %>%
    `rownames<-`(colnames(tidy_set)) %>%
    t()
  
  tidy_set %>%
    `exprs<-`(km_table) %>%
    `preproc<-`(list(hist_rate=hist_rate))
  
}
