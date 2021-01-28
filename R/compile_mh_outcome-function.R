#' Construct unified data as a TidySet
#'
#' This function compiles medical history and the outcome table into a TidySet
#' (i.e. ExpressionSet). One example of the output can be called by
#' \code{data(medhistdata)}.
#'
#' @param mh_table Target population data, a data frame with rows for visits
#' and standardized columns (please see Details below). This is an output of
#' \code{extract_medical_history()}.
#' @param outcome Subject list, a data frame with rows for unique subjects and
#' columns of \code{subject_id}, \code{latest_date}, and \code{outcome}. The
#' last column is a factor of which non-event is the first class between
#' non-event and event.
#'
#' @details
#' Target population data consisted \code{visit_id},\code{subject_id},
#' \code{healthcare_id}, \code{admission_date}, and \code{db_start_date} beyond
#' columns for medical histories. Columns of \code{visit_id},\code{subject_id},
#' and \code{healthcare_id} are characters of identification numbers idenitfying
#' respectively unique visits, subjects, and healthcare providers. Columns of
#' \code{admission_date} and \code{db_start_date} are dates of subject admission
#' in a visit and database start of recording period, respectively. The
#' remaining columns are named using ICD-10 code for either diagnosis or
#' procedure regardless the number of digit or k-mer. Each code are spread as a
#' column consisting the number of days from the latest \code{admission_date} on
#' which this code have ever been encountered to \code{admission_date} of each
#' visit. If no encounter is found, then the value returns NA to denote censored
#' data. This is because the code may be encountered before
#' \code{db_start_date}. If the latest date and that of each visit are the same,
#' then the value returns 0.
#' 
#' @return A TidySet (i.e. ExpressionSet) containing the visits of subjects in
#' medical history and the outcome datasets.
#'
#' @keywords TidySet, medical history, outcome
#'
#' @export
#'
#' @examples
#'
#' ## Create input example
#' data(visit_cap)
#' data(visit_ffs)
#' data(visit_drg)
#' data(diagnosis)
#' 
#' population=
#'   list(visit_cap,visit_ffs,visit_drg) %>%
#'   lapply(select,visit_id,subject_id,healthcare_id,admission_date) %>%
#'   do.call(rbind,.) %>%
#'   left_join(diagnosis,by='visit_id') %>%
#'   filter(!code_type%in%c('Admission diagnosis')) %>%
#'   select(-code_type) %>%
#'   mutate(db_start_date=as.Date('2015-01-01')) %>%
#'   .[!duplicated(.),]
#'
#' ## Extract outcome of subjects and sample some of them
#' outcome=
#'   extract_outcome(population,'O1[4-5]',first,-1,'Z3[3-7]',last,0) %>%
#'   group_by(outcome) %>%
#'   slice(sample(seq(n()),ceiling(n()*0.0125),F)) %>%
#'   ungroup()
#'
#' ## Filter medical history before the date of either event or non-event
#' input=
#'   outcome %>%
#'   right_join(population,by='subject_id') %>%
#'   select(visit_id, everything()) %>%
#'   filter(admission_date<latest_date) %>%
#'   select(-outcome,-latest_date)
#'
#' ## Extract medical history of subjects per healthcare provider
#' mh_table=extract_medical_history(input)
#' 
#' ## Construct unified data as a TidySet
#' medhisdata=compile_mh_outcome(mh_table,outcome,'ICD-10 (2016)')

compile_mh_outcome=function(mh_table,outcome){
  
  data=
    outcome %>%
    right_join(mh_table,by='subject_id') %>%
    select(visit_id, everything()) %>%
    mutate(
      id=paste0('I',str_pad(seq(nrow(.)),str_count(nrow(.)),'left','0'))
    ) %>%
    column_to_rownames(var='id')
  
  adata=
    data %>%
    select(
      -visit_id
      ,-subject_id
      ,-latest_date
      ,-outcome
      ,-healthcare_id
      ,-admission_date
      ,-db_start_date
    ) %>%
    t()
  
  phdata=
    data %>%
    select(outcome) %>%
    AnnotatedDataFrame()
  
  fdata=
    data %>%
    select(
      -visit_id
      ,-subject_id
      ,-latest_date
      ,-outcome
      ,-healthcare_id
      ,-admission_date
      ,-db_start_date
    ) %>%
    colnames() %>%
    data.frame(feature=.) %>%
    column_to_rownames(var='feature') %>%
    AnnotatedDataFrame()
  
  prdata=
    data %>%
    select(
      visit_id
      ,subject_id
      ,latest_date
      ,healthcare_id
      ,admission_date
      ,db_start_date
    ) %>%
    AnnotatedDataFrame()
  
  ExpressionSet(
    assayData=adata
    ,phenoData=phdata
    ,featureData=fdata
    ,protocolData=prdata
  )
  
}
