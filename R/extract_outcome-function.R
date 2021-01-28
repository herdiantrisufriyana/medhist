#' Extract outcome of subjects
#'
#' This function makes a list of subjects with either event or non-event
#' outcome, that visit healthcare providers up to the predefined latest date.
#'
#' @param population Target population data, a data frame with rows for visits
#' and standardized columns (please see Details below). If only a single
#' healthcare provider, then make \code{healthcare_id} consisting only a single
#' identification number.
#' @param icd10_event Event code, a character of ICD-10 code for either
#' diagnosis or procedure, that identify subjects with event outcome. This
#' parameter should be a regular expression if detecting >1 ICD-10 codes. The
#' detection is not exact but anywhere within the character sequence; thus,
#' the code with more than k-mer will be extracted.
#' @param latest_event Latest date of event, a function to choose
#' \code{admission_date} with the \code{icd10_event} among many for each
#' subject. This date will be adjusted with \code{day_to_event} to limit visits
#' accounted for medical histories, since these are considerably before the
#' outcome. The function is conceivably \code{first()} or \code{last()} from
#' \code{dplyr} package, preferably \code{first()}. This may be respectively
#' \code{min()} or \code{max()} if using base R.
#' @param day_to_event Day to event, an integer to adjust \code{latest_event}
#' for limiting visits accounted for medical histories. These are considerably
#' before the outcome, i.e. up to -1 day.
#' @param icd10_nonevent Non-event code, a character of ICD-10 code for either
#' diagnosis or procedure, that identify subjects with non-event outcome. This
#' parameter should be a regular expression if detecting >1 ICD-10 codes. Make
#' this parameter as \code{''} (character of none) if all except event codes
#' are used. The detection is not exact but anywhere within the character
#' sequence; thus, the code with more than k-mer will be extracted.
#' @param latest_nonevent Latest date of non-event, a function to choose
#' \code{admission_date} with the \code{icd10_nonevent} among many for each
#' subject. This date will be adjusted with \code{day_to_nonevent} to limit
#' visits accounted for medical histories, since these are considerably up to
#' the day of the outcome. The function is conceivably \code{first()} or
#' \code{last()} from \code{dplyr} package, preferably \code{last()}. This may
#' be respectively \code{min()} or \code{max()} if using base R.
#' @param day_to_nonevent Day to non-event, an integer to adjust
#' \code{latest_nonevent} for limiting visits accounted for medical histories.
#' These are considerably up to the day of the outcome, i.e. up to 0 day.
#' @param verbose Verbosity, a logical indicating whether progress should be
#' shown.
#'
#' @details
#' Target population data consisted \code{visit_id},\code{subject_id},
#' \code{healthcare_id}, \code{admission_date}, \code{code}, and
#' \code{db_start_date}.  Columns of \code{visit_id},\code{subject_id}, and
#' \code{healthcare_id} are characters of identification numbers identifying
#' respectively unique visits, subjects, and healthcare providers. Columns of
#' \code{admission_date} and \code{db_start_date} are dates of subject admission
#' in a visit and database start of recording period, respectively. Column of
#' \code{code} is a character of ICD-10 code for either diagnosis or procedure
#' regardless the number of digit or k-mer.
#' 
#' @return Subject list, a data frame with rows for unique subjects and columns
#' of \code{subject_id}, \code{latest_date}, and \code{outcome}. The last column
#' is a factor of which non-event is the first class between non-event and
#' event.
#'
#' @keywords outcome
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
#' ## Extract outcome of subjects
#' outcome=extract_outcome(population,'O1[4-5]',first,-1,'Z3[3-7]',last,0)

extract_outcome=function(population
                         ,icd10_event
                         ,latest_event=first
                         ,day_to_event=-1
                         ,icd10_nonevent
                         ,latest_nonevent=last
                         ,day_to_nonevent=0
                         ,verbose=T){
  
  if(verbose){
    cat('Create an outcome table\n')
    pb=startpb(0,7)
    on.exit(closepb(pb))
    setpb(pb,0)
  }
  
  if(verbose) setpb(pb,1)
  event=
    population %>%
    filter(str_detect(code,icd10_event))
  
  if(verbose) setpb(pb,2)
  event=
    event %>%
    group_by(subject_id) %>%
    summarize(
      latest_date=latest_event(admission_date)+day_to_event
      ,.groups='drop'
    )
  
  if(verbose) setpb(pb,3)
  event=
    event %>%
    mutate(outcome='event')
  
  
  if(verbose) setpb(pb,4)
  nonevent=
    population %>%
    filter(!subject_id%in%event$subject_id) %>%
    filter(str_detect(code,icd10_nonevent))
  
  if(verbose) setpb(pb,5)
  nonevent=
    nonevent %>%
    group_by(subject_id) %>%
    summarize(
      latest_date=latest_nonevent(admission_date)+day_to_nonevent
      ,.groups='drop'
    )
  
  if(verbose) setpb(pb,6)
  nonevent=
    nonevent %>%
    mutate(outcome='nonevent')
  
  
  if(verbose) setpb(pb,7)
  rbind(event,nonevent) %>%
    mutate(outcome=factor(outcome,c('nonevent','event')))
  
}
