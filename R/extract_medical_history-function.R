#' Extract medical history of subjects per healthcare provider
#'
#' This function spread medical histories into different columns.
#'
#' @param population Target population data, a data frame with rows for visits
#' and standardized columns (please see Details below). If only a single
#' healthcare provider, then make \code{healthcare_id} consisting only a single
#' identification number.
#' @param n_batch Number of batch, a non-negative integer of number of batch in
#' which a number of subjects per provider are grouped. This is intended to
#' ensure the computation fits to the memory.
#' @param cl Parallel cluster, a non-negative integer of number of CPU cluster
#' used for computation in parallel. Set to 1 if no parallelism is expected.
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
#' @return Target population data, a data frame with rows for visits
#' and standardized columns (please see Details above), except \code{code}.
#' Each code are spread as a column consisting the number of days from the
#' latest \code{admission_date} on which this code have ever been encountered to
#' \code{admission_date} of each visit. If no encounter is found, then the value
#' returns NA to denote censored data. This is because the code may be
#' encountered before \code{db_start_date}. If the latest date and that of each
#' visit are the same, then the value returns 0.
#'
#' @keywords medical history
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
#'   extract_outcome(population,'O1[4-5]',min,-1,'Z3[3-7]',max,0) %>%
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

extract_medical_history=function(population,n_batch=50,cl=1){
  
  cat('Create a sparse binary data frame of diagnosis/procedure code\n')
  sparse_binary_df=
    population %>%
    select(code) %>%
    pblapply(X=pull(.,code) %>% .[!duplicated(.)],Y=.,function(X,Y){
      mutate(Y,code=as.integer(code==X)) %>% setNames(X)
    }) %>%
    do.call(cbind,.)
  
  sparse_binary_df=
    population %>%
    select(-code) %>%
    cbind(sparse_binary_df) %>%
    arrange(subject_id,healthcare_id,admission_date)
  
  
  cat('Split into batches of subject per healthcare provider\n')
  subject_per_provider=
    population %>%
    select(subject_id,healthcare_id) %>%
    .[!duplicated(.),]
  
  visit_per_batch=
    round(seq(1,nrow(subject_per_provider),len=n_batch)) %>%
    pblapply(X=2:length(.)
             ,Y=.
             ,Z=subject_per_provider
             ,K=sparse_binary_df
             ,function(X,Y,Z,K){
      
      K %>%
        left_join(
          Z %>%
            slice(seq(ifelse(X==2,1,Y[X-1]+1),Y[X])) %>%
            mutate(selected=1)
          ,by=colnames(Z)
          ) %>%
        filter(selected==1) %>%
        select(-selected)
      
    })
  
  
  cat('Compute day interval of a medical history to each visit\n')
  cat('Started:',as.character(now()),'\n')
  cl=makeCluster(cl)
  clusterEvalQ(cl,{
    library('tidyverse')
    library('pbapply')
  })
    
    medical_history=
      visit_per_batch %>%
      pblapply(X=seq(length(.)),Y=.,cl=cl,function(X,Y){
        Z=Y[[X]] %>%
          select(subject_id,healthcare_id) %>%
          .[!duplicated(.),]
        
        Z %>%
          lapply(X=seq(nrow(.)),Y=.,Z=Y[[X]],function(X,Y,Z){
            K=Z %>%
              filter(
                subject_id==Y$subject_id[X] &
                  healthcare_id==Y$healthcare_id[X]
              )
            
            K %>%
              lapply(X=seq(nrow(.)),Y=.,function(X,Y){
                Z=Y %>%
                  filter(between(
                    admission_date
                    ,Y$db_start_date[X]
                    ,Y$admission_date[X]
                  )) %>%
                  select(
                    -visit_id
                    ,-subject_id
                    ,-healthcare_id
                    ,-db_start_date
                  )
                
                Z=Z %>%
                  select(-admission_date) %>%
                  lapply(X=seq(ncol(.)),Y=.,Z=Z$admission_date,function(X,Y,Z){
                    code_enc=which(Y[[X]]>0)
                    latest_enc=
                      ifelse(
                        length(code_enc)==0
                        ,NA,max(Z)-Z[max(code_enc)]
                      )
                    data.frame(time=latest_enc) %>% setNames(colnames(Y)[X])
                  }) %>%
                  do.call(cbind,.)
                
                Y[X,] %>%
                  select(
                    visit_id
                    ,subject_id
                    ,healthcare_id
                    ,admission_date
                    ,db_start_date
                  ) %>%
                  cbind(Z)
              })%>%
              do.call(rbind,.)
            
          }) %>%
          do.call(rbind,.)
        
      }) %>%
      do.call(rbind,.)
    
  stopCluster(cl)
  rm(cl)
  gc()
  cat('End:',as.character(now()))
  
  medical_history
  
}
