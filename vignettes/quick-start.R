# Install and load
  
## Install and load these packages
devtools::install_github('herdiantrisufriyana/medhist')
library(medhist)
library(tidyverse)
library(pbapply)
library(parallel)
library(BiocGenerics)
library(lubridate)
library(Biobase)
library(broom)
library(survival)
library(imputeTS)
library(caret)
library(MLeval)


# Load artificial data

## Select population
data(visit_cap)
data(visit_ffs)
data(visit_drg)
data(diagnosis)
data(annotation)

population=
  list(visit_cap,visit_ffs,visit_drg) %>%
  lapply(select,visit_id,subject_id,healthcare_id,admission_date) %>%
  do.call(rbind,.) %>%
  left_join(diagnosis,by='visit_id') %>%
  filter(!code_type%in%c('Admission diagnosis')) %>%
  select(-code_type) %>%
  mutate(db_start_date=as.Date('2015-01-01')) %>%
  .[!duplicated(.),]


# Run preprocessor

## Determine outcome
outcome=
  population %>%
  extract_outcome('O1[4-5]',min,-1,'Z3[3-7]',max,0)

set.seed(33)
outcome=
  outcome %>%
  group_by(outcome) %>%
  slice(sample(seq(n()),ceiling(n()*0.05),F)) %>%
  ungroup()

## Construct medical history dataset
mh_table=
  outcome %>%
  right_join(population,by='subject_id') %>%
  select(visit_id, everything()) %>%
  filter(admission_date<latest_date) %>%
  select(-outcome,-latest_date) %>%
  extract_medical_history()

## Construct unified data as a TidySet
medhistdata=compile_mh_outcome(mh_table,outcome)

## Add optional data
data(mdata)
data(edata)

medhistdata=
  medhistdata %>%
  
  # Update metadata in phenotype table
  `phenoData<-`(
    phenoData(.) %>%
      `varMetadata<-`(mdata['outcome',,drop=F])
  ) %>%
  
  # Add annotation in feature table
  `featureData<-`(
    fData(.) %>%
      rownames_to_column(var='code') %>%
      left_join(annotation,by='code') %>%
      column_to_rownames(var='code') %>%
      AnnotatedDataFrame()
  )  %>%
  
  # Update experimenter information
  `experimentData<-`(edata) %>%
  
  # Update annotation
  `annotation<-`(value='ICD-10 (2016)') %>%
  
  # Update metadata in protocol table
  `protocolData<-`(
    protocolData(.) %>%
      `varMetadata<-`(mdata %>% .[rownames(.) %>% .[.!='outcome'],,drop=F])
  )

## Split data for external validation
set.seed(33)
idx=createDataPartition(medhistdata$outcome,times=1,p=0.8)

## Construct train set
train_set=medhistdata[,idx$Resample1]

## Construct test set
test_set=medhistdata[,-idx$Resample1]

## Extract medical history with no perfect separation
ps_remover_train=extract_nps_mh(train_set)

mh_nps_train=
  train_set[ps_remover_train$key,] %>%
  `exprs<-`(
    exprs(.) %>%
      t() %>%
      as.data.frame() %>%
      rownames_to_column(var='id') %>%
      mutate_all(function(x)ifelse(is.na(x),0,x)) %>%
      column_to_rownames(var='id') %>%
      t()
  )

mh_nps_test=
  test_set[ps_remover_train$key,] %>%
  `exprs<-`(
    exprs(.) %>%
      t() %>%
      as.data.frame() %>%
      rownames_to_column(var='id') %>%
      mutate_all(function(x)ifelse(is.na(x),0,x)) %>%
      column_to_rownames(var='id') %>%
      t()
  )

## Transform medical history to Kaplan-Meier estimate
mh_hlin_nps_train=
  mh_nps_train %>%
  trans_hist_rate(
    interpolation='linear'
    ,verbose=F
  )

mh_hlin_nps_test=
  mh_nps_test %>%
  trans_hist_rate(
    hist_rate=preproc(mh_hlin_nps_train)$hist_rate
    ,interpolation='linear'
    ,verbose=F
  )


# Predictive performance

## Set up internal validation for training set
set.seed(33)
int_val=
  trainControl(
    method='boot'
    ,number=30
    ,savePredictions=T
    ,classProbs=T
    ,summaryFunction=twoClassSummary
    ,allowParallel=F
  )

## Training the models
set.seed(33)
mod_nps=
  suppressWarnings(train(
    outcome~.
    ,data=
      mh_nps_train %>%
      exprs() %>%
      t() %>%
      as.data.frame() %>%
      cbind(pData(mh_nps_train))
    ,method='glm'
    ,metric='ROC'
    ,trControl=int_val
    ,tuneLength=10
  ))

set.seed(33)
mod_hlin_nps=
  suppressWarnings(train(
    outcome~.
    ,data=
      mh_hlin_nps_train %>%
      exprs() %>%
      t() %>%
      as.data.frame() %>%
      cbind(pData(mh_hlin_nps_train))
    ,method='glm'
    ,metric='ROC'
    ,trControl=int_val
    ,tuneLength=10
  ))

## Evaluating the models using testing set
eval_nps=
  mod_nps %>%
  predict(
    newdata=
      mh_nps_test %>%
      exprs() %>%
      t() %>%
      as.data.frame() %>%
      cbind(pData(mh_nps_test))
    ,type='prob'
  ) %>%
  cbind(data.frame(obs=mh_nps_test$outcome)) %>%
  evalm(showplots=F,silent=T) %>%
  .$optres %>%
  .$Group1 %>%
  .['AUC-ROC',]

eval_hlin_nps=
  mod_hlin_nps %>%
  predict(
    newdata=
      mh_hlin_nps_test %>%
      exprs() %>%
      t() %>%
      as.data.frame() %>%
      cbind(pData(mh_hlin_nps_test))
    ,type='prob'
  ) %>%
  cbind(data.frame(obs=mh_hlin_nps_test$outcome)) %>%
  evalm(showplots=F,silent=T) %>%
  .$optres %>%
  .$Group1 %>%
  .['AUC-ROC',]

rbind(
  mutate(eval_nps,hist_rate='No')
  ,mutate(eval_hlin_nps,hist_rate='Yes')
) %>%
  select(hist_rate,everything()) %>%
  rename(
    `Applying historical rate based on Kaplan-Meier estimation?`=hist_rate
    ,AUROC=Score
  ) %>%
  knitr::kable(
    format='html'
    ,caption='Predictive performance using different feature preprocessing'
  ) %>%
  kableExtra::kable_styling(full_width=T)
