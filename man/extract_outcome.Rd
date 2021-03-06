% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_outcome-function.R
\name{extract_outcome}
\alias{extract_outcome}
\title{Extract outcome of subjects}
\usage{
extract_outcome(
  population,
  icd10_event,
  latest_event = min,
  day_to_event = -1,
  icd10_nonevent,
  latest_nonevent = max,
  day_to_nonevent = 0,
  verbose = T
)
}
\arguments{
\item{population}{Target population data, a data frame with rows for visits
and standardized columns (please see Details below). If only a single
healthcare provider, then make \code{healthcare_id} consisting only a single
identification number.}

\item{icd10_event}{Event code, a character of ICD-10 code for either
diagnosis or procedure, that identify subjects with event outcome. This
parameter should be a regular expression if detecting >1 ICD-10 codes. The
detection is not exact but anywhere within the character sequence; thus,
the code with more than k-mer will be extracted.}

\item{latest_event}{Latest date of event, a function to choose
\code{admission_date} with the \code{icd10_event} among many for each
subject. This date will be adjusted with \code{day_to_event} to limit visits
accounted for medical histories, since these are considerably before the
outcome. The function is conceivably \code{min()} and \code{max()} for the
earliest (preferred) and latest date, respectively.}

\item{day_to_event}{Day to event, an integer to adjust \code{latest_event}
for limiting visits accounted for medical histories. These are considerably
before the outcome, i.e. up to -1 day.}

\item{icd10_nonevent}{Non-event code, a character of ICD-10 code for either
diagnosis or procedure, that identify subjects with non-event outcome. This
parameter should be a regular expression if detecting >1 ICD-10 codes. Make
this parameter as \code{''} (character of none) if all except event codes
are used. The detection is not exact but anywhere within the character
sequence; thus, the code with more than k-mer will be extracted.}

\item{latest_nonevent}{Latest date of non-event, a function to choose
\code{admission_date} with the \code{icd10_nonevent} among many for each
subject. This date will be adjusted with \code{day_to_nonevent} to limit
visits accounted for medical histories, since these are considerably up to
the day of the outcome. The function is conceivably \code{min()} and
\code{max()} for the earliest and latest date (preferred), respectively.}

\item{day_to_nonevent}{Day to non-event, an integer to adjust
\code{latest_nonevent} for limiting visits accounted for medical histories.
These are considerably up to the day of the outcome, i.e. up to 0 day.}

\item{verbose}{Verbosity, a logical indicating whether progress should be
shown.}
}
\value{
Subject list, a data frame with rows for unique subjects and columns
of \code{subject_id}, \code{latest_date}, and \code{outcome}. The last column
is a factor of which non-event is the first class between non-event and
event.
}
\description{
This function makes a list of subjects with either event or non-event
outcome, that visit healthcare providers up to the predefined latest date.
}
\details{
Target population data consisted \code{visit_id},\code{subject_id},
\code{healthcare_id}, \code{admission_date}, \code{code}, and
\code{db_start_date}.  Columns of \code{visit_id},\code{subject_id}, and
\code{healthcare_id} are characters of identification numbers identifying
respectively unique visits, subjects, and healthcare providers. Columns of
\code{admission_date} and \code{db_start_date} are dates of subject admission
in a visit and database start of recording period, respectively. Column of
\code{code} is a character of ICD-10 code for either diagnosis or procedure
regardless the number of digit or k-mer.
}
\examples{

## Create input example
data(visit_cap)
data(visit_ffs)
data(visit_drg)
data(diagnosis)

population=
  list(visit_cap,visit_ffs,visit_drg) \%>\%
  lapply(select,visit_id,subject_id,healthcare_id,admission_date) \%>\%
  do.call(rbind,.) \%>\%
  left_join(diagnosis,by='visit_id') \%>\%
  filter(!code_type\%in\%c('Admission diagnosis')) \%>\%
  select(-code_type) \%>\%
  mutate(db_start_date=as.Date('2015-01-01')) \%>\%
  .[!duplicated(.),]

## Extract outcome of subjects
outcome=extract_outcome(population,'O1[4-5]',min,-1,'Z3[3-7]',max,0)
}
\keyword{outcome}
