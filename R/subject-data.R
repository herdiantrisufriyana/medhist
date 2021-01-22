#' Subject sample (single-payer health insurance client).
#'
#' A dataset containing the subjects or clients. Subjects are sampled from
#' all available providers in all of the countries, weighted by household
#' numbers in a provider and subject numbers in a household. Based on insurance
#' utilization, there are three categories of household. Category \code{1}
#' refers to a household that all of the subjects never utilize the insurance.
#' Category \code{2} refers to a household that all or some of the subjects have
#' ever utilized the insurance for primary care only. Category \code{3} refers
#' to a household that all or some of the subjects have ever utilized the
#' insurance for primary and secondary/tertiary care.
#'
#' @format A data frame with 1,697,452 rows and 11 columns:
#' \describe{
#'   \item{subject_id}{Subject ID. This connects to \code{healthcare},
#'       \code{visit_cap}, \code{visit_ffs}, and \code{visit_drg} data.}
#'   \item{householder_id}{Householder ID. A household only has one householder.
#'       Several subjects may be registered to a household. This ID is the
#'       \code{subject_id} of the householder.}
#'   \item{healthcare_id}{Provider ID. The provider is the one of which this
#'       subject registered to (not always one where this subject visits).}
#'   \item{birth_date}{Birth date of this subject.}
#'   \item{family_status}{Categorical variable of family status, consisting
#'       \code{person} and \code{other} for non-family household, or
#'       \code{husband}, \code{wife}, and \code{child} for family household.
#'       These may be mixed.}
#'   \item{sex}{Categorical variable of sex, consisting \code{female},
#'       \code{male}, and \code{unspecified}.}
#'   \item{marital_status}{Categorical variable of marital status, consisting
#'       \code{single}, \code{married}, \code{divorced/widowed}, and
#'       \code{unspecified}.}
#'   \item{insurance_class}{Categorical variable of insurance class, consisting
#'       \code{first}, \code{second}, and \code{third}. The \code{first} class
#'       is the one indicating the highest socioeconomic class compared to the
#'       \code{second} and \code{third} classes}
#'   \item{occupation_segment}{Categorical variable of occupation segment of
#'       the householder (not necessarily this subject). This consists of
#'       \code{central-government-paid householder}, \code{local-government-paid
#'       householder}, \code{employee householder} (private company),
#'       \code{employer householder}, and \code{unemployed householder}.}
#'   \item{subject_country}{country code where the provider is located.}
#'   \item{subject_city}{city code where the provider is located.}
#' }
#' @source Artificial data
'subject'