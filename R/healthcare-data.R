#' Healthcare providers.
#'
#' A dataset containing the healthcare providers. Subjects are sampled from
#' all available providers in all of the countries/cities, weighted by household
#' numbers in a provider and subject numbers in a household. Based on insurance
#' utilization, there are three categories of household. Category \code{1}
#' refers to a household that all of the subjects never utilize the insurance.
#' Category \code{2} refers to a household that all or some of the subjects have
#' ever utilized the insurance for primary care only. Category \code{3} refers
#' to a household that all or some of the subjects have ever utilized the
#' insurance for primary and secondary/tertiary care. In this table,
#' \code{sample_cat} implicitly refers to these categories but not the same.
#'
#' @format A data frame with 237,717 rows and 5 columns:
#' \describe{
#'   \item{healthcare_id}{Provider ID. This connects to \code{subject},
#'       \code{visit_cap}, \code{visit_ffs}, and \code{visit_drg} data.}
#'   \item{sample_cat}{Category of this provider. Category \code{1} and \code{2}
#'       refer to primary care. Category \code{3} refers to secondary/tertiary
#'       care.}
#'   \item{sampling_weight}{Weight that determines how likely a subject sample
#'       taken from this provider over others.}
#'   \item{healthcare_country}{Country code where this provider is located.}
#'   \item{healthcare_city}{City code where this provider is located.}
#' }
#' @source Artificial data
'healthcare'