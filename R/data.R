#' Delay discounting data
#'
#' Delay discounting data with repeated measures for subjects across delayed outcomes.
#' Data were obtained from a subset of data from DeHart et al. (2020).
#'
#'
#' Note: The DD data shares the same indifference points used in the PD data.
#' The PD data were created by using the DD data and using
#'  probabilities instead of delays. The PD was created to demonstrate features
#'  of the discAUC package and does not represent real data.
#'
#' @format A data frame with 360 rows and 4 variables:
#' \describe{
#'   \item{subject}{Subject ID. Positive values are experimentally obtained.
#'   -987.987 are median indifference points. -1 and -2 values have indifference
#'   points of all 0 and all 1, respectively. These extra data were added for testing
#'   and debugging to ensure that AUC calculations will result in 0 when all
#'   indifference points are zero and 1 when all indifference points are 1.}
#'   \item{delay_months}{Delay to receiving the outcome, in months}
#'   \item{outcome}{Delayed outcome type (all were scaled to $100)}
#'   \item{prop_indiff}{Indifference point scaled to the maximum amount of each
#'   outcome. The maximum amount was the number of servings of each outcome
#'   worth $100.}
#' }
#' @source \doi{10.1002/jeab.623}
"examp_DD"

#' Probability discounting data
#'
#' Probability discounting data with repeated measures for subjects across
#' unlikely outcomes. 
#'
#' Note: The PD data shares the same indifference points used in the DD data.
#' The PD data were created by using the DD data and using
#'  probabilities instead of delays. The PD was created to demonstrate features
#'  of the discAUC package and does not represent real data.
#'
#' @format A data frame with 360 rows and 4 variables:
#' \describe{
#'   \item{subject}{Subject ID. Positive values are experimentally obtained.
#'   -987.987 are median indifference points. -1 and -2 values have indifference
#'   points of all 0 and all 1, respectively. These extra data were added for testing
#'   and debugging to ensure that AUC calculations will result in 0 when all
#'   indifference points are zero and 1 when all indifference points are 1.}
#'   \item{prob}{Probability of receiving the outcome}
#'   \item{outcome}{Delayed outcome type (all were scaled to $100)}
#'   \item{prop_indiff}{Indifference point scaled to the maximum amount of each
#'   outcome. The maximum amount was the number of servings of each outcome
#'   worth $100.}
#' }
#' @source \doi{10.1002/jeab.623}
"examp_PD"
