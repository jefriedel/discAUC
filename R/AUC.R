#' Area Under the Curve for Discounting Data
#'
#' @param dat Discounting data tibble
#' @param indiff Indifference points Variable in \code{dat}
#' @param x_axis Delays/probabilities/social distance variable in \code{dat}
#' @param prob_disc Boolean for whether data are probability discounting
#' @param max_x_axis Numeric; Maximum possible value in \code{x_axis}. If left
#' \code{NULL}, the function will determine and use the maximum value in 
#' the \code{x_axis} variable. Useful for comparing AUC across non-standard
#' data sets where the maximum \code{x_axis} value is not shared.
#' @param amount Numeric; Maximum amount of indifference points. (A in discounting models.)
#' @param groupings Variables for grouping (e.g., subject, experimental group)
#' as a character or vector of characters
#' @param imp_zero Boolean for whether indifference points at \code{x_axis = 0} (e.g., delay = 0, odds against = 0, etc.) should be added to the data.
#' @param type String for the type of AUC that should be calculated. Acceptable 
#' values are one of \code{c("linear","log","ordinal")}
#' @param log_base If using logarithmic, what is the base of the log
#'
#' @return Tibble or with AUC by all grouping factors. If no grouping factor specified
#' then a tibble with one AUC will be returned.
#' @export
#' @import dplyr
#' @importFrom tibble is_tibble
#' @importFrom glue glue
#'
#' @examples
#' AUC(examp_DD,
#'   indiff = "prop_indiff",
#'   x_axis = "delay_months",
#'   amount = 1,
#'   type = "linear",
#'   prob_disc = FALSE,
#'   groupings = c("subject", "outcome")
#' )
AUC <- function(dat,
                indiff,
                x_axis,
                prob_disc = FALSE,
                max_x_axis = NULL,
                amount,
                groupings = NULL,
                imp_zero = TRUE,
                type = "linear",
                log_base = 2) {
  {
    type_options <- base::c("linear", "log", "ordinal")

    if (!tibble::is_tibble(dat)) {
      base::stop("dat must be a tibble")
    }

    if (!base::is.character(indiff)) {
      base::stop("indiff Column must be a character string")
    }

    if (!base::is.character(x_axis)) {
      base::stop("x_axis must be a string indicating the x-axis variable")
    }

    if (!base::is.logical(prob_disc)) {
      base::stop("prob_disc must be a boolean, either TRUE or FALSE")
    }

    if (!base::is.null(max_x_axis) & !base::is.numeric(max_x_axis)) {
      base::stop("max_x_axis must be NULL or a numeric")
    }

    if (!base::is.numeric(amount) | amount <= 0) {
      base::stop("amount must be a numeric greater than 0")
    }

    if (!base::all(base::is.character(groupings)) & !base::is.null(groupings)) {
      base::stop("groupings must be a character or vector of characters for column names")
    }

    if (!base::is.logical(imp_zero)) {
      base::stop("imp_zero must be a logical with the value of TRUE or FALSE")
    }

    if (!((type == "linear") | (type == "ordinal") | (type == "log")) &
      (base::NROW(type) > 1)) {
      base::stop("type must be a string of \"linear\", \"ordinal\", or \"log\"")
    }

    if (!base::is.numeric(log_base) | log_base <= 0) {
      base::stop("log_base must be a numeric greater than 0")
    }

    rm(type_options)
  } # Checks

  #Start
  orig_group = TRUE
  fake_grouping = NULL
    
  #Handling no grouping factor
  if(is.null(groupings)){
    
    dat <- dat %>%
      dplyr::mutate(fake_grouping = 1)
    
    orig_group = FALSE
    
    groupings = "fake_grouping"
    
  }
  
  # Decided to have a new name to track what was original and
  # whatever the monster of the end column name is
  new_x_axis <- x_axis

  # If zero values are to be imputed, do that first.
  if (imp_zero) {
    dat <- AUC_zeros(
      dat,
      indiff = indiff,
      x_axis = new_x_axis,
      prob_disc = prob_disc,
      amount = amount,
      groupings = groupings
    )
  }

  # If probability discounting, calculate odds against
  if (prob_disc) {
    dat <- prep_odds_against(dat,
      x_axis = x_axis,
      groupings = groupings
    )

    new_x_axis <- glue::glue("{new_x_axis}_against")
  }

  # Linear is the "default" AUC calculation, so nothing has to be done
  # for the current version
  if (type == "linear") {

  }

  # Calculate log(x_axis) based on the adjust type that I think is best
  if (type == "log") {
    dat <- prep_log_AUC(dat,
      x_axis = new_x_axis,
      log_base = log_base,
      type = "adjust",
      correction = 1,
      dec_offset = TRUE
    )

    # Update the x_axis name for downstream functions.
    new_x_axis <- glue::glue("log_{new_x_axis}")
  }
  # Calculate ordinal values
  # Uses ordinal_all because it is a bit safer

  if (type == "ord") {

    # NOTE that the flag for prob_disc was set to FALSE here because
    # the calculating of the odds_against was seperated out to a sepaate
    # function above. Therefore, if the ordinal value is taken it is
    # taken against the odds against values which go in ascending order (like
    # they were delays).
    dat <- prep_ordinal_all(dat,
      x_axis = new_x_axis,
      prob_disc = FALSE
    )

    new_x_axis <- glue::glue("{new_x_axis}_ord")
  }

  dat <- prep_AUC(
    dat = dat,
    x_axis = new_x_axis,
    groupings = groupings,
    prob_disc = prob_disc
  )

  # Pull max delay/probability if none is specified. Changed to occur right
  # before AUC is run so it doesn't have to pull a bunch of times
  if (base::is.null(max_x_axis)) {
    max_x_axis <- dat %>%
      dplyr::pull({{ new_x_axis }}) %>%
      base::max()
  }

  #Trapezoids are created from first to last
  dat <- dat %>%
    dplyr::mutate(
      trap =
        ((dplyr::lead(.data[[new_x_axis]]) - .data[[new_x_axis]]) / max_x_axis) *
          (.data[[indiff]] + dplyr::lead(.data[[indiff]])) / 2 / amount
    )

  # Separated to new line in case future want to export trapezoids
  out <- dat %>%
    dplyr::summarize(AUC = base::sum(.data$trap, na.rm = TRUE))

  #delete fake grouping if it exists
  if(!orig_group){
    
    out<- 
      out %>%
      ungroup() %>%
      dplyr::select(-fake_grouping)
    
  }
  
  base::return(out)
}
