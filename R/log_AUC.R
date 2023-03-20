#' Calculate log x_axis values for AUClog
#'
#' @section Correction types for handling zero x-axis values:
#' "Corr" adds a set correction value to each \code{x_axis}
#' value and then takes the log of those values. "Adjust" implements increasing
#' the \code{x_axis} values by the average difference between the log values on the \code{x_axis}.
#' "IHS" calculates the inverse hyperbolic sine for the \code{x_axis},
#' which is different than the logarithm
#' but is highly correlated with log transformed values. The IHS transformation
#' does not require adjustments because \code{IHS(0) == 0}.
#'
#' @param dat Discounting data tibble. \code{AUC_zeroes} should be run first if
#' zero values on the \code{x_axis} will need to be included.
#' @param x_axis Delays/probabilities/social distance variable
#' @param log_base Base of the logarithm
#' @param type Type of correction to handle 0 values on x_axis. Acceptable values
#' are "corr"., "adjust", and "IHS". "Corr" adds a set value to each x_axis
#' value and then takes the log of those values. "Adjust" implements increasing
#' the x_axis values by the average difference between the log values on the x_axis.
#' "IHS" calculates the inverse hyperbolic sine, which is different than the logarithm
#' but is highly correlated with log transformed values. The IHS transformation
#' does not require corrections
#' @param correction If \code{type == "corr"} this value is what is added to the
#' x_axis prior to taking the log values.
#' @param dec_offset If \code{TRUE}, offsets the log x_axis values if the lowest
#' non-zero x_axis value is a decimal. This calculation is preferred because if
#' x_axis values are negative then the log values will be negative. The negative
#' log values can cause inconsistencies in how AUC is calculated.
#'
#' @return Original data frame (a tibble) that includes an appended column with log scale
#' version of x_axis
#'
#' @import dplyr
#' @importFrom glue glue
#' @importFrom tibble is_tibble
#' @importFrom rlang :=
#' @export
#'
#' @examples
#'
#' prep_log_AUC(
#'   dat = examp_DD,
#'   x_axis = "delay_months",
#'   log_base = 10,
#'   dec_offset = TRUE,
#'   type = "adjust",
#'   correction = 1
#' )
prep_log_AUC <- function(dat,
                         x_axis,
                         log_base = 2,
                         type = "adjust",
                         correction = 1,
                         dec_offset = TRUE) {
  {    if (!tibble::is_tibble(dat)) {
    stop("dat must be a tibble")
  }

  if (!base::is.character(x_axis)) {
    base::stop("x_axis must be a string indicating the x-axis variable")
  }

  if (!base::is.numeric(log_base) | (log_base <= 0)) {
    base::stop("log_base must be a number greater than 0")
  }

  if (!(type %in% (c("corr", "adjust", "IHS")))) {
    base::stop("type must be a string of either \"corr\", \"adjust\", or \"IHS\".")
  }

  if (!base::is.numeric(correction) | (correction <= 0)) {
    base::stop("correction must be a number greater than 0")
  }

  if (!base::is.logical(dec_offset)) {
    base::stop("dec_offset must be either TRUE or FALSE")
  }  } # Checks

  x_vals <- dat %>%
    dplyr::pull({{ x_axis }}) %>%
    base::unique()

  # For ease seperated out of single tidy function
  log_vals <- 
    dplyr::tibble(orig = x_vals) %>%
    dplyr::arrange("orig") %>%
    dplyr::mutate(
      log_val = base::log(.data[["orig"]], log_base),
      log_diff = .data[["log_val"]] - dplyr::lag(.data[["log_val"]])
    )

  # It is possible that other forms will be implemented in the future,
  # so keeping the type variable and just setting it

  if (type == "adjust") {

    # Find mean differences between x_axis
    mean_diff <-
      log_vals %>%
      dplyr::filter(!base::is.na(.data[["log_diff"]]) & 
                      !base::is.infinite(.data[["log_diff"]])
                    ) %>%
      dplyr::pull(.data[["log_diff"]]) %>%
      base::mean()
  } else if (type == "corr") {
    mean_diff <- 1
  }

  inc_zero <- base::min(x_vals) == 0

  if (!inc_zero) {
    mean_diff <- 0
  }

  # The offset exists to eliminate negative x_axis values for clean log values
  if (dec_offset) {
    log_offset <- 
      log_vals %>%
      dplyr::filter(!base::is.infinite(.data[["log_val"]])) %>%
      dplyr::pull(.data[["log_val"]]) %>%
      base::min()

    if (log_offset < 0) {
      log_offset <- base::abs(log_offset)
    } else {
      log_offset <- 0
    }
  } else {
    log_offset <- 0
  }

  if (type == "adjust") {
    log_vals <- 
      log_vals %>%
      dplyr::mutate(log_val_adj = .data[["log_val"]] + 
                      log_offset + mean_diff)
  } else if (type == "corr") {
    log_vals <- 
      log_vals %>%
      dplyr::mutate(log_val_adj = base::log(.data[["orig"]] + correction,
        base = log_base
      ))
  } else if (type == "IHS") {
    log_vals <- 
      log_vals %>%
      dplyr::mutate(log_val_adj = base::asinh(.data[["orig"]]))
  }

  if (inc_zero) {
    log_vals[1, "log_val_adj"] <- 0
  }

  new_col <- glue::glue("log_{x_axis}")

  log_vals <-
    log_vals %>%
    dplyr::select(
      dplyr::all_of(
        base::c("orig","log_val_adj"))) %>%
    dplyr::rename(
      {{ x_axis }} := "orig",
      {{ new_col }} := "log_val_adj"
    )

  dat <- dplyr::left_join(dat,
    log_vals,
    by = x_axis
  )

  base::return(dat)
}

