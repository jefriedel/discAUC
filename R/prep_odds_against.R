#' Calculate odds against values for \code{x_axis} for probability discounting
#'
#' @param dat Discounting data tibble. \code{AUC_zeroes} should be run first if
#' zero values on the \code{x_axis} will need to be included.
#' @param x_axis Probabilities distance variable
#' @param groupings Variables for grouping (e.g., subject, expeirmental group)
#' as a character or vector of characters
#'
#' @return Original data frame (a tibble) that includes an appended column odds against
#'
#' @export
#' @importFrom dplyr group_by_at vars one_of mutate arrange
#' @importFrom tibble is_tibble
#' @importFrom glue glue
#' @importFrom rlang :=
#'
#' @examples
#' prep_odds_against(examp_PD,
#'   "prob",
#'   groupings = c("subject", "outcome")
#' )
prep_odds_against <- function(dat,
                              x_axis,
                              groupings = NULL) {
  {    if (!tibble::is_tibble(dat)) {
    base::stop("dat must be a tibble")
  }

  if (!base::is.character(x_axis)) {
    base::stop("x_axis must be a string indicating the x-axis variable")
  }

  if (!base::all(base::is.character(groupings)) & !base::is.null(groupings)) {
    base::stop("groupings must be a character or vector of characters for column names")
  }  }

  new_col <- glue::glue("{x_axis}_against")

  dat <- dat %>%
    dplyr::group_by_at(dplyr::vars(dplyr::one_of(groupings))) %>%
    dplyr::mutate(!!{{ new_col }} := (1 - .data[[x_axis]]) / (.data[[x_axis]])) %>%
    dplyr::arrange(.data[[new_col]],
      .by_group = TRUE
    )

  base::return(dat)
}
