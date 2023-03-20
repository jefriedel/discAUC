#' Shared ordinal x-axis
#'
#' Helper function to create ordinal values for x-axis variable.
#' This helper function is designed to be used if the x-axis values
#' are not identical across every set of indifference points. For example,
#' if one subject was exposed to delays of 1 day and 1 month and a
#' second subject was exposed to delays of 1 week and 1 month. In
#' such a case, \code{prep_ordinal_all()} would return ordinal delays of (1, 3)
#' for subject 1 and ordinal delays of (2, 3) for subject 2. If 0 exists, will
#' be coded as 0.
#'
#' @param dat Discounting data tibble
#' @param x_axis Delays/probabilities/social distance variable
#' @param prob_disc Boolean for probability discounting, if set to true
#' function will calculate odrinals based on descending \code{x_axis}
#' values which would be in line with increasing odds against.
#'
#' @return Tibble that has ordinal values for each \code{x_axis} value
#' based on all possible \code{x_axis} values.
#'
#' @export
#' @import dplyr
#' @importFrom tibble is_tibble
#' @importFrom rlang :=
#'
#' @examples
#' library(dplyr)
#'
#' PD <- tibble(
#'   prob = c(
#'     c(.05, 1 / 100, 1 / 300, 1 / 750, 1 / 1000, 1 / 3000),
#'     c(.1, 1 / 100, 1 / 300, 1 / 750, 1 / 1000, 1 / 4000)
#'   ),
#'   indiff = c(c(95, 75, 50, 20, 5, 1), c(95, 75, 50, 20, 5, 1) + .25),
#'   sub = c(rep(1, 6), rep(2, 6))
#' )
#'
#' prep_ordinal_all(PD, "prob", prob_disc = TRUE)
prep_ordinal_all <- function(dat,
                             x_axis,
                             prob_disc = FALSE) {
  {
    if (!tibble::is_tibble(dat)) {
      stop("dat must be a tibble")
    }

    if (!base::is.character(x_axis)) {
      base::stop("x_axis must be a string indicating the x-axis variable")
    }

    if (!base::is.logical(prob_disc)) {
      base::stop("prob_disc must be a boolean, either TRUE or FALSE")
    }

    # if(!all(is_character(groupings)) & !is.null(groupings)){
    #   stop("groupings must be a character or vector of characters for column names")
    # }
  } # Checks


  
  # For probability discounting, create ordinal on descending probabilities

  # Changed from symbols to strings through rlang
  x_col <- x_axis
  new_col <- base::paste0(x_axis, "_ord")

  # Removed because groups are not necessary for this function
  # if(is_empty(enexprs(...))){
  #   grps = NULL
  # }else{
  #   grps = as.symbol(...)
  # }

  ord_tibble <- dplyr::tibble({{ x_col }} := dat %>%
    dplyr::pull(.data[[x_col]]) %>%
    base::unique())

  if (prob_disc) {

    # order by probability descending (which will be increasing odds against)
    ord_tibble <- ord_tibble %>%
      dplyr::arrange(dplyr::desc(.data[[x_col]]))
  } else {
    ord_tibble <- ord_tibble %>%
      dplyr::arrange(.data[[x_col]])
  }

  if (((base::min(ord_tibble) == 0) & !prob_disc) | # Min = 0 and delay, social, etc
    ((base::max(ord_tibble) == 1) & prob_disc)) { # Max = 100% and prob discounting
    ord_tibble <- ord_tibble %>%
      dplyr::mutate({{ new_col }} := dplyr::row_number() - 1)
  } else {
    ord_tibble <- ord_tibble %>%
      dplyr::mutate({{ new_col }} := dplyr::row_number())
  }

  dat <- dat %>%
    dplyr::left_join(ord_tibble,
                     by = {{x_axis}})

  base::return(dat)
}


#' Ordinal x-axis by grouping
#'
#' Helper function to create ordinal values for x-axis variable.
#' This helper function is designed to be used if the x-axis values
#' are identical across every set of indifference points or if you
#' desire ordinal x-axis values by subject. For the second case,
#' if one subject was exposed to delays of 1 day and 1 month and a
#' second subject was exposed to delays of 1 week and 1 month. In
#' such a case, \code{prep_ordinal()} would return ordinal delays of (1, 2)
#' for subject 1 and ordinal delays of (1, 2) for subject 2. If zeroes exist,
#' will code as ordinal 0.
#'
#' @param dat Discounting data tibble
#' @param x_axis Delays/probabilities/social distance variable
#' @param groupings Variables for grouping (e.g., subject, expeirmental group)
#' @param prob_disc Boolean for probability discounting, if set to true
#' function will calculate ordinals based on descending \code{x_axis}
#' values which would be in line with increasing odds against.
#'
#' @return Tibble that has ordinal values for each \code{x_axis} value
#' based on all possible \code{x_axis} values. Output tibble is arranged
#' in the same order as original tibble.
#'
#' @export
#' @import dplyr
#' @importFrom tibble is_tibble
#' @importFrom rlang :=
#'
#' @examples
#' library(dplyr)
#'
#' PD <- tibble(
#'   prob = c(
#'     c(.05, 1 / 100, 1 / 300, 1 / 750, 1 / 1000, 1 / 3000),
#'     c(.1, 1 / 100, 1 / 300, 1 / 750, 1 / 1000, 1 / 4000)
#'   ),
#'   indiff = c(c(95, 75, 50, 20, 5, 1), c(95, 75, 50, 20, 5, 1) + .25),
#'   sub = c(rep(1, 6), rep(2, 6))
#' )
#'
#' # Scramble data to demonstrate preserved original order
#' PD <- PD %>%
#'   mutate(scramble = rnorm(NROW(PD), 0, 1)) %>%
#'   arrange(scramble)
#'
#' PD
#'
#' prep_ordinal(PD, "prob", prob_disc = TRUE, "sub")
prep_ordinal <- function(dat,
                         x_axis,
                         groupings = NULL,
                         prob_disc = FALSE) {
  
  {    if (!tibble::is_tibble(dat)) {
    base::stop("dat must be a tibble")
  }


  if (!base::is.character(x_axis)) {
    base::stop("x_axis must be a string indicating the x-axis variable")
  }

  if (!base::is.logical(prob_disc)) {
    base::stop("prob_disc must be a boolean, either TRUE or FALSE")
  }

  if (!base::all(base::is.character(groupings)) & !base::is.null(groupings)) {
    base::stop("groupings must be a character or vector of characters for column names")
  }  } # Checks

  orig_group = TRUE
  
  #Clear no visibile binding note
  fake_grouping <- NULL
  
  #Handling no grouping factor
  if(is.null(groupings)){
    
    dat <- dat %>%
      dplyr::mutate(fake_grouping = 1)
    
    orig_group = FALSE
    
    groupings = "fake_grouping"
    
  }
  
  # Kept x_col because of older version
  x_col <- x_axis
  new_col <- base::paste0(x_axis, "_ord")

  # Create original rows for preservation
  dat <- dat %>%
    dplyr::mutate(orig_row = dplyr::row_number())



  if (base::is.null(groupings)) {
    # dat <- dat %>%
    #   arrange(NULL) %>%
    #   group_by(NULL)
  } else {

    # grps = vars(one_of(groupings))

    dat <- dat %>%
      dplyr::arrange(dplyr::vars(dplyr::one_of(groupings))) %>%
      dplyr::group_by_at(dplyr::vars(dplyr::one_of(groupings)))
  }

  if (prob_disc) {
    dat <- dat %>%
      dplyr::arrange(dplyr::desc(.data[[x_col]]), .by_group = TRUE)
  } else {
    dat <- dat %>%
      dplyr::arrange(.data[[x_col]], .by_group = TRUE)
  }

   dat <-
    dat %>%
    # temp_bol checks for experimental 0 and adjust ordinal
    dplyr::mutate(
      temp_bol = (base::max(.data[[x_col]]) == 1 & prob_disc) |
        (base::min(.data[[x_col]]) == 0 & !prob_disc),
      {{ new_col }} :=
        base::ifelse(.data$temp_bol,
          dplyr::row_number() - 1,
          dplyr::row_number()
        )
    ) %>%
    dplyr::arrange("orig_row") %>%
    dplyr::select(-"orig_row", -"temp_bol")

  # dat = semi_join(dat,
  #                 dat %>%
  #                   filter({{x_axis}}==1) %>%
  #                   select(...)) %>%
  #   #mutate("{{x_axis}}_ord" := !!(enquo(x_axis))-1)
  #   mutate("{{x_axis}}_ord" := !!"{{x_axis}}_ord"-1)

  # Error occurrs above. Tries to reference x_axis and throws error, I think

  # Just recode, split the data into groups that include zero delays and non-zero delays
  # Then the do row_number() - 1 for group one and row_number for group two.

  # Still post on website on about {{x_axis}}_ord issue

  # arrange(orig_row) %>%
  # select(-orig_row)
  
  #delete fake grouping if it exists
  if(!orig_group){
    
    dat<- 
      dat %>%
      ungroup() %>%
      dplyr::select(-fake_grouping)
    
  }

  base::return(dat)
}
