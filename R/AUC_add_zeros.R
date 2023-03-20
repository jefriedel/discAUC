#' Impute zero delay/100\% likely indifference points
#'
#' As defined by Myerson et al. (2001) the indifference point at 0 delay (100\%
#' likelihood) is set to 0. This function will add that indifference point,
#' wherever it is missing. If the 0 delay (100\% likelihood) is included in the
#' data then it will not be overwritten.
#'
#' @param dat Discounting data tibble
#' @param indiff Indifference points Variable
#' @param x_axis Delays/probabilities/social distance variable
#' @param amount Amount of the larger delayed/probablistic/etc. outcome
#' (A in discounting formulas)
#' @param groupings Variables for grouping (e.g., subject, expeirmental group)
#' as a character or vector of characters
#' @param prob_disc Boolean for probability discounting, if set to true
#' function will calculate and report odds against \code{x_axis}
#'
#' @return Tibble that is grouped by \code{groupings} but in the same
#' order as supplied to the function. If \code{prob_disc == FALSE}, then the
#' function will add indifference points of \code{amount} at \code{x_axis = 0}.
#' If \code{prob_disc == TRUE}, then the
#' function will add indifference points of \code{amount} at \code{x_axis = 1}.
#' Additionally, a \code{orig} column will be added to indicate whether the
#' indifference point was included in the data or was imputed.
#'
#' @export
#' @import dplyr
#' @importFrom tibble is_tibble
#' @importFrom rlang :=
#'
#'
#' @examples
#' AUC_zeros(
#'   examp_DD,
#'   indiff = "prop_indiff",
#'   x_axis = "delay_months",
#'   amount = 1,
#'   groupings = c("subject", "outcome")
#' )
#'
#' AUC_zeros(
#'   examp_PD,
#'   indiff = "prop_indiff",
#'   x_axis = "prob",
#'   amount = 1,
#'   groupings = c("subject", "outcome"),
#'   prob_disc = TRUE
#' )
AUC_zeros <- function(dat,
                      indiff,
                      x_axis,
                      amount,
                      groupings = NULL,
                      prob_disc = FALSE) {

    {    if (!tibble::is_tibble(dat)) {
    stop("dat must be a tibble")
  }

  if (!base::is.character(indiff) & !base::is.null(indiff)) {
    base::stop("indiff Column must be a symbol or NULL")
  }

  if (!base::is.character(x_axis)) {
    base::stop("x_axis must be a string indicating the x-axis variable")
  }

  if (!is.logical(prob_disc)) {
    base::stop("prob_disc must be a boolean, either TRUE or FALSE")
  }

  if (!base::all(base::is.character(groupings)) & !base::is.null(groupings)) {
    base::stop("groupings must be a character or vector of characters for column names")
  }  } # Checks

  orig_group <- TRUE
  
  #Clear no visibile binding note
  orig_order <- fake_grouping <- NULL
  
  #Handling no grouping factor
  if(is.null(groupings)){
    
    dat <- dat %>%
      dplyr::mutate(fake_grouping = 1)
  
  orig_group = FALSE
  
  groupings = "fake_grouping"
      
  }

  # Add original row numbers and identify experimental data
  dat <- dat %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      orig = TRUE,
      orig_order = dplyr::row_number()
    )

  # Find groups of data that already have 0 x_axis
  zero_list <- dat %>%
    dplyr::filter(.data[[x_axis]] == 0) %>%
    dplyr::select(dplyr::all_of(groupings))

  # Remove data with 0 x_axis
  missing_zero <- dplyr::anti_join(dat,
    zero_list,
    by = groupings
  )

  out <- missing_zero %>%
    dplyr::group_by_at(dplyr::vars(dplyr::one_of(groupings))) %>% # Group data
    dplyr::slice_head(n = 1) %>% # Take the top row for each data groupings
    dplyr::select(dplyr::all_of((groupings))) %>% # select only the grouping values
    dplyr::mutate({{ x_axis }} := base::ifelse(prob_disc, 1, 0), # Set x_axis
      {{ indiff }} := amount, # Set indifference point to Amount
      orig = FALSE # Set flag that the data was imputed so that users
                   # are not confused by the imputed data
    ) %>% 
    
    dplyr::bind_rows(dat) %>% # Add zero data back into original file
    dplyr::group_by_at(dplyr::vars(dplyr::one_of(groupings)))

  # Group by x_axis then arrange the data by x_axis
  if (prob_disc) {
    out <- out %>%
      dplyr::arrange(dplyr::desc(.data[[x_axis]]), .by_group = TRUE)
  } else {
    out <- out %>%
      dplyr::arrange(.data[[x_axis]], .by_group = TRUE)
  }

  # Create fake original row number for imputed zeros
  out <- out %>%
    dplyr::mutate(orig_order = base::ifelse(is.na(.data$orig_order),
      dplyr::lead(orig_order) - .5,
      .data$orig_order
    )) %>%
    dplyr::arrange(orig_order) %>%
    dplyr::select(-orig_order) %>%
    dplyr::ungroup()
  
  #delete fake grouping if it exists
  if(!orig_group){
    
    out<- 
      out %>%
      dplyr::select(-fake_grouping)
    
  }

  base::return(out)
}
