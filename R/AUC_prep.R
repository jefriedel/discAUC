#' AUC data peperation helper function
#'
#' Helper function to take AUC tibble and preprocess for other AUC
#' calculations
#'
#' @param dat Discounting data tibble
#' @param indiff Indifference points Variable
#' @param x_axis Delays/probabilities/social distance variable
#' @param groupings Variables for grouping (e.g., subject, expeirmental group)
#' as a character or vector of characters
#' @param prob_disc Boolean for probability discounting (MAYBE NOT NECESSARY
#' PULLED OUT ODDS AGAINST TO DIFFERENT FUNCTION)
#'
#' @return Tibble that is grouped and arranged by \code{groupings}
#' and \code{x_axis} (or \code{x_axis_against}, if probability
#' discounting)
#'
#' @import dplyr
#' @importFrom tibble is_tibble
#'
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' # Prep single set of data
#' DD <- tibble(
#'   delay = c(1 / 7, 1, 2, 4, 26, 52),
#'   indiff = c(95, 75, 50, 20, 5, 1)
#' ) %>%
#'   arrange(delay)
#'
#' prep_AUC(dat = DD, indiff = "indiff", x_axis = "delay")
#' # Prep multiple subject data
#'
#' # Create DD data disorganize by delay
#' DD <- tibble(
#'   delay = rep(c(1 / 7, 1, 2, 4, 26, 52), 2),
#'   indiff = c(c(95, 75, 50, 20, 5, 1), c(95, 75, 50, 20, 5, 1) + .25),
#'   sub = c(rep(1, 6), rep(2, 6))
#' ) %>%
#'   arrange(delay)
#'
#' # Group by subject and organize by subject and delay
#' prep_AUC(dat = DD, indiff = "indiff", x_axis = "delay", groupings = "sub")
#'
#'
#' # Probability discounting with subjects and different outcomes
#'
#' # Create PD data and disorganize by probability
#' PD <- tibble(
#'   prob = rep(c(.1, 1 / 100, 1 / 300, 1 / 750, 1 / 1000, 1 / 3000), 4),
#'   value = rep(c(c(95, 75, 50, 20, 5, 1), c(95, 75, 50, 20, 5, 1) + .25), 2),
#'   sub = rep(c(rep(1, 6), rep(2, 6)), 2),
#'   outcome = c(rep("money", 12), rep("cigarettes", 12))
#' ) %>%
#'   arrange(prob)
#'
#' # Calculate odds against, organize by subject, outcome, odds against
#' prep_AUC(PD,
#'   indiff = "value",
#'   x_axis = "prob",
#'   groupings = c("sub", "outcome"),
#'   prob_disc = TRUE
#' )
prep_AUC <- function(dat,
                     indiff = NULL,
                     x_axis,
                     groupings = NULL,
                     prob_disc = FALSE) {
  
  {  if (!tibble::is_tibble(dat)) {
    base::stop("dat must be a tibble")
  }

  if (!base::is.character(indiff) & !base::is.null(indiff)) {
    base::stop("indiff Column must be a symbol or NULL")
  }

  if (!base::is.character(x_axis)) {
    base::stop("x_axis must be a string indicating the x-axis variable")
  }

  if (!base::is.logical(prob_disc)) {
    base::stop("prob_disc must be a boolean, either TRUE or FALSE")
  }

  if (!base::all(base::is.character(groupings)) & !base::is.null(groupings)) {
    base::stop("groupings must be a character or vector of characters for column names")
  }} # Checks

  orig_group <- TRUE
  
  #Clear no visibile binding note
  fake_grouping <- NULL
  
  #Handling no grouping factor
  if(is.null(groupings)){
    
    dat <- dat %>%
      dplyr::mutate(fake_grouping = 1)
    
    orig_group = FALSE
    
    groupings = "fake_grouping"
    
  }
  
  x_col <- x_axis
  new_col <- base::paste0(x_axis, "_against")


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

  dat <- dat %>%
    dplyr::arrange(.data[[x_col]],
      .by_group = TRUE
    )

  #delete fake grouping if it exists
  if(!orig_group){
    
    dat<- 
      dat %>%
      ungroup() %>%
      dplyr::select(-fake_grouping)
    
  }
  
  base::return(dat)
}
