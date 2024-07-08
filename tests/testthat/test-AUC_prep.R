test_that("AUC prep arranges data", {
  test_dat <- dplyr::tibble(
    delay = base::rep(base::c(1 / 7, 1, 2, 4, 26, 52), 2),
    indiff = base::c(base::c(95, 75, 50, 20, 5, 1), c(95, 75, 50, 20, 5, 1) + .25),
    sub = base::c(base::rep(1, 6), base::rep(2, 6))
  ) %>%
    dplyr::arrange(indiff)

  # Arrange data for delay
  expect_equivalent(
    prep_AUC(test_dat, "indiff", "delay", "sub", prob_disc = FALSE),
    test_dat %>% dplyr::arrange(sub, delay)
  )

  # Arrange data for probability
  expect_equivalent(
    prep_AUC(test_dat, "indiff", "delay", "sub", prob_disc = TRUE),
    test_dat %>% dplyr::arrange(sub, delay)
  )
})
