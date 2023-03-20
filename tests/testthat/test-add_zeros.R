test_that("Flagging original vs imputed values", {
  test_dat <- tibble(
    delay = c(
      c(1 / 7, 1, 2, 4, 26, 104),
      c(1 / 14, 1, 2, 4, 26, 52)
    ),
    indiff = c(c(95, 75, 50, 20, 5, 1), c(95, 75, 50, 20, 5, 1) + .25),
    sub = c(rep(1, 6), rep(2, 6))
  )

  expect_equivalent(
    AUC_zeros(test_dat,
      indiff = "indiff",
      x_axis = "delay",
      amount = 100,
      groupings = "sub",
      prob_disc = FALSE
    ) %>% pull(orig),
    rep(c(FALSE, rep(TRUE, 6)), 2)
  )


  # Data that has zeros in it
  test_dat[1, 1] <- 0

  # TEST CURRENTLY FAILS BECAUSE FUNCTION NOT WORKING CORRECTLY
  expect_equivalent(
    AUC_zeros(test_dat,
      indiff = "indiff",
      x_axis = "delay",
      amount = 100,
      groupings = "sub",
      prob_disc = FALSE
    ) %>% pull(orig),
    c(rep(TRUE, 6), FALSE, rep(TRUE, 6))
  )
})

test_that("Outputs imputed zeros correctly", {
  delays <- c(1 / 7, 1, 2, 4, 26, 104)

  test_dat <- tibble(
    delay = rep(delays, 2),
    indiff = c(c(95, 75, 50, 20, 5, 1), c(95, 75, 50, 20, 5, 1) + .25),
    sub = c(rep(1, 6), rep(2, 6))
  )

  expect_equivalent(
    AUC_zeros(test_dat,
      indiff = "indiff",
      x_axis = "delay",
      amount = 100,
      groupings = "sub",
      prob_disc = FALSE
    ) %>% pull(delay),
    rep(c(0, delays), 2)
  )


  # Data that has zeros in it
  test_dat[1, 1] <- 0

  # Correctly handle data with some zeros
  expect_equivalent(
    AUC_zeros(test_dat,
      indiff = "indiff",
      x_axis = "delay",
      amount = 100,
      groupings = "sub",
      prob_disc = FALSE
    ) %>% pull(delay),
    c(0, delays[2:NROW(delays)], 0, delays)
  )

  # Correctly handle data with some zeros
  expect_equivalent(
    AUC_zeros(test_dat %>% filter(sub == 1),
      indiff = "indiff",
      x_axis = "delay",
      amount = 100,
      groupings = "sub",
      prob_disc = FALSE
    ) %>% pull(delay),
    c(0, delays[2:NROW(delays)])
  )
})
