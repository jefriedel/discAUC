test_that("Correction works", {
  test_dat <- tibble(
    delay = c(0, 1 / 7, 1, 2, 4, 26, 52),
    indiff = c(100, 95, 75, 50, 20, 5, 1)
  )

  corr <- 1

  log_delays <- prep_log_AUC(
    dat = test_dat,
    x_axis = "delay",
    log_base = 10,
    dec_offset = TRUE,
    type = "corr",
    correction = corr
  ) %>%
    pull(log_delay)

  expect_equal(log_delays[1], 0)
  expect_equal(log_delays[2], log((1 / 7) + corr, base = 10))
})


test_that("Adjusting works", {
  test_dat <- tibble(
    delay = c(0, 1 / 7, 1, 2, 4, 26, 52),
    indiff = c(100, 95, 75, 50, 20, 5, 1)
  )


  base <- 10

  log_delays <- prep_log_AUC(
    dat = test_dat,
    x_axis = "delay",
    log_base = base,
    dec_offset = TRUE,
    type = "adjust"
  ) %>%
    pull(log_delay)

  expect_equal(log_delays[1], 0)

  adjustment_correction <- test_dat %>%
    filter(delay != 0) %>%
    pull(delay) %>%
    log(base = base) %>%
    diff() %>%
    mean()

  expect_equal(log_delays[2], log(1 / 7, base = base) +
    adjustment_correction +
    abs(log(1 / 7, base = base)))
})

test_that("IHS works", {
  test_dat <- tibble(
    delay = c(0, 1 / 7, 1, 2, 4, 26, 52),
    indiff = c(100, 95, 75, 50, 20, 5, 1)
  )

  base <- 10

  log_delays <- prep_log_AUC(
    dat = test_dat,
    x_axis = "delay",
    log_base = base,
    dec_offset = TRUE,
    type = "IHS"
  ) %>%
    pull(log_delay)

  expect_equal(log_delays[1], 0)

  expect_equal(
    log_delays[2],
    asinh(1 / 7)
  )
})
