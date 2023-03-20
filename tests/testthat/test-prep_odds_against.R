
test_that("odds against calculations", {
  test_dat <- tibble(
    delay = rep(c(1 / 7, 1, 2, 4, 26, 52), 2),
    indiff = c(c(95, 75, 50, 20, 5, 1), c(95, 75, 50, 20, 5, 1) + .25),
    sub = c(rep(1, 6), rep(2, 6))
  )

  output <- prep_odds_against(test_dat, "delay", "sub")

  delays <- output %>%
    pull(delay)

  expect_equal((1 - delays) / delays, output %>% pull(delay_against))
})
