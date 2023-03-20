test_that("Ord by subject", {
  test_dat <- tibble(
    delay = c(
      c(1 / 7, 1, 2, 4, 26, 104),
      c(1 / 14, 1, 2, 4, 26, 52)
    ),
    indiff = c(c(95, 75, 50, 20, 5, 1), c(95, 75, 50, 20, 5, 1) + .25),
    sub = c(rep(1, 6), rep(2, 6))
  )

  expect_equivalent(
    prep_ordinal(test_dat, "delay", "sub") %>%
      pull(delay_ord),
    rep(1:6, 2)
  )

  test_PD <-
    tibble(
      prob = c(
        c(.05, 1 / 100, 1 / 300, 1 / 750, 1 / 1000, 1 / 3000),
        c(.1, 1 / 100, 1 / 300, 1 / 750, 1 / 1000, 1 / 4000)
      ),
      indiff = c(c(95, 75, 50, 20, 5, 1), c(95, 75, 50, 20, 5, 1) + .25),
      sub = c(rep(1, 6), rep(2, 6))
    )

  expect_equivalent(
    prep_ordinal(test_PD, "prob", "sub", prob_disc = TRUE) %>%
      pull(prob_ord),
    rep(1:6, 2)
  )
})

test_that("Ord across all subjects", {
  test_dat <- tibble(
    delay = c(
      c(1 / 7, 1, 2, 4, 26, 104),
      c(1 / 14, 1, 2, 4, 26, 52)
    ),
    indiff = c(c(95, 75, 50, 20, 5, 1), c(95, 75, 50, 20, 5, 1) + .25),
    sub = c(rep(1, 6), rep(2, 6))
  )

  expect_equivalent(
    prep_ordinal_all(test_dat, "delay") %>%
      pull(delay_ord),
    c(2:6, 8, 1, 3:7)
  )

  test_PD <-
    tibble(
      prob = c(
        c(.05, 1 / 100, 1 / 300, 1 / 750, 1 / 1000, 1 / 3000),
        c(.1, 1 / 100, 1 / 300, 1 / 750, 1 / 1000, 1 / 4000)
      ),
      indiff = c(c(95, 75, 50, 20, 5, 1), c(95, 75, 50, 20, 5, 1) + .25),
      sub = c(rep(1, 6), rep(2, 6))
    )

  expect_equivalent(
    prep_ordinal_all(test_PD, "prob", prob_disc = TRUE) %>%
      pull(prob_ord),
    c(2:7, 1, 3:6, 8)
  )
})
