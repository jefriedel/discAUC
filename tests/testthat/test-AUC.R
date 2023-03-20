test_that("Reg AUC 0 and 1", {
  test_dat <- dplyr::tibble(
    delay = base::rep(base::c(1 / 7, 1, 2, 4, 26, 52), 3),
    indiff = base::c(
      base::rep(100, 6),
      base::rep(0, 6),
      c(95, 75, 50, 20, 5, 1)
    ),
    sub = base::c(
      base::rep(1, 6),
      base::rep(2, 6),
      base::rep(3, 6)
    )
  ) %>%
    dplyr::arrange(indiff)

  test_dat <- prep_AUC(test_dat,
    indiff = "indiff",
    x_axis = "delay",
    grouping = "sub"
  ) %>%
    ungroup()

  test_val_zero <-
    AUC(
      test_dat %>% filter(sub == 2),
      indiff = "indiff",
      x_axis = "delay",
      amount = 100,
      groupings = "sub",
      imp_zero = FALSE
    ) %>%
    pull(AUC)

  test_val_one <-
    AUC(
      test_dat %>% filter(sub == 1),
      indiff = "indiff",
      x_axis = "delay",
      amount = 100,
      groupings = "sub",
      imp_zero = TRUE
    ) %>%
    pull(AUC)

  expect_equal(0, test_val_zero)
  expect_equal(1, test_val_one)
})

test_that("Log AUC 0 and 1", {
  test_dat <- dplyr::tibble(
    delay = base::rep(base::c(1 / 7, 1, 2, 4, 26, 52), 3),
    indiff = base::c(
      base::rep(100, 6),
      base::rep(0, 6),
      c(95, 75, 50, 20, 5, 1)
    ),
    sub = base::c(
      base::rep(1, 6),
      base::rep(2, 6),
      base::rep(3, 6)
    )
  ) %>%
    dplyr::arrange(indiff)

  test_dat <- prep_AUC(test_dat,
    indiff = "indiff",
    x_axis = "delay",
    grouping = "sub"
  ) %>%
    ungroup()

  test_val_zero <-
    AUC(
      test_dat %>% filter(sub == 2),
      indiff = "indiff",
      x_axis = "delay",
      amount = 100,
      groupings = "sub",
      imp_zero = FALSE,
      type = "log"
    ) %>%
    pull(AUC)

  test_val_one <-
    AUC(
      test_dat %>% filter(sub == 1),
      indiff = "indiff",
      x_axis = "delay",
      amount = 100,
      groupings = "sub",
      imp_zero = TRUE,
      type = "log"
    ) %>%
    pull(AUC)

  expect_equal(0, test_val_zero)
  expect_equal(1, test_val_one)
})

test_that("Ord AUC 0 and 1", {
  test_dat <- dplyr::tibble(
    delay = base::rep(base::c(1 / 7, 1, 2, 4, 26, 52), 3),
    indiff = base::c(
      base::rep(100, 6),
      base::rep(0, 6),
      c(95, 75, 50, 20, 5, 1)
    ),
    sub = base::c(
      base::rep(1, 6),
      base::rep(2, 6),
      base::rep(3, 6)
    )
  ) %>%
    dplyr::arrange(indiff)

  test_dat <- prep_AUC(test_dat,
    indiff = "indiff",
    x_axis = "delay",
    grouping = "sub"
  ) %>%
    ungroup()

  test_val_zero <-
    AUC(
      test_dat %>% filter(sub == 2),
      indiff = "indiff",
      x_axis = "delay",
      amount = 100,
      groupings = "sub",
      imp_zero = FALSE,
      type = "ord"
    ) %>%
    pull(AUC)

  test_val_one <-
    AUC(
      test_dat %>% filter(sub == 1),
      indiff = "indiff",
      x_axis = "delay",
      amount = 100,
      groupings = "sub",
      imp_zero = TRUE,
      type = "ord"
    ) %>%
    pull(AUC)

  expect_equal(0, test_val_zero)
  expect_equal(1, test_val_one)
})

test_that("AUC calcs are correct", {
  test_dat <- dplyr::tibble(
    delay = base::c(0, 1 / 7, 1, 2, 4, 26, 52),
    indiff = base::c(100, 95, 75, 50, 20, 5, 1),
    sub = base::rep(1, 7)
  )

  max_amt <- 100
  max_delay <- 52

  lin_AUC <- AUC(
    dat = test_dat,
    x_axis = "delay",
    indiff = "indiff",
    groupings = "sub",
    amount = 100
  ) %>%
    pull(AUC)

  lin_AUC_manual <- test_dat %>%
    mutate(
      delay = delay / max_delay,
      indiff = indiff / max_amt,
      trap = (delay - lag(delay)) *
        (indiff + lag(indiff)) / 2
    ) %>%
    pull(trap) %>%
    sum(na.rm = TRUE)

  ord_AUC <- AUC(
    dat = test_dat,
    x_axis = "delay",
    indiff = "indiff",
    groupings = "sub",
    amount = 100,
    type = "ord"
  ) %>%
    pull(AUC)

  ord_AUC_manual <-
    test_dat %>%
    mutate(
      delay = (row_number() - 1) / 6,
      indiff = indiff / max_amt,
      trap = (delay - lag(delay)) *
        (indiff + lag(indiff)) / 2
    ) %>%
    pull(trap) %>%
    sum(na.rm = TRUE)

  log_AUC <- AUC(
    dat = test_dat,
    x_axis = "delay",
    indiff = "indiff",
    groupings = "sub",
    amount = 100,
    type = "log"
  ) %>%
    pull(AUC)

  test_dat <- test_dat %>%
    prep_log_AUC(
      x_axis = "delay",
      type = "adjust"
    )

  log_AUC_manual <- test_dat %>%
    mutate(
      delay = log_delay / test_dat %>%
        pull(log_delay) %>%
        max(),
      indiff = indiff / max_amt,
      trap = (delay - lag(delay)) *
        (indiff + lag(indiff)) / 2
    ) %>%
    pull(trap) %>%
    sum(na.rm = TRUE)

  expect_equal(lin_AUC, lin_AUC_manual)
  expect_equal(ord_AUC, ord_AUC_manual)
  expect_equal(log_AUC, log_AUC_manual)
})

test_that("AUC of single tibble works",
  {
    test_dat <- dplyr::tibble(
      delay = base::rep(base::c(1 / 7, 1, 2, 4, 26, 52), 3),
      indiff = base::c(
        base::rep(100, 6),
        base::rep(0, 6),
        c(95, 75, 50, 20, 5, 1)
      ),
      sub = base::c(
        base::rep("Max", 6),
        base::rep("Zero", 6),
        base::rep("Reg", 6)
      )
    )
    
    expect_equal(
      AUC(dat = test_dat %>% filter(sub == "Reg"),
        indiff = "indiff",
        x_axis = "delay",
        amount = 100) %>%
      pull(AUC),
      0.1100549, #Value determined when original test was build
      tolerance = 10^-7) 
    
    expect_equal(
      AUC(dat = test_dat %>% filter(sub == "Zero"),
          indiff = "indiff",
          x_axis = "delay",
          amount = 100) %>%
        pull(AUC),
      0.001373626, #Value determined when original test was build
      tolerance = 10^-7) 
    
    expect_equal(
      AUC(dat = test_dat %>% filter(sub == "Max"),
          indiff = "indiff",
          x_axis = "delay",
          amount = 100) %>%
        pull(AUC),
      1, #Value determined when original test was build
      tolerance = 10^-7) 
    
    expect_equal(
      AUC(dat = test_dat %>% filter(sub == "Max"),
          indiff = "indiff",
          x_axis = "delay",
          amount = 100,
          imp_zero = FALSE) %>%
        pull(AUC),
      0.9972527, #Value determined when original test was build
      tolerance = 10^-7)
    
    expect_equal(
      AUC(dat = test_dat %>% filter(sub == "Reg"),
          indiff = "indiff",
          x_axis = "delay",
          amount = 100,
          type = "ord") %>%
        pull(AUC),
      0.4925, #Value determined when original test was build
      tolerance = 10^-7) 
    
    expect_equal(
      AUC(dat = test_dat %>% filter(sub == "Reg"),
          indiff = "indiff",
          x_axis = "delay",
          amount = 100,
          type = "log") %>%
        pull(AUC),
      0.5277344, #Value determined when original test was build
      tolerance = 10^-7)


    expect_equal( 
    AUC(dat = test_dat %>%
            filter(sub == "Reg") %>% 
            bind_cols(
              tibble(prob = c(0.95,.85,.5,.1,.01,0.001))
            ),
        indiff = "indiff",
        x_axis = "prob",
        amount = 100,
        prob_disc = TRUE) %>%
      pull(AUC),
    0.04176305, #Value determined when original test was build
    tolerance = 10^-7)
    
            
  })
