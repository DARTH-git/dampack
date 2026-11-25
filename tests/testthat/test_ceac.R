library(dampack)

# test the class
test_that("ceac has all methods we'd expect", {
  current_methods <- as.vector(methods(class = ceac))
  expected_methods <- c("plot.ceac", "summary.ceac")
  expect_setequal(current_methods, expected_methods)
})

# test class creation

## setup
data("cancer_psa_data")
wtp <- seq(1000, 141000, by = 10000)
psa_obj <- make_psa_obj(cancer_psa_data$cost,
                        cancer_psa_data$effectiveness,
                        cancer_psa_data$parameters,
                        cancer_psa_data$strategies)

test_that("result has class 'ceac'", {
  c <- ceac(psa_obj, wtp)
  expect_true(inherits(c, "data.frame"))
  expect_true(inherits(c, "ceac"))
})

test_that("handles missing strategy", {
  psa_missing <- make_psa_obj(cancer_psa_data$cost,
                              cancer_psa_data$effectiveness,
                              cancer_psa_data$parameters)
  c_missing <- ceac(psa_missing, wtp)
  expected_generic_strat <- factor(c("Strategy_1", "Strategy_2", "Strategy_3"), ordered = TRUE)
  obtained_generic_strat <- sort(unique(c_missing$Strategy))
  expect_equal(expected_generic_strat, obtained_generic_strat)
})


# test methods

## summary
test_that("message is correct in summary.ceac", {
  c <- ceac(psa_obj, wtp)
  df_sum <- summary(c)
  expect_equal(df_sum$optimal_strategy, c("Radio", "Chemo"))
})

test_that("summary.ceac is correct when the frontier switches twice", {
  # frontier optimal on A -> B -> C, so two switches and three intervals
  ceac_obj <- data.frame(WTP = c(10000, 30000, 50000, 70000, 90000),
                         Strategy = c("A", "A", "B", "C", "C"),
                         On_Frontier = TRUE,
                         stringsAsFactors = FALSE)
  class(ceac_obj) <- c("ceac", "data.frame")
  df_sum <- summary(ceac_obj)
  expect_equal(nrow(df_sum), 3)
  expect_equal(df_sum$range_min, c(10000, 50000, 70000))
  expect_equal(df_sum$range_max, c(50000, 70000, 90000))
  expect_equal(df_sum$optimal_strategy, c("A", "B", "C"))
})

test_that("summary.ceac summarizes a real psa whose frontier switches many times", {
  # df_example_psa_elc switches optimal strategy three times, so the summary must
  # have four intervals. built through the public api - make_psa_obj() then
  # ceac() - so the test also proves the bad shape was reachable from real data,
  # which a hand-built ceac object cannot.
  data("df_example_psa_elc")
  l_psa <- make_psa_obj(cost = df_example_psa_elc[, 1:6],
                        effectiveness = df_example_psa_elc[, 7:12])
  df_ceac <- ceac(l_psa, seq(1000, 150000, 1000))
  df_sum <- summary(df_ceac)

  # expected intervals derived independently, by run-length encoding the frontier
  df_front <- df_ceac[df_ceac$On_Frontier == TRUE, ]
  l_runs <- rle(as.character(df_front$Strategy))
  v_starts <- c(1, head(cumsum(l_runs$lengths), -1) + 1)

  expect_equal(nrow(df_sum), length(l_runs$values))
  expect_equal(df_sum$optimal_strategy, l_runs$values)
  expect_equal(df_sum$range_min, df_front$WTP[v_starts])
  expect_equal(df_sum$range_max, c(df_front$WTP[v_starts[-1]], max(df_front$WTP)))
  expect_false(anyNA(df_sum))
  expect_equal(anyDuplicated(df_sum), 0L)
})

## plot
test_that("plot.ceac produces ggplot object", {
  ceac_obj <- ceac(psa_obj, wtp)
  gf <- plot(ceac_obj, frontier = TRUE)
  expect_s3_class(gf, "ggplot")

  gnof <- plot(ceac_obj, frontier = FALSE)
  expect_s3_class(gnof, "ggplot")

  custom_breaks <- plot(ceac_obj, xbreaks = seq(0, 140))
  expect_s3_class(custom_breaks, "ggplot")

  # black and white
  expect_s3_class(plot(ceac_obj, col = "bw"), "ggplot")
})

# max.col() treats values within a relative tolerance of 1e-5 as ties and, by default, breaks
# them at random, so strategies whose NMB differs by a few dollars at the scale of ~1e6 were
# picked at random (#167). Here B's NMB is exactly $1 higher than A's in every sample.
test_that("near-tied strategies are not chosen at random (#167)", {
  n_sim <- 500
  df_c <- data.frame(A = rep(0, n_sim), B = rep(0, n_sim))
  df_e <- data.frame(A = rep(10, n_sim), B = rep(10 + 1e-5, n_sim))
  psa_tie <- make_psa_obj(df_c, df_e, strategies = c("A", "B"))
  for (seed in 1:3) {
    set.seed(seed)
    df_ceac <- ceac(psa_tie, 1e5)
    expect_equal(df_ceac$Proportion[df_ceac$Strategy == "B"], 1)
    expect_equal(calc_evpi(psa_tie, 1e5)$EVPI, 0)
    df_elc <- calc_exp_loss(psa_tie, 1e5)
    expect_true(df_elc$On_Frontier[df_elc$Strategy == "B"])
  }
})

test_that("exact ties are broken deterministically, in favor of the first strategy", {
  df_same <- data.frame(A = rep(1, 50), B = rep(1, 50))
  psa_same <- make_psa_obj(df_same, df_same, strategies = c("A", "B"))
  v_prop <- vapply(1:5, function(seed) {
    set.seed(seed)
    df_ceac <- ceac(psa_same, 100)
    df_ceac$Proportion[df_ceac$Strategy == "A"]
  }, numeric(1))
  expect_equal(v_prop, rep(1, 5))
})
