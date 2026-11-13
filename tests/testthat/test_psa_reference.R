context("summary.psa() and calculate_icers_psa() against a reference strategy (#209, #202)")

# Four PSA samples; the difference "New" - "Ref" falls in a different quadrant in each:
#   sample  inc effect  inc cost  quadrant
#   1          +0.2       +100    NE (more effective, more costly)
#   2          +0.2        -50    SE (more effective, less costly)
#   3          -0.1        -50    SW (less effective, less costly)
#   4          -0.1       +100    NW (less effective, more costly)
df_cost <- data.frame(Ref = c(1000, 1000, 1000, 1000), New = c(1100, 950, 950, 1100))
df_eff <- data.frame(Ref = c(1, 1, 1, 1), New = c(1.2, 1.2, 0.9, 0.9))
psa_q <- make_psa_obj(df_cost, df_eff, strategies = c("Ref", "New"))

test_that("summary.psa() is unchanged without ref_str and wtp", {
  expect_identical(names(summary(psa_q)), c("Strategy", "meanCost", "meanEffect"))
})

test_that("summary.psa(ref_str = ) gives mean differences and quadrant shares", {
  s <- summary(psa_q, ref_str = "Ref")
  r_new <- s[s$Strategy == "New", ]
  r_ref <- s[s$Strategy == "Ref", ]
  expect_equal(r_new$meanIncCost, mean(df_cost$New - df_cost$Ref))
  expect_equal(r_new$meanIncEffect, mean(df_eff$New - df_eff$Ref))
  expect_equal(unlist(r_new[, c("propNE", "propSE", "propSW", "propNW")], use.names = FALSE),
               rep(0.25, 4))
  expect_true(all(is.na(r_ref[, c("meanIncCost", "meanIncEffect", "propNE", "propSE", "propSW", "propNW")])))
  expect_error(summary(psa_q, ref_str = "Nope"), "must be one of the strategies")
})

test_that("summary.psa(wtp = ) gives mean NMB and the share of cost-effective samples", {
  s <- summary(psa_q, wtp = 1000)
  expect_equal(s$meanNMB, unname(1000 * colMeans(df_eff) - colMeans(df_cost)))
  expect_false(any(c("meanIncNMB", "propCostEffective") %in% names(s)))

  s_ref <- summary(psa_q, ref_str = "Ref", wtp = 1000)
  inmb <- 1000 * (df_eff$New - df_eff$Ref) - (df_cost$New - df_cost$Ref)  # 100, 250, -50, -200
  r_new <- s_ref[s_ref$Strategy == "New", ]
  expect_equal(r_new$meanIncNMB, mean(inmb))
  expect_equal(r_new$propCostEffective, mean(inmb > 0))
  expect_equal(r_new$propCostEffective, 0.5)
})

data("cancer_psa_data")
psa_big <- make_psa_obj(cancer_psa_data$cost, cancer_psa_data$effectiveness,
                        cancer_psa_data$parameters, c("Chemotherapy", "Radiation therapy", "Surgery"))

test_that("calculate_icers_psa() passes ref_str, wtp, and nhb to calculate_icers()", {
  s <- summary(psa_big)
  expected <- calculate_icers(s$meanCost, s$meanEffect, s$Strategy,
                              ref_str = "Surgery", wtp = 50000, nhb = TRUE, currency = psa_big$currency)
  obtained <- calculate_icers_psa(psa_big, ref_str = "Surgery", wtp = 50000, nhb = TRUE)
  expect_equal(obtained, expected)
})

test_that("calculate_icers_psa(uncertainty = TRUE) adds intervals for the reference comparison", {
  x <- calculate_icers_psa(psa_big, uncertainty = TRUE, ref_str = "Surgery", wtp = 50000)
  expect_identical(attr(x, "ref_str"), "Surgery")
  r <- x[x$Strategy == "Chemotherapy", ]
  d_cost <- cancer_psa_data$cost[[1]] - cancer_psa_data$cost[[3]]
  d_eff <- cancer_psa_data$effectiveness[[1]] - cancer_psa_data$effectiveness[[3]]
  expect_equal(r$Lower_95_Inc_Cost_ref, unname(quantile(d_cost, 0.025)))
  expect_equal(r$Upper_95_Inc_Cost_ref, unname(quantile(d_cost, 0.975)))
  expect_equal(r$Lower_95_Inc_Effect_ref, unname(quantile(d_eff, 0.025)))
  expect_equal(r$Upper_95_iNMB_ref, unname(quantile(50000 * d_eff - d_cost, 0.975)))
  r_ref <- x[x$Strategy == "Surgery", ]
  expect_true(is.na(r_ref$Lower_95_Inc_Cost_ref))
  # the frontier-based columns and their existing intervals are unchanged
  x0 <- calculate_icers_psa(psa_big, uncertainty = TRUE)
  expect_equal(as.data.frame(x)[, names(x0)], as.data.frame(x0)[, names(x0)])
})

test_that("print.icers() keeps reference intervals out of the sequential view", {
  x <- calculate_icers_psa(psa_big, uncertainty = TRUE, ref_str = "Surgery")
  out_seq <- capture.output(print(x))
  expect_false(any(grepl("Inc_Cost_ref", out_seq, fixed = TRUE)))
  out_ref <- capture.output(print(x, ref = "reference"))
  expect_true(any(grepl("Lower_95_Inc_Cost_ref", out_ref, fixed = TRUE)))
})
