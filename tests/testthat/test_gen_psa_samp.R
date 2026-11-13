library(dampack)



test_that("gen_psa_samp runs", {
  expect_silent(test <- gen_psa_samp(params = c("normal_param", "lognorm_param", "beta_param",
                                                "gamma_param", "dirichlet_param", "bootstrap_param"),
                                     dists = c("normal", "log-normal", "beta", "gamma", "dirichlet", "bootstrap"),
                                     parameterization_types = c("mean, sd", "mean, sd", "mean, sd", "mean, sd",
                                                                "value, mean_prop, sd", "value, weight"),
                                     dists_params = list(c(1, 2), c(1, 3), c(.5, .2), c(100, 1),
                                                         data.frame(value = c("level1", "level2", "level3"),
                                                                    mean_prop = c(.1, .4, .5), sd = c(.05, .01, .1)),
                                                         data.frame(value = c(1, 2, 4, 6, 7, 8),
                                                                    weight = c(1, 1, 1, 1, 1, 4))),
                                     nsamp = 100))
})

# "value, weight" used to be missing from the parameterization choices, so it was
# silently dropped and every later parameterization shifted by one position (#175)
test_that("a bootstrap parameter before others keeps their parameterizations aligned", {
  set.seed(175)
  df_boot <- data.frame(value = c(1, 2, 4), weight = c(1, 1, 2))
  samp <- gen_psa_samp(params = c("boot_param", "beta_param", "gamma_param"),
                       dists = c("bootstrap", "beta", "gamma"),
                       parameterization_types = c("value, weight", "a, b", "shape, scale"),
                       dists_params = list(df_boot, c(2, 8), c(4, 0.5)),
                       nsamp = 5000)
  expect_true(all(samp$boot_param >= 1 & samp$boot_param <= 4))
  # beta(a = 2, b = 8) has mean 0.2; a shifted parameterization would not
  expect_equal(mean(samp$beta_param), 0.2, tolerance = 0.02)
  expect_true(all(samp$beta_param > 0 & samp$beta_param < 1))
  # gamma(shape = 4, scale = 0.5) has mean 2
  expect_equal(mean(samp$gamma_param), 2, tolerance = 0.1)
})

test_that("unknown distributions or parameterizations give informative errors", {
  expect_error(gen_psa_samp(params = c("a", "b"), dists = c("beta", "betta"),
                            parameterization_types = c("a, b", "a, b"),
                            dists_params = list(c(1, 1), c(1, 1))),
               "Unknown `dists`: \"betta\"")
  expect_error(gen_psa_samp(params = "a", dists = "beta",
                            parameterization_types = "alpha, beta",
                            dists_params = list(c(1, 1))),
               "Unknown `parameterization_types`: \"alpha, beta\"")
})

test_that("arguments of different lengths give an informative error", {
  expect_error(gen_psa_samp(params = c("a", "b"), dists = "beta",
                            parameterization_types = c("a, b", "a, b"),
                            dists_params = list(c(1, 1), c(1, 1))),
               "must all have the same length")
})

# ---- numerical validation (#215) ------------------------------------------------------------
# method-of-moments helpers reproduce the requested mean and spread exactly

test_that("beta_params() matches the requested mean and standard deviation", {
  l_p <- beta_params(mean = 0.3, sigma = 0.05)
  a <- l_p$alpha
  b <- l_p$beta
  expect_equal(a / (a + b), 0.3)
  expect_equal(sqrt(a * b / ((a + b)^2 * (a + b + 1))), 0.05)
})

test_that("gamma_params() matches the requested mean and standard deviation", {
  l_scale <- gamma_params(mu = 10, sigma = 2)
  expect_equal(l_scale$shape * l_scale$scale, 10)
  expect_equal(sqrt(l_scale$shape) * l_scale$scale, 2)
  l_rate <- gamma_params(mu = 10, sigma = 2, scale = FALSE)
  expect_equal(l_rate$shape / l_rate$rate, 10)
  expect_equal(sqrt(l_rate$shape) / l_rate$rate, 2)
})

test_that("lnorm_params() matches the requested mean and variance", {
  l_p <- lnorm_params(m = 2, v = 0.5)
  expect_equal(exp(l_p$mu + l_p$sigma^2 / 2), 2)
  expect_equal((exp(l_p$sigma^2) - 1) * exp(2 * l_p$mu + l_p$sigma^2), 0.5)
  expect_error(lnorm_params(m = 0, v = 1), "'m' needs to be greater than 0")
})

test_that("dirichlet_params() matches the requested proportions and spread", {
  v_p <- c(0.2, 0.3, 0.5)
  v_alpha <- dirichlet_params(p.mean = v_p, sigma = c(0.05, 0.06, 0.07))
  expect_equal(v_alpha / sum(v_alpha), v_p)
  # the spread is set from the first component: Var = p (1 - p) / (alpha0 + 1)
  expect_equal(sqrt(v_p[1] * (1 - v_p[1]) / (sum(v_alpha) + 1)), 0.05)
  expect_error(dirichlet_params(p.mean = v_p, sigma = c(0.05, 0.06)), "Length of mean different")
})

# every distribution and parameterization of gen_psa_samp() gives samples with the
# expected moments (large nsamp, fixed seed, tolerances of a few standard errors)

test_that("gen_psa_samp() samples every distribution with the right moments", {
  set.seed(215)
  n <- 20000
  samp <- gen_psa_samp(
    params = c("norm", "lnorm_ms", "lnorm_log", "tnorm", "beta_ms", "beta_ab",
               "gamma_ms", "gamma_ss", "boot", "tri", "const"),
    dists = c("normal", "log-normal", "log-normal", "truncated-normal", "beta", "beta",
              "gamma", "gamma", "bootstrap", "triangle", "constant"),
    parameterization_types = c("mean, sd", "mean, sd", "meanlog, sdlog", "mean, sd, ll, ul",
                               "mean, sd", "a, b", "mean, sd", "shape, scale",
                               "value, weight", "ll, ul, mode", "val"),
    dists_params = list(c(5, 1), c(2, 0.5), c(0, 0.25), c(0.5, 0.2, 0.3, 0.9),
                        c(0.3, 0.05), c(2, 8), c(10, 2), c(4, 0.5),
                        data.frame(value = c(1, 2, 4), weight = c(1, 1, 2)),
                        c(0, 1, 0.25), 7),
    nsamp = n
  )
  expect_equal(nrow(samp), n)
  expect_equal(mean(samp$norm), 5, tolerance = 0.01)
  expect_equal(sd(samp$norm), 1, tolerance = 0.02)
  expect_equal(mean(samp$lnorm_ms), 2, tolerance = 0.01)
  expect_equal(sd(samp$lnorm_ms), 0.5, tolerance = 0.03)
  expect_equal(mean(samp$lnorm_log), exp(0.25^2 / 2), tolerance = 0.01)
  expect_true(all(samp$tnorm >= 0.3 & samp$tnorm <= 0.9))
  expect_equal(mean(samp$beta_ms), 0.3, tolerance = 0.01)
  expect_equal(sd(samp$beta_ms), 0.05, tolerance = 0.03)
  expect_equal(mean(samp$beta_ab), 0.2, tolerance = 0.01)
  expect_equal(mean(samp$gamma_ms), 10, tolerance = 0.01)
  expect_equal(sd(samp$gamma_ms), 2, tolerance = 0.03)
  expect_equal(mean(samp$gamma_ss), 2, tolerance = 0.01)
  # bootstrap means of resamples drawn with probabilities proportional to the weights
  expect_equal(mean(samp$boot), (1 * 1 + 2 * 1 + 4 * 2) / 4, tolerance = 0.01)
  expect_equal(mean(samp$tri), (0 + 1 + 0.25) / 3, tolerance = 0.01)
  expect_true(all(samp$const == 7))
})

test_that("gen_psa_samp() Dirichlet parameterizations give proportions with the right means", {
  set.seed(2150)
  n <- 20000
  samp <- gen_psa_samp(
    params = c("d_msd", "d_n", "d_alpha"),
    dists = c("dirichlet", "dirichlet", "dirichlet"),
    parameterization_types = c("value, mean_prop, sd", "value, n", "value, alpha"),
    dists_params = list(data.frame(value = c("a1", "a2", "a3"), mean_prop = c(0.2, 0.3, 0.5),
                                   sd = c(0.05, 0.06, 0.07)),
                        data.frame(value = c("b1", "b2"), n = c(30, 70)),
                        data.frame(value = c("c1", "c2", "c3"), alpha = c(2, 3, 5))),
    nsamp = n
  )
  expect_equal(unname(rowSums(samp[, c("a1", "a2", "a3")])), rep(1, n))
  expect_equal(unname(colMeans(samp[, c("a1", "a2", "a3")])), c(0.2, 0.3, 0.5), tolerance = 0.01)
  expect_equal(unname(colMeans(samp[, c("b1", "b2")])), c(0.3, 0.7), tolerance = 0.01)
  expect_equal(unname(colMeans(samp[, c("c1", "c2", "c3")])), c(0.2, 0.3, 0.5), tolerance = 0.01)
})
