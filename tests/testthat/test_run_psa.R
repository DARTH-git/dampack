test <- gen_psa_samp(params = c("normalboi", "loggyboi","betaboi","gammaboi","dirchboi"),
                     dist = c("normal", "log-normal","beta","gamma","dirichlet"),
                     parameterization_type = c("mean, sd", "mean, sd","mean, sd","mean, sd", "value, mean_prop, sd"),
                     dist_params = list(c(1,2),c(1,3), c(.5,.2), c(100,1), data.frame(value = c("egg","tofu","bacon"), mean_prop = c(.1,.4,.5), sd = c(.05, .01, .1))),
                     n_samp =100)

test_func <- function(params, cheetos){
  normalboi <- params[["normalboi"]]
  loggyboi <- params[["loggyboi"]]

  effect <- normalboi + loggyboi *cheetos
  cost <- -normalboi - loggyboi

  output <- data.frame(strategy = "mystrat",
                       effect = effect,
                       cost = cost)
  return(output)
}
