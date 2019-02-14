context("dsa twoway")
library(dampack)

# creation of example data
nsamps <- 20
params <- expand.grid("parm1" = seq(0, 1, length.out = nsamps),
                      "parm2" = seq(4, 5, length.out = nsamps))
effect <- data.frame("s1" = 5 * params$parm1 + 4 * params$parm2 ,
                     "s2" = 2 * params$parm1^2 - 3 * params$parm2 + 10)
cost <- data.frame("s1" = 1e4 + 1000 * params$parm1 - 900 * params$parm2,
                   "s2" = 1e5 + 1000 * params$parm1 - 900 * params$parm2)

strategies <- c("s1", "s2")

# class creation
dsa <- create_dsa_twoway(params, effect, strategies, cost)

# print
print(dsa)

# plots
t <- twsa(dsa)
plot(t)
owsa_opt_strat(t)
