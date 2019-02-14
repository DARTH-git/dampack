context("dsa oneway")
library(dampack)

# creation of example data
nsamps <- 20
p1 <- seq(0, 1, length.out = nsamps)
e1 <- data.frame("s1" = 4 * p1 - 2,
                 "s2" = 5 * p1 + 3)
c1 <- data.frame("s1" = 1e4 + 1000 * p1,
                 "s2" = 1e2 + 2000 * p1)

p2 <- seq(4, 5, length.out = nsamps)
e2 <- data.frame("s1" = 2 * p2^2 - 10,
                 "s2" = 3 * p2^2 + 4 * p2 - 20)
c2 <- data.frame("s1" = 1e5 - 2000 * p2,
                 "s2" = 1e6 - 1300 * p2)

strategies <- c("s1", "s2")

ps <- list("p1" = p1,
           "p2" = p2)

es <- list("p1" = e1,
           "p2" = e2)

cs <- list("p1" = c1,
           "p2" = c2)

# class creation
dsa <- create_dsa_oneway(ps, es, strategies, cs)

# print
print(dsa)

# plots
o <- owsa(dsa)
owsa_tornado(o, strategy = "s2", min_rel_diff = -1)
owsa_opt_strat(o)
