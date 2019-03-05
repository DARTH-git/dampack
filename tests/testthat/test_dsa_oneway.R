context("dsa oneway")
library(dampack)

# creation of example data
nsamps <- 20
p1 <- data.frame("parameter" = "p1",
                 "parmval" = seq(0, 1, length.out = nsamps),
                 stringsAsFactors = FALSE)
e1 <- data.frame("s1" = 4 * p1$parmval - 2,
                 "s2" = 5 * p1$parmval + 3)
c1 <- data.frame("s1" = 1e4 + 1000 * p1$parmval,
                 "s2" = 1e2 + 2000 * p1$parmval)

p2 <- data.frame("parameter" = "p2",
                 "parmval" = seq(4, 5, length.out = nsamps),
                 stringsAsFactors = FALSE)
e2 <- data.frame("s1" = 2 * p2$parmval^2 - 10,
                 "s2" = 3 * p2$parmval^2 + 4 * p2$parmval - 20)
c2 <- data.frame("s1" = 1e5 - 2000 * p2$parmval,
                 "s2" = 1e6 - 1300 * p2$parmval)

strategies <- c("s1", "s2")

ps <- rbind(p1, p2)
es <- rbind(e1, e2)
cs <- rbind(c1, c2)

# class creation
dsa <- create_dsa_oneway(ps, es, strategies, cs)

# class creation with just one parameter
dsa <- create_dsa_oneway(p1, e1, strategies, c1)

# print
print(dsa)

# plots
o <- owsa(dsa, outcome = "nmb", wtp = 1e5)
owsa_tornado(o, strategy = "s2", min_rel_diff = -1)
owsa_opt_strat(o)
