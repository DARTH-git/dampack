# Superseded: build a psa object with make_psa_obj(cancer_psa_data$cost, ...).
# Kept for backward compatibility until dampack 2.0. Rebuilt from cancer_psa_data.rda
# at install time (in the v1.0.x object layout) so the samples are stored only once.
local({
  e <- new.env()
  load("cancer_psa_data.rda", envir = e)
  d <- e$cancer_psa_data
  cost <- d$cost
  effectiveness <- d$effectiveness
  names(cost) <- names(effectiveness) <- d$strategies
  obj <- list(n_strategies = length(d$strategies),
              strategies = d$strategies,
              n_sim = nrow(cost),
              cost = cost,
              effectiveness = effectiveness,
              parameters = d$parameters,
              parnames = names(d$parameters),
              currency = "$")
  class(obj) <- "psa"
  obj
}) -> example_psa_obj
