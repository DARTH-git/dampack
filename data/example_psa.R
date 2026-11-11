# Superseded by `cancer_psa_data`; kept for backward compatibility until dampack 2.0.
# Rebuilt from cancer_psa_data.rda at install time so the samples are stored only once.
local({
  e <- new.env()
  load("cancer_psa_data.rda", envir = e)
  d <- e$cancer_psa_data
  list(strategies = d$strategies,
       wtp = seq(1000, 141000, by = 10000),
       cost = d$cost,
       effectiveness = d$effectiveness,
       parameters = d$parameters)
}) -> example_psa
