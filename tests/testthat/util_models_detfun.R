# markov model for testing
markov_model <- function(param, state_name, cycle,
                         popsize = 1000,
                         init_state = c(1, 0, 0, 0)) {
  # 4 states: susceptible, asymptomatic infection,
  # symptomatic infection, and treated
  p_inf <- param[["p_inf"]]
  p_asymp <- param[["p_asymp"]]
  p_test_symp <- param[["p_test_symp"]]
  p_test_asymp <- param[["p_test_asymp"]]
  p_rec <- param[["p_rec"]]

  # Transition matrix
  trans_mat <- matrix(0, nrow = length(state_name), ncol = length(state_name),
                      dimnames = list(state_name, state_name))

  trans_mat["S", "IS"] <- p_inf * (1 - p_asymp)
  trans_mat["S", "IA"] <- p_inf * p_asymp
  trans_mat["S", "S"] <- 1 - p_inf

  trans_mat["IS", "T"] <- p_test_symp
  trans_mat["IS", "S"] <- p_rec
  trans_mat["IS", "IS"] <- 1 - p_test_symp - p_rec

  trans_mat["IA", "T"] <- p_test_asymp
  trans_mat["IA", "S"] <- p_rec
  trans_mat["IA", "IA"] <- 1 - p_test_asymp - p_rec

  trans_mat["T", "S"] <- 1

  trace <- matrix(0, ncol = length(state_name), nrow = cycle + 1)
  colnames(trace) <- state_name
  trace[1, ] <- init_state

  incidence <- matrix(0, ncol = length(state_name), nrow = cycle)
  colnames(incidence) <- state_name

  for (t in c(1:cycle)) {
    trace[t + 1, ] <- trace[t, ] %*% trans_mat
    expand_state <- matrix(rep(trace[t, ], length(state_name)),
                           nrow = length(state_name), byrow = F)
    tmp_trans <- expand_state * trans_mat
    diag(tmp_trans) <- 0
    incidence[t, ] <- colSums(tmp_trans) * popsize
  }

  prev <- sum(trace[cycle + 1, c("IS", "IA")])
  tot_incidence <- sum(incidence[, c("IA", "IS")])

  output <- data.frame("prev" = prev,
                       "tot_incidence" = tot_incidence)
  return(output)
}

strategy_func <- function(param, state_name, cycle,
                          mystrategy = NULL, popsize = 1000,
                          init_state = c(1, 0, 0, 0)) {

  st1 <- markov_model(param, state_name, cycle, popsize, init_state)

  param2 <- param
  param2[["p_inf"]] <- param2[["p_inf"]] * (1 - 0.5)
  st2 <- markov_model(param2, state_name, cycle, popsize, init_state)

  param3 <- param
  param3[["p_test_asymp"]] <- param3[["p_test_asymp"]] * (1 + 0.1)
  st3 <- markov_model(param3, state_name, cycle, popsize, init_state)

  st2 <- st2 - st1
  st3 <- st3 - st1

  tmp_output <- data.frame(rbind(st2, st3))

  if (is.null(mystrategy)) {
    output <- data.frame(strategy = c(seq_len(nrow(tmp_output))))
  } else {
    output <- data.frame(strategy = mystrategy)
  }

  output <- cbind(output, tmp_output)
  return(output)
}
