#' linear regression metamodeling
#'
#' @param psa psa object
#' @param parm String with the name of the parameter of interest
#' @param strategies vector of strategies to consider. the default (NULL) is that all strategies are considered. The
#' @param outcome either effectiveness ("eff"), cost, net health benefit ("nhb"), or net monetary benefit ("nmb")
#' @param wtp if outcome is NHB or NMB, must provide the willingness-to-pay threshold
#' @param poly.order Order of polynomial for the linear regression metamodel.
#' Default: 2
#'
#' @importFrom stats as.formula formula getCall lm
#' @export
metamod <- function(psa, parm, strategies = NULL,
                    outcome = c("eff", "cost", "nhb", "nmb"),
                    wtp = NULL,
                    poly.order = 2) {
  # get parameter names
  pnames <- psa$parnames

  # make sure parm is in parameter names
  if (!(parm %in% pnames)) {
    stop("parm is not in the parameter names. misspelled?")
  }
  other_parms <- pnames[-which(pnames == parm)]

  # define dependent variables
  outcome <- match.arg(outcome)

  ## make sure wtp is not null if nmb or nhb
  if ( (outcome == "nmb" | outcome == "nhb") & is.null(wtp) ) {
    stop("wtp must be provided if nmb or nhb is the outcome to be modeled")
  }

  # define y, the outcome matrix
  if (outcome == "eff") {
    y <- psa$effectiveness
  }
  if (outcome == "cost") {
    y <- psa$cost
  }
  if (outcome == "nhb") {
    y <- psa$effectiveness - psa$cost / wtp
  }
  if (outcome == "nmb") {
    y <- psa$effectiveness * wtp - psa$cost
  }

  # restrict to strategies of interest
  strat <- psa$strategies

  if (!is.null(strategies)) {
    ## make sure all subset strats are in strat
    if (all(strategies %in% strat)) {
      y <- y[, strategies, drop = FALSE]
    } else {
      wrong_strats <- setdiff(strategies, strat)
      errmsg <- paste0("these are not in psa$strategies: ",
                       paste(wrong_strats, collapse = ","))
      stop(errmsg)
    }
  }

  dep <- colnames(y)

  # data frame to pass to lm
  sim_data <- data.frame(y, psa$parameters)

  #Generate a formula by pasting column names for both dependent and independent variables.
  # Imposes a 1 level interaction
  # if there's only one outcome, don't cbind
  n_out <- length(dep)
  if (n_out > 1) {
    bind_txt <- c("cbind(", ")")
  } else {
    bind_txt <- c("", "")
  }

  f <- as.formula(paste0(bind_txt[1], paste(dep, collapse = ","), bind_txt[2], " ~ (",
                         "poly(", parm, ",", poly.order, ", raw=TRUE) + ",
                         paste(other_parms, collapse = " + "), ")"))

  #Run Multiple Multivariate Regression (MMR) Metamodel
  metamodel <- lm(f, data = sim_data)
  metamodel$call <- call("lm", formula = f, data = quote(sim_data))
  # for accessing later in predict
  metamodel$parm_of_int <- parm
  metamodel$strategies <- dep

  # class depends on number of dependent variables
  if (length(dep) > 1){
    # must have mlm class for predict
    class(metamodel) <- c("metamodel", "mlm", "lm")
  } else {
    class(metamodel) <- c("metamodel", "lm")
  }
  return(metamodel)
}

#' Predict from a metamodel
#'
#' @param object object with class "metamodel"
#' @param newdata values for parameter of interest
#' @param ... further arguments to \code{predict} (not used)
#'
#' @importFrom stats quantile predict
#' @export
predict.metamodel <- function(object, newdata = NULL, ...) {
  # hard to get original data, this is thanks to
  # https://stackoverflow.com/questions/22921765/way-to-extract-data-from-lm-object-before-function-is-applied
  df <- eval(getCall(object)$data, environment(formula(object)))

  parm <- object$parm_of_int

  # use range of parameter if user does not provide new data
  if (is.null(newdata)){
    prange <- quantile(df[, parm], c(0.025, 0.975))

    # define 400 samples of parameter range
    prange_400samp <- seq(prange[1], prange[2], length.out = 400)

    # Create data frame with all combinations between both parameters of interest
    newdata <- data.frame(prange_400samp)
    names(newdata) <- parm
  }

  # Generate matrix to use for prediction
  # use the means of all parameters other than parameter of interest
  pdata <- data.frame(matrix(rep(colMeans(df)),
                               nrow = nrow(newdata),
                               ncol = ncol(df),
                               byrow = T))

  colnames(pdata) <- colnames(df) # Name data frame's columns with parameters' names

  # replace parameter of interest
  pdata[, parm] <- newdata

  # Predict Outcomes using MMMR Metamodel fit
  # we have to get a little hacky for the MLM
  # so predict.metamodel doesn't get called again
  # for some reason we can't call predict.mlm directly
  # so we remove the first entry in the class vector (which is "metamodel")
  # this way predict.lm will get called if there's only one outcome
  tmp_obj <- object
  class(tmp_obj) <- class(tmp_obj)[-1]
  outcome <- data.frame(predict(tmp_obj, newdata = pdata))

  #Name the predicted outcomes columns with strategies names
  colnames(outcome) <- object$strategies

  # join with parameter values
  outcome <- cbind(newdata, outcome)

  return(outcome)
}
