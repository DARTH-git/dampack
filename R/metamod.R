#' linear regression metamodeling
#'
#' @param y data frame with the model outputs of interest
#' @param psa psa object
#' @param parm String with the name of the parameter of interest
#' @param poly.order Order of polynomial for the linear regression metamodel.
#' Default: 2
#' @export
metamod <- function(y, psa, parm, poly.order = 2) {
  # get parameter names
  pnames <- psa$parnames

  # make sure parm is in parameter names
  if (!(parm %in% pnames)) {
    stop('parm is not in the parameter names. misspelled?')
  }
  other_parms <- pnames[-which(pnames == parm)]

  # define dependent variables
  # if the outcome is a single variable and not a data.frame, coerce
  if (!inherits(y, 'data.frame')) {
    warning('y is not a data frame - coercing')
    y <- as.data.frame(y)
  }
  dep <- colnames(y)

  sim_data <- data.frame(y, psa$parameters)

  #Generate a formula by pasting column names for both dependent and independent variables.
  # Imposes a 1 level interaction
  f <- as.formula(paste0('cbind(', paste(dep, collapse=','),
                         ') ~ (','poly(', parm,',', poly.order,', raw=TRUE) + ',
                         paste(other_parms, collapse=' + '), ')'))

  #Run Multiple Multivariate Regression (MMR) Metamodel
  metamodel <- lm(f, data=sim_data)
  metamodel$call <- call("lm", formula = f, data = quote(sim_data))
  # # for accessing later in predict
  metamodel$parm_of_int <- parm
  metamodel$strategies <- psa$strategies
  class(metamodel) <- c("metamodel", "mlm", "lm")
  return(metamodel)
}

#' @export
predict.metamodel <- function(object, newdata = NULL) {
  # hard to get original data, this is thanks to
  # https://stackoverflow.com/questions/22921765/way-to-extract-data-from-lm-object-before-function-is-applied
  df <- eval(getCall(object)$data, environment(formula(object)))

  parm <- object$parm_of_int

  #Determine range of of the parameter
  if (is.null(newdata)){ #If user does not define a range
    prange <- quantile(df[, parm], c(0.025, 0.975))

    # define 400 samples of parameter range
    prange_400samp <- seq(prange[1], prange[2], length.out=400)

    # Create data frame with all combinations between both parameters of interest
    newdata <- data.frame(prange_400samp)
    names(newdata) <- parm
  }

  #Generate matrix to use for prediction
  # use the means of all parameters other than parameter of interest
  pdata <- data.frame(matrix(rep(colMeans(df)),
                               nrow = nrow(newdata),
                               ncol = ncol(df),
                               byrow = T))

  colnames(pdata) <- colnames(df) #Name data frame's columns with parameters' names

  # replace parameter of interest
  pdata[, parm] <- newdata

  # Predict Outcomes using MMMR Metamodel fit
  # we have to get a little hacky for the MLM
  # so predict.metamodel doesn't get called again
  # for some reason we can't call predict.mlm directly
  tmp_obj <- object
  class(tmp_obj) <- c("mlm", "lm")
  outcome <- data.frame(predict(tmp_obj, newdata = pdata))

  strategies <- object$strategies
  colnames(outcome) <- strategies #Name the predicted outcomes columns with strategies names

  # join with parameter values
  outcome[, parm] <- newdata

  return(outcome)
}
