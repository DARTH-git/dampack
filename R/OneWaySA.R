#' One-way sensitivity analysis using linear regression metamodeling
#'
#' This function displays a one-way sensitivity analysis (OWSA) graph
#' by estimating a linear regression metamodel of a PSA for a given
#' decision-analytic model
#' @param strategies String vector with the name of the strategies
#' @param x Matrix with the model inputs or parameters
#' @param y Matrix with the model outputs
#' @param parm String with the name of the parameter of interest
#' @param range Range of the parameter of interest. Default: NULL range. If
#' range=NULL, the 2.5th and 9.75th percentile of the parameter
#' of interest will be used as lower and upper bounds of the
#' range, respectively.
#' @param poly.order Order of polynomial for the linear regression metamodel.
#' Default: 2
#' @param txtsize Font size for ggplot graph. Default: 12
#' @keywords one-way sensitivity analysis; linear regression metamodel
#' @return owsa A `ggplot` object with the OWSA graph of the `parm` on the
#' outcome of interest
#'
owsa <- function(strategies, y, x,
                     parm, range = NULL,
                     poly.order = 2,
                     txtsize = 12){

  #Run Multiple Multivariate Regression (MMR) Metamodel
  mm <- metamod(y, x, parm, strategies, poly.order)

  # Extract parameter column number in x matrix
  par.col <- which(colnames(x)==parm)

  #Determine range of of the parameer to be plotted
  if (is.null(range)){ #If user does not define a range
    #Default range given by the domain of the parameter's sample
    #vector to define 400 samples between the 2.5th and 97.5th percentiles
    percentiles = seq(2.5, 97.5, length = 400)
    j = round(percentiles*(length(x[,par.col])/100)) #indexing vector;j=round(y*n/100) where n is the size of vector of interest
    vector<-sort(x[j, par.col])
  }
  else{ #If user defines a range
    vector <- seq(range[1],range[2],length.out=400)
  }

  # Create data frame with all combinations between both parameters of interest
  OWSA <- data.frame(parm = vector)

  #Generate matrix to use for prediction
  Sim.fit <- matrix(rep(colMeans(x)),
                    nrow = length(vector),
                    ncol = ncol(x),
                    byrow = T)
  Sim.fit[, par.col] <- OWSA[, 1]
  # Transform to data frame, the format required for predict
  Sim.fit <- data.frame(Sim.fit)
  # Name data frame's columns with parameters' names
  colnames(Sim.fit) <- colnames(x) #Name data frame's columns with parameters' names

  # Predict Outcomes using MMMR Metamodel fit
  Sim.OW = data.frame(predict(mm, newdata = Sim.fit))
  colnames(Sim.OW) <- strategies #Name the predicted outcomes columns with strategies names

  # define class as owsa and dataframe


  #Reshape dataframe for ggplot
  Sim.OW = stack(Sim.OW, select=strategies) #
  Sim.OW = cbind(Sim.fit, Sim.OW) #Append parameter's dataframe to predicted outcomes dataframe

  #A simple trick I use to define my variables in my functions environment
  #Borrowed from http://stackoverflow.com/questions/5106782/use-of-ggplot-within-another-function-in-r
  Sim.OW$parm<-Sim.OW[,parm];

  owsa <- ggplot(data = Sim.OW, aes(x = parm, y = values, color = ind)) +
    geom_line() +
    ggtitle("One-way sensitivity analysis") + #\n Net Health Benefit
    xlab(parm) +
    ylab("E[Outcome]") +
    scale_colour_hue("Strategy", l=50) +
    scale_x_continuous(breaks=number_ticks(6)) + #Adjust for number of ticks in x axis
    scale_y_continuous(breaks=number_ticks(6)) +
    theme_bw(base_size = 14) +
    theme(legend.position = "bottom")
  return(owsa)
}
