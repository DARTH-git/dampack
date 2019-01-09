#' Two-way sensitivity analysis using linear regression metamodeling
#'
#' This function displays a two-way sensitivity analysis (TWSA) graph 
#' by estimating a linear regression metamodel of a PSA for a given
#' decision-analytic model
#' @param strategies String vector with the name of the strategies
#' @param x Matrix with the model inputs or parameters
#' @param y Matrix with the model outputs
#' @param parm1 String with the name of the first parameter of interest
#' @param parm2 String with the name of the second parameter of interest
#' @param range1 Range of the first parameter of interest. Default: NULL range. If
#' range1=NULL, the lower and upper bounds of the sample are used.
#' @param range2 Range of the second parameter of interest. Default: NULL range. If
#' range2=NULL, the lower and upper bounds of the sample are used.
#' @param poly.order Order of polynomial for the linear regression metamodel. 
#' Default: 2
#' @param maximize If \code{TRUE}, plot of strategy with maximum expected outcome 
#' (default); if \code{FALSE}, plot of strategy with minimum expected outcome
#' @param txtsize Font size for ggplot graph. Default: 12
#' @keywords two-way sensitivity analysis; linear regression metamodel
#' @return twsa A \code{ggplot2} object with the TWSA graph of \code{parm1} and 
#' \code{parm2} on the outcome of interest that can be posteriorly formatted
#' with \code{ggplot2} function
#'
TwoWaySA <- function(strategies = NULL, y, x,  
                     parm1, parm2, 
                     range1 = NULL, range2 = NULL,
                     poly.order = 2,
                     maximize = TRUE,
                     txtsize = 12){
  # Load dependencies
  require(ggplot2)
  # Create scalar with number of strategies (i.e. number of columns of 
  # `y`)
  n.strategies <- ncol(y)
  # If the name of the strategies is not provided, generate a generic vector
  if (is.null(strategies)){
    strategies <- paste(rep("Strategy_", n.strategies), seq(1, n.strategies), " ", sep = "")
  }
  # Extract parameter column number in Parms matrix
  x1  <- which(colnames(x)==parm1)
  x2  <- which(colnames(x)==parm2)
  # Number of dependent variables, i.e., strategies
  dep <- n.strategies
  # Number of independent variables, i.e., parameters
  indep <- ncol(x) 
  # Generate data frame with outputs and inputs of the model
  Sim <- data.frame(y, x)
  
  # Determine range of of the parameer to be plotted base in user's inputs
  if (is.null(range1) & is.null(range2)){ # If user doesn't define either range
    range1 <- range(x[,x1])
    range2 <- range(x[,x2])
  }
  else if (is.null(range2)){ # If user only defines first range
    range2 <- range(x[,x2])   
  }
  else if (is.null(range1)){ # If user only defines second range
    range1 <- range(x[,x1])   
  }
  # Create vectors with values to use to evaluate TWSA
  vector1 <- seq(from = range1[1], 
                 to = range1[2],
                 length.out = 301)
  vector2 <- seq(from = range2[1], 
                 to = range2[2],
                 length.out = 301)
  
  # Generate a formula by pasting column names for both dependent and independent variables
  # with interactions
  f <- as.formula(paste('cbind(',paste(colnames(Sim)[1:dep],collapse=','), ') ~ (',
                        'poly(',parm1, ',', poly.order,')*',
                        'poly(',parm2, ',', poly.order,')+',
                        paste(colnames(x)[c(-x1,-x2)], collapse='+'),')'))
  # Run Multiple Multivariate Regression (MMR) Metamodel
  Tway.mlm <- lm(f, data=Sim)
  
  # Create data frame with all combinations between both parameters of interest
  TWSA <- expand.grid(parm1 = vector1, 
                      parm2 = vector2)
  
  #Generate matrix to use for prediction 
  Sim.fit <- matrix(rep(colMeans(x)), 
                    nrow = nrow(TWSA),
                    ncol = ncol(x), byrow = T)
  Sim.fit[, x1] <- TWSA[, 1]
  Sim.fit[, x2] <- TWSA[, 2]
  # Transform to data frame, the format required for predict
  Sim.fit <- data.frame(Sim.fit) 
  # Name data frame's columns with parameters' names
  colnames(Sim.fit) <- colnames(x)
  
  # Predict Outcomes using MMMR Metamodel fit
  Sim.TW <- data.frame(predict(Tway.mlm, newdata = Sim.fit))
  # Find optimal strategy in terms of maximum expected outcome
  if (maximize){
    Optimal <- max.col(Sim.TW)
  } else { # Find optimal strategy in terms of minimum expected outcome
    Optimal <- min.col(Sim.TW)
  }
  
  # Add a variable with optimal startegy as factor
  TWSA$Strategy <- factor(Optimal, labels = strategies)
  
  twsa <- ggplot(TWSA, aes(x = parm1, y = parm2))+ 
    geom_tile(aes(fill = Strategy)) +
    theme_bw() +
    ggtitle(expression(atop("Two-way sensitivity analysis", 
                            atop("Net Health Benefit")))) + 
    scale_fill_discrete("Strategy: ", l=50) +
    scale_x_continuous(breaks = number_ticks(6)) +
    scale_y_continuous(breaks = number_ticks(6)) +
    xlab(parm1)+
    ylab(parm2)+
    theme(legend.position = "bottom", 
          legend.title = element_text(size = txtsize),
          legend.key = element_rect(colour = "black"),
          legend.text = element_text(size = txtsize),
          title = element_text(face="bold", size=15),
          axis.title.x = element_text(face="bold", size=txtsize),
          axis.title.y = element_text(face="bold", size=txtsize),
          axis.text.y = element_text(size=txtsize),
          axis.text.x = element_text(size=txtsize))
  return(twsa)
}