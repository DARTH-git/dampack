gen_psa_samp(params = NULL, dist = NULL, parameterization_type = NULL, dist_params = NULL, n_samp = NULL)

n_params <- length(params)
params_df <- "initialize list or df here"

for (i in 1:n_params) {

  #normal
  if (dist[i] == "normal") {
    params_df[[i]] <- rnorm(n_samp, mean = dist_params[[i]][1], sd = dist_params[[i]][2])
  }

  #log-normal
  if (dist[i] == "log-normal") {
    mu <- lnorm_params(dist_params[[i]][1], dist_params[[i]][2])[[1]]
    sd <- lnorm_params(dist_params[[i]][1], dist_params[[i]][2])[[2]]
    params_df[[i]] <- rlnorm(n_samp, meanlog = mu, sdlog = sd)
  }

  #beta
  if (dist[i] == "beta") {
    if (parameterization_type == "mean, sd") {
      a <- beta_params(dist_params[[i]][1], dist_params[[i]][2])[[1]]
      b <- beta_params(dist_params[[i]][1], dist_params[[i]][2])[[2]]
      params_df[[i]] <- rbeta(n_samp, a, b)
    } else if (parameterization_type == "a, b") {
      a <- dist_params[[i]][1]
      b <- dist_params[[i]][2]
      params_df[[i]] <- rbeta(n_samp, a, b)
    }
  }

  #gamma
  if (dist[i] == "gamma") {
    if (parameterization_type == "mean, sd") {
      shape <- gamma_params(dist_params[[i]][1], dist_params[[i]][2], scale = TRUE)[[1]]
      scale <- gamma_params(dist_params[[i]][1], dist_params[[i]][2], scale = TRUE)[[2]]
      params_df[[i]] <- rgamma(n_samp, shape = shape, scale = scale)
    } else if (parameterization_type == "shape, scale") {
      shape <- dist_params[[i]][1]
      scale <- dist_params[[i]][2]
      params_df[[i]] <- rgamma(n_samp, shape = shape, scale = scale)
    }
  }

  #dirichlet
  if (dist[i] == "dirichlet") {
    if (parameterization_type == "value, mean_prop, sd") {

    } else if (parameterization_type == "alpha") {

    }
  }

}
params <- c("param1","param2")
dist <- c("normal", "log-normal", "beta", "gamma", "dirichlet", "bootstrap")
parameterization_type <- c("")
dist_params <- c("")
n_samp <- 100

test<-c(c(1,2),c(3,4),c(5,6))

rnorm()
rlnorm()
rbeta()
rgamma()
rmultinom()
rbootstrap()

rdirichlet <-function (n, alpha) {
    if (is.matrix(alpha)) {
      k <- dim(alpha)[2]
      alpha <- as.vector(t(alpha))
    }
    else k <- length(alpha)
    out <- matrix(rgamma(n * k, shape = alpha), n, k, byrow = TRUE)
    out <- out/rowSums(out)
    return(out)
}

rdirichlet(1, c(.1,.1,.1))

n=100
k=3
alpha = c(1,2,3)
rgamma(n * k, shape = 1)


get_alpha <- function(mean, var){

}
alpha0 <- sum(alpha)


#' Calculate alpha and beta parameters of Beta distribution.
#'
#' Function to calculate the alpha and beta parameters of the Beta distribution
#' based on the method of moments using the mean \eqn{\mu} and standard
#' deviation \eqn{\sigma} of the random variable of interest.
#' @param mean Mean of the random variable.
#' @param sigma Standard deviation of the random variable (i.e., standar error).
#' @keywords beta distribution; methods of moments
#' @section Details:
#' Based on methods of moments. If \eqn{\mu} is the mean and
#' \eqn{\sigma} is the standard deviation of the random variable, then
#' \deqn{\alpha = (\frac{1-\mu}{\sigma^2} - \frac{1}{\mu}) \mu^2}
#' and
#' \deqn{\beta = \alpha (\frac{1}{\mu} -1)}
#'
#' @return alpha Alpha parameter of beta distribution
#' @return beta Beta parameter of beta distribution
beta_params <- function(mean, sigma){
  alpha <- ((1-mean) / sigma^2 - 1 / mean) * mean^2
  beta  <- alpha*(1 / mean - 1)
  params <- list(alpha = alpha, beta = beta)
  return(params)
}

#' Calculate alpha parameters of Dirichlet distribution.
#'
#' Function to calculate the \eqn{\alpha} parameters of the Dirichlet distribution
#' based on the method of moments (MoM) using the mean \eqn{\mu} and standard
#' deviation \eqn{\sigma} of the random variables of interest.
#' @param p.mean Vector of means of the random variables.
#' @param sigma Vector of standard deviation of the random variables
#' (i.e., standar error).
#' @keywords dirichlet distribution; method of moments
#' @section Details:
#' Based on methods of moments. If \eqn{\mu} is a vector of means and
#' \eqn{\sigma} is a vector of standard deviations of the random variables, then
#' the second moment \eqn{X_2} is defined by \eqn{\sigma^2 + \mu^2}. Using the
#' mean and the second moment, the \eqn{J} alpha parameters are computed as follows
#' \deqn{\alpha_i = \frac{(\mu_1-X_{2_{1}})\mu_i}{X_{2_{1}}-\mu_1^2}}
#' for \eqn{i = 1, \ldots, J-1}, and
#' \deqn{\alpha_J = \frac{(\mu_1-X_{2_{1}})(1-\sum_{i=1}^{J-1}{\mu_i})}{X_{2_{1}}-\mu_1^2}}
#'
#' @references
#' \enumerate{
#' \item Fielitz BD, Myers BL. Estimation of parameters in the beta distribution.
#' Dec Sci. 1975;6(1):1–13.
#' \item Narayanan A. A note on parameter estimation in the multivariate beta
#' distribution. Comput Math with Appl. 1992;24(10):11–7.
#' }
#' @return alpha Alpha parameters of dirichlet distribution
#' @examples
#' \dontrun{
#' p.mean <- c(0.5, 0.15, 0.35)
#' p.se   <- c(0.035, 0.025, 0.034)
#' dirichlet_params(p.mean, p.se)
#' # True values: 100, 30, 70
#' }
dirichlet_params <- function(p.mean, sigma){
  n.params <- length(p.mean)
  if(n.params != length(sigma)){
    stop("Length of mean different from length of sigma")
  }
  # Compute second moment
  p.2 <- sigma^2 + p.mean^2
  # Initialize alpa vector
  alpha <- numeric(n.params)
  for (i in 1:(n.params-1)){
    alpha[i] <- (p.mean[1] - p.2[1])*p.mean[i]/(p.2[1] - p.mean[1]^2)
  }
  alpha[n.params] <- (p.mean[1] - p.2[1])*(1-sum(p.mean[-n.params]))/(p.2[1] - p.mean[1]^2)
  return(alpha)
}

#' Calculate shape and scale (or rate) parameters of a gamma distribution.
#'
#' Function to calculate the shape, \eqn{\alpha}, and scale, \eqn{\theta}, (or rate, \eqn{\beta})
#' parameteres of a gamma distribution based on the method of moments (MoM)
#' using the mean \eqn{\mu} and standard deviation \eqn{\sigma} of the random
#' variable of interest.
#' @param mu Scalar with the mean of the random variable.
#' @param sigma Scalar with the standard deviation of the random variable.
#' @param scale Logical variable indicating scale parameterization of gamma distribution
#' (Default is TRUE). If FALSE, rate parameterization is retrieved
#' @keywords gamma distribution; method of moments
#' @section Details:
#' Based on method of moments. If \eqn{\mu} is the mean and
#' \eqn{\sigma} is the standard deviation of the random variable, then the
#' the shape, \eqn{\alpha}, scale, \eqn{\theta}, and rate, \eqn{\beta}, parameteres are computed
#' as follows
#' \deqn{\alpha=\frac{\mu^2}{\sigma^2},}
#' \deqn{\theta = \frac{\sigma^2}{\mu}}
#' and
#' \deqn{\beta = \frac{\mu}{\sigma^2}}
#'
#' @references
#' \itemize{
#' \item Gamma distribution. (2018, February 7). In Wikipedia, The Free
#' Encyclopedia. Retrieved 17:23, February 11, 2018,
#' from https://en.wikipedia.org/w/index.php?title=Gamma_distribution&oldid=824541785
#' }
#' @return
#' shape Shape parameter of gamma distribution
#' scale Scale parameter of gamma distribution (If scale=TRUE)
#' rate Rate parameter of gamma distribution (If scale=FALSE)
#' @examples
#' \dontrun{
#' mu    <- 2
#' sigma <- 1
#' # Scale specification
#' gamma_params(mu, sigma)
#' # Rate specification
#' gamma_params(mu, sigma, scale = FALSE)
#' }
gamma_params <- function(mu, sigma, scale = TRUE){
  if (scale){
    shape <- (mu^2)/(sigma^2)
    scale <- (sigma^2)/mu
    params <- list(shape = shape,
                   scale = scale)
  } else {
    shape <- (mu^2)/(sigma^2)
    rate  <- mu/(sigma^2)
    params <- list(shape = shape,
                   rate  = rate)
  }
  return(params)
}


#' Calculate location and scale parameters of a log-normal distribution.
#'
#' Function to calculate the location, \eqn{\mu}, and scale, \eqn{\sigma},
#' parameteres of a log-normal distribution based on the method of moments (MoM)
#' using the mean \eqn{m} and variance \eqn{v} of the non-logarithmized random
#' variable of interest.
#' @param m Scalar with the mean of the random variable.
#' @param v Scalar with the variance of the random variable.
#' (i.e., squared standar error).
#' @keywords log-normal distribution; method of moments
#' @section Details:
#' Based on method of moments. If \eqn{m} is the mean and
#' \eqn{v} is the variance of the random variable, then the
#' the location, \eqn{\mu}, and scale, \eqn{\sigma}, parameteres are computed
#' as follows
#' \deqn{\mu = \ln{(\frac{m}{\sqrt{(1 + \frac{v}{m^2})}})}}
#' and
#' \deqn{\sigma = \sqrt{\ln{( 1 + \frac{v}{m^2})}}}
#'
#' @references
#' \enumerate{
#' \item Ginos BF. Parameter Estimation for the Lognormal Distribution.
#' Brigham Young University; 2009.
#' \item Log-normal distribution. (2017, April 20). In Wikipedia, The Free
#' Encyclopedia. Retrieved 16:47, April 23, 2017,
#' from https://en.wikipedia.org/w/index.php?title=Log-normal_distribution&oldid=776357974
#' }
#' @return
#' mu Location parameter of log-normal distribution
#' sigma Scale parameter of log-normal distribution
#' @examples
#' \dontrun{
#' m <- 3
#' v <- 0.01
#' lnorm_params(m, v)
#' # True values: 100, 30, 70
#' }
lnorm_params <- function(m = 1, v = 1){
  ### Sanity checkd
  if(m <= 0){stop("'m' needs to be greater than 0")}
  if(v <= 0){stop("'v' needs to be greater than 0")}
  mu    <- log(m/sqrt(1 + v/m^2))
  sigma <- sqrt(log(1 + v/m^2))
  return(list(mu = mu,
              sigma = sigma))
}
