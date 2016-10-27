## R Simulations
# Load Packages
library(pscl)  # For Zero-Inflated and Hurdle Count Models
library(e1071) # For Kurtosis of DV
library(psych) # For the Summary Stats
library(lmtest) # For the BP-Test for Heteroscedasticity (H0: No Het.)

# Random number generator
rzinbinom <- function(n, mu, size, zprob){ 
  ifelse(rbinom(n, 1, zprob) == 1, 0, rnbinom(n, size = size, mu = mu))
}

set.seed(8675309) # Set the seed for reproducible results

reps <- 10000 # Set the number of repetitions at the top of the script
n <- 1000 # Sample size
b0z <- -.8 # True value for the inflation intercept
b1z <- .2 # True value for the inflation slope                                                  
b0c <- .3 # True value for the count intercept
b1c <- .5 # True value for the count slope

X <- runif(n, 1, 11)  # Create a sample of n observations on the IV X
Z <- rnorm(n, X, 1)   # Inflation independent variable

par.est.coef <- matrix(NA, nrow = reps, ncol = 10) # Empty matrix to store estimates
par.est.se <- matrix(NA, nrow = reps, ncol = 9) # Empty matrix to store standard errors
est.aic <- matrix(NA, nrow = reps, ncol = 9) # Empty matrix to store models' AIC
est.white <- matrix(NA, nrow = reps, ncol = 4) # Empty matrix to store models' White's Test Statistic & P-values 

y.stats <- matrix(NA, nrow = reps, ncol = 7) # Empty matrix to store summary statistics for DV
y.log.stats <- matrix(NA, nrow = reps, ncol = 7) # Empty matrix to store summary statistics for DV


for(i in 1:reps){
  # Generate data with a zero-inflation component; the true DGP
  Y.zi <- rzinbinom(n, mu = exp(b0c + b1c*X), size = .6,    # mu = means; size = dispersion parameter
                    zprob = exp(b0z + b1z*Z)/(1 + exp(b0z + b1z*Z)))
  
  # Take the ln(Y.zi) for Logged DV
  Y.zi.log <- ifelse(Y.zi>0, log(Y.zi), NA)
  
  # Create a binary indicator for the Two-Part Models
  Y.zi.inf <- ifelse(Y.zi>0, 1, 0)  

  # Estimate Regression Models  
  model.inf <- glm(Y.zi.inf ~ X, family = "binomial") # (1) Estimate Logit Model for Two-Part Inflation Eq.
  vcv.inf <- vcov(model.inf) # Variance-covariance matrix
  aic.inf <- AIC(model.inf)  
  
  model.ols1 <- lm(Y.zi ~ X) # (2) Estimate Naive OLS model for Full DV
  vcv.ols1 <- vcov(model.ols1) # Variance-covariance matrix
  aic.ols1 <- AIC(model.ols1)
  white.ols1 <- bptest(model.ols1, ~ X + I(X^2))  

  model.ts2 <- lm(Y.zi.log ~ X) # (3) Estimate Two-Part OLS model for Logged DV
  vcv.ts2 <- vcov(model.ts2) # Variance-covariance matrix
  aic.ts2 <- AIC(model.ts2)
  white.ts2 <- bptest(model.ts2, ~ X + I(X^2))
  
  model.p1 <- glm(Y.zi ~ X, family=poisson()) # (4) Poisson Count Model
  vcv.p1 <- vcov(model.p1) # Variance-covariance matrix
  aic.p1 <- AIC(model.p1)

  model.nb1 <- glm.nb(Y.zi ~ X) # (5) Negative Binomial Count Model
  vcv.nb1 <- vcov(model.nb1) # Variance-covariance matrix
  aic.nb1 <- AIC(model.nb1)

  model.zip1 <- zeroinfl(Y.zi ~ X | Z, dist = "poisson") # (6) Zero-Inflated Poisson Count Model
  vcv.zip1 <- vcov(model.zip1) # Variance-covariance matrix
  aic.zip1 <- AIC(model.zip1)

  model.zinb1 <- zeroinfl(Y.zi ~ X | Z, dist = "negbin") # (7) Zero-Inflated NB Count Model
  vcv.zinb1 <- vcov(model.zinb1) # Variance-covariance matrix
  aic.zinb1 <- AIC(model.zinb1)

  model.hp1 <- hurdle(Y.zi ~ X, dist="poisson", zero.dist="binomial", link="logit") # (8) Hurdle Model (P)
  vcv.hp1 <- vcov(model.hp1) # Variance-covariance matrix
  aic.hp1 <- AIC(model.hp1)

  model.hnb1 <- hurdle(Y.zi ~ X, dist="negbin", zero.dist="binomial", link="logit") # (9) Hurdle Model (NB)
  vcv.hnb1 <- vcov(model.hnb1) # Variance-covariance matrix
  aic.hnb1 <- AIC(model.hnb1)

  # Store the estimates of the coefficient on X (count equation)
  par.est.coef[i, 1] <- as.numeric(model.inf$coef[2])            # Logit Inflation Equation   
  par.est.coef[i, 2] <- as.numeric(model.ols1$coef[2])           # Naive OLS for Full DV    
  par.est.coef[i, 3] <- as.numeric(model.ts2$coef[2])            # Naive Two-Part OLS model for Logged DV
  par.est.coef[i, 4] <- as.numeric(model.p1$coef[2])             # Poisson
  par.est.coef[i, 5] <- as.numeric(model.nb1$coef[2])            # NB
  par.est.coef[i, 6] <- as.numeric(model.zip1$coef$count[2])     # ZIP
  par.est.coef[i, 7] <- as.numeric(model.zinb1$coef$count[2])    # ZINB
  par.est.coef[i, 8] <- as.numeric(model.hp1$coef$count[2])      # HURDLE (P)
  par.est.coef[i, 9] <- as.numeric(model.hnb1$coef$count[2])     # HURDLE (NB)
  par.est.coef[i, 10] <- as.numeric(model.hnb1$theta)            # NB Dispersion Parameter
  
  # Store the standard errors of the coefficient on X (count equation)
  par.est.se[i, 1] <- as.numeric(sqrt(diag(vcv.inf)[2]))    # Logit Inflation Equation  
  par.est.se[i, 2] <- as.numeric(sqrt(diag(vcv.ols1)[2]))   # Naive OLS for Full DV  
  par.est.se[i, 3] <- as.numeric(sqrt(diag(vcv.ts2)[2]))    # Naive Two-Part OLS model for Logged DV  
  par.est.se[i, 4] <- as.numeric(sqrt(diag(vcv.p1)[2]))     # Poisson
  par.est.se[i, 5] <- as.numeric(sqrt(diag(vcv.nb1)[2]))    # NB
  par.est.se[i, 6] <- as.numeric(sqrt(diag(vcv.zip1)[2]))   # ZIP
  par.est.se[i, 7] <- as.numeric(sqrt(diag(vcv.zinb1)[2]))  # ZINB
  par.est.se[i, 8] <- as.numeric(sqrt(diag(vcv.hp1)[2]))    # HURDLE (P)
  par.est.se[i, 9] <- as.numeric(sqrt(diag(vcv.hnb1)[2]))   # HURDLE (NB)
  
  # Store AIC from each model
  est.aic[i, 1] <- as.numeric(aic.inf)     # AIC for Logit Inflation Equation
  est.aic[i, 2] <- as.numeric(aic.ols1)    # AIC for Naive OLS for Full DV
  est.aic[i, 3] <- as.numeric(aic.ts2)     # AIC for Naive Two-Part OLS model for Logged DV
  est.aic[i, 4] <- as.numeric(aic.p1)      # AIC for Poisson
  est.aic[i, 5] <- as.numeric(aic.nb1)     # AIC for NB
  est.aic[i, 6] <- as.numeric(aic.zip1)    # AIC for ZIP
  est.aic[i, 7] <- as.numeric(aic.zinb1)   # AIC for ZINB
  est.aic[i, 8] <- as.numeric(aic.hp1)    # AIC for HURDLE (P)
  est.aic[i, 9] <- as.numeric(aic.hnb1)   # AIC for HURDLE (NB)
  
  # Store White's Test Statistic from each model
  est.white[i, 1] <- as.numeric(white.ols1[1])    # White's Statistic for Naive OLS for Full DV
  est.white[i, 2] <- as.numeric(white.ols1[4])    # White's P-value for Naive OLS for Full DV
  est.white[i, 3] <- as.numeric(white.ts2[1])     # White's Statistic for Naive Two-Part OLS model for Logged DV
  est.white[i, 4] <- as.numeric(white.ts2[4])    # White's P-value for Naive Two-Part OLS model for Logged DV
   
  # Store Summary Stats from each DV
  y.stats[i, 1] <- mean(Y.zi)                 # Mean of Y.zi
  y.stats[i, 2] <- sd(Y.zi)                   # SD of Y.zi
  y.stats[i, 3] <- skew(Y.zi)                 # Skew of Y.zi 
  y.stats[i, 4] <- kurtosis(Y.zi)             # Kurtosis of Y.zi
  y.stats[i, 5] <- max(Y.zi) - min(Y.zi)      # Range of Y.zi  
  y.stats[i, 6] <- sum(Y.zi == 0)/n           # Proportion of 0s in Y.zi
  y.stats[i, 7] <- sum(complete.cases(Y.zi))  # Number of Observations in Y.zi
  
  # Store Summary Stats from each DV
  y.log.stats[i, 1] <- mean(Y.zi.log, na.rm=TRUE)                 # Mean of Y.zi.log
  y.log.stats[i, 2] <- sd(Y.zi.log, na.rm=TRUE)                   # SD of Y.zi.log
  y.log.stats[i, 3] <- skew(Y.zi.log, na.rm=TRUE)                 # Skew of Y.zi.log
  y.log.stats[i, 4] <- kurtosis(Y.zi.log, na.rm=TRUE)             # Kurtosis of Y.zi.log
  y.log.stats[i, 5] <- max(Y.zi.log, na.rm=TRUE) - min(Y.zi.log, na.rm=TRUE)  # Range of Y.zi.log  
  y.log.stats[i, 6] <- sum(Y.zi.log == 0, na.rm=TRUE)/n           # Proportion of 0s in Y.zi.log
  y.log.stats[i, 7] <- sum(complete.cases(Y.zi.log))              # Number of Observations in Y.zi.log
  
  
  cat("Completed", i, "of", reps, "\n")
}

# Means of the coefficient of X estimates
mean(par.est.coef[ , 1])  # Logit Inflation Coeff. 
mean(par.est.coef[ , 2])  # Naive OLS for Full DV Slope Coeff.
mean(par.est.coef[ , 3])  # Naive Two-Part OLS model for Logged DV Slope Coeff. 
mean(par.est.coef[ , 4])  # Poisson Slope Coeff.
mean(par.est.coef[ , 5])  # NB Slope Coeff.
mean(par.est.coef[ , 6])  # ZIP Slope Coeff. 
mean(par.est.coef[ , 7]) # ZINB Slope Coeff. 
mean(par.est.coef[ , 8]) # Hurdle (P) Slope Coeff.
mean(par.est.coef[ , 9]) # Hurdle (NB) Slope Coeff.
mean(par.est.coef[ , 10]) # Hurdle (NB) Dispersion Parameter
log(mean(par.est.coef[ , 10])) # LN of the HNB Dispersion Parameter

# Means of the standard errors of X estimates
mean(par.est.se[ , 1])  # Logit Inflation Coeff. 
mean(par.est.se[ , 2])  # Naive OLS for Full DV Slope Coeff.
mean(par.est.se[ , 3])  # Naive Two-Part OLS model for Logged DV Slope Coeff. 
mean(par.est.se[ , 4])  # Poisson Slope Coeff.
mean(par.est.se[ , 5])  # NB Slope Coeff.
mean(par.est.se[ , 6])  # ZIP Slope Coeff. 
mean(par.est.se[ , 7]) # ZINB Slope Coeff. 
mean(par.est.se[ , 8]) # Hurdle (P) Slope Coeff.
mean(par.est.se[ , 9]) # Hurdle (NB) Slope Coeff.

# AIC of the Models
mean(est.aic[ , 1]) # AIC of Logit Model [NOTE: ONLY PARTIAL MODEL]
mean(est.aic[ , 2]) # AIC of Naive OLS Model for Full DV
mean(est.aic[ , 3]) # AIC of Naive Two-Part OLS Model for Logged DV [NOTE: ONLY PARTIAL MODEL]
mean(est.aic[ , 4]) # AIC of Poisson 
mean(est.aic[ , 5]) # AIC of NB 
mean(est.aic[ , 6]) # AIC of ZIP 
mean(est.aic[ , 7]) # AIC of ZINB 
mean(est.aic[ , 8]) # AIC of Hurdle (P)  
mean(est.aic[ , 9]) # AIC of Hurdle (NB)  

# White's Test Statistic of the Models
mean(est.white[ , 1]) # White's of Naive OLS Model for Full DV
mean(est.white[ , 2]) # White's P-Value
mean(est.white[ , 3]) # White's of Naive Two-Part OLS Model for Logged DV [NOTE: ONLY PARTIAL MODEL] 
mean(est.white[ , 4]) # White's P-Value

# Means of the Relative Bias of X estimates
mean((par.est.coef[ , 2] - b1c)/b1c) # Relative Bias of Naive OLS Model for Full DV
mean((par.est.coef[ , 3] - b1c)/b1c) # Relative Bias of Naive Two-Part OLS Model for Logged DV
mean((par.est.coef[ , 4] - b1c)/b1c) # Poisson Relative Bias
mean((par.est.coef[ , 5] - b1c)/b1c) # NB Relative Bias
mean((par.est.coef[ , 6] - b1c)/b1c)  # ZIP Relative Bias
mean((par.est.coef[ , 7] - b1c)/b1c) # ZINB Relative Bias
mean((par.est.coef[ , 8] - b1c)/b1c)  # HURDLE (P) Relative Bias
mean((par.est.coef[ , 9] - b1c)/b1c)  # HURDLE (NB) Relative Bias 

# CP Functions
coverage95 <- function(b, se, true, level = .95, df = Inf){ 
  # Estimate, 
  # standard error,
  # true parameter, 
  # confidence level, 
  # and df  
  qtile <- level + (1 - level)/2 # Compute the proper quantile
  lower.bound <- b - qt(qtile, df = df)*se # Lower bound
  upper.bound <- b + qt(qtile, df = df)*se # Upper bound 
  # Is the true parameter in the confidence interval? (yes = 1)
  true.in.ci <- ifelse(true >= lower.bound & true <= upper.bound, 1, 0)
  cp <- mean(true.in.ci) # The coverage probability
  mc.lower.bound <- cp - 1.96*sqrt((cp*(1 - cp))/length(b)) # Monte Carlo error  
  mc.upper.bound <- cp + 1.96*sqrt((cp*(1 - cp))/length(b))  
  return(list(coverage.probability = cp, # Return results
              true.in.ci = true.in.ci,
              ci = cbind(lower.bound, upper.bound),
              mc.eb = c(mc.lower.bound, mc.upper.bound)))
}

# Coverage Probabilities
# 2 - Full OLS
coverage95(par.est.coef[ , 2], par.est.se[ , 2], b1c,
           df = n - model.ols1$rank)$coverage.probability

# 3 - Two-Part Logged OLS
coverage95(par.est.coef[ , 3], par.est.se[ , 4], b1c,
           df = n - model.ols1$rank)$coverage.probability

# Poisson SEs
coverage95(par.est.coef[ , 4], par.est.se[ , 4], b1c,
         df = n - model.p1$rank)$coverage.probability

# NB SEs
coverage95(par.est.coef[ , 5], par.est.se[ , 5], b1c,
         df = n - model.nb1$rank)$coverage.probability

# ZIP SEs
coverage95(par.est.coef[ , 6], par.est.se[ , 6], b1c,
         df = 996)$coverage.probability

# ZINB SEs
coverage95(par.est.coef[ , 7], par.est.se[ , 7], b1c,
         df = 995)$coverage.probability

# Hurdle Poisson SEs
coverage95(par.est.coef[ , 8], par.est.se[ , 8], b1c,
         df = 996)$coverage.probability

# Hurdle NB SEs
coverage95(par.est.coef[ , 9], par.est.se[ , 9], b1c,
         df = 995)$coverage.probability

# Summary Stats for Count DV
mean(y.stats[ , 1]) # Mean of Y.zi
mean(y.stats[ , 2]) # SD of Y.zi
mean(y.stats[ , 3]) # Skew of Y.zi 
mean(y.stats[ , 4]) # Kurtosis of Y.zi
mean(y.stats[ , 5]) # Range of Y.zi  
mean(y.stats[ , 6]) # Proportion of 0s in Y.zi
mean(y.stats[ , 7]) # Number of Observations in Y.zi

# Summary Stats for Logged Count DV
mean(y.log.stats[ , 1]) # Mean of Y.zi.log
mean(y.log.stats[ , 2]) # SD of Y.zi.log
mean(y.log.stats[ , 3]) # Skew of Y.zi.log
mean(y.log.stats[ , 4]) # Kurtosis of Y.zi.log
mean(y.log.stats[ , 5]) # Range of Y.zi.log  
mean(y.log.stats[ , 6]) # Proportion of 0s in Y.zi.log
mean(y.log.stats[ , 7]) # Number of Observations in Y.zi.log
