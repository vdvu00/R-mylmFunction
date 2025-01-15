## Vanessa Vu
## BS803 Final Project
## --------------------------------------------------------------------

mylm <- function(data, outcome, predictors) {
  ## checks if data inputted is a data frame
  if (!is.data.frame(data)) {
    stop("argument 'data' must be a data frame")
  }
  ## checks if outcome exists in data
  if (!(outcome %in% names(data))) {
    stop(paste("outcome", outcome, "is not a column name found in the data\n",
               "(syntax is case-sensitive)"))
  }
  ## checks if predictors exist in data
  if (!all(predictors %in% names(data))) {
    stop(paste("at least one of the predictors", 
               "is not a column name found in the data\n", 
               "(syntax is case-sensitive)"))
  }
  ## checks for NAs in outcome and predictors
  if (any(is.na(data[[outcome]]))) {
    stop(paste("outcome", outcome, "contains missing values",
               "\nremove NAs to run function"))
  }
  for (onepred in predictors) {
    if (any(is.na(data[[onepred]]))) {
      stop(paste("predictor", onepred, "contains missing values",
                 "\nremove NAs to run function"))
    }
  }
  
  ## inputting the variables as y and x
  y <- data[[outcome]]
  x <- data.frame(Intercept=rep(1,nrow(data)))
  
  for (onepred in predictors) {
    x[[onepred]] <- data[[onepred]]
  }
  x <- as.matrix(x)
  
  ## print formula
  formula <- paste(outcome, "~", paste(predictors, collapse = " + "))
  cat("Linear Regression Formula:", formula, "\n")
  
  ## beta = ((X'X)^-1)X'Y
  beta <- solve(t(x)%*%x)%*%(t(x)%*%y)
  colnames(beta) <- c("Beta Estimate")
  
  ## RSS = y'y - b'X'y
  RSS <- t(y)%*%y - t(beta)%*%t(x)%*%y
  ## s2 = RSS/(n-p-1)
  s2 <- RSS/(length(y)-ncol(x))
  ## variance matrix
  var <- as.numeric(s2)*(solve(t(x)%*%x))
  ## standard error
  se <- sqrt(diag(var))
  se <- data.frame(se)
  colnames(se) <- c("Standard Error")
  
  ## t-test 
  tstat <- beta / se
  tstat <- data.frame(tstat)
  colnames(tstat) <- c("T-Stat")
  ## p-value
  tstat2 <- unlist(tstat)
  df <- length(y) - ncol(x)
  pval <- 2*pt(as.numeric(abs(tstat2)), df=df, lower.tail=F)
  pval <- data.frame(pval)
  colnames(pval) <- c("P-Value")
  
  ## print coefficients
  coeff <- data.frame(round(beta, digits = 5), round(se, digits = 5),
                       round(tstat, digits = 5), pval)
  cat("\nCoefficients:\n")
  print(coeff)
  
  ## print degrees of freedom
  cat("\nDegrees of Freedom for T-Test:", df)
  
  ## finding R2 and adj R2
  ## SYY = y'y - n*y_bar^2
  SYY <- t(y)%*%y - length(y)*mean(y)**2
  ## R^2 = 1 - RSS/SYY
  R2 <- 1 - RSS/SYY
  ## R^2 adjusted = ((n-1)R2 - p) / (n-p-1)
  R2.adj <- ((length(y)-1)*R2 - (ncol(x)-1)) / (length(y)-ncol(x))
  
  ## print R2 and adj R2
  cat("\nR-Squared:", round(R2, digits = 5), "and",
      "Adjusted R-Squared:", round(R2.adj, digits = 5), "\n")
  
  ## f-test
  df1 = (ncol(x)-1)
  df2 = (length(y)-ncol(x))
  num <- (SYY - RSS) / df1
  den <- RSS / df2
  fstat <- num/den
  pval_f <- 1-pf(fstat, df1, df2)
  
  ## print f-stat and p-value
  cat("F-Statistic:", round(fstat, digits = 5), "on", df1, "and", df2, "DF,",
      "p-value:", pval_f, "\n")
  
}

## example 1
?USArrests
data(USArrests)
mylm(data = USArrests, outcome = "UrbanPop", predictors = c("Murder", "Assault", "Rape"))
summary(lm(UrbanPop ~ Murder + Assault + Rape, data = USArrests))

## data frame error
wrong <- c(1, 2, 3)
mylm(data = wrong, outcome = "UrbanPop", predictors = c("Murder", "Assault", "Rape"))

## outcome does not exist error
mylm(data = USArrests, outcome = "Urban", predictors = c("Murder", "Assault", "Rape"))

## predictor does not exist error
mylm(data = USArrests, outcome = "UrbanPop", predictors = c("Wrong", "Assault", "Rape"))
mylm(data = USArrests, outcome = "UrbanPop", predictors = c("Wrong", "Wrong", "Rape"))

## missing values error
USArrests$UrbanPop[1] <- NA
mylm(data = USArrests, outcome = "UrbanPop", predictors = c("Murder", "Assault", "Rape"))
data(USArrests)
USArrests$Murder[1] <- NA
mylm(data = USArrests, outcome = "UrbanPop", predictors = c("Murder", "Assault", "Rape"))
data(USArrests)

## example 2
?quakes
data(quakes)
mylm(data = quakes, outcome = "mag", predictors = c("lat", "long", "depth"))
summary(lm(mag ~ lat + long + depth, data = quakes))

