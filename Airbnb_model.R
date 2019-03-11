Listings <- read.csv('~/Desktop/MSDS/DATA 557/Final_Project/DataSets/CleanListings_SeaWA.csv', stringsAsFactors = FALSE)

# Function for generating linear models
# In: dataSrc (data file)
# In: depVar (string representing the dependent variable)
# In: predictVars (single/concatenated string(s) representing predictor variables)
# Out: linear model object
lmGen <- function(dataSrc, depVar, predictVars) {
  ResponseVar <- paste(depVar, '~')
  formula <- as.formula(paste(ResponseVar, paste(predictVars, collapse="+")))
  lm(formula, dataSrc)
}

# convert NAs to 0
# In: predictVars (single/concatenated string(s) representing numerical predictor variables)
NA_to_Zero <- function(predictVars) {
  for (p in predictVars) {
    if (p != "(Intercept)") {
      if (is.numeric(Listings[,p])){
        Listings[is.na(Listings[,p]), p] <- 0
      }
    }
  }
}

# Build model parameters
DependentVar <- 'price'
predictors <- c("bedrooms", "bathrooms", "factor(bed_type)")
model <- lmGen(Listings, 'price', predictors)

modCoef <- data.frame(model$coefficients)
modPredictors <- rownames(modCoef)

NA_to_Zero(predictors[1:2])

# Compute price estimates
for (p in modPredictors) {
  if(p != "(Intercept)") {
    if (length(grep('factor', p))){
      # Factored predictors
      patternPred <- regexpr("\\((.*)\\)",p)
      patternVal <- regexpr("\\((.*)",p)
      predictor <- regmatches(p,patternPred)
      predValue <- regmatches(p,patternVal)
      
      l <- nchar(predictor)
      l2 <- nchar(predValue)
      predictor <- substr(predictor,2,l-1)
      predValue <- substr(predValue,l+1,l2)
      
      Listings[,'estPrice'][Listings[,predictor] == predValue] <- 
        Listings[,'estPrice'][Listings[,predictor] == predValue] + modCoef[p,1]
    }
    else {
      Listings[,'estPrice'] = Listings[,'estPrice'] + (Listings[,p]*modCoef[p,1])
    }
  }
  else {
    # Initialize with the intercept
    Listings[,'estPrice'] = modCoef[p,1]
  }
}

#Evaluate performance
priceDiff <- Listings$price - Listings$estPrice
summary(priceDiff)
sd(priceDiff)
summary(model)
