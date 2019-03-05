options(scipen=999)

rm(list = ls())
CleanListings <- read.csv('~/Documents/DATA557-Bruins/dataset/CleanListings.csv')

DependentVar <- 'price'
PredictorVars <- colnames(CleanListings)
PredictorVars <- PredictorVars[PredictorVars!=DependentVar]

ResultDF <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(ResultDF) <- c('DependentVar', 'PredictorVar', 'PredictorCoeff', 'PredictorSignificanct', 'ResidualStdError')

for (PredictorVar in PredictorVars) {
	CoeffVal <- NA
	SignVal <- NA
	ResidualStdErrorVal <- NA
	if (length(unique(CleanListings[, PredictorVar])) < 500 & length(unique(CleanListings[, PredictorVar])) >= 2) {
		tryCatch(
			{
				summaryLmModel <- summary(lm(CleanListings[,DependentVar] ~ CleanListings[,PredictorVar]))
				ResidualStdErrorVal <- sd(summaryLmModel$residuals)
				CoeffVal <-	abs(summaryLmModel$coefficients[2, 'Estimate'])
				if (summaryLmModel$coefficients[2, 'Pr(>|t|)'] <= 0.001) {
					SignVal <- '<0.001'
				} else if (summaryLmModel$coefficients[2, 'Pr(>|t|)'] <= 0.05) {
					SignVal <- '<0.05'
				} else {
					SignVal <- 'Insignif'
				}
			}, error = function(e) print(e)
		)
	}
	AppDF <- data.frame(DependentVar = DependentVar,
											PredictorVar = PredictorVar, 
											PredictorCoeff = CoeffVal,
											PredictorSignificanct = SignVal,
											ResidualStdError = ResidualStdErrorVal)
	ResultDF <- rbind(ResultDF, AppDF)
}