###################################################################################################################################################
###################################################################################################################################################
####################################  CONTINUOUS BOYCE INDEX, Hirzel et al. 2006     ##############################################################
###################################################################################################################################################
###################################################################################################################################################
# by Juanmi Requena (contact: juanmir@ual.es)

## This R script aims to estimate the continuous Boyce Index proposed by Hirzel et al. 2006
## (see also Boyce et al. 2002)

# References used: 
# Hirzel, A.H., Le Lay, G., Helfer, V., Randin, C. & Guisan, A. 2006.
# Evaluating the ability of habitat suitability models to predict species occurrences.
# Ecological Modelling 199, 142-152.

# Boyce, M.S., Vernier, P.R., Nielsen, S.E., Schmiegelow, F.K.A., 2002.
# Evaluating resource selection functions. Ecol. Model. 157,281-300

#####################
# The input data must be a data frame with two columns called "suitability" (i.e., logistic output from the model) 
# and "presence" (i.e., one option for each location-pixel: 1 or 0).

# Note: the script assumes that the input data frame is called "data".

# Simulate input data frame
data <- data.frame(suitability = rbeta(100, shape1 = 1, shape2 = 0.5), 
        presence = rbinom(100, 1, 0.6))

# compute Boyce index
w <- 0.01 # window size. This parameter is adjustable by user (see Hirzel et al. 2006)
vectori <- NULL
boyce <- NULL
for (i in seq(0, 0.99, w)){
  pitotal <- length(which(data$presence==1)) # total number of presences
  pipresence <- length(which(data$presence==1&data$suitability>=i&data$suitability<(i+w))) # number of observations predicted by the model to fall in the habitat suitability class i
  aitotal <- length(data$suitability) # overall number of observations in the data set
  aicategory <- length(which(data$suitability>=i&data$suitability<(i+w))) # number of observations belonging to the habitat suitability class i
  
  Pi <- pipresence / pitotal # predicted frequency
  Ei <- aicategory / aitotal # expected frequency
  Fi <- Pi / Ei # predicted-to-expected (P/E) ratio Fi
  
  vectori <- as.array(Fi) # put Fi into an array
  boyce <- abind(boyce, vectori, along=) # join it to the array created in the previous step 
}
boyce_index <- suppressWarnings({cor.test(seq(0, 0.99, w), boyce, method="spearman")})
boyce_index$estimate
