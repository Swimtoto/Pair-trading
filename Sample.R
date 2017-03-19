install.packages("PairTrading")
library(PairTrading)
library(caTools)

options(warn=-1) # Turning off warning message | options(warn=-0) to turn on

DATASET = read.csv("SnP500.csv", header = TRUE)
DATASET$X = as.POSIXct(DATASET$X, format="%m/%d/%Y %H:%M") # Converting first column as as Date format

# Creating a sample for training set
set.seed(211313)
split = sample.split(DATASET, SplitRatio = 0.75)
DATA = subset(DATASET, split == TRUE) 

WORKING_PAIRS = matrix(data = NA, ncol = 4, nrow = 0)

cpt = 0

val = c(2:(dim(DATA)[2]-1))

for(i in val) {
  for(j in (i+1):(dim(DATA)[2])) {

    pair = cbind(DATA[1], DATA[i], DATA[j]) # Column 1 = Date
    pair = xts(pair[,-1], order.by = pair[,1]) # Converting our data frame in a xts object
    
    
    # ----------------------------------------
    #   PAIR TRADING DETECTION ALGORITHM
    # ----------------------------------------
    reg = EstimateParameters(pair, method = lm)
    
    stationary = IsStationary(reg$spread, 0) # Stationarity with % of certitude
    
    # We save the pair if it's stationary
    if((stationary[1] == TRUE) && (stationary[2]) == TRUE) {
      
      cpt = cpt + 1
      # We save the effective pair
      WORKING_PAIRS = rbind(WORKING_PAIRS, nrow(WORKING_PAIRS)) # Add a row to the matrix
      WORKING_PAIRS [cpt,2] = colnames(pair)[1]
      WORKING_PAIRS [cpt,4] = colnames(pair)[2]
      
      # We save the index from the original dataset DATA
      WORKING_PAIRS [cpt, 1] = i
      WORKING_PAIRS [cpt, 3] = j
    }
  }
}




# ----------------------------------------
#   PAIR TRADING RETURN ALGORITHM
# ----------------------------------------


window = 800
params = EstimateParametersHistorically(pair, period = window, method = lm)
plot(params$spread)


# signal = Simple(params$spread, threshold]))
signal = Simple(params$spread, mean(params$spread[window:length(params$spread)]))

barplot(signal,col="blue",space = 0, border = "blue",xaxt="n",yaxt="n",xlab="",ylab="")
par(new=TRUE)
plot(params$spread)

return = Return(pair, lag(signal), lag(params$hedge.ratio))
plot(100 * cumprod(1 + return))

