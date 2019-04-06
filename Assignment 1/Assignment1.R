library(yaml)
library(radiant)
library(radiant.model)

#setwd("/home/pranav/Desktop/Decision Models Assignments/Assignment 1/")
tree = yaml.load_file(input = "./trees/Board_Production.yaml")

result =dtree(yl = tree)

#Exercise 1
plot(result, final = FALSE)

#Exercise 2
plot(result, final = TRUE)

#Exercise 3
utilityFunctionExp <- function(X, R) {
  res <- 1- exp(-X/R)
  return(res)
}

CertEquivalent = function(EU, R){
  CE = -R*ln(1-EU)
  return(CE)
}

CalcExpectedUtilityFunction = function(profit, R){
  #------Branch 1----------#
  UF1 = utilityFunctionExp(profit$profitBranch1, R)
  EU1 = UF1[1]*0.5 + UF1[2]*0.5
  
  #------Branch 2----------#
  UF2 = utilityFunctionExp(profit$profitBranch2, R)
  EU2 = UF2[1]*0.5 + UF2[2]*0.5
  
  #----Return Final Result----#
  return(c(EU1, EU2))
}

CalcBranchCE = function(profit, R){
  CE_vett = CertEquivalent(CalcExpectedUtilityFunction(profit, R), R)
  return(CE_vett)
}

#Create a DataFrame with Profits per Branch

index <- 1:2
profitBranch1 <- c(35,-15)
profitBranch2 <- c(10,5)
profit <- data.frame("X"=index,"profitBranch1"=profitBranch1,"profitBranch2"=profitBranch2)
R=10

CE <- CalcBranchCE(profit,R)

CE_Branch1 <- CE[1]*1000
CE_Branch2 <- CE[2]*1000
cat(paste0('Certainty Equivalent of Branch of producing eveything together: ',CE_Branch1),
    paste0('Certainty Equivalent of Branch of producing separately: ',CE_Branch2),
    'NOTE: The values are re-scaled to the right scale.',
    '********************************************************',sep='\n')

#Exercise 3 Extra
R = seq(2, 1300, 1)
res1 = c()
res2 = c()
for (i in 1:length(R)) {
  vRes = CalcBranchCE(profit, R[i])
  res1[i] = vRes[1]
  res2[i] = vRes[2]
}
plot(R, res1, type="l", col="blue", ylab="CE(scaled by 1000)", xlab = "R(scaled by 1000)",
     xlim=c(-1, max(R)+1), ylim=c(min(res1), max(res2)))
points (R, res2, type="l", col="red")
legend(500,-50,legend=c('Produce Separately','Produce Together'),col=c("red", "blue"), lty=1)


#Exercise 4
tree_RnD = yaml.load_file(input = "./trees/Board_Production_RnD.yaml")
result_RnD = dtree(yl = tree_RnD)

plot(result_RnD, final = TRUE)


#Exercise 5
tree_PI = yaml.load_file(input = "./trees/Board_Production_PI.yaml")
result_PI = dtree(yl = tree_PI)


plot(result_PI, final = TRUE)

