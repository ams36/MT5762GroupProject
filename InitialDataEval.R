# Title     : Initial Checks on Data
# Objective : Decide what elements to further check and what models to consider
# Created by: Group 9
# Created on: 10/8/20
#What relationships are there between the measured
#variables and the birth weight of babies?
library(tidyverse)
#import dataset in
babies <- read.table("babies23.data", header = TRUE)
babies <- as.data.frame(babies)

#Used when removing 999 / 99 etc. to check the proportion of data taken out
NumRowOriginal <- NROW(babies)
print(paste0("Number of rows in original data set: ", NumRowOriginal))
#Made initial list of ones I want to look at, now do asFactor for those that need it and plot them

#After this, automated it. Decided what should be as factor and what should be as
# number and created SBS box plots for those I wanted to look at more
newCols <- c("id", "wt", "date", "gestation", "parity", "race", "age",
             "ht", "wt.1", "drace", "dage", "dht", "dwt", "smoke", "time", "number")
analyseBabies <-
  as.data.frame(cbind(babies$id, babies$wt, babies$date, babies$gestation,
        babies$parity, babies$race, babies$age, babies$ht, babies$wt.1,
        babies$drace, babies$dage, babies$dht, babies$dwt,
        babies$smoke, babies$time, babies$number))
colnames(analyseBabies) <- newCols

#checkNumberOfUnknownValues for Each
#using 2 bc im not looking at weight nor ID
output <- matrix(ncol = (NCOL(analyseBabies) - 2), nrow = 5)
for (i in 1:(NCOL(analyseBabies) - 2)){
  col_name <- colnames(analyseBabies)[i+2]
  num99 <- sum(pull(analyseBabies, !!col_name) == 99)
  num98 <- sum(pull(analyseBabies, !!col_name) == 98)
  num999 <- sum(pull(analyseBabies, !!col_name) == 999)
  num9 <- sum(pull(analyseBabies, !!col_name) == 9)
  proportion <- (((num99 + num98 + num9 + num999) / NumRowOriginal ) * 100)
  output[,i] <- c(num9, num98, num99,num999, proportion)
}

output <- as.data.frame(output)
colnames(output) <- newCols[3:NCOL(analyseBabies)]
rownames(output) <- c("9", "98", "99", "999", "proportion")

remove(num99)
remove(num98)
remove(num9)
remove(num999)
remove(i)
remove(col_name)
remove(proportion)


#Set asNumeric or asFactor
analyseBabies$wt <- as.numeric(analyseBabies$wt)
analyseBabies$gestation <- as.numeric(analyseBabies$gestation)
analyseBabies$date <- as.numeric(analyseBabies$date)
analyseBabies$parity <- as.numeric(analyseBabies$parity)
analyseBabies$race <- as.factor(analyseBabies$race)
analyseBabies$age <- as.numeric(analyseBabies$age)
analyseBabies$ht <- as.numeric(analyseBabies$ht)
analyseBabies$wt.1 <- as.numeric(analyseBabies$wt.1)
analyseBabies$drace <- as.factor(analyseBabies$drace)
analyseBabies$dage <- as.numeric(analyseBabies$dage)
analyseBabies$dht <- as.numeric(analyseBabies$dht)
analyseBabies$dwt <- as.numeric(analyseBabies$dwt)
analyseBabies$smoke <- as.factor(analyseBabies$smoke)
analyseBabies$time <- as.factor(analyseBabies$time)
analyseBabies$number <- as.factor(analyseBabies$number)

factorOrNumeric <- sapply(analyseBabies, is.factor)
dir.create("Initial_Eval")
#data will be a bit skewed, but only using this to look for strong
# correlation to choose a few factors to consider
for (x in 1:(NCOL(analyseBabies) - 2)){
  #filters out all values = 99, 98, 999, 9
  twoCol <- data.frame(pull(analyseBabies, "wt"), pull(analyseBabies, x+2))
  twoCol <- filter(twoCol, twoCol[2] != 99)
  twoCol <- filter(twoCol, twoCol[2] != 98)
  twoCol <- filter(twoCol, twoCol[2] != 999)
  twoCol <- filter(twoCol, twoCol[2] != 9)
  variable <- colnames(analyseBabies)[x+2]
  colnames(twoCol) <- c("wt", variable)

  #Prints output
  #creates SBS box plot if factor (and runs ANOVA)
  #creates scatter plot if numeric
  print("***********************")
  print(paste0(variable, ": QUICK SUMMARY ANALYSIS"))
  print("summary")
  print(summary(twoCol[2]))
  save <- paste0("InitialEval-", variable, ".png")
  if (factorOrNumeric[variable]){
    variableBoxPlot <- ggplot(data = twoCol, aes_string("wt", variable)) + geom_boxplot() + ggtitle(variable)
    print(variableBoxPlot)
    print("quick ANOVA")
    print(summary(aov(unlist(twoCol[,1])~ unlist(twoCol[,2]))))
    ggsave(save, variableBoxPlot, path = "Initial_Eval")
  } else {
    variableScatterPlot <- ggplot(data = twoCol, aes_string("wt", variable)) + geom_point() + ggtitle(variable)
    print(variableScatterPlot)
    print("correlation test")
    print(cor.test(as.vector(twoCol[1][,]), as.vector(twoCol[2][,])))
    ggsave(save, variableScatterPlot, path = "Initial_Eval")
  }
  print("***********************")
  print("***********************")
  dev.off()
}

remove(factorOrNumeric)
remove(twoCol)
remove(variable)
remove(variableBoxPlot)
remove(variableScatterPlot)
remove(x)
remove(save)
