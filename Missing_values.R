#Reading airquality dataset in R as R
R = read.csv("C:\\Users\\Admin\\Desktop\\airquality.csv", header = T)
#Looking at first few observations of R dataset
View(head(R))
#Drop X column from R dataset
R$X = NULL
#Looking at first few observation of dataset R 
head(R)
#Checking for NA values in R dataset
apply(R, 2, function(x) any(is.na(x)))
#So Ozone and Solar.R has NA values
#Adding NA values in R dataset for 3rd column and rows 4 to 10 
R[4:10,3] <- rep(NA,7)
#Checking if the values get added
head(R, 10)
#Adding NA values in R dataset for 4th column and rows 1 to 5 
R[1:5,4] <- NA
#Checking if the values get added
head(R, 10)
#For simplicity of imputing out missing values let us drop column Month and Day from dataset
R$Month = NULL
R$Day = NULL
#Looking at the dataset first few values
head(R)
#Let us see which varaible has how much missing values
summary(R) #So Ozone has most missing values
#Calling library mice
library(mice)
#Let us seethe missing-data patterns
md.pattern(R) #Returns a matrix containing the incomplete data
#md.pattern output says 104 samples are complete, 34 NA's in Ozone,
#4 NA's in Solar.R value and so on
#Let us see the plot of missing values using VIM package
library (VIM)
NA_plot <- aggr(R, col=c('pink','blue'), numbers=T, sortVars=T, labels=names(R),
                cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
#The plot shows that approximately 70% of the samples are not missing anything, 
#22% are missing the Ozone value, and the remaining ones show other missing patterns. 
#Let us plot the two variables together
marginplot(R[c(1,2)]) #Here we have red and blue box plots, if our assumption of 
#MCAR data is correct, then we expect both to be very similar.
marginplot(R[c(3,4)])
#Let us impute the missing values
R_impute <- mice(R,maxit=50,meth='pmm',seed=500)
#Above use mice function and predictive mean matching method to impute missing values
summary(R_impute)
#Checking for imputed values
R_impute$imp
#Let us put the imputed values in first of the five datasets
R_data = complete(R_impute,1)
#Check if all values have been imputed successfully 
summary(R_data)
#Checking the distribution for balance against original and imputed values
#Let us plot Ozone against all other variables
library(lattice)
xyplot(R_impute,Ozone ~ Wind+Temp+Solar.R,pch=18,cex=1)
#Here we have the shape of the magenta points (imputed) matching to the shape of the blue ones (observed).
#The matching shape tells us that the imputed values are indeed "plausible values
#Let us see the density of the observed and imputed values as well
densityplot(R_impute)
#Here imputed data density is showed in magenta while observed data densirty in blue
#We can also do a stripplot of to see the distribution of the variables as individual points
stripplot(R_impute, pch = 20, cex = 1.2)
#The results suggest that there is not much difference between observed and imputed values