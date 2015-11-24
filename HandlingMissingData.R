library(ElemStatLearn)
data("marketing")
?marketing

# Note that we don't want to replace any NA values which
# occur in the "Income" column because this is what we are
# trying to predict. Luckily, there are no NA's here:
sum(is.na(marketing[,1]))
# ... is zero.

#####################
## Option 1: Remove missing data: 
#####################



sum(is.na(marketing))
# We lose 2694 observations by simply removing all
# entries which contain atleast one missing value.

# Method:
marketing = na.omit(marketing)


#####################
## Option 2: Fill-in or Impute missing values:
#####################

# Method 1: Replace each value with the mean for 
# it's respective predictor. With all the variables
# already being initially coded into catagories
# we will use the mode of the values.

# implimentation logic:
# for every column, if NA then set = to mode of the
# corresponding column is na.omit(marketing), 
# these 14 values need only be calculated once

# Turns out R doesn't have a mode() function...
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

## Calculating the column modes:

# colModes = numeric(14)
# for ( i in 2:14) {
#   colModes[i] = Mode(na.omit(marketing)[,i])
# }
# colModes
#

# ## Replacing all NA's as needed:

# for ( i in 2:14){
#   marketing[,i][is.na(marketing[,i])] = colModes[i]
# }

## The above two loops can be compacted to one:
NA_to_mode = function(m){
  for ( i in 2:14) {
    m[,i][is.na(m[,i])] = Mode(na.omit(m)[,i])
  }
  return(m)
}

## Make sure this has worked:
data(marketing)
Mode(marketing[,14])
head(marketing)
# So we hope the NA Language entry in row 1
# has been replaced by the colMode of 1.

marketing = NA_to_mode(marketing)

head(marketing)
# Looks like it worked.


#########################
## Fitting the other predictors to 
## impute missing values:
#########################

# install.packages("mi")
install.packages("mice")
data("marketing")

# We will be fitting a model so we change 
# predictors to factor types:

marketing$Sex = factor(marketing$Sex)
marketing$Marital = factor(marketing$Marital)
marketing$Edu = factor(marketing$Edu)
marketing$Occupation = factor(marketing$Occupation)
marketing$Lived = factor(marketing$Lived)
marketing$Dual_Income = factor(marketing$Dual_Income)
marketing$Status = factor(marketing$Status)
marketing$Home_Type = factor(marketing$Home_Type)
marketing$Ethnic = factor(marketing$Ethnic)
marketing$Language = factor(marketing$Language)

# This package called "mice" does everything.
# I don't understand it fully but it uses different
# methods for each predictor depending on the data 
# type of each column as well as how many levels there are.

method = c("", "", "lda", "", "lda", "lda", "lda", "", "pmm", "", "lda", "lda", "lda", "lda")
require(mice)

# Impute missing values:
result = (mice(marketing, m =3, me = method))

# Update our data set with the, now, completed one.
marketing = complete(result,1)

# Check all NA's have gone:
sum(is.na(marketing))



#####################
## plots of missing data. 
## Might be useful later??
#####################

install.packages("ggplot2")
library(ggplot2)
data(marketing)

prop_miss = numeric(14)
h = nrow(marketing)

for (i in 1:14){
  prop_miss[i] = sum(is.na(marketing[,i]))/h
}

x = colnames(marketing)
y = prop_miss

props = data.frame(x, y)

g = ggplot(data = props, aes(x, y))
g + geom_point(size = 5) + xlab("Predictor name.") + ylab("Proportion of missing data.") +
  theme(axis.text.x = element_text(color="black", size=14, angle=45),
        axis.text.y = element_text(color="black", size=14, angle=45)) +
  labs(title = "Proportion of missing data for each predictor") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        plot.title = element_text(size = rel(2)))

## It will be worth noting that 10% of the data for "Lived" 
## has to be imputed/filled-in.

