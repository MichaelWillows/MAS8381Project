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


