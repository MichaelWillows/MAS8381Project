library(ElemStatLearn)
?marketing
str(marketing)

##change data
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