library("ElemStatLearn")
data(marketing)

marketing = na.omit(marketing)

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


xmat = model.matrix(Income ~ . , data = marketing)

labels= c("Sex2", "Marital2", "Marital3", "Marital4", "Marital5", "Age", 
          "Edu2", "Edu3", "Edu4", "Edu5", "Edu6", "Ocuptn2", "Ocuptn3", 
          "Ocuptn4", "Ocuptn5", "Ocuptn6", "Ocuptn7", "Ocuptn8", 
          "Ocuptn9", "Lived2", "Lived3", "Lived4", "Lived5", "D_In2", 
          "D_In3", "Hhold", "Hhldu18", "Status2", "Status3", 
          "Hm_Tp2", "Hm_Tp3", "Hm_Tp4", "Hm_Tp5", "Ethnic2", 
          "Ethnic3", "Ethnic4", "Ethnic5", "Ethnic6", "Ethnic7", "Ethnic8", 
          "Lang2", "Lang3")
length(labels)
image((abs(cor(xmat[,-1])) > 0.5) + 0 , axes = F ,main = "Image plot highlighting highly correlated factors (abs()>0.5)")
mtext(text = c(labels), side = 2, line=0 ,cex = 0.9, at = seq(0, 1, 1/43), las = 1)
mtext(text = c(labels),side = 1, line=0 ,cex = 1, at = seq(0, 1, 1/43), las = 2)
