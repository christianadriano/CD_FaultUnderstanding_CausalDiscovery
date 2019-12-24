"
Exercise on Item Response Theory.
I am interested in the aspect of choosing the items that make the my 
classifications mode discriminating. This could be a way to read 
the results. 

Which configuration of factors have higher discriminating power?
How to measure discriminating power? 

"

install.packages("ltm") #logistic distribution estimation
install.packages("psych") #normal distribution estimation
install.packages("mirt") #polytomous package
library(ltm)
library(psych)
library(mirt)

data(LSAT)
head(LSAT)

IRT_model <- ltm(LSAT ~ z1, IRT.param=TRUE)
# Call:
#   ltm(formula = LSAT ~ z1, IRT.param = TRUE)
# 
# Coefficients:
#   Dffclt  Dscrmn
# Item 1  -3.360   0.825
# Item 2  -1.370   0.723
# Item 3  -0.280   0.890
# Item 4  -1.866   0.689
# Item 5  -3.124   0.657
# 
# Log.Lik: -2466.653