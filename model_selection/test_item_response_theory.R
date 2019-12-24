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
#         Dffclt  Dscrmn
# Item 1  -3.360   0.825
# Item 2  -1.370   0.723
# Item 3  -0.280   0.890
# Item 4  -1.866   0.689
# Item 5  -3.124   0.657

#Dffclt = difficulty (x-axis for Median probability)
#Dscrmn = discrmination (slope, how good the question is in figuring out if
#are above or below the )

#These Dffclt values show the items are very easy, because they are far away
#to the negative side. 
#Recall the logit scale (log-odds = log(p/1-p)) = 
#-4 (never p=1), #-3 (almost never  p=0.95), 
#-2 (rarely p=0.88), -1 (p=0.73), 0 (p=0.5)

#Looking at the discrminators, they are very bad too, because they are almost
#flat. We want discriminators above 1.
# Log.Lik: -2466.653

plot(IRT_model, type="ICC")

plot(IRT_model, type="ICC", items=c(2,3))

"Plot the information, which tells me which are in the
x-axis gives me more information in terms of discrimination 
power of the items (all items). This is important to show design the items
in a way that they focus more or less on certain parameter
configurations, which in the case of the example is 
ability. See plot."
plot(IRT_model, type="IIC", items=0)

     