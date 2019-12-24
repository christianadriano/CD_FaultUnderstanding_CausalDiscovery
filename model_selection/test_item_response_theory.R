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

factor.scores.ltm(IRT_model)
# Factor-Scores for observed response patterns:
#     Item 1 Item 2 Item 3 Item 4 Item 5 Obs     Exp     z1 se.z1
# 1       0      0      0      0      0   3   2.277 -1.895 0.795
# 2       0      0      0      0      1   6   5.861 -1.479 0.796
# 3       0      0      0      1      0   2   2.596 -1.460 0.796
# 4       0      0      0      1      1  11   8.942 -1.041 0.800
# ...
# 26      1      1      0      1      1 173 173.310 -0.022 0.827
# 27      1      1      1      0      0  11   8.445 -0.329 0.816
# 28      1      1      1      0      1  61  62.520  0.117 0.832
# 29      1      1      1      1      0  28  29.127  0.139 0.833
# 30      1      1      1      1      1 298 296.693  0.606 0.855

#Obs = number o people who got row N. 
#The most common combination of answers is row 30 (getting all items correct)
#The second most common is get only one wrong, row 26.

#Exp = the prediction
#z1 = the actual ability associate with that row
#for instance, for row=1, the person ability is -1.895
#se.z1 = standard error 

person.fit(IRT_model)
# Person-Fit Statistics and P-values
# Call:  ltm(formula = LSAT ~ z1, IRT.param = TRUE)
# Alternative: Inconsistent response pattern under the estimated model
# 
#    Item 1 Item 2 Item 3 Item 4 Item 5      L0      Lz Pr(<Lz)
# 1       0      0      0      0      0 -4.0633 -1.2085  0.1134
# 2       0      0      0      0      1 -3.8211 -0.9436  0.1727
# 3       0      0      0      1      0 -4.6645 -1.8837  0.0298
# 4       0      0      0      1      1 -3.9565 -1.1454   0.126
# 5       0      0      1      0      0 -6.1607 -3.5228  0.0002

#Shows how each row fit with the model. The goodness of fit 
#is either significant or not (p-value in the last column Pr(<Lz))
#L0 is a statistic based on Levine and Rubin 1979 (see help documentation)
#Lz is another statistic bsed on Drasgow 1985 

#Continue analysis - https://www.youtube.com/watch?v=VtWsyUCGfhg&pbjreload=10