# for corvif() function
source("http://www.highstat.com/Book2/HighstatLibV6.R")

# 3d plot libraries
library(scatterplot3d)
library(rgl)

# craft data to imitate collinearity of predictors
x=rnorm(100,1,1)
y=rnorm(100,1,1)

z = x + y
cx = x+y/4
cy = y + 2*x

# VIF of cx and cy is big ( = 16.6318)
corvif(data.frame(x=cx,y=cy))

# lm gives confident coefficients
summary(lm(formula = z ~ cx + cy))

# lm gives confident but with reverted sign for cx
summary(lm(formula = z ~ cx))
# lm gives confident but less coefficient for cy
summary(lm(formula = z ~ cy))