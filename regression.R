data("airquality")
View(airquality)
summary(airquality)
str(airquality)
is.na(airquality)

#  Find the line of regression.
lm(Temp ~ Wind, data=airquality)
lm(formula=Temp~Wind, data=airquality)
#  y = b0 + b1 X = 90.13 -1.23 X

# Use this equation to predict the temp. on a day when the observed wind speed is 8 mph.

plot(airquality$Wind,airquality$Temp)
abline(lm(Temp ~ Wind, data=airquality), col= "blue", lwd=2)

90.13 -1.23 *8

# Determine the residual of the observation in this set with a wind
# speed of 8 mph and a tenp. of 72 fahrenheit.

72-80.29   # Actual value - predicted value

model = lm(Temp ~ Wind, data=airquality)
summary(model)

plot(model,1)
###############################################
df=data.frame(airquality)
summary(df)






