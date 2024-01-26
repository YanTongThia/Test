dt = read.csv("DBS_SingDollar.csv") #alternatively read.csv
model=lm(dt$DBS ~ dt$SGD) #Multiple R-squared:  0.6665,	Adjusted R-squared:  0.6637 
summary(model)

pred = predict(model)
err = dt$DBS - pred
rmse=(mean(err^2)^0.5)
print(rmse) #0.6283699

dt$SGD2 = dt$SGD^2
model=lm(dt$DBS ~ dt$SGD + dt$SGD2) #Multiple R-squared:  0.685,	Adjusted R-squared:  0.6797 
summary(model)
pred = predict(model, newdata = dt)
err=dt$DBS - pred
rmse = (mean(err^2)^0.5)
print(rmse) #0.6107192

plot(dt$SGD, dt$DBS, main="Regression", xlab = "SGD", ylab="DBS")
abline(model) #model1
