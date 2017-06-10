# R-Code-Dickey-Fuller-Test
R Code I used to verify NICE assumptions on the dataset using an ARIMA model and applying a Dickey-Fuller Test
x=LakeHuron #dataset being used; found directly on R; has 98 observations
plot.ts(LakeHuron) #timeseries plot of dataset
x.acf=acf(x) #acf of original dataset
x.pacf=pacf(x) #pacf of original dataset
x.diff1=diff(x,1) #take the first difference to make it stationary
plot.ts(xdiff1) #plotting differenced dataset
x.diff2=diff(x,2) #taking second difference because first difference was not stationary
plot.ts(x.diff2) #plotting second-differenced dataset
model1=arima(x,order=c(2,1,0)) #first model ARIMA(2,1,0)
model1 #estimates of the paramters of the model
model2=arima(x,order=c(2,2,0)) #second model, ARIMA(2,2,0)
model2 #estimates of the paramters of the second model
qq1=qqnorm(model1$residuals) #qq plot of the first model
qq2=qqnorm(model2$residuals) #qq plot of the second model
h1=hist(model1$residuals) #histogram of the first model
h2=hist(model2$residuals) #histogram of the second model
plot1=plot(model1$residuals) #plot of the residuals of the first model
plot2=plot(model2$residuals) #plot of the residuals of the second model
j=seq(1:1:98) #variable to be entered in the x-axis of the upcoming plot
plot21=plot(j,model1$residuals) #second resuidual plot of the first model
plot22=plot(j,model2$residuals) #second residual plot of the second model
summary1=summary(model1) #summary stats on model 1
summary2=summary(model2) #summary stats on model 2
Bt1=Box.test(model1$residuals,lag=20) #box test of the first model
Bt2=Box.test(model2$residuals,lag=20) #box test of the second model
install.packages("tseries") #installs the tseries package that contains the dickey-fuller test command
library(tseries) #loading the package tseries
#Accept the H0 pvalue > 0.05
adf.test(x) #Augmented Dickey-Fuller test with lag order=(length(x)-1)^(1/3), augmented DF Test is a general version of the DF test
