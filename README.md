# 413week2

directory <- "C:/Users/Andrew/Documents/R"

setwd(directory)
ge <- read.table('m-ge3dx8113.txt', header=TRUE)

library(fBasics)
library(fpp)

#1a
basicStats(ge[3:6])

#1b
logge <- log(ge+1)
logge[1:2] <- ge[1:2]
basicStats(logge)[3:6]

#1c
t.test(logge[3])

#1d
densityPlot.n<-function (x, name, labels = TRUE, col = "steelblue", fit = TRUE, hist = TRUE, 
                         title = TRUE, grid = TRUE, rug = TRUE, skip = FALSE, ...) 
{
  stopifnot(is.timeSeries(x))
  N = NCOL(x)
  Units = colnames(x)
  if (length(col) == 1) 
    col = rep(col, times = N)
  for (i in 1:N) {
    X = as.vector(x[, i])
    if (skip) 
      X = X[X != 0]
    if (hist) {
      H = hist(x = X, , breaks = "FD", plot = FALSE)
      plot(x = H$mids, y = H$density, type = "h", lwd = 2, 
           main = "", xlab = "", ylab = "", col = "grey")
    }
    D = density(X, ...)
    if (hist) {
      lines(D$x, D$y, lwd = 2, col = "brown")
    }
    else {
      plot(D, col = col[i], ann = FALSE, ...)
    }
    if (title) {
      title(main = name, xlab = "Value", ylab = "Density")
    }
    if (fit) {
      mean = mean(X)
      sd = sd(X)
      xlim = range(H$breaks)
      s = seq(xlim[1], xlim[2], length = 201)
      lines(s, dnorm(s, mean, sd), lwd = 2, col = "darkgrey")
    }
    if (labels) {
      abline(v = mean, lwd = 2, col = "orange")
      Text = paste("Mean:", signif(mean, 3))
      mtext(Text, side = 4, adj = 0, col = "darkgrey", 
            cex = 0.7)
    }
    if (grid) {
      grid()
    }
    if (labels) {
      abline(h = 0, col = "grey")
    }
    if (rug) {
      rug(X, ticksize = 0.01, quiet = TRUE)
    }
  }
  invisible()
}

par(mfcol=c(2,2))
plot(ecdf(logge$ge), main='Empirical Cumulative Distribution Function: \nGE Log Returns', xlab='log return')
plot(ecdf(logge$sprtrn), main='Empirical Cumulative Distribution Function: \nS&P Log Returns', xlab='log return')
densityPlot.n(timeSeries(logge$ge, logge$date), name='Netflix Log Returns (in red) \nNormal Plot (in grey)')
densityPlot.n(timeSeries(logge$sprtrn, logge$date), name='S&P Composite Log Returns (in red) \nNormal Plot (in grey)')
par(mfcol=c(1,1))

#2a
netflix <- read.table('d-nflx3dx0913.txt', header=TRUE)

lognetflix <- log(netflix+1)
lognetflix[1:2] <- netflix[1:2]

skew=skewness(lognetflix$nflx)/sqrt(6/length(lognetflix$nflx))
pval <- 2*(1-pnorm(abs(skew)))
pval[1]

#2b
kur=kurtosis(lognetflix$nflx)/sqrt(24/length(lognetflix$nflx))
pval <- 2*(1-pnorm(abs(kur)))
pval[1]

#2c
t.test(lognetflix$nflx)

#3a
require(fpp)
plot(ukcars)

#3b
fit <- stl(ukcars, t.window=15, s.window="periodic", robust=T)   #break into components
plot(fit)

ukcars.adj <- seasadj(fit)                                       #get seasonally adjusted data

seasonal.component <- fit$time.series[,"seasonal"]
seasonal.component <- c(seasonal.component[2:5])                    #get seasonal component, starting with Q2, in this case
                                                                   #(Q2 is where the forecast data begins)
#3C
par(mfcol=c(2,2))
additive.fit <- holt(ukcars.adj, alpha=0.8, beta=0.2, damped=T, initial="optimal", h=8)
plot(additive.fit, main='Additive damped trend')                  #above, forecast seasonally adjusted data


additive.fit$mean <- additive.fit$mean + seasonal.component              #add seasonal component back into data (take advantage of vector recycling)
plot(additive.fit, main='Additive damped trend, reseasoned')


hist(residuals(additive.fit), nclass=10, main="Histogram of reseasoned residuals")    #residuals
plot(residuals(additive.fit), main="Plot of reseasoned residuals")
additive.fit$model$states                   #parameters
accuracy(additive.fit)                      #RMSE
par(mfcol=c(1,1))

#3d
par(mfcol=c(2,2))
linear.fit <- holt(ukcars.adj, alpha=0.8, beta=0.2, initial="simple", h=8)
plot(linear.fit, main='Holt\'s linear trend')                  #above, forecast seasonally adjusted data

linear.fit$mean <- linear.fit$mean + seasonal.component              #add seasonal component back into data (take advantage of vector recycling)
plot(linear.fit, main='Holt\'s linear trend, reseasoned')

hist(residuals(linear.fit), nclass=10, main="Histogram of reseasoned residuals")     #residuals
plot(residuals(linear.fit), main="Plot of reseasoned residuals", type="l")
linear.fit$model$states                    #paramaters
accuracy(linear.fit)                       #RMSE
par(mfcol=c(1,1))

#3e
ets.fit <- ets(ukcars, model="ZZZ")
ets.fit <- forecast(ets.fit, h=8)
plot(ets.fit)

par(mfcol=c(1,2))
hist(residuals(ets.fit), nclass=10, main = 'Histogram of residuals')    #residuals
plot(residuals(ets.fit), main='Plot of residuals')
ets.fit$states                         #parameters
accuracy(ets.fit)                      #RMSE
par(mfcol=c(1,1))

#3f

#RMSE calculated previously
#low RMSE = better in-sample fits

#3g
#refer to 3g, from previous
#plot residuals
