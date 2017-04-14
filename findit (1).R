#This code gives an example of using Kosuke Imais package to find heterogenous treatment effects
#install.packages("FindIt")
#library(FindIt)
data(LaLonde)
F1 <-FindIt(model.treat= outcome ~ treat,
            model.main= ~ age+educ+black+hisp+white+
              marr+nodegr+log.re75+u75,
            model.int= ~ age+educ+black+hisp+white+
              marr+nodegr+log.re75+u75,
            data = LaLonde,
            type="binary",
            treat.type="single")

pred <- predict(F1)
head(pred$data, n=10)
tail(pred$data ,n=10)
plot(pred)
