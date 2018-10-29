
  #============================================================================================================
  #
  # FIXED EFFECTS / RANDOM EFFECTS MODELLING
  # 
  #  Source: https://www.princeton.edu/~otorres/Panel101R.pdf
  #
  #============================================================================================================

    library(foreign)
    library(car)
    library(gplots)
    library(plm)
    library(tseries)
    library(lmtest)
  
    Panel <- read.dta("http://dss.princeton.edu/training/Panel101.dta")
    
    coplot(y ~ year|country, type="l", data=Panel)
    coplot(y ~ year|country, type="b", data=Panel)
    
    scatterplot(y~year|country, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=Panel)
    
    plotmeans(y ~ country, main="Heterogeineity across countries", data=Panel)
    plotmeans(y ~ year, main="Heterogeineity across years", data=Panel)
    
    # OLS model
    ols <-lm(y ~ x1, data=Panel)
    summary(ols)
    yhat <- ols$fitted
    plot(Panel$x1, Panel$y, pch=19, xlab="x1", ylab="y")
    abline(lm(Panel$y~Panel$x1),lwd=3, col="red")
    
    # LSDV model
    fixed.dum <-lm(y ~ x1 + factor(country) - 1, data=Panel)                                         # -1 means no intercept
    summary(fixed.dum)    
    yhat <- fixed.dum$fitted
    scatterplot(yhat~Panel$x1|Panel$country, boxplots=FALSE, xlab="x1", ylab="yhat",smooth=FALSE)
    abline(lm(Panel$y~Panel$x1),lwd=3, col="red")                                                    # OLS regression line for comparison
    
    # Fixed effects (same as LSDV model)
    fixed <- plm(y ~ x1, data=Panel, index=c("country", "year"), model="within")
    summary(fixed)
    
    # Random effects
    random <- plm(y ~ x1, data=Panel, index=c("country", "year"), model="random")
    summary(random)
    
    # Fixed or random - which is more approriate?
    #   To decide between fixed or random effects you can run a Hausman test where the null
    #   hypothesis is that the preferred model is random effects vs. the alternative the fixed
    #   effects (see Green, 2008, chapter 9). It basically tests whether the unique errors (ui) 
    #   are correlated with the regressors, the null hypothesis is they are not.
    #   Run a fixed effects model and save the estimates, then run a random model and save the
    #   estimates, then perform the test. If the p-value is significant (for example <0.05) then use
    #   fixed effects, if not use random effects.
    #   If p value is less than 0.05 use fixed effects
    phtest(fixed, random)
    
    # Testing for time-fixed effects
    #   The null is that no time-fixed effects needed. If p is less than 0.05 in pFtest and/or plmtest then need to use time-fixed effects.
    fixed.time <- plm(y ~ x1 + factor(year), data=Panel, index=c("country","year"), model="within")
    summary(fixed.time)
    pFtest(fixed.time, fixed)
    plmtest(fixed, c("time"), type=("bp"))
    
    # Testing random fixed effects 
    #   Breusch-Pagan Lagrange Multiplier for random effects. Null is no panel effect (i.e. OLS better). If p is less than 0.05 in plmtest use OLS.
    pool <- plm(y ~ x1, data=Panel, index=c("country", "year"), model="pooling")
    summary(pool)
    plmtest(pool, type=c("bp"))
    
    # Testing for stationarity
    #   Augmented Dickey-Fuller test for unit root. Nul is that the series has a unit root (not stationary). If p is less than 0.05, reject Null
    #   and assume no unit root present
    Panel.set <- plm.data(Panel, index = c("country", "year"))
    adf.test(Panel.set$y, k=2)
    
    # Testing for homoskedasiticity
    #   The null hypothesis for the Breush-Pagan test is homoskedasicity, if p is less than 0.05, reject Null and conclude the series has heteroskedasicity
    bptest(y ~ x1 + factor(country), data = Panel, studentize=F)
        
    # Controlling for heteroskedasicity
    coeftest(random)
    coeftest(random, vcovHC)
    coeftest(random, vcovHC(random, type = "HC3"))
    