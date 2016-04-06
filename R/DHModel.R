####################################################
#### Calculate RMSE (Root mean square error)

rmse <- function(error){
  sqrt(mean(error^2))
}

####################################################
#### Calculate AIC from residual sum

AIC_RSS <- function(n, k, error){
  AIC_fit <- (n * log(mean(error^2)) + 2*k)
}

#### Calculate RSE (Residual standard error):
#### sqrt(SSE/(n-p)), i.e., sqrt(RSS/df), where n = sample size,
#### p = numbers of parameters, df=degree of freedom

rse <- function(error, n, p){
  (sum(error^2)/(n-p))
}

####################################################
#### Calculate basal area (multiply by 1000 to express in cm2)

BAfun <- function(D){0.00007854*D^2 * 10000}

####################################################



#heightRegression <- function(tag, diameter, actual_height, sub_dat, E_Value){

# should be a dataframe

# tagAndDiameterWithHeightsFrame (tag, diameter, measure height)
# unheightedDiameterFrame (tag, diameter)
# single e_value for the plot

## returns - frame with tags, diameter, computed heights and algorithms
heightRegression <- function(tagAndDiameterWithHeightsFrame, unheightedDiameterFrame, E_Value)
{
  if (missing(diameter) || missing(tag) || missing(E_Value)) {
    stop("missing parameters")
  }
  Ht_act <- as.numeric(actual_height)
  D <- as.numeric(diameter)
  n <- length(Ht_act)
  E_Value <- as.numeric(E_Value)

  ##### Calculating RSD for Uncertainty & Organizing Output
  
  results.fun <- function (fit, equation, Ht_act, D, error, PredHt, coef){
    
    AIC <- AIC_RSS(n = length(Ht_act), length(fit$par), error)
    RMSE <- rmse(error)
    RSD <- sqrt(sum(error^2)/(length(Ht_act)-2))
    Xstand <- (D - mean(D))^2
    SEM <- RSD * sqrt(1/length(Ht_act) + Xstand/sum(Xstand))
    res.df <- cbind(D, Ht_act, PredHt, Residuals = error, SEM = SEM)
    return(list(equation = equation, RSD = RSD, vars = c(coef, AIC = AIC, RMSE = RMSE),  res = res.df))
  }
  
  ##### Below are all the different H-D models to be compared
  ##### EQ1: Linear model: H = a + b*D, where D = DBH
  
  startvals1 <- list(a = 2, b = 1)
  Fit_nls1 <- function(x) {
    a <- x[1]
    b <- x[2]
    t1 <- (a + b* D)
    t1 <- sum((Ht_act-t1)^2)
    return(t1)
  }
  
  Eq1 <- optim(par=startvals1, fn = Fit_nls1, hessian = FALSE, control = list(parscale = unlist(startvals1)))
  a1 = Eq1$par[1]
  b1 = Eq1$par[2]
  PredHt <- a1 + b1 * D
  error <- (Ht_act-PredHt)
  RMSE_Eq1 <- rmse(error)
  AIC_Eq1 <- AIC_RSS(n, length(startvals1), error)
  equation <- as.formula(y ~ a + b * D)
  
  res1 <- results.fun(fit = Eq1, equation, Ht_act = Ht_act, D = D, error = error,
                      PredHt = PredHt, coef = c(a1, b1))
  
  
  ##### EQ2: Quadratic model, Eq. 27 in Table 2 of page 128 (Huang et al., 2000): H = a + b*D + c*D^2, where D = DBH
  
  startvals2 <- list(a = -5, b = 1, c = 0.01)
  Fit_nls2 <- function(x) {
    a <- x[1]
    b <- x[2]
    c <- x[3]
    t1 <- (a + b * D + c * (D^2))
    t1 <- sum((Ht_act-t1)^2)
    return(t1)
  }
  
  Eq2 <- optim(par=startvals2, fn = Fit_nls2, hessian = FALSE, control = list(parscale = unlist(startvals2)))
  a2 = Eq2$par[1]
  b2 = Eq2$par[2]
  c2 = Eq2$par[3]
  PredHt <- (a2 + b2 * D + c2 * (D^2))
  error<- (Ht_act-PredHt)
  RMSE_Eq2 <- rmse(error)
  AIC_Eq2 <- AIC_RSS(n, length(startvals2), error)
  equation <- as.formula(y ~ a + b * D + c * (D^2))
  
  res2 <- results.fun(fit = Eq2, equation, Ht_act = Ht_act, D = D, error = error,
                      PredHt = PredHt, coef = c(a2, b2, c2))
  
  ##### EQ3: Linear models on log scale (Log~Log): H=exp(a+b*ln(D)), where D = DBH (Eq2 of page 8617 (Djomo et al., 2010; Molto et al., 2013))
  
  startvals3 <- list(a = 1, b =1)
  Fit_nls3 <- function(x) {
    a <- x[1]
    b <- x[2]
    t1 <- exp(a + b*log(D))
    t1 <- sum((Ht_act-t1)^2)
    return(t1)
  }
  
  Eq3 <- optim(par=startvals3, fn = Fit_nls3, hessian = FALSE, control = list(parscale = unlist(startvals3)))
  a3 = Eq3$par[1]
  b3 = Eq3$par[2]
  PredHt <- exp(a3 + b3*log(D))
  error<- (Ht_act-PredHt)
  RMSE_Eq3 <- rmse(error)
  AIC_Eq3 <- AIC_RSS(n, length(startvals3), error)
  equation <- as.formula(y ~ exp(a + b*log(D)))
  
  res3 <- results.fun(fit = Eq3, equation, Ht_act = Ht_act, D = D, error = error,
                      PredHt = PredHt, coef = c(a3, b3))
  
  
  ##### EQ4: Quadratic model on log scale: Ln(H) = a + b * ln(D) + c * ln(D^2) (Djomo et al., 2010)
  
  startvals4 <- list(a = -5, b = 1, c = 0.01)
  Fit_nls4 <- function(x) {
    a <- x[1]
    b <- x[2]
    c <- x[3]
    t1 <- (a + b * log(D) + c * log(D^2))
    t1 <- sum((Ht_act-exp(t1))^2)
    return(t1)
  }
  
  Eq4 <- optim(par=startvals4, fn = Fit_nls4, hessian = FALSE, control = list(parscale = unlist(startvals4)))
  a4 = Eq4$par[1]
  b4 = Eq4$par[2]
  c4 = Eq4$par[3]
  PredHt <- a4 + b4 * log(D) + c4 * log(D^2)
  error<- (Ht_act-PredHt)
  RMSE_Eq4 <- rmse(error)
  AIC_Eq4 <- AIC_RSS(n, length(startvals4), error)
  equation <- as.formula(y ~ a + b * log(D) + c * log(D^2))
  
  res4 <- results.fun(fit = Eq4, equation, Ht_act = Ht_act, D = D, error = error,
                      PredHt = PredHt, coef = c(a4, b4, c4))
  
  ##### EQ5: Non-linear model: H = a + b * ln(D), where D = DBH (Fang and Bailey, 1998; Djomo et al., 2010; Molto et al., 2013)
  ##### Log-linear eq.1 in page 8617 (Fang and Bailey, 1998; Molto et al., 2013)
  startvals5 <- list(a = 1, b =2)
  Fit_nls5 <- function(x) {
    a <- x[1]
    b <- x[2]
    t1 <- (a + b*log(D))              #Log-linear eq.1 in page 8617 (Fang and Bailey, 1998; Molto et al., 2013)
    t1 <- sum((Ht_act-t1)^2)
    return(t1)
  }
  
  Eq5 <- optim(par=startvals5, fn = Fit_nls5, hessian = FALSE, control = list(parscale = unlist(startvals5)))
  a5 = Eq5$par[1]
  b5 = Eq5$par[2]
  PredHt <- a5 + b5*log(D)
  error<- (Ht_act-PredHt)
  RMSE_Eq5 <- rmse(error)
  AIC_Eq5 <- AIC_RSS(n, length(startvals5), error)
  equation <- as.formula(y ~ a + b*log(D))
  
  res5 <- results.fun(fit = Eq5, equation = equation, Ht_act = Ht_act, D = D, error = error,
                      PredHt = PredHt, coef = c(a5, b5))
  
  
  ##### EQ6: Simplified (two-parameter) Weibull equation: H = a*(1-exp(-D/b)), where D=DBH, page 8617 (Molto et al., 2013)
  
  startvals6 <- list(a = 30, b = 5)
  Fit_nls6 <- function(x) {
    a <- x[1]
    b <- x[2]
    t1 <- (a * (1-exp(I(-D/b))))
    t1 <- sum((Ht_act-t1)^2)
    return(t1)
  }
  
  Eq6 <- optim(par=startvals6, fn = Fit_nls6, hessian = FALSE, control = list(parscale = unlist(startvals6)))
  a6 = Eq6$par[1]
  b6 = Eq6$par[2]
  PredHt <- a6 * (1-exp(I(-D/b6)))
  error<- (Ht_act-PredHt)
  RMSE_Eq6 <- rmse(error)
  AIC_Eq6 <- AIC_RSS(n, length(startvals6), error)
  equation <- as.formula(y ~ a * (1-exp(I(-D/b))))
  
  res6 <- results.fun(fit = Eq6, equation, Ht_act = Ht_act, D = D, error = error,
                      PredHt = PredHt, coef = c(a6, b6))
  
  ##### EQ7: Three-parameter Weibull equation: H = a * (1-exp(-b*D^c)), where D = DBH
  ##### Equation 13 of Huang et al., 1992; Lewis et al., 2009; Feldpausch et al., 2012; Mitchard et al., 2014)
  startvals7 <- list(a = 54.01, b = -0.053, c = 0.0759)
  Fit_nls7 <- function(x) {
    a <- x[1]
    b <- x[2]
    c <- x[3]
    t1 <- (a*(1-exp(-b*D^c)))
    t1 <- sum((Ht_act-t1)^2)
    return(t1)
  }
  
  Eq7 <- optim(par=startvals7, fn = Fit_nls7, hessian = FALSE, control = list(parscale = unlist(startvals7)))
  a7 = Eq7$par[1]
  b7 = Eq7$par[2]
  c7 = Eq7$par[3]
  PredHt <- a7*(1-exp(-b7*D^c7))
  error<- (Ht_act-PredHt)
  RMSE_Eq7 <- rmse(error)
  AIC_Eq7 <- AIC_RSS(n, length(startvals7), error)
  equation <- as.formula(y ~ a*(1-exp(-b*D^c)))
  
  res7 <- results.fun(fit = Eq7, equation, Ht_act = Ht_act, D = D, error = error,
                      PredHt = PredHt, coef = c(a7, b7, c7))
  
  ##### EQ8: Michaelis-Menten equation: H = a*D /(b+D), in page 8618 (Molto et al., 2013; Equation 3 in Huang et al. (1992))
  
  startvals8 <- list(a = 10, b = 2)              #Start points were estimated from Figure 1 in page 8632 (Molto et al., 2013)
  Fit_nls8 <- function(x) {
    a <- x[1]
    b <- x[2]
    t1 <- (a * I(D)/(b + I(D)))
    t1 <- sum((Ht_act-t1)^2)
    return(t1)
  }
  
  Eq8 <- optim(par=startvals8, fn = Fit_nls8, hessian = FALSE, control = list(parscale = unlist(startvals8)))
  a8 = Eq8$par[1]
  b8 = Eq8$par[2]
  PredHt <- a8 * I(D)/(b8 + I(D))
  error<- (Ht_act-PredHt)
  RMSE_Eq8 <- rmse(error)
  AIC_Eq8 <- AIC_RSS(n, length(startvals8), error)
  equation <- as.formula(y ~ (a * I(D)/(b + I(D))))
  
  res8 <- results.fun(fit = Eq8, equation, Ht_act = Ht_act, D = D, error = error,
                      PredHt = PredHt, coef = c(a8, b8))
  
  
  ##### EQ9: Power-law equation: H=a*D^b (Eq 1 of Huang et al., 1992; Chave et al., 2005; Feldpausch et al., 2012; Ngomanda et al., 2014)
  ##### Start points were estimated from Ngomanda et al.(2014)
  
  startvals9 <- list(a = 26.6049, b = -9.4854)
  Fit_nls9 <- function(x) {
    a <- x[1]
    b <- x[2]
    t1 <- ( a* D^b )
    t1 <- sum((Ht_act-t1)^2)
    return(t1)
  }
  
  Eq9 <- optim(par=startvals9, fn = Fit_nls9, hessian = FALSE, control = list(parscale = unlist(startvals9)))
  a9 = Eq9$par[1]
  b9 = Eq9$par[2]
  PredHt <- a9* D^b9
  error<- (Ht_act-PredHt)
  RMSE_Eq9 <- rmse(error)
  AIC_Eq9 <- AIC_RSS(n, length(startvals9), error)
  equation <- as.formula(y ~ a* D^b)
  
  res9 <- results.fun(fit = Eq9, equation, Ht_act = Ht_act, D = D, error = error,
                      PredHt = PredHt, coef = c(a9, b9))
  
  ##### EQ10:Model 3: Ln(H) = a + b/D or H = a * exp(b/D), where D=DBH (Djomo et al., 2010; Equation 6 in Huang et al. (1992))
  
  startvals10 <- list(a = 26.6049, b = -9.4854)
  Fit_nls10 <- function(x) {
    a <- x[1]
    b <- x[2]
    t1 <- (exp(a + b/D))
    t1 <- sum((Ht_act-t1)^2)
    return(t1)
  }
  
  Eq10 <- optim(par=startvals10, fn = Fit_nls10, hessian = FALSE, control = list(parscale = unlist(startvals10)))
  a10 = Eq10$par[1]
  b10 = Eq10$par[2]
  PredHt <- exp(a10 + b10/D)
  error<- (Ht_act-PredHt)
  RMSE_Eq10 <- rmse(error)
  AIC_Eq10 <- AIC_RSS(n, length(startvals10), error)
  equation <- as.formula(y ~ exp(a + b/D))
  
  res10 <- results.fun(fit = Eq10, equation, Ht_act = Ht_act, D = D, error = error,
                       PredHt = PredHt, coef = c(a10, b10))
  
  ##### EQ11: H = a + b*(1-exp(-c*(D-Dmin))), where D=DBH (Fang and Bailey, 1998)
  ##### Start points were for Balsam poplar of Equation 12 in page 1289 (Huang et al., 1992)
  startvals11 <- list(a = 44.7, b =42.3, c = 0.026)
  Fit_nls11 <- function(x) {
    a <- x[1]
    b <- x[2]
    c <- x[3]
    t1 <- a + b*(1-exp(-c*(D-min(D))))
    t1 <- sum((Ht_act-t1)^2)
    return(t1)
  }
  
  Eq11 <- optim(par=startvals11, fn = Fit_nls11, hessian = FALSE, control = list(parscale = unlist(startvals11)))
  a11 = Eq11$par[1]
  b11 = Eq11$par[2]
  c11 = Eq11$par[3]
  PredHt <- a11 + b11 * (1-exp(-c11 * (D - min(D))))
  error<- (Ht_act-PredHt)
  RMSE_Eq11 <- rmse(error)
  AIC_Eq11 <- AIC_RSS(n, length(startvals11), error)
  equation <- as.formula(y ~ a + b*(1-exp(-c*(D-min(D)))))
  
  res11 <- results.fun(fit = Eq11, equation, Ht_act = Ht_act, D = D, error = error,
                       PredHt = PredHt, coef = c(a11, b11, c11))
  
  
  ##### EQ12:H = a + (a-1.3)*b/(D+b), where D=DBH  (Fang and Bailey, 1998)
  ##### Start points were for Balsam poplar of Equation 12 in page 1289 (Huang et al., 1992)
  startvals12 <- list(a = 33.62, b =20.88)
  Fit_nls12 <- function(x) {
    a <- x[1]
    b <- x[2]
    c <- x[3]
    t1 <- (a + (a - 1.3) * b/(D + b))
    t1 <- sum((Ht_act-t1)^2)
    return(t1)
  }
  
  Eq12 <- optim(par=startvals12, fn = Fit_nls12, hessian = FALSE, control = list(parscale = unlist(startvals12)))
  a12 = Eq12$par[1]
  b12 = Eq12$par[2]
  c12 = Eq12$par[3]
  PredHt <- a12 + (a12 - 1.3) * b12/(D + b12)
  error<- (Ht_act-PredHt)
  RMSE_Eq12 <- rmse(error)
  AIC_Eq12 <- AIC_RSS(n, length(startvals12), error)
  equation <- as.formula(y ~ (a + (a - 1.3) * b/(D + b)))
  
  res12 <- results.fun(fit = Eq12, equation, Ht_act = Ht_act, D = D, error = error,
                       PredHt = PredHt, coef = c(a12, b12, c12))
  
  ##### EQ13:H = 1.3 + exp(a + b*D^c), where D=DBH (Eq 10 of Huang et al. (1992, 2000); Colbert et al., 2002)
  ##### Start points were for Black williows in page 173 (Colbert et al., 2002)
  startvals13 <- list(a = 4.5535, b =-3.7529, c = 1)
  Fit_nls13 <- function(x) {
    a <- x[1]
    b <- x[2]
    c <- x[3]
    t1 <- (1.3 + exp(a + b * D^c))
    t1 <- sum((Ht_act-t1)^2)
    return(t1)
  }
  
  Eq13 <- optim(par=startvals13, fn = Fit_nls13, hessian = FALSE, control = list(parscale = unlist(startvals13)))
  a13 = Eq13$par[1]
  b13 = Eq13$par[2]
  c13 = Eq13$par[3]
  PredHt <- 1.3 + exp(a13 + b13 * D^c13)
  error<- (Ht_act-PredHt)
  RMSE_Eq13 <- rmse(error)
  AIC_Eq13 <- AIC_RSS(n, length(startvals13), error)
  equation <- as.formula(y ~ 1.3 + exp(a + b * D^c))
  
  res13 <- results.fun(fit = Eq13, equation, Ht_act = Ht_act, D = D, error = error,
                       PredHt = PredHt, coef = c(a13, b13, c13))
  
  ##### EQ14:H = 1.3 + a*(1-exp(-b*D))^c, where D=DBH (Equation 12 in Huang et al. (1992, 2000); Equation 2 in Sharma and Parton (2007))
  ##### Start points were for Balsam poplar of Equation 12 in page 1289 (Huang et al., 1992)
  startvals14 <- list(a = 1.0462, b = 1.0464, c=0.9465)
  Fit_nls14 <- function(x) {
    a <- x[1]
    b <- x[2]
    c <- x[3]
    t1 <- (1.3 + a*(1-exp(-b*D))^c)
    t1 <- sum((Ht_act-t1)^2)
    return(t1)
  }
  
  Eq14 <- optim(par=startvals14, fn = Fit_nls14, hessian = FALSE, control = list(parscale = unlist(startvals14)))
  a14 = Eq14$par[1]
  b14 = Eq14$par[2]
  c14 = Eq14$par[3]
  PredHt <- 1.3 + a14 *(1-exp(-b14 * D))^c14
  error<- (Ht_act-PredHt)
  RMSE_Eq14 <- rmse(error)
  AIC_Eq14 <- AIC_RSS(n, length(startvals14), error)
  equation <- as.formula(y ~ 1.3 + a*(1-exp(-b*D))^c)
  
  res14 <- results.fun(fit = Eq14, equation, Ht_act = Ht_act, D = D, error = error,
                       PredHt = PredHt, coef = c(a14, b14, c14))
  
  
  ##### Following equations from Table 3 in Huang et al. (1992) and Table 2 in Huang et al. (2000).
  ##### Parameter estimations of Balsam poplar (broad leaf) in Tables 4 and 5 (Huang et al., 1992)
  ##### were used as start values in the new fitting for Gabonese rainforests.
  
  ##### EQ15: Equation 4 in Huang et al.(1992 and 2000): H = 1.3 + a * (1 - exp(-bD)), where D = DBH
  
  startvals15 <- list(a = 25.3302, b = 0.0512)
  Fit_nls15 <- function(x) {
    a <- x[1]
    b <- x[2]
    t1 <- (a * (1 - exp(-b * D)) )
    t1 <- sum((Ht_act-t1)^2)
    return(t1)
  }
  
  Eq15 <- optim(par=startvals15, fn = Fit_nls15, hessian = FALSE, control = list(parscale = unlist(startvals15)))
  a15 = Eq15$par[1]
  b15 = Eq15$par[2]
  PredHt <- a15 * (1 - exp(-b15 * D))
  error<- (Ht_act-PredHt)
  RMSE_Eq15 <- rmse(error)
  AIC_Eq15 <- AIC_RSS(n, length(startvals15), error)
  equation <- as.formula(y ~ a * (1 - exp(-b * D)))
  
  res15 <- results.fun(fit = Eq15, equation, Ht_act = Ht_act, D = D, error = error,
                       PredHt = PredHt, coef = c(a15, b15))
  
  
  ##### EQ16:Equation 5 in Huang et al. (1992 and 2000): H = 1.3 + D^2 /(a + bD)^2, where D = DBH
  
  startvals16 <- list(a = 1.3209, b = 0.1813)
  Fit_nls16 <- function(x) {
    a <- x[1]
    b <- x[2]
    t1 <- (1.3 + D^2 /((a + b * D)^2) )
    t1 <- sum((Ht_act-t1)^2)
    return(t1)
  }
  
  Eq16 <- optim(par=startvals16, fn = Fit_nls16, hessian = FALSE, control = list(parscale = unlist(startvals16)))
  a16 = Eq16$par[1]
  b16 = Eq16$par[2]
  PredHt <- 1.3 + D^2 /((a16 + b16 * D)^2)
  error<- (Ht_act-PredHt)
  RMSE_Eq16 <- rmse(error)
  AIC_Eq16 <- AIC_RSS(n, length(startvals16), error)
  equation <- as.formula(y ~ 1.3 + D^2 /((a + b * D)^2))
  
  res16 <- results.fun(fit = Eq16, equation, Ht_act = Ht_act, D = D, error = error,
                       PredHt = PredHt, coef = c(a16, b16))
  
  
  ##### EQ17: Equation 7 in Huang et al. (1992 and 2000): H = 1.3 + 10^a*(D^b), where D = DBH
  
  startvals17 <- list(a = 0.4305, b = 0.5871)
  Fit_nls17 <- function(x) {
    a <- x[1]
    b <- x[2]
    t1 <- (1.3 + (10^a) * (D^b) )
    t1 <- sum((Ht_act-t1)^2)
    return(t1)
  }
  
  Eq17 <- optim(par=startvals17, fn = Fit_nls17, hessian = FALSE, control = list(parscale = unlist(startvals17)))
  a17 = Eq17$par[1]
  b17 = Eq17$par[2]
  PredHt <- 1.3 + (10^a17) * (D^b17)
  error<- (Ht_act-PredHt)
  RMSE_Eq17 <- rmse(error)
  AIC_Eq17 <- AIC_RSS(n, length(startvals17), error)
  equation <- as.formula(y ~ 1.3 + (10^a) * (D^b))
  
  res17 <- results.fun(fit = Eq17, equation, Ht_act = Ht_act, D = D, error = error,
                       PredHt = PredHt, coef = c(a17, b17))
  
  
  ##### EQ18:Equation 8 in Huang et al. (1992 and 2000): H = 1.3 + (a*D)/(D+1) + b*D, where D = DBH
  
  startvals18 <- list(a = 6.5487, b = 0.4507)
  Fit_nls18 <- function(x) {
    a <- x[1]
    b <- x[2]
    t1 <- (1.3 + a*D/(D+1) + b*D )
    t1 <- sum((Ht_act-t1)^2)
    return(t1)
  }
  
  Eq18 <- optim(par=startvals18, fn = Fit_nls18, hessian = FALSE, control = list(parscale = unlist(startvals18)))
  a18 = Eq18$par[1]
  b18 = Eq18$par[2]
  PredHt <- 1.3 + a18 * D/(D+1) + b18*D
  error<- (Ht_act-PredHt)
  RMSE_Eq18 <- rmse(error)
  AIC_Eq18 <- AIC_RSS(n, length(startvals18), error)
  equation <- as.formula(y ~ 1.3 + a*D/(D+1) + b*D)
  
  res18 <- results.fun(fit = Eq18, equation, Ht_act = Ht_act, D = D, error = error,
                       PredHt = PredHt, coef = c(a18, b18))
  
  
  #####  EQ19:Equation 9 in Huang et al. (1992 and 2000): H = 1.3 + a*(D/(D+1))^b, where D = DBH
  
  startvals19 <- list(a = 27.1752, b = 10.1979)
  Fit_nls19 <- function(x) {
    a <- x[1]
    b <- x[2]
    t1 <- (1.3 + a*(D/(1 + D))^b )
    t1 <- sum((Ht_act-t1)^2)
    return(t1)
  }
  
  Eq19 <- optim(par=startvals19, fn = Fit_nls19, hessian = FALSE, control = list(parscale = unlist(startvals19)))
  a19 = Eq19$par[1]
  b19 = Eq19$par[2]
  PredHt <- 1.3 + a19*(D/(1 + D))^b19
  error<- (Ht_act-PredHt)
  RMSE_Eq19 <- rmse(error)
  AIC_Eq19 <- AIC_RSS(n, length(startvals19), error)
  equation <- as.formula(y ~ 1.3 + a*(D/(1 + D))^b)
  
  res19 <- results.fun(fit = Eq19, equation, Ht_act = Ht_act, D = D, error = error,
                       PredHt = PredHt, coef = c(a19, b19))
  
  ##### EQ20:Equation 10 in Huang et al. (1992 and 2000): H = 1.3 + exp(a + b*D^c), where D = DBH
  startvals20 <- list(a = 18.804146, b = -16.760486, c=-0.025197)
  Fit_nls20 <- function(x) {
    a <- x[1]
    b <- x[2]
    c <- x[3]
    t1 <- 1.3 + exp(a + b * (D^c))
    t1 <- sum((Ht_act-t1)^2)
    return(t1)
  }
  
  Eq20 <- optim(par=startvals20, fn = Fit_nls20, hessian = FALSE, control = list(parscale = unlist(startvals20)))
  a20 = Eq20$par[1]
  b20 = Eq20$par[2]
  c20 = Eq20$par[3]
  PredHt <- 1.3 + exp(a20 + b20 * (D^c20))
  error<- (Ht_act-PredHt)
  RMSE_Eq20 <- rmse(error)
  AIC_Eq20 <- AIC_RSS(n, length(startvals20), error)
  equation <- as.formula(y ~ 1.3 + exp(a + b * (D^c)))
  
  res20 <- results.fun(fit = Eq20, equation, Ht_act = Ht_act, D = D, error = error,
                       PredHt = PredHt, coef = c(a20, b20, c20))
  
  ##### EQ21: Equation 11 in Huang et al. (1992 and 2000): H = 1.3 + a/(1+b*exp(-c*D)), where D = DBH
  startvals21 <- list(a = 2.5241, b = 0.0012, c=0.1404)
  Fit_nls21 <- function(x) {
    a <- x[1]
    b <- x[2]
    c <- x[3]
    t1 <- 1.3 + a/(1 + b*exp(-c*D))     #Equation 11 in Huang et al. (1992)
    t1 <- sum((Ht_act-t1)^2)
    return(t1)
  }
  
  Eq21 <- optim(par=startvals21, fn = Fit_nls21, hessian = FALSE, control = list(parscale = unlist(startvals21)))
  a21 = Eq21$par[1]
  b21 = Eq21$par[2]
  c21 = Eq21$par[3]
  PredHt <- 1.3 + a21 /(1 + b21 *exp(-c21 * D))
  error <- (Ht_act - PredHt)
  RMSE_Eq21 <- rmse(error)
  AIC_Eq21 <- AIC_RSS(n, length(startvals21), error)
  equation <- as.formula(y ~ 1.3 + a/(1 + b*exp(-c*D)))
  
  res21 <- results.fun(fit = Eq21, equation, Ht_act = Ht_act, D = D, error = error,
                       PredHt = PredHt, coef = c(a21, b21, c21))
  
  ##### EQ22: Equation 14 in Huang et al. (1992 and 2000): H = 1.3 + a*exp(-b*exp(-c*D)), where D = DBH   (For Eqs. 12 and 13 in Huang et al. (1992 and 2002), please see the above eqs 14 and 7 in the file.)
  startvals22 <- list(a = 1.6368, b = 2.1570, c=1.0951)
  Fit_nls22 <- function(x) {
    a <- x[1]
    b <- x[2]
    c <- x[3]
    t1 <- (1.3 + a*exp(-b*exp(-c*D)))
    t1 <- sum((Ht_act-t1)^2)
    return(t1)
  }
  
  Eq22 <- optim(par=startvals22, fn = Fit_nls22, hessian = FALSE, control = list(parscale = unlist(startvals22)))
  a22 = Eq22$par[1]
  b22 = Eq22$par[2]
  c22 = Eq22$par[3]
  PredHt <- 1.3 + a22 * exp(-b22 * exp(-c22 * D))
  error <- (Ht_act - PredHt)
  RMSE_Eq22 <- rmse(error)
  AIC_Eq22 <- AIC_RSS(n, length(startvals22), error)
  equation <- as.formula(y ~ 1.3 + a*exp(-b*exp(-c*D)))
  
  res22 <- results.fun(fit = Eq22, equation, Ht_act = Ht_act, D = D, error = error,
                       PredHt = PredHt, coef = c(a22, b22, c22))
  
  ##### EQ23: Equation 15 in Huang et al. (1992) and Eq. 16 in Huang et al. (2000): H = (1.3^b + (c^b-1.3^b)*(1-exp(-a*D))/(1-exp(-a*100)))^(1/b)
  startvals23 <- list(a = 0.0464, b = 1.0716, c=27.0745)
  Fit_nls23 <- function(x) {
    a <- x[1]
    b <- x[2]
    c <- x[3]
    t1 <- ((1.3^b + (c^b-1.3^b)*(1-exp(-a*D))/(1-exp(-a*100)))^(1/b))
    t1 <- sum((Ht_act-t1)^2)
    return(t1)
  }
  
  Eq23 <- optim(par=startvals23, fn = Fit_nls23, hessian = FALSE, control = list(parscale = unlist(startvals23)))
  a23 = Eq23$par[1]
  b23 = Eq23$par[2]
  c23 = Eq23$par[3]
  PredHt <- (1.3^b23 + (c23^b23-1.3^b23)*(1-exp(-a23*D))/(1-exp(-a23*100)))^(1/b23)
  error <- (Ht_act - PredHt)
  RMSE_Eq23 <- rmse(error)
  AIC_Eq23 <- AIC_RSS(n, length(startvals23), error)
  equation <- as.formula(y ~ (1.3^b + (c^b-1.3^b)*(1-exp(-a*D))/(1-exp(-a*100)))^(1/b))
  
  res23 <- results.fun(fit = Eq23, equation, Ht_act = Ht_act, D = D, error = error,
                       PredHt = PredHt, coef = c(a23, b23, c23))
  
  ##### EQ24: Equation 16 in Huang et al. (1992) and Eq. 17 in Huang et al. (2000): H =1.3 + D^2/(a + b*D + c*D^2)
  ##### Start points were for Balsam poplar of Equation 12 in page 1289 (Huang et al., 1992)
  
  startvals24 <- list(a = 0.0038, b = 0.7027, c = 0.0270)
  Fit_nls24 <- function(x) {
    a <- x[1]
    b <- x[2]
    c <- x[3]
    t1 <- (1.3 + D^2 /(a + b * D + c * D^2))
    t1 <- sum((Ht_act-t1)^2)
    return(t1)
  }
  
  Eq24 <- optim(par=startvals24, fn = Fit_nls24, hessian = FALSE, control = list(parscale = unlist(startvals24)))
  a24 = Eq24$par[1]
  b24 = Eq24$par[2]
  c24 = Eq24$par[3]
  PredHt <- 1.3 + D^2 /(a24 + b24 * D + c24 * D^2)
  error <- (Ht_act - PredHt)
  RMSE_Eq24 <- rmse(error)
  AIC_Eq24 <- AIC_RSS(n, length(startvals24), error)
  equation <- as.formula(y ~ 1.3 + D^2 /(a + b * D + c * D^2))
  
  res24 <- results.fun(fit = Eq24, equation, Ht_act = Ht_act, D = D, error = error,
                       PredHt = PredHt, coef = c(a24, b24, c24))
  
  
  ##### EQ25: Equation 17 in Huang et al. (1992) and Eq. 18 in Huang et al. (2000): H = 1.3 + a *D^(b*D^-c), where D = DBH
  startvals25 <- list(a = 22.9433, b = -20.9985, c=0.7680)
  Fit_nls25 <- function(x) {
    a <- x[1]
    b <- x[2]
    c <- x[3]
    t1 <- (1.3 + a*D^(b*D^-c))
    t1 <- sum((Ht_act-t1)^2)
    return(t1)
  }
  
  Eq25 <- optim(par=startvals25, fn = Fit_nls25, hessian = FALSE, control = list(parscale = unlist(startvals25)))
  a25 = Eq25$par[1]
  b25 = Eq25$par[2]
  c25 = Eq25$par[3]
  PredHt <- 1.3 + a25 * D^(b25 * D^-c25)
  error <- (Ht_act - PredHt)
  RMSE_Eq25 <- rmse(error)
  AIC_Eq25 <- AIC_RSS(n, length(startvals25), error)
  equation <- as.formula(y ~ 1.3 + a*D^(b*D^-c))
  
  res25 <- results.fun(fit = Eq25, equation, Ht_act = Ht_act, D = D, error = error,
                       PredHt = PredHt, coef = c(a25, b25, c25))
  
  ##### EQ26: Equation 18 in Huang et al. (1992) and Eq. 20 in Huang et al. (2000): H = 1.3 + a*exp(b/(D+c)), where D = DBH
  startvals26 <- list(a = 3.2971, b = -0.4014, c = 5.5088)
  Fit_nls26 <- function(x) {
    a <- x[1]
    b <- x[2]
    c <- x[3]
    t1 <- (1.3 + a * exp(b /(D+c)))
    t1 <- sum((Ht_act-t1)^2)
    return(t1)
  }
  
  Eq26 <- optim(par=startvals26, fn = Fit_nls26, hessian = FALSE, control = list(parscale = unlist(startvals26)))
  a26 = Eq26$par[1]
  b26 = Eq26$par[2]
  c26 = Eq26$par[3]
  PredHt <- 1.3 + a26 * exp(b26 /(D + c26))
  error <- (Ht_act - PredHt)
  RMSE_Eq26 <- rmse(error)
  AIC_Eq26 <- AIC_RSS(n, length(startvals26), error)
  equation <- as.formula(y ~ 1.3 + a * exp(b /(D+c)))
  
  res26 <- results.fun(fit = Eq26, equation, Ht_act = Ht_act, D = D, error = error,
                       PredHt = PredHt, coef = c(a26, b26, c26))
  
  ##### EQ27: Equation 19 in Huang et al.(1992) and Eq.19 in Huang et al. (2000): H = 1.3 + a/(1 + b^-1 * D^-c), where D = DBH
  startvals27 <- list(a = 34.4682, b = 0.0369, c = 1.0589)
  Fit_nls27 <- function(x) {
    a <- x[1]
    b <- x[2]
    c <- x[3]
    t1 <- (1.3 + a/(1+1/(b*D^c)))
    t1 <- sum((Ht_act-t1)^2)
    return(t1)
  }
  
  Eq27 <- optim(par=startvals27, fn = Fit_nls27, hessian = FALSE, control = list(parscale = unlist(startvals27)))
  a27 = Eq27$par[1]
  b27 = Eq27$par[2]
  c27 = Eq27$par[3]
  PredHt <- 1.3+a27 /(1+1/(b27 * D^c27))
  error <- (Ht_act - PredHt)
  RMSE_Eq27 <- rmse(error)
  AIC_Eq27 <- AIC_RSS(n, length(startvals27), error)
  equation <- as.formula(y ~ 1.3 + a/(1+1/(b*D^c)))
  
  res27 <- results.fun(fit = Eq27, equation, Ht_act = Ht_act, D = D, error = error,
                       PredHt = PredHt, coef = c(a27, b27, c27))
  
  ##### EQ28: Equation 20 in Huang et al. (1992) and Eq. 21 in Huang et al. (2000): H = 1.3 + a*(1 - b*exp(-c*D))^d, where D = DBH
  startvals28 <- list(a = 15.2716, b = 0.9574, c = 0.0530, d = 1.1025)
  Fit_nls28 <- function(x) {
    a <- x[1]
    b <- x[2]
    c <- x[3]
    d <- x[4]
    t1 <- (1.3 + a*(1-b*exp(-c*D))^d)
    t1 <- sum((Ht_act-t1)^2)
    return(t1)
  }
  
  Eq28 <- optim(par=startvals28, fn = Fit_nls28, hessian = FALSE, control = list(parscale = unlist(startvals28)))
  a28 = Eq28$par[1]
  b28 = Eq28$par[2]
  c28 = Eq28$par[3]
  d28 = Eq28$par[4]
  PredHt<- 1.3 + a28 *(1- b28 * exp(-c28 * D))^d28
  error <- (Ht_act - PredHt)
  RMSE_Eq28 <- rmse(error)
  AIC_Eq28 <- AIC_RSS(n, length(startvals28), error)
  equation <- as.formula(y ~ 1.3 + a*(1-b*exp(-c*D))^d)
  
  res28 <- results.fun(fit = Eq28, equation, Ht_act = Ht_act, D = D, error = error,
                       PredHt = PredHt, coef = c(a28, b28, c28, d28))
  
  ##### EQ29: Equation 23 in Huang et al. (2000): H = 1.3 + a*D*exp(-b*D), where D = DBH.
  ##### Start values were for white spruce (Table 3 in page 131 of Huang et al. (2000))
  
  startvals29 <- list(a = 1.0264, b = 0.01157)
  Fit_nls29 <- function(x) {
    a <- x[1]
    b <- x[2]
    t1 <- (1.3 + a * D * exp(-b*D))
    t1 <- sum((Ht_act-t1)^2)
    return(t1)
  }
  
  Eq29 <- optim(par=startvals29, fn = Fit_nls29, hessian = FALSE, control = list(parscale = unlist(startvals29)))
  a29 = Eq29$par[1]
  b29 = Eq29$par[2]
  PredHt <- 1.3 + a29 * D * exp(-b29 * D)
  error <- (Ht_act - PredHt)
  RMSE_Eq29 <- rmse(error)
  AIC_Eq29 <- AIC_RSS(n, length(startvals29), error)
  equation <- as.formula(y ~ 1.3 + a * D * exp(-b*D))
  
  res29 <- results.fun(fit = Eq29, equation, Ht_act = Ht_act, D = D, error = error,
                       PredHt = PredHt, coef = c(a29, b29))
  
  ##### EQ30: Equation 24 in Huang et al. (2000): H = 1.3 + a * D^b * exp(-c*D), where D = DBH. Start values were for white spruce (Table 3 in page 131 of Huang et al. (2000))
  startvals30 <- list(a = 0.687824, b = 1.175135, c = 0.017699)
  Fit_nls30 <- function(x) {
    a <- x[1]
    b <- x[2]
    c <- x[3]
    t1 <- (1.3 + a * D^b * exp(-c*D))
    t1 <- sum((Ht_act-t1)^2)
    return(t1)
  }
  
  Eq30 <- optim(par=startvals30, fn = Fit_nls30, hessian = FALSE, control = list(parscale = unlist(startvals30)))
  a30 = Eq30$par[1]
  b30 = Eq30$par[2]
  c30 = Eq30$par[3]
  PredHt <- 1.3 + a30 * D^b30 * exp(-c30*D)
  error <- (Ht_act - PredHt)
  RMSE_Eq30 <- rmse(error)
  AIC_Eq30 <- AIC_RSS(n, length(startvals30), error)
  equation <- as.formula(y ~ 1.3 + a * D^b * exp(-c*D))
  
  res30 <- results.fun(fit = Eq30, equation, Ht_act = Ht_act, D = D, error = error,
                       PredHt = PredHt, coef = c(a30, b30, c30))
  
  ##### EQ31: Pantropical DBH-H model (Chave et al., 2014. GCB 20: 3183): Ln(H) = a - E + b*Ln(D)+ c*(Ln(D))^2 , where D = DBH
  startvals31 <- list(a = 0.893, b = 0.760, c = -0.034)
  Fit_nls31 <- function(x) {
    a <- x[1]
    b <- x[2]
    c <- x[3]
    t1 <- (a - E_Value + b * log(D) + c * (log(I(D)))^2)
    t1 <- sum((Ht_act-t1)^2)
    return(t1)
  }
  
  if(!is.na(E_Value)){
    
    Eq31 <- optim(par=startvals31, fn = Fit_nls31, hessian = FALSE, control = list(parscale = unlist(startvals31)))
    a31 = Eq31$par[1]
    b31 = Eq31$par[2]
    c31 = Eq31$par[3]
    PredHt <- a31 - E_Value + b31 * log(D) + c31 * (log(I(D)))^2
    error <- (Ht_act - PredHt)
    RMSE_Eq31 <- rmse(error)
    AIC_Eq31 <- AIC_RSS(n, length(startvals31), error)
    equation <- as.formula(y ~ a - E_Value + b * log(D) + c * (log(I(D)))^2)
    
    res31 <- results.fun(fit = Eq31, equation, Ht_act = Ht_act, D = D, error = error,
                         PredHt = PredHt, coef = c(a31, b31, c31))
  }else{AIC_Eq31 <- 9999999}
  
  ##### EQ32: The Mitscherlisch model (Ngomanda et al., 2014): H = a - b * exp(-cD), where D = DBH
  startvals32 <- list(a = 44.7, b = 42.3, c = 0.026)
  Fit_nls32 <- function(x) {
    a <- x[1]
    b <- x[2]
    c <- x[3]
    t1 <- (a - b * exp(-c * D))
    t1 <- sum((Ht_act-t1)^2)
    return(t1)
  }
  
  Eq32 <- optim(par=startvals32, fn = Fit_nls32, hessian = FALSE, control = list(parscale = unlist(startvals32)))
  a32 = Eq32$par[1]
  b32 = Eq32$par[2]
  c32 = Eq32$par[3]
  PredHt <- a32 - b32 * exp(-c32 * D)
  error <- (Ht_act - PredHt)
  RMSE_Eq32 <- rmse(error)
  AIC_Eq32 <- AIC_RSS(n, length(startvals32), error)
  equation <- as.formula(y ~ a - b * exp(-c * D))
  
  res32 <- results.fun(fit = Eq32, equation, Ht_act = Ht_act, D = D, error = error,
                       PredHt = PredHt, coef = c(a32, b32, c32))
  
  ##### Model comparison
  
  AIC_best.fit <- paste("Eq", 1, sep="")
  AIC_min_temp = AIC_Eq1
  AIC.df <- matrix(nrow = 32, ncol = 2, dimnames = list(c(), c("Equ", "AIC")))
  
  
  for (k in 1: 32){
    AIC_min <- eval(parse(text=paste("AIC_Eq", k, sep="")))
    AIC.df[k,] <- c(Equ = paste("Eq", k, sep=""), AIC = round(AIC_min, 1))
    AIC_min <- min(AIC_min, AIC_min_temp)
    if (AIC_min < AIC_min_temp) {AIC_best.fit <- paste("Eq", k, sep="")}
    AIC_min_temp = AIC_min
  }
  
  AIC.df <- data.frame(AIC.df)
  AIC.df$AIC <- as.numeric(as.character(AIC.df$AIC))
  
  best.equ <- eval(parse(text=AIC_best.fit))
  
  rnum <- paste("res", gsub("[^[:digit:]]", "", AIC_best.fit), sep="")
  res_fin <- get(rnum)
  
  predht.fun <- function(res, D){
    a <- res$vars["a"]
    b <- res$vars["b"]
    c <- res$vars["c"]
    d <- res$vars["d"]
    equ.fun <- function(D) {
      eval(parse(text = res$equ))
      }
    ph <- equ.fun(D)
  }
  
  predht <- predht.fun(res = res_fin, D)
  full.list <- res_fin
  full.list$predht <- predht
  full.list
}


