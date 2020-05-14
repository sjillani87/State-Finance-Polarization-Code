require(multiwayvcov)
require(lmtest)
require(foreign)

state_finance_polarization<-read.dta("state_finance_polarization.dta")


### ANALYSIS ###


#########################################################################################

#DEFICITS and POLARIZATION (HOUSE)

#Run OLS regressions, cluster standard errors by state, and produce regression tables.
#The result illustrates how the impact of general deficit per capita on polarization in 
#state legislatures is conditional on whether or not the state has a no-deficit-carryover
#provision (bbr9)

model1<-lm(h_diffs ~ general_deficit_capita_lag*bbr9 +unemployment_rate + lnincome + divided_gov + as.factor(state) + as.factor(year), data = state_finance_polarization)
robcov1 <- cluster.vcov(model1, state_finance_polarization$state)
coeftest(model1, robcov1)

#Choosing on a confidence interval

ci<-0.90
alpha <- 1-ci
z <- qnorm(1-alpha/2)

#Recovering regression coefficients

beta.hat1 <- coef(model1)

#Calculating marginal effects and creating plots to illustrate the interaction effect

z1 <- seq(min(model.frame(model1)[,"bbr9"],na.rm=T),max(model.frame(model1)[,"bbr9"],na.rm=T),length.out=2)
dy.dx1 <- beta.hat1["general_deficit_capita_lag"] + beta.hat1["general_deficit_capita_lag:bbr9"]*z1
se.dy.dx1 <- sqrt(robcov1["general_deficit_capita_lag","general_deficit_capita_lag"] + z1^2*robcov1[nrow(robcov1),ncol(robcov1)] + 2*z1*robcov1["general_deficit_capita_lag",ncol(robcov1)])

#Creating upper and lower bounds using standard errors

upr1 <- dy.dx1 + z*se.dy.dx1
lwr1 <- dy.dx1 - z*se.dy.dx1

#Creating a temporary dataset and plotting the marginal effects

newdata1<-data.frame(z1, dy.dx1, upr1, lwr1)
ggplot(newdata1, aes(x = z1, y = dy.dx1), colour=supp) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymax = upr1, ymin = lwr1, width=.05))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  scale_x_continuous(breaks=c(0, 1))+
  xlab("No Deficit Carryovers")+ylab("Effects on Linear Prediction")+
  ggtitle("House") +
  theme(plot.title = element_text(hjust = 0.5))

##############################################################################

#DEBT and POLARIZATION (HOUSE)


#Run OLS regressions, cluster standard errors by state, and produce regression tables.
#The result illustrates how the impact of debt per capita on polarization in 
#state legislatures is conditional on the stringency of the state's balanced budget system
#(Hou-Smith Score)

model2<-lm(h_diffs ~ ln_debt_capita*hou_smith +unemployment_rate + lnincome + divided_gov + as.factor(state) + as.factor(year) , data = state_finance_polarization)
robcov2 <- cluster.vcov(model2, state_finance_polarization$state)
coeftest(model2, robcov2)

#Creating a marginal effects plot function for a non-binary interaction effect
#NOTE: Unlike bbr9, which is a binary variable, Hou-Smith scores range from 0 to 8

meplot <- function(model,var1,var2,int,vcov,ci=.95,
                   xlab="BBR Score",ylab=paste("Marginal Effect of Debt Per Capita (Logged)"),
                   main="House",
                   me_lty=1,me_lwd=1,me_col="black",
                   ci_lty=1,ci_lwd=.5,ci_col="black",
                   yint_lty=2,yint_lwd=1,yint_col="black"){
  require(ggplot2)
  alpha <- 1-ci
  z <- qnorm(1-alpha/2)
  beta.hat <- coef(model)
  cov <- vcov
  z0 <- seq(min(model.frame(model)[,var2],na.rm=T),max(model.frame(model)[,var2],na.rm=T),length.out=1000)
  dy.dx <- beta.hat[var1] + beta.hat[int]*z0
  se.dy.dx <- sqrt(cov[var1,var1] + z0^2*cov[nrow(cov),ncol(cov)] + 2*z0*cov[var1,ncol(cov)])
  upr <- dy.dx + z*se.dy.dx
  lwr <- dy.dx - z*se.dy.dx
  ggplot(data=NULL,aes(x=z0, y=dy.dx)) +
    labs(x=xlab,y=ylab,title=main) +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_line(aes(z0, dy.dx),size = me_lwd, 
              linetype = me_lty, 
              color = me_col) +
    geom_line(aes(z0, lwr), size = ci_lwd, 
              linetype = ci_lty, 
              color = ci_col) +
    geom_line(aes(z0, upr), size = ci_lwd, 
              linetype = ci_lty, 
              color = ci_col) +
    geom_hline(yintercept=0,linetype=yint_lty,
               size=yint_lwd,
               color='grey')
}

#Plotting the marginal effects

meplot(model=model3,var1="ln_debt_capita",var2="hou_smith",int="ln_debt_capita:hou_smith",vcov=robcov3)


