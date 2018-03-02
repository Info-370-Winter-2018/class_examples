#
# Based on GPL-3 code that used to be at 
# https://web.archive.org/web/20150913160650/http://www.stat.cmu.edu:3838/hseltman/LogReg#server_R_code
#

library(shiny)
require(arm) # for binnedplot()

expit = function(x) 1/(1+exp(-x))

# Hosmer-Lemeshow GOF test
HL = function(y, LOy, k=min(10, length(y)/4)) {
  breaks = c(min(LOy)-0.1, quantile(LOy, (1:(k-1))/k), max(LOy)+0.1)
  breaks = breaks[!duplicated(breaks)]
  k = length(breaks)
  grp = cut(LOy, breaks)
  Obs = table(grp, y)
  ns = apply(Obs,1,sum)
  zero = ns==0
  if (any(zero)) {
    k = k - sum(zero)
    Obs = Obs[!zero,]
    ns = ns[!zero]
  }
  pe = aggregate(LOy, list(grp), function(y)sum(expit(y)))$x
  pne = ns - pe
  Exp = cbind(pne, pe)
  rslt = cbind(Obs, Exp)
  colnames(rslt) = c("Obs0", "Obs1", "Exp0", "Exp1")
  X2 = apply((Obs - Exp)^2/Exp, 1, sum)
  p.value = 1 - pchisq(sum(X2), k-2)
  rslt = cbind(rslt, X2 = X2)
  return(list(pattern = rslt, stat=sum(X2), p.value=p.value))
}

sim = function(n, b0=0, b1=0.5, b2=0.5, b1sq=0, b12=0) {
  x1 = rnorm(n)
  x2 = rnorm(n)
  x1sq = x1*x1
  x12 = x1*x2
  loy = cbind(1,x1,x2,x1sq,x12) %*% c(b0,b1,b2,b1sq,b12)
  dtf =  data.frame(x1,x2,y=rbinom(n,1,expit(loy)))
  return(dtf)
}

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("Logistic Regression Residual Analysis"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("n","N:", min=50, max=500, value=250, step=1,
                  animate=FALSE),
      sliderInput("beta0","Beta0:", min=-3, max=3, value=0, step=0.1,
                  animate=FALSE),
      sliderInput("beta1","Beta1:", min=-3, max=3, value=1.5, step=0.1,
                  animate=FALSE),
      sliderInput("beta2","Beta2:", min=-3, max=3, value=1.5, step=0.1,
                  animate=FALSE),
      sliderInput("beta1sq","Beta1^2:", min=-2, max=2, value=0,
                  step=0.1, animate=FALSE),
      sliderInput("beta12","Beta12:", min=-2, max=2, value=0,
                  step=0.1, animate=FALSE),
      sliderInput("nclass","Binned plot 'nclass':", min=5, max=30, value=10,
                  step=1, animate=TRUE),
      actionButton("doit", "Repeat"),
      radioButtons("bpX","Binned Plot X:", c("fitted", "x1", "x2")),
      radioButtons("resType","Residual Type:", c("deviance", "pearson", "working", 
                                                 "response", "partial"))
    ), # end sidebarLayout()
    
    mainPanel(
      plotOutput("models"),
      plotOutput("bp"),
      verbatimTextOutput("HL"),
      tableOutput("HLtable"),
      verbatimTextOutput("summary")
    ) # end mainPanel()
  ) # end sidebarLayout()
) # end fluidPage()
)


