#
# Based on GPL-3 code that used to be at 
# https://web.archive.org/web/20150913160650/http://www.stat.cmu.edu:3838/hseltman/LogReg#server_R_code
#

library(shiny)
require(arm)
# Define server logic required to draw a histogram

# Inverse logit

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

shinyServer(function(input,output){
  
  # Return the requested dataset
  dataInput <- reactive({
    doit = input$doit    
    sim(input$n, input$beta0, input$beta1, input$beta2,
        input$beta1sq, input$beta12)
  })
  
  regInput <- reactive({
    glm(y~x1+x2, dataInput(), family="binomial")
  })
  
  
  HLInput <- reactive({
    HL(dataInput()$y, predict(regInput()))
  })
  
  
  # Generate a summary of the dataset
  output$summary <- renderPrint({
    coef(summary(regInput()))
  })
  
  output$HL <- renderPrint({
    HL = HLInput()
    cat("Hosmer Lemeshow stat =", HL$stat, "  p =", HL$p.value)
  })
  
  output$HLtable <- renderTable({
    as.data.frame(HLInput()$pattern)
  })
  
  output$bp <- renderPlot({
    nclass = input$nclass
    dtf = dataInput()
    rslt = regInput()
    res = residuals(rslt, type=input$resType)
    if (input$bpX=="fitted") {
      x = fitted(rslt)
    } else if (input$bpX=="x1") {
      x = dtf$x1
    } else {
      x = dtf$x2
    }
    binnedplot(x, res, nclass=nclass)
  })
  
  
  output$models <- renderPlot({
    rslt = regInput()
    LEN=100
    x1seq = seq(-3, 3, len=LEN)
    x2levels = round(qnorm(c(1/4,2/3)),2)
    # Predictions of fitted model
    dtfFL = data.frame(x1=x1seq, x2=x2levels[1])
    dtfFH = data.frame(x1=x1seq, x2=x2levels[2])
    fitFL = predict(rslt, dtfFL, se.fit=TRUE)
    fitFH = predict(rslt, dtfFH, se.fit=TRUE)
    lwrFL = pmax(0, expit(fitFL$fit - 1.96 * fitFL$se.fit))
    uprFL = pmin(1, expit(fitFL$fit + 1.96 * fitFL$se.fit))
    lwrFH = pmax(0, expit(fitFH$fit - 1.96 * fitFH$se.fit))
    uprFH = pmin(1, expit(fitFH$fit + 1.96 * fitFH$se.fit))
    plot(x1seq, expit(fitFL$fit), xlab="x1", ylab="Pr(Success)",
         ylim=c(0,1), type="l", col=1, main="95% CI (Bl+Rd) and Truth (Greeen)")
    lines(x1seq, lwrFL, col=1, lty=3)
    lines(x1seq, uprFL, col=1, lty=3)
    lines(x1seq, expit(fitFH$fit), col=2)
    lines(x1seq, lwrFH, col=2, lty=3)
    lines(x1seq, uprFH, col=2, lty=3)
    matTL = cbind(1, x1seq, x2levels[1], x1seq^2, x1seq*x2levels[1])
    matTH = cbind(1, x1seq, x2levels[2], x1seq^2, x1seq*x2levels[2])
    loTL = matTL %*% rbind(input$beta0,input$beta1,input$beta2,input$beta1sq,input$beta12)
    loTH = matTH %*% rbind(input$beta0,input$beta1,input$beta2,input$beta1sq,input$beta12)
    lines(x1seq, expit(loTL), col=3)
    lines(x1seq, expit(loTH), col=3)
    legend("topleft", paste("x2=",x2levels), col=1:2, lty=1)
  })
}) # end anonymous function and shinyServer()
