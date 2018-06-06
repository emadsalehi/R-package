#' @title  random variable generator
#'
#' @description  generate your arbitrary random variable
#'
#' @param
#'
#' @return
#'
#' @examples
#'
#' @export
library(nanotime)
library(bit64)
library(ggplot2)
rgenerator<-function()
{
  MMM = 0xffffda61
  sss = 0xffffffff
  #x=as.integer64(as.numeric((Sys.time()))%%sss)
  #x1 = bitwAnd(x/0xffff , 0xffff)
  #x2 = bitwAnd (x%%0xffff, 0xffff)
  #X = x1 * 0xffff + x2
  #x = (MMM * X) + (x/0x100000000)
  return(runif(1, 0 , 0xffff))
}
dugen<-function(a,b)
{
  X = rgenerator()
  X = X %% 0xffff
  x = X*(b-a)/(0xffff)+ a
  return (x)
}

cugen<-function()
{
  return (dugen(0,1))
}

brgen<-function(p)
{
  if (cugen()<p)
  {
    return (1)
  }
  return (0)
}

bigen<-function(n,p)
{
  ans=0
  for (i in 1:n)
  {
    if(brgen(p)==1)
    {
      ans=ans+1
    }
  }
  return (ans)
}

gegen<-function(p)
{
  i=1
  j=1
  while (brgen(p)==0)
  {
    i=i+1
    j=j+1
  }
  while (brgen(p)==0)
  {
    j=j+1
  }
  return (j-i)
}

expgen<-function(lambda)
{
  x=cugen()
  return (-1/lambda*log(x))
}

gagen<-function(k,lambda)
{
  X=0
  for (i in 1:k)
  {
    X=X+expgen(lambda)
  }
  return (X)
}

pogen<-function(lambda,t)
{
  X=0
  while (t>0)
  {
    t=t-expgen(lambda)
    X=X+1
  }
  return (X)
}

nogen<-function(u,s)
{
  m = 100
  x=pogen(m,1)
  x = x - m
  x = x / sqrt(m)
  return (sqrt(s)*x + u)
}

draw_rgenerator<-function()
{
  data<-c()
  for (i in 1:10000)
  {
    data[i]=rgenerator()
  }
  plot(density(data), type = "n")
  lines(density(data))
}

draw_dugen<-function(a,b)
{
  data<-c()
  for (i in 1:10000)
  {
    data[i]=dugen(a,b)
  }
  bin = (b-a)/100
  qplot(data ,geom="histogram",binwidth = bin ,main = "Histogram for dugen", ylab="number of instances",xlab="random var",fill=I("blue"), col=I("green"), alpha=I(.2),xlim=c(a,b))
}

draw_cugen<-function()
{
  data<-c()
  for (i in 1:10000)
  {
    data[i]=cugen()
  }
  bin = 0.02
  qplot(data ,geom="histogram",binwidth = bin ,main = "Histogram for cugen", ylab="number of instances",xlab="random var",fill=I("blue"), col=I("green"), alpha=I(.2),xlim=c(0,1))
}

draw_brgen<-function(p)
{
  data<-c()
  for (i in 1:10000)
  {
    data[i]=brgen(p)
  }
  bin = 1
  qplot(data ,geom="histogram",binwidth = bin ,main = "Histogram for brgen", ylab="number of instances",xlab="bernouli RV.",fill=I("blue"), col=I("green"), alpha=I(.2),xlim=c(-0.5,1.5))
}

draw_bigen<-function(n,p)
{
  data<-c()
  for (i in 1:10000)
  {
    data[i]=bigen(n,p)
  }
  bin = 1
  qplot(data ,geom="histogram",binwidth = bin ,main = "Histogram for bigen", ylab="number of instances",xlab="binomial RV.",fill=I("blue"), col=I("green"), alpha=I(.2),xlim=c(-0.5,n+0.5))
}

draw_gegen<-function(p)
{
  data<-c()
  for (i in 1:10000)
  {
    data[i]=gegen(p)
  }
  bin = 1
  qplot(data ,geom="histogram",binwidth = bin ,main = "Histogram for gegen", ylab="number of instances",xlab="geometric RV.",fill=I("blue"), col=I("green"), alpha=I(.2),xlim=c(-0.5,20.5))
}

draw_expgen<-function(lambda)
{
  data<-c()
  for (i in 1:10000)
  {
    data[i]=expgen(lambda)
  }
  plot(density(data), main = "pdf for expgen", xlab = "exponential RV." , ylab = "pdf", type = "n")
  lines(density(data))
}

draw_gagen<- function(k, lambda)
{
  data<-c()
  for (i in 1:10000)
  {
    data[i]=gagen(k,lambda)
  }
  plot(density(data),main = "pdf for gapgen", xlab = "gamma RV." , ylab = "pdf", type = "n")
  lines(density(data))
}

draw_pogen<-function(lambda)
{
  data<-c()
  for (i in 1:10000)
  {
    data[i]=pogen(lambda,1)
  }
  bin = 1
  qplot(data ,geom="histogram",binwidth = bin ,main = "Histogram for pogen", ylab="number of instances",xlab="poisson RV.",fill=I("blue"), col=I("green"), alpha=I(.2),xlim=c(-0.5,20.5))
}

draw_nogen<-function(u,s)
{
  data<-c()
  for (i in 1:10000)
  {
    data[i]=nogen(u,s)
  }
  plot(density(data),main = "pdf for nopgen", xlab = "normal RV." , ylab = "pdf", type = "n")
  lines(density(data))
}
