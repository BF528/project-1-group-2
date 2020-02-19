data=read.csv("Step5data.csv", header=TRUE)
row.names(data)=data[,1]

data = data[,-1]

#4.2
greaterthan20percent = c()
for(i in c(1:length(data[,1])))
{
  temp = table(data[i,]>log2(15))
  if(dim(temp)==2) {
    if(temp[2]>length(data[1,])*0.2){
      greaterthan20percent=c(greaterthan20percent, i)
      }
  }
  if(dim(temp)==1&data[i,1]>log2(15)){
    greaterthan20percent=c(greaterthan20percent, i)
  }
}
#4.3
## Create function to perform chi-square test.
var.interval = function(data,sigma0 = 0.01,conf.level = 0.95) {
  df = length(data) - 1
  chilower = qchisq((1 - conf.level)/2, df)
  chiupper = qchisq((1 - conf.level)/2, df, lower.tail = FALSE)
  v = var(data)
  testchi = df*v/(sigma0^2)
  alpha = 1-conf.level
  return(c(testchi, chilower, chiupper))
  # print(paste("Standard deviation = ", round(sqrt(v),4)),quote=FALSE)
  # print(paste("Test statistic = ", round(testchi,4)),quote=FALSE)
  # print(paste("Degrees of freedom = ", round(df,0)),quote=FALSE)
  # print(" ",quote=FALSE)
  # print("Two-tailed test critical values, alpha=0.05",quote=FALSE)
  # print(paste("Lower = ", round(qchisq(alpha/2,df),4)),quote=FALSE)
  # print(paste("Upper = ", round(qchisq(1-alpha/2,df),4)),quote=FALSE)
  # print(" ",quote=FALSE)
  # print("95% Confidence Interval for Standard Deviation",quote=FALSE)
  # print(c(round(sqrt(df * v/chiupper),4),
  #         round(sqrt(df * v/chilower),4)),quote=FALSE)
}
signifchisq = c()
for(i in c(1:length(data[,1])))
{
  row=as.numeric(as.vector(data[i,]))
  chisq=var.interval(as.numeric(as.vector(data[i,])))
  if(chisq[1]<chisq[2]|chisq[1]>chisq[3]){
    signifchisq=c(signifchisq, i)
  }
}

#4.4
#install.packages("goeveg")
library(goeveg)
signifcv=c()
for(i in c(1:length(data[,1])))
{
  row=as.numeric(as.vector(data[i,]))
  cov=cv(as.numeric(as.vector(data[i,])))
  if(cov>0.186){
    signifcv=c(signifcv, i)
  }
}
#find the intersection of all 3 vectors
finallist=Reduce(intersect, list(greaterthan20percent,signifchisq,signifcv))
#get the subset of data from the data frame
sigdata=data[finallist,]

#5.1
d = dist(t(as.matrix(sigdata)))
hc = hclust(d)
plot(hc)
#5.2
twogrp=cutree(hc, k = 2, h = NULL)


#5.3
metadata=read.csv("proj_metadata.csv")
samplesubtype=rep(0, length(names(sigdata)))
for(i in c(1:length(names(sigdata))))
{
  for (j in c(1:length(metadata[,18]))) {
    if(substr(names(sigdata)[i], 1, 9)==metadata[j,18]){
      samplesubtype[i]= metadata[j, 16]
    }
}
}
