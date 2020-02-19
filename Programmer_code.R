#1
BiocManager::install(c("affy","affyPLM","sva","AnnotationDbi","hgu133plus2.db"))
#2
t1 <- proc.time()

library(affy)
library(affyPLM)
library(sva)
library(AnnotationDbi)
library(hgu133plus2.db)
setwd("/projectnb/bf528/users/group2/project1/samples")
#3
dat <- ReadAffy()
norm.dat <- rma(dat)
#4
RLE.and.NUSE <- fitPLM(dat,normalize = TRUE,background = TRUE)
medianRLE.for.samples <- RLE(RLE.and.NUSE,type="stats")
medianNUSE.for.samples <- NUSE(RLE.and.NUSE,type="stats")
hist(medianRLE.for.samples[1,],col="blue",nclass=8,main="Histogram of median RLE",xlab="median RLE")
hist(medianNUSE.for.samples[1,],col="blue",nclass=8,main="Histogram of median NUSE",xlab="median NUSE")
#5
anno_data <- read.csv("/project/bf528/project_1/doc/proj_metadata.csv")
new.dat <- exprs(norm.dat)
new.dat <- ComBat(new.dat,batch = anno_data$normalizationcombatbatch,mod = anno_data$normalizationcombatmod)
#write.csv(new.dat,file = "/projectnb/bf528/users/group2/project1/programmer_deliverables/Step5data.csv")
#test.data <- read.csv("/projectnb/bf528/users/group2/project1/programmer_deliverables/Step5data.csv")
#6
library(ggplot2)
new.dat <- t(scale(t(new.dat)))
PCA.data <- prcomp(new.dat,scale. = FALSE,center = FALSE)
summary.data <- summary(PCA.data)
PC.for.plot <- PCA.data$rotation[,1:2]
plot(PC.for.plot,col="red",xlab=paste0("PC1:",summary.data$importance[2,1]),ylab=paste0("PC2:",summary.data$importance[2,2]),
     main="PCA Plot",cex=2.5)
PC.df <- as.data.frame(PC.for.plot)
ggplot(PC.df,aes(x = PC1, y = PC2))+geom_point(size=5)+labs(title = "PCA plot")+
  xlab(paste0("PC1:",summary.data$importance[2,1]))+
  ylab(paste0("PC2:",summary.data$importance[2,2]))

t2 <- proc.time()
t <- t2 - t1
print(t[3][[1]])
#visualization for different batch
batch <- anno_data$normalizationcombatbatch
PC.for.plot <- as.data.frame(PC.for.plot)
ggplot(PC.for.plot,aes(x = PC1, y = PC2))+geom_point(aes(col=batch),size=5)+
  labs(title="PCA plot")+xlab(paste0("PC1:",summary.data$importance[2,1]))+
  ylab(paste0("PC2:",summary.data$importance[2,2]))

packageVersion("affy")
packageVersion("affyPLM")
packageVersion("sva")
packageVersion("ggplot2")
  