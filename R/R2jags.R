library(R2jags)
library(dplyr)

setwd("\PATH\TO\FILE")

read.datafile<-("export_VPB_main_docinfect.csv")
datafile<-read.csv(read.datafile, header = TRUE, fileEncoding = "UTF-8-BOM")
nvac<-max(datafile[,ncol(datafile)])
data<-list(ntALL=max(cbind(datafile[,1:nvac]), na.rm = TRUE), t=cbind(datafile[,1:nvac]),r=cbind(datafile[,(nvac+1):(nvac*2)]),
               n=cbind(datafile[,(nvac*2+1):(nvac*3)]),na=datafile[,ncol(datafile)])
para<-c("d", "sdALL", "sdRCT", "sdCC", "sdRC", "sdPC", "lor", "rk")
file<-("VPB_main_docinfect.bugs")


jagsfit<-jags(data = data, inits = NULL, parameters.to.save = para, 
     model.file= file, n.chains=3, n.iter=100000, n.burnin=floor(100000/5),
     n.thin=max(1, floor((100000 - floor(100000/5)) / 1000)),
     DIC=TRUE, working.directory=NULL, jags.seed = 123,
     refresh = 100000/50, progress.bar = "text", digits=5,
     RNGname = c("Wichmann-Hill", "Marsaglia-Multicarry",
                 "Super-Duper", "Mersenne-Twister"),
     jags.module = c("glm","dic"), quiet = FALSE)

traceplot(jagsfit)
