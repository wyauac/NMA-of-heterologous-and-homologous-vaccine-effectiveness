library(R2jags)
library(dplyr)

setwd("C:\\Users\\user\\OneDrive - The Chinese University of Hong Kong\\Vaccine Projects\\Vaccine SM\\data_analysis\\bmj_2nd-round_submission\\clean-folder")

file.to.analyse<-11
consistency<-TRUE

R2jags_filenames <- c(
        "VPB_main_docinfect",
        "VPB_main_symp", 
        "VPB_main_severe",
        "VPB_main_hosp", 
        "VPB_main_death", 
        "VPB_sub_docinfect+symp_age",
        "VPB_sub_docinfect+symp_sex", 
        "VPB_sub_docinfect+symp_ethnic", 
        "VPB_sub_docinfect+symp_risk", 
        "VPB_sub_docinfect+symp_variant",
        "PFB_main_docinfect",
        "PFB_main_symp",
        "PFB_main_severe",
        "PFB_main_hosp",
        "PFB_main_death"
)

if ((file.to.analyse==1|file.to.analyse==2|file.to.analyse==3|file.to.analyse==4|file.to.analyse==5) && consistency==TRUE){
        N<-sprintf("./data/export_%s.csv", R2jags_filenames[file.to.analyse])
        datafile<-read.csv(N, header = TRUE, fileEncoding = "UTF-8-BOM")
        nvac<-max(datafile[,ncol(datafile)])
        data<-list(ntALL=max(cbind(datafile[,1:nvac]), na.rm = TRUE), t=cbind(datafile[,1:nvac]),r=cbind(datafile[,(nvac+1):(nvac*2)]),
                       n=cbind(datafile[,(nvac*2+1):(nvac*3)]),na=datafile[,ncol(datafile)])
        para<-c("d", "sdALL", "sdRCT", "sdCC", "sdRC","sdPC","resdev",
                     "lor","totresdev","rk", "rk_bad")
        file<-sprintf("%s.bugs", R2jags_filenames[file.to.analyse])
}

if ((file.to.analyse==11|file.to.analyse==12|file.to.analyse==13|file.to.analyse==14|file.to.analyse==15) && consistency==TRUE){
        N<-sprintf("./data/export_%s_coded.csv", R2jags_filenames[file.to.analyse])
        datafile<-read.csv(N, header = TRUE, fileEncoding = "UTF-8-BOM")
        nvac<-max(datafile[,ncol(datafile)])
        data<-list(ntALL=max(cbind(datafile[,1:nvac]), na.rm = TRUE), t=cbind(datafile[,1:nvac]),r=cbind(datafile[,(nvac+1):(nvac*2)]),
                   n=cbind(datafile[,(nvac*2+1):(nvac*3)]),na=datafile[,ncol(datafile)])
        para<-c("d", "sdALL", "sdRCT", "sdCC", "sdRC","sdPC","resdev",
                "lor","totresdev","rk", "SUCRA")
        file<-sprintf("%s.bugs", R2jags_filenames[file.to.analyse])
}


jagsfit<-jags(data = data, inits = NULL, parameters.to.save = para, 
     model.file= file, n.chains=3, n.iter=100000, n.burnin=floor(100000/5),
     n.thin=max(1, floor((100000 - floor(100000/5)) / 1000)),
     DIC=TRUE, working.directory=NULL, jags.seed = 123,
     refresh = 100000/50, progress.bar = "text", digits=5,
     RNGname = c("Wichmann-Hill", "Marsaglia-Multicarry",
                 "Super-Duper", "Mersenne-Twister"),
     jags.module = c("glm","dic"), quiet = FALSE)

#traceplot(jagsfit)
