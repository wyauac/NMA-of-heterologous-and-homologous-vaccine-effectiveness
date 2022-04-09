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

if ((file.to.analyse==1|file.to.analyse==2|file.to.analyse==3|file.to.analyse==4|file.to.analyse==5) && consistency==FALSE){
        N<-sprintf("./data/export_%s.csv", R2jags_filenames[file.to.analyse])
        datafile<-read.csv(N, header = TRUE, fileEncoding = "UTF-8-BOM")
        nvac<-max(datafile[,ncol(datafile)])
        data<-list(ntALL=max(cbind(datafile[,1:nvac]), na.rm = TRUE), t=cbind(datafile[,1:nvac]),r=cbind(datafile[,(nvac+1):(nvac*2)]),
                       n=cbind(datafile[,(nvac*2+1):(nvac*3)]),na=datafile[,ncol(datafile)])
        para<-c("d", "sdALL", "sdRCT", "sdCC", "sdRC","sdPC","resdev",
                     "lor","totresdev","rk")
        file<-sprintf("%s_inconsistency.bugs", R2jags_filenames[file.to.analyse])
}

if ((file.to.analyse==11|file.to.analyse==12|file.to.analyse==13|file.to.analyse==14|file.to.analyse==15) && consistency==FALSE){
        N<-sprintf("./data/export_%s_coded.csv", R2jags_filenames[file.to.analyse])
        datafile<-read.csv(N, header = TRUE, fileEncoding = "UTF-8-BOM")
        nvac<-max(datafile[,ncol(datafile)])
        data<-list(ntALL=max(cbind(datafile[,1:nvac]), na.rm = TRUE), t=cbind(datafile[,1:nvac]),r=cbind(datafile[,(nvac+1):(nvac*2)]),
                   n=cbind(datafile[,(nvac*2+1):(nvac*3)]),na=datafile[,ncol(datafile)])
        para<-c("d", "sdALL", "resdev",
                "lor","totresdev","rk")
        file<-sprintf("%s_inconsistency.bugs", R2jags_filenames[file.to.analyse])
}

if (file.to.analyse==6){
        datafile<-read.csv("./data/export_VPB_sub_docinfect+symp.csv", 
                           header = TRUE, fileEncoding = "UTF-8-BOM")
        nvac<-max(datafile[,ncol(datafile)])
        data<-list(nzALL=1, z=c(1), ntALL=max(cbind(datafile[,1:nvac]), na.rm = TRUE), 
          t=cbind(datafile[,1:nvac]),r=cbind(datafile[,(nvac+1):(nvac*2)]),
          n=cbind(datafile[,(nvac*2+1):(nvac*3)]),na=datafile[,ncol(datafile)],
          #young=as.numeric(datafile["young"][1:nrow(datafile),]), 
          adult=as.numeric(datafile["adult"][1:nrow(datafile),]),
          elder=as.numeric(datafile["elder"][1:nrow(datafile),]))
        para<-c("or", "oradult", "orelder")
        file<-"VPB_sub_docinfect+symp_simple_reg.bugs"
}

if (file.to.analyse==7){
   datafile<-read.csv("./data/export_VPB_sub_docinfect+symp_sex_coded.csv", 
                      header = TRUE, fileEncoding = "UTF-8-BOM")
   nvac<-max(datafile[,ncol(datafile)])
   data<-list(nzALL=1, z=c(1), ntALL=max(cbind(datafile[,1:nvac]), na.rm = TRUE), 
     t=cbind(datafile[,1:nvac]),r=cbind(datafile[,(nvac+1):(nvac*2)]),
     n=cbind(datafile[,(nvac*2+1):(nvac*3)]),na=datafile[,ncol(datafile)],
     fem=as.numeric(datafile["female.1"][1:nrow(datafile),]))
   para<-c("lor", "lorfem")
   file<-"VPB_sub_docinfect+symp_sex_coded.bugs"
}

if (file.to.analyse==8){
        datafile<-read.csv("./data/export_VPB_sub_docinfect+symp_ethnic_coded.csv", 
                           header = TRUE, fileEncoding = "UTF-8-BOM")
        nvac<-max(datafile[,ncol(datafile)])
        data<-list(nzALL=1, z=c(1), ntALL=max(cbind(datafile[,1:nvac]), na.rm = TRUE), 
                  t=cbind(datafile[,1:nvac]),r=cbind(datafile[,(nvac+1):(nvac*2)]),
                  n=cbind(datafile[,(nvac*2+1):(nvac*3)]),na=datafile[,ncol(datafile)],
                  his=as.numeric(datafile["hispanic"][1:nrow(datafile),]), 
                  bla=as.numeric(datafile["black"][1:nrow(datafile),]),
                  oth=as.numeric(datafile["other"][1:nrow(datafile),]),
                  whit=as.numeric(datafile["white"][1:nrow(datafile),]))
                  
        para<-c("or", "lor", "lorhis", "orhis", "mhis", "moth", "lorbeta", "orbeta")
        file<-"VPB_sub_docinfect+symp_ethnic_noconstant.bugs"
}

if (file.to.analyse==9){
        datafile<-read.csv("./data/export_VPB_sub_docinfect+symp.csv", 
                           header = TRUE, fileEncoding = "UTF-8-BOM")
        nvac<-max(datafile[,ncol(datafile)])
        data<-list(nzALL=1, z=c(1), ntALL=max(cbind(datafile[,1:nvac]), na.rm = TRUE), 
           t=cbind(datafile[,1:nvac]),r=cbind(datafile[,(nvac+1):(nvac*2)]),
           n=cbind(datafile[,(nvac*2+1):(nvac*3)]),na=datafile[,ncol(datafile)],
           chro=as.numeric(datafile["chronic"][1:nrow(datafile),]))
        para<-c("lor", "lorchro", "lorimmu")
        file<-"VPB_sub_docinfect+symp_risk_coded.bugs"
}

if (file.to.analyse==10){
        datafile<-read.csv("./data/export_VPB_sub_docinfect+symp_variant_coded.csv", 
                           header = TRUE, fileEncoding = "UTF-8-BOM")
        nvac<-max(datafile[,ncol(datafile)])
        data<-list(nzALL=1, z=c(1), ntALL=max(cbind(datafile[,1:nvac]), na.rm = TRUE), 
              t=cbind(datafile[,1:nvac]),r=cbind(datafile[,(nvac+1):(nvac*2)]),
              n=cbind(datafile[,(nvac*2+1):(nvac*3)]),na=datafile[,ncol(datafile)],
              alpha=as.numeric(datafile["alpha"][1:nrow(datafile),]),
              beta=as.numeric(datafile["beta"][1:nrow(datafile),]),
              gamma=as.numeric(datafile["gamma"][1:nrow(datafile),]), 
              delta=as.numeric(datafile["delta"][1:nrow(datafile),]),
              omicron=as.numeric(datafile["omicron"][1:nrow(datafile),]))
        para<-c("lor", "loralpha", "lorbeta", "lorgamma","lordelta", "loromicron")
        file<-"VPB_sub_docinfect+symp_variant_coded.bugs"
}

jagsfit_PFB_doc_sucra<-jags(data = data, inits = NULL, parameters.to.save = para, 
     model.file= file, n.chains=3, n.iter=100000, n.burnin=floor(100000/5),
     n.thin=max(1, floor((100000 - floor(100000/5)) / 1000)),
     DIC=TRUE, working.directory=NULL, jags.seed = 123,
     refresh = 100000/50, progress.bar = "text", digits=5,
     RNGname = c("Wichmann-Hill", "Marsaglia-Multicarry",
                 "Super-Duper", "Mersenne-Twister"),
     jags.module = c("glm","dic"), quiet = FALSE)

#traceplot(jagsfit)
