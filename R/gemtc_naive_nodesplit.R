library(gemtc)
library(dplyr)

alldata<-read.csv("https://raw.githubusercontent.com/wyauac/NMA-of-heterologous-and-homologous-vaccine-effectiveness/main/data/main_anytreat.csv")
#All studies
VPB_main_docinfect <- alldata %>% filter (outcome == "doc-infect")
VPB_main_symp <- alldata %>% filter (outcome == "symp")
VPB_main_severe <- alldata %>% filter (outcome == "severe")
VPB_main_hosp <- alldata %>% filter (outcome == "hosp")
VPB_main_death <- alldata %>% filter (outcome == "death")

network.docinfect<-mtc.network(VPB_main_docinfect)
network.symp<-mtc.network(VPB_main_symp)
network.severe<-mtc.network(VPB_main_severe)
network.hosp<-mtc.network(VPB_main_hosp)
network.death<-mtc.network(VPB_main_death)

nodesplit.docinfect<-mtc.nodesplit(network.docinfect,
                                   linearModel = "random", 
                                   likelihood = "binom",
                                   link = "logit")
nodesplit.symp<-mtc.nodesplit(network.symp,
                              linearModel = "random", 
                              likelihood = "binom",
                              link = "logit")
nodesplit.severe<-mtc.nodesplit(network.severe,
                                linearModel = "random", 
                                likelihood = "binom",
                                link = "logit")
nodesplit.hosp<-mtc.nodesplit(network.hosp,
                              linearModel = "random", 
                              likelihood = "binom",
                              link = "logit")
nodesplit.death<-mtc.nodesplit(network.death,
                               linearModel = "random", 
                               likelihood = "binom",
                               link = "logit")

#RCT only
VPB_main_docinfect_RCT<-VPB_main_docinfect %>% filter(study.type=="RCT")
VPB_main_symp_RCT<-VPB_main_symp %>% filter(study.type=="RCT")
VPB_main_severe_RCT<-VPB_main_severe %>% filter(study.type=="RCT")
VPB_main_hosp_RCT<-VPB_main_hosp %>% filter(study.type=="RCT")

network.docinfect.RCT<-mtc.network(VPB_main_docinfect_RCT)
network.symp.RCT<-mtc.network(VPB_main_symp_RCT)
network.severe.RCT<-mtc.network(VPB_main_severe_RCT)
network.hosp.RCT<-mtc.network(VPB_main_hosp_RCT)

nodesplit.docinfect.RCT<-mtc.nodesplit(network.docinfect.RCT,
                                       linearModel = "random", 
                                       likelihood = "binom",
                                       link = "logit")
nodesplit.symp.RCT<-mtc.nodesplit(network.symp.RCT,
                                       linearModel = "random", 
                                       likelihood = "binom",
                                       link = "logit")
nodesplit.severe.RCT<-mtc.nodesplit(network.severe.RCT,
                                       linearModel = "random", 
                                       likelihood = "binom",
                                       link = "logit")
nodesplit.hosp.RCT<-mtc.nodesplit(network.hosp.RCT,
                                       linearModel = "random", 
                                       likelihood = "binom",
                                       link = "logit")

#OBS only

VPB_main_docinfect_OBS<-VPB_main_docinfect %>% filter(study.type!="RCT")
VPB_main_symp_OBS<-VPB_main_symp %>% filter(study.type!="RCT")
VPB_main_severe_OBS<-VPB_main_severe %>% filter(study.type!="RCT")
VPB_main_hosp_OBS<-VPB_main_hosp %>% filter(study.type!="RCT")
VPB_main_death_OBS<-VPB_main_death %>% filter(study.type!="RCT")

network.docinfect.OBS<-mtc.network(VPB_main_docinfect.OBS)
network.symp.OBS<-mtc.network(VPB_main_symp.OBS)
network.severe.OBS<-mtc.network(VPB_main_severe.OBS)
network.hosp.OBS<-mtc.network(VPB_main_hosp.OBS)
network.death.OBS<-mtc.network(VPB_main_death.OBS)

nodesplit.docinfect.OBS<-mtc.nodesplit(network.docinfect.OBS,
                                       linearModel = "random", 
                                       likelihood = "binom",
                                       link = "logit")
nodesplit.symp.OBS<-mtc.nodesplit(network.symp.OBS,
                                       linearModel = "random", 
                                       likelihood = "binom",
                                       link = "logit")
nodesplit.severe.OBS<-mtc.nodesplit(network.severe.OBS,
                                       linearModel = "random", 
                                       likelihood = "binom",
                                       link = "logit")
nodesplit.hosp.OBS<-mtc.nodesplit(network.hosp.OBS,
                                       linearModel = "random", 
                                       likelihood = "binom",
                                       link = "logit")
nodesplit.death.OBS<-mtc.nodesplit(network.death.OBS,
                                       linearModel = "random", 
                                       likelihood = "binom",
                                       link = "logit")




