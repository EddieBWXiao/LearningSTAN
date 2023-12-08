#test1_demo; run RW1lr1beta2AFC
CurrentSourceWD<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(CurrentSourceWD)
source("test1_RW1lr1beta2AFC.R")
task<-data.frame(rewardRef=c(1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,0,1,1),
                 rewardAlt=c(0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0))
parameters<-list(alpha=1,beta=10)
out<-RW1lr1beta2AFC(parameters,task)