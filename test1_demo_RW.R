#test1_demo; run RW1lr1beta2AFC
CurrentSourceWD<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(CurrentSourceWD)
source("test1_RW1lr1beta2AFC.R")
source("RW1lr1beta_2arm.R")
task1<-data.frame(rewardRef=c(1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,0,1,1),
                 rewardAlt=c(0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0))
parameters<-list(alpha=1,beta=10)
set.seed(1202)
out1<-RW1lr1beta2AFC(parameters,task1)
set.seed(1202)
task2<-list(outcome=data.frame(A=c(1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,0,1,1),
                 B=c(0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0)))
parameters<-list(alpha=1,beta=10)
out2<-RW1lr1beta_2arm(parameters,task2)