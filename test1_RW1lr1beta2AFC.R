RW1lr1beta2AFC<-function(parameters,task){
  alpha<-parameters$alpha
  beta<-parameters$beta
  #list2env(parameters,environment()) #okay this is nuclear... is it?
  
  ntrials<-nrow(task)
  choices<-rep(NA,ntrials)
  
  Qref<-rep(NA,ntrials)
  Qalt<-rep(NA,ntrials)
  Qref[1]<-0.5
  Qalt[1]<-0.5
  pchoice<-rep(NA,ntrials)
  
  for(t in 1:ntrials){
    Qdiff<-Qref[t]-Qalt[t]
    pchoice[t]<-1/(1+exp(-beta*Qdiff))
    
    choices[t]<-rbinom(n=1,size=1,prob=pchoice[t])
    
    if(t<ntrials){
      if(choices[t]==1){
        Qref[t+1]<-Qref[t]+alpha*(task$rewardRef[t]-Qref[t])
        Qalt[t+1]<-Qalt[t]
      }else if(choices[t]==0){
        Qalt[t+1]<-Qalt[t]+alpha*(task$rewardAlt[t]-Qalt[t])
        Qref[t+1]<-Qref[t]
      }
    }
  }
  
  loglik<-NA
  Q<-data.frame(Qref,Qalt)
  ptp<-list(loglik=loglik,pchoice=pchoice,Q=Q,choices=choices)
  return(ptp)
}