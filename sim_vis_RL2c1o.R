sim_vis_RL2c1o<-function(theModel){
  #for any learning task with two options and one outcome
  #wrriten by Eddie Xiao 
  #adapting https://github.com/EddieBWXiao/Study_ProbabilisticLearningAndDM/blob/main/simvis_RW.m
  #(work in progress)
  
  #theModel: the "function handle", e.g. RW1lr1beta_2arm, no () behind
  
  task<-list(outcome=data.frame(ref=c(1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,0,1,1),
                                alt=c(0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0)))
  params<-list(alpha=0.1,beta=10)
  
  sim<-theModel(params,task)
  
  # plot(sim$choice_prob[,1],
  #      type = "l", #draw the line 
  #      col = "blue", lwd = 2,#thick blue
  #      xlab = "trials", ylab = "probability",#add labels
  #      legend = "choice prob") 
  # lines(sim$value[,1], col = "red", legend="value")
  # points(sim$choice, col = "black", legend="choices")
  #legend("bottomleft",) #bad: hard to add these
  
  
  
  
  return(sim)
}