sim_vis_RL2c1o<-function(theModel,params,task){
  #for any learning task with two options and one outcome
  #wrriten by Eddie Xiao 
  #adapting https://github.com/EddieBWXiao/Study_ProbabilisticLearningAndDM/blob/main/simvis_RW.m
  #(work in progress)
  
  #theModel: the "function handle", e.g. RW1lr1beta_2arm, no () behind
  
  # task<-list(outcome=data.frame(ref=c(1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,0,1,1),
  #                               alt=c(0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0)))
  # params<-list(alpha=0.1,beta=10)
  
  sim<-theModel(params,task=task)
  
  # plot(sim$choice_prob[,1],
  #      type = "l", #draw the line 
  #      col = "blue", lwd = 2,#thick blue
  #      xlab = "trials", ylab = "probability",#add labels
  #      legend = "choice prob") 
  # lines(sim$value[,1], col = "red", legend="value")
  # points(sim$choice, col = "black", legend="choices")
  #legend("bottomleft",) #bad: hard to add these
  
  #turn sim into data frame by trials
  df<-data.frame(value=sim$value[,1],
                 p_choose_ref = sim$choice_prob[,1],
                 choice = sim$choice,
                 o_ref = task$outcome$ref)
  df$trial<-1:nrow(df)
  
  p<-ggplot(df,aes(x=trial))+
    geom_line(aes(y=p_choose_ref),colour='blue')+
    geom_line(aes(y=value),colour='green')+
    geom_point(aes(y=o_ref),colour='black')+
    scale_y_continuous(expand = c(0, 0), #following Alex Pike's code
                       limits=c(-0.1,1.1))+
    labs(x = "trial",
         y = "probability",
         colour = "Legend")+
    theme_classic()
  #plot(p)
  
  
  
  return(p)
}
