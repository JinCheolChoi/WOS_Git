#*************************
#
# Empty the workspace ----
#
#*************************
rm(list=ls())

#
library(data.table)
library(dplyr)

#
Data=data.table(
  trial=c(1:10),
  cost=c(1, 2, 3, 4, 6, 8, 12, 15, 20, 25),
  reset=c(15, 14, 13, 10, 6, 5, 4, 3, 2, 1)
)
# Data[, win_prob:=1/(10-trial+1)]
Data[, win_prob:=NULL]

# Rewards
Rewards=data.table(
  Ind=c(1:10),
  Probs=c(
    1.79,
    19.95,
    19.95,
    19.95,
    7.67,
    7.67,
    7.67,
    9.21,
    3.07,
    3.07
  )
)

# th_reset=2
reward=85
n=100000

TH_reset=c()
Sum_Cost=c()
Mean_Cost=c()
Var_Cost=c()
for(th_reset in 1:10){
  # th_reset=5
  TH_reset=c(TH_reset, th_reset)
  Costs=sapply(1:n,
               function(x){
                 algo_stop=F
                 cost=c()
                 total_cost=c()
                 i=1
                 Rewards_Temp=Rewards
                 while(algo_stop==F){
                   
                   cost=c(cost, Data[i, cost])
                   
                   win_ind=sample(Rewards_Temp[["Ind"]],
                                  1,
                                  prob=Rewards_Temp[["Probs"]])
                   
                   if(win_ind==1){
                     algo_stop=T
                     Rewards_Temp=Rewards # reset the reward probs
                     next
                   }else if(win_ind!=1){
                     if(i==th_reset){ # reset after ith open
                       cost=c(cost, Data[i+1, reset])
                       Rewards_Temp=Rewards # reset the reward probs
                       i=1 # reset i to 1
                     }else{
                       Rewards_Temp=Rewards_Temp[Ind!=win_ind, ] # remove the probability of the reward just obtained
                       i=i+1
                     }
                   }
                   # print(Rewards_Temp)
                 }
                 sum_cost=sum(cost)
               })
  Sum_Cost=c(Sum_Cost,
             sum(Costs))
  Mean_Cost=c(Mean_Cost,
              mean(Costs))
  Var_Cost=c(Var_Cost,
             var(Costs))
}
Sum_Cost
Mean_Cost
Var_Cost

#
which.min(Mean_Cost^2+Var_Cost)

# 
95/Mean_Cost[which.min(Mean_Cost^2+Var_Cost)]

data.table(
  `reset after nth`=c(1:10),
  Sum_Cost,
  Mean_Cost,
  Var_Cost
)

#**************
# save and load
#**************
#save.image(paste0("C:/Users/jchoi02/Desktop/To delete/2024-09-05.Rdata"))
#load(paste0("C:/Users/jchoi02/Desktop/To delete/2024-09-05.Rdata"))
