#********************
#
# empty the workspace
#
#********************
rm(list=ls())


# default parameters
floors=c(1, 2, 3, 4, 5, 6, 7)
stars=c(5, 5, 20, 30, 70, 125, 160)
rewards=c(24, 75, 180, 750, 2160, 2980, 5960)
n_attempt=1000

records=data.table()
for(i in 1:n_attempt){
  # reset variables
  protected_exp="Yes"
  current_floor=1
  attempt_at_current_floor=1
  descent=0
  stars_used=c()
  record=data.table(
  )
  
  # climb up
  while(descent==0 & 
        current_floor<7){
    star_required=stars[which(current_floor==floors)]
    
    stars_used=c(stars_used, star_required)
    
    switch(
      protected_exp,
      Yes={
        if(current_floor==1 |
           attempt_at_current_floor==4){
          move=sample(
            x=c(1, 2, 3, 0, 0),
            size=1,
            replace=F,
            prob=c(0.24, 0.059, 0.001, 0, 0)
          )
        }else{
          move=sample(
            x=c(1, 2, 3, 0, 0),
            size=1,
            replace=F,
            prob=c(0.24, 0.059, 0.001, 0.6, 0.1)
          )
        }
      },
      No={
        if(current_floor==1){
          move=sample(
            x=c(1, 2, 3, -1, -2),
            size=1,
            replace=F,
            prob=c(0.24, 0.059, 0.001, 0, 0)
          )
        }else{
          move=sample(
            x=c(1, 2, 3, -1, -2),
            size=1,
            replace=F,
            prob=c(0.24, 0.059, 0.001, 0.6, 0.1)
          )
        }
      }
    )
    
    if(move<0){
      if(current_floor==2 & move==-2){
        move=-1
      }
      descent=1
      
      attempt_at_current_floor=1 # reset
      current_floor=current_floor+move
      
      record=rbind(
        record,
        data.table(
          floor=current_floor,
          stars_used=sum(stars_used)
        )
      )
    }
    
    if(move==0){
      attempt_at_current_floor=attempt_at_current_floor+1
    }
    
    if(move>0){
      if(current_floor==5 & move>=3){
        move=2
      }
      if(current_floor==6 & move>=2){
        move=1
      }
      
      attempt_at_current_floor=1 # reset
      current_floor=current_floor+move
      
      record=rbind(
        record,
        data.table(
          floor=current_floor,
          stars_used=sum(stars_used)
        )
      )
    }
  }
  
  # record
  records=rbind(
    records,
    record
  )
}


rewards[-1]/
  records[,
          mean(stars_used),
          by="floor"
  ][order(floor)][[2]]




# default parameters
floors=c(1, 2, 3, 4, 5, 6, 7)
stars=c(5, 0, 0, 0, 0, 0, 0)
rewards=c(24, 75, 180, 750, 2160, 2980, 5960)
n_attempt=10000

current_floors=c()
records=data.table()
for(i in 1:n_attempt){
  # i=1
  # reset variables
  protected_exp="No"
  current_floor=1
  attempt_at_current_floor=1
  descent=0
  stars_used=c()
  record=data.table(
  )
  
  # climb up
  while(descent==0 & 
        current_floor<4){ # if 5, up to 4 floor
    star_required=stars[which(current_floor==floors)]
    
    stars_used=c(stars_used, star_required)
    
    switch(
      protected_exp,
      Yes={
        if(current_floor==1 |
           attempt_at_current_floor==4){
          move=sample(
            x=c(1, 2, 3, 0, 0),
            size=1,
            replace=F,
            prob=c(0.24, 0.059, 0.001, 0, 0)
          )
        }else{
          move=sample(
            x=c(1, 2, 3, 0, 0),
            size=1,
            replace=F,
            prob=c(0.24, 0.059, 0.001, 0.6, 0.1)
          )
        }
      },
      No={
        if(current_floor==1){
          move=sample(
            x=c(1, 2, 3, -1, -2),
            size=1,
            replace=F,
            prob=c(0.24, 0.059, 0.001, 0, 0)
          )
        }else{
          move=sample(
            x=c(1, 2, 3, -1, -2),
            size=1,
            replace=F,
            prob=c(0.24, 0.059, 0.001, 0.6, 0.1)
          )
        }
      }
    )
    
    if(move<0){
      if(current_floor==2 & move==-2){
        move=-1
      }
      descent=1
      
      attempt_at_current_floor=1 # reset
      current_floor=current_floor+move
      
      record=rbind(
        record,
        data.table(
          floor=current_floor,
          stars_used=sum(stars_used)
        )
      )
    }
    
    if(move==0){
      attempt_at_current_floor=attempt_at_current_floor+1
    }
    
    if(move>0){
      if(current_floor==5 & move>=3){
        move=2
      }
      if(current_floor==6 & move>=2){
        move=1
      }
      
      attempt_at_current_floor=1 # reset
      current_floor=current_floor+move
      
      record=rbind(
        record,
        data.table(
          floor=current_floor,
          stars_used=sum(stars_used)
        )
      )
    }
  }
  
  # record
  current_floors=c(current_floors, current_floor)
  records=rbind(
    records,
    tail(record, 1)
  )
}
current_floors %>% table
records[,
        reward:=fcase(
          floor==1, 24,
          floor==2, 75,
          floor==3, 180,
          floor==4, 750,
          floor==5, 2160,
          floor==6, 2980,
          floor==7, 5960
        )]
records$floor %>% table
sum(records$reward)/(n_attempt*5)


stars=c(5, 5, 20, 30, 70, 125, 160)
rewards=c(24, 75, 180, 750, 2160, 2980, 5960, 11920)
current_floor=1
for(current_floor in 1:7){
  if(current_floor==1){
    print(
      sum((rewards[(current_floor+1):(current_floor+3)]-rewards[current_floor])*c(0.24, 0.059, 0.001)/sum(c(0.24, 0.059, 0.001)))/stars[current_floor]
    )
  }
  if(current_floor%in%c(2, 3, 4)){
    print(
      sum((rewards[(current_floor+1):(current_floor+3)]-rewards[current_floor])*c(0.24, 0.059, 0.001))/stars[current_floor]
    )
  }
  if(current_floor==5){
    print(
      sum((rewards[(current_floor+1):(current_floor+2)]-rewards[current_floor])*c(0.24, 0.06))/stars[current_floor]
    )
  }
  if(current_floor==6){
    print(
      sum((rewards[(current_floor+1):(current_floor+2)]-rewards[current_floor])*c(0.24, 0.06))/stars[current_floor]
    )
  }
  if(current_floor==7){
    print(
      sum((rewards[(current_floor+1):(current_floor+1)]-rewards[current_floor])*c(0.3))/stars[current_floor]
    )
  }
}

for(current_floor in 1:7){
  if(current_floor==1){
    print(
      sum(c(75-24, 180-24, 750-24)*(c(0.24, 0.059, 0.001)/sum(c(0.24, 0.059, 0.001))))
    )
  }
  if(current_floor%in%c(2)){
    print(
      sum(c(180-75, 750-75, 2160-75, 24-75, 24-75)*c(0.24, 0.059, 0.001, 0.6, 0.1))
    )
  }
  if(current_floor%in%c(3)){
    print(
      sum(c(750-180, 2160-180, 2980-180, 75-180, 24-180)*c(0.24, 0.059, 0.001, 0.6, 0.1))
    )
  }
  if(current_floor%in%c(4)){
    print(
      sum(c(2160-750, 2980-750, 5980-750, 180-750, 75-750)*c(0.24, 0.059, 0.001, 0.6, 0.1))
    )
  }
  if(current_floor==5){
    print(
      sum(c(2980-2160, 5960-2160, 5960-2160, 750-2160, 180-2160)*c(0.24, 0.059, 0.001, 0.6, 0.1))
    )
  }
  if(current_floor==6){
    print(
      sum(c(5960-2980, 5960-2980, 5960-2980, 2160-2980, 750-2980)*c(0.24, 0.059, 0.001, 0.6, 0.1))
    )
  }
  if(current_floor==7){
    print(
      sum(c(11920-5960, 11920-5960, 11920-5960, 2980-5960, 2160-5960)*c(0.24, 0.059, 0.001, 0.6, 0.1))
    )
  }
}
