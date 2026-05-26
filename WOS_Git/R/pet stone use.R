#********************
#
# empty the workspace
#
#********************
rm(list=ls())

# method 1
method_1=c()
for(abc in 1:10){
  categories=c(0, 0, 0, 0, 0, 0)
  categories_record=c()
  attempt=0
  while(sum(categories==100)<6){
    attempt=attempt+1
    
    target_categories_ind=which(categories<100)
    
    value=rnorm(length(target_categories_ind), mean=0.0114, sd=0.01)
    
    temp=value[value<0]
    temp[temp <= -0.23]=-0.23
    
    value[value<0]=temp
    
    temp=value[value>=0]
    temp[temp >= 0.22]=0.22
    
    value[value>=0]=temp
    
    if(sum(value)>0){
      categories[target_categories_ind]=categories[target_categories_ind]+value
    }
    
    #
    temp=categories[categories<=0]
    temp[temp <= 0]=0
    
    categories[categories<0]=temp
    
    temp=categories[categories>=100]
    temp[temp >= 100]=100
    
    categories[categories>=100]=temp
    
    categories_record=rbind(categories_record,
                            categories)
  }
  
  method_1=c(method_1, attempt)
}



# method 2
method_2=c()
for(abc in 1:10){
  categories=c(0, 0, 0, 0, 0, 0)
  categories_record=c()
  attempt=0
  for(r in 1:6){
    
    if(sum(categories==100)==6){
      break
    }
    
    i=which(categories==max(categories[which(categories<100)]))[1]
    
    while(categories[i]<100){
      attempt=attempt+1
      
      target_categories_ind=which(categories<100)
      
      value=rnorm(length(target_categories_ind), mean=0.0114, sd=0.01)
      
      temp=value[value<0]
      temp[temp <= -0.23]=-0.23
      
      value[value<0]=temp
      
      temp=value[value>=0]
      temp[temp >= 0.22]=0.22
      
      value[value>=0]=temp
      
      if(value[which(i==target_categories_ind)]>=0){
        categories[target_categories_ind]=categories[target_categories_ind]+value
      }
      
      #
      temp=categories[categories<=0]
      temp[temp <= 0]=0
      
      categories[categories<0]=temp
      
      temp=categories[categories>=100]
      temp[temp >= 100]=100
      
      categories[categories>=100]=temp
      
      categories_record=rbind(categories_record,
                              categories)
    }
  }
  
  method_2=c(method_2, attempt)
}

par(mfrow = c(rows=2, cols=3))
for(i in 1:6){
  categories_record[,i] %>% plot
}

summary(method_1)
summary(method_2)
