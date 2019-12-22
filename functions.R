
entropy <- function(dataset, colX){
  
  ret <- 0
  summarized_data <- dataset %>% group_by(.[,colX]) %>% summarise(n = n())
  rowCount <- nrow(dataset)
  level_items <- levels(dataset[,colX])
  for (item in level_items){
    prob <- summarized_data %>% filter(.[,1] == item) %>% pull(n) / rowCount
    ret <- ret + prob * log(prob,base = 2)
  }
  return (-ret)
}

cond_entropy <- function(dataset, colX, colY){
  ret <- 0
  rowCount <- nrow(dataset)
  summarized_y <- dataset %>% group_by(.[,colY]) %>% summarise(n = n()) 
  names(summarized_y)[1] <- "Y"
  level_itemsy <- levels(summarized_y$Y)
  
  for (itemy in level_itemsy){
    proby <- summarized_y %>% filter(Y == itemy) %>% pull(n) / rowCount
    summarized_x <- dataset %>% filter(.[,colY] == itemy) %>% group_by(.[,colX]) %>% summarise(n = n()) 
    names(summarized_x)[1] <- "X"
    level_itemsx <- levels(summarized_x$X)
    for (itemx in level_itemsx){
      filtered <- summarized_x %>% filter(X == itemx)
      if (nrow(filtered) > 0 && proby != 0){
        probxy <-  filtered$n / rowCount
        probx_at_y <- probxy / proby
        ret <- ret + probxy * log(probx_at_y,base = 2)
      }
    }
  }
  return (-ret)
}

uncertinity <- function(dataset, colX, colY){
  entr <- entropy(dataset,colX)
  cond_entr <- cond_entropy(dataset, colX, colY)
  if (entr == 0){
    return(0)
  }
  return ( (entr - cond_entr) / entr)
}

uncertinity_plot <- function(dataset){
  #calculate uncertinity for all feature pairs
  uncertinity_df <- data_frame(X=character(), idx=numeric(), Y=character(), idy=numeric(), value=numeric())
  labels <- names(dataset)
  for (i in 1:ncol(dataset))
  {
    for(j in 1:ncol(dataset))
    {
      uncertinity_df <- bind_rows(uncertinity_df, data_frame(Y = labels[j], idy=j , X=labels[i], idx=i, value=uncertinity(train_data,i,j)))
    }
  }
  
  #plot the uncertinity with colors
  uncertinity_df %>% ggplot(aes(x=reorder(X,idx), y=reorder(Y,-idy), fill=value)) + geom_tile() + 
    geom_text(aes(label=round(value,2)), color='white') +
    theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    xlab('X feature') + ylab('Y feature') + ggtitle('Uncertinity score U(X|Y)')
}

feature_ditribution_plot <- function(data){
  plots <- list()
  #Plots all the attributes for eatable <-> poisious
  for (i in 1:(ncol(data)-1))
  {
    summarized_data <- data %>% group_by(classes, .[,i+1]) %>% summarise(n = n())
    names(summarized_data)[2] <- "attr"
    plot <- summarized_data %>% ggplot(aes(attr , classes)) + geom_point(aes(size=n)) +
      xlab(names(data)[i+1]) + ylab("Edibility") 
    plots[[i]] <- plot
  }
  rm(summarized_data, i, plot)
  grid.arrange(grobs=plots,ncol=3)
}


#function for cross validation
#parameters:
#     trainset: the train set to use for the cross validation
#     cv_n:     the count of the cross validation
#     FUNC:     the function to call for the actual cross validation train and test set (calculated from the param trainset)
#     ...:      additional parameter necessary for calling the provided function
#
#return:
#     dataframe with the function result for the cross validations (the data frame has cv_n items)

cross_validation <- function(trainset, cv_n, FUNC,...){
  
  #get the count of the data rows on the train set
  data_count = nrow(trainset)
  
  #initialize the data frame for the result
  values_from_cv = data_frame()
  
  #randomise the trainset. 
  #If the train set is ordered (not randomised, like the movielens dataset) the cross validation
  #will not be independent and provide wrong result
  trainset_randomised <- trainset[sample(nrow(trainset)),]
  
  #create the train- and testset for the cross validation
  #we need cv_n run, therefore we use a loop 
  for (i in c(1:cv_n)){
    #evaulate the size of the test set. This will be the 1/cv_n part of the data
    part_count = data_count / cv_n
    
    #select the data from the parameter train set
    #we get the part_count size elements from the parameter train set 
    idx = c(   (trunc((i-1) * part_count) + 1) : trunc(i * part_count) )
    
    #tmp holds the new test set
    test = trainset_randomised[idx,]
    #train holds the new test set
    train = trainset_randomised[-idx,]
    
    #call the provided function to the actual train and test set.
    akt_value <- FUNC(train, test,...)
    
    #add the result to the data frame
    #the column 'cv' contains the idx of the cross validation run
    values_from_cv <- bind_rows(values_from_cv, akt_value %>% mutate(cv = i))
  }
  
  #return the results of each cross validation
  return(values_from_cv)
}