#function for calculating the entropy of one feature in the dataset
#the feature musst be a factor!
#
#parameters:
#     dataset:  the data set containing data for calculation
#     colX:     the index of the feature column for calculating the entropy (indexing starts with 1)
#
#return:
#     the entropy of the feature in the dataset as numeric value 

entropy <- function(dataset, colX){
  
  #the colX index must be between 1 and the count of the columns
  #if not, than the entropy is not calculable and we return 0 as entropy
  if (ncol(dataset) < colX | colX < 1){
    return (0)
  }
  
  #initialize the return variable
  ret <- 0
  #calculate the count of the records for each value of the given feature
  summarized_data <- dataset %>% group_by(.[,colX]) %>% summarise(n = n())
  #the row count of the whole dataset
  rowCount <- nrow(dataset)
  
  #get all possible feture values
  level_items <- levels(dataset[,colX])
  
  #calculate for all feature values..
  for (item in level_items){
    #probability of the actual feature value
    prob <- summarized_data %>% filter(.[,1] == item) %>% pull(n) / rowCount
    #add probability multiplied by the 2 base log of the probability to the overall summ
    ret <- ret + prob * log(prob,base = 2)
  }
  #return the entropy value
  return (-ret)
}


#function for calculating the conditional entropy between two features in the dataset
#Both features muss be a factor! The feature indexes start with 1.
#
#parameters:
#     dataset:  the data set containing the data for calculation
#     colX:     the index of the feature column for calculating the conditinal entropy for
#     colY:     the index of the condiion feature column for calculating the conditional entropy
#
#return:
#     the conditional entropy of the two features in the dataset as numeric value 

cond_entropy <- function(dataset, colX, colY){
  
  #the colX index must be between 1 and the count of the columns
  #if not, than the condiitonal entropy is not calculable and we return 0 as entropy
  if (ncol(dataset) < colX | colX < 1){
    return (0)
  }
  
  #the colY index must be between 1 and the count of the columns
  #if not, than the conditional entropy is not calculable and we return 0 as entropy
  if (ncol(dataset) < colY | colY < 1){
    return (0)
  }
  
  
  #initialize the return variable
  ret <- 0
  #the count of the data rows in the dataset
  rowCount <- nrow(dataset)
  
  #calculate the count of the records for each value of the condition feature
  summarized_y <- dataset %>% group_by(.[,colY]) %>% summarise(n = n()) 
  #rename the feature col name to Y (it's easier to handle in the further code)
  names(summarized_y)[1] <- "Y"
  
  #get all values for the condition feature
  level_itemsy <- levels(summarized_y$Y)
  
  #calculation loop for all possible condition values
  for (itemy in level_itemsy){
    
    #calculate the probability of the condition
    proby <- summarized_y %>% filter(Y == itemy) %>% pull(n) / rowCount
    
    #calculate the count of the compare feature values using the condition value as filter
    summarized_x <- dataset %>% filter(.[,colY] == itemy) %>% group_by(.[,colX]) %>% summarise(n = n()) 
    
    #rename the compare feature col name to X (it's easier to handle in the further code)
    names(summarized_x)[1] <- "X"
    
    #get all values for the compare feature
    level_itemsx <- levels(summarized_x$X)
    
    #calculation loop for all possible compare values
    for (itemx in level_itemsx){
      #calculate the count of the condition feature value
      filtered <- summarized_x %>% filter(X == itemx)
      #if there are values and the condition probability is not 0
      if (nrow(filtered) > 0 && proby != 0){
        #calculate the joined probabiliy P(X,Y)
        probxy <-  filtered$n / rowCount
        #calculate the conditional probabilit p(X|Y)
        probx_at_y <- probxy / proby
        #add conditinal probability multiplied by the 2 base log 
        #of the conditional probability to the overall summ
        ret <- ret + probxy * log(probx_at_y,base = 2)
      }
    }
  }
  #return the conditional entropy
  return (-ret)
}


#function for calculating the uncertainty score between two features in the dataset
#Both features muss be a factor! The feature indexes start with 1.
#
#parameters:
#     dataset:  the data set containing the data for calculation
#     colX:     the index of the feature column for calculating the conditinal entropy for
#     colY:     the index of the condiion feature column for calculating the conditional entropy
#
#return:
#     the uncertainty score of the two features in the dataset as numeric value 


uncertainty <- function(dataset, colX, colY){
  #calculate the entropy for the compare feature
  entr <- entropy(dataset,colX)
  
  #if the enropy is 0, we return 0 as uncertainty
  if (entr == 0){
    return(0)
  }
  
  #calculate the conditional entropy for the two feature
  cond_entr <- cond_entropy(dataset, colX, colY)
  
  #return the uncertainty score for the features
  return ( (entr - cond_entr) / entr)
}


#plot function for the uncertainty matrix for a given dataset
#All features must be factor
#
#parameters:
#     dataset:  the data set containing the data for the plot
#

uncertainty_plot <- function(dataset){
  #initialize the dataframe for the uncertainty score matrix
  uncertainty_df <- data_frame(X=character(), idx=numeric(), Y=character(), idy=numeric(), value=numeric())
  
  #feature names for the plot
  labels <- names(dataset)
  #loop for all features (x coordinates)
  for (i in 1:ncol(dataset))
  {
    #loop for all features (y coordinates)
    for(j in 1:ncol(dataset))
    {
      #add the calculated uncertainty score to the result dataframe
      uncertainty_df <- bind_rows(uncertainty_df, data_frame(Y = labels[j], idy=j , X=labels[i], idx=i, value=uncertainty(train_data,i,j)))
    }
  }
  
  #plot the uncertainty matrix with colors
  uncertainty_df %>% ggplot(aes(x=reorder(X,idx), y=reorder(Y,-idy), fill=value)) + geom_tile() + 
    geom_text(aes(label=round(value,2)), color='white') +
    theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    xlab('X feature') + ylab('Y feature') + ggtitle('uncertainty score U(X|Y)')
}


#plot function for the value distribution for all feature values to edibility classification 
#All features must be factor and the first feature must be the edibility classification
#
#parameters:
#     dataset:  the data set containing the data for the plot
#

feature_ditribution_plot <- function(data){
  
  #initialize the plot list
  plots <- list()
  
  #loop for all features except edibility
  for (i in 1:(ncol(data)-1))
  {
    #calculate the count of the feature values per edibility classification
    summarized_data <- data %>% group_by(classes, .[,i+1]) %>% summarise(n = n())
    #rename the feature column(it's easier to handle in the further code)
    names(summarized_data)[2] <- "attr"
    #create the plot for the current feature 
    plot <- summarized_data %>% ggplot(aes(attr , classes)) + geom_point(aes(size=n)) +
      xlab(names(data)[i+1]) + ylab("Edibility") 
    #add the plot to the plot list
    plots[[i]] <- plot
  }
  #remove the unnecessary variables
  rm(summarized_data, i, plot)
  
  #draw all the created plots in a grid with 3 columns
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