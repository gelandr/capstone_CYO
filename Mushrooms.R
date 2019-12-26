

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org", dependencies = TRUE)

# Mushrooms dataset:
# https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/
# https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data
# https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.names


#create directory for the data file if necessary
if (!dir.exists("mushrooms")){
  dir.create("mushrooms")
}

#download the data file only if not done yet
if (!file.exists("./mushrooms/agaricus-lepiota.data")){
  download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data", "./mushrooms/agaricus-lepiota.data")
}

fl <- file("mushrooms/agaricus-lepiota.data")
mushroom_data <- str_split_fixed(readLines(fl), ",", 23)
close(fl)

colnames(mushroom_data) <- c("classes", "cap-shape", "cap-surface", "cap-color", "bruises?", "odor", "gill-attachment",
                             "gill-spacing", "gill-size", "gill-color", "stalk-shape", "stalk-root", "stalk-surface-above-ring",
                             "stalk-surface-below-ring", "stalk-color-above-ring", "stalk-color-below-ring", "veil-type", "veil-color",
                             "ring-number", "ring-type", "spore-print-color", "population", "habitat")
mushroom_data <- as.data.frame(mushroom_data)

#initialize random sequenz 
set.seed(1, sample.kind = "Rounding")
#create index for train and test set
#20% of the data will be used for the test set
test_idx = createDataPartition(y = mushroom_data$classes, times=1, p=0.2, list=FALSE)
train_data = mushroom_data[-test_idx,]
test_data = mushroom_data[test_idx,]
rm(fl, mushroom_data, test_idx)

#function for calculating the entropy of one feature in the dataset
#the feature must be a factor!
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
#Both features must be a factor! The feature indexes start with 1.
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
#Both features must be a factor! The feature indexes start with 1.
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


feature_ditribution_plot(train_data)

odor_n <- train_data %>% filter(`odor` == 'n') %>% select(-`odor`)
feature_ditribution_plot(odor_n)

spore_print_color_w <- odor_n %>% filter(`spore-print-color` == 'w') %>% select(-`spore-print-color`)
feature_ditribution_plot(spore_print_color_w)

gill_color_w <- spore_print_color_w %>% filter(`gill-color` == 'w') %>% select(-`gill-color`)
feature_ditribution_plot(gill_color_w)

gill_size_n <- gill_color_w %>% filter(`gill-size` == 'n') %>% select(-`gill-size`)
feature_ditribution_plot(gill_size_n)

stalk_surface_below <- gill_size_n %>% filter(`stalk-surface-below-ring` == 's') %>% select(-`stalk-surface-below-ring`)
feature_ditribution_plot(stalk_surface_below)

#implementation of the naiv decision tree model based on the data analysis observation
#This implementation uses fix decision tree, therefore is not suitable for machine learning
#
#parameters:
#     dataset:  the data set containing the data for prediction
#
#return:
#     the predicted classification list

predict_dectree_naive <- function(dataset){
  
  #initalization for the result variable
  predicted <- tibble(y = character())
  
  #loop all the data in the dataset
  for (i in 1 : nrow(dataset))
  {
    y <- tryCatch({
      #get the feature values relevant for the fixed decision tree
      
      odor <- dataset[i,]$odor
      spore_print_color <- dataset[i,]$`spore-print-color`
      gill_color <- dataset[i,]$`gill-color`
      gill_size <- dataset[i,]$`gill-size`
      stalk_surface <- dataset[i,]$`stalk-surface-below-ring`
      ring_type <- dataset[i,]$`ring-type`
      
      #go throug the decision tree rules in an if - then tree
      if (odor %in% c('c','f','m','p','s','y')){
        'p'
      }
      else if (odor  %in% c('a','l')){
        'e'
      }
      else if (spore_print_color %in% c('e','g', 'n','o','p','y')){
        'e'
      }
      else if (spore_print_color %in% c('r')){
        'p'
      }
      else if (gill_color %in% c('y')){
        'p'
      }
      else if (gill_color %in% c('e','g','p')){
        'e'
      }
      else if (gill_size %in% c('b')){
        'e'
      }
      else if (stalk_surface %in% c('f')){
        'e'
      }
      else if (stalk_surface %in% c('y')){
        'p'
      }
      else if (ring_type %in% c('e')){
        'e'
      }
      #if not match in the tree, we predict poisonous
      else{
        'p'
      }
    }, warning = function(w){
      return('p')
    }, error = function(e) {
      return('p')
    })
    #add the current prediction to the result list
    predicted <- bind_rows(predicted, data_frame(y = y))
  }
  
  #convert the result to a factor with two values
  predicted <- predicted %>% mutate(y=factor(y, levels=c('e','p'))) %>% pull(y)
  #reutrn the prediction list
  return(predicted)
}

#predict with naive tree
predicted_naive <- predict_dectree_naive(test_data %>% select(-classes))
result_naive_tree_model <- confusionMatrix( predicted_naive, test_data$classes)
result_naive_tree_model

#Function for predict the edibility based on the given decision tree
#
#parameters:
#     dataset:  the data set containing the data for prediction
#     tree:     the decision tree for to use for the prediction
#
#return:
#     the predicted classification list

predict_decision_tree <- function(dataset, tree){
  #initialize the result list
  ret <- list()
  #loop through all data in the dataset
  for(i in 1:nrow(dataset)){
    #initialize the current classification
    #we initialize to the poisonous value, because we want to be sure,
    #that we classify 'p' for unknown results
    y <- 'p'
    #the decision rule count in the tree
    count_rules <- nrow(tree)
    #loop throug all the decision rules in the tree
    for(j in 1:count_rules){
      #get the feature name from the current decision rule
      feature <- tree$feature[j]
      
      if (length(tree$e_values[j]) > 0){
        #get the e rules from the current decision rule
        e_values <- unlist(str_split(tree$e_values[j], ','))
        #if the current data match to one of the e rule values
        if (dataset[i,feature] %in% e_values){
          #than we predic edible
          y <- 'e'
          #the current row is predicted, we can add the precition to the result
          break;
        }
      }
      
      if (length(tree$p_values[j]) > 0){
        #get the p rules from the current decision rule
        p_values <- unlist(str_split(tree$p_values[j], ','))
        
        #if the current data macht to one of the p rules
        if (dataset[i,feature] %in% p_values){
          #we predict poisonous
          y <- 'p'
          #the current row is predicted, we can add the precition to the result
          break;
        }
      }
    }
    #add the predicted value to the result
    ret <- c(ret, y)
  }
  #return the prediction list as factor
  return(factor(ret, levels = c('e','p')))
}

#Function to create a decision tree model based on the given dataset
#
#parameters:
#     dataset:  the data set containing the data for train the decision tree
#
#return:
#     the trained decisoin tree. The tree is a tibble with decision rules as row.
#     the columns contains the feature name for the rule, the values to predict 'e' edibility
#     and the valus to predict 'p' edibility

train_decision_tree_model <- function(dataset){
  
  #initialize the return tibble as empty
  tree <- data_frame(feature=character(), decided_proz=numeric(), e_values=character(), p_values=character())
  #call the recursive function to get the decision rules for the decision tree
  #we start the recursion with the whole dataset and an empty tree
  tree <- extend_decision_tree(dataset, tree)
  #return the decision tree
  return(tree)
}


#Recursive function to create the decision rules for the decision tree based on the given (remaining) dataset
#The function assumes, that the edibility classification is the first column in the dataset!
#All the features must be a factor
#
#parameters:
#     dataset:        the data set containing the data for train the decision tree
#     decision_tree:  the decision tree, we extend recursive with new rules based on the given dataset
#
#return:
#     the trained decision tree. The tree is a tibble with decision rules as row.
#     the columns contains the feature name for the rule, the values to predict 'e' edibility
#     and the valus to predict 'p' edibility

extend_decision_tree <- function(dataset, decision_tree){
  
  #initialize the frame with the possible new rules
  newRules <- data_frame(feature=character(),decided_proz=numeric(), e_values=character(), p_values=character())
  #the count of all data in the dataset
  count = nrow(dataset)
  
  #we go throug all the features except the edibility classification (col = 1)
  for (i in 1:(ncol(dataset)-1))
  {
    #Get the current feature name
    feature <- names(dataset)[i+1]
    #calculate the count of the feature values
    summarized_data <- dataset %>% group_by(classes, .[,i+1]) %>% summarise(n = n())
    #rename the feature col name to attr (it's easier to handle in the further code)
    names(summarized_data)[2] <- "attr"
    #get the possible feature values
    values <- levels(summarized_data$attr)
    #initialize the edible values list
    e_values <- list()
    #intialize the poisonous value list
    p_values <- list()
    #initialize the counter for the feature pereformance score
    decided_count = 0
    #initialize the index for the list entries
    e_pos <- 1
    p_pos <- 1
    
    #loop through all feature values
    for(val in values){
      
      #calculate the count of the edible entries for the feature value
      e_count <- summarized_data %>% filter(classes=='e' & attr == val) %>% pull(n)
      #if no definitely edible entries than set the count to 0
      e_count <- ifelse(is_empty(e_count),0,e_count)
      
      #calculate the count of the poisonous entries for the feature value
      p_count <- summarized_data %>% filter(classes=='p' & attr == val) %>% pull(n)
      #if no definitely edible entries than set the count to 0
      p_count <- ifelse(is_empty(p_count),0,p_count)
      
      #if the feature value has only p entries, than we can use it as p rule
      if (e_count == 0 && p_count > 0){
        #add the value to the p rules
        p_values[[p_pos]] <- val
        #update the feture performance score
        decided_count <- decided_count + p_count
        #update the p rule list index 
        p_pos <- p_pos + 1
      }
      
      #if the feature value has only e entries, than we can use it as e rule
      if (e_count > 0 && p_count == 0){
        #add the value to the e rules
        e_values[[e_pos]] <- val
        #update the feature performance score
        decided_count <- decided_count + e_count
        #update the e rule list index
        e_pos <- e_pos + 1
      }
    }
    
    #if the feature performance score is greater as 0 (we can use the feature as decision rule)
    if (decided_count > 0){
      #convert the lists to comma separated string
      e_values <- paste(e_values, collapse=",")
      p_values <- paste(p_values,collapse=",")
      
      #add the rule as possible new decision rule to the tibble
      #we add the percent of the data, that could be classified with the actuall rule
      #we will use this score for selecting the best rule
      newRules <- bind_rows(newRules, data_frame(feature=feature, decided_proz=decided_count / count, e_values = e_values, 
                                                 p_values = p_values))
      
    }
  }
  
  #if we found at least one decision rule for the given dataset 
  if (nrow(newRules) > 0){
    #look for the decision rule with the highest percentage of classified data
    idx <- which.max(newRules$decided_proz)
    bestRule <- newRules[idx,]
    
    #get the e rules from the best performing decision rule
    e_values <- unlist(str_split(bestRule$e_values, ','))
    #get the p rules from the best performing decision rule
    p_values <- unlist(str_split(bestRule$p_values, ','))
    
    #get the data from the dataset, that couldn't classify with the e and p rules
    remaining_data <- dataset %>% filter( !(.[,bestRule$feature] %in% e_values) & !(.[,bestRule$feature] %in% p_values))
    #add the best rule to the tree
    decision_tree <-bind_rows(decision_tree, bestRule)
    
    #if we have remaining data
    if (nrow(remaining_data) > 0){
      #than call the function recursvie again with the remaining data and the extended tree
      decision_tree <- extend_decision_tree(remaining_data, decision_tree)  
    }
  }
  
  #return the decision tree if we are ready with the recursion
  return(decision_tree)
}


#train decision tree
predict_tree <- train_decision_tree_model(train_data)
predicted <- predict_decision_tree(test_data, predict_tree)
result_decision_tree_model <- confusionMatrix( predicted, test_data$classes)
result_decision_tree_model

uncertainty_plot(train_data)

odor_sporeprintcolor_values <- train_data %>% group_by(classes, odor, `spore-print-color`) %>% summarise()
odor_sporeprintcolor_values %>% print(n = nrow(odor_sporeprintcolor_values))

#The function trains the feature model with the given amount of the features. To select the most relevant
#features, the function uses the uncertinity score calculation
#All the features must be a factor. The function assumes, that the edibility classification is
#the first column in the dataset
#
#parameters:
#     dataset:        the data set containing the data for train the feature model
#     feat_count:     the count of the feature to use for the training
#
#return:
#     the trained feature model. Thre result is a tibble containing the feature value combinations
#     for that we predict 'e' edibility

train_feature_model <- function(dataset, feat_count){
  
  #initialize the tibble for the uncertinity score
  uncertinity_df <- data_frame(X=character(), idx=numeric(), value=numeric())
  #get the feature name
  labels <- names(dataset)
  #loop all the features in the dataset
  for (i in 1:ncol(dataset))
  {
    #calculate the uncertinity score for the current feature with the first feature
    #(first feature contains the edibility classification)
    uncertinity_df <- bind_rows(uncertinity_df, data_frame(X=labels[i], idx=i, value=uncertainty(dataset,1,i)))
  }
  #order the data by the uncertinity score descending
  uncertinity_df <- uncertinity_df %>% arrange(-value)
  #get the edible classification + the following feat_count features 
  features <- head(uncertinity_df$X,feat_count+1)
  
  #select the unique data for all the features combination, where the classification is 'p'
  p_values <- dataset[,features] %>% filter(classes=='p') %>% select(-classes) %>% unique()
  #select the unique data for all the features combination, where the classification is 'e'
  e_values <- dataset[,features] %>% filter(classes=='e') %>% select(-classes) %>% unique()
  
  #filter out all the data from the e_values, that occures also in the p values
  e_values <- e_values %>% anti_join(p_values, by=names(e_values))
  #so we have the feature combinations with definitly 'e' classification
  #add this classification as column to the result
  e_values <- e_values %>% mutate(pred='e')
  
  #return the result
  return(e_values)
}

#The function predict the edibility classification for the given dataset
#based on the given feature model
#All the features must be a factor. 
#
#parameters:
#     dataset:        the data set containing the data for train the feature model
#     e_values:       the feature model (the e value combinations)
#
#return:
#     the predicted classification list

predict_feature_model <- function(dataset, e_values){
  #get the features containing in the feature model
  feature_names = names(e_values %>% select(-pred)) 
  
  #join the dataset to the feature model based on the selected features
  #we use left join, therefore there can be data without founded entry in the feature model
  #if we found an entry in the feture model (e values), than we predict 'e'
  #other case we predict 'p'
  #So we predict 'e' only for combination where we are sure, that the mushroom is edible
  pred <- dataset %>% left_join(e_values, by=feature_names) %>%
    mutate(y = factor(ifelse(is.na(pred),'p', pred), c('e','p'))) %>% pull(y)
  
  #return the predicted values
  return(pred)
}

#This function calculates the F1 score for the feature model for a given feature_count by 
#given train and test set. This function can be called by the n-fold cross validation function
#
#parameters:
#     train:            the train data set containing the data for train the feature model
#     test:             the test data set to predict the edibility classification
#     features_count:   the count of the feature to use for the training
#
#return:
#     the F1 score to the trained model

calculate_F1_score <- function(train, test, features_counts){
  F1_all = data_frame(feature_count = numeric(), F1 = numeric())
  for(i in features_counts){
    train_model <- train_feature_model(train, i)
    predicted <- predict_feature_model(test, train_model)
    result <- confusionMatrix(predicted, test$classes)
    F1_score <- result$byClass[["F1"]]
    F1_score <- ifelse(is.na(F1_score),0,F1_score)
    F1_all <- bind_rows(F1_all, data_frame(feature_count = i, F1=F1_score))
  }
  return(F1_all)
}

#train features model with cross validation
F1_scores <- cross_validation(train_data,5, calculate_F1_score, 1:22) %>% group_by(feature_count) %>%
  summarise(F1=mean(F1))
#plot the F1 scores depending on the used feature count 
F1_scores %>% ggplot(aes(feature_count, F1)) + geom_point()

#get the optimum feature count
opt_feature_count <- which.max(F1_scores$F1)
#calculate the result for our feature count based model
result_feature_model <- confusionMatrix(predict_feature_model(test_data, 
                                                              train_feature_model(train_data, opt_feature_count)), test_data$classes)

train_data2 <- train_data %>% select(-`veil-type`)
train_knn <- train(classes~., method='knn', data=train_data2)
result_knn <- confusionMatrix(predict(train_knn, test_data, type="raw"), test_data$classes)

cat("The F1 value for the naiv decision tree: ", result_naive_tree_model$byClass[["F1"]])
cat("The F1 value for the trained decision tree: ", result_decision_tree_model$byClass[["F1"]])
cat("The F1 value for the feature model: ", result_feature_model$byClass[["F1"]])
cat("The F1 value for the knn method: ", result_knn$byClass[["F1"]])
