
predict_dectree <- function(features){
  predicted <- tibble(y = character())
  for (i in 1 : nrow(features))
  {
    y <- tryCatch({
        odor <- features[i,]$odor
        spore_print_color <- features[i,]$`spore-print-color`
        gill_color <- features[i,]$`gill-color`
        gill_size <- features[i,]$`gill-size`
        stalk_surface <- features[i,]$`stalk-surface-below-ring`
        ring_type <- features[i,]$`ring-type`
        
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
        else{
          'p'
        }
    }, warning = function(w){
        return('p')
    }, error = function(e) {
        return('p')
    })
    predicted <- bind_rows(predicted, data_frame(y = y))
  }
    
  predicted <- predicted %>% mutate(y=factor(y, levels=c('e','p'))) %>% pull(y)
  return(predicted)
}

predict_decision_tree <- function(dataset, tree){
  ret <- list()
  for(i in 1:nrow(dataset)){
    y <- 'p'
    count_rules <- nrow(tree)
    for(j in 1:count_rules){
      feature <- tree$feature[j]
      e_values <- unlist(str_split(tree$e_values[j], ','))
      p_values <- unlist(str_split(tree$p_values[j], ','))
      
      if (dataset[i,feature] %in% e_values){
        y <- 'e'
        break;
      }
      
      if (dataset[i,feature] %in% p_values){
        y <- 'p'
        break;
      }
    }
    ret <- c(ret, y)
  }
  return(factor(ret, levels = c('e','p')))
}

train_decision_tree_model <- function(dataset){
  
  
  tree <- data_frame(feature=character(), decided_proz=numeric(), e_values=character(), p_values=character())
  tree <- extend_decision_tree(dataset, tree)
  return(tree)
}

extend_decision_tree <- function(dataset, decision_rule){
  
  newRules <- data_frame(feature=character(),decided_proz=numeric(), e_values=character(), p_values=character())
  
  for (i in 1:(ncol(dataset)-1))
  {
    feature <- names(dataset)[i+1]
    summarized_data <- dataset %>% group_by(classes, .[,i+1]) %>% summarise(n = n())
    names(summarized_data)[2] <- "attr"
    values <- levels(summarized_data$attr)
    e_values <- list()
    p_values <- list()
    count = nrow(dataset)
    decided_count = 0
    e_pos <- 1
    p_pos <- 1
    for(val in values){
      e_count <- summarized_data %>% filter(classes=='e' & attr == val) %>% pull(n)
      e_count <- ifelse(is_empty(e_count),0,e_count)
      p_count <- summarized_data %>% filter(classes=='p' & attr == val) %>% pull(n)
      p_count <- ifelse(is_empty(p_count),0,p_count)
      
      
      if (e_count == 0 && p_count > 0){
        p_values[[p_pos]] <- val
        decided_count <- decided_count + p_count
        p_pos <- p_pos + 1
      }
      if (e_count > 0 && p_count == 0){
        e_values[[e_pos]] <- val
        decided_count <- decided_count + e_count
        e_pos <- e_pos + 1
      }
    }
    if (decided_count > 0){
      e_values <- paste(e_values, collapse=",")
      p_values <- paste(p_values,collapse=",")
      newRules <- bind_rows(newRules, data_frame(feature=feature, decided_proz=decided_count / count, e_values = e_values, 
                                                    p_values = p_values))
      
    }
  }
  
  if (nrow(newRules) > 0){
    idx <- which.max(newRules$decided_proz)
    bestRule <- newRules[idx,]
    e_values <- unlist(str_split(bestRule$e_values, ','))
    p_values <- unlist(str_split(bestRule$p_values, ','))
    
    remaining_data <- dataset %>% filter( !(.[,bestRule$feature] %in% e_values) & !(.[,bestRule$feature] %in% p_values))
    decision_rule <-bind_rows(decision_rule, bestRule)
    if (nrow(remaining_data) > 0){
      decision_rule <- extend_decision_tree(remaining_data, decision_rule)  
    }
  }
  
  return(decision_rule)
}




train_feature_model <- function(dataset, feat_count){
  
  uncertinity_df <- data_frame(X=character(), idx=numeric(), value=numeric())
  labels <- names(dataset)
  for (i in 1:ncol(dataset))
  {
      uncertinity_df <- bind_rows(uncertinity_df, data_frame(X=labels[i], idx=i, value=uncertinity(dataset,1,i)))
  }
  uncertinity_df <- uncertinity_df %>% arrange(-value)
  features <- head(uncertinity_df$X,feat_count+1)
  
  p_values <- dataset[,features] %>% filter(classes=='p') %>% select(-classes) %>% unique()
  e_values <- dataset[,features] %>% filter(classes=='e') %>% select(-classes) %>% unique()
  
  e_values <- e_values %>% anti_join(p_values, by=names(e_values))
  e_values <- e_values %>% mutate(pred='e')
  return(e_values)
}

predict_feature_model <- function(dataset, e_values){
 feature_names = names(e_values %>% select(-pred))  
 pred <- dataset %>% left_join(e_values, by=feature_names) %>%
            mutate(y = factor(ifelse(is.na(pred),'p', pred), c('e','p'))) %>% pull(y)
  
 return(pred)
}

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