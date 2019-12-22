
predict_dectree <- function(features){
  predicted <- data.frame(y = character())
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
    predicted <- bind_rows(predicted, data.frame(y = y))
  }
    
  predicted <- predicted %>% mutate(y=as.factor(y))
  return(predicted)
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
  
  e_values <- e_values %>% anti_join(p_values, by=names(p_values))
  e_values <- e_values %>% mutate(pred='e')
  return(e_values)
}

predict_feature_model <- function(dataset, e_values){
 feature_names = names(e_values %>% select(-pred))  
 pred <- dataset %>% left_join(e_values, by=feature_) %>%
            mutate(y = as.factor(ifelse(is.na(pred),'p', pred))) %>% pull(y)
  
 return(pred)
}

calculate_F1_score <- function(train, test, features_counts){
  F1_all = data_frame(feature_count = numeric(), F1 = numeric())
  for(i in features_counts){
    train_model <- train_feature_model(train, i)
    predicted <- predict_feature_model(test, train_model)
    result <- confusionMatrix(predicted, test$classes, positive = 'e')
    F1_all <- bind_rows(F1_all, data_frame(feature_count = i, F1 = result$byClass[["F1"]]))
  }
  return(F1_all)
}