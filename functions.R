
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