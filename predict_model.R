
predict1 <- function(features){
  predicted <- data.frame(y = character())
  for (i in 1 : nrow(features))
  {
    y <- tryCatch({
        ring_type <- features[i,]$`ring-type`
        odor <- features[i,]$odor
        spore_print_color <- features[i,]$`spore-print-color`
        habitat <- features[i,]$habitat
        cap_color <- features[i,]$`cap-color`
      
        if (ring_type %in% c('l','n')){
          'p'
        }
        else if (ring_type  == 'f'){
          'e'
        }
        else if (odor %in% c('c','f', 'p','s','y')){
          'p'
        }
        else if (odor %in% c('a','l')){
          'e'
        }
        else if (spore_print_color %in% c('b','k','n','o','y')){
          'e'
        }
        else if (spore_print_color == 'r'){
          'e'
        }
        else if (habitat == 'd'){
          'p'
        }
        else if (habitat %in% c('g','p','w')){
          'e'
        }
        else if (cap_color %in% c('c','n')){
          'e'
        }
        else if (cap_color %in% c('w','y')){
          'p'
        }
        else {
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

predict2 <- function(features){
  predicted <- data.frame(y = character())
  for (i in 1 : nrow(features))
  {
    y <- tryCatch({
      ring_type <- features[i,]$`ring-type`
      odor <- features[i,]$odor
      spore_print_color <- features[i,]$`spore-print-color`
      habitat <- features[i,]$habitat
      population <- features[i,]$`population`
      
      if (ring_type %in% c('l','n')){
        'p'
      }
      else if (ring_type  == 'f'){
        'e'
      }
      else if (odor %in% c('c','f', 'p','s','y')){
        'p'
      }
      else if (odor %in% c('a','l')){
        'e'
      }
      else if (spore_print_color %in% c('b','k','n','o','y')){
        'e'
      }
      else if (spore_print_color == 'r'){
        'e'
      }
      else if (habitat == 'd'){
        'p'
      }
      else if (habitat %in% c('g','p','w')){
        'e'
      }
      else if (population == 'v'){
        'e'
      }
      else if (population == 'c'){
        'p'
      }
      else {
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

predict3 <- function(features){
  predicted <- data.frame(y = character())
  for (i in 1 : nrow(features))
  {
    y <- tryCatch({
      ring_type <- features[i,]$`ring-type`
      odor <- features[i,]$odor
      spore_print_color <- features[i,]$`spore-print-color`
      habitat <- features[i,]$habitat
      stalk_color_below_ring <- features[i,]$`stalk-color-below-ring`
      
      if (ring_type %in% c('l','n')){
        'p'
      }
      else if (ring_type  == 'f'){
        'e'
      }
      else if (odor %in% c('c','f', 'p','s','y')){
        'p'
      }
      else if (odor %in% c('a','l')){
        'e'
      }
      else if (spore_print_color %in% c('b','k','n','o','y')){
        'e'
      }
      else if (spore_print_color == 'r'){
        'e'
      }
      else if (habitat == 'd'){
        'p'
      }
      else if (habitat %in% c('g','p','w')){
        'e'
      }
      else if (stalk_color_below_ring == 'n'){
        'e'
      }
      else if (stalk_color_below_ring %in% c('w', 'y')){
        'p'
      }
      else {
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
