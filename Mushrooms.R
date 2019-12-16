

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(Rborist)) install.packages("Rborist", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(cluster)) install.packages("cluster", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org", dependencies = TRUE)

source("predict_model.R")

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
#10% of the data will be used for the test set
test_idx = createDataPartition(y = mushroom_data$classes, times=1, p=0.2, list=FALSE)
train_data = mushroom_data[-test_idx,]
test_data = mushroom_data[test_idx,]
rm(fl, mushroom_data, test_idx)


plots <- list()
#Plots all the attributes for eatable <-> poisious
for (i in 1:(ncol(train_data)-1))
{
  summarized_data <- train_data %>% group_by(classes, .[,i+1]) %>% summarise(n = n())
  names(summarized_data)[2] <- "attr"
  plot <- summarized_data %>% ggplot(aes(attr , classes)) + geom_point(aes(size=n)) +
          xlab(names(train_data)[i+1]) + ylab("Eatable") 
  plots[[i]] <- plot
}
rm(summarized_data, i, plot)
grid.arrange(grobs=plots,ncol=3)

ring_type_ep <- test_data %>% filter(`ring-type` %in% c('e','p')) %>% select(-`ring-type`)
plots <- list()
#Plots all the attributes for eatable <-> poisious
for (i in 1:(ncol(ring_type_ep)-1))
{
  summarized_data <- ring_type_ep %>% group_by(classes, .[,i+1]) %>% summarise(n = n())
  names(summarized_data)[2] <- "attr"
  plot <- summarized_data %>% ggplot(aes(attr , classes)) + geom_point(aes(size=n)) +
          xlab(names(ring_type_ep)[i+1]) + ylab("Eatable") 
  plots[[i]] <- plot
}
rm(summarized_data, i, plot)
grid.arrange(grobs=plots,ncol=3)


odor_n <- ring_type_ep %>% filter(odor == 'n') %>% select(-odor)
plots <- list()
#Plots all the attributes for eatable <-> poisious
for (i in 1:(ncol(odor_n)-1))
{
  summarized_data <- odor_n %>% group_by(classes, .[,i+1]) %>% summarise(n = n())
  names(summarized_data)[2] <- "attr"
  plot <- summarized_data %>% ggplot(aes(attr , classes)) + geom_point(aes(size=n)) +
    xlab(names(odor_n)[i+1]) + ylab("Eatable") 
  plots[[i]] <- plot
}
rm(summarized_data, i, plot)
grid.arrange(grobs=plots,ncol=3)

spore_print_color <- odor_n %>% filter(`spore-print-color` == 'w') %>% select(-`spore-print-color`)
plots <- list()
#Plots all the attributes for eatable <-> poisious
for (i in 1:(ncol(spore_print_color)-1))
{
  summarized_data <- spore_print_color %>% group_by(classes, .[,i+1]) %>% summarise(n = n())
  names(summarized_data)[2] <- "attr"
  plot <- summarized_data %>% ggplot(aes(attr , classes)) + geom_point(aes(size=n)) +
    xlab(names(spore_print_color)[i+1]) + ylab("Eatable") 
  plots[[i]] <- plot
}
rm(summarized_data, i, plot)
grid.arrange(grobs=plots,ncol=3)

habitat_l <- spore_print_color %>% filter(habitat == 'l') %>% select(-habitat)
plots <- list()
#Plots all the attributes for eatable <-> poisious
for (i in 1:(ncol(habitat_l)-1))
{
  summarized_data <- habitat_l %>% group_by(classes, .[,i+1]) %>% summarise(n = n())
  names(summarized_data)[2] <- "attr"
  plot <- summarized_data %>% ggplot(aes(attr , classes)) + geom_point(aes(size=n)) +
    xlab(names(habitat_l)[i+1]) + ylab("Eatable") 
  plots[[i]] <- plot
}
rm(summarized_data, i, plot)
grid.arrange(grobs=plots,ncol=3)

uncertinity_df <- data.frame(X=factor(), idx=numeric(), Y=factor(), idy=numeric(), value=numeric())
labels <- names(train_data)
for (i in 1:ncol(train_data))
{
  for(j in 1:ncol(train_data))
  {
    uncertinity_df <- bind_rows(uncertinity_df, data.frame(Y = labels[j], idy=j , X=labels[i], idx=i, value=uncertinity(train_data,i,j)))
  }
}

uncertinity_df %>% ggplot(aes(x=reorder(X,idx), y=reorder(Y,-idy), fill=value)) + geom_tile() + 
        geom_text(aes(label=round(value,2)), color='white') +
        theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
        xlab('X feature') + ylab('Y feature') + ggtitle('Uncertinity score U(X|Y)')


image(uncertinity_matrix)

#remove 'veil-type' it has only one value, therefore is not relevant
train_data2 <- train_data %>% select(-`veil-type`)

#train knn method
train_knn <- train(classes ~ ., method="knn", data = train_data2)  

result_knn <- confusionMatrix(predict(train_knn, test_data, type="raw"), test_data$classes)

train_rforest <- train(classes ~ .,
                    method = "Rborist",
                    tuneGrid = data.frame(predFixed = 2, minNode = c(3, 50)),
                    data = train_data2)
result_rforest <- confusionMatrix(predict(train_rforest, test_data), test_data$classes)

predicted <- predict(test_data %>% select(-classes))
result_model <- confusionMatrix( predicted$y, test_data$classes)