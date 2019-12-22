

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(Rborist)) install.packages("Rborist", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(cluster)) install.packages("cluster", repos = "http://cran.us.r-project.org", dependencies = TRUE)

source("predict_model.R")
source("functions.R")

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
test_idx = createDataPartition(y = mushroom_data$classes, times=1, p=0.1, list=FALSE)
train_data = mushroom_data[-test_idx,]
test_data = mushroom_data[test_idx,]
rm(fl, mushroom_data, test_idx)


feature_ditribution_plot(train_data)
uncertinity_plot(train_data)

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

#remove 'veil-type' it has only one value, therefore is not relevant
train_data2 <- train_data %>% select(-`veil-type`)

#train decision tree
predicted <- predict_dectree(test_data %>% select(-classes))
result_dec_tree_model <- confusionMatrix( predicted$y, test_data$classes)

#train features model with cross validation
F1_scores <- cross_validation(train_data,5, calculate_F1_score, 1:22) %>% group_by(feature_count) %>%
  summarise(F1=mean(F1))
opt_feature_count <- which.max(F1_scores$F1)
result_feature_model <- confusionMatrix(predict_feature_model(test_data, 
                        train_feature_model(train_data, opt_feature_count)), test_data$classes)


#train knn method
train_knn <- train(classes ~ ., method="knn", data = train_data2)  
result_knn <- confusionMatrix(predict(train_knn, test_data, type="raw"), test_data$classes)

#train random forest
train_rforest <- train(classes ~ .,
                    method = "Rborist",
                    tuneGrid = data.frame(predFixed = 2, minNode = c(3, 50)),
                    data = train_data2)
result_rforest <- confusionMatrix(predict(train_rforest, test_data), test_data$classes)