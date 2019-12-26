

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org", dependencies = TRUE)

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
#20% of the data will be used for the test set
test_idx = createDataPartition(y = mushroom_data$classes, times=1, p=0.2, list=FALSE)
train_data = mushroom_data[-test_idx,]
test_data = mushroom_data[test_idx,]
rm(fl, mushroom_data, test_idx)

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

#predict with naive tree
predicted_naive <- predict_dectree_naive(test_data %>% select(-classes))
result_naive_tree_model <- confusionMatrix( predicted_naive, test_data$classes)
result_naive_tree_model

#train decision tree
predict_tree <- train_decision_tree_model(train_data)
predicted <- predict_decision_tree(test_data, predict_tree)
result_decision_tree_model <- confusionMatrix( predicted, test_data$classes)
result_decision_tree_model

uncertainty_plot(train_data)

odor_sporeprintcolor_values <- train_data %>% group_by(classes, odor, `spore-print-color`) %>% summarise()
odor_sporeprintcolor_values %>% print(n = nrow(odor_sporeprintcolor_values))

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
confusionMatrix(predict(train_knn, test_data, type="raw"), test_data$classes)