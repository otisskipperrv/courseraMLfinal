library(tidyverse)
library(caret)

setwd('/Users/oskipper/Documents/Data Science/DSAccelerator/ThirdPhase/Coursera/MachineLearningFinal/')

training <- read.csv('pml-training.csv')
testing <- read.csv('pml-testing.csv')


#Create a validation set from our training set so we can estimate accuracy
trainIndex <- createDataPartition(training$classe, p = .8, list = FALSE)
validation_holdout <- training[-trainIndex,]
training_subset <- training[trainIndex,]

raw_train <- training_subset
#  select(classe, accel_belt_x, accel_belt_y, accel_belt_z) %>% 
  
#  head(1000)

# Replace Blank Values With NA
raw_train[raw_train==""] <- NA

# Determine which columns are mostly NA - want to get rid of them

# Get rid of sparse columns with mostly NA values
all_cols <- colnames(raw_train)

cols_na_count <- raw_train %>% 
  summarise_all(funs(sum(is.na(.))))



# Create a vector of only columns we want
# From data we see either columns have 938 NAs or 0, so we can use ==0 
# If data was different ==0 may not be a good assumption
cols_no_na <- all_cols[cols_na_count == 0]
# Get columns with no NA values - also manually drop some columns
train_1 <- raw_train %>% 
  select(cols_no_na) %>% 
  # Get rid of index column
  select(-X, -raw_timestamp_part_1, -user_name, -raw_timestamp_part_2, -cvtd_timestamp, -new_window, -num_window)




# See some variables with correlation ~0.93
# But nothing more extreme - keep all variables for the time being


# Create subset to train, just to determine 15 most 'important' variables
var_importance_train <- train_1[sample(nrow(train_1), 2000), ]


var_importance_model <- train(classe ~ .,method = "rf", data=var_importance_train) # build model

total_num_vars <- 15
important_vars <- data.frame(varImp(tmp_model)[1][1])
important_vars <- rownames_to_column(important_vars, 'var_name')

important_vars <- important_vars %>% 
  arrange(desc(Overall)) %>% 
  head(total_num_vars)

important_vars


train_imp_vars <- train_1 %>% 
  select(important_vars$var_name, classe)




train_control <- trainControl(method = "cv", number = 10, savePredictions = TRUE)


#rf_mod <- train(classe ~ . , method = "rf", data = train_1)
rf_mod <- train(classe ~ ., method = "rf", data = train_1, trControl = train_control)
glm_mod <- train(classe ~ ., method = "glm", data = train_1, trControl = train_control)
rpart_mod <- train(classe ~ ., method = "rpart", data = train_1, trControl = train_control)




#preds1 <- predict(rf_mod, validation_holdout)
rf_preds <- predict(rf_mod, validation_holdout)
glm_preds <- predict(glm_mod, validation_holdout)
rpart_preds <- predict(rpart_mod, validation_holdout)

comb_df <- data.frame(rf_preds, glm_preds, rpart_preds, classe =validation_holdout$classe)
comb_mod <- train(classe ~ ., method = "rf", data = comb_df)
comb_preds <- predict(comb_mod, validation_holdout)

preds_summary <- data.frame(rf_preds, glm_preds, rpart_preds, comb_preds, validation_holdout$classe)
preds_summary %>% group_by_(.dots = names(preds_summary)) %>% count() %>% arrange(desc(n)) %>% View

confusionMatrix(rf_preds, validation_holdout$classe)
confusionMatrix(glm_preds, validation_holdout$classe)
confusionMatrix(rpart_preds, validation_holdout$classe)
confusionMatrix(comb_preds, validation_holdout$classe)


tmp <- glm_preds %>% as.data.frame() 
colnames(tmp) <- 'preds'
tmp %>% group_by(preds) %>% count()
