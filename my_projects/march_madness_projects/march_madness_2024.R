# march_madness_2024.R
# by Lindsey Hornberger 
# a logistic regression analysis to predict if a team will make it to the March 
# Madness Tournament based on team statistics such as Pre-Tournament.AdjTempo,
# Pre-Tournament.AdjOE, Pre-Tournament.AdjDE, Pre-Tournament.AdjEM, AvgHeight, 
# and Experience. 

# import libraries 
library(dplyr)
library(readr)
library(vip)
library(caret)
library(pROC)

# get data
data <- read_csv("march_madness/DEV _ March Madness.csv", 
                 col_types = cols(`Tournament Winner?` = col_factor(levels = c("No", "Yes")), 
                                  `Tournament Championship?` = col_factor(levels = c("No", "Yes")), 
                                  `Final Four?` = col_factor(levels = c("No", "Yes")), 
                                  `Top 12 in AP Top 25 During Week 6?` = col_factor(levels = c("No", "Yes"))))
# filter data 
model_data <- data %>%
  filter(Season == 2024) %>% 
  mutate(MadeTournament=ifelse(`Post-Season Tournament`=="March Madness", "Yes", "No"))# select for the 2024 season 

# select predictors 
predictors <- model_data %>%
  select(`Pre-Tournament.AdjTempo`,
         `Pre-Tournament.AdjOE`,
         `Pre-Tournament.AdjDE`,
         `Pre-Tournament.AdjEM`,
         `AvgHeight`, `Experience`)

training_data <- bind_cols(
  predictors,
  MadeTournament = model_data$MadeTournament
)

# split into test and train datasets
set.seed(123)
train_idx <- sample(seq_len(nrow(training_data)), size = 0.7 * nrow(training_data))
train_data <- training_data[train_idx, ]
test_data  <- training_data[-train_idx, ]

#fit logistic model
model_marchmadness <- glm(MadeTournament ~ .,
                          data = train_data,
                          family = binomial)
summary(model_marchmadness)

# cross-validation 
training_data$MadeTournament <- as.factor(training_data$MadeTournament) 
train_ctrl <- trainControl(
  method = "cv",              # "cv" = k-fold cross-validation
  number = 10,                # 10-fold
  savePredictions = "final",  # keep the predictions
  classProbs = TRUE,          # needed for ROC curve/AUC
  summaryFunction = twoClassSummary  # to compute ROC, Sens, Spec
)
logit_model <- train(
  MadeTournament ~ .,
  data = training_data,
  method = "glm",
  family = "binomial",
  trControl = train_ctrl,
  metric = "ROC"
)

logit_model$results
logit_model$resample
logit_model$pred

# Predict probabilities on test set
# get only the probability of "Yes"
test_data$pred_prob_yes <- test_data$pred_prob$Yes

# classify using that column
test_data$pred_class <- ifelse(test_data$pred_prob_yes > 0.5, "Yes", "No")

# now confusion matrix will work
table(Predicted = test_data$pred_class, Actual = test_data$MadeTournament)

# roc/auc 
roc_obj <- roc(test_data$MadeTournament, test_data$pred_prob_yes)
plot(roc_obj, col = "blue")
auc(roc_obj)

# variable importance plots   
vip(model_marchmadness, num_features = 10)

# confusion matrix 
cm <- table(Predicted = test_data$pred_class,
           Actual = test_data$MadeTournament)

cm
TP <- cm["Yes", "Yes"]   # True Positives
FP <- cm["Yes", "No"]    # False Positives

PPV <- TP / (TP + FP)
PPV


# SUMMARY -- auc = 0.906 w/cross-validation, ppv = 0.75

