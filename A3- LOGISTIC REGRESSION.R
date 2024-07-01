options(repos = c(CRAN = "https://cran.rstudio.com/"))


install.packages(pROC)
install.packages(e1071)
install.packages(rpart)
install.packages(rpart.plot)
# Load necessary libraries
library(readr)
library(dplyr)
library(caret)
library(pROC)
library(e1071)
library(rpart)
library(rpart.plot)
library(ggplot2)

# Set working directory and load dataset
setwd('E:\\ASSIGNMENT\\Data')
getwd()
df <- read_csv("Car Evaluation.csv")

# Display the first few rows of the dataset
head(df)

# Clean column names
names(df) <- gsub(" ", "_", names(df))
print(names(df)) 

# Check the target variable distribution
table(df$decision)

df$buyingprice <- as.numeric(as.factor(df$buyingprice))
df$maintenancecost <- as.numeric(as.factor(df$maintenancecost))
df$safety <- as.numeric(as.factor(df$safety))
df$decision <- as.numeric(as.factor(df$decision)) - 1 

# Scale data to range [0, 1]
preProc <- preProcess(df, method = c("range"))
df_scaled <- predict(preProc, df)


# Split data into training and testing sets
set.seed(42)
trainIndex <- createDataPartition(df_scaled$decision, p = .8, 
                                  list = FALSE, 
                                  times = 1)
df_train <- df_scaled[trainIndex,]
df_test <- df_scaled[-trainIndex,]

x_train <- df_train %>% select(-decision)
y_train <- df_train$decision
x_test <- df_test %>% select(-decision)
y_test <- df_test$decision

# Feature importance using rpart
fit <- rpart(decision ~ ., data = df_train, method = "class")
importance <- as.data.frame(varImp(fit, scale = FALSE))
importance <- importance[order(-importance$Overall),]
selected_features <- rownames(head(importance, 15))
print(selected_features)




x_train_selected <- x_train[, selected_features]
x_test_selected <- x_test[, selected_features]

logreg <- glm(decision ~ ., data = data.frame(x_train_selected, decision = y_train), family = binomial)


y_pred <- predict(logreg, newdata = data.frame(x_test_selected), type = "response")
y_pred_class <- ifelse(y_pred > 0.5, 1, 0)

confusionMatrix(factor(y_pred_class), factor(y_test))

roc_curve <- roc(y_test, y_pred)
plot.roc(roc_curve, main = "ROC Curve for Logistic Regression")
auc(roc_curve)

# Check for missing values
sum(is.na(y_train))
sum(is.na(y_test))

# Convert to factor if necessary
y_train <- as.factor(y_train)
y_test <- as.factor(y_test)

# Check class distribution
table(y_train)
table(y_test)

fit <- rpart(decision ~ ., data = df_train, method = "class")
importance <- as.data.frame(varImp(fit, scale = FALSE))
importance <- importance[order(-importance$Overall),]
selected_features <- rownames(head(importance, 15))
print(selected_features)

x_train_selected <- x_train[, selected_features]
x_test_selected <- x_test[, selected_features]

y_pred <- predict(logreg, newdata = data.frame(x_test_selected), type = "response")
y_pred_class <- ifelse(y_pred > 0.5, 1, 0)

confusionMatrix(factor(y_pred_class), factor(y_test))

# ROC curve and AUC
roc_curve <- roc(y_test, y_pred)
plot.roc(roc_curve, main = "ROC Curve for Logistic Regression")
auc(roc_curve)

# Train decision tree model
dtree <- rpart(decision ~ ., data = data.frame(x_train_selected, decision = y_train), method = "class")


y_pred_dt <- predict(dtree, newdata = data.frame(x_test_selected), type = "class")

# Predict on test set
y_pred_dt <- predict(dtree, newdata = data.frame(x_test_selected), type = "class")

# Classification report
confusionMatrix(y_pred_dt, factor(y_test))

# ROC curve and AUC
y_pred_proba <- predict(dtree, newdata = data.frame(x_test_selected), type = "prob")[,2]
roc_curve_dt <- roc(y_test, y_pred_proba)
plot.roc(roc_curve_dt, main = "ROC Curve for Decision Tree")
auc(roc_curve_dt)

# Confusion matrix for Logistic Regression
confusionMatrix(factor(y_pred_class), factor(y_test))

# Confusion matrix for Decision Tree
confusionMatrix(y_pred_dt, factor(y_test))

# Function to parse classification report
parse_classification_report <- function(conf_matrix, model_name) {
  precision <- conf_matrix$byClass["Pos Pred Value"]
  recall <- conf_matrix$byClass["Sensitivity"]
  f1 <- 2 * (precision * recall) / (precision + recall)
  support <- conf_matrix$table[2,2] + conf_matrix$table[1,2]
  
  data.frame(
    model = model_name,
    class = c("0", "1"),
    precision = c(conf_matrix$byClass["Neg Pred Value"], precision),
    recall = c(conf_matrix$byClass["Specificity"], recall),
    f1_score = c(NA, f1),
    support = c(conf_matrix$table[1,1] + conf_matrix$table[1,2], support)
  )
}

# Logistic Regression report
conf_matrix_logreg <- confusionMatrix(factor(y_pred_class), factor(y_test))
df_logreg <- parse_classification_report(conf_matrix_logreg, "Logistic Regression")

# Decision Tree report
conf_matrix_dtree <- confusionMatrix(y_pred_dt, factor(y_test))
df_dtree <- parse_classification_report(conf_matrix_dtree, "Decision Tree")

# Combine and display
comparison_df <- rbind(df_logreg, df_dtree)
print(comparison_df)


