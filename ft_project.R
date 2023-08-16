set.seed(321)
options(max.print = 999)
mydata<-read.csv("E:/DS_final_project/heart.csv",header = TRUE)
print(mydata)
missing_values <- colSums(is.na(mydata))
print(missing_values)
str(mydata)
summary(mydata)
scale(mydata[,1:13])
cor_matrix <- cor(mydata[,1:13])
rounded_cor_matrix <- round(cor_matrix, digits = 3)
print(rounded_cor_matrix)
#install.packages("corrplot")
library(corrplot)
cor_matrix <- cor(mydata[,1:13], method = "pearson")
print(cor_matrix)
corrplot(cor_matrix, method = "number")

#kNN------train test
library(cluster)
features <- mydata[, 1:13]
target <- mydata[, 14]
# Split data into training and testing sets (e.g., 70-30 split)
set.seed(321)
train_indices <- sample(seq_len(nrow(features)), size = 0.7 * nrow(features))
train_data <- features[train_indices, ]
train_target <- target[train_indices]
test_data <- features[-train_indices, ]
test_target <- target[-train_indices]
library(class)
#install.packages("caret")
library(caret)
# Train kNN model
k <- 10  # Number of neighbors
knn_model <- knn(train_data, test_data, train_target, k)
knn_model
# Evaluate model performance
sum<- 0
accuracy <- sum(knn_model == test_target) / length(test_target)
print(paste("Accuracy:", accuracy))
confusion_matrix <- table(Actual = test_target, Predicted = knn_model)
confusion_matrix <- confusion_matrix / rowSums(confusion_matrix)
#install.packages("gplots")  
library(gplots)

# Create a heatmap 
heatmap.2(confusion_matrix,
          trace = "none", col = colorRampPalette(c("white", "red"))(100),
          main = "Confusion Matrix", xlab = "Predicted", ylab = "Actual")

# Perform 10-fold cross-validation
num_folds <- 10
fold_indices <- cut(seq_along(target), breaks = num_folds, labels = FALSE)
accuracy_scores <- numeric(num_folds)
sum<- 0
for (fold in 1:num_folds) {
       test_indices <- fold_indices == fold
         train_data <- features[!test_indices, ]
         train_target <- target[!test_indices]
         test_data <- features[test_indices, ]
         test_target <- target[test_indices]
        knn_model <- knn(train_data, test_data, train_target, k)
         accuracy_scores[fold] <- sum(knn_model == test_target) / length(test_target)
}
# Create a line plot with connected points
accuracy_data <- data.frame(Fold = factor(1:num_folds), Accuracy = accuracy_scores)
ggplot(accuracy_data, aes(x = Fold, y = Accuracy, group = 1)) +
       geom_line() +
       geom_point() +
       labs(x = "Fold", y = "Accuracy") +
       ggtitle("Accuracy Trend across Folds")

# True Positive (TP), False Positive (FP), False Negative (FN)
TP <- confusion_matrix[2, 2]
FP <- confusion_matrix[1, 2]
FN <- confusion_matrix[2, 1]

#Precision and Recall
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)

print(paste("Precision:", precision))
print(paste("Recall:", recall))
