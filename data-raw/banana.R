library("imbalance")
library("ggplot2")
library("caret")
set.seed(12345)

# Load original banana dataset
data(banana_orig)
# Load imbalanced banana dataset
data(banana)
# Limits for the plot axis
myxlim <- c(-4, 4)
myylim <- c(-3, 4)
# Methods to apply and plot
methods <- c("original", "imbalanced", "SMOTE", "MWMOTE", "RWO", "PDFOS")

for(i in seq_along(methods)){
  method <- methods[i]

  if(method == "original"){
    dataset <- banana_orig
  }else{
    if(method == "imbalanced"){
      dataset <- banana
    }else{
      dataset <- oversample(banana, ratio = 0.7, method = method, filtering = T)
    }

    model <- knn3Train(dataset[, -3], banana_orig[, -3], dataset$Class, k = 3, l = 0, prob = TRUE, use.all = TRUE)
    model[model == "negative"] <- -1
    model[model == "positive"] <- 1
    model <- as.numeric(model)
    modelauc <- auc(banana_orig[, 3], model)
    print(paste(method, "AUC:", modelauc))
  }

  qplot(At1, At2, col = Class, data = dataset,
        xlab = "at1", ylab = "at2", xlim = myxlim, ylim = myylim) +
        scale_colour_manual(values = c("#E69F00", "#000000")) +
        geom_point(size = 2, alpha = 0.3) +
        theme(text = element_text(size = 30), legend.position="none")

  ggplot(dataset, aes_string("At1", "At2", col = "Class")) +
    geom_point(alpha = 0.3) +
    scale_color_manual(values = c("#E69F00", "#000000")) +
    geom_point(size = 3, alpha = 0.3) +
    theme(text = element_text(size = 30), legend.position="none")

        # theme(axis.text.x = element_text(size = 15),
        #   axis.text.y = element_text(size = 15),
        #   axis.title.x = element_text(size = 20),
        #   axis.title.y = element_text(size = 20))
  ggsave(paste("banana-", method, ".png", sep = ""), device = "png", width = 14, height = 7.85)
}


# 3-repeated 3-fold cross validation
# control <- trainControl(method = "repeatedcv", number = 3, repeats = 3,
#                         summaryFunction = twoClassSummary, classProbs = TRUE)
#
# model <- train(Class~., data = dataset, method="kknn",
#                trControl=control,
#                metric = "ROC",
#                tuneGrid = data.frame(kmax = 5, distance = 2, kernel = "rectangular"))
# model$results


