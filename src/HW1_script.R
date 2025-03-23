## ----setup, include = FALSE------------------------------------------------------------------------------------------
knitr::opts_chunk$set(warning=FALSE,
message=FALSE,
tidy.opts=list(width.cutoff = 90),
tidy = TRUE,
fig.align = 'center')


## ----include = F-----------------------------------------------------------------------------------------------------
#Library import
library(pacman)
p_load(tidyverse, ROCR, ggplot2, readr, ggthemes, kableExtra, caret, tidymodels, class, proc)


## ----include = F-----------------------------------------------------------------------------------------------------
setwd("~/Desktop/UniTrento/Courses/Second Semester/SM/HW/HW1")
df <- read_csv("data/CHD_Data.csv")


## ----echo = FALSE----------------------------------------------------------------------------------------------------
summary(df) %>%
  kable(format = "latex", booktabs = TRUE, caption = "Summary of the Dataset") %>%
  kable_styling(latex_options = c("HOLD_position", "scale_down", 'striped'))


## --------------------------------------------------------------------------------------------------------------------
df <- na.omit(df)


## ----include=FALSE---------------------------------------------------------------------------------------------------
str(df)


## --------------------------------------------------------------------------------------------------------------------
df_cleaned <- df |>
  mutate(sex = as.factor(sex),
         education = factor(education, levels = c(1, 2, 3, 4), 
                            labels = c('NoHS', 'HS', 'COL', 'P-COL')),
         smoker = factor(smoker, levels = c(0,1), labels = c('No', 'Yes')),
         stroke = factor(stroke, levels = c(0,1), labels = c('No', 'Yes')),
         diabetes = factor(diabetes, levels = c(0,1), labels = c('No', 'Yes')),
         HTN = factor(HTN, levels = c(0,1), labels = c('No', 'Yes')),
         CHD = as.factor(CHD))


## ----include = F-----------------------------------------------------------------------------------------------------
categ <- numer <- c()

for (name in names(df_cleaned)){
  if (is.numeric(df_cleaned[[name]])) {
    numer <- append(numer, name)
  } else {
    categ <- append(categ, name)
  }
}


## ----echo=FALSE, fig.width=6, fig.height=4---------------------------------------------------------------------------
par(mfrow = c(2, 3), oma = c(0, 0, 2, 0))
for (col in numer){
  fmla <- as.formula(paste(col,'~CHD'))
  boxplot(fmla, data = df_cleaned, main = col, ylab = col)
}
mtext("Continuous Variables (By CHD)", outer = TRUE, cex = 1.5, font = 2)


## ----include = F-----------------------------------------------------------------------------------------------------
c2 <- V <- p_vals <- c()

for (col in categ) {
  
  ch2 <- chisq.test(df_cleaned[[col]], df$CHD)
  V2 <- sqrt((ch2$statistic/sum(ch2$observed))/ch2$parameter)
  
  c2 <- append(c2, ch2$statistic)
  
  p_vals <- append(p_vals, ch2$p.value)
  
  V <- append(V, V2)
  
}


## ----echo=FALSE------------------------------------------------------------------------------------------------------
sum_tab_cat <- as.data.frame(rbind(c2, V, p_vals))
names(sum_tab_cat) <- categ
sum_tab_cat <- select(sum_tab_cat, -CHD)
rownames(sum_tab_cat) <- c('Chi-squared', "Cramer's V", 'p-value')
sum_tab_cat <- round(sum_tab_cat, 2)

kable(sum_tab_cat, format = "latex", booktabs = TRUE, caption = "Chi-Squared, Association Index, and p-value for Pearson's Test of Independence") %>%
   kable_styling(latex_options = c("striped", "HOLD_position", "scale_down"))


## ----echo = F, fig.width=10, fig.height=5----------------------------------------------------------------------------

par(mfrow = c(2,4), oma = c(0, 0, 2, 0))
for (col in categ){
  tb <- as.data.frame(table(df_cleaned[[col]]))
  names(tb) <- c(col, 'frequency')
  
  fmla <- as.formula(paste('frequency~',col))
  
  barplot(fmla, data = tb)
}
mtext("Class Imbalance", outer = TRUE, cex = 1.5, font = 2)


## --------------------------------------------------------------------------------------------------------------------
set.seed(11)
train_idx <- caret::createDataPartition(df_cleaned$CHD, p=0.75)$Resample1

train_data <- df_cleaned[train_idx, ]
test_data <- df_cleaned[-train_idx, ]


## --------------------------------------------------------------------------------------------------------------------
lr <- glm(CHD ~ .,
          data = train_data,
          family = binomial)


## ----echo=FALSE------------------------------------------------------------------------------------------------------
sum <- tidy(lr) %>%
   mutate(
    highlight = ifelse(p.value < 0.05, "yes", "no")  # Identify significant rows
  ) 

sum %>%
  select(-highlight) %>%
   kable(format = "latex", booktabs = TRUE, caption = "Summary Table for Logistic Regression Model") %>%
   kable_styling(latex_options = c("HOLD_position", "scale_down"), font_size = 6) %>%
  row_spec(which(sum$highlight == 'yes'), background = "lightgray")  # Highlight significant rows


## ----fig.height=8, include = F---------------------------------------------------------------------------------------
pairs(df_cleaned, lower.panel = panel.smooth, upper.panel = NULL)


## ----include = F-----------------------------------------------------------------------------------------------------
train_scaled <- train_data %>%
  select(-categ) %>%
  scale() %>%
  as_tibble() %>%
  cbind(train_data[, categ]) %>%
  mutate(across(setdiff(categ, 'CHD'), as.numeric)) 
#converts all categorical columns into numeric values apart from
#the CHD variable

test_scaled <- test_data %>%
  select(-categ) %>%
  scale() %>%
  as_tibble() %>%
  cbind(test_data[, categ]) %>%
  mutate(across(setdiff(categ, 'CHD'), as.numeric))



## --------------------------------------------------------------------------------------------------------------------
X_train <- select(train_scaled, -CHD)
X_test <- select(test_scaled, -CHD)
y_train <- train_scaled$CHD
y_test <- test_scaled$CHD


## ----include=FALSE---------------------------------------------------------------------------------------------------
set.seed(11)
test_errors <- train_errors <-  ks <- c()
for (k in 1:20) {
  preds_test <- knn(X_train, X_test, y_train, k = k)
  preds_train <- knn(X_train, X_train, y_train, k = k)
  ks <- append(ks, 1/k)
  test_err <- mean(preds_test != y_test)
  test_errors <- append(test_errors, test_err)
  train_err <- mean(preds_train != y_train)
  train_errors <- append(train_errors, train_err) 
  }


## --------------------------------------------------------------------------------------------------------------------
predics <- predict(lr, newdata = test_data, type = 'response')
classes <- ifelse(predics > .5, 'Yes', 'No')


## ----echo=FALSE------------------------------------------------------------------------------------------------------
table(test_data$CHD, classes) %>%
  kable(format = "latex", booktabs = TRUE, caption = "Confusion Matrix for Logistic Regression") %>%
  kable_styling(latex_options = c("HOLD_position", "scale_down"))


## --------------------------------------------------------------------------------------------------------------------
precision <- 6/(6+7)
recall <- 6/(6+145)


## ----echo=FALSE------------------------------------------------------------------------------------------------------
print(paste('precision is:', round(precision, 2)))
print(paste('recall is:', round(recall, 2)))


## ----include = FALSE-------------------------------------------------------------------------------------------------
lr_prob <- predict(lr, newdata = test_data, type = "response")


## ----echo=FALSE, fig.height=3, fig.width=5---------------------------------------------------------------------------
predob <- ROCR::prediction(lr_prob, test_data$CHD)

perf <- ROCR::performance(predob, "tpr", "fpr")

plot(perf, main = 'ROC Curve')
abline(0, 1, col = "red", lty = 2)


## --------------------------------------------------------------------------------------------------------------------
# Compute ROC curve
roc_obj <- pROC::roc(test_data$CHD, lr_prob)

# Compute and print AUC
auc_value <- pROC::auc(roc_obj)
print(auc_value)


## ----echo = F, fig.width=6, fig.height=4-----------------------------------------------------------------------------

plot(ks, train_errors, type = 'l', main = 'Train vs Test Performance',
     ylab = 'Error Rate', xlab = '1/k', ylim = c(0, .3))

lines(ks, test_errors, col = 'red', type = 'l')

legend("topleft", legend = c("Train", "Test"), col = c("black", "red"), lty = 1, lwd = 2)


## --------------------------------------------------------------------------------------------------------------------
naive_class <- rep('No', nrow(test_scaled))

acc_knn <- mean(naive_class != test_scaled$CHD)


## ----echo=FALSE------------------------------------------------------------------------------------------------------
print(paste("The accuracy of the K-NN classifier is of", round(acc_knn, 2)))

