library(pacman)
p_load(tidyverse, ROCR, ggplot2, readr, ggthemes, gtsummary)

df <- read_csv("CHD Data.csv")
theme_set(theme_base())
df <- na.omit(df)
# EDA ---------------------------------------------------------------------

str(df)

df <- df |>
  mutate(sex = as.factor(sex),
         education = as.factor(education),
         smoker = as.factor(smoker),
         stroke = as.factor(stroke),
         diabetes = as.factor(diabetes),
         HTN = as.factor(HTN),
         CHD = as.factor(CHD))

str(df)

categ <- numer <- c()

for (name in names(df)){
  if (is.numeric(df[[name]])) {
    numer <- append(numer, name)
  } else {
    categ <- append(categ, name)
  }
}

summary(df)

sum(is.na(df))

par(mfrow = c(2,3))

for (col in numer){
  hist(df[[col]], main = col, xlab = col, freq = F)
  mini <- min(df[[col]], na.rm = T)
  maxi <- max(df[[col]], na.rm = T)
  
  x <- seq(mini, maxi, .001)
  avg <- mean(df[[col]], na.rm = T)
  std <- sd(df[[col]], na.rm = T)
  y <- dnorm(x, mean = avg, sd = std)
  
  lines(x, y, col = 'red')
  
}

par(mfrow = c(2,4))

for (col in categ){
  tb <- as.data.frame(table(df[[col]]))
  names(tb) <- c(col, 'frequency')
  
  fmla <- as.formula(paste('frequency~',col))
  
  barplot(fmla, data = tb)
}

#there are many imbalanced classes, for instance stroke, diabetes and the very dependent variable CHD

#do boxplots to check which predictor might have the highest potential to separate classes

par(mfrow = c(2,3))

for (col in numer){
  fmla <- as.formula(paste(col,'~CHD'))
  boxplot(fmla, data = df, main = col, ylab = col)
}

# ggplot(df, aes(x = CHD, y = age)) +
#   geom_boxplot(fill='grey')

#from the analysis above we see that age has a good predictive power as the median age of the patients developing CHD is much higher compared to the median of those that do not

# checking independence for categorical variables -------------------------

c2 <- V <- p_vals <- c()

for (col in categ) {
  
  ch2 <- chisq.test(df[[col]], df$CHD)
  V2 <- sqrt((ch2$statistic/sum(ch2$observed))/ch2$parameter)
  
  c2 <- append(c2, ch2$statistic)
  
  p_vals <- append(p_vals, ch2$p.value)
  
  V <- append(V, V2)
  
}

sum_tab_cat <- as.data.frame(rbind(c2, V, p_vals))
names(sum_tab_cat) <- categ
sum_tab_cat <- select(sum_tab_cat, -CHD)
rownames(sum_tab_cat) <- c('Chi Squared', "Cramer's V", 'p-value')

sum_tab_cat <- round(sum_tab_cat, 2)
