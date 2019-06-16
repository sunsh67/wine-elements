

#load libraries 
library(tidyverse)
setwd("/Users/sun.sh/Projects/data analysis/final_project")

#obtain data frame
read.files <- function(){
  df_red <- read.csv(file = "winequality-red.csv", head = TRUE, sep= ";") 
  df_white <- read.csv(file = "winequality-white.csv", head = TRUE, sep= ";") 
  df_red$category <- "red"
  df_white$category <- "white"
  df <- rbind(df_white, df_red)
  df %>% return()
}
df <- read.files()

#Tidy Data
#utilities
normalize <- function(x){
  (x - mean(x))/sd(x)
}

df <- df %>% 
  mutate(
  category = as.factor(category),
  chlorides = chlorides %>% round(2),
  density = density %>% round(2)
) %>% distinct() %>%drop_na()

df %>% glimpse() %>% summary()

cols <- df %>% select(-category) %>% names()

#features histogram with density,rug
plots.hists <- function(col, df, bins = 20){
  mean <- eval(parse(text = paste("df$", col, sep = ""))) %>% mean()
  p1 <- ggplot(df, aes_string(col)) + 
    geom_histogram(aes(y = ..density..), bins = bins, 
                   alpha = 0.3, color = 'blue') +
    geom_density(size = 1, alpha=0.2) +
    geom_rug(alpha=0.2, color='red') +
    geom_vline(xintercept = mean , color="green") +
    facet_wrap(~ category) + 
    theme_classic() +
    labs(title=str_c("Histogram, density and rug function \n for ", col, " and mean"), 
         x=str_c("value of ", col))
    p1 %>% print()
}
cols %>% walk(plots.hists, df)
#not symmetric: density, quality

#Violin plots
plots.volin <- function(col, df){
  p1 <- df %>% 
    ggplot(aes_string('category', col)) + 
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), 
                fill = 'orange', alpha = 0.3, size = 1.0) +
    theme_light()
  p1 %>% print()
}
cols %>% walk(plots.volin, df)

#dots plots
plots.dot <- function(col, df, bins = 60){
  binwidth <- (max(df[, col]) - min(df[, col])) / bins
  p1 <- ggplot(df, aes_string(col)) + 
    geom_dotplot(dotsize = 0.5, method = "histodot", binwidth = binwidth) +
    facet_wrap( ~ category)
  p1 %>% print()
}
cols %>% walk(plots.dot, df)

#explore trends
plots.relations <- function(data, x_col, y_col) {
  plt <- data %>% ggplot(aes_string(x = x_col, y = y_col)) + 
    geom_jitter(alpha = 0.3) + 
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~ category) + 
    ggtitle(str_c(y_col, " vs ", x_col), 
            subtitle = str_c("Row count:", data %>% nrow())) +
    theme_light()
    plt %>% print()
}

#pick residual.sugar 
cols_residual.sugar <- df %>% select(-category, -residual.sugar) %>% names()
cols_residual.sugar %>% walk(plots.relations, data = df, y_col = "residual.sugar")

#pick quality
cols_quality <- df %>% select(-category, -quality) %>% names()
cols_quality %>% walk(plots.relations, data = df, y_col = "quality")

#pick pH
cols_pH <- df %>% select(-category, -pH) %>% names()
cols_pH %>% walk(plots.relations, data = df, y_col = "pH")

#Hypothesis test 
df_red <- df %>% filter(category == "red")
df_red %>% summary()
df_red$sulphates %>% sd() %>% round(2)
df_white <- df %>% filter(category == "white")
df_white %>% summary()
df_white$sulphates %>% sd() %>% round(2)
#Two sided test 
t.test(df_red$residual.sugar, df_white$residual.sugar, conf.level = 0.95, alternative="two.sided")
#factor tests
t.test( pH ~ category, data = df)
t.test( alcohol ~ category, data = df) 
t.test( total.sulfur.dioxide ~ category, data = df) 
t.test( sulphates ~ category, data = df) 

#Regression Analysis
#multiRegression for red wine 
red.quality <- df_red %>% 
  select(volatile.acidity, total.sulfur.dioxide, alcohol, sulphates, quality, category) %>%
  mutate(volatile.acidity = normalize(volatile.acidity),
         total.sulfur.dioxide = normalize(total.sulfur.dioxide),
         alcohol = normalize(alcohol),
         sulphates = normalize(sulphates),
         scaled.quality = log(quality)) 

red.mod <- lm(scaled.quality ~ volatile.acidity + total.sulfur.dioxide + alcohol + sulphates,
              data = red.quality)
red.mod %>% summary()
red.mod %>% confint()

red.quality <- red.quality %>%
  mutate(score = predict(red.mod,  data = red.quality),
         resids = scaled.quality - score,
         predict.quality = exp(score)) 

red.quality %>% summary()
cols_red.quality <- red.quality %>% select(-quality, -category, -scaled.quality) %>% names()
cols_red.quality %>% walk(plots.relations, data = red.quality, y = "scaled.quality")

plot.resids <- function(df){
  p1 <- df %>% 
    ggplot(aes(resids, ..density..)) + 
    geom_histogram(bins = 10, alpha = 0.3, color = 'blue') +
    geom_density(size = 1) +
    labs(title="Histogram and density function \n for residuals", x="Residual value")
  
  p2 <- df %>% 
    ggplot(aes(sample = resids)) + 
    geom_qq() + 
    labs(title="Quantile-quantile Normal plot \n of residuals")
  p1 %>% print()
  p2 %>% print()
}

red.quality %>% plot.resids()

scatter.resids <- function(df){
  df %>% 
    ggplot(aes(score, resids)) + 
    geom_point(size = 2) +
    geom_smooth(size = 1, color = 'red', method="loess") +
    labs(title="Residuals vs fitted values", x="Fitted values", y="Residuals")
}

red.quality %>% scatter.resids()


#MultiClassification for predict category 
set.seed(1222)
df <- df %>% 
  select(pH, total.sulfur.dioxide, sulphates, residual.sugar, category) %>%
  mutate_at(vars(-category), normalize) %>%
  distinct()
df_train <- df %>% sample_frac(0.75)
df_test <- df %>% setdiff(df_train)
nrow(df_train) + nrow(df_test) == nrow(df)

category.mod <- glm(category ~ ., data = df_train,
                    family = binomial())
category.mod %>% summary()
category.mod %>% confint()

df_test <- df_test %>% 
  mutate( score = predict(category.mod, newdata = df_test),
          score.prob = exp(score)/ (1 + exp(score)),
          score.predict = if_else(score.prob > 0.50, "white", "red") %>% 
            as.factor())

caret::confusionMatrix(data = df_test$score.pred, 
                       reference = df_test$category, 
                       mode = "prec_recall")




