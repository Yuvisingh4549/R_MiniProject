# Load required packages
library(tidyverse)
library(caret)

# Load and combine data
red_wine <- read_delim("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv", delim = ";")
white_wine <- read_delim("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv", delim = ";")
wine_data <- bind_rows(red_wine %>% mutate(type = "red"), 
                       white_wine %>% mutate(type = "white")) %>%
  mutate(quality = factor(quality))

# Basic EDA
summary(wine_data)
ggplot(gather(wine_data, key, value, -type, -quality), aes(value)) + 
  geom_histogram() + facet_wrap(~key, scales = "free")

# Normalization function
normalize <- function(data, method) {
  preProcess(data %>% select(-type, -quality), method = method) %>% 
    predict(data)
}

# Apply normalizations
norm_data <- list(
  raw = wine_data,
  minmax = normalize(wine_data, "range"),
  zscore = normalize(wine_data, c("center", "scale"))
)

# Compare distributions
map(norm_data, ~ggplot(gather(.x, key, value, -type, -quality), aes(value)) + 
      geom_histogram() + facet_wrap(~key, scales = "free"))

# Evaluate model performance
set.seed(123)
train_index <- createDataPartition(wine_data$quality, p = 0.8, list = FALSE)
train <- wine_data[train_index, ]
test <- wine_data[-train_index, ]

accuracies <- map_dbl(norm_data, ~{
  model <- train(quality ~ . -type, data = .x[train_index, ], method = "rf")
  predict(model, .x[-train_index, ]) %>% 
    confusionMatrix(test$quality) %>% 
    .$overall["Accuracy"]
})

print(accuracies)