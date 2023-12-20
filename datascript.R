rm(list = ls())
data <- read.csv("")
library(lubridate)
library(stringr)
library(dplyr)
library(rpart)
library(rpart.plot)
library(caret)
library(glmnet)
library(ggplot2)
library(class)
library(randomForest)
library(randomForestExplainer)
library(tidyr)
library(tidyverse)
library(tidytext)
######## CLEANING ############
clean_data <- data
# change price to remove '$' and make numeric
clean_data <- clean_data[grepl("\\$", clean_data$Price) & !(grepl("NULL", clean_data$Price) | grepl("^CA|^A", clean_data$Price)), ]
clean_data <- clean_data %>%
  mutate(
    Price = as.numeric(stringr::str_replace(Price, "\\$", ""))
  )
# remove duplicated rows by link
clean_data <- clean_data[!duplicated(clean_data$Link), ]
# replace "NULL" with "None" for brands 
clean_data$Brand <- ifelse(clean_data$Brand == "NULL", "None", clean_data$Brand)
# remove values where the condition is NULL
clean_data <- clean_data[clean_data$Condition != "NULL", ]
# extract number of items sold and rating from "Sold.and.Reviews"
clean_data <- clean_data %>%
  mutate(
    numReviews = as.numeric(stringr::str_extract(Sold.and.Reviews, "\\b(\\d+)\\b")),
    rating_text = stringr::str_extract(Sold.and.Reviews, "\\d+(\\.\\d+)? out of 5 stars"),
    rating = as.numeric(stringr::str_extract(rating_text, "\\d+(\\.\\d+)?"))
  ) %>%
  select(-Sold.and.Reviews,-rating_text)
# takes the last word from the title and makes it the type
clean_data <- clean_data %>%
  mutate(
    type = stringr::str_extract(tolower(Title), "\\b\\w+$")
  )
# some types are plural, removing the letter 's' from the end
clean_data <- clean_data %>%
  mutate(type = str_replace(type, "s$", ""))
# removes values where 'Amt Sold' is wrong (doesn't have the word 'sold') then removes the words to make it numeric
clean_data <- clean_data %>%
  filter(stringr::str_detect(Amt.Sold, "sold")) %>%
  mutate(
    Amt.Sold = as.numeric(stringr::str_replace(Amt.Sold, "sold", ""))
  )
# change how long ago the item was listed into numeric
clean_data <- clean_data %>%
  mutate(
    Time.Listed = case_when(
      str_detect(Time.Listed, "DAYS?") ~ as.numeric(str_extract(Time.Listed, "\\d+")),
      str_detect(Time.Listed, "HOURS?") ~ as.numeric(str_extract(Time.Listed, "\\d+")) / 24,
      str_detect(Time.Listed, "MINUTES?") ~ as.numeric(str_extract(Time.Listed, "\\d+")) / (24 * 60)
    )
  )
clean_data <- na.omit(clean_data)
# changing NULL values to = 0 in likes
clean_data <- clean_data %>%
  mutate(
    Like = as.numeric(stringr::str_replace_all(Like, "\\+", "")),
    Like = coalesce(Like, 0)
  )
# changing free shipping to binary values 
clean_data <- clean_data %>%
  mutate(
    Free.Shipping = ifelse(Free.Shipping == "Free domestic shipping", 1, 0)
  )
# rename style columns and remove all punctiation, making them only the first word
clean_data <- clean_data %>%
  rename(style1 = Color, style2 = Material, style3 = Style) %>%
  mutate(
    style1 = str_extract(style1, "\\b\\w+\\b"),
    style2 = str_extract(style2, "\\b\\w+\\b"),
    style3 = str_extract(style3, "\\b\\w+\\b")
  )
# if sizes are multiple, dropshipping
clean_data <- clean_data %>%
  mutate(drop_shipping = ifelse(Size == "Multiple sizes", 1, 0))
# change discount into numeric
clean_data <- clean_data %>%
  mutate(Discount = ifelse(Discount == "NULL", 0, gsub("%", "", str_extract(Discount, "\\d+%"))))
# change number of bags into numeric
clean_data <- clean_data %>%
  mutate(In.Bags = ifelse(In.Bags == "NULL", 0, as.numeric(str_extract(In.Bags, "\\d+"))))
# extracts how many hashtags are in the description
clean_data$hashtags <- str_extract_all(clean_data$Description, "#\\w+")
clean_data$hashtags_count <- sapply(clean_data$hashtags, function(x) length(x))
clean_data <- clean_data[, !names(clean_data) %in% c("hashtags")]
# format style1, 2, and 3 to be binary values
clean_data_long <- clean_data %>%
  pivot_longer(cols = c(style1, style2, style3), names_to = "style_source", values_to = "style_value") %>%
  mutate(style_value = ifelse(is.na(style_value) | style_value == "NULL", "NoStyle", style_value)) %>%
  select(-style_source) %>%
  distinct()
# creating a new column 'present' and initialize with 1, duplicating rows
clean_data_long <- mutate(clean_data_long, present = 1)
# removing the duplicate rows and making it wide format
clean_data_wide <- spread(clean_data_long, key = "style_value", value = "present", fill = 0)
# combining the new style columns with the original data
clean_data_final <- bind_cols(clean_data, clean_data_wide)
# columns were duplicated, so filtering ones that don't end in ...number
style_columns <- grep("^[^\\.]+(?:(?:\\.\\d+)+)?$", names(clean_data_final), value = TRUE)
# selecting only the style columns from clean_data_final in order to merge
style_data <- clean_data_final %>%
  select(all_of(style_columns))
# appending style columns to clean_data
clean_data <- bind_cols(clean_data, style_data)
# removing categorical style values
clean_data <- clean_data %>%
  select(-starts_with("style"))
## remove price outliers
z_scores <- scale(clean_data$Price)
threshold <- 3
outliers <- which(abs(z_scores) > threshold)
clean_data <- clean_data[-outliers, ]
######################################################
## excluding vaariables and making dummies
exclude_variables <- c("Link", "Description", "Title", "Recent.Review1", "Date", "Time.Listed", "Datetime", "Activity")
selected_data <- clean_data %>%
  select(-one_of(exclude_variables))
dummies <- dummyVars(" ~ .", data = selected_data)
dummies$terms
selected_data <- data.frame(predict(dummies, newdata = selected_data))
#########SPLITTING#######
set.seed(123)
trainIdx <- createDataPartition(selected_data$Price,  p = 0.8, list = FALSE)
dataTrain <- selected_data[trainIdx, ]
dataTest <- selected_data[-trainIdx, ]
#########MODELS###########
### REGRESSION#####
model <- lm(Price ~ ., data = selected_data)
summary(model)
########TREE#############
tree_model <- rpart(Price ~ ., data = dataTrain)
regression_pred <- predict(tree_model, newdata = dataTest)
predictions_df <- data.frame(Actual = dataTest$Price, Predicted = regression_pred)
head(predictions_df)
mse <- sqrt(mean((dataTest$Price - regression_pred)^2))
cat("Root Mean Squared Error Tree:", mse, "\n")
prp(tree_model, extra = 1, varlen = 0, faclen = 0, cex = 0.8)
#######RANDOM FOREST############
rf_model <- randomForest(Price ~ ., data = dataTrain, ntree = 10)
rf_pred <- predict(rf_model, newdata = dataTest)
mse <- sqrt(mean((dataTest$Price - rf_pred)^2))
cat("Root Mean Squared Error Forest:", mse, "\n")
predictions_df_forest <- data.frame(Actual = dataTest$Price, Predicted = rf_pred)
head(predictions_df_forest)
##############RIDGE##############
X <- as.matrix(selected_data[, -which(names(selected_data) == "Price")])
y <- selected_data$Price
X_scaled <- scale(X)

set.seed(123)
trainIdx <- createDataPartition(y, p = 0.8, list = FALSE)
dataTrainX <- X_scaled[trainIdx, ]
dataTestX <- X_scaled[-trainIdx, ]
dataTrainY <- y[trainIdx]
dataTestY <- y[-trainIdx]

ridge_model <- cv.glmnet(dataTrainX, dataTrainY, alpha = 0, nfolds = 20)
best_lambda_ridge <- ridge_model$lambda.min
ridge_pred <- predict(ridge_model, s = best_lambda_ridge, newx = dataTestX)

coef <- as.matrix(coef(ridge_model, s = best_lambda_ridge))
coef <- coef[coef != 0, , drop = FALSE]
coef_df <- data.frame(
  Feature = rownames(coef),
  Coefficient = as.vector(coef)
)
ggplot(coef_df, aes(x = reorder(Feature, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Ridge Regression Coefficients",
       x = "Features",
       y = "Coefficients")
rmse_ridge <- sqrt(mean((ridge_pred - dataTestY)^2))
cat("Ridge Regression Root Mean Squared Error:", rmse_ridge, "\n")
predictions_df_ridge <- data.frame(Actual = dataTestY, Predicted = ridge_pred)
head(predictions_df_ridge)
#### EXPORTING TO CSV ##############
write.csv(predictions_df_forest, "predictions_df_forestAAP.csv", row.names = FALSE)
write.csv(selected_data, "clean_dataAAP2.csv", row.names = FALSE)
###########PLOTTING AND GRAPHS############
#### line graph actual vs predicted 
ggplot(predictions_df, aes(x = 1:nrow(predictions_df))) +
  geom_line(aes(y = Predicted, color = "Predicted"), linewidth = 1) +
  geom_line(aes(y = Actual, color = "Actual"), linewidth = 1) +
  labs(title = "Actual vs Predicted Line Graph: RF", x = "Row", y = "Value") +
  scale_color_manual(values = c("black", "red"), name = "Group") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 16, face = "bold")
  )
correlation <- cor(predictions_df_forest$Actual, predictions_df_forest$Predicted)
correlation
### average price for dropshipping vs not
dropship_1 <- clean_data[clean_data$drop_shipping == 1, ]
dropship_0 <- clean_data[clean_data$drop_shipping == 0, ]
avg_price_1 <- mean(dropship_1$Price, na.rm = TRUE)
avg_price_0 <- mean(dropship_0$Price, na.rm = TRUE)
plot_data <- data.frame(
  Dropshipping = c("Dropshipping 1", "Dropshipping 0"),
  Average_Price = c(avg_price_1, avg_price_0)
)
ggplot(plot_data, aes(x = Dropshipping, y = Average_Price, fill = "black")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5, color = "red") +
  labs(title = "Average Price Comparison for Dropshipping",
       x = "Dropshipping",
       y = "Average Price") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
  )
### average price for free shipping vs not
free_ship_1 <- clean_data[clean_data$Free.Shipping == 1, ]
free_ship_0 <- clean_data[clean_data$Free.Shipping == 0, ]
avg_price_1 <- mean(free_ship_1$Price, na.rm = TRUE)
avg_price_0 <- mean(free_ship_0$Price, na.rm = TRUE)
plot_data_free_ship <- data.frame(
  FreeShipping = c("Free Shipping 1", "Free Shipping 0"),
  Average_Price = c(avg_price_1, avg_price_0)
)
ggplot(plot_data_free_ship, aes(x = FreeShipping, y = Average_Price, fill = "black")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5, color = "red") +
  labs(title = "Average Price Comparison for Free Shipping",
       x = "Free Shipping",
       y = "Average Price") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
  )
#### average price by discount
clean_data$Discount <- as.factor(clean_data$Discount)
discount_data <- clean_data %>%
  group_by(Discount) %>%
  summarise(Average_Price = mean(Price, na.rm = TRUE))
ggplot(discount_data, aes(x = Discount, y = Average_Price, fill = Discount)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "Average Full Price by Discount Level",
       x = "Discount Level",
       y = "Average Price") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
### average price by condition
clean_data$Condition <- as.factor(clean_data$Condition)
condition_data <- clean_data %>%
  group_by(Condition) %>%
  summarise(Average_Price = mean(Price, na.rm = TRUE))
ggplot(condition_data, aes(x = Condition, y = Average_Price, fill = Condition)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "Average Price by Condition",
       x = "Condition",
       y = "Average Price") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )







