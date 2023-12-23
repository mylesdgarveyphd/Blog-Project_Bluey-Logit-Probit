# Install and load required packages
install.packages(c("ggplot2", "MASS"))
library(ggplot2)
library(MASS)

# Read the data from "bluey_data.csv"
data <- read.csv("bluey_data.csv")

# Rename columns for simplicity
colnames(data) <- c("Entry", "Decision", "Snack", "Anger", "Meltdown")

# Convert categorical variables to factors
data$Decision <- as.factor(data$Decision)
data$Snack <- as.factor(data$Snack)

##########Linear Model
# Fit linear probability model
linear_model <- lm(Meltdown ~ Decision + Snack + Anger, data = data)
# Predicted values for linear model
data$linear_pred <- predict(linear_model)
#Summary
summary(linear_model)
# Test Instance
new_data <- data.frame(Decision = "Turned_on_Bluey", Snack = "No", Anger = 0)
# Predict the probability using the linear model
predict(linear_model, newdata = new_data)


##########Logit Model
# Fit logit model
logit_model <- glm(Meltdown ~ Decision + Snack + Anger, data = data, family = "binomial")
data$logit_pred <- predict(logit_model, type = "response")
summary(logit_model)
# Test Instance
new_data <- data.frame(Decision = "Turned_on_Bluey", Snack = "No", Anger = 0)
# Predict the probability using the linear model
predict(logit_model, newdata = new_data,type="response")


# Predicted values for linear model
probit_model <- glm(Meltdown ~ Decision + Snack + Anger, data = data, family = binomial(link = "probit"))
data$probit_pred <- predict(probit_model, type = "response")
summary(probit_model)

# Test Instance
new_data <- data.frame(Decision = "Turned_on_Bluey", Snack = "No", Anger = 0)
# Predict the probability using the linear model
predict(probit_model, newdata = new_data,type="response")


################################Plotting
# Reshape data using dplyr
data_long <- data %>%
  gather(key = "Model", value = "Prediction", -c(Entry, Decision, Snack, Anger, Meltdown)) %>%
  mutate(Model = factor(Model, levels = c("linear_pred", "logit_pred", "probit_pred"),
                        labels = c("Linear Prob", "Logit", "Probit")))

# Create plots with side-by-side violins
# 4. Violin plots grouped by "Snack"
ggplot(data_long, aes(x = factor(Snack), y = Prediction, fill = Model)) +
  geom_violin(position = position_dodge(width = 0.8), show.legend = FALSE) +
  labs(title = "Fitted Probabilities by Snack",
       x = "Snack",
       y = "Predicted Meltdown Probability") +
  scale_fill_manual(values = c("Linear Prob" = "#66c2a5", "Logit" = "#fc8d62", "Probit" = "#8da0cb")) +
  theme_minimal()+
  coord_cartesian(ylim=c(0,1))

# 5. Violin plots grouped by "Decision"
ggplot(data_long, aes(x = factor(Decision), y = Prediction, fill = Model)) +
  geom_violin(position = position_dodge(width = 0.8), show.legend = FALSE) +
  labs(title = "Fitted Probabilities by Decision",
       x = "Decision",
       y = "Predicted Meltdown Probability") +
  scale_fill_manual(values = c("Linear Prob" = "#66c2a5", "Logit" = "#fc8d62", "Probit" = "#8da0cb")) +
  theme_minimal()

# Data wrangling with dplyr
data_long <- data %>%
  mutate(Curve = interaction(Decision, Snack, drop = TRUE)) %>%
  gather(linear_pred:probit_pred,key = "Model", value = "Prediction")


# Create scatter plot between anger and prob
ggplot(data_long, aes(x = Anger, y = Prediction, color = Model)) +
  geom_point(size = 1) +
  geom_smooth() +
  labs(title = "Fitted Probabilities vs. Anger",
       x = "Anger",
       y = "Predicted Meltdown Probability") +
  scale_color_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f", "#1f78b4", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00")) +
  coord_cartesian(ylim=c(0.35,.65))+
  theme_minimal()


# Create scatter plot with facets based on Snack and Decision Combinations
ggplot(data_long, aes(x = Anger, y = Prediction, color = Model)) +
  geom_point(size = 1) +
  geom_smooth() +
  labs(title = "Fitted Probabilities by Decision, Snack, and Anger",
       x = "Anger",
       y = "Predicted Meltdown Probability") +
  scale_color_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f", "#1f78b4", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00")) +
  coord_cartesian(ylim=c(0.35,.75))+
  facet_wrap(~Snack+Decision, scales = "free_y") +
  theme_minimal()
