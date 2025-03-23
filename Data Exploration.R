# Read the dataset (you might have it as a .txt or .csv file)
# Assuming it's tab-separated or space-separated
data <- read.table("/Users/seamusconlon/Desktop/KUL/1.2/Generalized Linear Models [G0A18a]/Project/Data.txt", header = FALSE)

# Check structure
str(data)

# Optional: Rename columns for easier access
colnames(data) <- c("Y", "Age", "HA", "Gender")

# show the observation with y set to NA

data[is.na(data$Y), ]

# delete this row.

data <- data[!is.na(data$Y), ]

# Convert Y and Gender to factors
data$Y <- factor(data$Y, levels = c(0, 1), labels = c("No", "Yes"))
data$Gender <- factor(data$Gender, levels = c(0, 1), labels = c("Female", "Male"))
data$HA <- as.numeric(as.character(data$HA))
data$Age <- as.numeric(as.character(data$Age))

# show the observation with y set to NA

data[is.na(data$Y), ]

# delete this row.

data <- data[!is.na(data$Y), ]

str(data)

# Bar plot
library(ggplot2)
ggplot(data, aes(x = Y, fill = Y)) +
  geom_bar() +
  labs(title = "Flu Shot Status", x = "Flu Shot", y = "Count") +
  theme_minimal()

# Cross-tab
table(data$Gender, data$Y)

# Proportions
prop.table(table(data$Gender, data$Y), margin = 1)

# Bar plot
## Results: 
## Men get flu shots at a higher rate than women - 11.11% vs 19.23%
ggplot(data, aes(x = Gender, fill = Y)) +
  geom_bar(position = "fill") +
  labs(title = "Flu Shot Rate by Gender", x = "Gender", y = "Proportion") +
  theme_minimal()

# HA vs age with flu shot covered
## Results: 
## seems to be a negative relationship between HA and age. This relationship doesn't appear to be dependent on gender

ggplot(data, aes(x = Age, y = HA, color = Gender)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Health Awareness vs. Age by Gender",
    x = "Age",
    y = "Health Awareness"
  ) +
  theme_minimal()


# Age with logistic trend per gender - Seems to be a postive relationship between age and getting a flu shot
## Results: 
## After 75, people seem very likely to get a shot
## It looks like the gender difference between men and women grows in old age.

library(ggplot2)

ggplot(data, aes(x = Age, y = as.numeric(Y) - 1, color = Gender)) +
  geom_jitter(height = 0.1, alpha = 0.4) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(title = "Probability of Flu Shot by Age and Gender",
       x = "Age", y = "Flu Shot (0 = No, 1 = Yes)") +
  theme_minimal()

# HA with logistic trend per gender
## Results: 
## Strangely people with the lowest levels of HA seem to be the most likely to get flu shots
ggplot(data, aes(x = HA, y = as.numeric(Y) - 1, color = Gender)) +
  geom_jitter(height = 0.1, width = 0, alpha = 0.6) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(title = "Flu Shot vs. Health Awareness by Gender (Logistic Fit)", x = "HA", y = "Flu Shot") +
  theme_minimal()

#check hist.
## Results: 
## This further evidences the non-intuitive finding that people with lower health awareness levels typically get flue shots.

# however, maybe HA is a confounding variable here.
## Results: 
## We saw that there is a negative relationship between HA and AGE.
## We know that older people are more likely to get shots and younger people score higher on the HA variable

ggplot(data, aes(x = HA)) +
  # Histogram for the full dataset (density scale)
  geom_histogram(aes(y = ..density..), binwidth = 2, fill = "#377EB8", color = "white", alpha = 0.6) +
  # Density curves for each flu shot status
  geom_density(aes(color = Y, fill = Y), alpha = 0.3, size = 1.2) +
  labs(
    title = "Health Awareness Distribution with Flu Shot Status Density Curves",
    x = "Health Awareness Score",
    y = "Density"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("No" = "#E41A1C", "Yes" = "#4DAF4A")) +
  scale_color_manual(values = c("No" = "#E41A1C", "Yes" = "#4DAF4A"))


# I will test the EDA findings below:

# 1. Compare mean HA for people who did and didn't get the shot
## Results: 
## we see the No flu shot group has a mean of 60 vs the yes group with a mean of 50.
aggregate(HA ~ Y, data = data, summary)

# t-test 
## Results: 
## the t test yields a p value of 0.0002372, 
## giving more evidence that there is a statistically significant separation in the HA means of the No and Yes groups

t.test(HA ~ Y, data = data)

#  logistic regression of flu shot on HA
## Results: 
## HA and intercept terms are significant
## Null and residual deviance seem to be having some problems.

model_ha_only <- glm(Y ~ HA, data = data, family = "binomial")
summary(model_ha_only)

# 2. Show that age and HA are negatively correlated
## Results: 
## they have a moderate negative correlation of -.47.
## Since older people are more likely to get vaccinated, and they also tend to score lower on HA, 
## it’s possible that HA is not causally reducing vaccine uptake, but instead acting as a proxy for age.
## We you should control for age to assess the true relationship between HA and flu shot status.

cor(data$HA, data$Age)  # Pearson correlation
plot(data$Age, data$HA, main = "HA vs Age", xlab = "Age", ylab = "HA")

# 3. Show that Age is positively associated with flu shots

# Compare mean age by flu shot status
## Results:
## The mean of people taking the shot is 8 years higher than those who did not
aggregate(Age ~ Y, data = data, summary)

# t-test
## Results:
## The P value is highly significant giving evidence that there is a seperation in the mean age of those who did and did not get the flu shot 
t.test(Age ~ Y, data = data)

# Logistic regression: Y ~ Age
## Results:
## Age and intercept terms are significant
## again, the deviance seems off from the degrees of freedom suggesting there may be issues with the model.
model_age_only <- glm(Y ~ Age, data = data, family = "binomial")
summary(model_age_only)

# Model with both HA and Age
## Results: 
## when we include both terms in the model, they are both highly significant
## The effects of both terms are reduced in magnitude and significance though suggesting multi-colinearity.
## the residual deviance is even worse here.
model_full <- glm(Y ~ HA + Age, data = data, family = "binomial")
summary(model_full)

# 4. Testing Gender for completeness
## Results:
## No significant relationship revealed

model_all <- glm(Y ~ HA + Age + Gender, data = data, family = "binomial")
summary(model_all)

# Add residuals to the dataframe
data$pearson_resid <- residuals(model_full, type = "pearson")
data$deviance_resid <- residuals(model_full, type = "deviance")

# Get fitted values
data$fitted <- fitted(model_full)

# Pearson Residuals Plot
ggplot(data, aes(x = fitted, y = pearson_resid)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE, color = "#D95F02", fill = "#FEC44F") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Pearson Residuals vs Fitted Values (with LOWESS)",
    x = "Fitted Values",
    y = "Pearson Residuals"
  ) +
  theme_minimal()


# Deviance Residuals Plot
ggplot(data, aes(x = fitted, y = deviance_resid)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE, color = "#1B9E77", fill = "#A6D854") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Deviance Residuals vs Fitted Values (with LOWESS)",
    x = "Fitted Values",
    y = "Deviance Residuals"
  ) +
  theme_minimal()

## Results:
## overall the fitted value residuals look good with 0 being included in the CI at pretty much every step in the graph

## Graphing the residuals vs age and HA

ggplot(data, aes(x = HA, y = pearson_resid)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE, color = "#D95F02", fill = "#FEC44F") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Pearson Residuals vs. Health Awareness (HA)",
    x = "Health Awareness",
    y = "Pearson Residuals"
  ) +
  theme_minimal()

ggplot(data, aes(x = Age, y = pearson_resid)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE, color = "#1B9E77", fill = "#A6D854") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Pearson Residuals vs. Age",
    x = "Age",
    y = "Pearson Residuals"
  ) +
  theme_minimal()

ggplot(data, aes(x = HA, y = deviance_resid)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE, color = "#D95F02", fill = "#FEC44F") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Deviance Residuals vs. Health Awareness (HA)",
    x = "Health Awareness",
    y = "Deviance Residuals"
  ) +
  theme_minimal()

ggplot(data, aes(x = Age, y = deviance_resid)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE, color = "#1B9E77", fill = "#A6D854") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Deviance Residuals vs. Age",
    x = "Age",
    y = "Deviance Residuals"
  ) +
  theme_minimal()

## Results:
## The deviance residuals for both HA and Age seem to be relatively stable but deviate from 0 in the middle of the curve. Maybe a second order effect or an interaction term would help the model?

# Checking the other model assumptions:
# Cook's distance
plot(model_full, which = 4)  # Cook's distance

# Leverage values
plot(model_full, which = 5)  # Residuals vs Leverage

## Results:
## three outliers: 48 83 and 124 seem to be outliers for the model in both measures.

## overall commentary:
## There is multicollinearity among the predictors. 
## There are some issues with the deviance in the model and the multicollinearity between the predictors requires some kind of fix.
## I suggest fitting new models with these discoveries in mind and then retesting the goodness of fit and model assumptions.
## Perhaps it's also worth exploring the gender effect, but in my opinion I did not think this was significant.



