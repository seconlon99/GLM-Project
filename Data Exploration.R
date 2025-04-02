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
# Task 2: Fit of the model with different link functions

# Fit a logistic regression model using the default logit link
model_logit <- glm(Y ~ HA + Age + Gender, data = data, family = binomial(link = "logit"))
summary(model_logit)

# Fit a model using the probit link.
# The probit link assumes a normal distribution for the underlying latent variable.
model_probit <- glm(Y ~ HA + Age + Gender, data = data, family = binomial(link = "probit"))
summary(model_probit)

# Fit a model using the complementary log–log (cloglog) link.
# This link is useful if the event probabilities are very small or very large.
model_cloglog <- glm(Y ~ HA + Age + Gender, data = data, family = binomial(link = "cloglog"))
summary(model_cloglog)

# Compare the models using AIC (lower AIC indicates a better trade-off between fit and complexity)
AIC_values <- AIC(model_logit, model_probit, model_cloglog)
print(AIC_values)

# The slightly lower AIC for the cloglog model suggests a marginally better fit, 
# but the differences are very small. This indicates that the choice of link function 
# does not dramatically alter the overall fit in this case.

# The flu shot uptake ("Yes") is relatively low.
# This might be an argument in favour of cloglog in our case as it might better
# accommodate the asymmetry in the distribution.

# However, since differences in fit are negligible and we do not have a strong
# theoretical argument for the use of cloglog, it might be wise to choose
# logit for interpretability.


# Task 3: Goodness of fit

# Define the Hosmer-Lemeshow test function
hosmerlem <- function(y, yhat, g = 10) {
  fcutyhat <- cut(yhat, breaks = quantile(yhat, probs = seq(0, 1, 1/g)), include.lowest = TRUE)
  obs <- xtabs(cbind(1 - y, y) ~ fcutyhat)
  expect <- xtabs(cbind(1 - yhat, yhat) ~ fcutyhat)
  chisq <- sum((obs - expect)^2 / expect)
  P <- 1 - pchisq(chisq, g - 2)
  return(list(chisq = chisq, p.value = P))
}

# Convert the factor response to a numeric variable (0 for "No", 1 for "Yes")
y_numeric <- ifelse(data$Y == "Yes", 1, 0)

# Final logit model (using the logit link)
model_logit <- glm(Y ~ HA + Age + Gender, data = data, family = binomial(link = "logit"))
summary(model_logit)

# Checking the Interaction effects
#result: Only HA is significant (p = 0.029)
interaction_model <- glm(Y ~ Age * Gender + HA * Gender, data = data, family = "binomial")
summary(interaction_model)

# Checking for Non-linear terms
#Result: None of the quadratic terlms are significant (I(age^2), p=0.710 and I(HA^2), p=0.244)
nonlinear_model <- glm(Y ~ Age + I(Age^2) + HA + I(HA^2) + Gender, data = data, family = "binomial")
summary(nonlinear_model)

# Compare models using AIC
#Conclusion: The simple Logit model has the best AIC 
AIC(model_logit, interaction_model, nonlinear_model)

# ROC Curve and AUC
#conclusion: AUC = 0.8224, which indicates good classification performance.
#The ROC-curve shows a decent balance of sensitivity and specificity.
library(pROC)
roc_obj <- roc(y_numeric, fitted(model_logit))
plot(roc_obj, col = "#2C7BB6", main = "ROC Curve for Logit Model")
auc(roc_obj)

# Perform the Hosmer-Lemeshow test on the final model
#conclusion: The null hypothesis of the Hosmer-Lemeshow test is that the model fits the data well.
#Your result (p = 0.458) indicates no significant lack of fit. The model fits the data well.
hl_test <- hosmerlem(y = y_numeric, yhat = fitted(model_logit), g = 10)

# Display the test statistic and p-value in a data frame
#Result: chisq = 7.75 is well within the acceptable range for a good-fitting model with 8 degrees of freedom.
hl_test_df <- data.frame(Chisq = hl_test$chisq, pvalue = hl_test$p.value)
print(hl_test_df)

#Task 4#
#Interpretation of Coefficients

# Fit the final logistic regression model
model_logit <- glm(Y ~ HA + Age + Gender, data = data, family = binomial(link = "logit"))

# Output model summary
summary(model_logit)

## Model Summary and Interpretation##

# Coefficients and their meaning:
# (Intercept): -1.177 (p = 0.693) Not statistically significant.
# HA: -0.099 (p = 0.003)   Significant, negative effect on flu shot uptake.
# Age: +0.073 (p = 0.017)  Significant, positive effect on flu shot uptake.
# Gender (Male): +0.434 (p = 0.406)  Not statistically significant

# Interpretation of each coefficient:

# 1. Health Awareness (HA):
# A one-unit increase in HA leads to a decrease in the log-odds of getting a flu shot by 0.099.
# The odds ratio is exp(-0.099) ≈ 0.91 , about 9% decrease in odds of vaccination.
# but, since HA is negatively correlated with Age (r = -0.47),
# and younger individuals are less likely to get vaccinated, this effect may be confounded.
# So the HA effect should be interpreted cautiously.

# 2. Age:
# Each additional year of age increases the log-odds of vaccination by 0.073.
# Odds ratio is exp(0.073) ≈ 1.076 → about 7.6% increase in odds per year.
# This result aligns with EDA and common expectations.

# 3. Gender (Male):
# The coefficient is not statistically significant (p = 0.406),
# suggesting gender does not have a meaningful effect on flu shot behavior after adjusting for Age and HA.

##Model Fit Summary##

# Chosen model: logit link logistic regression
# AIC = 113.09 → better than models with interaction/quadratic terms

# Goodness of fit:
# - Hosmer–Lemeshow test: p = 0.458  No significant, lack of fit
# - AUC = 0.822 → Good classification performance
# - Residuals and influence measures show no concerning outliers

## Final Notes ##

# - Age is the most important predictor.
# - HA is statistically significant, but may be confounded by Age.
# - Gender is not a significant predictor.
# - The model is stable and fits the data well.

# --Limitations and Extensions --

# While HA and Age are both statistically significant, their moderate negative correlation (r = -0.47)
# suggests potential multicollinearity. Including both in the model may dilute each other's effect.
# Additionally, the proportion of vaccinated individuals is low in the dataset.
# Finally, although Gender is not significant here, future models could explore gender-specific behaviors
# using interaction terms in larger or more balanced samples.



