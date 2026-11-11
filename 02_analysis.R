# HSCI 410 Assignment 3 - Apostoli Kasidoulis
# Open hsci410_volunteering.Rproj before running this file.
# Ordinal fits the FIVE ordered loneliness categories retained
# car provides vif()
# Install once if needed: install.packages(c("ordinal", "car"))
library(ordinal)

################ LOAD THE PROCESSED DATA ################
load("data/processed/cscs_analytic.RData")
# Save the diagnostic Console output
sink("data/processed/a3_diagnostics.txt", split = TRUE)
# Use readable units: age per 10 years and extraversion per 5 points.
analytic_data$age10 <- analytic_data$age / 10
analytic_data$extraversion5 <- analytic_data$extraversion / 5
nrow(analytic_data)

################ STEP 1: UNADJUSTED MODEL ################
# * includes volunteering, retirement, and their interaction.
crude <- clm(loneliness ~ volunteering * retired, data = analytic_data)
summary(crude)

################ STEP 2: ADJUSTED MODEL ################
# Same complete-case sample
adjusted <- clm(loneliness ~ volunteering * retired + age10 + gender +
  relationship + income + employed + university + physical_health +
  extraversion5, data = analytic_data)
summary(adjusted)
# Compare with a model without the interaction to test retirement moderation.
no_interaction <- update(adjusted, . ~ . - volunteering:retired)
anova(no_interaction, adjusted)

################ STEP 3: COMPARE THE RESULTS ################
# exp() converts each predictor coefficient into an odds ratio (OR).
# OR < 1 means lower odds of being above a loneliness-frequency threshold.
exp(crude$beta)
exp(adjusted$beta)
# The volunteering coefficient above is for NOT retired respondents.
# Changing the retirement reference gives the volunteering OR for retired people.
# This changes the displayed comparison, not the fitted model or sample.
retired_data <- analytic_data
retired_data$retired <- relevel(retired_data$retired, ref = "Retired")
crude_retired <- update(crude, data = retired_data)
adjusted_retired <- update(adjusted, data = retired_data)
summary(crude_retired)
summary(adjusted_retired)

################ STEP 4: DIAGNOSTICS ################
# Proportional odds: check whether one OR is adequate across all four thresholds.
nominal_test(crude)
nominal_test(adjusted)
# Linearity: check whether curved age/extraversion terms improve model fit.
quadratic <- update(adjusted, . ~ . + I(age10^2) + I(extraversion5^2))
anova(adjusted, quadratic)
AIC(adjusted, quadratic)
# Use numeric category ranks ONLY to define four binary diagnostic outcomes.
analytic_data$loneliness_rank <- as.numeric(analytic_data$loneliness)
check2 <- glm(I(loneliness_rank > 2) ~ volunteering * retired + age10 + gender +
  relationship + income + employed + university + physical_health +
  extraversion5, data = analytic_data, family = binomial)
check1 <- update(check2, I(loneliness_rank > 1) ~ .)
check3 <- update(check2, I(loneliness_rank > 3) ~ .)
check4 <- update(check2, I(loneliness_rank > 4) ~ .)
# These binary models are diagnostic screens, not the primary regression.
# VIF checks collinearity
car::vif(check2)
# Predict all five category probabilities from the adjusted ORDINAL model.
prediction_data <- analytic_data[, names(analytic_data) != "loneliness"]
probabilities <- predict(adjusted, newdata = prediction_data, type = "prob")$fit
p1 <- rowSums(probabilities[, 2:5])
p2 <- rowSums(probabilities[, 3:5])
p3 <- rowSums(probabilities[, 4:5])
p4 <- probabilities[, 5]
y1 <- analytic_data$loneliness_rank > 1
y2 <- analytic_data$loneliness_rank > 2
y3 <- analytic_data$loneliness_rank > 3
y4 <- analytic_data$loneliness_rank > 4
# AUC measures how well predicted probabilities rank higher-loneliness cases.
# Base R rank() gives the same AUC as pROC, including half-credit for ties.
# Formula: (sum of case ranks - minimum possible case-rank sum)/(cases*noncases).
auc1 <- (sum(rank(p1)[y1])-sum(y1)*(sum(y1)+1)/2)/(sum(y1)*sum(!y1))
auc2 <- (sum(rank(p2)[y2])-sum(y2)*(sum(y2)+1)/2)/(sum(y2)*sum(!y2))
auc3 <- (sum(rank(p3)[y3])-sum(y3)*(sum(y3)+1)/2)/(sum(y3)*sum(!y3))
auc4 <- (sum(rank(p4)[y4])-sum(y4)*(sum(y4)+1)/2)/(sum(y4)*sum(!y4))
c(auc1, auc2, auc3, auc4)
# Calibration: divide predictions into deciles, then compare predicted and observed means.
d1 <- cut(p1, quantile(p1, seq(0,1,0.1)), include.lowest = TRUE)
d2 <- cut(p2, quantile(p2, seq(0,1,0.1)), include.lowest = TRUE)
d3 <- cut(p3, quantile(p3, seq(0,1,0.1)), include.lowest = TRUE)
d4 <- cut(p4, quantile(p4, seq(0,1,0.1)), include.lowest = TRUE)
cal1 <- aggregate(cbind(predicted=p1, observed=y1), list(decile=d1), mean)
cal2 <- aggregate(cbind(predicted=p2, observed=y2), list(decile=d2), mean)
cal3 <- aggregate(cbind(predicted=p3, observed=y3), list(decile=d3), mean)
cal4 <- aggregate(cbind(predicted=p4, observed=y4), list(decile=d4), mean)
print(cal1); print(cal2); print(cal3); print(cal4)
# Save diagnostic plots. The dashed calibration line represents observed = predicted.
pdf("data/processed/a3_diagnostic_plots.pdf", width=9, height=7)
par(mfrow=c(2,2))
plot(cal1$predicted,cal1$observed,xlim=c(0,1),ylim=c(0,1),main="Above category 1",xlab="Predicted",ylab="Observed"); abline(0,1,lty=2)
plot(cal2$predicted,cal2$observed,xlim=c(0,1),ylim=c(0,1),main="Above category 2",xlab="Predicted",ylab="Observed"); abline(0,1,lty=2)
plot(cal3$predicted,cal3$observed,xlim=c(0,1),ylim=c(0,1),main="Above category 3",xlab="Predicted",ylab="Observed"); abline(0,1,lty=2)
plot(cal4$predicted,cal4$observed,xlim=c(0,1),ylim=c(0,1),main="Above category 4",xlab="Predicted",ylab="Observed"); abline(0,1,lty=2)
# Cook's distance screens use the four binary fits, not the ordinal model itself.
cook1 <- cooks.distance(check1)
cook2 <- cooks.distance(check2)
cook3 <- cooks.distance(check3)
cook4 <- cooks.distance(check4)
plot(cook1,type="h",main="Cook screen above category 1"); abline(h=4/nrow(analytic_data),lty=2)
plot(cook2,type="h",main="Cook screen above category 2"); abline(h=4/nrow(analytic_data),lty=2)
plot(cook3,type="h",main="Cook screen above category 3"); abline(h=4/nrow(analytic_data),lty=2)
plot(cook4,type="h",main="Cook screen above category 4"); abline(h=4/nrow(analytic_data),lty=2)
dev.off()
c(max(cook1),max(cook2),max(cook3),max(cook4))
flagged <- cook1>4/nrow(analytic_data) | cook2>4/nrow(analytic_data) |
           cook3>4/nrow(analytic_data) | cook4>4/nrow(analytic_data)
sum(flagged)
# Check separation: look for boundary probabilities and very large coefficients/SEs.
range(probabilities)
summary(check1); summary(check2); summary(check3); summary(check4)
adjusted$convergence

################ STEP 5: TABLE 2 ################
# Extract predictor coefficients only; omit the four outcome thresholds.
# Add the retired volunteering comparison as the final row in each coefficient table.
c1 <- summary(crude)$coefficients[names(crude$beta), ]
c2 <- summary(adjusted)$coefficients[names(adjusted$beta), ]
c1 <- rbind(c1, "Volunteering among retired respondents" = summary(crude_retired)$coefficients["volunteeringVolunteered", ])
c2 <- rbind(c2, "Volunteering among retired respondents" = summary(adjusted_retired)$coefficients["volunteeringVolunteered", ])
# Align crude rows with adjusted rows. Absent crude covariates become NA.
c1 <- c1[match(rownames(c2), rownames(c1)), ]
# Column 1 is B, column 2 is SE, and column 4 is p. Wald limits use B +/- 1.96*SE.
table2 <- data.frame(Predictor = rownames(c2),
  Crude_OR=exp(c1[,1]), Crude_lower=exp(c1[,1]-1.96*c1[,2]),
  Crude_upper=exp(c1[,1]+1.96*c1[,2]), Crude_p=c1[,4],
  Adjusted_OR=exp(c2[,1]), Adjusted_lower=exp(c2[,1]-1.96*c2[,2]),
  Adjusted_upper=exp(c2[,1]+1.96*c2[,2]), Adjusted_p=c2[,4], row.names=NULL)
table2$Predictor <- c("Volunteering among non-retired respondents", "Retired vs not retired among non-volunteers",
  "Age per 10 years", "Man vs woman", "Non-binary vs woman", "Single and dating vs in a relationship",
  "Single and not dating vs in a relationship", "$30,000-$59,999 vs under $30,000",
  "$60,000-$99,999 vs under $30,000", "$100,000 or more vs under $30,000", "Employed vs not employed",
  "Bachelor's degree or higher vs less", "Fair vs poor physical health", "Good vs poor physical health",
  "Very good vs poor physical health", "Excellent vs poor physical health", "Extraversion per 5 points",
  "Interaction ratio of volunteering ORs", "Volunteering among retired respondents")
print(table2)
write.csv(table2, "data/processed/table2.csv", row.names=FALSE)

################ STEP 6: LIMITATIONS AND SENSITIVITY CHECKS ################
# Physical health fails the proportional-odds check. Allow different slopes by threshold.
health_flexible <- update(adjusted, . ~ . - physical_health, nominal=~physical_health)
anova(adjusted, health_flexible)
nominal_test(health_flexible)
# Retirement also raises a concern in that fit. Allow both main associations to vary.
# The aliased retirement location coefficient is expected: nominal= represents it instead.
flexible <- update(adjusted, . ~ . - physical_health, nominal=~physical_health+retired)
summary(flexible)
nominal_test(flexible)
flexible_retired <- update(flexible, data=retired_data)
summary(flexible_retired)
# Display the same flexible-model ORs and confidence intervals
f <- rbind(summary(flexible)$coefficients[c("volunteeringVolunteered",
  "volunteeringVolunteered:retiredRetired"), ], summary(flexible_retired)$coefficients["volunteeringVolunteered", ])
exp(cbind(OR=f[,1], lower=f[,1]-1.96*f[,2], upper=f[,1]+1.96*f[,2]))
flexible_no_interaction <- update(flexible, . ~ . - volunteering:retired)
anova(flexible_no_interaction, flexible)
# Influence sensitivity: refit without flagged cases; retain ALL cases in Table 2.
influence_refit <- update(adjusted, data=analytic_data[!flagged, ])
summary(influence_refit)
# Sparse-category sensitivity: combine the two single groups ONLY for this refit.
sparse_data <- analytic_data
sparse_data$relationship <- factor(ifelse(sparse_data$relationship == "In a relationship",
  "In a relationship", "Single"), levels=c("In a relationship","Single"))
sparse_refit <- update(flexible, data=sparse_data)
summary(sparse_refit)
sessionInfo()
sink()
