# HSCI 410 Assignment 2 - Apostoli Kasidoulis

################ SECTION 1: DATA SETUP ################

# Load the cleaned, de-identified CSCS extract supplied.
load("data/raw/CSCS2025_full_cleaned_deidentified_data_and_metadata.RData")

################ SECTION 2: EXPLORATORY ANALYSIS ################

# Save Console output as plain text.
# If you stop early, run sink() to close the log.
sink("data/processed/cleaning_log.txt", split = TRUE)

# Initial sample size for the sample-description paragraph.
nrow(data) # 13,219 records.
ncol(data) # 3,247 variables.
length(unique(data$UNIQUE_id)) # 12,039 unique respondents across survey years.
n_raw <- nrow(data) # Retain this count to calculate exclusions in Section 3.

# Explore original columns BEFORE subsetting, renaming or cleaning.
# table() counts categories; prop.table() gives proportions.
# useNA = "ifany" includes missing values in the displayed counts.
# Plots appear in RStudio's Plots pane; no plot files are saved.
par(mar = c(15, 4, 3, 1)) # Bottom space for long response labels.

# Explore loneliness.
class(data$LONELY_direct)
summary(data$LONELY_direct)
table(data$LONELY_direct, useNA = "ifany")
prop.table(table(data$LONELY_direct, useNA = "ifany"))
barplot(table(data$LONELY_direct, useNA = "ifany"), las = 2, cex.names = 0.6,
        main = "Loneliness", ylab = "Respondents")

# Explore volunteering.
class(data$CONNECTION_activities_volunteered_p3m)
summary(data$CONNECTION_activities_volunteered_p3m)
table(data$CONNECTION_activities_volunteered_p3m, useNA = "ifany")
prop.table(table(data$CONNECTION_activities_volunteered_p3m, useNA = "ifany"))
barplot(table(data$CONNECTION_activities_volunteered_p3m, useNA = "ifany"), las = 2, cex.names = 0.6,
        main = "Volunteering", ylab = "Respondents")

# Explore retired.
class(data$DEMO_retired)
summary(data$DEMO_retired)
table(data$DEMO_retired, useNA = "ifany")
prop.table(table(data$DEMO_retired, useNA = "ifany"))
barplot(table(data$DEMO_retired, useNA = "ifany"), las = 2, cex.names = 0.6,
        main = "Retired", ylab = "Respondents")

# Explore age.
class(data$DEMO_age)
table(data$DEMO_age, useNA = "ifany")
prop.table(table(data$DEMO_age, useNA = "ifany"))
summary(data$DEMO_age)
sd(data$DEMO_age, na.rm = TRUE)
IQR(data$DEMO_age, na.rm = TRUE)
hist(data$DEMO_age, main = "Age", xlab = "Age")
# Moment skewness measures asymmetry; excess kurtosis measures tail weight.
# A normal distribution has skewness 0 and excess kurtosis 0.
x <- data$DEMO_age[!is.na(data$DEMO_age)]
mean((x - mean(x))^3) / mean((x - mean(x))^2)^1.5
mean((x - mean(x))^4) / mean((x - mean(x))^2)^2 - 3

# Explore gender.
class(data$DEMO_gender)
summary(data$DEMO_gender)
table(data$DEMO_gender, useNA = "ifany")
prop.table(table(data$DEMO_gender, useNA = "ifany"))
barplot(table(data$DEMO_gender, useNA = "ifany"), las = 2, cex.names = 0.6,
        main = "Gender", ylab = "Respondents")

# Explore relationship.
class(data$DEMO_relationship_status)
summary(data$DEMO_relationship_status)
table(data$DEMO_relationship_status, useNA = "ifany")
prop.table(table(data$DEMO_relationship_status, useNA = "ifany"))
barplot(table(data$DEMO_relationship_status, useNA = "ifany"), las = 2, cex.names = 0.6,
        main = "Relationship", ylab = "Respondents")

# Explore income.
class(data$DEMO_household_income)
summary(data$DEMO_household_income)
table(data$DEMO_household_income, useNA = "ifany")
prop.table(table(data$DEMO_household_income, useNA = "ifany"))
barplot(table(data$DEMO_household_income, useNA = "ifany"), las = 2, cex.names = 0.6,
        main = "Income", ylab = "Respondents")

# Explore employed.
class(data$DEMO_employment)
summary(data$DEMO_employment)
table(data$DEMO_employment, useNA = "ifany")
prop.table(table(data$DEMO_employment, useNA = "ifany"))
barplot(table(data$DEMO_employment, useNA = "ifany"), las = 2, cex.names = 0.6,
        main = "Employed", ylab = "Respondents")

# Explore university.
class(data$DEMO_university)
summary(data$DEMO_university)
table(data$DEMO_university, useNA = "ifany")
prop.table(table(data$DEMO_university, useNA = "ifany"))
barplot(table(data$DEMO_university, useNA = "ifany"), las = 2, cex.names = 0.6,
        main = "University", ylab = "Respondents")

# Explore physical_health.
class(data$WELLNESS_self_rated_physical_health)
summary(data$WELLNESS_self_rated_physical_health)
table(data$WELLNESS_self_rated_physical_health, useNA = "ifany")
prop.table(table(data$WELLNESS_self_rated_physical_health, useNA = "ifany"))
barplot(table(data$WELLNESS_self_rated_physical_health, useNA = "ifany"), las = 2, cex.names = 0.6,
        main = "Physical health", ylab = "Respondents")

# Explore extraversion.
class(data$PSYCH_big_five_inventory_extraverted_score)
table(data$PSYCH_big_five_inventory_extraverted_score, useNA = "ifany")
prop.table(table(data$PSYCH_big_five_inventory_extraverted_score, useNA = "ifany"))
summary(data$PSYCH_big_five_inventory_extraverted_score)
sd(data$PSYCH_big_five_inventory_extraverted_score, na.rm = TRUE)
IQR(data$PSYCH_big_five_inventory_extraverted_score, na.rm = TRUE)
hist(data$PSYCH_big_five_inventory_extraverted_score, main = "Extraversion", xlab = "Extraversion")
# Moment skewness measures asymmetry; excess kurtosis measures tail weight.
# A normal distribution has skewness 0 and excess kurtosis 0.
x <- data$PSYCH_big_five_inventory_extraverted_score[!is.na(data$PSYCH_big_five_inventory_extraverted_score)]
mean((x - mean(x))^3) / mean((x - mean(x))^2)^1.5
mean((x - mean(x))^4) / mean((x - mean(x))^2)^2 - 3

# Findings: loneliness has five substantive frequency levels; volunteering
# has seven. Retirement and employment include a valid "Not selected" level.
# Age is numeric in years and supplied extraversion is scored from 8 to 40.
# SD and IQR apply to numeric variables, categorical summaries use counts.

################ SECTION 3: DATA CLEANING AND TRANSFORMATION ################

# 3.1 HANDLE MISSING DATA AND DOCUMENT EXCLUSIONS
# Decision: analyse complete cases after applying our eligibility criteria.
# First apply each eligibility rule separately and show how many remain.
# Restrict to 2023. !is.na() prevents rows with missing year from being kept.
data <- data[!is.na(data$SURVEY_collection_year) &
             data$SURVEY_collection_year == 2023, ]
nrow(data)
n_2023 <- nrow(data)
# 9,870 excluded; 3,349 remain.

# Keep cross-sectional records, rather than cohort follow-up records.
data <- data[!is.na(data$SURVEY_collection_type) &
             data$SURVEY_collection_type == "cross", ]
nrow(data)
n_cross <- nrow(data)
# 375 excluded; 2,974 remain.

# Exclude records explicitly flagged as Spam, missing flags alone are retained.
data <- data[is.na(data$SURVEY_response_type) |
             data$SURVEY_response_type != "Spam", ]
nrow(data)
n_not_spam <- nrow(data)
# 2 excluded; 2,972 remain.

# Remove unusable IDs, then keep the first record for each respondent ID.
# duplicated() identifies later occurrences
data <- data[!is.na(data$UNIQUE_id) & as.character(data$UNIQUE_id) != "", ]
nrow(data)
n_valid_id <- nrow(data)
data <- data[!duplicated(data$UNIQUE_id), ]
nrow(data)
n_unique <- nrow(data)
# Both checks exclude 0 records in this 2023 cross-sectional extract.

# Age 50 is our study restriction to midlife/later life, not a retirement cutoff.
data <- data[!is.na(data$DEMO_age) & data$DEMO_age >= 50, ]
nrow(data)
n_age50 <- nrow(data)
# 1,101 excluded; 1,871 remain.

# Require confirmed residence in Canada. No/unknown residence is excluded.
data <- data[!is.na(data$GEO_residence_canada) &
             data$GEO_residence_canada == "Yes", ]
nrow(data)
n_eligible <- nrow(data)
# 3 excluded; 1,868 eligible respondents remain.

# Count missingness in the eleven planned variables before cleaning.
source_variables <- c("LONELY_direct", "CONNECTION_activities_volunteered_p3m",
  "DEMO_retired", "DEMO_age", "DEMO_gender", "DEMO_relationship_status",
  "DEMO_household_income", "DEMO_employment", "DEMO_university",
  "WELLNESS_self_rated_physical_health", "PSYCH_big_five_inventory_extraverted_score")
colSums(is.na(data[, source_variables]))
missing_original <- colSums(is.na(data[, source_variables]))

# Missingness recode to turn administrative nonresponses into real NA
data$LONELY_direct[data$LONELY_direct == "Presented but no response"] <- NA
data$CONNECTION_activities_volunteered_p3m[data$CONNECTION_activities_volunteered_p3m == "Presented but no response"] <- NA
data$DEMO_retired[data$DEMO_retired == "Presented but no response"] <- NA
data$DEMO_gender[data$DEMO_gender == "Presented but no response"] <- NA
data$DEMO_relationship_status[data$DEMO_relationship_status == "Presented but no response"] <- NA
data$DEMO_household_income[data$DEMO_household_income == "Presented but no response"] <- NA
data$DEMO_employment[data$DEMO_employment == "Presented but no response"] <- NA
data$DEMO_university[data$DEMO_university == "Presented but no response"] <- NA
data$WELLNESS_self_rated_physical_health[data$WELLNESS_self_rated_physical_health == "Presented but no response"] <- NA

# Count the effect of the nonresponse recode and examine the missing pattern.
administrative_recodes <- colSums(is.na(data[, source_variables])) - missing_original
administrative_recodes
colSums(is.na(data[, source_variables]))
round(100 * colSums(is.na(data[, source_variables])) / nrow(data), 1)
table(rowSums(is.na(data[, source_variables])))

# Findings: of 1,868 eligible respondents, loneliness is missing for 488,
# volunteering for 252, gender for 725, income for 804 and extraversion for 610.
# Income missingness is 43.0%. Routing, refusal or incomplete questionnaires
# could contribute, but the extract does not identify each missingness reason.
# We do not assume missing completely at random.

# 3.2 SELECT VARIABLES FOR ANALYSIS
# Square brackets select the outcome, exposure, moderator and covariates.
# source_variables above lists their actual names in the supplied extract.
data <- data[, c("LONELY_direct", "CONNECTION_activities_volunteered_p3m",
  "DEMO_retired", "DEMO_age", "DEMO_gender", "DEMO_relationship_status",
  "DEMO_household_income", "DEMO_employment", "DEMO_university",
  "WELLNESS_self_rated_physical_health", "PSYCH_big_five_inventory_extraverted_score")]

# Rename the eleven columns in the same order for easier reading.
names(data) <- c("loneliness", "volunteering", "retired", "age", "gender",
  "relationship", "income", "employed", "university", "physical_health", "extraversion")
# Use DEMO_university: DEMO_education has no observations in 2023.

# 3.3 REMOVE ROWS WITH MISSING DATA
# Count missing outcome, exposure and moderator exclusions separately.
data <- data[!is.na(data$loneliness), ]
nrow(data)
n_loneliness <- nrow(data)
# 488 excluded; 1,380 remain.
data <- data[!is.na(data$volunteering), ]
nrow(data)
n_volunteering <- nrow(data)
# 42 further excluded; 1,338 remain (not all 252, because counts overlap).
data <- data[!is.na(data$retired), ]
nrow(data)
n_retirement <- nrow(data)
# 10 further excluded; 1,328 remain.

# Only adjustment-variable missingness remains at this point.
analytic_data <- na.omit(data)
nrow(analytic_data)
ncol(analytic_data)
n_final <- nrow(analytic_data)
# 342 further excluded; final n = 986 with 11 variables and no missing values.

# 3.4 RECODE VARIABLES AS NEEDED
# Recode the complete-case dataset.
# LONELINESS: keep the five ordered frequency levels, with higher levels
# indicating more frequent loneliness.
analytic_data$loneliness <- factor(analytic_data$loneliness,
  levels = c("None of the time (e.g., 0 days)", "Rarely (e.g. less than 1 day)",
             "Some or a little of the time (e.g. 1-2 days)",
             "Occasionally or a moderate amount of time (e.g. 3-4 days)",
             "All of the time (e.g. 5-7 days)"), ordered = TRUE)

# VOLUNTEERING: compare any volunteering with none in the past three months.
# First convert the factor to text so we can assign new category names.
original_volunteering <- analytic_data$volunteering
analytic_data$volunteering <- as.character(analytic_data$volunteering)
analytic_data$volunteering[analytic_data$volunteering %in% c("Less than monthly", "Monthly",
  "A few times a month", "Weekly", "A few times a week", "Daily or almost daily")] <- "Volunteered"
analytic_data$volunteering[analytic_data$volunteering == "Not in the past three months"] <- "No volunteering"
analytic_data$volunteering <- factor(analytic_data$volunteering,
                            levels = c("No volunteering", "Volunteered"))
# Check exactly how each original category was mapped, including NA.
table(original_volunteering, analytic_data$volunteering, useNA = "ifany")

# RETIREMENT: the first level is the reference group for a later model.
# "Not selected" is a valid negative checkbox answer, not missing.
analytic_data$retired <- factor(analytic_data$retired, levels = c("Not selected", "Retired"),
                        labels = c("Not retired", "Retired"))

# EMPLOYMENT: separate from retirement. A person can report both.
analytic_data$employed <- factor(analytic_data$employed,
  levels = c("Not selected", "Employed full or part-time (including self-employed)"),
  labels = c("Not employed", "Employed"))

# Set meaningful categories and references for the other predictors.
# Reference categories: Woman, In a relationship, Less than Bachelors, Poor.
analytic_data$gender <- factor(analytic_data$gender, levels = c("Woman", "Man", "Non-binary"))
analytic_data$relationship <- factor(analytic_data$relationship,
  levels = c("In a relationship", "Single and dating", "Single and not dating"))
analytic_data$university <- factor(analytic_data$university,
  levels = c("Less than Bachelors", "Bachelors or Higher"))
analytic_data$physical_health <- factor(analytic_data$physical_health,
  levels = c("Poor", "Fair", "Good", "Very good", "Excellent"), ordered = FALSE)

# INCOME: group the supplied bands into four categories. None of these
# boundaries splits an original band. The lowest group is the reference.
original_income <- analytic_data$income
analytic_data$income <- as.character(analytic_data$income)
analytic_data$income[original_income %in% c("Under $5,000", "$5,000 to $9,999", "$10,000 to $14,999", "$15,000 to $19,999",
  "$20,000 to $24,999", "$25,000 to $29,999")] <- "Under $30,000"
analytic_data$income[original_income %in% c("$30,000 to $34,999", "$35,000 to $39,999", "$40,000 to $44,999", "$45,000 to $49,999",
  "$50,000 to $59,999")] <- "$30,000-$59,999"
analytic_data$income[original_income %in% c("$60,000 to $69,999", "$70,000 to $79,999", "$80,000 to $89,999", "$90,000 to $99,999")] <- "$60,000-$99,999"
analytic_data$income[original_income %in% c("$100,000 to $109,999", "$110,000 to $119,999", "$120,000 to $129,999",
  "$130,000 to $139,999", "$140,000 to $149,999", "$150,000 to $159,999",
  "$160,000 to $169,999", "$170,000 to $179,999", "$180,000 to $189,999",
  "$190,000 to $199,999", "$200,000 or more")] <- "$100,000 or more"
analytic_data$income <- factor(analytic_data$income,
  levels = c("Under $30,000", "$30,000-$59,999", "$60,000-$99,999", "$100,000 or more"))
table(original_income, analytic_data$income, useNA = "ifany")

# Continuous variables stay numeric and untransformed. Use the supplied
# extraversion score directly; no raw-item score reconstruction is needed.
analytic_data$age <- as.numeric(analytic_data$age)
analytic_data$extraversion <- as.numeric(analytic_data$extraversion)

# Verify every recode with table(). The first predictor level is its reference.
table(analytic_data$loneliness, useNA = "ifany")
table(analytic_data$volunteering, useNA = "ifany")
table(analytic_data$retired, useNA = "ifany")
table(analytic_data$gender, useNA = "ifany")
table(analytic_data$relationship, useNA = "ifany")
table(analytic_data$income, useNA = "ifany")
table(analytic_data$employed, useNA = "ifany")
table(analytic_data$university, useNA = "ifany")
table(analytic_data$physical_health, useNA = "ifany")
table(analytic_data$retired, analytic_data$employed) # Both selections allowed.

# 3.5 VERIFY VARIABLE CLASSES
# Age/extraversion should be numeric, categorical variables should be factors.
# Loneliness is an ordered factor, physical health is an unordered predictor.
str(analytic_data)

# Save these as plain text/CSV documentation, not Excel worksheets.
write.csv(data.frame(
  criterion = c("Raw records", "Outside 2023", "Cohort follow-up", "Spam",
                "Missing ID", "Duplicate ID", "Age below 50 or unknown",
                "Outside Canada or unknown", "Missing loneliness",
                "Missing volunteering", "Missing retirement", "Missing adjustment variable"),
  excluded = c(0, n_raw - n_2023, n_2023 - n_cross, n_cross - n_not_spam,
               n_not_spam - n_valid_id, n_valid_id - n_unique, n_unique - n_age50,
               n_age50 - n_eligible, n_eligible - n_loneliness,
               n_loneliness - n_volunteering, n_volunteering - n_retirement,
               n_retirement - n_final),
  remaining = c(n_raw, n_2023, n_cross, n_not_spam, n_valid_id, n_unique,
                n_age50, n_eligible, n_loneliness, n_volunteering, n_retirement, n_final)),
  "data/processed/exclusion_counts.csv", row.names = FALSE)

write.csv(data.frame(variable = source_variables, nonresponses_to_NA = administrative_recodes),
          "data/processed/recode_counts.csv", row.names = FALSE)
write.csv(data.frame(
  variable = names(analytic_data),
  source = c("LONELY_direct", "CONNECTION_activities_volunteered_p3m", "DEMO_retired", "DEMO_age",
  "DEMO_gender", "DEMO_relationship_status", "DEMO_household_income", "DEMO_employment",
  "DEMO_university", "WELLNESS_self_rated_physical_health",
  "PSYCH_big_five_inventory_extraverted_score"),
  coding = c("Ordered: 0 days, <1 day, 1-2 days, 3-4 days, 5-7 days; higher = more lonely",
             "Factor: No volunteering (reference), Volunteered; any positive frequency vs none",
             "Factor: Not retired (reference), Retired; valid Not selected means not retired",
             "Numeric: age in years; restricted to 50 or older; untransformed",
             "Factor: Woman (reference), Man, Non-binary",
             "Factor: In a relationship (reference), Single and dating, Single and not dating",
             "Factor: <30k (reference), 30-59,999, 60-99,999, >=100k annual household income",
             "Factor: Not employed (reference), Employed; can overlap with retirement",
             "Factor: Less than Bachelors (reference), Bachelors or Higher",
             "Factor: Poor (reference), Fair, Good, Very good, Excellent",
             "Numeric: supplied eight-item BFI extraversion score, 8-40; higher = more extraverted"),
  missing_handling = "Administrative nonresponse to NA; complete-case exclusion"),
  "data/processed/analytic_codebook.csv", row.names = FALSE)


################ SECTION 4: CREATE DESCRIPTIVE TABLE (TABLE 1) ################

# 4.1 GENERATE TABLE 1
# Install each package once if needed: install.packages("tableone")
library(tableone)
table1 <- CreateTableOne(data = analytic_data, test = FALSE)
print(table1, showAllLevels = TRUE)

# 4.2 CLEAN UP THE TABLE
# Remove unused factor levels, then regenerate Table 1 from the cleaned data.
analytic_data <- droplevels(analytic_data)
table1 <- CreateTableOne(data = analytic_data, test = FALSE)
print(table1, showAllLevels = TRUE)
# All eleven variables are included. Numeric variables use mean (SD);
# factors use n (%). Final n = 986; mean age = 65.11 (SD 8.74).
# Women = 827 (83.9%); retired = 578 (58.6%); volunteers = 398 (40.4%).

# 4.3 EXPORT TABLE 1
# Install once if needed: install.packages("writexl")
library(writexl)
# printToggle = FALSE stores the formatted table; keep row labels as a column.
table1_values <- print(table1, showAllLevels = TRUE, printToggle = FALSE,
                       noSpaces = TRUE, contDigits = 2, catDigits = 1)
table1_excel <- data.frame(Variable = rownames(table1_values), table1_values,
                           row.names = NULL, check.names = FALSE)
write_xlsx(table1_excel, "data/processed/table1.xlsx")

################ SECTION 5: SAVE YOUR ANALYTIC DATASET ################

# Save the final dataset for the next assignment. Its name is analytic_data.
save(analytic_data, file = "data/processed/cscs_analytic.RData")
sink() # Close the plain-text cleaning log opened in Section 2.
