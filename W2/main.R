load("miss.data.multi.RData")
summary(miss.data.multi)

str(miss.data.multi)

pct.missing <- round(colMeans(is.na(miss.data.multi)) * 100, 2) 
pct.missing

"Boolean array for missing values in cvd variable"
miss.cvd <- is.na(miss.data.multi$cvd)

#Installation of packages
install.packages("devtools")
devtools::install_github("amices/ggmice")
library(ggmice)

plot_pattern(miss.data.multi)
# The most common pattern is no missing values
# followed by missings in cvd

# In the case of missing values in DM, pulm and cvd
# ...

# Although we can't tell what the missing mechanishm is,
# it could be due to the no follow up of the patients
# probably because everything was alright

# Let's check that:
# Create a new variable called followup that is 1 if
# the patient has no missing values in DM, pulm and cvd
# and 0 otherwise
miss.data.multi$followup <- ifelse(is.na(miss.data.multi$cvd) == TRUE & 
                                    is.na(miss.data.multi$DM) == TRUE & 
                                    is.na(miss.data.multi$pulm) == TRUE, 0, 1)


# Summary for followup == 1
summary(miss.data.multi[miss.data.multi$followup == 1, ])
str(miss.data.multi[miss.data.multi$followup == 1, ])

# Plot the distribution of the vacc variable grouped by followup normalised by the counts of followup
ggplot(miss.data.multi, aes(x = vacc, fill = factor(followup))) + 
  geom_bar(position = "dodge") + 
  scale_fill_manual(values = c("red", "blue")) + 
  theme_minimal() + 
  labs(title = "Distribution of vacc variable grouped by followup", 
       x = "vacc", 
       y = "Counts", 
       fill = "followup")


# ANALYSING THE FULL DATASET
load("full.data.RData")
nomissing_model <- glm( hosp ~ vacc + DM + cvd + pulm + I(log(contact)) + age + sex, 
                        data = FULL.data,
                        family = binomial())

summary(nomissing_model)

# COMPLETE CASE ANALYSIS
cca_model <- glm( hosp ~ vacc + DM + cvd + pulm + I(log(contact)) + age + sex, 
                  data = miss.data.multi,
                  family = binomial())

summary(cca_model)
