library(tidyverse)
library(skimr)
library(NHANES)
library(corrplot)
library(naniar)

data(NHANESraw)
df <- NHANESraw

# 1. Initial data overview
dim(df)          # size: 10000 rows, 76 variables
head(df)         # first rows
str(df)          # variable types

# 1.1 Filter to adults
df <- df %>% filter(Age >= 18)

# 2. missing values
colSums(is.na(df)) # number of missing values per column

# number and percentage of missing values per column
data.frame(
  missing = colSums(is.na(df)),
  pct = round(colMeans(is.na(df)) * 100, 1)
) %>%
  filter(missing > 0) %>%
  arrange(desc(pct))

# 3. distribution of key numerical variables
# select the most important numerical variables
key_vars <- c("Age", "BMI", "BPSysAve", "BPDiaAve", 
              "TotChol", "Pulse", "Weight", "Height")

# histograms for all key variables
df %>%
  select(all_of(key_vars)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 30) +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(title = "Distribution of numerical variables (NHANES)",
       x = NULL, y = "count")

# outlier detection (boxplots)
df %>%
  select(all_of(key_vars)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = variable, y = value)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(title = "Boxplots — outlier detection", x = NULL)

# 4. categorical variables
# distribution by gender
df %>%
  count(Gender) %>%
  ggplot(aes(x = Gender, y = n, fill = Gender)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Gender distribution", y = "count")

# distribution by race/ethnicity
df %>%
  count(Race1) %>%
  ggplot(aes(x = reorder(Race1, -n), y = n, fill = Race1)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Race/ethnicity distribution", 
       x = "race/ethnicity", y = "count")

# distribution by education level
df %>%
  filter(!is.na(Education)) %>%
  count(Education) %>%
  ggplot(aes(x = Education, y = n, fill = Education)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(title = "Education level distribution", y = "count")

# diabetes
df %>%
  filter(!is.na(Diabetes)) %>%
  count(Diabetes) %>%
  ggplot(aes(x = Diabetes, y = n, fill = Diabetes)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Diabetes prevalence", y = "count")

# 5. relationships between variables
# bmi by gender
df %>%
  filter(!is.na(BMI)) %>%
  ggplot(aes(x = Gender, y = BMI, fill = Gender)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "BMI by gender")

# bmi vs age (scatter plot)
df %>%
  filter(!is.na(BMI), !is.na(Age)) %>%
  ggplot(aes(x = Age, y = BMI, color = Gender)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "BMI vs age by gender")

# blood pressure vs bmi
df %>%
  filter(!is.na(BPSysAve), !is.na(BMI)) %>%
  ggplot(aes(x = BMI, y = BPSysAve, color = Gender)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "systolic blood pressure vs bmi", 
       y = "systolic blood pressure (mmhg)")

# bmi by diabetes status
df %>%
  filter(!is.na(BMI), !is.na(Diabetes)) %>%
  ggplot(aes(x = Diabetes, y = BMI, fill = Diabetes)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "bmi in people with and without diabetes")

# 6. age groups
df %>%
  filter(!is.na(BMI), !is.na(Age)) %>%
  mutate(AgeGroup = cut(Age, 
                        breaks = c(0, 17, 35, 50, 65, 100),
                        labels = c("0-17", "18-35", "36-50", "51-65", "65+"))) %>%
  ggplot(aes(x = AgeGroup, y = BMI, fill = AgeGroup)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "BMI by age groups", x = "age group")

# 7. correlation matrix
cor_matrix <- df %>%
  select(all_of(key_vars)) %>%
  drop_na() %>%
  cor()

corrplot(cor_matrix, 
         method = "color", 
         type = "upper",
         tl.cex = 0.8,
         addCoef.col = "black",
         number.cex = 0.7,
         title = "nhanes correlation matrix",
         mar = c(0,0,1,0))