### Analyzing Survey Data in R

### 1. Introduction to survey data

# Load ggplot2
library(ggplot2)

# Construct a histogram of the weights
ggplot(data = ce, mapping = aes(x = FINLWT21)) +
    geom_histogram()

# Look at the apisrs dataset
glimpse(apisrs)

# Construct histogram of pw
ggplot(data = apisrs,
       mapping = aes(x = pw)) + 
    geom_histogram()

#Create table of average survey weights by race
tab_weights <- NHANESraw %>%
  group_by(Race1) %>%
  summarize(avg_wt = mean(WTMEC4YR))

#Print the table
tab_weights

# Specify the NHANES design
NHANES_design <- svydesign(data = NHANESraw, strata = ~SDMVSTRA, id = ~SDMVPSU, nest = TRUE, weights = ~WTMEC4YR)

# Print summary of design
summary(NHANES_design)

# Number of clusters
NHANESraw %>%
  summarize(n_clusters = n_distinct(SDMVSTRA, SDMVPSU))

# Sample sizes in clusters
NHANESraw %>%
  count(SDMVSTRA, SDMVPSU)

#######################

### 2. Exploring categorical data

# Specify the survey design
NHANESraw <- mutate(NHANESraw, WTMEC4YR = .5 * WTMEC2YR)
NHANES_design <- svydesign(data = NHANESraw, strata = ~SDMVSTRA, id = ~SDMVPSU, nest = TRUE, weights = ~WTMEC4YR)

# Determine the levels of Depressed
levels(NHANESraw$Depressed)

# Construct a frequency table of Depressed
tab_w <- svytable(~Depressed, design = NHANES_design)

# Determine class of tab_w
class(tab_w)

# Display tab_w
tab_w

# Add proportions to table
tab_w <- tab_w %>%
  as.data.frame() %>%
  mutate(Prop = Freq/sum(Freq))

# Create a barplot
ggplot(data = tab_w,
       mapping = aes(x = Depressed, y = Prop)) + 
  geom_col()


#######################

### 3. Exploring quantitative data

# Compute the survey-weighted mean
svymean(x = ~SleepHrsNight, 
        design = NHANES_design,
        na.rm = TRUE)

# Compute the survey-weighted mean by Gender
svyby(formula = ~SleepHrsNight, 
    by = ~Gender, 
    design = NHANES_design, 
    FUN = svymean, 
    na.rm = TRUE, 
    keep.names = FALSE)

# Compute the survey-weighted quantiles
svyquantile(x = ~SleepHrsNight, 
            design = NHANES_design, 
            na.rm = TRUE, 
            quantiles = c(0.01, 0.25, 0.5, 0.75, .99))

# Compute the survey-weighted quantiles by Gender
svyby(formula = ~SleepHrsNight, 
      by = ~Gender, 
      design = NHANES_design, 
      FUN = svyquantile, 
      na.rm = TRUE, 
      quantiles = .5, 
      keep.rows = FALSE, 
      keep.var = FALSE)

# Compute the survey-weighted mean by Gender
out <- svyby(formula = ~SleepHrsNight, 
             by = ~Gender, 
             design = NHANES_design, 
             FUN = svymean, 
             na.rm = TRUE, 
             keep.names = FALSE)
             
# Construct a bar plot of average sleep by gender
ggplot(data = out, mapping = aes(x = Gender, y = SleepHrsNight)) +
  geom_col() + 
  labs(y = "Average Nightly Sleep")


#######################

### 4. Modeling quantitative data


