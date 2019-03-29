### Feature Engineering in R

### 1. Creating Features from Categorical Data

# Load dplyr
library(dplyr)

discipline_logs <- discipline_logs %>%  
  mutate( 
      # Create male column
      male = ifelse(gender == "Male", 1, 0),

      # Create female column
      female = ifelse(gender == "Female", 1, 0))

# Create a new column with the proper string encodings
discipline_logs_new <-  discipline_logs %>%
  mutate(school_type = 
            case_when(grade >= 1 & grade <= 5 ~ "elementary_school",
                      grade >= 6 & grade <= 8 ~ "middle_school",
                      grade >= 9 & grade <=  12 ~ "high_school"))

# Look at a table of the new column 
discipline_logs_new %>%
  select(school_type) %>%
  table() 

discipline_logs_new <- discipline_logs_new %>%  
  mutate( 
        # Create elem_sch column
        elem_sch = ifelse(school_type == "elementary_school", 1, 0),

      # Create mid_sch column
        mid_sch = ifelse(school_type == "middle_school", 1, 0),

        # Create high_sch column
        high_sch = ifelse(school_type == "high_school", 1, 0))

# Create a table of the frequencies
discipline_table <- table(discipline_logs$grade, discipline_logs$discipline)

# Create a table of the proportions
prop_table <- prop.table(discipline_table, 1)

# Combine the proportions and discipline logs data
discipline <- inner_join(discipline_logs, dgr_prop, by = "grade")

# Display a glimpse of the new data frame
glimpse(discipline)

# Create a new column with three levels using the proportions as ranges
discipline_ed <- discipline %>%
   mutate(education_levels = 
      case_when(proportion >= 0 & proportion < .20 ~ "low_grade",
                proportion >= .20 & proportion < .25 ~ "middle_grade", 
                proportion >= .25 & proportion < 1 ~ "high_grade"))

#######################

### 2. Creating Features from Numeric Data

# Summarize the Quantity variable
summary(online_retail %>% 
select(Quantity))

# Create a histogram of the possible variable values
ggplot(online_retail, aes(x = Quantity)) + 
  geom_histogram(stat = "count")

# Use the cut function to create a variable quant_cat
online_retail <- online_retail %>% 
  mutate(quant_cat = cut(Quantity, breaks = seq(1, 50, by = 5)))

# Create a table of the new column quant_cat
online_retail %>%
  select(quant_cat) %>%
  table()

# Create new columns from the quant_cat feature
head(model.matrix(~ quant_cat -1, data = online_retail))

# Break the Quantity variable into 3 buckets
online_retail <- online_retail %>% 
  mutate(quant_q = ntile(Quantity, 3))

# Use table to look at the new variable
online_retail %>%
  select(quant_q) %>%
  table()

# Use table to look at the new variable
online_retail %>%
  select(quant_q) %>%
  table()

# Specify a full rank representation of the new column
head(model.matrix(~ quant_q - 1, data = online_retail))

# Load lubridate
library(lubridate)

# Look at the column timestamp
discipline_logs %>%
  select(timestamp) %>%
  glimpse()

# Assign date format to the timestamp_date column
discipline_logs %>%
  mutate(timestamp_date = ymd_hms(timestamp))

# Create new column dow (day of the week) 
discipline_logs <- discipline_logs %>% 
  mutate(dow = wday(timestamp_date, label = T))

head(discipline_logs)

# Create new column hod (hour of day) 
discipline_logs <- discipline_logs %>% 
  mutate(hod = hour(timestamp_date))

head(discipline_logs)

# Create histogram of hod 
ggplot(discipline_logs, aes(hod)) +
geom_histogram()

##########################

### 3. Transforming Numerical Features

# Select the variables
poke_vars <- poke_df %>%
	select(Defense, Speed) 

# Perform a Box-Cox transformation
processed_vars <- preProcess(poke_vars, method = c("BoxCox"))

# Use predict to transform data
poke_df <- predict(processed_vars, poke_df)

# Plot transformed features
ggplot(poke_df, aes(x = Defense)) + 
  geom_density()

ggplot(poke_df, aes(x = Speed)) + 
  geom_density()

# Select both variables
bank_vars <- bank_df %>%
	select(balance, duration)
    
# Perform a Yeo-Johnson transformation 
processed_vars <- preProcess(bank_vars, method = c("YeoJohnson"))

# Use predict to transform data
bank_df <- predict(processed_vars, bank_df)

# Plot transformed features
ggplot(bank_df,aes(x = balance)) + 
  geom_density()

ggplot(bank_df,aes(x = duration)) + 
  geom_density()

# Create a scaled new feature scaled_hp 
poke_df <- poke_df %>%
	mutate(scaled_hp = (HP - min(HP)) / 
   (max(HP) - min(HP)))

# Summarize both features
poke_df %>% 
	select(HP, scaled_hp) %>%
	summary()

# Use mutate to create column attack_mc
poke_df <- poke_df %>%
	mutate(attack_mc = Attack - mean(Attack))

# Select variables 
poke_vars <- poke_df %>% 
	select(Attack, SpAtk, SpDef)

# Use preProcess to mean center variables
processed_vars <- preProcess(poke_vars, method = c("center"))

# Use predict to include tranformed variables into data
poke_df <- predict(processed_vars, poke_df)

# Summarize the three new column scales
poke_df %>% 
	select(Attack, SpAtk, SpDef) %>%
	summary()

# Standardize Speed
poke_df <- poke_df %>% 
	mutate(z_speed = (Speed - mean(Speed))/
  		sd(Speed))

# Summarize new and original variable
poke_df %>% 
	select(Speed, z_speed) %>%
	summary()

# Select variables 
poke_vars <- poke_df %>% 
	select(Attack, Defense, SpAtk, SpDef)

# Create preProcess variable list 
processed_vars <- preProcess(poke_vars, method = c("center", "scale"))

# Use predict to assign standardized variables
poke_df <- predict(processed_vars, poke_df)

# Summarize new variables
poke_df %>% 
	select(Attack, Defense, SpAtk, SpDef) %>% 
	summary()

#########################

### 4. Advanced Methods

# Group the data and create a summary of the counts
adult_incomes %>%
  group_by(occupation, gender) %>%
  summarize(n = n()) %>%
  # Create a grouped bar graph
  ggplot(., aes(occupation, n, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Create a table of the variables of interest
adult_incomes %>%
  select(gender, occupation) %>%
  table()

# Create a feature cross between gender and occupation
dmy <- dummyVars(~ gender:occupation, data = adult_incomes)

# Create object of your resulting data frame
oh_data <- predict(dmy, adult_incomes)

# Summarize the resulting output
summary(oh_data)

# Create the df
poke_x <- poke_df %>% 
  select(HP, Attack, Defense, SpAtk, SpDef, Speed)

# Perform PCA 
poke_pca <- prcomp(poke_x, center = T, scale = T)

# Calculate the proportion of variance
prop_var <- prop_var %>%
  mutate(pca_comp = 1:n(),
  		pcVar = sdev^2, 
        propVar_ex = pcVar/sum(pcVar))

# Create a plot of the components and proportion of variance
ggplot(prop_var, aes(pca_comp, propVar_ex, group = 1)) + 
  geom_line() +
  geom_point()

# Create a plot of the first two components
autoplot(poke_pca, data = poke_df, colour = 'Type1')


