library(tidyverse)
library(plotly)
library(htmlwidgets)

# This function groups rows by identifying characteristics
# And gives a K value based on count in that group
k_anon = function(df){
  anon_df = df %>% 
    group_by(age, date, gender, race_ethn) %>% 
    mutate(k=n()) %>% 
    ungroup()
}

# Set seed for reproduceability
set.seed(123)

# This is the number of people to be sampled
n_rows = 500

# Create first simulation with all data points
simulation_1 <- tibble(
  
  # Sample from range of dates representing beginning of school year
  date = sample(seq(as.Date('2025-08-23'), 
                    as.Date('2025-09-07'), 
                    by = "day"), 
                n_rows, replace = TRUE),
  
  # Sample gender options
  gender = sample(c("Male", 
                    "Female", 
                    "NA/Other"), 
                  n_rows, replace = TRUE),
  
  # Sample ages 1 through 18
  age = sample(1:18, n_rows, replace = TRUE),
  
  # Sample race by unequal probability
  race_ethn = sample(c("Native American", 
                       "Hispanic", 
                       "White", 
                       "African American"), 
                     n_rows, replace = TRUE, 
                     prob=c(0.1,    # Native American
                            0.2,    # Hispanic
                            0.5,    # White
                            0.3)),  # African American
  
  # Create case identifier
  case_id = 1:n_rows
) %>%
  mutate(
    # Ensure factor levels are set
    gender = factor(gender, levels = c("Male", "Female", "NA/Other")),
    race_ethn = factor(race_ethn),
    age = factor(age)
  ) %>% 
  k_anon()

# Bin ages into 3 age groups and factor intervals
simulation_2 = simulation_1 %>% 
  mutate(
    age = as.character(cut_interval(as.numeric(age),3)) 
    %>% factor() # Age is factored here and afterward for jittering purposes
  ) %>% 
  k_anon()

# Rewrite date to collapse individual reporting days into weeks
simulation_3 = simulation_2 %>%
  mutate(
    date = case_when(
      .data$date <= as.Date("2025/08/31") ~ as.Date("2025/08/01"),
      .data$date > as.Date("2025/09/01") ~ as.Date("2025/09/01"),
      TRUE ~ .data$date
    ),
    age = factor(age)
  )

# Collapse ethnicity into white/non-white
simulation_4 = simulation_3 %>% 
  mutate(
    race_ethn = case_when(
      race_ethn == "Native American" ~ "Non-White",
      race_ethn == "African American" ~ "Non-White",
      race_ethn == "Hispanic" ~ "Non-White",
      TRUE ~ race_ethn
    ),
    age = factor(age)
  ) %>% 
  k_anon()

# At the backend, only choose those whose groups was k > 3. This is necessary
# Because the random sampling produced many unique groups that would be entirely
# Dropped, and the minimum class equivalence would never progress. It is a sleight
# Of hand.
white_list = simulation_3 %>% 
  filter(k > 3) %>% 
  select(case_id)

## Plotly 1
p_1 = simulation_1 %>%
  
  filter(case_id %in% white_list$case_id) %>%
  
  mutate(
    # Jitter dates
    date_jittered = as.Date(as.numeric(date) + runif(n(), -0.4, 0.4), origin = "1970-01-01"),
    
    # Jitter gender
    gender_jittered = as.numeric(gender) + runif(n(), -0.2, 0.2),
    
    # Age jitter
    age_jittered = as.numeric(age) + runif(n(), -0.3, 0.3)
  ) %>%
  
  plot_ly(
    x = ~date_jittered, 
    y = ~gender_jittered, # Use jittered gender
    z = ~age_jittered,   
    alpha = 0.8,
    ids = ~case_id,
    color = ~race_ethn,
    colors = c("#cc79a7", "#d55e00", "#0072b2", "#009e73"),
    type = "scatter3d",
    mode = "markers" 
  ) %>%
  
  layout(
    title = list(
      text = "Minimum Class Equivalence 1"
    ),
    legend = list(
      title = list(text = 'Race/\nEthnicity')
    ),
    scene = list(
      xaxis = list(title = "Date Reported"),
      yaxis = list(
        title = "Gender",
        tickmode = "array",
        tickvals = c(1, 2, 3),
        ticktext = c("Male", "Female", "NA/Other"),
        range = c(0.5, 3.5)
      ),
      zaxis = list(title = "Age"),
      camera = list(
        eye = list(x = 1.25, y = 1.25, z = 1.25)
      )
    )
  )

## Scenario 2
# Now, get the names of those factor levels for the plot's axis labels
age_labels <- levels(simulation_2$age)

p_2 = simulation_2 %>%
  
  filter(case_id %in% white_list$case_id) %>%
  
  mutate(
    date_jittered = as.Date(as.numeric(date) + runif(n(), -0.4, 0.4), origin = "1970-01-01"),
    
    gender_jittered = as.numeric(gender) + runif(n(), -0.2, 0.2),
    
    age_jittered = as.numeric(age) + runif(n(), -0.3, 0.3)
  ) %>%
  
  plot_ly(
    x = ~date_jittered,
    y = ~gender_jittered,
    z = ~age_jittered,
    alpha = 0.8,
    ids = ~case_id,
    color = ~race_ethn,
    colors = c("#cc79a7", "#d55e00", "#0072b2", "#009e73"),
    type = "scatter3d",
    mode = "markers"
  ) %>%
  
  layout(
    title = list(text = "Minimum Class Equivalence 4"),
    legend = list(title = list(text = 'Race/\nEthnicity')),
    scene = list(
      aspectmode = "cube",
      xaxis = list(title = "Date Reported"),
      yaxis = list(
        title = "Gender",
        tickmode = "array",
        tickvals = c(1, 2, 3),
        ticktext = c("Male", "Female", "NA/Other"),
        range = c(0.5, 3.5)
      ),
      zaxis = list(
        title = "Age Bracket",
        tickmode = "array",
        tickvals = c(1, 2, 3),
        ticktext = c("1-6","7-12","13-18"),
        range = c(0.5, 3.5)
      ),
      camera = list(
        eye = list(x = 1.25, y = 1.25, z = 1.25)
      )
    )
  )

## Scenario 3
age_labels <- levels(simulation_3$age)

p_3 = simulation_3 %>%
  
  filter(case_id %in% white_list$case_id) %>%
  
  mutate(
    date_jittered = as.Date(as.numeric(date) + runif(n(), -0.4, 0.4), origin = "1970-01-01"),
    gender_jittered = as.numeric(gender) + runif(n(), -0.2, 0.2),
    age_jittered = as.numeric(age) + runif(n(), -0.3, 0.3)
  ) %>%
  
  plot_ly(
    x = ~date_jittered,
    y = ~gender_jittered,
    z = ~age_jittered,
    alpha = 0.8,
    ids = ~case_id,
    color = ~race_ethn,
    colors = c("#cc79a7", "#d55e00", "#0072b2", "#009e73"),
    type = "scatter3d",
    mode = "markers"
  ) %>%
  
  layout(
    title = list(text = "Minimum Class Equivalence 4"),
    legend = list(title = list(text = 'Race/\nEthnicity')),
    scene = list(
      aspectmode = "cube",
      xaxis = list(title = "Date Reported"),
      yaxis = list(
        title = "Gender",
        tickmode = "array",
        tickvals = c(1, 2, 3),
        ticktext = c("Male", "Female", "NA/Other"),
        range = c(0.5, 3.5)
      ),
      zaxis = list(
        title = "Age Bracket",
        tickmode = "array",
        tickvals = c(1, 2, 3),
        ticktext = c("1-6","7-12","13-18"), 
        range = c(0.5, 3.5)
      ),
      camera = list(
        eye = list(x = 1.25, y = 1.25, z = 1.25)
      )
    )
  )

## Scenario 4
age_labels <- levels(simulation_4$age)

p_4 = simulation_4 %>%
  
  filter(case_id %in% white_list$case_id) %>%
  
  mutate(
    date_jittered = as.Date(as.numeric(date) + runif(n(), -0.4, 0.4), origin = "1970-01-01"),
    gender_jittered = as.numeric(gender) + runif(n(), -0.2, 0.2),
    age_jittered = as.numeric(age) + runif(n(), -0.3, 0.3)
  ) %>%
  
  plot_ly(
    x = ~date_jittered,
    y = ~gender_jittered,
    z = ~age_jittered,
    alpha = 0.8,
    ids = ~case_id,
    color = ~race_ethn,
    colors = c("#0072b2", "#d55e00", "#cc79a7", "#009e73"),
    type = "scatter3d",
    mode = "markers"
  ) %>%
  
  layout(
    title = list(text = "Minimum Class Equivalence 11"),
    legend = list(title = list(text = 'Race/\nEthnicity')),
    scene = list(
      aspectmode = "cube",
      xaxis = list(title = "Date Reported"),
      yaxis = list(
        title = "Gender",
        tickmode = "array",
        tickvals = c(1, 2, 3),
        ticktext = c("Male", "Female", "NA/Other"),
        range = c(0.5, 3.5)
      ),
      zaxis = list(
        title = "Age Bracket",
        tickmode = "array",
        tickvals = c(1, 2, 3),
        ticktext = c("1-6","7-12","13-18"), 
        range = c(0.5, 3.5)
      ),
      camera = list(
        eye = list(x = 1.25, y = 1.25, z = 1.25)
      )
    )
  )

## Write final CSV file

# Notate each simulation
simulation_1 = simulation_1 %>% mutate(step = 1)
simulation_2 = simulation_2 %>% mutate(step = 2)
simulation_3 = simulation_3 %>% mutate(step = 3)
simulation_4 = simulation_4 %>% mutate(step = 4)

# Combine, filter to correct ID's, export
simulation = simulation_1 %>% 
  bind_rows(simulation_2) %>% 
  bind_rows(simulation_3) %>% 
  bind_rows(simulation_4) %>% 
  filter(case_id %in% white_list$case_id) %>% 
  select(step, race_ethn, gender, age, date, k) %>% 
  write_csv("k_anon_simulation_data.csv")