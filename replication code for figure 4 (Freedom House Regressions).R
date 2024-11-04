rm(list=ls())


# libraries ---------------------------------------------------------------


library(rvest)
library(dplyr)
library(haven)
library(Synth) # SCM package
library(tidyverse)
library(Synth) # SCM package
library(fixest)
library(ggplot2)


# set wd ------------------------------------------------------------------

# reading the headers -----------------------------------------------------
url <- "https://www.heritage.org/index/pages/all-country-scores"

webpage <- read_html(url)

# Read the HTML code of the page 
html_code <- read_html(url) 

# Use the html_nodes function to extract the table 
table_html <- html_code %>% html_nodes("table") %>% .[[1]] 

# Use the html_table function to convert the table  
# HTML code into a data frame 
table_df <- table_html %>% html_table() 

head(table_df)

# reading the data --------------------------------------------------------
d <- read.csv("Economic_Freedom_Scraped.csv")

# Assign the column names from table_df to d
colnames(d) <- colnames(table_df)

# removing blank first row
d <- d[-1, ]

# removing blank five rows
d <- d[, -c(16:20)]


d <- d %>%
  mutate_at(3:15, function(x) {
    # Replace "N/A" with NA
    x[x == "N/A"] <- NA
    # Convert to numeric
    as.numeric(x)
  })

# reading the Funke et al data --------------------------------------------
ple_dataset <- read_dta("ple_dataset.dta")

summary(ple_dataset)
# merging -----------------------------------------------------------------

df <- ple_dataset %>%
  filter(year > 1995) %>% 
  left_join(d, by = c("country" = "Name", "year" = "IndexYear"))

# Create the "populist_5years" variable for 5 consecutive years of populism
df <- df %>%
  group_by(country) %>%
  arrange(year) %>%
  mutate(
    # Create a counter for consecutive populist years
    populist_years = ifelse(popepc == 1, 1, 0),  # Binary indicator for populist years
    populist_consec = cumsum(populist_years) - cumsum(lag(populist_years, default = 0) == 0),  # Track consecutive years
    # Adjust populist_5years to be either our method or Post_5
    populist_5years = ifelse(populist_consec >= 5 | Post_5 == 1, 1, 0)  # Use either condition
  ) %>%
  ungroup()


# making the regression ---------------------------------------------------


# Step 1: Create a new "relative_year" variable based on the timing of populist rule
df <- df %>%
  group_by(country) %>%
  arrange(year) %>%
  mutate(
    # Calculate relative year only if there's a valid populist entry year
    relative_year = ifelse(
      any(popepc == 1, na.rm = TRUE),
      year - min(year[popepc == 1], na.rm = TRUE),
      NA_real_ # or alternatively 0 if you want a starting baseline
    ),
    # Determine 5-year period post populism
    populist_5years = ifelse(populist_consec >= 5 | Post_5 == 1, 1, 0)
  ) %>%
  ungroup()

df <- df %>%
  filter(!is.na(OverallScore) & !is.na(popepc))

# Run the two-way fixed effects model
model_twfe <- feols(OverallScore ~ popepc | country + year, data = df)

# putting the predicted scores in
df$predicted_OS <- predict(model_twfe)


# facet plot -------------------------------------------------------------
# List of index variables with readable labels
index_labels <- c("Overall Score", "Property Rights", "Government Integrity", "Judicial Effectiveness", 
                  "Tax Burden", "Government Spending", "Fiscal Health", "Business Freedom", 
                  "Labor Freedom", "Monetary Freedom", "Trade Freedom", "Investment Freedom", 
                  "Financial Freedom")

# Named vector for easier matching
names(index_labels) <- c("OverallScore", "PropertyRights", "GovernmentIntegrity", "JudicialEffectiveness", 
                         "TaxBurden", "GovernmentSpending", "FiscalHealth", "BusinessFreedom", 
                         "LaborFreedom", "MonetaryFreedom", "TradeFreedom", "InvestmentFreedom", 
                         "FinancialFreedom")

# Combined data frame for all index variables
combined_data <- data.frame()

# Process each index variable
for (var in names(index_labels)) {
  print(paste("Processing", var))
  
  # Ensure column exists and remove NA rows
  if (!var %in% names(df)) {
    warning(paste("Index variable", var, "does not exist in the dataset."))
    next
  }
  
  data <- df %>% filter(!is.na(.[[var]]))
  
  if (length(unique(data$popepc)) <= 1) {
    warning(paste("No variation in 'popepc' for", var, "- Skipping"))
    next
  }
  
  model_twfe <- tryCatch({
    feols(as.formula(paste0(var, " ~ popepc | country + year")), data = data)
  }, error = function(e) {
    warning(paste("Model failed for", var, "due to:", e$message))
    return(NULL)
  })
  
  if (is.null(model_twfe)) next
  
  data$predicted_OS <- predict(model_twfe, newdata = data)
  
  avg_OS <- data %>%
    group_by(relative_year, populist_group = case_when(popepc == 1 ~ "Populist",
                                                       TRUE ~ "Non-Populist")) %>%
    summarize(
      mean_OS = mean(predicted_OS, na.rm = TRUE),
      se_OS = sd(predicted_OS, na.rm = TRUE) / sqrt(n()),
      lower_bound = mean_OS - 1.96 * se_OS,
      upper_bound = mean_OS + 1.96 * se_OS,
      index_var = index_labels[var],  # Use readable label
      .groups = 'drop'
    )
  
  combined_data <- rbind(combined_data, avg_OS)
}

# Create a faceted plot
facet_plot <- ggplot(combined_data, aes(x = relative_year, y = mean_OS, color = populist_group)) +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound, fill = populist_group), alpha = 0.2, color = NA) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", size = 1) +
  scale_color_manual(values = c("Populist" = "red", "Non-Populist" = "black")) +
  scale_fill_manual(values = c("Populist" = "lightgrey", "Non-Populist" = "lightgrey")) +
  facet_wrap(~ index_var, ncol = 3) +
  labs(
    #title = "Effects of Populism on Economic Indicators",
    x = "Time Relative to Populist Gaining Power (Years)",
    y = "Average Predicted Score",
    color = "Group",
    fill = "Group"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.minor = element_blank()
  )

# Save or display the combined plot
ggsave(filename = "plots/facet_plot.pdf", plot = facet_plot, width = 12, height = 10)

# Alternatively, display plot in R
print(facet_plot)


