library(readxl)
library(openxlsx)
library(tidymodels)
library(tidyverse)
library(skimr)
library(corrplot)
library(doParallel)
library(ranger)
library(vip)
library(gridExtra)
library(reshape2)
library(lubridate)
library(leaflet)
library(dplyr)
library(timetk)
library(xgboost)
library(caret)
library(ggmap)
library(ggplot2)
library(purrr)
library(stringr)

dealroom_data_path <- "dealroom_data.xlsx"
dealroom_data <- read_excel(dealroom_data_path)

#filter out rows with NA values for website or industries
dealroom_data <- dealroom_data %>%
  filter(!is.na(WEBSITE) & !is.na(INDUSTRIES))
skim(dealroom_data)

#step 1, analyze the columns and do the necessary cleaning and feature engineering (splitting)
#change the name of the variables
names(dealroom_data) <- tolower(names(dealroom_data))
names(dealroom_data) <- gsub(" ", "_", names(dealroom_data))

##split up founding location into country and city
dealroom_data$founding_location_country <- sapply(strsplit(dealroom_data$founding_location, ";"), `[`, 1)
dealroom_data$founding_location_city <- sapply(strsplit(dealroom_data$founding_location, ";"), `[`, 2)
#remove the original 'founding_location' column
dealroom_data$founding_location <- NULL

#create 'has_investor' binary variable
dealroom_data <- dealroom_data |>
  mutate(has_investor = ifelse(is.na(investors) | investors == "", NA_integer_, 1))
#separate 'investors' into individual rows only for rows where 'has_investor' is 1
investors_separated <- dealroom_data |>
  filter(has_investor == 1) |>
  select(investors) |>
  separate_rows(investors, sep = ";") |>
  mutate(investors = tolower(trimws(investors)))
#create dataframe with the count of investments for each investor
investor_counts <- investors_separated |>
  group_by(investors) |>
  summarise(count = n()) |>
  ungroup() |>
  arrange(desc(count))
#identify the top 100 investors
top_100_investors <- head(investor_counts, 100)
#check if a company's investors include any of the top 100 investors and count how many top investors are present
dealroom_data <- dealroom_data |>
  mutate(
    investors = ifelse(is.na(investors), NA, tolower(investors)),
    investors_list = str_split(investors, pattern = ";", simplify = FALSE),
    is_top_investor = map_int(investors_list, ~ sum(!is.na(.x) & tolower(.x) %in% top_100_investors$investors)),
    number_top_investors = if_else(is.na(investors), NA_integer_, if_else(is_top_investor > 0, is_top_investor, 0))
  ) |>
  select(-investors_list)

#update 'is_top_investor' to be binary, with NA preserved where 'investors' is NA
dealroom_data <- dealroom_data |>
  mutate(is_top_investor = if_else(is.na(investors), NA_integer_, if_else(is_top_investor > 0, 1, 0)))
dealroom_data$investors <- NULL

##feature engineering each_investor_type
#normalize the delimiter to semicolon if it's a comma or semicolon
dealroom_data <- dealroom_data |>
  mutate(each_investor_types = str_replace_all(each_investor_types, "[;,]", ";"))
#separate the investor types into individual elements and identify unique types
investor_types_separated <- dealroom_data |>
  #remove NA and empty strings
  filter(!is.na(each_investor_types), each_investor_types != "") |>
  #seperate investor types into individual rows using semicolon as the separator
  separate_rows(each_investor_types, sep = ";") |>
  #remove potential leading/trailing whitespaces
  mutate(each_investor_types = str_trim(each_investor_types)) |>
  #find unique investor types
  distinct(each_investor_types)
#extract unique investor types into a vector
unique_investor_types <- investor_types_separated$each_investor_types
#create binary columns for each unique investor type
for(investor_type in unique_investor_types) {
  column_name <- paste0("is_", str_replace_all(tolower(investor_type), " ", "_"))
  dealroom_data[[column_name]] <- 0
  dealroom_data[[column_name]] <- ifelse(str_detect(dealroom_data$each_investor_types, fixed(investor_type)), 1, 0)
}
#remove the original 'each_investor_types' column
dealroom_data$each_investor_types <- NULL
##delete lead investor, no input
dealroom_data <- dealroom_data |>
  select(-lead_investors)
##total funding cleaning
dealroom_data <- dealroom_data |>
  rename(total_funding_eur_m = `total_funding_(eur_m)`)
dealroom_data <- dealroom_data |>
  mutate(total_funding_eur_m = na_if(total_funding_eur_m, "N/A")) |>
  mutate(total_funding_eur_m = as.numeric(as.character(total_funding_eur_m)))

##last_funding_date clean
#split 'last_funding_date' into 'last_funding_month' and 'last_funding_year'
dealroom_data <- dealroom_data |>
  mutate(last_funding_date = as.character(last_funding_date)) |>
  mutate(
    last_funding_month = ifelse(str_detect(last_funding_date, "/"), 
                                word(last_funding_date, 1, sep = "/"), NA),
    last_funding_year = ifelse(str_detect(last_funding_date, "/"), 
                               word(last_funding_date, 2, sep = "/"),
                               last_funding_date)
  )
#convert last_funding_month to factor
dealroom_data$last_funding_month <- factor(dealroom_data$last_funding_month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
#convert 'last_funding_year' to numeric
dealroom_data$last_funding_year <- as.numeric(dealroom_data$last_funding_year)

duplicate_names_count <- dealroom_data %>%
  group_by(name) %>%
  summarize(count = n()) %>%
  filter(count > 1) %>%
  summarise(total_duplicates = sum(count) - n())

#remove duplicate companies and only keep the most recent funding year
dealroom_data <- dealroom_data %>%
  group_by(name) %>%
  filter(last_funding_year == max(last_funding_year, na.rm = TRUE)) %>%
  ungroup()


#convert months to full month names
month_conversion <- c("jan" = "January", "feb" = "February", "mar" = "March", "apr" = "April", 
                      "may" = "May", "jun" = "June", "jul" = "July", "aug" = "August", 
                      "sep" = "September", "oct" = "October", "nov" = "November", "dec" = "December")

#split 'last_funding_date' and convert months
dealroom_data <- dealroom_data %>%
  mutate(last_funding_date = as.character(last_funding_date),
         last_funding_month = tolower(str_extract(last_funding_date, "^[a-z]+")),
         last_funding_year = as.numeric(str_extract(last_funding_date, "\\d{4}$"))) %>%
  mutate(last_funding_month = recode(last_funding_month, !!!month_conversion))

#convert 'last_funding_month' to factor with the correct levels
dealroom_data$last_funding_month <- factor(dealroom_data$last_funding_month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

#remover the original 'last_funding_date' column if no longer needed
dealroom_data$last_funding_date <- NULL

##first_funding_date clean
#process 'first_funding_date' to split into month and year, then convert month abbreviations
dealroom_data <- dealroom_data %>%
  mutate(first_funding_date = as.character(first_funding_date),
         first_funding_month = tolower(str_extract(first_funding_date, "^[a-z]+")),
         first_funding_year = as.numeric(str_extract(first_funding_date, "\\d{4}$"))) %>%
  mutate(first_funding_month = recode(first_funding_month, !!!month_conversion))

#convert 'first_funding_month' to factor with correct levels
dealroom_data$first_funding_month <- factor(dealroom_data$first_funding_month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

#remove the original 'first_funding_date' column
dealroom_data$first_funding_date <- NULL

##seed_year clean
dealroom_data$seed_year <- as.numeric(dealroom_data$seed_year)

#ensure 'ownerships' values are cleaned and standardized
dealroom_data <- dealroom_data |>
  mutate(ownerships = str_replace_all(ownerships, "[;,]", ";"),
         ownerships = tolower(ownerships))

#create binary columns for each unique ownerships type
unique_ownerships_types <- str_split(unique(dealroom_data$ownerships[!is.na(dealroom_data$ownerships) & dealroom_data$ownerships != ""]), ";", simplify = FALSE) |>
  unlist() |>
  unique() |>
  str_trim() |>
  na.omit()

for(ownerships_type in unique_ownerships_types) {
  column_name <- paste0("is_ownerships_", str_replace_all(ownerships_type, "[^[:alnum:]_]", "_"))
  dealroom_data[[column_name]] <- ifelse(is.na(dealroom_data$ownerships), NA_integer_, ifelse(str_detect(dealroom_data$ownerships, fixed(ownerships_type)), 1, 0))
}

dealroom_data$is_ownerships_not_known <- ifelse(is.na(dealroom_data$ownerships), NA_integer_, ifelse(str_detect(dealroom_data$ownerships, regex("not\\s+known", ignore_case = TRUE)), 1, 0))

#remove the original 'ownerships' column if no longer needed
dealroom_data$ownerships <- NULL


##revenue_model clean
#split the 'revenue_model' column and create binary columns for each model
dealroom_data <- transform(dealroom_data, revenue_model = strsplit(as.character(revenue_model), ";"))
models <- unique(unlist(dealroom_data$revenue_model))
#create new columns in the dataframe for each unique revenue model
for (model in models) {
  dealroom_data[paste("revenue_model", gsub(" & ", "_", gsub(" ", "_", model)), sep = "_")] <- 0
}
#populate the new columns with binary data
for (i in seq_len(nrow(dealroom_data))) {
  models_in_row <- dealroom_data$revenue_model[[i]]
  dealroom_data$revenue_model_saas_only[i] <- as.integer(all(models_in_row == "saas"))
  
  for (model in models_in_row) {
    col_name <- paste("revenue_model", gsub(" & ", "_", gsub(" ", "_", model)), sep = "_")
    dealroom_data[i, col_name] <- 1
  }
}
#remove the original 'revenue_model' list-column as it is no longer needed
dealroom_data <- dealroom_data |>
  select(-revenue_model)

##launch_year clean
dealroom_data$launch_year <- as.numeric(dealroom_data$launch_year)
##closing_year clean
dealroom_data$closing_year <- as.numeric(dealroom_data$closing_year)
##industries clean
#split the 'industries' column into multiple binary columns for each industry
dealroom_data <- dealroom_data |>
  mutate(industries_split = strsplit(as.character(industries), ";"))
#create a unique list of all industries encountered in the dataset
all_industries <- unique(unlist(dealroom_data$industries_split))
#create new columns in the dataframe for each unique industry
for (industry in all_industries) {
  col_name <- paste0("industry_", gsub(" & ", "_", gsub("[^[:alnum:]_]", "", tolower(industry))))
  dealroom_data[[col_name]] <- 0
}
#populate the new columns with binary data
for (i in seq_along(dealroom_data$industries_split)) {
  for (industry in dealroom_data$industries_split[[i]]) {
    col_name <- paste0("industry_", gsub(" & ", "_", gsub("[^[:alnum:]_]", "", tolower(industry))))
    dealroom_data[i, col_name] <- 1
  }
}
#now we can remove the original 'industries_split' column as it is no longer needed
dealroom_data <- select(dealroom_data, -industries_split, -industries)

##growth_stage clean
dealroom_data$growth_stage <- factor(dealroom_data$growth_stage, levels = unique(dealroom_data$growth_stage))

##employees clean
dealroom_data$employees[dealroom_data$employees == "'n.a."] <- NA
dealroom_data$employees <- factor(dealroom_data$employees, levels = unique(dealroom_data$employees))

##employees year clean
#split the 'employees' column into separate columns for each year
employee_counts <- strsplit(as.character(dealroom_data$employees_.2016.2017.2018.2019.2020.2021.2022.2023.2024.), ";")
#convert "n/a" to NA and the rest to numeric
employee_counts <- lapply(employee_counts, function(x) ifelse(x == "n/a", NA, as.numeric(x)))
#create a dataframe from the list
employee_df <- do.call(rbind, employee_counts)
#set the column names for the new dataframe
colnames(employee_df) <- paste("employees", 2016:2024, sep = "_")
#convert the matrix to a dataframe if needed
employee_df <- as.data.frame(employee_df)
#bind the new employee columns to the original dataframe
dealroom_data <- cbind(dealroom_data, employee_df)
#remove the original 'EMPLOYEES' column
dealroom_data$employees_.2016.2017.2018.2019.2020.2021.2022.2023.2024. <- NULL

##EBITDA year column clean
ebitda_counts <- strsplit(as.character(dealroom_data$ebitda_.2016.2017.2018.2019.2020.2021.2022.2023.2024.2025.), ";")
ebitda_counts <- lapply(ebitda_counts, function(x) ifelse(x == "n/a", NA, as.numeric(x)))
ebitda_df <- do.call(rbind, ebitda_counts)
colnames(ebitda_df) <- paste("ebitda", 2016:2025, sep = "_")
ebitda_df <- as.data.frame(ebitda_df)
dealroom_data <- cbind(dealroom_data, ebitda_df)
dealroom_data$ebitda_.2016.2017.2018.2019.2020.2021.2022.2023.2024.2025. <- NULL

##EBITDA margin year clean 
ebitda_margin_counts <- strsplit(as.character(dealroom_data$ebitda_margin_.2015.2016.2017.2018.2019.2020.2021.2022.2023.2024.2025.), ";")
ebitda_margin_counts <- lapply(ebitda_margin_counts, function(x) ifelse(x == "n/a", NA, as.numeric(x)))
ebitda_margin_df <- do.call(rbind, ebitda_margin_counts)
colnames(ebitda_margin_df) <- paste("ebitda_margin", 2015:2025, sep = "_")
ebitda_margin_df <- as.data.frame(ebitda_margin_df)
dealroom_data <- cbind(dealroom_data, ebitda_margin_df)
dealroom_data$ebitda_margin_.2015.2016.2017.2018.2019.2020.2021.2022.2023.2024.2025. <- NULL

##revenue year clean
revenue_counts <- strsplit(as.character(dealroom_data$revenue_.2016.2017.2018.2019.2020.2021.2022.2023.2024.2025.), ";")
revenue_counts <- lapply(revenue_counts, function(x) ifelse(x == "n/a", NA, as.numeric(x)))
revenue_df <- do.call(rbind, revenue_counts)
colnames(revenue_df) <- paste("revenue", 2016:2025, sep = "_")
revenue_df <- as.data.frame(revenue_df)
dealroom_data <- cbind(dealroom_data, revenue_df)
dealroom_data$revenue_.2016.2017.2018.2019.2020.2021.2022.2023.2024.2025. <- NULL

##revenue growth year clean
revenue_growth_counts <- strsplit(as.character(dealroom_data$revenue_growth_.2015.2016.2017.2018.2019.2020.2021.2022.2023.2024.2025.), ";")
revenue_growth_counts <- lapply(revenue_growth_counts, function(x) ifelse(x == "n/a", NA, as.numeric(x)))
revenue_growth_df <- do.call(rbind, revenue_growth_counts)
colnames(revenue_growth_df) <- paste("revenue_growth", 2015:2025, sep = "_")
revenue_growth_df <- as.data.frame(revenue_growth_df)
dealroom_data <- cbind(dealroom_data, revenue_growth_df)
dealroom_data$revenue_growth_.2015.2016.2017.2018.2019.2020.2021.2022.2023.2024.2025. <- NULL

##valuation clean
#split in min and max and in case of a single value, both values are the same
dealroom_data <- dealroom_data |>
  mutate(valuation_split = strsplit(as.character(valuation_.eur.), "-"),
         min_valuation = sapply(valuation_split, function(x) as.numeric(x[1])),
         max_valuation = sapply(valuation_split, function(x) ifelse(length(x) > 1, as.numeric(x[2]), as.numeric(x[1]))))
dealroom_data$valuation_split <- NULL
#remove the original 'valuation_.eur.' column
dealroom_data$valuation_.eur. <- NULL

##valuation date clean
#split the 'valuation_date' into two new columns: 'valuation_month' and 'valuation_year'
dealroom_data <- dealroom_data |>
  mutate(valuation_month = ifelse(grepl("/", valuation_date), word(valuation_date, 1, sep = "/"), NA),
         valuation_year = ifelse(grepl("/", valuation_date), word(valuation_date, 2, sep = "/"), NA))
#convert 'valuation_year' to numeric
dealroom_data$valuation_year <- as.numeric(dealroom_data$valuation_year)
#convert abbreviated month names to full names or numeric month values
month_conversion <- c("jan" = "January", "feb" = "February", "mar" = "March", "apr" = "April",
                      "may" = "May", "jun" = "June", "jul" = "July", "aug" = "August",
                      "sep" = "September", "oct" = "October", "nov" = "November", "dec" = "December")

dealroom_data$valuation_month <- tolower(dealroom_data$valuation_month) |>
  map_chr(~month_conversion[.])
#remove the original 'valuation_date' column
dealroom_data$valuation_date <- NULL

##historical valuation clean
#adjusted function to handle NA or empty strings
process_valuation <- function(valuation_str) {
  #return NA directly if input is NA or an empty string
  if (is.na(valuation_str) || valuation_str == "") {
    return(list(min = NA, max = NA))
  }
  
  valuations <- str_split(valuation_str, ";", simplify = FALSE)[[1]]
  valuations <- lapply(valuations, function(v) {
    #check if v is not empty and contains '-'
    if (!is.na(v) && v != "" && str_detect(v, "-")) {
      vals <- str_split(v, "-", simplify = TRUE) |> as.numeric()
      c(min = vals[1] * 1e6, max = vals[2] * 1e6)
    } else if (!is.na(v) && v != "") {
      val <- as.numeric(v) * 1e6
      c(min = val, max = val)
    } else {
      c(min = NA, max = NA)
    }
  })
  return(valuations)
}
#apply the function to each row in the column
valuation_results <- lapply(dealroom_data$historical_valuations_._values_eur_m, process_valuation)
#continue with the rest of the data processing as previously described.
#find the maximum number of valuations for any company
max_valuations <- max(sapply(valuation_results, length), na.rm = TRUE)
#initialize a list to hold the expanded valuation data
expanded_valuations <- vector("list", length = max_valuations * 2)
#populate the list with data
for (i in 1:max_valuations) {
  expanded_valuations[[i * 2 - 1]] <- sapply(valuation_results, function(x) if (length(x) >= i) x[[i]]['min'] else NA, simplify = TRUE)
  names(expanded_valuations)[i * 2 - 1] <- paste("valuation", i, "min", sep = "_")
  expanded_valuations[[i * 2]] <- sapply(valuation_results, function(x) if (length(x) >= i) x[[i]]['max'] else NA, simplify = TRUE)
  names(expanded_valuations)[i * 2] <- paste("valuation", i, "max", sep = "_")
}
#combine the original dataframe with the expanded valuation data
dealroom_data_expanded <- cbind(dealroom_data, do.call(data.frame, expanded_valuations))
#calculate the average valuation for each timestamp and replace NaN with NA
for (i in 1:max_valuations) {
  min_col_name <- paste("valuation", i, "min", sep = "_")
  max_col_name <- paste("valuation", i, "max", sep = "_")
  avg_col_name <- paste("valuation", i, "avg", sep = "_")
  dealroom_data_expanded[[avg_col_name]] <- rowMeans(dealroom_data_expanded[, c(min_col_name, max_col_name)], na.rm = TRUE)
  dealroom_data_expanded[[avg_col_name]] <- ifelse(is.nan(dealroom_data_expanded[[avg_col_name]]), NA, dealroom_data_expanded[[avg_col_name]])
}
#filter out min and max columns.
columns_to_keep <- c(names(dealroom_data), grep("avg", names(dealroom_data_expanded), value = TRUE))
dealroom_data <- dealroom_data_expanded[, columns_to_keep]
#remove the original 'historical_valuations_._values_eur_m' column
dealroom_data$historical_valuations_._values_eur_m <- NULL

##ev.revenue year clean
ev_revenue_counts <- strsplit(as.character(dealroom_data$ev.revenue_.2017.2018.2019.2020.2021.2022.2023.2024.2025.), ";")
ev_revenue_counts <- lapply(ev_revenue_counts, function(x) ifelse(x == "n/a", NA, as.numeric(x)))
ev_revenue_df <- do.call(rbind, ev_revenue_counts)
colnames(ev_revenue_df) <- paste("ev_revenue", 2017:2025, sep = "_")
ev_revenue_df <- as.data.frame(ev_revenue_df)
dealroom_data <- cbind(dealroom_data, ev_revenue_df)
dealroom_data$ev.revenue_.2017.2018.2019.2020.2021.2022.2023.2024.2025. <- NULL

##ev.ebitda year clean
ev_ebitda_counts <- strsplit(as.character(dealroom_data$ev.ebitda_.2017.2018.2019.2020.2021.2022.2023.2024.2025.), ";")
ev_ebitda_counts <- lapply(ev_ebitda_counts, function(x) ifelse(x == "n/a", NA, as.numeric(x)))
ev_ebitda_df <- do.call(rbind, ev_ebitda_counts)
colnames(ev_ebitda_df) <- paste("ev_ebitda", 2017:2025, sep = "_")
ev_ebitda_df <- as.data.frame(ev_ebitda_df)
dealroom_data <- cbind(dealroom_data, ev_ebitda_df)
dealroom_data$ev.ebitda_.2017.2018.2019.2020.2021.2022.2023.2024.2025. <- NULL

##total_rounds_number clean
dealroom_data$total_rounds_number <- as.numeric(dealroom_data$total_rounds_number)

##founders split
#create a list column where each element is a vector of founders for each company
dealroom_data <- dealroom_data |>
  mutate(founders_list = str_split(founders, pattern = ";"))
#find the maximum number of founders in any single row
max_founders <- max(sapply(dealroom_data$founders_list, length))
#create separate columns for each founder
for (i in 1:max_founders) {
  dealroom_data <- dealroom_data |>
    mutate(!!paste0("founder_", i) := map_chr(founders_list, ~ ifelse(length(.) >= i, .[i], NA)))
}

#update the code to correctly handle 'no founders given'
dealroom_data <- dealroom_data %>%
  mutate(no_founders_given = ifelse(is.na(founders) | founders == "" | founders == "NA", 1, 0),
         #ensure 'founders' is treated as NA when it should be
         founders = ifelse(founders == "" | founders == "NA", NA, founders),
         #recalculate the number of founders based on the list column
         num_founders = map_int(founders_list, ~ sum(!is.na(.))),
         #adjust conditions based on the corrected 'num_founders'
         is_solo_founder = ifelse(num_founders == 1, 1, 0),
         is_two_founders = ifelse(num_founders == 2, 1, 0),
         is_three_plus_founders = ifelse(num_founders >= 3, 1, 0))

#ensure it is accurately reflecting cases with no founder information
dealroom_data <- dealroom_data %>%
  mutate(no_founders_given = ifelse(num_founders == 0, 1, 0))


#combine all founder columns into a single vector
all_founders <- dealroom_data |>
  select(starts_with("founder_")) |>
  pivot_longer(cols = everything(), values_drop_na = TRUE) |>
  pull(value)
#count the occurrences of each founder's name
founder_counts <- table(all_founders)
#convert to a dataframe, sort, and get top 10
top_10_founders <- as.data.frame(founder_counts) |>
  arrange(desc(Freq)) |>
  head(10)
#rename columns for clarity
names(top_10_founders) <- c("Founder", "Occurrences")
#print the top 10 most recurring founders
print(top_10_founders)
#remove the original 'founders' columns
dealroom_data <- dealroom_data |>
  select(-founders, -founders_list)

##founder statuses
dealroom_data <- dealroom_data |>
  mutate(founders_statuses_list = str_split(founders_statuses, pattern = ";"))
#create separate columns for each founder's status
for (i in 1:max_founders) {
  dealroom_data <- dealroom_data |>
    mutate(!!paste0("founder_status_", i) := map_chr(founders_statuses_list, ~ ifelse(length(.) >= i, .[i], NA)))
}
#remove the original 'founder statuses' and 'founders_statuses_list' columns if they are no longer needed
dealroom_data <- select(dealroom_data, -founders_statuses, -founders_statuses_list)

#general function to split and expand a column based on semicolon as the separator
split_and_expand <- function(data, column_name, max_count) {
  #create a list column for each attribute
  list_col_name <- paste0(column_name, "_list")
  data <- data %>%
    mutate(!!list_col_name := str_split(.[[column_name]], pattern = ";"))
  #create separate columns for each attribute
  for (i in 1:max_count) {
    new_col_name <- paste0(column_name, "_", i)
    data <- data %>%
      mutate(!!new_col_name := map_chr(.[[list_col_name]], ~ ifelse(length(.) >= i, .[i], NA)))
  }
  #remove the original column and the temporary list column if they are no longer needed
  data <- select(data, -!!sym(column_name), -!!sym(list_col_name))
  return(data)
}

dealroom_data <- split_and_expand(dealroom_data, "founders_genders", max_founders)
dealroom_data <- split_and_expand(dealroom_data, "founders_is_serial", max_founders)
dealroom_data <- split_and_expand(dealroom_data, "founders_backgrounds", max_founders)
dealroom_data <- split_and_expand(dealroom_data, "founders_universities", max_founders)
dealroom_data <- split_and_expand(dealroom_data, "founders_company_experience", max_founders)
dealroom_data <- split_and_expand(dealroom_data, "founders_first_degree_year", max_founders)
dealroom_data <- split_and_expand(dealroom_data, "founders_linkedin", max_founders)
dealroom_data <- split_and_expand(dealroom_data, "founders_experience", max_founders)
dealroom_data <- split_and_expand(dealroom_data, "founders_founded_companies_total_funding", max_founders)
dealroom_data <- split_and_expand(dealroom_data, "founders_years_of_education", max_founders)
dealroom_data <- split_and_expand(dealroom_data, "is_founders_first_company", max_founders)
dealroom_data <- split_and_expand(dealroom_data, "founders_strength", max_founders)
#n/a to NA
columns_to_convert <- c("founders_statuses", "founders_is_serial",
                        "founders_backgrounds", "founders_universities", "founders_company_experience",
                        "founders_first_degree_year", "founders_linkedin", "founders_experience",
                        "founders_founded_companies_total_funding", "founders_years_of_education",
                        "is_founders_first_company", "founders_strength")
#loop through each column and its expanded counterparts
for (col_name in columns_to_convert) {
  cols <- names(dealroom_data)[grepl(paste0("^", col_name, "_"), names(dealroom_data))]
  for (col in cols) {
    dealroom_data[[col]] <- ifelse(dealroom_data[[col]] == "n/a", NA, dealroom_data[[col]])
  }
}
##company status clean to factor
dealroom_data$company_status <- factor(dealroom_data$company_status, levels = unique(dealroom_data$company_status))

#ensure the technologies column is character, not factor
dealroom_data$technologies <- as.character(dealroom_data$technologies)

#replace NA in 'technologies' with unique placeholder to handle them separately
dealroom_data$technologies[is.na(dealroom_data$technologies)] <- "NA_placeholder"

#split column 'technologies' into list where each element is a vector of technologies
tech_list <- strsplit(dealroom_data$technologies, ";", fixed = TRUE)

#identify all unique technologies, excluding the placeholder for NA
unique_techs <- unique(unlist(tech_list))
unique_techs <- unique_techs[unique_techs != "NA_placeholder"]

#function to check the presence of each technology and return 1, 0, or NA
check_presence <- function(techs, tech) {
  if("NA_placeholder" %in% techs) {
    return(NA)
  } else {
    return(as.integer(tech %in% techs))
  }
}

#create new column for each technology
for (tech in unique_techs) {
  #remove spaces and special characters from tech names to create valid column names
  clean_tech <- gsub("[^[:alnum:]_]", "", gsub(" ", "_", tech))
  column_name <- paste0("is_", clean_tech)
  
  #apply function to each element of tech_list, passing the current technology as an argument
  dealroom_data[[column_name]] <- sapply(tech_list, check_presence, tech = tech)
}

#remove the original 'technologies' column and the placeholder
dealroom_data$technologies <- NULL

#ensure the sdgs column is character, not factor
dealroom_data$sdgs <- as.character(dealroom_data$sdgs)

#replace NA in 'sdgs' with a unique placeholder to handle them separately
dealroom_data$sdgs[is.na(dealroom_data$sdgs)] <- "NA_placeholder"

#split the 'sdgs' column into a list where each element is a vector of SDGs
sdg_list <- strsplit(dealroom_data$sdgs, ";", fixed = TRUE)

#identify all unique SDGs, excluding the placeholder for NA
unique_sdgs <- unique(unlist(sdg_list))
unique_sdgs <- unique_sdgs[unique_sdgs != "NA_placeholder"]

#function to check the presence of each SDG and return 1, 0, or NA
check_presence <- function(sdgs, sdg) {
  if("NA_placeholder" %in% sdgs) {
    return(NA)  #return NA if the placeholder is present, indicating the original value was NA
  } else {
    return(as.integer(sdg %in% sdgs))
  }
}

#create a new column for each SDG
for (sdg in unique_sdgs) {
  #clean up SDG names to create valid column names
  clean_sdg <- gsub("[^[:alnum:]_]", "", gsub(" ", "_", sdg))
  column_name <- paste0("is_", clean_sdg)
  
  #apply the function to each element of sdg_list, passing the current SDG as an argument
  dealroom_data[[column_name]] <- sapply(sdg_list, check_presence, sdg = sdg)
}

#remove the original 'sdgs' column and the placeholder
dealroom_data$sdgs <- NULL

#######
##year_company_became_unicorn make numeric
dealroom_data$year_company_became_unicorn <- as.numeric(dealroom_data$year_company_became_unicorn)

##year_company_became_future_unicorn
dealroom_data$year_company_became_future_unicorn <- as.numeric(dealroom_data$year_company_became_future_unicorn)

##dealroom_signal_._completeness numeric
dealroom_data$dealroom_signal_._completeness <- as.numeric(dealroom_data$dealroom_signal_._completeness)
##dealroom_signal_._founding_team_score numeric
dealroom_data$dealroom_signal_._founding_team_score <- as.numeric(dealroom_data$dealroom_signal_._founding_team_score)
##dealroom_signal_._growth_rate
dealroom_data$dealroom_signal_._growth_rate <- as.numeric(dealroom_data$dealroom_signal_._growth_rate)
##dealroom_signal_._timing
dealroom_data$dealroom_signal_._timing <- as.numeric(dealroom_data$dealroom_signal_._timing)
##number_of_patents
dealroom_data$number_of_patents <- as.numeric(dealroom_data$number_of_patents)


#### PART 2 - Turn everything into the right type ####
#has_investor to factor
dealroom_data$has_investor <- factor(dealroom_data$has_investor)
#also to factor: is_top_investor	number_top_investors	is_venture_capital	is_angel	is_accelerator	is_angel_fund	is_corporate	is_private_equity	is_crowdfunding	is_corporate_venture_fund	is_investment_fund	is_government	is_university	is_incubator	is_non.profit	is_family_office	is_government_._non.profit	is_advisor	is_workspace	is_sovereign_wealth_fund	is_service_provider	is_company	is_workspace_location	is_event is_ownerships_angel	is_ownerships_venture_capital	is_ownerships_subsidiary	is_ownerships_accelerator	is_ownerships_private_equity	is_ownerships_crowdfunded	is_ownerships_public	is_ownerships_not_known	is_ownerships_bootstrapped	revenue_model_saas	revenue_model_manufacturing	revenue_model_marketplace_&_ecommerce	revenue_model_saas_only	industry_enterprisesoftware	industry_legal	industry_fintech	industry_semiconductors	industry_health	industry_fashion	industry_energy	industry_media	industry_security	industry_realestate	industry_education	industry_NA	industry_space	industry_wellnessbeauty	industry_jobsrecruitment	industry_travel	industry_sports	industry_robotics	industry_marketing	industry_transportation	industry_kids	industry_gaming	industry_eventtech	industry_serviceprovider	industry_food	industry_telecom	industry_chemicals	industry_music	industry_homeliving	industry_engineeringandmanufacturingequipment	industry_hosting	industry_dating	industry_consumerelectronics
dealroom_data$is_top_investor <- factor(dealroom_data$is_top_investor)
dealroom_data$number_top_investors <- as.numeric(dealroom_data$number_top_investors)
dealroom_data$is_venture_capital <- factor(dealroom_data$is_venture_capital)
dealroom_data$is_angel <- factor(dealroom_data$is_angel)
dealroom_data$is_accelerator <- factor(dealroom_data$is_accelerator)
dealroom_data$is_angel_fund <- factor(dealroom_data$is_angel_fund)
dealroom_data$is_corporate <- factor(dealroom_data$is_corporate)
dealroom_data$is_private_equity <- factor(dealroom_data$is_private_equity)
dealroom_data$is_crowdfunding <- factor(dealroom_data$is_crowdfunding)
dealroom_data$is_corporate_venture_fund <- factor(dealroom_data$is_corporate_venture_fund)
dealroom_data$is_investment_fund <- factor(dealroom_data$is_investment_fund)
dealroom_data$is_government <- factor(dealroom_data$is_government)
dealroom_data$is_university <- factor(dealroom_data$is_university)
dealroom_data$is_incubator <- factor(dealroom_data$is_incubator)
dealroom_data$is_non.profit <- factor(dealroom_data$is_non.profit)
dealroom_data$is_family_office <- factor(dealroom_data$is_family_office)
dealroom_data$is_government_._non.profit <- factor(dealroom_data$is_government_._non.profit)
dealroom_data$is_advisor <- factor(dealroom_data$is_advisor)
dealroom_data$is_workspace <- factor(dealroom_data$is_workspace)
dealroom_data$is_sovereign_wealth_fund <- factor(dealroom_data$is_sovereign_wealth_fund)
dealroom_data$is_service_provider <- factor(dealroom_data$is_service_provider)
dealroom_data$is_company <- factor(dealroom_data$is_company)
dealroom_data$is_workspace_location <- factor(dealroom_data$is_workspace_location)
dealroom_data$is_event <- factor(dealroom_data$is_event)
dealroom_data$is_ownerships_angel <- factor(dealroom_data$is_ownerships_angel)
dealroom_data$is_ownerships_venture_capital <- factor(dealroom_data$is_ownerships_venture_capital)
dealroom_data$is_ownerships_subsidiary <- factor(dealroom_data$is_ownerships_subsidiary)
dealroom_data$is_ownerships_accelerator <- factor(dealroom_data$is_ownerships_accelerator)
dealroom_data$is_ownerships_private_equity <- factor(dealroom_data$is_ownerships_private_equity)
dealroom_data$is_ownerships_crowdfunded <- factor(dealroom_data$is_ownerships_crowdfunded)
dealroom_data$is_ownerships_public <- factor(dealroom_data$is_ownerships_public)
dealroom_data$is_ownerships_not_known <- factor(dealroom_data$is_ownerships_not_known)
dealroom_data$is_ownerships_bootstrapped <- factor(dealroom_data$is_ownerships_bootstrapped)
dealroom_data$revenue_model_saas <- factor(dealroom_data$revenue_model_saas)
dealroom_data$revenue_model_manufacturing <- factor(dealroom_data$revenue_model_manufacturing)
dealroom_data$`revenue_model_marketplace_&_ecommerce` <- factor(dealroom_data$`revenue_model_marketplace_&_ecommerce`)
dealroom_data$revenue_model_saas_only <- factor(dealroom_data$revenue_model_saas_only)
dealroom_data$industry_enterprisesoftware <- factor(dealroom_data$industry_enterprisesoftware)
dealroom_data$industry_legal <- factor(dealroom_data$industry_legal)
dealroom_data$industry_fintech <- factor(dealroom_data$industry_fintech)
dealroom_data$industry_semiconductors <- factor(dealroom_data$industry_semiconductors)
dealroom_data$industry_health <- factor(dealroom_data$industry_health)
dealroom_data$industry_fashion <- factor(dealroom_data$industry_fashion)
dealroom_data$industry_energy <- factor(dealroom_data$industry_energy)
dealroom_data$industry_media <- factor(dealroom_data$industry_media)
dealroom_data$industry_security <- factor(dealroom_data$industry_security)
dealroom_data$industry_realestate <- factor(dealroom_data$industry_realestate)
dealroom_data$industry_education <- factor(dealroom_data$industry_education)
dealroom_data$industry_space <- factor(dealroom_data$industry_space)
dealroom_data$industry_wellnessbeauty <- factor(dealroom_data$industry_wellnessbeauty)
dealroom_data$industry_jobsrecruitment <- factor(dealroom_data$industry_jobsrecruitment)
dealroom_data$industry_travel <- factor(dealroom_data$industry_travel)
dealroom_data$industry_sports <- factor(dealroom_data$industry_sports)
dealroom_data$industry_robotics <- factor(dealroom_data$industry_robotics)
dealroom_data$industry_marketing <- factor(dealroom_data$industry_marketing)
dealroom_data$industry_transportation <- factor(dealroom_data$industry_transportation)
dealroom_data$industry_kids <- factor(dealroom_data$industry_kids)
dealroom_data$industry_gaming <- factor(dealroom_data$industry_gaming)
dealroom_data$industry_eventtech <- factor(dealroom_data$industry_eventtech)
dealroom_data$industry_serviceprovider <- factor(dealroom_data$industry_serviceprovider)
dealroom_data$industry_food <- factor(dealroom_data$industry_food)
dealroom_data$industry_telecom <- factor(dealroom_data$industry_telecom)
dealroom_data$industry_chemicals <- factor(dealroom_data$industry_chemicals)
dealroom_data$industry_music <- factor(dealroom_data$industry_music)
dealroom_data$industry_homeliving <- factor(dealroom_data$industry_homeliving)
dealroom_data$industry_engineeringandmanufacturingequipment <- factor(dealroom_data$industry_engineeringandmanufacturingequipment)
dealroom_data$industry_hosting <- factor(dealroom_data$industry_hosting)
dealroom_data$industry_dating <- factor(dealroom_data$industry_dating)
dealroom_data$industry_consumerelectronics <- factor(dealroom_data$industry_consumerelectronics)
dealroom_data$founder_status_1 <- factor(dealroom_data$founder_status_1)
dealroom_data$founder_status_2 <- factor(dealroom_data$founder_status_2)
dealroom_data$founder_status_3 <- factor(dealroom_data$founder_status_3)
dealroom_data$founder_status_4 <- factor(dealroom_data$founder_status_4)
dealroom_data$founder_status_5 <- factor(dealroom_data$founder_status_5)
dealroom_data$founder_status_6 <- factor(dealroom_data$founder_status_6)
dealroom_data$founder_status_7 <- factor(dealroom_data$founder_status_7)
dealroom_data$founder_status_8 <- factor(dealroom_data$founder_status_8)
dealroom_data$founder_status_9 <- factor(dealroom_data$founder_status_9)
dealroom_data$founder_status_10 <- factor(dealroom_data$founder_status_10)
dealroom_data$founders_genders_1 <- factor(dealroom_data$founders_genders_1)
dealroom_data$founders_genders_2 <- factor(dealroom_data$founders_genders_2)
dealroom_data$founders_genders_3 <- factor(dealroom_data$founders_genders_3)
dealroom_data$founders_genders_4 <- factor(dealroom_data$founders_genders_4)
dealroom_data$founders_genders_5 <- factor(dealroom_data$founders_genders_5)
dealroom_data$founders_genders_6 <- factor(dealroom_data$founders_genders_6)
dealroom_data$founders_genders_7 <- factor(dealroom_data$founders_genders_7)
dealroom_data$founders_genders_8 <- factor(dealroom_data$founders_genders_8)
dealroom_data$founders_genders_9 <- factor(dealroom_data$founders_genders_9)
dealroom_data$founders_genders_10 <- factor(dealroom_data$founders_genders_10)
#founders_is_serial_1 until _10 to factor
dealroom_data$founders_is_serial_1 <- factor(dealroom_data$founders_is_serial_1)
dealroom_data$founders_is_serial_2 <- factor(dealroom_data$founders_is_serial_2)
dealroom_data$founders_is_serial_3 <- factor(dealroom_data$founders_is_serial_3)
dealroom_data$founders_is_serial_4 <- factor(dealroom_data$founders_is_serial_4)
dealroom_data$founders_is_serial_5 <- factor(dealroom_data$founders_is_serial_5)
dealroom_data$founders_is_serial_6 <- factor(dealroom_data$founders_is_serial_6)
dealroom_data$founders_is_serial_7 <- factor(dealroom_data$founders_is_serial_7)
dealroom_data$founders_is_serial_8 <- factor(dealroom_data$founders_is_serial_8)
dealroom_data$founders_is_serial_9 <- factor(dealroom_data$founders_is_serial_9)
dealroom_data$founders_is_serial_10 <- factor(dealroom_data$founders_is_serial_10)
dealroom_data$founders_backgrounds_1 <- factor(dealroom_data$founders_backgrounds_1)
dealroom_data$founders_backgrounds_2 <- factor(dealroom_data$founders_backgrounds_2)
dealroom_data$founders_backgrounds_3 <- factor(dealroom_data$founders_backgrounds_3)
dealroom_data$founders_backgrounds_4 <- factor(dealroom_data$founders_backgrounds_4)
dealroom_data$founders_backgrounds_5 <- factor(dealroom_data$founders_backgrounds_5)
dealroom_data$founders_backgrounds_6 <- factor(dealroom_data$founders_backgrounds_6)
dealroom_data$founders_backgrounds_7 <- factor(dealroom_data$founders_backgrounds_7)
dealroom_data$founders_backgrounds_8 <- factor(dealroom_data$founders_backgrounds_8)
dealroom_data$founders_backgrounds_9 <- factor(dealroom_data$founders_backgrounds_9)
dealroom_data$founders_backgrounds_10 <- factor(dealroom_data$founders_backgrounds_10)
dealroom_data$is_founders_first_company_1 <- factor(dealroom_data$is_founders_first_company_1)
dealroom_data$is_founders_first_company_2 <- factor(dealroom_data$is_founders_first_company_2)
dealroom_data$is_founders_first_company_3 <- factor(dealroom_data$is_founders_first_company_3)
dealroom_data$is_founders_first_company_4 <- factor(dealroom_data$is_founders_first_company_4)
dealroom_data$is_founders_first_company_5 <- factor(dealroom_data$is_founders_first_company_5)
dealroom_data$is_founders_first_company_6 <- factor(dealroom_data$is_founders_first_company_6)
dealroom_data$is_founders_first_company_7 <- factor(dealroom_data$is_founders_first_company_7)
dealroom_data$is_founders_first_company_8 <- factor(dealroom_data$is_founders_first_company_8)
dealroom_data$is_founders_first_company_9 <- factor(dealroom_data$is_founders_first_company_9)
dealroom_data$is_founders_first_company_10 <- factor(dealroom_data$is_founders_first_company_10)
dealroom_data$founders_strength_1 <- factor(dealroom_data$founders_strength_1)
dealroom_data$founders_strength_2 <- factor(dealroom_data$founders_strength_2)
dealroom_data$founders_strength_3 <- factor(dealroom_data$founders_strength_3)
dealroom_data$founders_strength_4 <- factor(dealroom_data$founders_strength_4)
dealroom_data$founders_strength_5 <- factor(dealroom_data$founders_strength_5)
dealroom_data$founders_strength_6 <- factor(dealroom_data$founders_strength_6)
dealroom_data$founders_strength_7 <- factor(dealroom_data$founders_strength_7)
dealroom_data$founders_strength_8 <- factor(dealroom_data$founders_strength_8)
dealroom_data$founders_strength_9 <- factor(dealroom_data$founders_strength_9)
dealroom_data$founders_strength_10 <- factor(dealroom_data$founders_strength_10)
dealroom_data$is_artificial_intelligence <- factor(dealroom_data$is_artificial_intelligence)
dealroom_data$is_big_data <- factor(dealroom_data$is_big_data)
dealroom_data$is_blockchain <- factor(dealroom_data$is_blockchain)
dealroom_data$is_deep_tech <- factor(dealroom_data$is_deep_tech)
dealroom_data$is_mobile_app <- factor(dealroom_data$is_mobile_app)
dealroom_data$is_augmented_reality <- factor(dealroom_data$is_augmented_reality)
dealroom_data$is_iot_internetofthings <- factor(dealroom_data$is_iot_internetofthings)
dealroom_data$is_connected_device <- factor(dealroom_data$is_connected_device)
dealroom_data$is_computer_vision <- factor(dealroom_data$is_computer_vision)
dealroom_data$is_natural_language_processing <- factor(dealroom_data$is_natural_language_processing)
dealroom_data$is_deep_learning <- factor(dealroom_data$is_deep_learning)
dealroom_data$is_machine_learning <- factor(dealroom_data$is_machine_learning)
dealroom_data$is_virtual_reality <- factor(dealroom_data$is_virtual_reality)
dealroom_data$is_3d_technology <- factor(dealroom_data$is_3d_technology)
dealroom_data$is_hardware <- factor(dealroom_data$is_hardware)
dealroom_data$is_recognition_technology <- factor(dealroom_data$is_recognition_technology)
dealroom_data$is_autonomous__sensor_tech <- factor(dealroom_data$is_autonomous__sensor_tech)
dealroom_data$is_quantum_technologies <- factor(dealroom_data$is_quantum_technologies)
dealroom_data$is_nanotech <- factor(dealroom_data$is_nanotech)
dealroom_data$is_life_on_land_15 <- factor(dealroom_data$is_life_on_land_15)
dealroom_data$is_responsible_consumption_and_production_12 <- factor(dealroom_data$is_responsible_consumption_and_production_12)
dealroom_data$is_climate_action_13 <- factor(dealroom_data$is_climate_action_13)
dealroom_data$is_good_health_and_wellbeing_3 <- factor(dealroom_data$is_good_health_and_wellbeing_3)
dealroom_data$is_sustainable_cities_and_communities_11 <- factor(dealroom_data$is_sustainable_cities_and_communities_11)
dealroom_data$is_gender_equality_5 <- factor(dealroom_data$is_gender_equality_5)
dealroom_data$is_industry_innovation_and_infrastructure_9 <- factor(dealroom_data$is_industry_innovation_and_infrastructure_9)
dealroom_data$is_reduced_inequalities_10 <- factor(dealroom_data$is_reduced_inequalities_10)
dealroom_data$is_clean_water_and_sanitation_6 <- factor(dealroom_data$is_clean_water_and_sanitation_6)
dealroom_data$is_zero_hunger_2 <- factor(dealroom_data$is_zero_hunger_2)
dealroom_data$is_quality_education_4 <- factor(dealroom_data$is_quality_education_4)
dealroom_data$is_life_below_water_14 <- factor(dealroom_data$is_life_below_water_14)
dealroom_data$is_peace_justice_and_strong_institutions_16 <- factor(dealroom_data$is_peace_justice_and_strong_institutions_16)
dealroom_data$is_no_poverty_1 <- factor(dealroom_data$is_no_poverty_1)
dealroom_data$is_affordable_and_clean_energy_7 <- factor(dealroom_data$is_affordable_and_clean_energy_7)
dealroom_data$is_partnerships_17 <- factor(dealroom_data$is_partnerships_17)
dealroom_data$is_decent_work_and_economic_growth_8 <- factor(dealroom_data$is_decent_work_and_economic_growth_8)
dealroom_data$founders_experience_1 <- as.numeric(dealroom_data$founders_experience_1)
dealroom_data$founders_experience_2 <- as.numeric(dealroom_data$founders_experience_2)
dealroom_data$founders_experience_3 <- as.numeric(dealroom_data$founders_experience_3)
dealroom_data$founders_experience_4 <- as.numeric(dealroom_data$founders_experience_4)
dealroom_data$founders_experience_5 <- as.numeric(dealroom_data$founders_experience_5)
dealroom_data$founders_experience_6 <- as.numeric(dealroom_data$founders_experience_6)
dealroom_data$founders_experience_7 <- as.numeric(dealroom_data$founders_experience_7)
dealroom_data$founders_experience_8 <- as.numeric(dealroom_data$founders_experience_8)
dealroom_data$founders_experience_9 <- as.numeric(dealroom_data$founders_experience_9)
dealroom_data$founders_experience_10 <- as.numeric(dealroom_data$founders_experience_10)
dealroom_data$founders_first_degree_year_1 <- as.numeric(dealroom_data$founders_first_degree_year_1)
dealroom_data$founders_first_degree_year_2 <- as.numeric(dealroom_data$founders_first_degree_year_2)
dealroom_data$founders_first_degree_year_3 <- as.numeric(dealroom_data$founders_first_degree_year_3)
dealroom_data$founders_first_degree_year_4 <- as.numeric(dealroom_data$founders_first_degree_year_4)
dealroom_data$founders_first_degree_year_5 <- as.numeric(dealroom_data$founders_first_degree_year_5)
dealroom_data$founders_first_degree_year_6 <- as.numeric(dealroom_data$founders_first_degree_year_6)
dealroom_data$founders_first_degree_year_7 <- as.numeric(dealroom_data$founders_first_degree_year_7)
dealroom_data$founders_first_degree_year_8 <- as.numeric(dealroom_data$founders_first_degree_year_8)
dealroom_data$founders_first_degree_year_9 <- as.numeric(dealroom_data$founders_first_degree_year_9)
dealroom_data$founders_first_degree_year_10 <- as.numeric(dealroom_data$founders_first_degree_year_10)
dealroom_data$founders_founded_companies_total_funding_1 <- as.numeric(dealroom_data$founders_founded_companies_total_funding_1)
dealroom_data$founders_founded_companies_total_funding_2 <- as.numeric(dealroom_data$founders_founded_companies_total_funding_2)
dealroom_data$founders_founded_companies_total_funding_3 <- as.numeric(dealroom_data$founders_founded_companies_total_funding_3)
dealroom_data$founders_founded_companies_total_funding_4 <- as.numeric(dealroom_data$founders_founded_companies_total_funding_4)
dealroom_data$founders_founded_companies_total_funding_5 <- as.numeric(dealroom_data$founders_founded_companies_total_funding_5)
dealroom_data$founders_founded_companies_total_funding_6 <- as.numeric(dealroom_data$founders_founded_companies_total_funding_6)
dealroom_data$founders_founded_companies_total_funding_7 <- as.numeric(dealroom_data$founders_founded_companies_total_funding_7)
dealroom_data$founders_founded_companies_total_funding_8 <- as.numeric(dealroom_data$founders_founded_companies_total_funding_8)
dealroom_data$founders_founded_companies_total_funding_9 <- as.numeric(dealroom_data$founders_founded_companies_total_funding_9)
dealroom_data$founders_founded_companies_total_funding_10 <- as.numeric(dealroom_data$founders_founded_companies_total_funding_10)
dealroom_data$founders_years_of_education_1 <- as.numeric(dealroom_data$founders_years_of_education_1)
dealroom_data$founders_years_of_education_2 <- as.numeric(dealroom_data$founders_years_of_education_2)
dealroom_data$founders_years_of_education_3 <- as.numeric(dealroom_data$founders_years_of_education_3)
dealroom_data$founders_years_of_education_4 <- as.numeric(dealroom_data$founders_years_of_education_4)
dealroom_data$founders_years_of_education_5 <- as.numeric(dealroom_data$founders_years_of_education_5)
dealroom_data$founders_years_of_education_6 <- as.numeric(dealroom_data$founders_years_of_education_6)
dealroom_data$founders_years_of_education_7 <- as.numeric(dealroom_data$founders_years_of_education_7)
dealroom_data$founders_years_of_education_8 <- as.numeric(dealroom_data$founders_years_of_education_8)
dealroom_data$founders_years_of_education_9 <- as.numeric(dealroom_data$founders_years_of_education_9)
dealroom_data$founders_years_of_education_10 <- as.numeric(dealroom_data$founders_years_of_education_10)
#valuation month to factor
dealroom_data$valuation_month <- factor(dealroom_data$valuation_month)

str(dealroom_data, list.len = length(dealroom_data))

#### PART 3: Further feature engineering ####
##look into the founders information
##founder status
#are all founder still in the currently in the company?
dealroom_data <- dealroom_data %>%
  mutate(founding_team_active = if_else(
    rowSums(across(starts_with("founder_status_"), ~ .x == "past"), na.rm = TRUE) > 0, 
    0, #if any founder has status 'past'
    if_else(
      rowSums(across(starts_with("founder_status_"), ~ !is.na(.x)), na.rm = TRUE) == 0,
      NA_integer_, #if there are no founder status columns filled
      1 #if all founders have status 'current'
    )
  ))
#make it a factor
dealroom_data$founding_team_active <- factor(dealroom_data$founding_team_active)
str(dealroom_data$founding_team_active)

##gender
#Fixing the unique factor values of the founder genders
#ensure that "not given" is added as a level of the factor
unique(dealroom_data$founders_genders_1)
dealroom_data$founders_genders_1 <- factor(dealroom_data$founders_genders_1)
levels(dealroom_data$founders_genders_1)[levels(dealroom_data$founders_genders_1) %in% c("none of the options", "n/a")] <- "not given"

#now do it for the founders_genders_2 factors
unique(dealroom_data$founders_genders_2)
dealroom_data$founders_genders_2 <- factor(dealroom_data$founders_genders_2)
levels(dealroom_data$founders_genders_2)[levels(dealroom_data$founders_genders_2) %in% c("none of the options", "n/a")] <- "not given"

#now do it for founders_genders_3
unique(dealroom_data$founders_genders_3)
#replace NA, "none of the options", and "prefer not to say" with "not given"
unique(dealroom_data$founders_genders_3)
dealroom_data$founders_genders_3 <- factor(dealroom_data$founders_genders_3)
levels(dealroom_data$founders_genders_3)[levels(dealroom_data$founders_genders_3) %in% c("prefer not to say","none of the options", "n/a")] <- "not given"
unique(dealroom_data$founders_genders_3)

#now do it for the 4th level
unique(dealroom_data$founders_genders_4)
dealroom_data$founders_genders_4 <- factor(dealroom_data$founders_genders_4)
levels(dealroom_data$founders_genders_4)[levels(dealroom_data$founders_genders_4) %in% c("none of the options", "n/a")] <- "not given"
unique(dealroom_data$founders_genders_4)

unique(dealroom_data$founders_genders_5)
dealroom_data$founders_genders_5 <- factor(dealroom_data$founders_genders_5)
levels(dealroom_data$founders_genders_5)[levels(dealroom_data$founders_genders_5) %in% c("n/a")] <- "not given"
unique(dealroom_data$founders_genders_5)

unique(dealroom_data$founders_genders_6)
dealroom_data$founders_genders_6 <- factor(dealroom_data$founders_genders_6)
levels(dealroom_data$founders_genders_6)[levels(dealroom_data$founders_genders_6) %in% c("n/a")] <- "not given"
unique(dealroom_data$founders_genders_6)

unique(dealroom_data$founders_genders_7)
dealroom_data$founders_genders_7 <- factor(dealroom_data$founders_genders_7)
levels(dealroom_data$founders_genders_7)[levels(dealroom_data$founders_genders_7) %in% c("n/a")] <- "not given"
unique(dealroom_data$founders_genders_7)

unique(dealroom_data$founders_genders_8)
dealroom_data$founders_genders_8 <- factor(dealroom_data$founders_genders_8)
levels(dealroom_data$founders_genders_8)[levels(dealroom_data$founders_genders_8) %in% c("n/a")] <- "not given"
unique(dealroom_data$founders_genders_8)

unique(dealroom_data$founders_genders_9)
dealroom_data$founders_genders_9 <- factor(dealroom_data$founders_genders_9)
levels(dealroom_data$founders_genders_9)[levels(dealroom_data$founders_genders_9) %in% c("n/a")] <- "not given"
unique(dealroom_data$founders_genders_9)

unique(dealroom_data$founders_genders_10)

#other founder gender variables are good
dealroom_data <- dealroom_data %>%
  mutate(
    is_male_only = 0,
    is_female_only = 0,
    is_mixed_team = 0,
    founder_gender_unknown = 0
  )

#assuming you have already transformed "none of the options" and "n/a" to "not given"
#and you have a correct `num_founders` column
for (i in 1:nrow(dealroom_data)) {
  count_male <- count_female <- count_not_given <- count_na <- 0
  for (j in 1:10) {
    gender_col <- paste("founders_genders_", j, sep = "")
    if (!is.na(dealroom_data[[gender_col]][i])) {
      if (dealroom_data[[gender_col]][i] == "male") count_male <- count_male + 1
      if (dealroom_data[[gender_col]][i] == "female") count_female <- count_female + 1
      if (dealroom_data[[gender_col]][i] == "not given") count_not_given <- count_not_given + 1
    } else {
      count_na <- count_na + 1
    }
  }
  
  if (count_not_given > 0 || count_na == 10 || (count_male + count_female < dealroom_data$num_founders[i])) {
    dealroom_data$founder_gender_unknown[i] <- 1
    dealroom_data$is_male_only[i] <- 0
    dealroom_data$is_female_only[i] <- 0
    dealroom_data$is_mixed_team[i] <- 0
  } else {
    if (count_male == dealroom_data$num_founders[i]) dealroom_data$is_male_only[i] <- 1
    if (count_female == dealroom_data$num_founders[i]) dealroom_data$is_female_only[i] <- 1
    if (count_male > 0 && count_female > 0) dealroom_data$is_mixed_team[i] <- 1
  }
}

#make variable to see if there is a serial entrepreneur in the team
#ensure 'is_serial_in_team' is initialized correctly for all rows
#loop through each column that starts with 'founders_is_serial_'
#make factor of founder_is_serial_1 until _10
dealroom_data$founders_is_serial_1 <- factor(dealroom_data$founders_is_serial_1)
dealroom_data$founders_is_serial_2 <- factor(dealroom_data$founders_is_serial_2)
dealroom_data$founders_is_serial_3 <- factor(dealroom_data$founders_is_serial_3)
dealroom_data$founders_is_serial_4 <- factor(dealroom_data$founders_is_serial_4)
dealroom_data$founders_is_serial_5 <- factor(dealroom_data$founders_is_serial_5)
dealroom_data$founders_is_serial_6 <- factor(dealroom_data$founders_is_serial_6)
dealroom_data$founders_is_serial_7 <- factor(dealroom_data$founders_is_serial_7)
dealroom_data$founders_is_serial_8 <- factor(dealroom_data$founders_is_serial_8)
dealroom_data$founders_is_serial_9 <- factor(dealroom_data$founders_is_serial_9)
dealroom_data$founders_is_serial_10 <- factor(dealroom_data$founders_is_serial_10)

str(dealroom_data, list.len = length(dealroom_data))
for (col_name in names(dealroom_data)[grepl("^founders_is_serial_", names(dealroom_data))]) {
  #convert "Yes"/"No" to "1"/"0"
  dealroom_data[[col_name]] <- factor(ifelse(dealroom_data[[col_name]] == "yes", "1", "0"))
}

dealroom_data$is_serial_in_team <- 0

#iterate through each row
for (i in 1:nrow(dealroom_data)) {
  #initialize a flag to track if a serial founder is found
  serial_founder_found <- FALSE
  #check each of the serial founder columns for a "1"
  for (col_name in paste0("founders_is_serial_", 1:10)) {
    #check if the value is not NA and equals "1"
    if (!is.na(dealroom_data[i, col_name]) && dealroom_data[i, col_name] == "1") {
      dealroom_data$is_serial_in_team[i] <- 1
      serial_founder_found <- TRUE
      break #stop checking further columns for this row, as we found a "1"
    }
  }
  #if no serial founder is found, ensure the column is set to 0
  if (!serial_founder_found) {
    dealroom_data$is_serial_in_team[i] <- 0
  }
}
#convert 'is_serial_in_team' to a factor for consistency with other binary variables, if needed
dealroom_data$is_serial_in_team <- factor(dealroom_data$is_serial_in_team, levels = c(0, 1), labels = c("No", "Yes"))
str(dealroom_data, list.len = length(dealroom_data))
#make an average experience of the founders in years
#calculate the average years of experience per row
dealroom_data$avg_years_experience <- rowMeans(dealroom_data[, grep("^founders_experience_", names(dealroom_data))], na.rm = TRUE)

str(dealroom_data, list.len = length(dealroom_data))

##study all the backgrounds of the founders
all_backgrounds <- unlist(dealroom_data[, grep("^founders_backgrounds_", names(dealroom_data))])
split_backgrounds <- unlist(strsplit(as.character(all_backgrounds), ",\\s*"))
split_backgrounds <- na.omit(split_backgrounds)
unique_backgrounds <- unique(split_backgrounds)

#calculate the frequency of each unique background
backgrounds_freq <- table(split_backgrounds)
#convert the table to a dataframe
backgrounds_freq_df <- as.data.frame(backgrounds_freq, stringsAsFactors = FALSE)
#rename columns for clarity
names(backgrounds_freq_df) <- c("Background", "Count")
#sort the dataframe by count in descending order to get the most common backgrounds
backgrounds_freq_df <- backgrounds_freq_df[order(-backgrounds_freq_df$Count), ]
#extract the top 5 most common backgrounds
top_5_backgrounds <- head(backgrounds_freq_df, 5)
print(top_5_backgrounds)

#add an initial column for is_complementary_team, defaulting to 0 (not complementary)
dealroom_data$is_complementary_team <- 0

#backgrounds to look for
business_backgrounds <- c("business", "entrepreneurship")
technical_backgrounds <- c("technical", "it")

#iterate through each row of the dataframe
for (i in 1:nrow(dealroom_data)) {
  has_business_bg <- FALSE
  has_technical_bg <- FALSE
  
  #check founders_backgrounds columns
  for (col_name in paste0("founders_backgrounds_", 1:10)) {
    #value of the current cell
    cell_value <- tolower(dealroom_data[i, col_name])
    
    #check if cell contains any of the business or technical backgrounds
    if (length(grep(paste(business_backgrounds, collapse="|"), cell_value)) > 0) {
      has_business_bg <- TRUE
    }
    if (length(grep(paste(technical_backgrounds, collapse="|"), cell_value)) > 0) {
      has_technical_bg <- TRUE
    }
  }
  
  #if both business and technical backgrounds are found, mark team as complementary
  if (has_business_bg && has_technical_bg) {
    dealroom_data$is_complementary_team[i] <- 1
  }
}

str(dealroom_data, list.len = length(dealroom_data))

##average years of education in the team
dealroom_data$avg_years_education <- rowMeans(dealroom_data[, grep("^founders_years_of_education_", names(dealroom_data))], na.rm = TRUE)

##study founder universities
#combine all founders_universities columns into one big character vector
all_universities <- c(dealroom_data$founders_universities_1, dealroom_data$founders_universities_2,
                      dealroom_data$founders_universities_3, dealroom_data$founders_universities_4,
                      dealroom_data$founders_universities_5, dealroom_data$founders_universities_6,
                      dealroom_data$founders_universities_7, dealroom_data$founders_universities_8,
                      dealroom_data$founders_universities_9, dealroom_data$founders_universities_10)

#split the universities by comma and unlist into a single vector
universities_split <- unlist(strsplit(all_universities, ",", fixed = TRUE))

#remove NA and trim whitespace
universities_clean <- trimws(universities_split[!is.na(universities_split)])

#count the frequencies of each unique university
university_counts <- table(universities_clean)

#convert to dataframe and sort by frequency
university_freq_df <- as.data.frame(university_counts, stringsAsFactors = FALSE)
names(university_freq_df) <- c("University", "Count")
university_freq_df <- university_freq_df[order(-university_freq_df$Count),]

#get the top 100 universities by frequency
top_100_universities <- head(university_freq_df, 100)

#print top 100 universities with counts
print(top_100_universities)

#use top 50 for this operation
top_50_universities <- head(top_100_universities, 50)$University

#function to check if any of the founder universities is in top 50
check_top_50_uni <- function(universities_row) {
  #concatenate all university fields into a single string
  combined_universities <- paste(universities_row, collapse = ",")
  #split into individual universities
  individual_universities <- unlist(strsplit(combined_universities, ",", fixed = TRUE))
  #check if any of the individual universities is in top 50
  any(individual_universities %in% top_50_universities)
}

#apply the function to each row and create binary factor column
dealroom_data$founder_top_50_uni <- apply(dealroom_data[paste0("founders_universities_", 1:10)], 1, check_top_50_uni) * 1

#convert binary numeric column to a factor for consistency
dealroom_data$founder_top_50_uni <- factor(dealroom_data$founder_top_50_uni, levels = c(0, 1), labels = c("No", "Yes"))

str(dealroom_data, list.len = length(dealroom_data))

##study the founder companies background
#combine all company experience columns into one big list
all_company_experiences <- unlist(dealroom_data[paste0("founders_company_experience_", 1:10)])

#split combined strings on commas to separate individual companies
split_companies <- unlist(strsplit(all_company_experiences, ",", fixed = TRUE))

#remove NA values and trim whitespace
clean_companies <- na.omit(trimws(split_companies))

#count occurrences of each company
company_counts <- table(clean_companies)

#convert to dataframe and sort by frequency
company_counts_df <- as.data.frame(company_counts, stringsAsFactors = FALSE)
colnames(company_counts_df) <- c("Company", "Count")
company_counts_sorted <- company_counts_df[order(-company_counts_df$Count),]

#get top 100 companies
top_100_companies <- head(company_counts_sorted, 100)

#extract top 50 companies for easier reference
top_50_companies <- head(company_counts_sorted, 50)$Company

#function to check each row for top 50 company experience
dealroom_data$founder_top_50_company <- apply(dealroom_data[paste0("founders_company_experience_", 1:10)], 1, function(row) {
  #combine and split similar to before
  combined_experience <- paste(row, collapse = ",")
  individual_companies <- unlist(strsplit(combined_experience, ",", fixed = TRUE))
  
  #check if any company in the row is in the top 50
  any(trimws(individual_companies) %in% top_50_companies) * 1
})

#convert the binary numeric column to a factor for clarity
dealroom_data$founder_top_50_company <- factor(dealroom_data$founder_top_50_company, levels = c(0, 1), labels = c("No", "Yes"))

#back to 1 and zero
#convert to numeric first where Yes = 1 and No = 0
dealroom_data$founder_top_50_uni <- as.numeric(dealroom_data$founder_top_50_uni == "Yes")
dealroom_data$founder_top_50_company <- as.numeric(dealroom_data$founder_top_50_company == "Yes")

#convert these numeric columns back to factors with levels 1 and 0
dealroom_data$founder_top_50_uni <- factor(dealroom_data$founder_top_50_uni, levels = c(0, 1), labels = c("0", "1"))
dealroom_data$founder_top_50_company <- factor(dealroom_data$founder_top_50_company, levels = c(0, 1), labels = c("0", "1"))

str(dealroom_data, list.len = length(dealroom_data))

#study the linkedin of the founders
#initialize new column with zeros
dealroom_data$full_team_on_linkedin <- "0"

#iterate through each row
for (i in 1:nrow(dealroom_data)) {
  #initialize linkedin_count and na_count for the current row
  linkedin_count <- 0
  na_count <- 0
  
  #check each of the LinkedIn URL columns
  for (col_name in paste0("founders_linkedin_", 1:10)) {
    if (!is.na(dealroom_data[i, col_name]) && dealroom_data[i, col_name] != "") {
      linkedin_count <- linkedin_count + 1
    } else if (is.na(dealroom_data[i, col_name])) {
      na_count <- na_count + 1
    }
  }
  
  #check for all NA scenario
  if (na_count == 10) {
    dealroom_data$full_team_on_linkedin[i] <- "unknown"
  } else if (linkedin_count == dealroom_data$num_founders[i]) {
    dealroom_data$full_team_on_linkedin[i] <- "1"
  } else {
    dealroom_data$full_team_on_linkedin[i] <- "0"
  }
}

#after processing all rows, convert "unknown" values to "0"
dealroom_data$full_team_on_linkedin <- ifelse(dealroom_data$full_team_on_linkedin == "unknown", "0", dealroom_data$full_team_on_linkedin)

#convert full_team_on_linkedin to a factor with two levels, since "unknown" is now treated as "0"
dealroom_data$full_team_on_linkedin <- factor(dealroom_data$full_team_on_linkedin, levels = c("0", "1"))

#view the structure of the modified dealroom_data
str(dealroom_data, list.len = length(dealroom_data))

##founders strength
#initialize the new column with "unknown"
dealroom_data$team_strength <- "unknown"

#iterate through each row
for (i in 1:nrow(dealroom_data)) {
  #if first founder's strength is not NA, take its value
  if (!is.na(dealroom_data$founders_strength_1[i])) {
    dealroom_data$team_strength[i] <- as.character(dealroom_data$founders_strength_1[i])
  } else {
    #check if any of other founders_strength columns are not NA
    strength_found <- FALSE
    for (col_name in paste0("founders_strength_", 2:10)) {
      if (!is.na(dealroom_data[i, col_name])) {
        strength_found <- TRUE
        break
      }
    }
    #if any other column is not NA, update accordingly
    if (strength_found) {
    }
  }
}

#convert team_strength to a factor
dealroom_data$team_strength <- factor(dealroom_data$team_strength, levels = unique(dealroom_data$team_strength))
#change the NaN in is_serial_in_team and avg_years_experience to NA
dealroom_data$avg_years_experience[is.nan(dealroom_data$avg_years_experience)] <- NA
dealroom_data$is_serial_in_team[is.nan(dealroom_data$is_serial_in_team)] <- NA

#convert all variables of years to factors
#valuation_year
dealroom_data$launch_year <- as.factor(dealroom_data$launch_year)
dealroom_data$seed_year <- as.factor(dealroom_data$seed_year)
dealroom_data$closing_year <- as.factor(dealroom_data$closing_year)
dealroom_data$year_company_became_unicorn <- as.factor(dealroom_data$year_company_became_unicorn)
dealroom_data$year_company_became_future_unicorn <- as.factor(dealroom_data$year_company_became_future_unicorn)
dealroom_data$first_funding_year <- as.factor(dealroom_data$first_funding_year)
dealroom_data$last_funding_year <- as.factor(dealroom_data$last_funding_year)
dealroom_data$valuation_year <- as.factor(dealroom_data$valuation_year)
#convert "0" to NA and keep "yes" and "no" as is
dealroom_data$is_founders_first_company_1 <- factor(ifelse(dealroom_data$is_founders_first_company_1 == "0", NA, as.character(dealroom_data$is_founders_first_company_1)))

#if you need to ensure "yes" and "no" are the only levels in the factor (removing "0" if it's still listed as a level)
dealroom_data$is_founders_first_company_1 <- factor(dealroom_data$is_founders_first_company_1, levels = c("yes", "no"))

str(dealroom_data, list.len = length(dealroom_data))
skim(dealroom_data)
unique(dealroom_data$employees)


#### PART 5 ###### EDA
#EDA Skimr

#count number of company_status of the companies that are acquired
dealroom_data %>%
  filter(company_status == "acquired") %>%
  count(company_status)
dealroom_data %>%
  filter(company_status == "operational") %>%
  count(company_status)
dealroom_data %>%
  filter(company_status == "closed") %>%
  count(company_status)
#count total rows that have a value for year company became unicorn
dealroom_data %>%
  filter(!is.na(year_company_became_future_unicorn)) %>%
  summarise(total_count = n())
dealroom_data %>%
  filter(!is.na(year_company_became_unicorn)) %>%
  summarise(total_count = n())

#average
dealroom_data <- dealroom_data %>%
  mutate(avg_valuation = (min_valuation + max_valuation) / 2)
#success variables
dealroom_data <- dealroom_data %>%
  mutate(
    is_unicorn = ifelse(!is.na(year_company_became_unicorn), 1, 0),
    is_future_unicorn = ifelse(!is.na(year_company_became_future_unicorn), 1, 0),
    is_acquired = ifelse(company_status == "acquired", 1, 0),
    is_operational = ifelse(company_status == "operational", 1, 0),
    is_acquired_or_operational = ifelse(company_status %in% c("acquired", "operational"), 1, 0)
  )

#count occurences
dealroom_data %>%
  summarise(
    count_is_unicorn = sum(is_unicorn, na.rm = TRUE),
    count_is_future_unicorn = sum(is_future_unicorn, na.rm = TRUE),
    count_is_acquired = sum(is_acquired, na.rm = TRUE),
    count_is_operational = sum(is_operational, na.rm = TRUE),
    count_is_acquired_or_operational = sum(is_acquired_or_operational, na.rm = TRUE)
  )

#focus on total funding rounds
#without total funding eur, employees, dealroom completeness, company_status, team_strength, gender_unknown, is_ownership not known
dealroom_data_final <- select(dealroom_data, name, hq_region, hq_country, hq_city, seed_year, launch_year, closing_year, growth_stage, total_rounds_number, company_status, year_company_became_unicorn, year_company_became_future_unicorn, dealroom_signal_._founding_team_score, dealroom_signal_._growth_rate, dealroom_signal_._timing, 
                              number_of_patents, has_investor, number_top_investors, is_venture_capital, is_angel, is_accelerator, is_corporate, is_private_equity, is_crowdfunding, is_corporate_venture_fund, is_investment_fund, is_government, is_university, is_incubator,
                              is_non.profit, is_family_office, is_government_._non.profit, is_advisor, is_workspace, is_sovereign_wealth_fund, is_service_provider,
                              is_company, is_workspace_location, is_event, last_funding_month, last_funding_year, first_funding_month, first_funding_year, 
                              is_ownerships_angel, is_ownerships_venture_capital, is_ownerships_subsidiary, is_ownerships_accelerator, is_ownerships_private_equity, is_ownerships_crowdfunded, is_ownerships_public, is_ownerships_bootstrapped, revenue_model_saas, revenue_model_manufacturing, `revenue_model_marketplace_&_ecommerce`, revenue_model_saas_only, industry_enterprisesoftware,
                              industry_legal, industry_fintech, industry_semiconductors, industry_health, industry_fashion, industry_energy, industry_media, industry_security, industry_realestate,
                              industry_education, industry_space, industry_wellnessbeauty, industry_jobsrecruitment, industry_travel, industry_sports, industry_robotics, industry_marketing, industry_transportation, industry_kids, industry_gaming, 
                              industry_eventtech, industry_serviceprovider, industry_food, industry_telecom, industry_chemicals, industry_music, industry_homeliving, industry_engineeringandmanufacturingequipment, industry_hosting, industry_consumerelectronics,
                              avg_valuation, valuation_month, valuation_year, is_artificial_intelligence, is_big_data, is_blockchain, is_deep_tech, is_mobile_app, 
                              is_augmented_reality, is_iot_internetofthings, is_connected_device, is_computer_vision, is_natural_language_processing, is_deep_learning, is_virtual_reality, is_3d_technology, 
                              is_hardware, is_recognition_technology, is_autonomous__sensor_tech, is_quantum_technologies, is_nanotech, is_life_on_land_15, is_responsible_consumption_and_production_12, is_climate_action_13, is_good_health_and_wellbeing_3, is_sustainable_cities_and_communities_11, 
                              is_affordable_and_clean_energy_7, is_partnerships_17, is_decent_work_and_economic_growth_8, is_gender_equality_5, is_industry_innovation_and_infrastructure_9, 
                              is_reduced_inequalities_10, is_clean_water_and_sanitation_6, is_zero_hunger_2, is_quality_education_4, is_life_below_water_14, is_peace_justice_and_strong_institutions_16, is_no_poverty_1, 
                              founding_team_active, is_male_only, is_female_only, is_mixed_team, is_serial_in_team, avg_years_experience, is_complementary_team, avg_years_education, founder_top_50_uni, founder_top_50_company, full_team_on_linkedin)

#NaN in avg years education
dealroom_data_final$avg_years_education[is.nan(dealroom_data$avg_years_education)] <- NA


#EDA 
write.csv(dealroom_data_final, "dealroom_data_final.csv", row.names = FALSE)
save(dealroom_data_final, file = "dealroom_data_final.RData")