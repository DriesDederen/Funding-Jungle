library(tidymodels)
library(tidyverse)

load("dealroom_data_final.RData")

#calculate IQR and define bounds for outliers
#this takes away everything above 7 rounds
iqr_value <- IQR(dealroom_data_final$total_rounds_number)
q1 <- quantile(dealroom_data_final$total_rounds_number, 0.25)
q3 <- quantile(dealroom_data_final$total_rounds_number, 0.75)
lower_bound <- q1 - 1.5 * iqr_value
upper_bound <- q3 + 1.5 * iqr_value
#filter out outlier rows based on IQR method
dealroom_data_final <- dealroom_data_final %>%
  filter(total_rounds_number >= lower_bound & total_rounds_number <= upper_bound)

#calculate IQR and define bounds for outliers

#calculate IQR and define bounds for outliers
#this takes away everything above 7 rounds
iqr_value <- IQR(dealroom_data_final$avg_valuation, na.rm = TRUE)
q1 <- quantile(dealroom_data_final$avg_valuation, 0.25, na.rm = TRUE)
q3 <- quantile(dealroom_data_final$avg_valuation, 0.75, na.rm = TRUE)
lower_bound <- q1 - 1.5 * iqr_value
upper_bound <- q3 + 1.5 * iqr_value
#filter out outlier rows based on IQR method
dealroom_data_final <- dealroom_data_final %>%
  filter((avg_valuation >= lower_bound & avg_valuation <= upper_bound)  | is.na(avg_valuation))

#calculate IQR and define bounds for outliers
#this takes away everything above 7 rounds
iqr_value <- IQR(dealroom_data_final$avg_years_education, na.rm = TRUE)
q1 <- quantile(dealroom_data_final$avg_years_education, 0.25, na.rm = TRUE)
q3 <- quantile(dealroom_data_final$avg_years_education, 0.75, na.rm = TRUE)
lower_bound <- q1 - 1.5 * iqr_value
upper_bound <- q3 + 1.5 * iqr_value
#filter out outlier rows based on IQR method
dealroom_data_final <- dealroom_data_final %>%
  filter((avg_years_education >= lower_bound & avg_years_education <= upper_bound) | is.na(avg_years_education))

#calculate IQR and define bounds for outliers
#this takes away everything above 7 rounds
iqr_value <- IQR(dealroom_data_final$avg_years_experience, na.rm = TRUE)
q1 <- quantile(dealroom_data_final$avg_years_experience, 0.25, na.rm = TRUE)
q3 <- quantile(dealroom_data_final$avg_years_experience, 0.75, na.rm = TRUE)
lower_bound <- q1 - 1.5 * iqr_value
upper_bound <- q3 + 1.5 * iqr_value
#filter out outlier rows based on IQR method
dealroom_data_final <- dealroom_data_final %>%
  filter((avg_years_experience >= lower_bound & avg_years_experience <= upper_bound) | is.na(avg_years_experience))


### LOCATION - REGION, COUNTRY, CITY ###
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(ggplot2)
library(dplyr)

dealroom_data_final %>%
  count(hq_region) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = reorder(hq_region, -n), y = n)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribution of HQ Regions", x = "Region", y = "Count")

dealroom_data_final %>%
  count(hq_country) %>%
  arrange(desc(n)) %>%
  head(20) %>%
  ggplot(aes(x = reorder(hq_country, -n), y = n)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "", x = "Country", y = "Count")

dealroom_data_final %>%
  filter(hq_city != "N/A") %>%
  count(hq_city) %>%
  arrange(desc(n)) %>%
  head(20) %>%
  ggplot(aes(x = reorder(hq_city, -n), y = n)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 65, hjust = 1)) +
  labs(title = "", x = "City", y = "Count")

### TOTAL ROUNDS NUMBER ###
funding_rounds_distribution <- dealroom_data_final %>%
  count(total_rounds_number) %>%
  arrange(total_rounds_number)

ggplot(funding_rounds_distribution, aes(x = as.factor(total_rounds_number), y = n)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "",
       x = "Number of Total Funding Rounds",
       y = "Count of Companies")

#percentage distribution of the total funding rounds
total_companies <- nrow(dealroom_data_final)

funding_rounds_percentage <- dealroom_data_final %>%
  count(total_rounds_number) %>%
  mutate(percentage = n / total_companies * 100) %>%
  arrange(total_rounds_number)

ggplot(funding_rounds_percentage, aes(x = as.factor(total_rounds_number), y = percentage)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "",
       x = "Number of Total Funding Rounds",
       y = "Percentage of Companies (%)")

dealroom_data_final <- dealroom_data_final %>%
  mutate(
    last_funding_year = as.numeric(as.character(last_funding_year)),
    target_invest = as.numeric(last_funding_year >= 2022)
  )

#mean of the funding rounds
mean_funding_rounds <- mean(dealroom_data_final$total_rounds_number, na.rm = TRUE)

#create a bar plot to show the distribution of target_invest
ggplot(dealroom_data_final, aes(x = as.factor(target_invest))) +
  geom_bar() +
  labs(title = "",
       x = "Target Invest (0 = No, 1 = Yes)",
       y = "Count") +
  theme_minimal()

### LAUNCH AND SEED YEAR ###
library(dplyr)
library(tidyr)
library(ggplot2)

year_distribution <- dealroom_data_final %>%
  select(launch_year, seed_year) %>%
  filter(launch_year != "N/A" & seed_year != "N/A") %>%
  pivot_longer(everything(), names_to = "year_type", values_to = "year") %>%
  filter(!is.na(year) & year != "0") %>%
  mutate(year = factor(year, levels = as.character(2014:2024)))

#bar chart using ordered factors
ggplot(year_distribution, aes(x = year, fill = year_type)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "",
       x = "Year",
       y = "Frequency") +
  theme(legend.position = "top")

#time from launch to seed
time_to_seed <- dealroom_data_final %>%
  filter(!is.na(launch_year) & !is.na(seed_year) & launch_year != "0" & seed_year != "0") %>%
  mutate(
    launch_year = factor(launch_year, levels = as.character(2014:2024)),
    seed_year = factor(seed_year, levels = as.character(2014:2024)),
    time_to_seed = as.numeric(as.character(seed_year)) - as.numeric(as.character(launch_year))
  )

#filter for positive time to seed
positive_time_to_seed <- time_to_seed %>%
  filter(time_to_seed > 0)

#histogram of time to seed
ggplot(positive_time_to_seed, aes(x = time_to_seed)) +
  geom_histogram(binwidth = 1, color = "black") +
  theme_minimal() +
  labs(title = "",
       x = "Years to Seed Funding",
       y = "Frequency") +
  xlim(c(1, max(positive_time_to_seed$time_to_seed, na.rm = TRUE))) +
  scale_x_continuous(breaks = seq(1, max(positive_time_to_seed$time_to_seed, na.rm = TRUE), by = 1)) +
  theme(legend.position = "none")

### GROWTH STAGE ###
growth_stage_counts <- dealroom_data_final %>%
  group_by(growth_stage) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Percentage = Count / sum(Count) * 100)

print(growth_stage_counts)

growth_stage_counts$FormattedPercentage <- sprintf("%.2f%%", growth_stage_counts$Percentage)

print(growth_stage_counts)
ggplot(growth_stage_counts, aes(x = "", y = Percentage, fill = growth_stage)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "", fill = "Growth Stage") +
  scale_fill_brewer(palette = "Pastel1")
  # geom_text(aes(label = sprintf("%.2f%%", Percentage)), position = position_stack(vjust = 0.5))

### COMPANY STATUS ###
company_status_counts <- dealroom_data_final %>%
  group_by(company_status) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Percentage = Count / sum(Count) * 100)
print(company_status_counts)

company_status_counts$FormattedPercentage <- sprintf("%.2f%%", company_status_counts$Percentage)
print(company_status_counts)

ggplot(company_status_counts, aes(x = "", y = Percentage, fill = company_status)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "", fill = "Company Status") +
  scale_fill_brewer(palette = "Pastel2")

### UNICORN ###
year_distribution <- dealroom_data_final %>%
  select(year_company_became_unicorn, year_company_became_future_unicorn) %>%
  pivot_longer(cols = everything(), names_to = "unicorn_status", values_to = "year") %>%
  filter(!is.na(year)) %>%
  mutate(unicorn_status = ifelse(unicorn_status == "year_company_became_unicorn", "Unicorn", "Future Unicorn"))

year_distribution_summary <- year_distribution %>%
  group_by(unicorn_status, year) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(year)

ggplot(year_distribution_summary, aes(x = year, y = count, fill = unicorn_status)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_manual(values = c("cornflowerblue", "salmon"), name = "Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribution of Unicorn and Future Unicorn Years",
       x = "Year", 
       y = "Count of Companies")

### PATENTS ###
patent_distribution <- dealroom_data_final %>%
  count(number_of_patents) %>%
  mutate(Percentage = n / sum(n) * 100)

patent_distribution <- patent_distribution %>%
  mutate(FormattedPercentage = paste0(round(Percentage, 2), "%"))

print(patent_distribution)

### HAS INVESTOR ###
investor_distribution <- dealroom_data_final %>%
  mutate(has_investor = ifelse(is.na(has_investor), "NA", as.character(has_investor))) %>%
  group_by(has_investor) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

investor_distribution <- investor_distribution %>%
  mutate(FormattedPercentage = paste0(round(Percentage, 2), "%"))

print(investor_distribution)

### INVESTOR TYPE ###
investor_data <- dealroom_data_final %>%
  select(is_venture_capital, is_angel, is_accelerator, is_corporate,
         is_private_equity, is_crowdfunding, is_corporate_venture_fund,
         is_investment_fund, is_government, is_university, is_incubator,
         is_non.profit, is_family_office, `is_government_._non.profit`,
         is_advisor, is_workspace, is_sovereign_wealth_fund, is_service_provider,
         is_company, is_workspace_location, is_event)
investor_long <- pivot_longer(investor_data, cols = everything(), names_to = "investor_type", values_to = "presence")

investor_freq <- investor_long %>%
  filter(presence == 1) %>%
  group_by(investor_type) %>%
  summarise(frequency = n(), .groups = 'drop') %>%
  arrange(desc(frequency))

ggplot(investor_freq, aes(x = reorder(investor_type, frequency), y = frequency)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(title = "",
       x = "Investor Type",
       y = "Frequency")

### OWNERSHIP TYPE ###
ownership_data <- dealroom_data_final %>%
  select(is_ownerships_angel, is_ownerships_venture_capital, is_ownerships_subsidiary,
         is_ownerships_accelerator, is_ownerships_private_equity, is_ownerships_crowdfunded,
         is_ownerships_public, is_ownerships_bootstrapped)

ownership_long <- pivot_longer(ownership_data, cols = everything(), 
                               names_to = "ownership_type", values_to = "presence")

ownership_freq <- ownership_long %>%
  filter(presence == 1) %>%
  group_by(ownership_type) %>%
  summarise(frequency = n(), .groups = 'drop') %>%
  arrange(desc(frequency))

ggplot(ownership_freq, aes(x = reorder(ownership_type, frequency), y = frequency)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 65, hjust = 1)) +
  labs(title = "", x = "Ownership type", y = "Frequency")

### INDUSTRY DISTRIBUTION  ###
industry_data <- dealroom_data_final %>%
  select(industry_enterprisesoftware, industry_legal, industry_fintech, 
         industry_semiconductors, industry_health, industry_fashion, industry_energy, 
         industry_media, industry_security, industry_realestate, industry_education, 
         industry_space, industry_wellnessbeauty, industry_jobsrecruitment, industry_travel, 
         industry_sports, industry_robotics, industry_marketing, industry_transportation, 
         industry_kids, industry_gaming, industry_eventtech, industry_serviceprovider, 
         industry_food, industry_telecom, industry_chemicals, industry_music, 
         industry_homeliving, industry_engineeringandmanufacturingequipment, 
         industry_hosting, industry_consumerelectronics)

industry_long <- pivot_longer(industry_data, cols = everything(), 
                              names_to = "industry_type", values_to = "presence")

industry_freq <- industry_long %>%
  filter(presence == 1) %>%
  group_by(industry_type) %>%
  summarise(frequency = n(), .groups = 'drop') %>%
  arrange(desc(frequency))

ggplot(industry_freq, aes(x = reorder(industry_type, frequency), y = frequency)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "", x = "Industry", y = "Frequency")

### AVG VALUATION ###
min_valuation <- min(dealroom_data_final$avg_valuation, na.rm = TRUE)
max_valuation <- max(dealroom_data_final$avg_valuation, na.rm = TRUE)
avg_valuation <- mean(dealroom_data_final$avg_valuation, na.rm = TRUE)

cat("Minimum Valuation:", min_valuation, "\n")
cat("Maximum Valuation:", max_valuation, "\n")
cat("Average Valuation:", avg_valuation, "\n")

#print how many NAs in the avg_valuation column
cat("Number of NAs in avg_valuation column:", sum(is.na(dealroom_data_final$avg_valuation)), "\n")

### FUNDING MONTHS AND YEARS ###
funding_months <- bind_rows(
  dealroom_data_final %>% select(funding_event = first_funding_month) %>% mutate(type = "First Funding"),
  dealroom_data_final %>% select(funding_event = last_funding_month) %>% mutate(type = "Last Funding")
)

funding_months$funding_event <- factor(funding_months$funding_event, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

ggplot(funding_months, aes(x = funding_event, fill = type)) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal() +
  labs(title = "",
       x = "Month",
       y = "Count of funding events") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

funding_years <- bind_rows(
  dealroom_data_final %>% select(funding_year = first_funding_year) %>% mutate(funding_year = as.factor(funding_year), type = "First funding"),
  dealroom_data_final %>% select(funding_year = last_funding_year) %>% mutate(funding_year = as.factor(funding_year), type = "Last funding")
)


ggplot(funding_years, aes(x = funding_year, fill = type)) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal() +
  labs(title = "",
       x = "Year",
       y = "Count of funding events") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### REVENUE MODEL ###
revenue_model_data <- dealroom_data_final %>%
  select(revenue_model_saas, revenue_model_manufacturing, `revenue_model_marketplace_&_ecommerce`, revenue_model_saas_only)

revenue_model_long <- pivot_longer(revenue_model_data, 
                                   cols = everything(), 
                                   names_to = "revenue_model", 
                                   values_to = "presence")

revenue_model_freq <- revenue_model_long %>%
  filter(presence == 1) %>%
  group_by(revenue_model) %>%
  summarise(frequency = n(), .groups = 'drop')

ggplot(revenue_model_freq, aes(x = revenue_model, y = frequency, fill = revenue_model)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "", x = "Revenue model", y = "Frequency")

### SECTOR DISTRIBUTION ###
sectors_data <- dealroom_data_final %>%
  select(is_artificial_intelligence, is_big_data, is_blockchain, is_deep_tech, 
         is_mobile_app, is_augmented_reality, is_iot_internetofthings, 
         is_connected_device, is_computer_vision, is_natural_language_processing, 
         is_deep_learning, is_virtual_reality, is_3d_technology, is_hardware, 
         is_recognition_technology, is_autonomous__sensor_tech, is_quantum_technologies, is_nanotech)
sectors_long <- pivot_longer(sectors_data, 
                             cols = everything(), 
                             names_to = "sector", 
                             values_to = "presence")
sector_freq <- sectors_long %>%
  filter(presence == 1) %>%
  group_by(sector) %>%
  summarise(frequency = n(), .groups = 'drop')

ggplot(sector_freq, aes(x = reorder(sector, -frequency), y = frequency)) +
  geom_bar(stat = "identity") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "",
       x = "Sector",
       y = "Frequency")

### SDGs ###
sdg_data <- dealroom_data_final %>%
  select(is_life_on_land_15, is_responsible_consumption_and_production_12, 
         is_climate_action_13, is_good_health_and_wellbeing_3, 
         is_sustainable_cities_and_communities_11, is_affordable_and_clean_energy_7, 
         is_partnerships_17, is_decent_work_and_economic_growth_8, 
         is_gender_equality_5, is_industry_innovation_and_infrastructure_9, 
         is_reduced_inequalities_10, is_clean_water_and_sanitation_6, 
         is_zero_hunger_2, is_quality_education_4, is_life_below_water_14, 
         is_peace_justice_and_strong_institutions_16, is_no_poverty_1)

sdgs_long <- pivot_longer(sdg_data, 
                          cols = everything(), 
                          names_to = "sdg", 
                          values_to = "presence")

sdg_freq <- sdgs_long %>%
  filter(presence == 1) %>%
  group_by(sdg) %>%
  summarise(frequency = n(), .groups = 'drop')

ggplot(sdg_freq, aes(x = reorder(sdg, -frequency), y = frequency)) +
  geom_bar(stat = "identity") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "",
       x = "Sustainable Development Goal",
       y = "Frequency")

#count the number of NAs in column is_climate_action_13
cat("Number of NAs in column is_climate_action_13:", sum(is.na(dealroom_data_final$is_climate_action_13)), "\n")

### FOUNDING TEAM ACTIVE ###
founding_team_active_counts <- dealroom_data_final %>%
  mutate(founding_team_active = ifelse(is.na(founding_team_active), "NA", as.character(founding_team_active))) %>%
  count(founding_team_active) %>%
  mutate(prop = n / sum(n))

print(founding_team_active_counts)

ggplot(founding_team_active_counts, aes(x = "", y = prop, fill = founding_team_active)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "", x = NULL, y = NULL, fill = "Active Status") +
  theme_void()

### MALE / FEMALE ###
male_only_counts <- dealroom_data_final %>%
  mutate(is_male_only = ifelse(is.na(is_male_only), "NA", as.character(is_male_only))) %>%
  count(is_male_only) %>%
  mutate(prop = n / sum(n))

ggplot(male_only_counts, aes(x = "", y = prop, fill = is_male_only)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = scales::percent(prop)), position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "", x = NULL, y = NULL, fill = "Male only") +
  theme_void()

print(male_only_counts)

female_only_counts <- dealroom_data_final %>%
  mutate(is_female_only = ifelse(is.na(is_female_only), "NA", as.character(is_female_only))) %>%
  count(is_female_only) %>%
  mutate(prop = n / sum(n))

ggplot(female_only_counts, aes(x = "", y = prop, fill = is_female_only)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = scales::percent(prop)), position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "", x = NULL, y = NULL, fill = "Female Only") +
  theme_void()

print(female_only_counts)

### MIXED / NON-MIXED ###
mixed_team_counts <- dealroom_data_final %>%
  mutate(is_mixed_team = ifelse(is.na(is_mixed_team), "NA", as.character(is_mixed_team))) %>%
  count(is_mixed_team) %>%
  mutate(prop = n / sum(n))

ggplot(mixed_team_counts, aes(x = "", y = prop, fill = is_mixed_team)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = scales::percent(prop)), position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "", x = NULL, y = NULL, fill = "Mixed Team") +
  theme_void()

print(mixed_team_counts)
### SERIALS IN TEAM ###
serial_in_team_counts <- dealroom_data_final %>%
  count(is_serial_in_team)

ggplot(serial_in_team_counts, aes(x = is_serial_in_team, y = n, fill = is_serial_in_team)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "", x = "Serial entrepreneur", y = "Count") +
  theme_minimal()

### AVG YEARS EXPERIENCE ###
avg_years_experience_stats <- dealroom_data_final$avg_years_experience
avg_value <- mean(avg_years_experience_stats, na.rm = TRUE)
min_value <- min(avg_years_experience_stats, na.rm = TRUE)
max_value <- max(avg_years_experience_stats, na.rm = TRUE)

cat("Average Years of Experience:", avg_value, "\n")
cat("Minimum Years of Experience:", min_value, "\n")
cat("Maximum Years of Experience:", max_value, "\n")
max_experience_row <- dealroom_data_final[which.max(dealroom_data_final$avg_years_experience), ]
#delete this outlier max_experience_row row in the dataset
dealroom_data_final <- dealroom_data_final[-which.max(dealroom_data_final$avg_years_experience), ]

#print number of NAs in avg_years_experience column
cat("Number of NAs in avg_years_experience column:", sum(is.na(dealroom_data_final$avg_years_experience)), "\n")
### COMPLEMENTARY TEAM ###
complementary_counts <- dealroom_data_final %>%
  mutate(is_complementary_team = ifelse(is.na(is_complementary_team), "NA", as.character(is_complementary_team))) %>%
  count(is_complementary_team) %>%
  mutate(prop = n / sum(n))

ggplot(complementary_counts, aes(x = "", y = prop, fill = is_complementary_team)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = scales::percent(prop)), position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "", x = NULL, y = NULL, fill = "Complementary status") +
  theme_void()


### AVG YEARS EDUCATION ###
avg_years_education_stats <- dealroom_data_final$avg_years_education

avg_value <- mean(avg_years_education_stats, na.rm = TRUE)
min_value <- min(avg_years_education_stats, na.rm = TRUE)
max_value <- max(avg_years_education_stats, na.rm = TRUE)

cat("Average Years of Education:", avg_value, "\n")
cat("Minimum Years of Education:", min_value, "\n")
cat("Maximum Years of Education:", max_value, "\n")

#print number of NAs in avg_years_education column
cat("Number of NAs in avg_years_education column:", sum(is.na(dealroom_data_final$avg_years_education)), "\n")

### FOUNDER TOP 50 UNI ###
ggplot(dealroom_data_final, aes(x = factor(founder_top_50_uni, levels = c("0", "1")))) +
  geom_bar() +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  labs(title = "",
       x = "Presence in Top 50 Universities",
       y = "Frequency") +
  theme_minimal()

#print proportions founder_top_50_uni variable
founder_top_50_uni_counts <- dealroom_data_final %>%
  count(founder_top_50_uni) %>%
  mutate(prop = n / sum(n))

print(founder_top_50_uni_counts)




### FOUNDER TOP 50 COMPANY ###
ggplot(dealroom_data_final, aes(x = factor(founder_top_50_company, levels = c("0", "1")))) +
  geom_bar() +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  labs(title = "",
       x = "Experience in Top 50 companies",
       y = "Frequency") +
  theme_minimal()

#print proportion founder_top_50_company
founder_top_50_company_counts <- dealroom_data_final %>%
  count(founder_top_50_company) %>%
  mutate(prop = n / sum(n))

founder_top_50_company_counts
### FULL TEAM ON LINKEDIN ###
ggplot(dealroom_data_final, aes(x = factor(full_team_on_linkedin, levels = c("0", "1")))) +
  geom_bar() +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  labs(title = "",
       x = "Full team on LinkedIn",
       y = "Frequency") +
  theme_minimal()

#print proportion of full_team_on_linkedin
full_team_on_linkedin_counts <- dealroom_data_final %>%
  count(full_team_on_linkedin) %>%
  mutate(prop = n / sum(n))

full_team_on_linkedin_counts

### CORRELATION ANALYSIS ###

##### CRAMER AND CHI ####
dealroom_data_corr <- select(dealroom_data_final, name, hq_region, hq_country, hq_city, seed_year, launch_year, closing_year, growth_stage, total_rounds_number, company_status, year_company_became_unicorn, year_company_became_future_unicorn,
                              number_of_patents, number_top_investors, is_venture_capital, is_angel, is_accelerator, is_corporate, is_private_equity,
                              last_funding_month, last_funding_year, first_funding_month, first_funding_year, target_invest,
                              is_ownerships_angel, is_ownerships_venture_capital, is_ownerships_subsidiary, is_ownerships_accelerator, revenue_model_saas,
                              avg_valuation, valuation_month, valuation_year, founding_team_active, is_male_only, is_female_only, is_mixed_team, is_serial_in_team, avg_years_experience, is_complementary_team, avg_years_education, founder_top_50_uni, founder_top_50_company, full_team_on_linkedin)
install.packages("ggcorrplot")
library(ggcorrplot)

#select only numeric columns
numeric_vars <- dealroom_data_final %>% select(where(is.numeric))

#compute correlation matrix
cor_matrix <- cor(numeric_vars, use = "complete.obs")

#plot the correlation matrix
ggcorrplot(cor_matrix, method = "circle", type = "upper", 
           tl.col = "black", tl.srt = 45, lab = TRUE)


install.packages("vcd")
library(vcd)

#factor columns
categorical_vars <- dealroom_data_final %>% select(where(is.factor))

#initialize matrix for store Cramer's V values
cramer_v_matrix <- matrix(ncol = ncol(categorical_vars), nrow = ncol(categorical_vars))
colnames(cramer_v_matrix) <- colnames(categorical_vars)
rownames(cramer_v_matrix) <- colnames(categorical_vars)

#df for storing significant correlations
strong_correlations <- data.frame(Relation = character(), CramersV = numeric(), stringsAsFactors = FALSE)

#compute Cramér's V
compute_cramers_v <- function(tbl) {
  chi_test <- chisq.test(tbl)
  if (any(chi_test$expected < 5)) {
    warning("Chi-squared approximation may be incorrect")
  }
  cramer_v <- sqrt(chi_test$statistic / (sum(tbl) * (min(dim(tbl)) - 1)))
  list(cramer_v = cramer_v, p_value = chi_test$p.value)
}

#Cramer's V matrix and perform Chi-square test
for (i in 1:ncol(categorical_vars)) {
  for (j in 1:ncol(categorical_vars)) {
    if (i != j) {
      tbl <- table(categorical_vars[[i]], categorical_vars[[j]])
      test_results <- tryCatch(compute_cramers_v(tbl), error = function(e) NULL)
      if (!is.null(test_results)) {
        cramer_v <- test_results$cramer_v
        p_value <- test_results$p_value
        
        if (is.infinite(cramer_v)) {
          cramer_v <- NA
        }
        
        cramer_v_matrix[i, j] <- cramer_v
        
        if (!is.na(cramer_v) && !is.na(p_value) && p_value < 0.05 && abs(cramer_v) > 0.7) {
          relation <- paste(rownames(cramer_v_matrix)[i], "-", colnames(cramer_v_matrix)[j])
          strong_correlations <- rbind(strong_correlations, data.frame(Relation = relation, CramersV = cramer_v))
        }
      }
    } else {
      cramer_v_matrix[i, j] <- NA
    }
  }
}

print(strong_correlations)

#####

#correlation valuation year and first funding year
valuation_year <- as.numeric(as.character(dealroom_data_final$valuation_year))
first_funding_year <- as.numeric(as.character(dealroom_data_final$first_funding_year))

valid_data <- na.omit(data.frame(valuation_year, first_funding_year))

#correlation test
correlation_test <- cor.test(valid_data$valuation_year, valid_data$first_funding_year, method = "pearson")

print(correlation_test)


### numbertarget variables
unicorn_distribution <- dealroom_data_final %>%
  select(year_company_became_unicorn, year_company_became_future_unicorn) %>%
  pivot_longer(cols = everything(), names_to = "unicorn_status", values_to = "year") %>%
  filter(!is.na(year)) %>%
  group_by(unicorn_status, year) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(unicorn_status, year)

print(unicorn_distribution)

zero_rounds_count <- sum(dealroom_data_final$total_rounds_number == 0, na.rm = TRUE)

na_rounds_count <- sum(is.na(dealroom_data_final$total_rounds_number))
rounds_count_summary <- data.frame(
  Type = c("Zero Rounds", "NA Rounds"),
  Count = c(zero_rounds_count, na_rounds_count)
)

print(rounds_count_summary)


#print all column names with their types
print(sapply(dealroom_data_final, class))

#corr analysis demographic variables and number of funding rounds
demographic_analysis <- dealroom_data_final %>%
  select(total_rounds_number, avg_years_experience, avg_years_education, founding_team_active, is_male_only, is_female_only, is_mixed_team) %>%
  mutate(across(where(is.character), as.factor)) %>%
  correlate() %>%
  focus(total_rounds_number)
print(demographic_analysis)
