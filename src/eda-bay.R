# Author: Sierra Wells
# Stanford University - COMM 177I
# comm177i/agenda-watch/src/eda-bay.R

# TODO
# organize script
# update SCC ICE award list for team

# Packages 
pacman::p_load(tidyverse,
               here,
               scales,
               stringr)

# Files 
input_paths <- list(us_local_gov = here("agenda-watch/contracts/input/us-local-gov-ice-contracts.csv"),
                    us_reg_state_gov = here("agenda-watch/contracts/input/us-reg-state-gov-ice-contracts.csv"))

output_paths <- list(
  sc_ice_year_count = here("agenda-watch/contracts/output/sc-ice-year-count.jpg"),
  sc_ice_year_value = here("agenda-watch/contracts/output/sc-ice-year-value.jpg"),
  bay_ice_year_count = here("agenda-watch/contracts/output/bay-ice-year-count.jpg"),
  bay_ice_year_value = here("agenda-watch/contracts/output/bay-ice-year-value.jpg"),
  bay_local_gov_compare_count = here("agenda-watch/contracts/output/bay-local-gov-compare-count.jpg"),
  bay_local_gov_compare_val = here("agenda-watch/contracts/output/bay-local-gov-compare-val.jpg"),
  scc_ice_awards = here("agenda-watch/contracts/output/scc-ice-awards.csv"))

# Read data
local_gov_raw <- read.csv(input_paths$us_local_gov,
                          colClasses = "character")

reg_state_gov_raw <- read.csv(input_paths$us_reg_state_gov,
                              colClasses = "character")

us_contracts_raw <- bind_rows(local_gov_raw,
                              reg_state_gov_raw) %>% 
  mutate(total_obligated_amount = as.numeric(total_obligated_amount))

# Select variables of interest
vars_contracts <- c("contract_award_unique_key",
                    "total_obligated_amount",
                    "period_of_performance_start_date",
                    "period_of_performance_current_end_date",
                    "awarding_agency_name",
                    "awarding_sub_agency_name",
                    "recipient_name_raw",
                    "prime_award_base_transaction_description",
                    "naics_code",
                    "naics_description",
                    "recipient_city_name",
                    "recipient_county_name",
                    "recipient_state_name",
                    "city_local_government", 
                    "county_local_government")

bay_counties <- c("ALAMEDA",
                  "CONTRA COSTA",
                  "MARIN",
                  "NAPA",
                  "SAN FRANCISCO",
                  "SAN MATEO",
                  "SANTA CLARA",
                  "SOLANO",
                  "SONOMA")
  
bay_contracts <- us_contracts_raw %>% 
  select(all_of(vars_contracts)) %>% 
  filter(recipient_county_name %in% bay_counties) %>% 
  mutate(recipient_name = case_when(str_detect(recipient_name_raw, "COLMA") ~ "COLMA, CITY OF",
                                    str_detect(recipient_name_raw, "ALAMEDA") ~ "ALAMEDA, COUNTY OF",
                                    str_detect(recipient_name_raw, "SCC") |
                                      str_detect(recipient_name_raw, "SANTA CLARA") ~ "SANTA CLARA, COUNTY OF",
                                    T ~ recipient_name_raw),
         # adjust last SCC contract end date to account for forum
         period_of_performance_current_end_date =
           ifelse(contract_award_unique_key == "CONT_AWD_70CDCR19P00000047_7012_-NONE-_-NONE-",
                  "2019-12-10", period_of_performance_current_end_date),
         perf_start_year = substr(period_of_performance_start_date, 1, 4),
         perf_end_year = substr(period_of_performance_current_end_date, 1, 4))

# SCC & ICE over time 
sc_contracts <- bay_contracts %>% 
  filter(recipient_name == "SANTA CLARA, COUNTY OF")

sc_ice_summ_by_year <- sc_contracts %>%
  group_by(perf_start_year, perf_end_year) %>%
  summarize(num_contracts = n(),
            total_amount = sum(as.numeric(total_obligated_amount))) %>% 
  mutate(year = map2(perf_start_year, perf_end_year, seq)) %>%
  unnest(year) %>%
  group_by(year) %>%
  summarize(num_active_contracts_sc = sum(num_contracts),
            total_active_amount_sc = sum(total_amount)) %>% 
  complete(year = min(year):2023,
           fill = list(num_active_contracts_sc = 0, total_active_amount_sc = 0))

sc_ice_count_by_year_gg <- ggplot(sc_ice_summ_by_year,
                               aes(x = year, y = num_active_contracts_sc)) +
  geom_col() +
  labs(x = "Year",
       y = "Number of active contracts",
       title = "Active Santa Clara County - ICE contracts",
       caption = "Source: USAspending.gov") +
  scale_x_continuous(breaks = seq(min(sc_ice_summ_by_year$year),
                                  max(sc_ice_summ_by_year$year), by = 1)) +
  theme_gdocs() +
  scale_fill_gdocs() +
  theme(panel.grid.major.x = element_blank())

sc_ice_value_by_year_gg <- ggplot(sc_ice_summ_by_year,
                               aes(x = year, y = total_active_amount_sc)) +
  geom_col() +
  labs(x = "Year",
       y = "Total value of active contracts (USD)",
       title = "Value of active ICE - Santa Clara County contracts",
       caption = "Source: USAspending.gov") +
  scale_x_continuous(breaks = seq(min(sc_ice_summ_by_year$year),
                                  max(sc_ice_summ_by_year$year), by = 1)) +
  theme_gdocs() +
  scale_fill_gdocs() +
  theme(panel.grid.major.x = element_blank()) +
  scale_y_continuous(labels = comma)

# Bay Area & ICE over time
bay_ice_summ_by_year <- bay_contracts %>%
  group_by(perf_start_year, perf_end_year) %>%
  summarize(num_contracts = n(),
            total_amount = sum(as.numeric(total_obligated_amount))) %>% 
  mutate(year = map2(perf_start_year, perf_end_year, seq)) %>%
  unnest(year) %>%
  group_by(year) %>%
  summarize(num_active_contracts_bay = sum(num_contracts),
            total_active_amount_bay = sum(total_amount)) %>% 
  left_join(sc_ice_summ_by_year, by = "year")

bay_ice_count_by_year_gg <- ggplot(bay_ice_summ_by_year, aes(x = year)) +
  geom_col(aes(y = num_active_contracts_bay), fill = "grey") +
  geom_col(aes(y = num_active_contracts_sc)) +
  labs(x = "Year",
       y = "Number of Active Contracts",
       title = "Active ICE - Bay Area Local Gov Contracts",
       subtitle = "Dark fill represents Santa Clara County") +
  scale_x_continuous(breaks = seq(min(bay_ice_summ_by_year$year),
                                  max(bay_ice_summ_by_year$year), by = 1)) + 
  scale_y_continuous(breaks = seq(min(bay_ice_summ_by_year$num_active_contracts_bay),
                                   max(bay_ice_summ_by_year$num_active_contracts_bay),
                                   by = 1))

bay_ice_value_by_year_gg <- ggplot(bay_ice_summ_by_year, aes(x = year)) +
  geom_col(aes(y = total_active_amount_bay), fill = "grey") +
  geom_col(aes(y = total_active_amount_sc)) +
  labs(x = "Year",
       y = "Number of Active Contracts",
       title = "Total Value of ICE - Bay Area Local Gov Contracts",
       subtitle = "Dark fill represents Santa Clara County") + 
  scale_x_continuous(breaks = seq(min(bay_ice_summ_by_year$year),
                                  max(bay_ice_summ_by_year$year), by = 1))

# Compare Bay Area local govs
bay_ice_local_gov_counts <- bay_contracts %>% 
  mutate(recipient_name = str_wrap(recipient_name, 30)) %>% 
  group_by(recipient_name) %>% 
  summarize(total_ice_contracts = n()) %>% 
  arrange(desc(total_ice_contracts))

bay_ice_local_gov_value <- bay_contracts %>% 
  mutate(recipient_name = str_wrap(recipient_name, 30)) %>% 
  group_by(recipient_name) %>% 
  summarize(total_ice_contracts_value = sum(total_obligated_amount)) %>% 
  arrange(desc(total_ice_contracts_value))

bay_ice_local_gov_counts$recipient_name <- factor(bay_ice_local_gov_counts$recipient_name ,
                                     levels = bay_ice_local_gov_counts$recipient_name [order(bay_ice_local_gov_counts$total_ice_contracts, decreasing = TRUE)])

min_year <- substr(min(us_contracts_raw$period_of_performance_start_date),
  1, 4)

max_year <- substr(max(us_contracts_raw$period_of_performance_start_date),
  1, 4)

bay_ice_local_gov_count_gg <- ggplot(bay_ice_local_gov_counts,
                                aes(x = recipient_name, y = total_ice_contracts)) +
  geom_col() +
  geom_text(aes(label = total_ice_contracts), vjust = -0.5) + 
  labs(x = "Recipient",
       y = "Number of total contracts",
       title = "Total Number of ICE - Bay Area Local Gov Contracts",
       subtitle = paste0(min_year, " - ", max_year),
       caption =  "Includes any ICE contract with a local government in the following counties:
       Alameda, Contra Costa, Marin, Napa, San Francisco, San Mateo, Santa Clara, Solano, and Sonoma") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

bay_ice_local_gov_value$recipient_name <- factor(bay_ice_local_gov_value$recipient_name,
                                                  levels = bay_ice_local_gov_value$recipient_name[order(bay_ice_local_gov_value$total_ice_contracts_value, decreasing = TRUE)])

bay_ice_local_gov_value_gg <- ggplot(bay_ice_local_gov_value,
                                     aes(x = recipient_name,
                                         y = total_ice_contracts_value)) +
  geom_col() +
  geom_text(aes(label = total_ice_contracts_value), vjust = -0.5) + 
  labs(x = "Recipient",
       y = "Value of total contracts",
       title = "Total Value of ICE - Bay Area Local Gov Contracts",
       subtitle = paste0(min_year, " - ", max_year),
       caption =  "Includes any ICE contract with a local government in the following counties:
       Alameda, Contra Costa, Marin, Napa, San Francisco, San Mateo, Santa Clara, Solano, and Sonoma") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Explore descriptions of contracts ====
# SANTA CLARA COUNTY
# Of 25 ICE contracts w/ Santa Clara County,

# prime_award_base_transaction_description:
# SLETS (or SCLET?): 8 ("law enforcement investigative research for DRO San Jose sub-office")
# CJIC criminal database access (or CJIS?): 4
# other mention of database access: 3
# firearms range: 1
# no description: 5

# naics_description:
# DATA PROCESSING, HOSTING, AND RELATED SERVICES: 15
# COMPUTING INFRASTRUCTURE PROVIDERS, DATA PROCESSING, WEB HOSTING, AND RELATED SERVICES: 2
# ADMINISTRATION OF PUBLIC HEALTH PROGRAMS: 1
# ALL OTHER MISCELLANEOUS SCHOOLS AND INSTRUCTION: 1 (firearms range)
# None: 6

# BAY AREA
# All contracts w/ San Jose are for parking
# Contracts w/ Alameda have no description of purpose
# All contracts w/ Colma for use of firing range

# Create list of SCC contracts for export ====
scc_for_export <- sc_contracts %>% 
  select(c("contract_award_unique_key",
           "total_obligated_amount",
           "period_of_performance_start_date",
           "period_of_performance_current_end_date",
           "recipient_name",
           "prime_award_base_transaction_description",
           "naics_code",
           "naics_description")) %>% 
  mutate(usaspending_link = paste0("https://www.usaspending.gov/award/",
                                   contract_award_unique_key))

write.csv(scc_for_export,
          output_paths$scc_ice_awards,
          row.names = F)

# Export graphs ====
ggsave(plot = sc_ice_count_by_year_gg,
       filename = output_paths$sc_ice_year_count, width = 10, height = 7)

ggsave(plot = sc_ice_value_by_year_gg,
       filename = output_paths$sc_ice_year_value, width = 10, height = 7)

ggsave(plot = bay_ice_count_by_year_gg,
       filename = output_paths$bay_ice_year_count, width = 10, height = 7)

ggsave(plot = bay_ice_value_by_year_gg,
       filename = output_paths$bay_ice_year_value, width = 10, height = 7)

ggsave(plot = bay_ice_local_gov_count_gg,
       filename = output_paths$bay_local_gov_compare_count,
       width = 10, height = 7)

ggsave(plot = bay_ice_local_gov_value_gg,
       filename = output_paths$bay_local_gov_compare_val,
       width = 10, height = 7)

# done.
