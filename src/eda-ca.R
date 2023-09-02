# Author: Sierra Wells
# Stanford University - COMM 177I
# comm177i/agenda-watch/contracts/src/eda-ca.R

# Packages
pacman::p_load(tidyverse,
               here,
               ggthemes,
               scales,
               stringr)

# Files
input_paths <- list(
  us_local_gov = here("agenda-watch/contracts/input/us-local-gov-ice-contracts.csv"),
  us_reg_state_gov = here("agenda-watch/contracts/input/us-reg-state-gov-ice-contracts.csv"))

output_paths <- list(
  ca_local_gov_compare_count = here("agenda-watch/contracts/output/ca-local-gov-compare-count.jpg"),
  ca_local_gov_compare_val = here("agenda-watch/contracts/output/ca-local-gov-compare-val.jpg"),
  ca_db_local_gov_compare_count = here("agenda-watch/contracts/output/ca-db-local-gov-compare-count.jpg"),
  ca_db_local_gov_compare_val = here("agenda-watch/contracts/output/ca-db-local-gov-compare-val.jpg"),
  ca_db_contracts = here("agenda-watch/contracts/output/ca-db-contracts.csv"))

# Read data
local_gov_raw <- read.csv(input_paths$us_local_gov,
                          colClasses = "character")

reg_state_gov_raw <- read.csv(input_paths$us_reg_state_gov,
                              colClasses = "character")

us_contracts_raw <- bind_rows(local_gov_raw,
                              reg_state_gov_raw) %>% 
  mutate(total_obligated_amount = as.numeric(total_obligated_amount))

# Filter for contracts in CA
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

ca_contracts <- us_contracts_raw %>% 
  select(all_of(vars_contracts)) %>% 
  filter(recipient_state_name == "CALIFORNIA") %>% 
  mutate(recipient_name = case_when(str_detect(recipient_name_raw, "COLMA") ~ "COLMA, CITY OF",
                                    str_detect(recipient_name_raw, "ALAMEDA") ~ "ALAMEDA, COUNTY OF",
                                    str_detect(recipient_name_raw, "SCC") |
                                      str_detect(recipient_name_raw, "SANTA CLARA") ~ "SANTA CLARA, COUNTY OF",
                                    str_detect(recipient_name_raw, "ARJIS") ~ "AUTOMATED REGIONAL JUSTICE INFORMATION SYSTEM",
                                    str_detect(recipient_name_raw, "CORRECTIONS") ~ "CORRECTIONS, CALIFORNIA DEPT OF",
                                    str_detect(recipient_name_raw, "KERN") ~ "KERN, COUNTY OF",
                                    str_detect(recipient_name_raw, "SACRAMENTO") &
                                      str_detect(recipient_name_raw, "COUNTY") ~ "SACRAMENTO, COUNTY OF",
                                    (str_detect(recipient_name_raw, "SACRAMENTO") &
                                       str_detect(recipient_name_raw, "CITY")) | 
                                      str_detect(recipient_name_raw, "SAC CITY") ~ "SACRAMENTO, CITY OF",
                                    str_detect(recipient_name_raw, "SAN DIEGO") &
                                      str_detect(recipient_name_raw, "COUNTY") ~ "SAN DIEGO, COUNTY OF",
                                    str_detect(recipient_name_raw, "IMPERIAL") ~ "IMPERIAL, COUNTY OF",
                                    str_detect(recipient_name_raw, "ROSEVILLE") ~ "ROSEVILLE, CITY OF",
                                    str_detect(recipient_name_raw, "SAN JOAQUIN") ~ "SAN JOAQUIN, COUNTY OF",
                                    str_detect(recipient_name_raw, "TEALE") ~ "TEALE DATA SYSTEMS",
                                    str_detect(recipient_name_raw, "ORANGE") ~ "ORANGE, COUNTY OF",
                                    str_detect(recipient_name_raw, "VENTURA") ~ "VENTURA, COUNTY OF",
                                    str_detect(recipient_name_raw, "POMONA") ~ "POMONA, CITY OF",
                                    T ~ recipient_name_raw),
         perf_start_year = substr(period_of_performance_start_date, 1, 4),
         perf_end_year = substr(period_of_performance_current_end_date, 1, 4))

# Filter for contracts w/ local govs
ca_local_gov <- ca_contracts %>% 
  filter(str_detect(recipient_name, "COUNTY") |
           str_detect(recipient_name, "CITY") |
           str_detect(recipient_name, "GOVERNMENT"))

# Compare CA local govs by contract count & value
ca_local_gov_summary <- ca_local_gov %>% 
  mutate(recipient_name = str_wrap(recipient_name, 30)) %>% 
  group_by(recipient_name) %>% 
  summarize(total_ice_contracts = n(),
            total_ice_contracts_value = sum(total_obligated_amount))

min_year <- substr(
  min(us_contracts_raw$period_of_performance_start_date),
  1, 4)

ca_local_gov_count_gg <- ggplot(ca_local_gov_summary,
                                    aes(y = fct_rev(fct_reorder(recipient_name, recipient_name)),
                                        x = total_ice_contracts)) +
  geom_col() +
  geom_text(aes(label = total_ice_contracts), hjust = -0.5) + 
  labs(y = "Recipient",
       x = "Cumulative total of contracts signed",
       title = "Total Number of ICE - CA Local Gov Contracts",
       subtitle = paste0("Since ", min_year),
       caption = "Source: USAspending.gov") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme_gdocs() +
  scale_fill_gdocs() +
  theme(panel.grid.major.y = element_blank())

ca_ice_local_gov_value <- ca_contracts %>% 
  filter(str_detect(recipient_name, "COUNTY") |
           str_detect(recipient_name, "CITY") |
           str_detect(recipient_name, "GOVERNMENT")) %>% 
  mutate(recipient_name = str_wrap(recipient_name, 30)) %>% 
  group_by(recipient_name) %>% 
  summarize(total_ice_contracts_value = sum(total_obligated_amount)) %>% 
  arrange(desc(total_ice_contracts_value))

ca_local_gov_value_gg <- ggplot(ca_local_gov_summary,
                                    aes(y = fct_rev(fct_reorder(recipient_name, recipient_name)),
                                        x = total_ice_contracts_value)) +
  geom_col() +
  geom_text(aes(label = total_ice_contracts_value), hjust = -0.1) + 
  labs(y = "Recipient",
       x = "Cumulative total value of contracts",
       title = "Total Value of ICE - CA Local Gov Contracts",
       subtitle = paste0("Since ", min_year),
       caption = "Source: USAspending.gov") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  coord_cartesian(xlim = c(0, 500000)) +
  theme_gdocs() +
  scale_fill_gdocs() +
  scale_x_continuous(labels = comma) +
  theme(panel.grid.major.y = element_blank())

# Filter for database contracts w/ local govs
ca_local_gov_db_var <- ca_local_gov %>% 
  mutate(data_sharing = case_when(
    str_detect(naics_description, "DATA") |
      str_detect(prime_award_base_transaction_description, "DATA") |
      str_detect(prime_award_base_transaction_description, "SLET") |
      str_detect(prime_award_base_transaction_description, "CLET") |
      str_detect(prime_award_base_transaction_description, "CJIC") |
      str_detect(prime_award_base_transaction_description, "CJIS") |
      str_detect(prime_award_base_transaction_description, "ESUN") |
      str_detect(prime_award_base_transaction_description, "E-SUN") |
      str_detect(prime_award_base_transaction_description, "ARJIS") |
      str_detect(prime_award_base_transaction_description, "LAW ENFORCEMENT TELECOMMUNICATIONS SYSTEM") ~ T,
    T ~ F))

ca_db_local_gov <- ca_local_gov_db_var %>% 
  filter(data_sharing == T)

# Compare CA local govs by *database sharing* contract count & value
ca_db_local_gov_summ <- ca_db_local_gov %>% 
  mutate(recipient_name = str_wrap(recipient_name, 30)) %>% 
  group_by(recipient_name) %>% 
  summarize(total_ice_contracts = n(),
            total_ice_contracts_value = sum(total_obligated_amount))

ca_db_local_gov_count_gg <- ggplot(ca_db_local_gov_summ,
                                       aes(y = fct_rev(fct_reorder(recipient_name, recipient_name)),
                                           x = total_ice_contracts)) +
  geom_col() +
  geom_text(aes(label = total_ice_contracts), hjust = -0.5) + 
  labs(y = "Recipient",
       x = "Cumulative total number of data sharing contracts signed",
       title = "Total Number of ICE - CA Local Gov Contracts Related to Database Sharing",
       subtitle = paste0("Since ", min_year),
       caption = "Source: USAspending.gov") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme_gdocs() +
  scale_fill_gdocs() +
  theme(panel.grid.major.y = element_blank())

ca_db_local_gov_value_gg <- ggplot(ca_db_local_gov_summ,
                                       aes(y = fct_rev(fct_reorder(recipient_name, recipient_name)),
                                           x = total_ice_contracts_value)) +
  geom_col() +
  geom_text(aes(label = total_ice_contracts_value), hjust = -0.1) + 
  labs(y = "Recipient",
       x = "Cumulative total value of data sharing contracts",
       title = "Total Value of ICE - CA Local Gov Contracts Related to Database Sharing",
       subtitle = paste0("Since ", min_year),
       caption = "Source: USAspending.gov") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  coord_cartesian(xlim = c(0, 450000)) +
  theme_gdocs() +
  scale_fill_gdocs() +
  scale_x_continuous(labels = comma) +
  theme(panel.grid.major.y = element_blank())

# Export graphs
ggsave(plot = ca_local_gov_count_gg,
       filename = output_paths$ca_local_gov_compare_count,
       width = 12, height = 9)

ggsave(plot = ca_local_gov_value_gg,
       filename = output_paths$ca_local_gov_compare_val,
       width = 14, height = 7)

ggsave(plot = ca_db_local_gov_count_gg,
       filename = output_paths$ca_db_local_gov_compare_count,
       width = 14, height = 7)

ggsave(plot = ca_db_local_gov_value_gg,
       filename = output_paths$ca_db_local_gov_compare_val,
       width = 14, height = 7)

# Export df of CA local gov data sharing contracts
write.csv(ca_db_local_gov %>% select(all_of(vars_contracts)), 
          output_paths$ca_db_contracts, row.names = F)

# done.