# Author: Sierra Wells
# Stanford University - COMM 177I
# comm177i/agenda-watch/contracts/src/timeline.R

# Packages
pacman::p_load(tidyverse,
               ggthemes,
               here,
               stringr)

# Files
input_paths <- list(
  us_local_gov = here("agenda-watch/contracts/input/us-local-gov-ice-contracts.csv"),
  us_reg_state_gov = here("agenda-watch/contracts/input/us-reg-state-gov-ice-contracts.csv"))

output_paths <- list(
  zoomed_in = here("agenda-watch/contracts/output/zoomed-in-timeline.jpg"),
  zoomed_out = here("agenda-watch/contracts/output/zoomed-out-timeline.jpg"))

# Read data
local_gov_raw <- read.csv(input_paths$us_local_gov,
                          colClasses = "character")

reg_state_gov_raw <- read.csv(input_paths$us_reg_state_gov,
                              colClasses = "character")

us_contracts_raw <- bind_rows(local_gov_raw,
                              reg_state_gov_raw) %>% 
  mutate(total_obligated_amount = as.numeric(total_obligated_amount))

# Filter & process contracts
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

scc_contracts <- us_contracts_raw %>% 
  select(all_of(vars_contracts)) %>% 
  filter(recipient_state_name == "CALIFORNIA") %>% 
  mutate(
    # standardize recipient names
    recipient_name = case_when(
      str_detect(recipient_name_raw, "SCC") |
        str_detect(recipient_name_raw, "SANTA CLARA") ~ "SANTA CLARA, COUNTY OF"),
    mentions_data = case_when(
      str_detect(naics_description, "DATA") |
       str_detect(prime_award_base_transaction_description, "CLET") |
        str_detect(prime_award_base_transaction_description, "SLET") |
        str_detect(prime_award_base_transaction_description, "CJIS") |
        str_detect(prime_award_base_transaction_description, "CJIC") ~ "Mentions data",
      T ~ "Does not mention data")) %>% 
  filter(recipient_name == "SANTA CLARA, COUNTY OF") %>% 
  arrange(period_of_performance_start_date) %>% 
  mutate(order = row_number(),
         # adjust last end date to account for forum
         period_of_performance_current_end_date =
           ifelse(contract_award_unique_key == "CONT_AWD_70CDCR19P00000047_7012_-NONE-_-NONE-",
                  "2019-12-10", period_of_performance_current_end_date),
         period_of_performance_start_date = as.Date(period_of_performance_start_date),
         period_of_performance_current_end_date = as.Date(period_of_performance_current_end_date))

# Important dates
non_contract_dates <- data.frame(
  desc = c("10/1/2018:\nCA Values Act data sharing guidelines go into effect",
           "6/4/2019:\nSanta Clara County Board Policy 3.54 amended",
           "12/10/2019:\nTRUTH Act forum held"),
  date = as.Date(c("2018-10-01",
                 "2019-06-04",
                 "2019-12-10")),
  y_pos = c(5, 10, 15),
  y_pos_zoomed = c(9, 10, 11))

# Graph timeline (all dates)
zoomed_out <- ggplot() +
  geom_rect(
    data = scc_contracts,
    aes(xmin = period_of_performance_start_date,
        xmax = period_of_performance_current_end_date,
        ymin = order - 0.4, ymax = order + 0.4, fill = mentions_data)) + 
  geom_vline(data = non_contract_dates,
             aes(xintercept = date),
             linetype = "dashed") +
  geom_label(data = non_contract_dates,
             aes(x = date, y =  y_pos,
                 label = str_wrap(desc, 30)),
             nudge_x = 1, nudge_y = 1) +
  coord_cartesian(xlim = c(as.Date("2004-06-30"), as.Date("2020-10-30"))) +
  labs(title = "Timeline of Santa Clara County-ICE contracts",
       fill = "Topic of contract",
       caption = "Source of contract data: USAspending.gov",
       y = "Order of contract", x = "Date") +
  theme_gdocs() +
  scale_fill_gdocs()

# Graph timeline (zoomed in)
zoomed_in <- ggplot() +
  geom_rect(
    data = scc_contracts,
    aes(xmin = period_of_performance_start_date,
        xmax = period_of_performance_current_end_date,
        ymin = (order - 17) - 0.4, ymax = (order - 17) + 0.4, fill = mentions_data)) + 
  geom_vline(data = non_contract_dates,
             aes(xintercept = date),
             linetype = "dashed") +
  geom_label(data = non_contract_dates,
             aes(x = date, y =  y_pos_zoomed,
                 label = str_wrap(desc, 30)),
             nudge_x = 1, nudge_y = 1) +
  geom_label(aes(
    label = str_wrap("Previously: an additional 18 contracts signed between Santa Clara & ICE", 30),
    x = as.Date("2017-08-30"), y = 10)) +
  coord_cartesian(xlim = c(as.Date("2017-06-30"), as.Date("2020-02-28")),
                  ylim = c(0, 15)) +
  labs(title = "Timeline of Santa Clara County-ICE contracts",
       fill = "Topic of contract",
       caption = "Source of contract data: USAspending.gov",
      x = "Date", y = "") +
  theme_gdocs() +
  scale_fill_gdocs() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank())

# Save plots
ggsave(plot = zoomed_out, filename = output_paths$zoomed_out,
       width = 16, height = 8)

ggsave(plot = zoomed_in, filename = output_paths$zoomed_in,
       width = 15, height = 7)

# done.