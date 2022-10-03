library(tidyverse)
library(writexl)
library(here)
library(spData)

sp <- read_csv(here::here("data", "bat_species.csv")) %>% select(Genus, Species) %>% unite("bat", 1:2, remove=T, sep = " ")

states <- us_states["NAME"]

c_names <- c("sci_name", "common_name", "num_2005_states","states_2005", "num_2015_states","states_2015", "tax_group")

sgcn <- read.delim(here::here("data", "SGCN_data.csv"), header = T) %>%  
  separate(., 1, into = c_names, sep="\\|") %>% select(1,6) %>% 
  full_join(sp, by = c("sci_name" = "bat")) %>% filter(sci_name %in% sp$bat) %>% 
  mutate(states_2015 = na_if(states_2015, "")) 

sgcn_long <- sgcn %>% 
  separate_rows(., states_2015, sep=",") %>% arrange(sci_name)

state_sp_list <- sgcn_long %>% group_by(states_2015) %>% summarise(text = str_c(sci_name, collapse = ", "))

total_sgcn <- sgcn_long %>% group_by(states_2015) %>% summarise(num_SGCN = n())

total_states <- sgcn_long %>% group_by(sci_name) %>% summarise(num_states= n())

sgcn_long$yesno <- if_else(is.na(sgcn_long$states_2015) | sgcn_long$states_2015=="", 0, 1)

data <- sgcn_long %>% pivot_wider(names_from=sci_name, values_from = yesno, values_fill=0) %>% 
  drop_na(states_2015) %>% arrange(states_2015)

#add in any states that don't have any SGCN
data <- data %>% add_row(states_2015 = setdiff(states[[1]], data$states_2015)) %>% replace(is.na(.), 0)

sheets <- list("state list for each species" = sgcn, 
               "species list for each state" = state_sp_list,
               "# SGCN by state" = total_sgcn,
               "# states by SGCN" = total_states,
               "data" = data)

shiny_data <- data %>% left_join(state_sp_list, by = c('states_2015'='states_2015'))

write.csv(shiny_data, here::here("data", "sgcn_final_data.csv"))
write_xlsx(sheets, here::here("data", "SGCN_info.xlsx"))
