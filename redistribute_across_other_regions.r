
  #====================================================================================================
  #
  # Redistribute some data proportionally
  # 
  #   Here we redistribute the estimated resident population from 'migratory, 'no usual address' 
  #   and 'unincorporated' zones proportionally to all the other LGAs in the state.
  #
  #====================================================================================================
  
    # Gather migratory, no usual address and unincorporated regions into own data frame sum by state
    total_non_lga_regions <- erp %>% 
      filter(grepl("Migratory", Region)) %>% 
      bind_rows(erp %>% filter(grepl("Unincorp", Region))) %>% 
      bind_rows(erp %>% filter(grepl("No usual", Region))) %>% 
      group_by(State, Sex, Year, Age_bracket) %>%
      summarise(Non_LGA = sum(Value))
    
    # Data frame with LGA regions by state
    lga_regions <- erp %>% 
      filter(!grepl("Migratory", Region) & !grepl("No usual", Region) & !grepl("Unincorp", Region)) %>%
      select(-Code)
    
    # Data frame with state totals of LGA regions
    total_lga_regions <- lga_regions %>%
      group_by(State, Sex, Year, Age_bracket) %>%
      summarise(LGA_total = sum(Value))
    
    # Calculate scaled estimates using previous two data frames
    lga_regions_scaled <- lga_regions %>%
      left_join(total_lga_regions) %>%
      left_join(total_non_lga_regions) %>%
      mutate(Scaled = Value + ((Value / LGA_total) * Non_LGA)) %>%
      select(-c(LGA_total, Non_LGA))
    
    # Use smart rounding to maintain cohort totals but round to nearest whole number
    lga_regions_rounded <- lga_regions_scaled %>%
      group_by(State, Sex, Year, Age_bracket) %>%
      mutate(Rounded = round_preserve_sum(Scaled, 0)) %>% 
      ungroup()
    
    # Check it all adds up
    erp %>% filter(State == "Victoria" & Sex == "Female") %>% summarise(sum(Value)) ==
      lga_regions_rounded %>% filter(State == "Victoria" & Sex == "Female") %>% summarise(sum(Rounded))
    erp %>% filter(State == "Victoria" & Sex == "Male") %>% summarise(sum(Value)) ==
      lga_regions_rounded %>% filter(State == "Victoria" & Sex == "Male") %>% summarise(sum(Rounded))
    erp %>% filter(State == "Victoria" & Sex == "Female" & Year == 2010) %>% summarise(sum(Value)) ==
      lga_regions_rounded %>% filter(State == "Victoria" & Sex == "Female" & Year == 2010) %>% summarise(sum(Rounded))
    erp %>% filter(State == "Victoria" & Sex == "Male" & Year == 2006 & Age_bracket == "A04") %>% summarise(sum(Value)) ==
      lga_regions_rounded %>% filter(State == "Victoria" & Sex == "Male" & Year == 2006 & Age_bracket == "A04") %>% summarise(sum(Rounded))
    
    # Tidy up
    erp <- lga_regions_rounded %>% select(-c(Value, Scaled))