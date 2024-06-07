# Load necessary libraries
library(dplyr)
library(DBI)

# Source external R scripts
source("/projects/hcpr/activity/s112837.CDW/production/rpgm/onthefly/cost_dss_data.R")
source("/projects/hcpr/activity/s112837.CDW/production/rpgm/onthefly/inflate.R")

cost_dss_data <- function(indsn, costeddsn) {
  # Define paths to reference data
  ref_path <- "/projects/hcpr/activity/s112837.CDW/production/ref_master"
  
  # Load reference data
  ref_feesched <- readRDS(file.path(ref_path, "feesched.rds"))
  ref_headersched <- readRDS(file.path(ref_path, "header.rds"))
  ref_imputerate <- readRDS(file.path(ref_path, "imputerate.rds"))
  ref_mayo_ccr <- readRDS(file.path(ref_path, "mayo_ccr.rds"))
  
  # Rename columns in the input dataset
  indsn <- indsn %>%
    rename(year = fyear, cpt4 = cpt)
  
  # Split the data into categories
  zerochg <- indsn %>% filter(actchrg %in% c(0, NA))
  ubcode <- indsn %>% filter(cpt4 %in% c('', '00000') | (partbchg %in% c(0, NA) & location == 'Hospital'))
  header <- indsn %>% filter(cpt4hdr == 'H')
  cpt4 <- indsn %>% filter(!(actchrg %in% c(0, NA) | cpt4 %in% c('', '00000') | cpt4hdr == 'H' | (partbchg %in% c(0, NA) & location == 'Hospital')))
  
  # Process CPT4 data
  cpt4 <- cpt4 %>%
    mutate(mod = case_when(
      cpt4mod %in% c('26', 'TC') ~ cpt4mod,
      cpt4mod2 %in% c('26', 'TC') ~ cpt4mod2,
      cpt4mod3 %in% c('26', 'TC') ~ cpt4mod3,
      cpt4mod4 %in% c('26', 'TC') ~ cpt4mod4,
      TRUE ~ NA_character_
    ))
  
  # Merge with reference fee schedule
  cpt4_costed <- cpt4 %>%
    left_join(ref_feesched, by = c("cpt4", "mod", "year")) %>%
    mutate(cost_partb = case_when(
      location == 'Hospital' & orgvolum != 0 ~ feef * orgvolum * adj1 * adj2 * adj3 * adj4 * adj5 * adj6 * adj7,
      location == 'Hospital' & orgvolum == 0 ~ feef * adj1 * adj2 * adj3 * adj4 * adj5 * adj6 * adj7,
      location != 'Hospital' & orgvolum != 0 ~ fee * orgvolum * adj1 * adj2 * adj3 * adj4 * adj5 * adj6 * adj7,
      location != 'Hospital' & orgvolum == 0 ~ fee * adj1 * adj2 * adj3 * adj4 * adj5 * adj6 * adj7,
      TRUE ~ NA_real_
    )) %>%
    mutate(cmeth_partb = ifelse(is.na(cost_partb), NA_character_, "F"))
  
  # Process Header data
  header_costed <- header %>%
    left_join(ref_headersched, by = c("feederky", "year")) %>%
    mutate(cost_partb = fee * orgvolum, cmeth_partb = 'H')
  
  # Impute costs
  cpt4_costed <- cpt4_costed %>%
    left_join(ref_imputerate, by = "year") %>%
    mutate(cost_partb = ifelse(cost_partb == 0, partbchg * imputeccr, cost_partb),
           cmeth_partb = ifelse(cost_partb == 0, 'IMP', cmeth_partb)) %>%
    select(-imputeccr)
  
  # Apply CCR
  ubcode <- bind_rows(ubcode, filter(cpt4_costed, !is.na(partachg))) %>%
    left_join(ref_mayo_ccr, by = c("ubcode", "year")) %>%
    mutate(cost_parta = ifelse(partachg != 0, partachg * ccr, 0)) %>%
    mutate(cmeth_parta = 'C') %>%
    select(-ccr)
  
  # Process zero charges
  zerochg <- zerochg %>%
    mutate(cost_parta = 0, cost_partb = 0, cmeth_parta = 'Z', cmeth_partb = 'Z')
  
  # Combine costed files
  costed <- bind_rows(cpt4_costed, header_costed, ubcode, zerochg) %>%
    select(encnbr, clinic, dateserv, cpt4, cpt4hdr, cpt4mod, cpt4mod2, cpt4mod3, cpt4mod4, feederky, orgvolum, location,
           ubcode, year, billstat, ldx1, ldx2, ldx3, ldx4, cost_parta, cost_partb, cmeth_parta, cmeth_partb) %>%
    mutate(across(c(cost_parta, cost_partb), ~replace_na(., 0)))
  
  # Output the costed data
  assign(costeddsn, costed, envir = .GlobalEnv)
}

# Example usage
cost_dss_data(indsn = your_input_data, costeddsn = "costed_data")

inflate <- function(dsn, inflateyear) {
  # Load necessary library
  library(dplyr)
  
  # Define paths to reference data
  ref_path <- "/projects/hcpr/activity/s112837.CDW/production/ref_master"
  
  # Load inflation reference data
  ref_inflate <- readRDS(file.path(ref_path, "inflation.rds")) %>%
    filter(inflateyear == inflateyear) %>%
    select(year, index)
  
  # Sort reference inflation data by year
  ref_inflate <- ref_inflate %>% arrange(year)
  
  # Merge the input dataset with the inflation reference data
  dsn <- dsn %>%
    left_join(ref_inflate, by = "year") %>%
    mutate(cost_parta_inflated = cost_parta * index,
           cost_partb_inflated = cost_partb * index,
           total_cost_inflated = cost_parta_inflated + cost_partb_inflated) %>%
    select(-index, -inflateyear)
  
  # Rename and format columns
  names(dsn) <- gsub("cost_parta_inflated", paste0("cost_parta", inflateyear), names(dsn))
  names(dsn) <- gsub("cost_partb_inflated", paste0("cost_partb", inflateyear), names(dsn))
  names(dsn) <- gsub("total_cost_inflated", paste0("total_cost", inflateyear), names(dsn))
  
  # Assign labels
  dsn <- dsn %>%
    mutate(across(starts_with("cost_parta"), ~ .x, .names = "Uninflated Cost (Part A)"),
           across(starts_with("cost_partb"), ~ .x, .names = "Uninflated Cost (Part B)"),
           across(starts_with("cost_parta"), ~ .x, .names = paste0("Cost (Part A) inflated to ", inflateyear, " dollars")),
           across(starts_with("cost_partb"), ~ .x, .names = paste0("Cost (Part B) inflated to ", inflateyear, " dollars")),
           across(starts_with("total_cost"), ~ .x, .names = paste0("Total Cost inflated to ", inflateyear, " dollars")))
  
  # Return the updated dataset
  return(dsn)
}

# Example usage
your_dataset <- inflate(dsn = your_dataset, inflateyear = 2014)

