    library(dplyr)
    library(readr)
    library(readxl)
    library(data.table)
    library(tidycensus)
    library(purrr)
    library(ggplot2)
    library(rgeoda)
    library(sf)
    library(spdep)
    library(tseries)

    #test OLS
    library(spatialreg)
    library(lmtest)

This script contains the conversion from 3-digit ZIP code into county
level and the MAUP evaluation of the proportion of patients receiving at
least one opioid prescription from Indiana Medicaid. This script is part
of the methodology for the paper called “A spatiotemporal analysis of
opioid prescriptions in Indiana from 2015 to 2019”.

# Functions

The following function extract the total Indiana Medicaid enrollees from
the ACS.

    SelectCensus<-function(Year, geo = "zcta"){ 
      
      female_estimation<- get_acs(
        geography = geo,
        variables = "C27007_017",
        state = "IN", 
        survey = "acs5",
        year = as.numeric(Year)
      )
      
      female<-data.frame(NAME=female_estimation$NAME, estimate_female=female_estimation$estimate)
      
      male_estimation<- get_acs(
        geography = geo,
        variables = "C27007_007",
        state = "IN", 
        survey = "acs5",
        year = as.numeric(Year)
      )
      
      male<-data.frame(NAME=male_estimation$NAME, estimate_male=male_estimation$estimate)
      
      total_pop_Ind<- get_acs(
        geography = geo,
        variables =  "B01001_001", #"C27007_007",
        state = "IN", 
        survey = "acs5",
        geometry = TRUE,
        year = as.numeric(Year)
      )
      
      total_pop_Ind <- merge(total_pop_Ind,female, by="NAME")
      total_pop_Ind <- merge(total_pop_Ind,male, by="NAME")
      total_pop_Ind$estimate <- total_pop_Ind$estimate_male + total_pop_Ind$estimate_female
      
      return(total_pop_Ind)
    }

    # Load your 3-digit ZIP code data
    path <- "C:/Users/paula/OneDrive - University of Texas at San Antonio/UTSA/HEAL Lab/[A] Opioid prescription in Indiana/CSV files/08-20-24_LastPrescriptionByYear_2015to2019_19to64_IndianaDatasetFinal.csv"

    zip3_data <- fread(path)
    zip3_data <- zip3_data %>%
      mutate(
        FirstThreeDigits = as.character(Recipient_Addr_Zip5)
      )

    #Join the dataframes of crosswalks per year 

    #2015
    crosswalk_2015 <-  read_excel("C:/Users/paula/OneDrive - University of Texas at San Antonio/UTSA/HEAL Lab/[A] Opioid prescription in Indiana/Manuscripts/Final scripts/Crosswalk_MAUP/ZIP_COUNTY_032015.xlsx", col_types =  c("text", "text", "numeric","numeric","numeric","numeric")) %>% 
      mutate(
        year = 2015
      )

    #2016
    crosswalk_2016 <-  read_excel("C:/Users/paula/OneDrive - University of Texas at San Antonio/UTSA/HEAL Lab/[A] Opioid prescription in Indiana/Manuscripts/Final scripts/Crosswalk_MAUP/ZIP_COUNTY_032016.xlsx", col_types =  c("text", "text", "numeric","numeric","numeric","numeric")) %>% 
      mutate(
        year = 2016
      )


    #2017
    crosswalk_2017 <-  read_excel("C:/Users/paula/OneDrive - University of Texas at San Antonio/UTSA/HEAL Lab/[A] Opioid prescription in Indiana/Manuscripts/Final scripts/Crosswalk_MAUP/ZIP_COUNTY_032017.xlsx", col_types =  c("text", "text", "numeric","numeric","numeric","numeric")) %>% 
      mutate(
        year = 2017
      )


    #2018
    crosswalk_2018 <-  read_excel("C:/Users/paula/OneDrive - University of Texas at San Antonio/UTSA/HEAL Lab/[A] Opioid prescription in Indiana/Manuscripts/Final scripts/Crosswalk_MAUP/ZIP_COUNTY_032018.xlsx", col_types =  c("text", "text", "numeric","numeric","numeric","numeric"))

    colnames(crosswalk_2018) <- c("ZIP","COUNTY","RES_RATIO","BUS_RATIO","OTH_RATIO","TOT_RATIO")

    crosswalk_2018 <- crosswalk_2018 %>% 
      mutate(
        year = 2018
      )

    #2019
    crosswalk_2019 <-  read_excel("C:/Users/paula/OneDrive - University of Texas at San Antonio/UTSA/HEAL Lab/[A] Opioid prescription in Indiana/Manuscripts/Final scripts/Crosswalk_MAUP/ZIP_COUNTY_032019.xlsx", col_types =  c("text", "text", "numeric","numeric","numeric","numeric"))

    colnames(crosswalk_2019) <- c("ZIP","COUNTY","RES_RATIO","BUS_RATIO","OTH_RATIO","TOT_RATIO")

    crosswalk_2019 <- crosswalk_2019 %>% 
      mutate(
        year = 2019
      )

    crosswalk <- bind_rows(crosswalk_2015, crosswalk_2016, crosswalk_2017, crosswalk_2018, crosswalk_2019)

# Conversion from 3-digit ZIP codes to county

Obtain the total Indiana Medicaid enrollees per year.

    # Define years to collect data from
    years <- 2015:2019

    # Run the SelectCensus function for each year and combine results
    combined_data_ACS<- map_dfr(years, function(y) {
      result <- SelectCensus(y)  # Run function for the year
      result$year <- y  # Add a year column
      return(result)
    })

Aggregate total Indiana Medicaid enrollees at the 3-digit ZIP code
level.

    combined_data_ACS$FirstThreeDigits <- substr(combined_data_ACS$GEOID, 1, 3)
      
    # Summarize population counts by 3-digit ZIP codes
    population_counts <- combined_data_ACS %>%
      group_by(FirstThreeDigits, year) %>%
      summarize(total=sum(estimate)) %>% 
      ungroup()

Joined the ACS with the FSSA dataset.

    zip3_data_aggregate <- zip3_data %>% 
      group_by(FirstThreeDigits, year) %>% 
      count %>% 
      mutate(
        total_patients = n
      ) %>% 
      ungroup() %>% 
      select(-n) %>% 
      left_join(population_counts, by = c("FirstThreeDigits", "year"))

Join with crosswalk tables and calculate weighted redistribution.

    crosswalk <- crosswalk %>%
      filter(substr(ZIP, 1, 3) %in% unique(zip3_data$FirstThreeDigits)) %>% 
      mutate(ZIP3 = substr(ZIP, 1, 3))

    # Merge your data with the crosswalk
    merged_data <- zip3_data_aggregate %>%
      left_join(crosswalk, by = c('FirstThreeDigits' = 'ZIP3', "year"))

    merged_data <- merged_data %>%
      group_by(COUNTY, year) %>% 
      # mutate(pop_weighted_value = normal_count * RES_RATIO) %>% 
      mutate(pop_weighted_value = total_patients * RES_RATIO) %>%
      mutate(total_Weighted = total * RES_RATIO) %>% 
      ungroup()

Calculate proportion of patients receiving at least one opioid
prescription per 1,000 population.

    # Aggregate to the county level using population-based weighting
    county_pop_aggregated <- merged_data %>%
      group_by(COUNTY, year) %>%
      summarise(pop_weighted_sum = sum(pop_weighted_value, na.rm = TRUE),
                total_Weighted_sum = sum(total_Weighted, na.rm = TRUE)) %>% 
      mutate(
        county_opioid_rate = (pop_weighted_sum / total_Weighted_sum) * 1000  # Compute final rate
      ) %>% 
      ungroup()

Obtain map of counties.

    map_county <- get_acs(
      geography = "county",
      variables =  "B01001_001", #"C27007_007",
      state = "IN", 
      survey = "acs5",
      geometry = TRUE,
      year = 2015
    )

    map_county <- map_county %>% 
      left_join(county_pop_aggregated, by = c('GEOID' = 'COUNTY') )

# OLS regression analysis

To explore how the scale is affecting the results, we run the OLS
bivariate regression model between year and the proportion of patients
receiving at least one opioid prescription per spatial unit.

First, identify the results at the 3-digit ZIP code level.

    # Define years to collect data from
    years <- 2015:2019

    # Run the SelectCensus function for each year and combine results
    zip_codes<- map_dfr(years, function(y) {
      result <- SelectCensus(y)  # Run function for the year
      result$year <- y  # Add a year column
      return(result)
    })


    data_unique_patients_per_Year_all <- zip3_data %>% 
      arrange(desc(AllDate), DE_Identified_Recipient_ID) %>%
      group_by(DE_Identified_Recipient_ID, year) %>% 
      slice(1) %>% 
      ungroup()

    zip_codes$FirstThreeDigits <- substr(zip_codes$GEOID, 1, 3)

    # Summarize population counts by 3-digit ZIP codes
    population_counts <- zip_codes %>%
      group_by(FirstThreeDigits, year) %>%
      summarize(total=sum(estimate)) %>% 
      ungroup()

    data_unique_patients_per_Year_all_2 <- data_unique_patients_per_Year_all %>% 
      dplyr::count(Recipient_Addr_Zip5, year)

    colnames(data_unique_patients_per_Year_all_2)[1]="FirstThreeDigits"

    #Merge between total estimates from ACS and our cohort 
    merged_data <- merge(population_counts, data_unique_patients_per_Year_all_2, by = c("FirstThreeDigits", "year"))
    merged_data$normal_count<- (merged_data$n/merged_data$total)*1000

Identify the counties and the connection with 3-digit ZIP codes.

    counties_3iditsZip <- crosswalk %>% 
      select(COUNTY, ZIP3) %>% 
      unique 

    colnames(counties_3iditsZip)[1] <- "GEOID"
    colnames(counties_3iditsZip)[2] <- "FirstThreeDigits"

Unify proportion by county and by 3-digit ZIP code level.

    prop_county_3digit  <- map_county %>% 
      st_drop_geometry() %>% 
      select(GEOID, NAME, year, county_opioid_rate) %>% 
      left_join(counties_3iditsZip, by = "GEOID") %>% 
      left_join(dplyr::select(merged_data, FirstThreeDigits, year, normal_count), by = c("FirstThreeDigits", "year"))

## County

Run the OSL model - county level

    # Perform OLS regression
    ols_model_county <- lm(year ~ county_opioid_rate, data = map_county)

    # Display summary results
    summary(ols_model_county)

Evaluate spatial autocorrelation in residuals - county model

    # Define neighbors for county-level spatial units
    county_neighbors <- poly2nb(map_county)

    # Convert to a row-standardized spatial weights matrix
    county_weights <- nb2listw(county_neighbors, style = "W", zero.policy = TRUE)

    # Compute OLS residuals
    ols_residuals_county <- residuals(ols_model_county)

    # Moran’s I test for spatial autocorrelation in residuals
    moran_test <- moran.test(ols_residuals_county, county_weights)

    # Print results
    moran_test

If spatial depends exists (spatial autocorrelation of the residuals of
OLS):

    # Spatial Lag Model (SLM)
    slm_model_county <- lagsarlm(year ~ county_opioid_rate, 
                           data = map_county, 
                           listw = county_weights, 
                           method = "eigen")

    # Print results
    summary(slm_model_county)

Normality using Jarque-Bera test (residuals)

    jb_test_county <- jarque.bera.test(ols_residuals_county)


    jb_test_county

Reject normality assumption, residuals is not normally distributed.

Breush-Pagan test

    bptest(year ~ county_opioid_rate, 
                           data = map_county)

## 3-digit ZIP

Perform OLS model at the digit ZIP code

    # Perform OLS regression
    ols_model_3zip <- lm(year ~ normal_count, data = merged_data)

    # Display summary results
    summary(ols_model_3zip)

Evaluate spatial autocorrelation in residuals - 3-digit ZIP code model

    # Define neighbors for county-level spatial units
    threeZip_neighbors <- poly2nb(merged_data)

    # Convert to a row-standardized spatial weights matrix
    threeZip_weights <- nb2listw(threeZip_neighbors, style = "W", zero.policy = TRUE)

    # Compute OLS residuals
    ols_residuals_3Zip <- residuals(ols_model_3zip)

    # Moran’s I test for spatial autocorrelation in residuals
    moran_test <- moran.test(ols_residuals_3Zip, threeZip_weights)

    # Print results
    moran_test

If spatial depends exists (spatial autocorrelation of the residuals of
OLS):

    library(spatialreg)
    # Spatial Lag Model (SLM)
    slm_model_3zip  <- lagsarlm(year ~ normal_count, 
                           data = merged_data, 
                           listw = threeZip_weights, 
                           method = "eigen")

    # Print results
    summary(slm_model_3zip)

Normality using Jarque-Bera test (residuals)

    jb_test_3ZIP <- jarque.bera.test(ols_residuals_3Zip)


    jb_test_3ZIP

Reject normality assumption, residuals is not normally distributed.

Breush-Pagan test

    bptest(year ~ normal_count, data = merged_data)
