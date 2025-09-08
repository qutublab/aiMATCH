
# Filtering, processing and analyze the data

The script contains all the necessary steps for the statistical tests
between demographic group estimations from Indiana Medicaid
reimbursement claims data from individuals receiving an opioid
prescription, from 2015 to 2019. This script is part of the methodology
for the paper called “A spatiotemporal analysis of opioid prescriptions
in Indiana from 2015 to 2019”.

## Libraries

The following libraries are necessary to run the script.

    library(dplyr)
    library(data.table)
    library(tableone)
    library(chisq.posthoc.test)
    library(tidycensus)
    library(readr)

    library(ggplot2)
    library(tidyr)
    library(MASS)
    library(broom)
    library(gt)
    library(purrr)
    library(sf)

# Functions

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

## Processing of the dataset

    path <- "C:/Users/paula/OneDrive - University of Texas at San Antonio/UTSA/HEAL Lab/[A] Opioid prescription in Indiana/CSV files/OpioidPrescription2014to2020.csv"

First, load the data and start to delete repeated claims.

    Indiana_df <-  read_csv(path, col_types = cols(.default = "c"))

    #Eliminating duplicates
    Indiana_df <- Indiana_df[!duplicated(Indiana_df),]
     
    print(paste0("Patients after eliminating duplicated rows: ", length(unique(Indiana_df$DE_Identified_Recipient_ID))))

Then, eliminate NA values.

    print(paste0("Rows with NA values: ", sum(is.na(Indiana_df))))

    Indiana_df <- na.omit(Indiana_df)

    print(paste0("After deleting NA values: ", length(unique(Indiana_df$DE_Identified_Recipient_ID))))

Some patients appear with 2 gender. We are going to delete those. Also,
the patients with unknown gender.

    #### Delete the patients with more than 2 gender 
    Patients_Gender <-Indiana_df %>% 
      group_by(DE_Identified_Recipient_ID,Recipient_Gender) %>% 
      count %>% 
      ungroup %>% 
      group_by(DE_Identified_Recipient_ID) %>% 
      count %>%
      ungroup() %>% 
      filter(n > 1) %>% 
      dplyr::select(DE_Identified_Recipient_ID) %>% 
      unlist 

    Indiana_df <- Indiana_df %>% 
      filter(! DE_Identified_Recipient_ID %in% Patients_Gender & 
               Recipient_Gender != "U")

    print(paste0("Patients reported with more than 1 gender: ", length(Patients_Gender)))
    print(paste0("Patients after gender filtering: ", length(unique(Indiana_df$DE_Identified_Recipient_ID))))

Now, we deleted invalid entries of race.

    invalid_races <- Indiana_df %>% 
      group_by(Recipient_Race) %>% 
      count

    print(invalid_races)

    Indiana_df <- Indiana_df %>% 
      filter(!Recipient_Race %in% c("7", "8"))

    print(paste0("Patients after race filtering: ", length(unique(Indiana_df$DE_Identified_Recipient_ID))))

Now, we only want to keep patients aged 18 to 64.

    Indiana_df$Date_Birth_Recipient <- as.numeric(Indiana_df$Date_Birth_Recipient)

    Indiana_df<-Indiana_df %>% 
      filter(Date_Birth_Recipient >= 18 & Date_Birth_Recipient <= 64)

    print(paste0("Patients after age filtering: ", length(unique(Indiana_df$DE_Identified_Recipient_ID))))

Filtering also patients who resided outside Indiana.

    Indiana_df$Recipient_Addr_Zip5 <- as.numeric(Indiana_df$Recipient_Addr_Zip5)

    Indiana_df <- Indiana_df %>% 
      filter(Recipient_Addr_Zip5 <= 479 & Recipient_Addr_Zip5 >= 460) 

    print(paste0("Patients after residence filtering: ", length(unique(Indiana_df$DE_Identified_Recipient_ID))))

Finally, patients with records between 2015 to 2019.

    Indiana_df$Date_Begin_Service_Header <- as.numeric(Indiana_df$Date_Begin_Service_Header)

    Indiana_df <- Indiana_df %>% 
      filter(Date_Begin_Service_Header >= 732107 & Date_Begin_Service_Header <= 733932)

    print(paste0("Patients with a record between 2015 to 2019: ", length(unique(Indiana_df$DE_Identified_Recipient_ID))))

Change the NDC number by their respective drug name.

    opioidNDC <- readxl::read_excel("C:/Users/paula/OneDrive - University of Texas at San Antonio/UTSA/HEAL Lab/[A] Opioid prescription in Indiana/CSV files/OpioidNDC.xlsx")

    Allndc_opioids <- as.numeric(unlist(opioidNDC$`NDC 9 digit code`))
    OpioidNames <- unlist(opioidNDC$`Drug Name`)
    ndc_df <- unlist(unique(Indiana_df$NDC_Code))
    non_ndc  <- c()

    for(i in 1:length(ndc_df)){
      is_ndc = FALSE
      for(j in 1:length(Allndc_opioids)){
        if(grepl(Allndc_opioids[j], ndc_df[i])){
          is_ndc = TRUE
          Indiana_df$NDC_Code <- replace(Indiana_df$NDC_Code,Indiana_df$NDC_Code == ndc_df[i], OpioidNames[j])
          break
        }
      }
      if(is_ndc == FALSE){
        non_ndc <- c(non_ndc, ndc_df[i])
      }
    }

    Indiana_df <- Indiana_df %>% 
      filter(!NDC_Code %in% non_ndc)

    print(paste0("Patients after filtering just the ones who received opioid prescription: ", length(unique(Indiana_df$DE_Identified_Recipient_ID))))

## Prepare the data

First, we obtained the corresponding date according with the sequence of
numbers.

    ######## Convert number in respective dates ############

    ConvertNumber2Date <- function(lower, upper, year){

      start_date <- paste0(year,"-01-01")
      origin_date <- as.Date(start_date) - (lower - 1) 

      # Generate the sequence of dates from lower limit to upper limit
      date_sequence <- seq.Date(from = origin_date + lower - 1, to = origin_date + upper - 1, by = "day")

      number2date <- data.frame(Date_Begin_Service_Header = lower:upper, 
                                AllDate = date_sequence)
      
      return(number2date)
    }

    year_list <-c(2015,2016,2017,2018,2019)
    upper_date<-c(732471,732837,733202,733567,733932)
    lower_date<-c(732107,732472,732838,733203,733568)


    ConvertNumber2Date_df <- dplyr::bind_rows(lapply(1:length(year_list), function(x) ConvertNumber2Date(lower_date[x],
                                                                                   upper_date[x], 
                                                                                   year_list[x])))
    indiana_df_filter <- Indiana_df %>% 
      left_join(ConvertNumber2Date_df, 
                by = "Date_Begin_Service_Header")


    ######## Categorized dates in years , and respective letter in race ############


    indiana_df_filter <- indiana_df_filter %>% 
      mutate(
        year = case_when(
          Date_Begin_Service_Header >= 732107 & Date_Begin_Service_Header <=  732471 ~ 2015,
          Date_Begin_Service_Header >= 732472 & Date_Begin_Service_Header <=  732837 ~ 2016,
          Date_Begin_Service_Header >= 732838 & Date_Begin_Service_Header <=  733202 ~ 2017,
          Date_Begin_Service_Header >= 733203 & Date_Begin_Service_Header <=  733567 ~ 2018,
          Date_Begin_Service_Header >= 733568 & Date_Begin_Service_Header <=  733932 ~ 2019,
          TRUE ~ 0
        ), 
        race = case_when( 
          Recipient_Race == "H" ~ "Hispanic", 
          Recipient_Race == "B" ~ "Black",
          Recipient_Race == "C" ~ "White", 
          TRUE ~ "Other"
          ), 
        age = case_when(
          Date_Birth_Recipient >= 18 & Date_Birth_Recipient <= 25 ~ "18 to 25",
          Date_Birth_Recipient >= 26 & Date_Birth_Recipient <= 34 ~ "26 to 34",
          Date_Birth_Recipient >= 35 & Date_Birth_Recipient <= 44 ~ "35 to 44",
          Date_Birth_Recipient >= 45 & Date_Birth_Recipient <= 54 ~ "45 to 54",
          Date_Birth_Recipient >= 55 & Date_Birth_Recipient <= 64 ~ "55 to 64",
          TRUE ~ "Unknown"
        )
      )

    #path <- "C:/Users/paula/OneDrive - University of Texas at San Antonio/UTSA/HEAL Lab/[A] Opioid prescription in Indiana/CSV files/08-13-24_2015to2019_19to64_IndianaDatasetFinal.csv"

    # path <- "C:/Users/paula/OneDrive - University of Texas at San Antonio/UTSA/HEAL Lab/[A] Opioid prescription in Indiana/CSV files/08-13-24_2015to2019_19to64_IndianaDatasetFinal.csv"

    # indiana_df_filter <- fread(path)

    ######## Extract last entry for each year by patient ############
    indiana_df_LastPrescription <- indiana_df_filter %>% 
      arrange(desc(AllDate), DE_Identified_Recipient_ID) %>% 
      group_by(DE_Identified_Recipient_ID, year) %>% 
      slice(1) %>% 
      ungroup()

    print(paste0("Patients receiving an opioid prescription: ", length(unique(indiana_df_LastPrescription$DE_Identified_Recipient_ID))))

    #Save the new file. 

    # path <- "C:/Users/paula/OneDrive - University of Texas at San Antonio/UTSA/HEAL Lab/[A] Opioid prescription in Indiana/CSV files/08-13-24_2015to2019_19to64_IndianaDatasetFinal.csv"
    # 
    # fwrite(indiana_df_filter, path)

Then, we identify the urban/rural communities utilizing Rural-Urban
Commuting Area Codes (RUCA) using the 3-digit ZIP code zones from
Indiana.

The first plan consisted in convert RUCA which are in census tracts, to
ZIP codes, and then to 3-digit ZIP codes, using the relationship files
from US Census Bureau
<https://www.census.gov/geographies/reference-files/time-series/geo/relationship-files.2010.html#par_textimage_674173622>.

However, RUCA webpage already did it. Citing: “A second dataset applies
2010 RUCA classifications to ZIP code areas by transferring RUCA values
from the census tracts that comprise them.”

So, we are going to use the classification of RUCA using the ZIP code
areas.

    path <- "C:/Users/paula/OneDrive - University of Texas at San Antonio/UTSA/HEAL Lab/[A] Opioid prescription in Indiana/CSV files/RUCA2010zipcode.xlsx"

    RUCA_ZipCodes <- readxl::read_excel(path, 
                                        col_types = "text", 
                                        sheet = "Data")

RUCA has two columns depending of the primary and secondary RUCA codes.
Primary represents the whole number, eg., if you have 4.1, the whole
number would be the 4. The primary is the principal classification of
that census tract or, in this case, the ZIP code. But secondary are also
provided, citing:

“These 10 codes offer a relatively straightforward and complete
delineation of metropolitan and nonmetropolitan areas based on the size
and direction of primary commuting flows. However, secondary flows may
indicate other connections among rural and urban places. Thus, the
primary RUCA codes are further subdivided to identify areas where
classifications overlap, based on the size and direction of the
secondary, or second largest, commuting flow (table 2). For example, 1.1
and 2.1 codes identify areas where the primary flow is within or to a
metropolitan core, but another 30 percent or more commute to a larger
metropolitan core. Similarly, 10.1, 10.2, and 10.3 identify rural tracts
for which the primary commuting share is local, but more than 30 percent
also commute to a nearby metropolitan, micropolitan, or small town core,
respectively.”

In this case, the RUCA1 is the primary and the RUCA2 is the
primary+secondary. We used RUCA1. For more documentation, visit the
webpage
<https://www.ers.usda.gov/data-products/rural-urban-commuting-area-codes/documentation/>

Now, because we are going to work with Indiana, filter just the zip
codes for that zone.

    RUCA_ZipCodes_IN <- RUCA_ZipCodes %>% 
      filter(STATE == "IN")

    #Create same column in RUCA IN
    RUCA_ZipCodes_IN$FirstThreeDigits <- substr(RUCA_ZipCodes_IN$ZIP_CODE, 1,3)

However, if you observed, for example, the 3-digit ZIP code 460, you
realized that there were some of them with a RUCA value ranging from 1-3
(metropolitan) and other to 4-10, which it is known as rural.

**How can we summarize the RUCA for multiple 5-digit ZIP codes?**

A possible idea is to get a weighted average considering the population
for each 5-digit ZIP code.

*R**U**C**A*<sub>3 − *d**i**g**i**t*</sub> = /*f**r**a**c*/*s**u**m**R**U**C**A*<sub>5 − *d**i**g**i**t*</sub> \* *P**o**p**u**l**a**t**i**o**n*<sub>5 − *d**i**g**i**t*</sub>/*s**u**m**P**o**p**u**l**a**t**i**o**n*<sub>5 − *d**i**g**i**t*</sub>

And this is for each 3-digit ZIP code zone. First, eliminate all the
post office locations, where they are located P.O. boxes, and also,
exclude the 99 RUCA (which is the zones where “Census tract has zero
population and no rural-urban identifier information”).

    RUCA_ZipCodes_IN <- RUCA_ZipCodes_IN %>% 
      filter(ZIP_TYPE == "Zip Code Area" & 
               RUCA1 != 99)

For the weighted average, we must be careful about manipulating the
decimals. Because the secondary numbers are pre-defined, we can:/ /
**Review RUCA Code Definitions:**

-   RUCA code 2: Metropolitan area high commuting (primary flow 30% or
    more to a UA).

-   RUCA code 3: Metropolitan area low commuting (primary flow 10% to
    30% to a UA).

1.  **Rounding to Nearest Primary RUCA Code:**

    -   Since the RUCA classification system uses whole numbers and, in
        some cases, a single decimal place to indicate secondary
        commuting patterns, you need to round the weighted average to
        the nearest allowable RUCA code.

2.  **Apply Rounding Logic:**

    -   RUCA code 2.73 should be rounded to the nearest whole number or
        primary classification. In this case, the nearest whole number
        is 3.

Because we are more interested in the whole numbers, i.e., the primary
classification, we are going to round up the number obtained in the
weighted average.

Now, after creating the weighted average of the RUCA, we can create a
group depending of the RUCA obtained such as:

-   RUCA 10: Isolated

-   RUCA 7-9: Small Town

-   RUCA 4-6 Micropolitan

-   RUCA 1-3: Metropolitan

Or we can also use the other definition, where isolated, small town and
micropolitan are called “rural” and the rest, “urban”.

This process has to be made by each year, in order to accurately
represent the total population of each year. Is this necessary?

    RUCA_Aggregated_calculation <- function(year_select, RUCA_IN){
      
      #Extract the estimation by year for the 5-digit ZIP codes 
      total_pop_Ind<- get_acs(
          geography = "zcta",
          variables =  "B01001_001", #"C27007_007",
          state = "IN", 
          survey = "acs5",
          # geometry = TRUE,
          year = year_select
        )
      
      total_pop_Ind$FirstThreeDigits <- substr(total_pop_Ind$GEOID, 1, 3)
      
      #Aggregate by first 3-digits 
      population_counts <- total_pop_Ind %>%
          group_by(FirstThreeDigits) %>%
          summarize(total=sum(estimate))
      
      
      total_pop_Ind_df <- total_pop_Ind %>% 
        mutate(ZIP_CODE = GEOID) %>% 
        dplyr::select(-GEOID) 

      RUCA_ZipCodes_IN <- RUCA_ZipCodes_IN %>% 
        left_join(dplyr::select(total_pop_Ind_df, ZIP_CODE, estimate))
      
      #Calculate weighted average
      RUCA_ZipCodes_IN_wa<- RUCA_ZipCodes_IN %>% 
        filter(!is.na(estimate)) %>%
        group_by(FirstThreeDigits) %>%
        summarise(RUCA_3Digit = sum(as.numeric(RUCA1) * estimate)/sum(estimate)) %>%
        ungroup()

      RUCA_ZipCodes_IN_wa_shp <- population_counts %>%
        left_join(RUCA_ZipCodes_IN_wa, by = "FirstThreeDigits")
      
      #Round up and classify 
      RUCA_ZipCodes_IN_wa_shp <- RUCA_ZipCodes_IN_wa_shp %>%
        mutate(RUCA_3Digit_round = round(RUCA_3Digit))
      
        RUCA_ZipCodes_IN_year <- RUCA_ZipCodes_IN_wa_shp %>% 
          mutate(group = ifelse(RUCA_3Digit_round >= 4 & 
                                RUCA_3Digit_round <= 10, 
                                "Rural", "Urban"), 
                 year = year_select)
        
      return(RUCA_ZipCodes_IN_year)
    }

After generating the urban/rural classification, now we can join it with
the our study cohort.

    # census_api_key("d19c622a3976396f6cf5c446d10c0e71e0feefb8", overwrite = TRUE, install = TRUE)

    RUCA_ZipCodes_IN_wa_shp_2 <- dplyr::bind_rows(lapply(2015:2019, RUCA_Aggregated_calculation, RUCA_IN = RUCA_ZipCodes_IN))


    RUCA_Class_3DigitZIP_IN <- RUCA_ZipCodes_IN_wa_shp_2 %>% 
      dplyr::select(FirstThreeDigits, RUCA_3Digit_round, group, year) %>% 
      mutate(Recipient_Addr_Zip5 = as.numeric(FirstThreeDigits)) %>% 
      dplyr::select(-FirstThreeDigits)

    indiana_df_LastPrescription <- indiana_df_LastPrescription %>% 
      left_join(RUCA_Class_3DigitZIP_IN, by = c("Recipient_Addr_Zip5", "year"))

    # Save the new file.
    # 
    # path <- "C:/Users/paula/OneDrive - University of Texas at San Antonio/UTSA/HEAL Lab/[A] Opioid prescription in Indiana/CSV files/08-20-24_LastPrescriptionByYear_2015to2019_19to64_IndianaDatasetFinal.csv"
    # 
    # write.csv(indiana_df_LastPrescription, path, row.names = FALSE)

# Descriptive statistics

    # Save the new file.
    # path <- "C:/Users/paula/OneDrive - University of Texas at San Antonio/UTSA/HEAL Lab/[A] Opioid prescription in Indiana/CSV files/08-20-24_LastPrescriptionByYear_2015to2019_19to64_IndianaDatasetFinal.csv"
    # 
    # indiana_df_LastPrescription <- fread(path)

After that, create table with estimations.

    ######## Estimations by year ############

    vars <- c("Recipient_Gender", "race", "age", "group")
    strata <- "year"
    indiana_Df_estimations <- CreateTableOne(vars = vars, 
                                             strata = strata, 
                                             data = indiana_df_LastPrescription, 
                                             test = TRUE)

    print(indiana_Df_estimations, pDigits = 3, showAllLevels = TRUE, quote = TRUE, formatOptions = list(big.mark = ","))

## Pooled table with ACS estimates

To create a pooled table instead of one divide it by year. Also, create
one similar but using the ACS estimates.

    Pooled_indiana_df_LastPrescription <- indiana_df_LastPrescription %>% 
      dplyr::select(DE_Identified_Recipient_ID, race, age, Recipient_Gender, group, year) %>% 
      unique %>% 
      arrange(desc(year)) %>% 
      group_by(DE_Identified_Recipient_ID) %>% 
      slice(1) %>% 
      ungroup() 


    vars <- c("Recipient_Gender", "race", "age", "group")
    # strata <- "year"
    indiana_Df_estimations_2 <- CreateTableOne(vars = vars, 
                                             #strata = strata, 
                                             data = Pooled_indiana_df_LastPrescription, 
                                             test = TRUE)

    print(indiana_Df_estimations, pDigits = 3, showAllLevels = TRUE, quote = TRUE, formatOptions = list(big.mark = ","))

# Chi-square - bivariate analysis (old)

Chi-square by row.

    year_count <- c(34889, 46571, 17134, 20274, 13441)
    ExtractTable <- function(year, variable, table_df){
      estimations <- table_df[["CatTable"]][[as.character(year)]][[variable]][,5]
      return(estimations)
    }


    gender_estimations <- unlist(lapply(2015:2019, 
                                          ExtractTable,  
                                          variable = "Recipient_Gender", 
                                          table_df = indiana_Df_estimations))

    gender_estimations_mx_female <- matrix(gender_estimations,nrow=2,ncol=5)
    colnames(gender_estimations_mx_female) <- 2015:2019
    rownames(gender_estimations_mx_female) <- c("F", "NoF")

    gender_estimations_mx_female["NoF",] <- year_count - gender_estimations_mx_female["F",] 
                                   

    chi_square_gender <- chisq.test(gender_estimations_mx_female)

For ages.

    age_estimations <- unlist(lapply(2015:2019, 
                                          ExtractTable,  
                                          variable = "age", 
                                          table_df = indiana_Df_estimations))

    age_estimations_mx <- matrix(age_estimations,nrow=5,ncol=5)
    colnames(age_estimations_mx) <- 2015:2019
    rownames(age_estimations_mx) <- c("18 to 25", 
                                        "26 to 34",
                                        "35 to 44", 
                                        "45 to 54", 
                                        "55 to 64")
    #18 to 25
    age_estimations_mx_18to25 <- age_estimations_mx[c("18 to 25", "26 to 34"), ]
    rownames(age_estimations_mx_18to25) <- c("18 to 25", "Other")

    age_estimations_mx_18to25["Other",] <- year_count - age_estimations_mx_18to25["18 to 25",] 
                                   
    chi_square_18to25 <- chisq.test(age_estimations_mx_18to25)

    #26 to 34
    age_estimations_mx_26to34 <- age_estimations_mx[c("26 to 34", "35 to 44"), ]
    rownames(age_estimations_mx_26to34) <- c("26 to 34", "Other")

    age_estimations_mx_26to34["Other",] <- year_count - age_estimations_mx_26to34["26 to 34",] 
                                   
    chi_square_26to34<-chisq.test(age_estimations_mx_26to34)

    #35 to 44
    age_estimations_mx_35to44 <- age_estimations_mx[c("35 to 44", "45 to 54"), ]
    rownames(age_estimations_mx_35to44) <- c("35 to 44", "Other")

    age_estimations_mx_35to44["Other",] <- year_count - age_estimations_mx_35to44["35 to 44",] 
                                   
    chi_square_35to44<-chisq.test(age_estimations_mx_35to44)

    #45 to 54
    age_estimations_mx_45to54 <- age_estimations_mx[c("45 to 54", "55 to 64"), ]
    rownames(age_estimations_mx_45to54) <- c("45 to 54", "Other")

    age_estimations_mx_45to54["Other",] <- year_count - age_estimations_mx_45to54["45 to 54",] 
                                   
    chi_square_45to54<-chisq.test(age_estimations_mx_45to54)

    #55 to 64
    age_estimations_mx_55to64 <- age_estimations_mx[c("55 to 64", "45 to 54"), ]
    rownames(age_estimations_mx_55to64) <- c("55 to 64", "Other")

    age_estimations_mx_55to64["Other",] <- year_count - age_estimations_mx_55to64["55 to 64",] 
                                   
    chi_square_55to64<-chisq.test(age_estimations_mx_55to64)

Race.

    race_estimations <- unlist(lapply(2015:2019, 
                                          ExtractTable,  
                                          variable = "race", 
                                          table_df = indiana_Df_estimations))

    race_estimations_mx <- matrix(race_estimations,nrow=4,ncol=5)
    colnames(race_estimations_mx) <- 2015:2019
    rownames(race_estimations_mx) <- c("Black", 
                                         "Hispanic",
                                         "Other", 
                                         "White")

    #White 
    race_estimations_mx_White <- race_estimations_mx[c("White", "Black"), ]
    rownames(race_estimations_mx_White) <- c("White", "Other")

    race_estimations_mx_White["Other",] <- year_count - race_estimations_mx_White["White",] 
                                   
    chi_square_White<-chisq.test(race_estimations_mx_White)

    #Black 
    race_estimations_mx_black <- race_estimations_mx[c("Black", "White"), ]
    rownames(race_estimations_mx_black) <- c("Black", "Other")

    race_estimations_mx_black["Other",] <- year_count - race_estimations_mx_black["Black",] 
                                   
    chi_square_black<-chisq.test(race_estimations_mx_black)

    #Hispanic 
    race_estimations_mx_hispanic <- race_estimations_mx[c("Hispanic", "White"), ]
    rownames(race_estimations_mx_hispanic) <- c("Hispanic", "Other")

    race_estimations_mx_hispanic["Other",] <- year_count - race_estimations_mx_hispanic["Hispanic",] 
                                   
    chi_square_hispanic<-chisq.test(race_estimations_mx_hispanic)

    #Other 
    race_estimations_mx_other <- race_estimations_mx[c("Other", "White"), ]
    rownames(race_estimations_mx_other) <- c("OtherRaces", "Other")

    race_estimations_mx_other["Other",] <- year_count - race_estimations_mx_other["OtherRaces",] 
                                   
    chi_square_other<-chisq.test(race_estimations_mx_other)

# Line chart for the proportion of urban and rural

    urbanVsRural <- unlist(lapply(as.character(2015:2019), function(x) indiana_Df_estimations[["CatTable"]][[x]][["group"]][["percent"]]))

    urbanVsRural <- as.data.frame(matrix(urbanVsRural, nrow = 2, ncol = 5)) %>% 
      mutate(group = c("Rural", "Urban"))

    colnames(urbanVsRural) <- c(as.character(2015:2019), "group")

    urbanVsRural <- urbanVsRural %>% 
      pivot_longer(
        cols = c(as.character(2015:2019)), 
        names_to = "year", 
        values_to = "Byyear"
      )

    urbanVsRural %>% 
      ggplot(aes(x = year, y = Byyear, group = group, color = group)) + 
      geom_line(color = "grey", lwd = 2) + 
      geom_point(aes(fill = group), shape = 21, color = "black", size = 5) +
      scale_fill_manual(values = c("Rural" = "#8babf1", "Urban" = "#029356")) + 
      theme_bw(base_size = 22) + 
      theme(
       panel.grid.major = element_blank(),
       # explicitly set the horizontal lines (or they will disappear too)
       panel.grid.minor = element_blank(),
       legend.position = "top"
      ) +
      labs(title = "",
           x = "Year", 
           y = "Proportion of patients (%)", 
           size = "", 
           fill = "") 

    # Scale Rural values to align with a secondary y-axis
    scale_factor <- max(urbanVsRural$Byyear[urbanVsRural$group == "Urban"]) / 
                    max(urbanVsRural$Byyear[urbanVsRural$group == "Rural"])

    urbanVsRural <- urbanVsRural %>% 
      mutate(scaled_Byyear = ifelse(group == "Rural", Byyear * scale_factor, Byyear))

    # Create the dual y-axis plot
    ggplot(urbanVsRural, aes(x = as.numeric(year))) + 
      geom_line(aes(y = scaled_Byyear, color = group, group = group), size = 1.2) +
      geom_point(aes(y = scaled_Byyear, fill = group), shape = 21, color = "black", size = 5) +
      
      # Manually define colors
      scale_color_manual(values = c("Rural" = "#8babf1", "Urban" = "#029356")) +  
      scale_fill_manual(values = c("Rural" = "#8babf1", "Urban" = "#029356")) +  

      # Define y-axis and secondary y-axis  
      scale_y_continuous(
        name = "Proportion of urban patients (%)",
        sec.axis = sec_axis(~ . / scale_factor, name = "Proportion of rural patients (%)")  
      ) +

      # Improve Theme  
      theme_bw(base_size = 22) + 
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "top",
        axis.title.y = element_text(color = "#029356"),  # Urban axis color
        axis.title.y.right = element_text(color = "#8babf1")  # Rural axis color
      ) +
      
      labs(title = "",
           x = "Year",
           fill = "", 
           color = "")

    # Line chart by month
    # Counting by month

    ByMonth_UrbanRural <- indiana_df_filter %>% 
      mutate(month_prescription = substr(AllDate, 1, 7)) %>% 
      arrange(desc(month_prescription), DE_Identified_Recipient_ID) %>% 
      group_by(DE_Identified_Recipient_ID, year) %>% 
      slice(1) %>% 
      ungroup() %>%
      left_join(RUCA_Class_3DigitZIP_IN, by = c("Recipient_Addr_Zip5", "year")) %>%
      group_by(month_prescription, group) %>% 
      count %>% 
      ungroup() %>% 
      group_by(month_prescription) %>% 
      mutate(
        total_month = sum(n)
      ) %>% 
      ungroup() %>% 
      mutate(
        prop_byMonth = n/total_month * 100,
      ) %>% 
      group_by(group) %>% 
      mutate( 
        month_prescription = 1:60
      ) %>% 
      ungroup()


    ByMonth_UrbanRural %>% 
      ggplot(aes(x = month_prescription, y = prop_byMonth, group = group, color = group)) + 
      geom_line(color = "grey", lwd = 2) + 
      geom_point(aes(fill = group), shape = 21, color = "black", size = 3) +
      scale_fill_manual(values = c("Rural" = "#8babf1", "Urban" = "#029356")) + 
      scale_x_continuous(breaks=c(0,15, 30, 45, 60), 
                         labels = 2015:2019) +
      theme_bw(base_size = 22) + 
      theme(
       panel.grid.major = element_blank(),
       # explicitly set the horizontal lines (or they will disappear too)
       panel.grid.minor = element_blank(),
       legend.position = "top"
      ) +
      labs(title = "",
           x = "Year", 
           y = "Proportion of patients (%)", 
           size = "", 
           fill = "") 

### Post hoc chi-squared analysis

According with the Chi-square test, all the demographics are
significantly different across years. To find the most significant
group, we are going to do a post-hoc test.

    ######## Extractions of frequencies for post-hoc by gender ############

    ExtractTable <- function(year, variable, table_df){
      estimations <- table_df[["CatTable"]][[as.character(year)]][[variable]][,5]
      return(estimations)
    }


    gender_Estimatations <- unlist(lapply(2015:2019, 
                                          ExtractTable,  
                                          variable = "Recipient_Gender", 
                                          table_df = indiana_Df_estimations))

    gender_Estimatations_mx <- matrix(gender_Estimatations,nrow=2,ncol=5)
    colnames(gender_Estimatations_mx) <- 2015:2019
    rownames(gender_Estimatations_mx) <- c("F", "M")
                                   
    gender_posthoc_year <- chisq.posthoc.test(gender_Estimatations_mx, method = "bonferroni")

    #Possible idea to show the post-hoc results.
    # Transform the data
    gender_posthoc_year_long <- gender_posthoc_year %>%
      pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Measurement") %>%
      pivot_wider(names_from = Value, values_from = Measurement) %>%
      mutate(`p values` = as.numeric(`p values`),
             Residuals = as.numeric(Residuals))

    # Define significance threshold
    significance_threshold <- 0.05

    # Plot the data
    ggplot(gender_posthoc_year_long, aes(x = Year, y = Residuals, fill = Dimension)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      #geom_text(aes(label = ifelse(`p values` < significance_threshold, "*", "")),
      #          position = position_dodge(width = 0.9), vjust = -0.8) +
      labs(title = "Residuals by Year and Gender with Significance",
           x = "Year", y = "Residuals") +
      scale_fill_manual(values = c("#42D9C8", "#2C8C99")) +
      theme_classic()

    ######## Extractions of frequencies for post-hoc by age ############


    age_Estimatations <- unlist(lapply(2015:2019, 
                                          ExtractTable,  
                                          variable = "age", 
                                          table_df = indiana_Df_estimations))

    age_Estimatations_mx <- matrix(age_Estimatations,nrow=5,ncol=5)
    colnames(age_Estimatations_mx) <- 2015:2019
    rownames(age_Estimatations_mx) <- c("18 to 25", 
                                        "26 to 34",
                                        "35 to 44", 
                                        "45 to 54", 
                                        "55 to 64")
                                   
    # chisq.posthoc.test(age_Estimatations_mx, method = "bonferroni")
    age_posthoc_df <- chisq.posthoc.test(age_Estimatations_mx, method = "bonferroni")

    # Transform the data
    age_posthoc_df_long <- age_posthoc_df %>%
      pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Measurement") %>%
      pivot_wider(names_from = Value, values_from = Measurement) %>%
      mutate(`p values` = as.numeric(`p values`),
             Residuals = as.numeric(Residuals))

    # Define significance threshold
    significance_threshold <- 0.05

    # Plot the data
    ggplot(age_posthoc_df_long, aes(x = Year, y = Residuals, fill = Dimension)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      geom_text(aes(label = ifelse(`p values` < significance_threshold, "*", "")),
                position = position_dodge(width = 0.9), vjust = -0.8) +
      labs(title = "Residuals by Year and Gender with Significance",
           x = "Year", y = "Residuals") +
      scale_fill_manual(values = c("#42D9C8", "#2C8C99", "#326771", "#28464B", "#677DB7")) +
      theme_classic()

    ######## Extractions of frequencies for post-hoc by race ############


    race_Estimatations <- unlist(lapply(2015:2019, 
                                          ExtractTable,  
                                          variable = "race", 
                                          table_df = indiana_Df_estimations))

    race_Estimations_mx <- matrix(race_Estimatations,nrow=4,ncol=5)
    colnames(race_Estimations_mx) <- 2015:2019
    rownames(race_Estimations_mx) <- c("Black", 
                                         "Hispanic",
                                         "Other", 
                                         "White")
                                   
    race_posthoc_df <- chisq.posthoc.test(race_Estimations_mx, method = "bonferroni")

    ######## Extractions of frequencies for post-hoc by urban/rural ############


    urbanRural_Estimatations <- unlist(lapply(2015:2019, 
                                          ExtractTable,  
                                          variable = "group", 
                                          table_df = indiana_Df_estimations))

    urbanRural_Estimatations_mx <- matrix(urbanRural_Estimatations,nrow=2,ncol=5)
    colnames(urbanRural_Estimatations_mx) <- 2015:2019
    rownames(urbanRural_Estimatations_mx) <- c("Rural", 
                                         "Urban")
                                   
    ruralUrban_posthoc_df <- chisq.posthoc.test(urbanRural_Estimatations_mx, method = "bonferroni")

Overall statistics.

    gender_overall <- indiana_df_filter %>% 
      dplyr::select(DE_Identified_Recipient_ID, Recipient_Gender) %>% 
      unique 

    sum(gender_overall$Recipient_Gender == "F")
    sum(gender_overall$Recipient_Gender == "M")


    sum(gender_overall$Recipient_Gender == "F")/(nrow(gender_overall))
    sum(gender_overall$Recipient_Gender == "M")/(nrow(gender_overall))

    race_overall <- indiana_df_filter %>% 
      dplyr::select(DE_Identified_Recipient_ID, year, race) %>% 
      arrange(desc(year)) %>% 
      group_by(DE_Identified_Recipient_ID) %>% 
      slice(1)

    sum(race_overall$race == "White")
    sum(race_overall$race == "Black")
    sum(race_overall$race == "Hispanic")
    sum(race_overall$race == "Other")

    sum(race_overall$race == "White")/nrow(race_overall)

    sum(race_overall$race == "Black")/nrow(race_overall)
      

    sum(race_overall$race == "Hispanic")/nrow(race_overall)


    sum(race_overall$race == "Other")/nrow(race_overall)

# Multivariate analysis: log-linear model

    # path <- "C:/Users/paula/OneDrive - University of Texas at San Antonio/UTSA/HEAL Lab/[A] Opioid prescription in Indiana/CSV files/08-20-24_LastPrescriptionByYear_2015to2019_19to64_IndianaDatasetFinal.csv"
    # 
    # indiana_df_LastPrescription <- fread(path)

Log-linear models go beyond single summary statistics and specify how
the cell counts depend on the levels of categorical variables. They
model the association and interaction patterns among categorical
variables. Because categorical values (Age, race, sex) can interact
between them, it is better to run a multivariate instead of bivariate.
Some aspects to keep in mind is first, the variable to evaluate must be
positive. Second, to select the best fitted model, it is important to
choose the model with a residual deviance closer to the degrees of
freedom.

-   Good fit: Residual Deviance ≈ Degrees of Freedom
-   Overfitting: Residual Deviance much smaller than df (model is too
    complex)
-   Underfitting: Residual Deviance much larger than df (model does not
    explain enough variation → needs more terms)

A way to evaluate it is to calculate the dispersion ratio.

-   If dispersion\_ratio &gt; 1.5, there may be overdispersion
    (suggesting the need for a quasi-Poisson or negative binomial
    model).
-   If dispersion\_ratio &lt; 1, the model is too complex (consider
    simplifying it).

The goal is to balance model complexity and interpretability.

    groups_count_patients <- indiana_df_LastPrescription %>% 
      group_by(year, age, race, Recipient_Gender, group) %>% 
      count() %>% 
      ungroup() %>% 
      mutate(
        patients_count = n
      ) %>% 
      dplyr::select(-n)

### Fit a full model (most complex)

A full log-linear model includes all main effects and all interactions:

    # Fit log-linear model using Poisson regression
    full_model <- glm(patients_count ~ year * age * race * Recipient_Gender * group, 
                        family = poisson(link = "log"), 
                        data = groups_count_patients)

    # Model summary
    summary(full_model)

Check for overdispersion (where variance is much larger than the mean)

    # Check residual deviance and degrees of freedom
    deviance(full_model)  # Residual deviance
    df.residual(full_model)  # Degrees of freedom

    # Compute rule of thumb
    dispersion_ratio <- deviance(full_model) / df.residual(full_model)
    dispersion_ratio  # Should be close to 1

-   The dispersion rate is too large.
-   Deviance is much larger than residuals.
-   May overfit.

### Fit a simpler model

A simplified model should retain significant interactions but remove
unnecessary complexity:

    simpler_model <- glm(patients_count ~ 
                           year + age + race + Recipient_Gender +  # Main effects
                           year:age + year:race + age:race +       # Two-way interactions
                           year:age:race,                          # Important three-way interaction
                         family = poisson(link = "log"), 
                         data = groups_count_patients)

    summary(simpler_model)

Compare the models using ANOVA.

    anova(full_model, simpler_model, test = "Chisq")

If p &gt; 0.05, the simpler model is sufficient, which in this case is
not.

Compare AIC Values

-   AIC (Akaike Information Criterion) helps compare models:
-   Lower AIC is better (simpler model that fits well).

<!-- -->

    AIC(full_model, simpler_model)

# Fit negative binomial model

The negative binomial (NB) model extends the Poisson model by allowing
for overdispersion (i.e., when the variance exceeds the mean). While the
negative binomial model is often introduced in probability theory as
describing the number of failures before a certain number of successes,
in regression modeling, it is commonly used to model count data with
overdispersion, including relationships between multiple predictor
variables

Just like Poisson regression, the negative binomial model is a type of
Generalized Linear Model (GLM) that relates a count outcome (e.g., the
number of patients using opioids) to multiple predictor variables (e.g.,
year, race, age, gender).

First, it is important to change the “year” vairable as “cateogircal” to
estaimte separate effects for each year, best when there are changes por
external factors that may cause year-to-year variations.

    groups_count_patients$year <- as.numeric(groups_count_patients$year)

Then, we change the refence of the categories:

    groups_count_patients$age <- factor(groups_count_patients$age, 
                                        levels = c("18 to 25", 
                                                   "26 to 34", 
                                                   "35 to 44", 
                                                   "45 to 54", 
                                                   "55 to 64"))
    groups_count_patients$race <- factor(groups_count_patients$race, 
                                         levels = c("White",
                                                    "Black", 
                                                    "Hispanic", 
                                                    "Other"
                                                    ))
    groups_count_patients$Recipient_Gender <- factor(groups_count_patients$Recipient_Gender, levels = c("F", "M"))

    groups_count_patients$group <- factor(groups_count_patients$group, levels = c("Urban", "Rural"))

Run the first model.

    nb_model <- glm.nb(patients_count ~ year * age * race * Recipient_Gender * group, 
                       data = groups_count_patients)

    summary(nb_model)

Instead of manually removing terms, you can use step() for automated
backward selection:

    best_nb_model <- step(nb_model, direction = "backward", test = "Chisq")
    summary(best_nb_model)

Final model:

    best_nb_model <- glm.nb(patients_count ~ year + age + race + Recipient_Gender + 
        group + year:age + year:race + age:race + year:Recipient_Gender + 
        age:Recipient_Gender + race:Recipient_Gender + year:group + 
        race:group + Recipient_Gender:group + year:Recipient_Gender:group, 
        data = groups_count_patients, init.theta = 12.15911018, 
        link = log)
    summary(best_nb_model)

A rate ratio is a statistical measurement that compares the rate of an
event in one group to the rate of the same event in another group. It
this case, is calculated by calculating the exponential of the log count
(estimate).

    # Extract model coefficients
    results <- tidy(best_nb_model, exponentiate = TRUE, conf.int = TRUE)

    # Rename columns for clarity
    results <- results %>%
      rename(Variable = term, 
             Rate_Ratio = estimate, 
             CI_Lower = conf.low, 
             CI_Upper = conf.high, 
             p_value = p.value)

    # Round values for readability
    results <- results %>%
      mutate(across(where(is.numeric), ~ round(.x, 3)))

    # Display as a formatted table
    results %>%
      gt() %>%
      tab_header(title = "Negative Binomial Model Results: Opioid Prescriptions") %>%
      fmt_scientific(columns = vars(p_value), decimals = 3) %>%
      fmt_number(columns = vars(Rate_Ratio, CI_Lower, CI_Upper), decimals = 2)


    results$Variable <- gsub(":", " × ", results$Variable)  # Change "age:race" to "age × race"
    results$Variable <- gsub("age", "Age ", results$Variable)
    results$Variable <- gsub("race", "Race ", results$Variable)
    results$Variable <- gsub("Recipient_GenderM", "Male", results$Variable)


    gtsave(results %>% gt(), "nb_model_results_NewRef_RuralUrban.docx")

### How to interpret the results?

We have the following interpretation as an example.

For main effects, we have (from the best model after stepwise
regression):

-   **For year:**

    -   Estimate = -0.32045, p-value &lt; 2e-16

        -   Opioid prescription declined significantly overtime, with a
            rate ratio of 0.73 (*e*<sup>−0.32045</sup> = 0.73)

        -   Each year. opioid prescriptions decreasde by ~27% on
            average.

-   Age group effects:

    -   **Age 26-34 (0.50405, p &lt; 0.001)**
    -   **Age 35-44 (0.41619, p = 0.0036)**
    -   **Age 45-54 (0.39705, p = 0.0055)**
    -   **Age 55-64 (0.32026, p = 0.0254)**
        -   Compared to the 18-25 reference group, older age groups had
            significantly higher opioid prescriptions.

-   **Race Other (-635.25, p &lt; 2e-16)** (**Highly significant**)

    -   **Interpretation:** Patients classified as **Other races had
        significantly lower opioid prescription rates** than Black
        patients.

    -   **Rate Ratio:** `exp(-635.25) ≈ ~0` (**Almost no prescriptions**
        for this group).

-   **Recipient\_GenderM (-90.68, p = 0.131)**

    -   **Not statistically significant**, meaning **no strong evidence
        of gender differences** in opioid prescriptions.

For interaction effects:

-   **Year x Race Other (0.31361, p &lt; 2e-16)**

    -   Over time, opioid prescription rates for other race group
        increased by 37% per year compared to black patients.

-   **Age 55-64 x Race hispanic (-0.38427, p = 0.039)**

    -   Hispanic patients aged 55-64 received significanyly fewer opioid
        prescriptions than black patients in the same age group.

-   **Age 35-44 × Race White (0.3643, p = 0.041)**

    -   **Interpretation:** White patients aged **35-44 received
        significantly more opioid prescriptions** than Black patients in
        the same age group.

-   **Age 45-54 × Gender Male (0.5265, p &lt; 0.001)**

    -   **Interpretation:** **Men aged 45-54 had significantly higher
        opioid prescriptions than women in this age group**.

-   **Race Other × Gender Male (0.36698, p = 0.0031)**

    -   **Interpretation:** Among patients classified as **Other
        races**, men were **more likely to receive opioid
        prescriptions** than women.

# ACS estimates: rural vs urban

Extract the total Medicaid population by year.

    # Define years to collect data from
    years <- 2015:2019

    # Run the SelectCensus function for each year and combine results
    combined_data_ACS<- map_dfr(years, function(y) {
      result <- SelectCensus(y)  # Run function for the year
      result$year <- y  # Add a year column
      return(result)
    })

    colnames(combined_data_ACS)[2]<- "PAT_ZIP"

Using RUCA codes.

    path <- "C:/Users/paula/OneDrive - University of Texas at San Antonio/UTSA/HEAL Lab/[A] Contraceptive disparities/Files/RUCA2010zipcode.xlsx"
    RUCA_ZipCodes <- readxl::read_excel(path, 
                                        col_types = "text", 
                                        sheet = "Data")
    colnames(RUCA_ZipCodes)[1]<- "PAT_ZIP"

Classified them according with RUCA codes.

    combined_data_ACS_class <- combined_data_ACS %>%
      left_join(dplyr::select(RUCA_ZipCodes, PAT_ZIP, RUCA1),by="PAT_ZIP") %>% 
      mutate(
          group = case_when(
          RUCA1 %in% as.character(1:3) ~"Urban",
          RUCA1 %in% as.character(4:10) ~"Rural",
          TRUE ~ "Unknown"
        )
      )

Calculate the proportion by urban/rural classification

    UrbanRural_num <- combined_data_ACS_class %>% 
      group_by(group, year) %>% 
      summarise(total_urbanRural = sum(estimate)) %>% 
      ungroup() 

    UrbanRural_num <- st_drop_geometry(UrbanRural_num)

    TotalPopulation_year <- combined_data_ACS_class %>% 
      group_by(year) %>% 
      summarise(total_patients = sum(estimate)) %>% 
      ungroup()

    TotalPopulation_year <- st_drop_geometry(TotalPopulation_year)

    #Join dataframes

    UrbanRural_prop <- UrbanRural_num %>% 
      left_join(TotalPopulation_year, by = "year") %>% 
      mutate(
        Byyear = total_urbanRural/total_patients * 100, 
        group = case_when(
          group == "Rural" ~ "Rural (Medicaid population - ACS)", 
          group == "Urban" ~ "Urban (Medicaid population - ACS)"
        ),
        year = as.character(year)
      ) %>% 
      dplyr::select(group, year, Byyear)

### Linear chart with ACS estimates

Linechart including ACS estimates

    urbanVsRural <- dplyr::bind_rows(UrbanRural_prop, urbanVsRural)

    # Scale Rural values to align with a secondary y-axis
    scale_factor <- max(urbanVsRural$Byyear[urbanVsRural$group == "Urban"]) / 
                    max(urbanVsRural$Byyear[urbanVsRural$group == "Rural (Medicaid population - ACS)"])

    urbanVsRural <- urbanVsRural %>% 
      mutate(scaled_Byyear = ifelse(group == "Rural (Medicaid population - ACS)", Byyear * scale_factor, Byyear))

    # Create the dual y-axis plot
    ggplot(urbanVsRural, aes(x = as.numeric(year))) + 
      geom_line(aes(y = scaled_Byyear, color = group, group = group), size = 1.2) +
      geom_point(aes(y = scaled_Byyear, fill = group), shape = 21, color = "black", size = 5) +
      
      # Manually define colors
      scale_color_manual(values = c("Rural" = "#8babf1", "Urban" = "#029356",   
    "Rural (Medicaid population - ACS)" = "#054fb9", "Urban (Medicaid population - ACS)"="#005e1d")) +  
      scale_fill_manual(values = c("Rural" = "#8babf1", "Urban" = "#029356",    
    "Rural (Medicaid population - ACS)" = "#054fb9", "Urban (Medicaid population - ACS)"="#005e1d")) +  

      # Define y-axis and secondary y-axis  
      scale_y_continuous(
        name = "Proportion of urban patients (%)",
        sec.axis = sec_axis(~ . / scale_factor, name = "Proportion of rural patients (%)")  
      ) +

      # Improve Theme  
      theme_bw(base_size = 22) + 
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "top",
        axis.title.y = element_text(color = "black"),  # Urban axis color
        axis.title.y.right = element_text(color = "black")  # Rural axis color
      ) +
      
      labs(title = "",
           x = "Year",
           fill = "", 
           color = "")

### Evaluate significant differences

Here we ran a Wilcoxon rank test to evaluate if there was significant
differences between rural/urban population in our cohort and in the
total Indiana Medicaid enrollees from ACS.

    # Extract Rural and Urban Medicaid data
    rural_medicaid <- urbanVsRural %>% filter(group == "Rural")
    urban_medicaid <- urbanVsRural %>% filter(group == "Urban")

    # Extract ACS Data
    rural_acs <- urbanVsRural %>% filter(group == "Rural (Medicaid population - ACS)")
    urban_acs <- urbanVsRural %>% filter(group == "Urban (Medicaid population - ACS)")

    results_rural_acs <- rural_medicaid %>%
      inner_join(rural_acs, by = "year", suffix = c("_medicaid", "_acs")) %>%
      rowwise() %>%
      mutate(
        #t_test_p = t.test(Byyear_medicaid, Byyear_acs, paired = TRUE)$p.value,
        wilcox_p = wilcox.test(Byyear_medicaid, Byyear_acs, paired = TRUE)$p.value
      )

    print(results_rural_acs)

    results_urban_acs <- urban_medicaid %>%
      inner_join(urban_acs, by = "year", suffix = c("_medicaid", "_acs")) %>%
      rowwise() %>%
      mutate(
        # t_test_p = t.test(Byyear_medicaid, Byyear_acs, paired = TRUE)$p.value,
        wilcox_p = wilcox.test(Byyear_medicaid, Byyear_acs, paired = TRUE)$p.value
      )

    print(results_urban_acs)

# Supplementary table: 3-digit ZIP code, 5 -digit ZIP code contained and county.

To determine the 5-digit ZIP codes aggregated by 3-digit ZIP code, and
also the county related to that 3-digit ZIP code, we are going to use
the relationship files from ZCTA to county, using also the land area
info. First, identify all the 5-digit ZIP codes aggregated by 3-digit
ZIP codes.

    ZipCode_info <- get_acs(
          geography = "zcta",
          variables =  "B01001_001", #"C27007_007",
          state = "IN", 
          survey = "acs5",
          geometry = FALSE,
          year = 2019
        )
      
    ZipCode_info$FirstThreeDigits <- substr(ZipCode_info$GEOID, 1, 3) 

    ZipCode_info_2 <- ZipCode_info %>% 
      dplyr::select(FirstThreeDigits) %>% 
      unique 

    ZipCode_info_2$ZIP_code_group <- unlist(lapply(unlist(unique(ZipCode_info$FirstThreeDigits)), function(x){
      
      allZipCodes <- ZipCode_info$GEOID[ZipCode_info$FirstThreeDigits == x]
      
      paste0(sort(allZipCodes), collapse = ",")
        
    }))

Now, we are going to load the relationship file.

    ZCTA2County <- read.delim("C:/Users/paula/OneDrive - University of Texas at San Antonio/UTSA/HEAL Lab/[A] Opioid prescription in Indiana/CSV files/tab20_zcta520_county20_natl.txt", 
                              sep = "|", 
                              colClasses = c("GEOID_ZCTA5_20" = "character"))

Extract the ZIP codes from Indiana.

    IN_ZCTA2County <- ZCTA2County %>% 
      filter(GEOID_ZCTA5_20 %in% unique(ZipCode_info$GEOID))

    IN_ZCTA2County$FirstThreeDigits <- substr(IN_ZCTA2County$GEOID_ZCTA5_20, 1, 3) 

To know how many counties are repeated in multiple 3-digit ZIP codes.

    Counties_IN_ZIP3 <- IN_ZCTA2County %>% 
      dplyr::select(FirstThreeDigits, NAMELSAD_COUNTY_20) %>% 
      unique %>% 
      group_by(NAMELSAD_COUNTY_20) %>% 
      count %>% 
      ungroup()

To classify the county in a 3-digit ZIP code it is necessary to
aggregate the land area of all the 5-digit ZIP codes by 3-digit ZIP
code, and the one that has the highest land area, is the assigned
3-digit ZIP code to that county.

    #Create the dataframe for the counties who just have one 3-digit ZIP code 
    ZIP32County_One <- Counties_IN_ZIP3 %>% 
      filter( n == 1) %>% 
      dplyr::select(NAMELSAD_COUNTY_20) %>% 
      unlist 

    ZIP32County_One_df <- IN_ZCTA2County %>% 
      filter(NAMELSAD_COUNTY_20 %in% ZIP32County_One) %>% 
      dplyr::select(NAMELSAD_COUNTY_20, FirstThreeDigits) %>% 
      unique 

    #Identify the counties which have more than 1 3-digit ZIP code assigned. 
    Counties_IN_ZIP3_MoreThan1 <- Counties_IN_ZIP3 %>% 
      filter(n > 1)

    CountiMaxLandArea_ZIP3 <- dplyr::bind_rows(lapply(unique(Counties_IN_ZIP3_MoreThan1$NAMELSAD_COUNTY_20), function(x){
      IN_ZCTA2County_eachCounty <- IN_ZCTA2County %>% 
        filter(NAMELSAD_COUNTY_20 == x) 
      
      County_TotalArea <- IN_ZCTA2County_eachCounty %>% 
        dplyr::select(AREALAND_COUNTY_20) %>% 
        unique %>% 
        unlist 
      
      IN_ZCTA2County_eachCounty <- IN_ZCTA2County_eachCounty %>% 
        group_by(FirstThreeDigits) %>% 
        summarise(land_area_aggregated = sum(AREALAND_PART)) %>% 
        ungroup() %>% 
        mutate(
          land_area_aggregated_prop = land_area_aggregated/County_TotalArea
        ) %>% 
        filter(land_area_aggregated_prop == max(land_area_aggregated_prop)) 
      

      CountAndZip3 <- IN_ZCTA2County %>% 
        dplyr::select(FirstThreeDigits, NAMELSAD_COUNTY_20) %>% 
        unique %>% 
        filter(NAMELSAD_COUNTY_20 == x & 
                 FirstThreeDigits == IN_ZCTA2County_eachCounty$FirstThreeDigits)
      
      return(CountAndZip3)

    }))

There are some 3-digit ZIP codes which does not have an assigned county,
because the only county they have is sharing bigger land area to their
neighboring 3-digit ZIP codes. Those cases are 464, 466, and 468. For
those 3-digit ZIP codes that only have one county assigned, just leave
it and not assigned it to other 3-digit ZIP codes. To do that, identify
the 3-digit ZIP code with just one or two county assigned, and keep the
one with the highest proportion.

    #Join the two dataframes
    ZIP3_to_Counties <- dplyr::bind_rows(ZIP32County_One_df, CountiMaxLandArea_ZIP3)


    NoZIP3_county <- ZipCode_info_2 %>% 
      filter(!FirstThreeDigits %in% ZIP3_to_Counties$FirstThreeDigits) %>% 
      dplyr::select(FirstThreeDigits) %>% 
      unique %>% 
      unlist 
      
      
    Proporiton_ByCounty <- dplyr::bind_rows(lapply(unique(Counties_IN_ZIP3_MoreThan1$NAMELSAD_COUNTY_20), function(x){
      IN_ZCTA2County_eachCounty <- IN_ZCTA2County %>% 
        filter(NAMELSAD_COUNTY_20 == x) 
      
      County_TotalArea <- IN_ZCTA2County_eachCounty %>% 
        dplyr::select(AREALAND_COUNTY_20) %>% 
        unique %>% 
        unlist 
      
      IN_ZCTA2County_eachCounty <- IN_ZCTA2County_eachCounty %>% 
        group_by(FirstThreeDigits) %>% 
        mutate(land_area_aggregated = sum(AREALAND_PART)) %>% 
        ungroup() %>% 
        mutate(
          land_area_aggregated_prop = land_area_aggregated/County_TotalArea
        ) %>% 
        dplyr::select(FirstThreeDigits, NAMELSAD_COUNTY_20, land_area_aggregated_prop) %>% 
        unique
      
      return(IN_ZCTA2County_eachCounty)

    })) 


    #Keep the highest proportion 
    ZIP3_highProp <- Proporiton_ByCounty %>% 
      filter(FirstThreeDigits %in% NoZIP3_county) %>% 
      arrange(FirstThreeDigits, desc(land_area_aggregated_prop)) %>% 
      group_by(FirstThreeDigits) %>% 
      slice(1) %>% 
      ungroup() %>% 
      dplyr::select(NAMELSAD_COUNTY_20, FirstThreeDigits)

    #Eliminate the counties for the other ZIP code
    ZIP3_to_Counties <- ZIP3_to_Counties %>% 
      filter(!NAMELSAD_COUNTY_20 %in% ZIP3_highProp$NAMELSAD_COUNTY_20) %>% 
      dplyr::bind_rows(ZIP3_highProp)

    ZIP3_to_Counties_df <- ZIP3_to_Counties %>% 
      dplyr::select(FirstThreeDigits) %>% 
      unique 

    #Join the counties in just one column 
    ZIP3_to_Counties_df$countyGroup <- unlist(lapply(unlist(unique(ZIP3_to_Counties$FirstThreeDigits)), function(x){
      
      allCounties <- ZIP3_to_Counties$NAMELSAD_COUNTY_20[ZIP3_to_Counties$FirstThreeDigits == x]
      
      paste0(sort(allCounties), collapse = ",")
      
    }))

Now, join with the other dataframe that contains the 5-digit ZIP codes.

    Suppl_Table_ZIP3_5_county <- ZipCode_info_2 %>% 
      left_join(ZIP3_to_Counties_df, by = "FirstThreeDigits")

Finally, join the info of the rural/urban classification from 2020.

    Suppl_Table_ZIP3_5_county_RUCA <- RUCA_Class_3DigitZIP_IN %>% 
      filter(year == 2019) %>% 
      dplyr::select(Recipient_Addr_Zip5, group) %>% 
      unique %>% 
      sf::st_drop_geometry() %>% 
      mutate(FirstThreeDigits = as.character(Recipient_Addr_Zip5)) %>% 
      dplyr::select(-Recipient_Addr_Zip5) %>% 
      left_join(Suppl_Table_ZIP3_5_county, by = "FirstThreeDigits") %>% 
      dplyr::select(-ZIP_code_group)

    #Save as csv to edit. 

    # write.table(Suppl_Table_ZIP3_5_county_RUCA, "C:/Users/paula/OneDrive - University of Texas at San Antonio/UTSA/HEAL Lab/[A] Opioid prescription in Indiana/CSV files/Suppl_Table_ZIP3_5_county_RUCA.txt", sep = "/t")

**References**

<https://library.virginia.edu/data/articles/an-introduction-to-loglinear-models>
