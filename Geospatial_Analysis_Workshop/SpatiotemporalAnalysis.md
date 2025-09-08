This script contains all the necessary steps for the spatiotemporal
analysis for the paper called “A spatiotemporal analysis of opioid
prescriptions in Indiana from 2015 to 2019”.

## Libraries

The following libraries are necessary to run the script.

## Functions

Necessary functions to extract information from the last prescription
and to obtain the Indiana Medicaid population from American Community
Survey (ACS).

    SelectCensus<-function(Year){ 
      
      female_estimation<- get_acs(
        geography = "zcta",
        variables = "C27007_017",
        state = "IN", 
        survey = "acs5",
        year = as.numeric(Year)
      )
      
      female<-data.frame(NAME=female_estimation$NAME, estimate_female=female_estimation$estimate)
      
      male_estimation<- get_acs(
        geography = "zcta",
        variables = "C27007_007",
        state = "IN", 
        survey = "acs5",
        year = as.numeric(Year)
      )
      
      male<-data.frame(NAME=male_estimation$NAME, estimate_male=male_estimation$estimate)
      
      total_pop_Ind<- get_acs(
        geography = "zcta",
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

## Dataset and necessary arrays

Initialize necessary arrays and vectors, and load the dataset. First
step of the dataset is to include the patients between 19 to 64.

    #path <- "C:/Users/paula/OneDrive - University of Texas at San Antonio/UTSA/HEAL Lab/[A] Opioid prescription in Indiana/CSV files/2015to2019_19to64_IndianaDatasetFinal.csv"

    path <- "C:/Users/paula/OneDrive - University of Texas at San Antonio/UTSA/HEAL Lab/[A] Opioid prescription in Indiana/CSV files/08-13-24_2015to2019_19to64_IndianaDatasetFinal.csv"

    #path <-  "C:/Users/paula/OneDrive - University of Texas at San Antonio/UTSA/HEAL Lab/[A] Opioid prescription in Indiana/CSV files/08-20-24_LastPrescriptionByYear_2015to2019_19to64_IndianaDatasetFinal.csv"

    ext<-c("2015","2016","2017","2018","2019")
    max_date_total<-c(732471,732837,733202,733567,733932)
    min_date_total<-c(732107,732472,732838,733203,733568)


    global_MoranI_wnormal<-vector("numeric", length(ext))
    global_MoranI_wNormal_MC<-vector("numeric", length(ext))
    global_MoranI_normal<-vector("numeric", length(ext))
    global_MoranI_Normal_MC<-vector("numeric", length(ext))
    global_GTest_Normal<-vector("numeric", length(ext))


    Indiana_final_filter <- read.csv(path)

## Spatiotemporal analysis: Global and Local Moran’s I

The following `for statement` contains:

-   The identification of the cohort for each year, where individuals
    part of the study must aged between 19 to 64 years.

-   Extraction of Indiana Medicaid estimations from the ACS and the
    necessary shapefiles by 5-digit ZIP codes.

-   Obtain the final prescription for each patient, to obtain the age,
    gender, and race for each year.

-   Aggregate the estimation per 5-digit ZIP code zone to 3-digit ZIP
    code areas.

-   Calculate the global and the local Moran’s I.

<!-- -->

    for(i in 1:length(ext)){
      
      #Obtain the estimates of Indiana Medicaid population each year
      zip_codes<-SelectCensus(ext[i])

      data_unique_patients_per_Year_all <- Indiana_final_filter %>% 
        filter(year == ext[i]) %>% 
        arrange(desc(AllDate), DE_Identified_Recipient_ID) %>%
        group_by(DE_Identified_Recipient_ID) %>% 
        slice(1) %>% 
        ungroup()
      
      #Print the cohort for each year 
      print("Cohorts")
      print(length(unique(data_unique_patients_per_Year_all$DE_Identified_Recipient_ID)))

      zip_codes$FirstThreeDigits <- substr(zip_codes$GEOID, 1, 3)
      
      # Summarize population counts by 3-digit ZIP codes
      population_counts <- zip_codes %>%
        group_by(FirstThreeDigits) %>%
        summarize(total=sum(estimate))
      
      
      data_unique_patients_per_Year_all_2 <- data_unique_patients_per_Year_all %>% dplyr::count(Recipient_Addr_Zip5)
      
      colnames(data_unique_patients_per_Year_all_2)[1]="FirstThreeDigits"
      
      #Merge between total estimates from ACS and our cohort 
      merged_data <- merge(population_counts, data_unique_patients_per_Year_all_2, by = "FirstThreeDigits")
      merged_data$normal_count<- (merged_data$n/merged_data$total)*1000
      
      
      #To obtain the number of prescription per year 
      Count_prescriptions <- Indiana_final_filter %>% 
        filter(year == ext[i]) %>% 
        select(NDC_Code, Recipient_Addr_Zip5) %>% 
        group_by(NDC_Code, Recipient_Addr_Zip5) %>%
        count %>% 
        mutate(FirstThreeDigits = as.character(Recipient_Addr_Zip5), 
               count_presc = n) %>% 
        left_join(.,merged_data, by = "FirstThreeDigits") %>% 
        mutate(rate_presc = (count_presc/total)*1000) %>% 
        select(-Recipient_Addr_Zip5,-n.x, -n.y, -geometry, -normal_count) %>% 
        na.omit()
      
      assign(paste0("FinalDF_",ext[i]),merged_data)
      assign(paste0("CountPrescriptions_",ext[i]),Count_prescriptions)
      
      print(paste0("Most prescribed opioid - rate of prescription",ext[i]))
      print(Count_prescriptions %>% 
              group_by(FirstThreeDigits) %>% 
              filter(rate_presc == max(rate_presc)) %>% 
              ungroup() %>% 
              arrange(FirstThreeDigits))
      
      count_patients <- merged_data %>% ggplot((aes(fill = n))) +
        geom_sf(color = "black", lwd = 0.15) +
        theme_void() + scale_fill_gradient2()
      
      assign(paste("plot_CountPatients_withoutNormal", ext[i], sep=""),count_patients)
      
      
      # Calculate a spatial weights matrix
      nb <- poly2nb(st_geometry(merged_data$geometry))
      lw <- nb2listw(nb, style = "W")
      
      print("#### Global Moran's I - Not normalized data ####")
      moran_test_Global<-moran.test(merged_data$n,lw)
      print(moran_test_Global)
      
      global_MoranI_wnormal[i]<-moran_test_Global$estimate[1]
      
      print("#### Global Moran's I MonteCarlo Simulation - Not normalized data ####")
      MC<- moran.mc(merged_data$n, lw, nsim=99999, alternative="greater")
      print(MC)
      
      global_MoranI_wNormal_MC[i]<-MC$statistic[1]
      
      
      local <- localmoran(x = merged_data$n, listw = lw)
      moran.map <- cbind(merged_data, local)
      
      moran_map_wNormal <- tm_shape(moran.map) +
        tm_fill(col = "Ii",
                style = "quantile",
                title = "local moran statistic") 
      
      assign(paste("plot_moranLocal_withoutNormal", ext[i], sep=""),moran_map_wNormal)
      
      ########### Normalizing #######

      
      count_patients_normal<-merged_data %>% ggplot((aes(fill = normal_count))) +
        geom_sf(color = "black", lwd = 0.15) +
        theme_void() + scale_fill_gradient2(breaks = c(100,200,300,400,500,600)) + labs(title = ext[i])
      
      assign(paste("plot_CountPatients_Normal", ext[i], sep=""),count_patients_normal)
      
      
      # Calculate a spatial weights matrix
      nb <- poly2nb(as(merged_data, "Spatial"), queen = TRUE)
      lw <- nb2listw(nb, style = "W")
      
      claims_lag <- lag.listw(lw, merged_data$normal_count)
      
      print("#### Global Moran's I - Normalized data ####")
      moran_test_Global<-moran.test(merged_data$normal_count, lw, zero.policy = TRUE,  adjust.n = FALSE)
      print(moran_test_Global)
      
      global_MoranI_normal[i]<-moran_test_Global$estimate[1]
      
      print("#### Global Moran's I MonteCarlo Simulation - Normalized data ####")
      MC<- moran.mc(merged_data$normal_count, lw, nsim=99999, alternative="greater", zero.policy = TRUE,  adjust.n = FALSE)
      print(MC)
      
      global_MoranI_Normal_MC[i]<-MC$statistic[1]
      
      print("#### Global G test - Normalized data ####")
      global_GTest<- globalG.test(merged_data$normal_count, lw)
      print(global_GTest)
      
      global_GTest_Normal[i]<-global_GTest$estimate[1]

      
      claims_nbs <- merged_data |> 
        mutate(
          nb = st_contiguity(geometry),        # neighbors share border/vertex
          wt = st_weights(nb),                 # row-standardized weights
          tes_lag = st_lag(normal_count, nb, wt)    # calculate spatial lag of TreEqty
        ) 
      
      claims_hot_spots <- claims_nbs |> 
        mutate(
          Gi = local_g_perm(normal_count, nb, wt, nsim = 99999)
          # nsim = number of Monte Carlo simulations (999 is default)
        ) |> 
        # The new 'Gi' column itself contains a dataframe 
        # We can't work with that, so we need to 'unnest' it
        unnest(Gi) 
      
      
      plot_Gi_normal<-claims_hot_spots |> 
        ggplot((aes(fill = gi))) +
        geom_sf(color = "black", lwd = 0.15) +
        scale_fill_gradient2() # makes the value 0 (random) be the middle
      
      assign(paste("plot_Gi_normal", ext[i], sep=""),plot_Gi_normal)
      
      
      # Create a new data frame called 'tes_hot_spots"
      plot_claims_hot_spotsNormal<-claims_hot_spots |> 
        # with the columns 'gi' and 'p_folded_sim"
        # 'p_folded_sim' is the p-value of a folded permutation test
        select(gi, p_folded_sim) |> 
        mutate(
          # Add a new column called "classification"
          classification = case_when(
            # Classify based on the following criteria:
            gi > 0 & p_folded_sim <= 0.01 ~ "Very hot",
            gi > 0 & p_folded_sim <= 0.05 ~ "Hot",
            gi > 0 & p_folded_sim <= 0.1 ~ "Somewhat hot",
            gi < 0 & p_folded_sim <= 0.01 ~ "Very cold",
            gi < 0 & p_folded_sim <= 0.05 ~ "Cold",
            gi < 0 & p_folded_sim <= 0.1 ~ "Somewhat cold",
            TRUE ~ "Insignificant"
          ),
          # Convert 'classification' into a factor for easier plotting
          classification = factor(
            classification,
            levels = c("Very hot", "Hot", "Somewhat hot",
                       "Insignificant",
                       "Somewhat cold", "Cold", "Very cold")
          )
        ) |>
        # Visualize the results with ggplot2
        ggplot(aes(fill = classification)) +
        geom_sf(color = "black", lwd = 0.1) +
        scale_fill_brewer(type = "div", palette = 5, drop = FALSE) +
        theme_void() + 
        labs(
          fill = "Hot Spot Classification",
          title = ext[i]
        )
      
      assign(paste("claims_hot_spotsNormal", ext[i], sep=""),plot_claims_hot_spotsNormal)
      
      #### Local Moran's I with R ##### 
      
      g <- merged_data %>% 
        mutate(
          nb = st_contiguity(geometry),
          wt = st_weights(nb, style="W"),
          claims_lag = st_lag(normal_count, nb, wt)
        )
      
      
      lisa_Morans <- g %>% 
        mutate(moran = sfdep::local_moran(normal_count, nb, wt, nsim = 99999, alternative = "two.sided", mlvar = FALSE, zero.policy = TRUE))
      
      # pull(lisa_Morans, moran) %>% 
      #   glimpse()
      # 
      # 
      plot_claims_hot_spotsNormal_Morans<-lisa_Morans %>% 
        tidyr::unnest(moran) %>% 
        mutate(pysal = ifelse(p_folded_sim <= 0.05, as.character(pysal), "Insignificant"),
               pysal = factor(
                 pysal,
                 levels = c("High-High", "High-Low", "Low-High", "Low-Low", "Insignificant")
               )) |>
        ggplot(aes(fill = pysal)) + theme_void() +
        geom_sf(lwd = 0.2, color = "black") + 
        scale_fill_manual(values = c("#1C4769", "#24975E", "#EACA97", "#B20016", "#E0E0E0"), 
                          labels = c("High-High", "High-Low", "Low-High", "Low-Low", "Insignificant") , drop = FALSE) + 
        labs(title = ext[i], fill = "Local Indicators of Spatial Autocorrelation (LISA)") + theme(
          text = element_text(size = 20),
          legend.text = element_text(size = 22),
          legend.title = element_text(size = 20),
          axis.text = element_text(size = 10))
      
      assign(paste("claims_hot_spotsNormal_LocalMorans", ext[i], sep=""),plot_claims_hot_spotsNormal_Morans)
      
      print(paste0("Significant areas -", ext[i]))
      print(lisa_Morans %>% 
              tidyr::unnest(moran) %>% 
              select(FirstThreeDigits,ii,p_folded_sim) %>%
              filter(p_folded_sim <= 0.05)  %>% 
              st_drop_geometry() )
      
      
      ###### with GeoDa ##### 
      
      queen_w <- queen_weights(merged_data)
      lisa <- local_moran(queen_w,  merged_data['normal_count'], permutations = 99999, significance_cutoff = 0.05)
      
      assign(paste("lisa_geoda", ext[i], sep=""),lisa)
      
      lisa_colors <- lisa_colors(lisa)
      lisa_labels <- lisa_labels(lisa)
      lisa_clusters <- lisa_clusters(lisa)
      
      Geoda_MoransI <- plot(st_geometry(merged_data), 
           col=sapply(lisa_clusters, function(x){return(lisa_colors[[x+1]])}), 
           border = "#333333", lwd=0.2)
      title(main = ext[i])
      legend('topleft', legend = lisa_labels, fill = lisa_colors, border = "#eeeeee")
      
      assign(paste("Geoda_LocalMoransI", ext[i], sep=""),recordPlot())


      #plot.new()
      
    # "#eeeeee" "#FF0000" "#0000FF" "#a7adf9" "#f4ada8" "#464646" "#999999"
    }

Values for the Global Moran’s I.

    global_table <- matrix(global_MoranI_Normal_MC, ncol = 5, nrow = 1)
    colnames(global_table) <-as.character(c(2015:2019))

    print(global_table)

## Plots of Local Moran’s I by year

    x11()
    plot(ext,global_MoranI_wNormal_MC,ylim=range(-1:1), xlab = "Years", ylab = "Global Moran's I- MC")
    lines(ext[order(ext)], global_MoranI_wNormal_MC[order(ext)], ylim=range(-1:1), pch=16)

    x11()
    plot(ext,global_MoranI_wNormal_MC, xlab = "Years", ylab = "Global Moran's I - MC")
    lines(ext[order(ext)], global_MoranI_wNormal_MC[order(ext)], pch=16)

    x11()
    plot(ext,global_MoranI_Normal_MC,ylim=range(-1:1),xlab = "Years", ylab = "Global Moran's I Normal - MC")
    lines(ext[order(ext)], global_MoranI_Normal_MC[order(ext)], ylim=range(-1:1), pch=16)

    x11()
    par(mar = c(5, 5, 5, 5))
    plot(ext,global_MoranI_Normal_MC,xlab = "Years", ylab = "Global Moran's I",
         cex.lab=2, cex.axis=2, cex.main=2, lwd = 2, type = "b")
    lines(ext[order(ext)], global_MoranI_Normal_MC[order(ext)], ylim=range(-1:1), pch=16, lwd = 2, type = "b")

    x11()
    plot(ext,global_GTest_Normal, ylim=range(0:1), xlab = "Years", ylab = "Global Getis-Ord")
    lines(ext[order(ext)], global_GTest_Normal[order(ext)], ylim=range(0:1), pch=16)

    x11()
    plot(ext,global_GTest_Normal,xlab = "Years", ylab = "Global Getis-Ord")
    lines(ext[order(ext)], global_GTest_Normal[order(ext)], pch=16)



    # visualize normal counts
    x11()
    ggarrange(plot_CountPatients_Normal2015, plot_CountPatients_Normal2016, plot_CountPatients_Normal2017, plot_CountPatients_Normal2018, plot_CountPatients_Normal2019, ncol = 2, nrow = 3)


    ### Local Getis
    x11()
    ggarrange(claims_hot_spotsNormal2015, claims_hot_spotsNormal2016, claims_hot_spotsNormal2017, claims_hot_spotsNormal2018, claims_hot_spotsNormal2019, ncol = 2, nrow = 3)


    ## Local Moran's in R

    coordinates_data <- data.frame(
      longitude = c(-86.2978997, -85.233472, -87.550552,-86.3584564,-86.0516692,-86.214956,-86.5731313,-87.500160,-86.0588469,-86.8995012),  # Example coordinates
      latitude = c(39.7799313, 41.0827129, 37.977222,41.6748191,39.9641271,39.9647051,39.1712722,41.584660,40.0443036,40.4050391),
      city = c("Indianapolis","Fort Wayne", "Evansville","South Bend","Fishers","Carmel","Bloomington","Hammond","Noblesville","Lafayette")
    )



    claims_hot_spotsNormal_LocalMorans20151<-claims_hot_spotsNormal_LocalMorans2015 + geom_point(data = coordinates_data, aes(longitude, latitude), size  = 4,  fill = "yellow") + geom_label(data = coordinates_data, aes(longitude, latitude, label = city), size  = 8, fill = "white", nudge_y = c(-0.10,-0.10,0.10,-0.10,-0.10,0.10,-0.10,-0.10,0.10,0.12), nudge_x = c(0,0,0,0,0.2,-0.2,0,0,0.4,0))

    claims_hot_spotsNormal_LocalMorans20161<-claims_hot_spotsNormal_LocalMorans2016 + geom_point(data = coordinates_data, aes(longitude, latitude), size  = 4,  fill = "yellow") + geom_label(data = coordinates_data, aes(longitude, latitude, label = city), size  = 8, fill = "white", nudge_y = c(-0.10,-0.10,0.10,-0.10,-0.10,0.10,-0.10,-0.10,0.10,0.12), nudge_x = c(0,0,0,0,0.2,-0.2,0,0,0.4,0))

    claims_hot_spotsNormal_LocalMorans20171<-claims_hot_spotsNormal_LocalMorans2017 + geom_point(data = coordinates_data, aes(longitude, latitude), size  = 4,  fill = "yellow") + geom_label(data = coordinates_data, aes(longitude, latitude, label = city), size  = 8, fill = "white", nudge_y = c(-0.10,-0.10,0.10,-0.10,-0.10,0.10,-0.10,-0.10,0.10,0.12), nudge_x = c(0,0,0,0,0.2,-0.2,0,0,0.4,0))

    claims_hot_spotsNormal_LocalMorans20181<-claims_hot_spotsNormal_LocalMorans2018 + geom_point(data = coordinates_data, aes(longitude, latitude), size  = 4,  fill = "yellow") + geom_label(data = coordinates_data, aes(longitude, latitude, label = city), size  = 8, fill = "white", nudge_y = c(-0.10,-0.10,0.10,-0.10,-0.10,0.10,-0.10,-0.10,0.10,0.12), nudge_x = c(0,0,0,0,0.2,-0.2,0,0,0.4,0))

    claims_hot_spotsNormal_LocalMorans20191<-claims_hot_spotsNormal_LocalMorans2019 + geom_point(data = coordinates_data, aes(longitude, latitude), size  = 4,  fill = "yellow") + geom_label(data = coordinates_data, aes(longitude, latitude, label = city), size  = 8, fill = "white", nudge_y = c(-0.10,-0.10,0.10,-0.10,-0.10,0.10,-0.10,-0.10,0.10,0.12), nudge_x = c(0,0,0,0,0.2,-0.2,0,0,0.4,0))

    x11()
    claims_hot_spotsNormal_LocalMorans20151

    x11()
    claims_hot_spotsNormal_LocalMorans20161

    x11()
    claims_hot_spotsNormal_LocalMorans20171

    x11()
    claims_hot_spotsNormal_LocalMorans20181

    x11()
    claims_hot_spotsNormal_LocalMorans20191

Maximum of each rate of prescription by 3-digit ZIP code

    CountPrescriptions_2015 %>% 
      arrange(Recipient_Addr_Zip5, desc(rate_presc)) %>% 
      group_by(Recipient_Addr_Zip5) %>% 
      slice(1) %>% 
      ungroup() %>% 
      select(Recipient_Addr_Zip5, NDC_Code, rate_presc)

    CountPrescriptions_2016 %>% 
      arrange(Recipient_Addr_Zip5, desc(rate_presc)) %>% 
      group_by(Recipient_Addr_Zip5) %>% 
      slice(1) %>% 
      ungroup() %>% 
      select(NDC_Code, rate_presc)

    CountPrescriptions_2017 %>% 
      arrange(Recipient_Addr_Zip5, desc(rate_presc)) %>% 
      group_by(Recipient_Addr_Zip5) %>% 
      slice(1) %>% 
      ungroup() %>% 
      select(NDC_Code, rate_presc)

    CountPrescriptions_2018 %>% 
      arrange(Recipient_Addr_Zip5, desc(rate_presc)) %>% 
      group_by(Recipient_Addr_Zip5) %>% 
      slice(1) %>% 
      ungroup() %>% 
      select(NDC_Code, rate_presc)


    CountPrescriptions_2019 %>% 
      arrange(Recipient_Addr_Zip5, desc(rate_presc)) %>% 
      group_by(Recipient_Addr_Zip5) %>% 
      slice(1) %>% 
      ungroup() %>% 
      select(NDC_Code, rate_presc)

General rate of prescription by each 3-digit ZIP code.

    #2015
    CountPrescriptions_2015 %>%
      ungroup() %>% 
      select(Recipient_Addr_Zip5, count_presc, total) %>%
      group_by(Recipient_Addr_Zip5) %>% 
      mutate(total_prescriptions = sum(count_presc),
             general_rate_prescrip = total_prescriptions/total * 1000) %>%
      slice(1) %>% 
      ungroup() %>% 
      arrange(Recipient_Addr_Zip5) %>% 
      select(Recipient_Addr_Zip5, general_rate_prescrip)

    #2016 
    CountPrescriptions_2016 %>%
      ungroup() %>% 
      select(Recipient_Addr_Zip5, count_presc, total) %>%
      group_by(Recipient_Addr_Zip5) %>% 
      mutate(total_prescriptions = sum(count_presc),
             general_rate_prescrip = total_prescriptions/total * 1000) %>%
      slice(1) %>% 
      ungroup() %>% 
      arrange(Recipient_Addr_Zip5) %>% 
      select(Recipient_Addr_Zip5, general_rate_prescrip)

    #2017
    CountPrescriptions_2017 %>%
      ungroup() %>% 
      select(Recipient_Addr_Zip5, count_presc, total) %>%
      group_by(Recipient_Addr_Zip5) %>% 
      mutate(total_prescriptions = sum(count_presc),
             general_rate_prescrip = total_prescriptions/total * 1000) %>%
      slice(1) %>% 
      ungroup() %>% 
      arrange(Recipient_Addr_Zip5) %>% 
      select(general_rate_prescrip)

    #2018
    CountPrescriptions_2018 %>%
      ungroup() %>% 
      select(Recipient_Addr_Zip5, count_presc, total) %>%
      group_by(Recipient_Addr_Zip5) %>% 
      mutate(total_prescriptions = sum(count_presc),
             general_rate_prescrip = total_prescriptions/total * 1000) %>%
      slice(1) %>% 
      ungroup() %>% 
      arrange(Recipient_Addr_Zip5) %>% 
      select(general_rate_prescrip)

    #2019
    CountPrescriptions_2019 %>%
      ungroup() %>% 
      select(Recipient_Addr_Zip5, count_presc, total) %>%
      group_by(Recipient_Addr_Zip5) %>% 
      mutate(total_prescriptions = sum(count_presc),
             general_rate_prescrip = total_prescriptions/total * 1000) %>%
      slice(1) %>% 
      ungroup() %>% 
      arrange(Recipient_Addr_Zip5) %>% 
      select(general_rate_prescrip)
