##### CTD variables #####

# Load the packages and libraries
#devtools::install_github("jiho/castr")
library(castr)
library(ggplot2)
library(dplyr)
library(tidyverse)

# Load CTD variables
radehydro_ctd <- read.csv("data/radehydro_ctd.csv.gz")

# Inspect the data
summary(radehydro_ctd)
head(radehydro_ctd)
colnames(radehydro_ctd)
length(unique(radehydro_ctd$id))
unique(radehydro_ctd$station)
length(unique(radehydro_ctd$date)) #not the same number, so several id per date?

##################################################################################
#####Step 1: Clean-up : Identification of station name problems #####

# Keep point B station
#radehydro_ctd<- radehydro_ctd|> filter(station=="B"|station=="B+")

# Look at data distribution
max(radehydro_ctd$pressure)
unique(radehydro_ctd$station)
length(unique(radehydro_ctd$date))



# Create a data table with the max depth for each date
ctd_pointB <- radehydro_ctd|> group_by(date,station,id)|>summarise(maxd=max(pressure))


##### Rename stations #####

#Look at how many stations per date
station_counts <- ctd_pointB |>
  group_by(date) |>
  summarise(station_count = n(), .groups = 'drop')

# Select stations that have more than one station per date
multiple_stations <- station_counts |>
  filter(station_count > 1)


# Change name of stations according to max depth
ctd_pointB_corrected <-ctd_pointB |>
  group_by(date,id) |>
  mutate(new_station = ifelse(maxd < 100 & station!="Unknown"& station!="C" & date%in%multiple_stations$date, "B", station) ) |>
  mutate(new_station = ifelse(maxd > 100 & new_station == "B"& new_station!="Unknown"& new_station!="C", "B+", new_station))|>
  ungroup()

# Look at if there are some problems if duplicated stations
duplicate_stations <- ctd_pointB_corrected |>
  group_by(date, new_station) |>
  summarise(station_count = n(), .groups = 'drop')|>
  filter(station_count > 1)

# Adjust the correction
ctd_pointB_corrected<- ctd_pointB_corrected|>
  group_by(date)|>
  mutate(new_station=ifelse(date %in% duplicate_stations$date & new_station=="B",station,new_station))

# Check with Céline
ctd_pointB_corrected |>
  group_by(date, new_station) |>
  summarise(station_count = n(), .groups = 'drop')|>
  filter(station_count > 1)

# Check if no more problem
ctd_pointB_corrected |>
  group_by(date, new_station) |>
  summarise(station_count = n(), .groups = 'drop')|>
  filter(station_count > 1)|>filter(new_station=="B+")

# Further check
ctd_pointB_corrected|>filter(date=="2018-10-16") #enlever des analyses le B
ctd_pointB_corrected|>filter(date=="2020-06-02")


# How many C renamed in B?
ctd_pointB_corrected|>filter(station=="C")|>filter(station!=new_station)#ok
ctd_pointB_corrected|>filter(station=="C")|>filter(maxd<100) #problem to correct manually
ctd_pointB_corrected|>filter(station=="B+")|>filter(maxd<100)


# Manual check
ctd_pointB_corrected|>filter(date=="2005-02-08")
ctd_pointB_corrected|>filter(date=="2005-02-15")


# Correct manually one misnamed --> TO DO OR NOT?
ctd_pointB_corrected <- ctd_pointB_corrected|>mutate(new_station=ifelse(id=="B15132_02_SBE25plus","B",new_station))

# Other check
ctd_pointB_corrected |>
  filter(str_starts(id, "C") & new_station != "C" &station!=new_station)

# Check that worked properly
c<-ctd_pointB_corrected|>filter(new_station=="B")
max(c$maxd)

#####Get the id of badly named stations #####

# How many mismatches
diff_stations <- ctd_pointB_corrected |>
  filter(station != new_station)
length(unique(diff_stations$date)) #360 dates for which it has been mismatched

#save(diff_stations, file="diff_stations.Rdata")
#save(diff_stations, file="diff_stations2.Rdata")

#Save it
write.csv(diff_stations, file = "diff_stations2.csv", row.names = FALSE)
save(ctd_pointB_corrected,file="ctd_pointB_corrected.Rdata")

#Inspect weird cases
ctd_pointB_corrected|>filter(date=="2015-05-12")



#####Change the initial data table######
#Change the initial datatable
joined_radehydro_ctd <- radehydro_ctd |>
  left_join(ctd_pointB_corrected |> select(date, station, new_station,id),
            by = c("date", "station","id")) |>
  # Replace when necessary
  mutate(station_corrected = ifelse(!is.na(new_station), new_station, station))

head(joined_radehydro_ctd)


#Save it
save(joined_radehydro_ctd, file="joined_radehydro_ctd.Rdata")

#Check it worked
joined_radehydro_ctd|>filter(date=="2002-01-02")
joined_radehydro_ctd|>filter(station=="C")|>filter(station!=new_station)|>select(date,id,station,station_corrected)|>distinct()

#####Last inspection
#Compare number of B compared to initial data table
table(joined_radehydro_ctd$station_corrected)
table(radehydro_ctd$station) #77 C have disappeared --> due to "2015-05-12"


#Inspection of stations renamed
diff_stations|>group_by(date,new_station)|>summarise(count=n())|>
  filter(count>1)

test<-ctd_pointB_corrected|>filter(station=="B+")|>filter(maxd<100)
ctd_pointB_corrected|>filter(date%in%test$date)|>group_by(date)|>summarise(count=n())|>filter(count>2)

#Inspect the weird ones
ctd_pointB_corrected|>filter(date=="2015-05-12")
ctd_pointB_corrected|>filter(date=="2004-12-21")
#Further inspect
radehydro_ctd|>filter(id=="B15132_02_SBE25plus")|>head()
radehydro_ctd|>filter(id=="B15132_SBE25plus")|>head()

#Inspect the dates that have a problem
ctd_pointB_corrected|>filter(date=="2015-12-22")
ctd_pointB_corrected|>filter(date=="2021-01-05")
ctd_pointB_corrected|>filter(date=="2022-09-06")

#Investigate this
ctd_pointB_corrected |>
  group_by(date, new_station) |>
  summarise(station_count = n(), .groups = 'drop')|>filter(station_count > 1)

#Select only B stations
point_B<- joined_radehydro_ctd|>filter(station_corrected=="B")
