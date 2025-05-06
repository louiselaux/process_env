#####HPLC data #####


#Load the libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)


##### Step 1: Read and reformat the data table #####

#Read the data table
hplc <- read_delim("data/HPLC_data.csv",
                   delim = ";", escape_double = FALSE, trim_ws = TRUE)


#Basic inspection
summary(hplc)

hplc<- as.data.frame(hplc)



# How many LODs?
hplc|> filter(phda=="LOD")|>group_by(`Sampling date`)|>dplyr::summarize(count=n())


#Change the disposition of the data table
hplc <- hplc |>
  mutate(across(
    chlc3:q_anthera,
    ~ as.numeric(gsub(",", ".", .))      # Replace comma by points and point in numeric format
  ))

head(hplc)

# Rename some columns

hplc <- hplc |> rename (depth =`Depth (m)`,
                        date = `Sampling date`)

# Remove last two columns that are full of NAs

hplc <- hplc [,-59:-60]



##### Step 2 : inspect quality of data by looking at chla vs TAP to look at the qc quality#####

hplc <- hplc |>
  mutate(TAP = rowSums(across(c(chlc3, chlc2c1, tchlb, peri, fuco, hex, but, tcar, viola, diadino, diato, zea)), na.rm = TRUE), # TAP is the sum of characteristic pigments
         ind=chlda / tchla) # ind the degradation index 

# Compute the log of tchla and TAP
hplc<- hplc|> mutate(
  TAP_log= log10(TAP),
  tchla_log=log10(tchla))

# test that we have TAP values at depth 75
hplctest<-hplc|>filter(depth %in% c(75))
head(hplctest)

# Plot the inspection plot
ggplot()+ geom_point(aes(x=tchla_log,y=TAP_log), data=hplc)+ theme_bw()

#Plot it with the R square and the value of the slope
ggplot(hplc, aes(x = tchla_log, y = TAP_log)) +
  geom_point(aes(color = as.factor(q_tchla))) +  # points colored by tchla
  stat_smooth(method = "lm", color = "red", se = FALSE) +
  stat_regline_equation(aes(label = ..eq.label..), formula = y ~ x) +
  stat_cor(aes(label = ..rr.label..), label.x.npc = "center", label.y.npc = "top") +
  theme_bw() +
  labs(color = "Q_tchla")

# Plot it from raw values but in log10 scale 
ggplot(hplc, aes(x = tchla, y = TAP)) +
  geom_point(aes(color = as.factor(q_tchla))) +  # points colored by tchla
  stat_smooth(method = "lm", color = "red", se = FALSE) +
  stat_regline_equation(aes(label = ..eq.label..), formula = y ~ x) +
  stat_cor(aes(label = ..rr.label..), label.x.npc = "center", label.y.npc = "top") +
  theme_bw() +
  labs(color = "Q_tchla")+ scale_x_log10()+scale_y_log10()


# Inspect the qc values 

#check something
count_qc2 <- hplc |>
  filter(q_tchla == 2) |>
  summarize(count = n())

#With everything
hplc_long <- hplc |>
  pivot_longer(
    cols = starts_with("q_"),
    names_to = "q_variable",
    values_to = "q_value"
  ) |>
  mutate(q_value = as.factor(q_value))

#Some plots to look at the quality of data

ggplot(hplc_long, aes(x = tchla_log, y = TAP_log)) +
  geom_point(aes(color = q_value)) +
  stat_smooth(method = "lm", color = "red", se = FALSE) +
  stat_regline_equation(aes(label = ..eq.label..), formula = y ~ x) +
  facet_wrap(~ q_variable, scales = "free_y") +
  theme(strip.background=element_rect(fill="lightblue"))+
  labs(color = "QC Value", title = "TAP vs Tchla with points colored by qc value of one pigment")

#Show only bad QC
ggplot(hplc_long, aes(x = tchla_log, y = TAP_log)) +
  geom_point(data = hplc_long |> filter(q_value != "1"), aes(color = q_value)) +
  stat_smooth(method = "lm", color = "red", se = FALSE) +
  stat_regline_equation(aes(label = ..eq.label..), formula = y ~ x) +
  facet_wrap(~ q_variable, scales = "free_y") +
  theme(strip.background = element_rect(fill = "lightblue")) +
  labs(color = "QC Value", title = "TAP vs Tchla with points colored by qc value of one pigment")

#Get how many qc==3 per variable
hplc_long <- hplc |>
  pivot_longer(cols = starts_with("q_"),
               names_to = "q_variable",
               values_to = "q_value")

hplc_long|>group_by(q_variable,q_value)|>summarize(count=n())|>filter(q_value=="3")

# Number of values where bad qc per variable
count_qc3_by_variable <- hplc_long |>
  filter(q_value == 3) |>
  group_by(q_variable,depth) |>
  summarize(count = n(), .groups = "drop")

# Print the data_table
count_qc3_by_variable

# Number of values where bad qc per variable
count_qc0_by_variable <- hplc_long |>
  filter(q_value == 0) |>
  group_by(q_variable) |>
  summarize(count = n(), .groups = "drop")

# Print the data_table
count_qc0_by_variable

#Check the degradation ratio
# Visualize the distribution of ratio per QC
ggplot(hplc_long, aes(x = q_value, y = ind, fill = as.factor(q_value))) +
  geom_boxplot(outlier.color = "red") +
  labs(
    x = "QC Value",
    y = "Degradation Ratio (chlorophyllide_a / TChla)",
    title = "Distribution of Degradation Ratio by QC Value"
  ) +
  theme_minimal()

#Look at the degradation ratio
ggplot(hplc_long, aes(x = ind, fill = as.factor(q_value))) +
  geom_histogram(binwidth = 0.01, color = "black", alpha = 0.7, position = "identity")  +
  facet_wrap(~ q_variable, scales = "free_y") +
  labs(
    x = "Degradation Ratio (chlorophyllide_a / tchla)",
    y = "Count",
    fill = "QC Value",
    title = "Histogram of Degradation Ratio by Variable and Colored by QC"
  ) +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "lightblue"))

##### Third  step: keep only good QC##### --> Remove QC=3 #####

hplcd <- hplc|>
  dplyr::select(-starts_with("q")) |>
  pivot_longer(-c('Cruise or Project', 'date', 'Latitude N', 'Longitude E', 'HPLC file', 'depth'))|>mutate(value=as.numeric(value))
               
hplcc <- hplc |>
  dplyr::select('date','depth', starts_with("q")) |>
  pivot_longer(-c('date', 'depth')) |>
  mutate(name=name |> str_remove("^q") |> str_remove("^_")) |>
  rename(qcode=value)

# Combine the two

hplccomb <- inner_join(hplcd, hplcc) |>
  mutate(
    value=if_else(qcode==3, NA, value)
  ) |>distinct()

hplccomb<-hplccomb|>dplyr::select(-qcode)|>pivot_wider(names_from=name, values_from=value)


