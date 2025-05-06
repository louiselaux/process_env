##### Process of SOMLIT STD data #####

# Load the libraries 
library("tidyverse")
library("dplyr")
library("ggplot2")
library("readr")

##### Step 1 : Read the data #####

## Read the environment data from seanoe
# read data from https://www.seanoe.org/data/00890/100175/
d_all <- read_delim("data/somlit_std_110524.csv", delim = ";",
                    escape_double = FALSE, comment = "//",
                    trim_ws = TRUE, skip = 1, show_col_types=FALSE)

# Cleanup by removing useless columns and renaming some variables 
d <- d_all |>
  dplyr::select(DATE:q_O2sat, -HEURE, -(COEF_MAREE:PROF_TEXT), -(`nomSite*`:`gpsLong*`)) |>
  rename(depth=PROF_NUM, date=DATE, T=TempPot, Sigma=SigmaTheta)

head(d)

# Correct extra depths
count(d, depth)
d <- d |> mutate(depth=if_else(depth == 2, 1, depth))
count(d, depth)

#if you want to look at other variables
#d<- d |> mutate(COP_NOP= COP/NOP,
#               COP_MES= COP/MES,
#               COP_chla= COP/CHLA,
#               qCOP_NOP=qCOP,
#               qCOP_MES=qMES,
#               qCOP_chla=qCOP)


##### Step 2 : Reformat data according to quality codes #####

# Remove data according to quality codes
# and reformat
dd <- d |>
  dplyr::select(-starts_with("q")) |>
  pivot_longer(-c(date, depth))

dq <- d |>
  dplyr::select(date, depth, starts_with("q")) |>
  pivot_longer(-c(date, depth)) |>
  mutate(name=name |> str_remove("^q") %>% str_remove("^_")) |>
  rename(qcode=value)

# Combine
dl <- inner_join(dd, dq) |>
  mutate(
    # replace by NA according to quality codes
    # 0	not detectable
    # 1	not measurable
    # 3	dubious
    # 4	bad
    # 5	not analysed
    # 9	missing
    value=if_else(qcode %in% c(0, 1, 3,4,5,9), NA, value),
    
    # still some missing = those are mistakes
    value=if_else(value > 9999, NA, value)
  ) %>%
  arrange(date)

##### Step 3 : Save it  #####
ggplot(dl) + facet_wrap(~name, scales="free_y") +
  geom_point(aes(x=date, y=value), size=0.2) + theme_bw()

# Plot at 10 meters depth
dl|> filter(depth==10)|> filter(name%in%c("CHLA","COP","MES","NH4","NO2","NO3","O","PO4","S","Sigma","T","SIOH4"))|> ggplot()+facet_wrap(~name, scales="free_y") +
  geom_point(aes(x=date, y=value), size=0.2)

# Remove irrelevant/too short variables
dl <- dl |> 
  filter(!name %in% c("CN", "DC13", "DN15", "Fluo", "NH4", "O2ctd", "O2sat", "PAR", "PHEO")) |> 
  filter(date > "1967-01-01") # filter to have the date at which plankton starts


#Remove the quality code column because not useful anymore

dl <- dl |> select(-qcode) 

# Save it 
std_all_depths <- dl
std_all_depths <- write_tsv(std_all_depths,"output/std_all_depths.tsv")


# Take the mean per variable per date 
data_env_mean<- dl |> group_by(date,name)|>summarize(value=mean(value, na.rm=TRUE))

# Inspection plot for one variable
data_env_mean|>filter(name=="T")|>ggplot()+geom_point(aes(x=date, y=value))

std_mean<- data_env_mean

#Save it
std_mean <- write_tsv(std_mean, "output/std_mean.tsv")
