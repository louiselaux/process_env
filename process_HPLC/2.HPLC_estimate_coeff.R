
#####Pre-cleaning of data#####
hplccomb <- hplccomb |> mutate(zea = ifelse(zea == 0, zea + lut, zea),
                               tchlb=ifelse(tchlb == 0 & (chlb != 0 | dvchlb != 0), chlb + dvchlb, tchlb))


##### Estimation of coeff: regression to look at diagnostic pigments #####

##### Step 1: Perform the multiple regressions to estimate the different coefficients of the different pigments #####

##### Define several datasets 

# One : Integrate on the whole water column
hplccomb_int <- hplccomb |>
  select(-depth) |>
  group_by(date) |>
  summarize(across(chlc3:anthera, ~ mean(., na.rm = TRUE)), .groups = "drop")

# Two : take the value at the surface 
hplccomb_surf <- hplccomb |>
  filter(depth==1)

# Perform the multiple regressions 

##### Multiple regression with surface data 
reg_multiple <- lm(tchla ~ fuco+peri+hex+but+allo+tchlb+zea, data = hplccomb_surf)

summary(reg_multiple)

#When integrated

reg_multiple_integrate<-lm(tchla ~ fuco+peri+hex+but+allo+tchlb+zea, data = hplccomb_int)

summary(reg_multiple_integrate)

#Partial effect of each variable
library(car)
avPlots(reg_multiple)

# Multiple regression with all the points

reg_multiple_all<-lm(tchla ~ fuco+peri+hex+but+allo+tchlb+zea, data = hplccomb)

summary(reg_multiple_all)



##### Step 2 : Estimation of pico, nano, micro phytoplankton based on the different coefficients #####


# With the coefficients of the multiple regression found in the litterature, paper from DiCicco in the Med Sea 

hplccomb_dicicco<- hplccomb|>mutate(SDPw=1.60*fuco+ 1.67*peri+1.18*hex+0.57*but+2.70*allo+0.88*tchlb+1.79*zea,
                                    fmicro=(1.60*fuco+1.67*peri)/SDPw,
                                    fnano=(1.18*hex+0.57*but+2.70*allo)/SDPw,
                                    fpico=(0.88*tchlb+1.79*zea)/SDPw,
                                    micro_chla=fmicro*tchla,
                                    nano_chla=fnano*tchla,
                                    pico_chla=fpico*tchla
                                    
)

# With my coefficients with all points
hplccomb_allpoints<- hplccomb|>mutate(SDPw=1.88*fuco+ 1.68*peri+1.28*hex+0.55*but+3.46*allo+1.17*tchlb+1.45*zea,
                                      fmicro=(1.88*fuco+1.68*peri)/SDPw,
                                      fnano=(1.28*hex+0.55*but+3.46*allo)/SDPw,
                                      fpico=(1.17*tchlb+1.45*zea)/SDPw,
                                      micro_chla=fmicro*tchla,
                                      nano_chla=fnano*tchla,
                                      pico_chla=fpico*tchla
                                      
)

######Taxonomic information######
hplccomb_dicicco<- hplccomb_dicicco|> mutate(diatoms=(1.60*fuco)/SDPw,
                                             dinoflagellates=(1.67*peri)/SDPw,
                                             green_algae=(0.88*chlb)/SDPw,
                                             prokaryotes=(1.79*zea)/SDPw,
                                             prochlorococcus=(dvchla)/tchla,
                                             micro=(fuco+peri)/tchla,
                                             pico=(tchlb+zea)/tchla
)

hplccomb_allpoints<- hplccomb_allpoints|> mutate(diatoms=(1.88*fuco)/SDPw,
                                                 dinoflagellates=(1.68*peri)/SDPw,
                                                 green_algae=(1.17*chlb)/SDPw,
                                                 prokaryotes=(1.45*zea)/SDPw,
                                                 prochlorococcus=(dvchla)/tchla,
                                                 micro=(fuco+peri)/tchla,
                                                 pico=(tchlb+zea)/tchla
)
###### Step 3: Save the final data table#####

hplc_coeffs<- hplccomb_allpoints %>% mutate( date= as.Date(date, format =("%d/%m/%y")))
hplc_coeffs<- write_tsv(hplc_coeffs, file="output/hplc_coeffs.tsv")
