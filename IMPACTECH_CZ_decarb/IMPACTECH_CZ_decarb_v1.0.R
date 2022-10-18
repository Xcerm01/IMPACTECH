##########################################################################
# impactech decarb_emp_cz

# Sectors: production of electricity, metallurgy, cement production, chemicals (plastics, n-fertilizers), motor vehicles manufacturing

# Indicators: employment requirements (persons employed), compensation of employees, consumption of fixed capital

# Geographic scope: MRIO (CZ+RoW)

# Input data:
# (1) IO data: EXIOBASE 3.6 monetary industry-by-industry 2015 data: https://zenodo.org/record/3583071#.YhqMUujMKUk
# (2) Other input data from this repository ("input")

##########################################################################

install.packages("readxl")
install.packages("tidyverse")
install.packages("tidyr")
install.packages("reshape2")
install.packages("data.table")
install.packages("dplyr")
install.packages("janitor")
install.packages("ggplot2")
install.packages("stringr")
install.packages("backports")
install.packages("gridExtra")
install.packages("zoo")

library(readxl)
library(tidyverse)
library(dplyr)
library(tidyr)
library(janitor)
library(data.table)
library(reshape2)
library(ggplot2)
library(stringr)
library(tibble)
library(gridExtra)
library(zoo)



##########################################################################
# 0. Set path and load data
##########################################################################

##########################################################################
# 0.1. Set path

## To be defined by the user
exiopath <- "D:/EXIOBASE 3.6/parsed/" ## storage of original exiobase data
datapath <- "C:/Users/Cerny/Documents/Work/Models/IMPACTECH/data/input_data/" ## storage of input files
intpath <- "C:/Users/Cerny/Documents/Work/Models/IMPACTECH/data/intermediate_data/" ## storage of intermediate data
respath <- "C:/Users/Cerny/Documents/Work/Models/IMPACTECH/results/" ## storage of results


##########################################################################
# 0.2. Load original and adjusted EXIOBASE 3.6 data

Q.codes <- read.csv(paste0(intpath,"Q.codes.csv"), header = TRUE, sep = ";", dec = ",")
load(paste0(intpath,"IO_CZ.codes.RData"))
load(paste0(intpath,"Y_CZ.codes.RData"))
load(paste0(intpath,"Z_CZ.RData"))
load(paste0(intpath,"Y_CZ.RData"))
load(paste0(intpath,"x2015_CZ.RData"))
load(paste0(intpath,"x2015el_CZ.RData"))
load(paste0(intpath,"x2015el_repl.RData"))
load(paste0(intpath,"x2015fuel_CZ.RData"))
load(paste0(intpath,"EUSTi24.d_CZ.RData"))
load(paste0(intpath,"EUSTi40.2_CZ.RData"))
load(paste0(intpath,"A_CZ.RData"))
load(paste0(intpath,"E_CZ.RData"))
load(paste0(intpath,"Q_CZ.codes.RData"))
load(paste0(intpath,"Ext_el_CZ.RData"))
load(paste0(intpath,"Ext_CZ.RData"))
load(paste0(intpath,"Ext_CZ_agg.RData"))
load(paste0(intpath,"E_el_CZ.RData"))
load(paste0(intpath,"E_CZ_short.RData"))


##########################################################################
# 0.3. Define initial sets

countries <- c("CZ")
countries_all <- c("CZ","RoW")
years = c("2015","2030","2050")  ## years considered in the analysis
scenarios <- c("bau","circular","electro_hydro","ccs")
decarb <- c("i24.a.1","i24.a.2","i24.a.3","i24.a.w","i24.b.1","i24.b.2","i24.b.3","i26.d.1","i26.d.2","i26.d.3","i26.d.w","i27.a.1","i27.a.2","i27.a.3","i27.a.w","i34.1","i34.2","i40.11.e.1","i40.11.g","i40.11.h.1","i40.11.h.2")
decarb_CZ <- c("CZ_i24.a.1","CZ_i24.a.2","CZ_i24.a.3","CZ_i24.a.w","CZ_i24.b.1","CZ_i24.b.2","CZ_i24.b.3","CZ_i26.d.1","CZ_i26.d.2","CZ_i26.d.3","CZ_i26.d.w","CZ_i27.a.1","CZ_i27.a.2","CZ_i27.a.3","CZ_i27.a.w","CZ_i34.1","CZ_i34.2","CZ_i40.11.e.1","CZ_i40.11.g","CZ_i40.11.h.1","CZ_i40.11.h.2")
decarb_modeled <- c("CZ_i24.a.1","CZ_i24.a.2","CZ_i24.a.3","CZ_i24.b.1","CZ_i24.b.2","CZ_i24.b.3","CZ_i26.d.1","CZ_i26.d.2","CZ_i26.d.3","CZ_i27.a.1","CZ_i27.a.2","CZ_i27.a.3","CZ_i34.1","CZ_i34.2")
industrial_CZ <- c("CZ_i15.a","CZ_i15.b","CZ_i15.c","CZ_i15.d","CZ_i15.e","CZ_i15.f","CZ_i15.g","CZ_i15.h","CZ_i15.i","CZ_i15.j","CZ_i15.k",
                   "CZ_i16","CZ_i17","CZ_i18","CZ_i19",
                   "CZ_i20","CZ_i20.w","CZ_i21.1","CZ_i21.w.1","CZ_i21.2","CZ_i22","CZ_i23.1","CZ_i23.2","CZ_i23.3",
                   "CZ_i24.c","CZ_i24.d","CZ_i25","CZ_i26.a","CZ_i26.a.w","CZ_i26.b","CZ_i26.c","CZ_i26.e",
                   "CZ_i27.41","CZ_i27.41.w","CZ_i27.42","CZ_i27.42.w","CZ_i27.43","CZ_i27.43.w","CZ_i27.44","CZ_i27.44.w","CZ_i27.45","CZ_i27.45.w","CZ_i27.5",
                   "CZ_i28","CZ_i29","CZ_i30","CZ_i31","CZ_i32","CZ_i33","CZ_i35","CZ_i36","CZ_i37","CZ_i37.w.1",
                   "CZ_i40.11.l","CZ_i40.12","CZ_i40.13","CZ_i40.2","CZ_i40.3","CZ_i41","CZ_i45","CZ_i45.w")
industrial <- c("i15.a","i15.b","i15.c","i15.d","i15.e","i15.f","i15.g","i15.h","i15.i","i15.j","i15.k",
                "i16","i17","i18","i19",
                "i20","i20.w","i21.1","i21.w.1","i21.2","i22","i23.1","i23.2","i23.3",
                "i24.c","i24.d","i25","i26.a","i26.a.w","i26.b","i26.c","i26.e",
                "i27.41","i27.41.w","i27.42","i27.42.w","i27.43","i27.43.w","i27.44","i27.44.w","i27.45","i27.45.w","i27.5",
                "i28","i29","i30","i31","i32","i33","i35","i36","i37","i37.w.1",
                "i40.11.l","i40.12","i40.13","i40.2","i40.3","i41","i45","i45.w")

## INFORMATION ABOUT THE SCENARIOS
## 1) ELECTRO_HYDRO - preference for electrification or hydrogen-based technologies, scenarios relying on electrification of energy end-use
## 2) CCS - preference for carbon capture and storage/use-based technologies
## 3) CIRCULAR - preference for recycling, secondary production and/or renewable materials (biomass etc.)
## 4) BAU - preference for unabated routes or scenarios based to a large extent on existing technology mix

## The typology of scenarios is adapted to the situation in each sector.
## In the vast majority of cases, these are adapted (simplified, keeping the essential definitional contours and assumptions) scenarios
## from the Material Economics (2019) study, supplemented by scenarios in the energy sector.

## For scenarios in the energy sector (especially electricity), we consider approximate compatibility
## when combining with scenarios from other decarbonized sectors.
## We look at how compatible the rate of increase/decrease in electricity and other fuel consumption in other decarbonized sectors is
## with their demand for electricity and these fuels. In other words, whether the increase in demand for a given fuel associated with
## changes in a given sector "fits" the (electro)energy scenario, or conversely whether the decrease in demand for a given fuel
## corresponds to a proportional decrease in its production in the (electro)energy scenario.


##########################################################################
# 0.4. Load necessary functions

agg <- function(x)
{
  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
  return(x)
}

`%!in%` = Negate(`%in%`)

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))


##########################################################################
# 0.5. Information about replaced missing values from other countries - AT, DE, ES, FR

## i40.11.e.2 (wind offshore) <- DE
## i40.11.g (biomass and waste) <- AT
## i40.11.i (solar thermal) <- ES
## i40.11.j (tide, wave, ocean) <- FR
## i40.11.k (Geothermal) <- AT



##########################################################################
# 1. Extract and export original opex columns for the electricity sectors and other modeled decarbonizing sectors
##########################################################################

##########################################################################
# 1.1. Extract xdecarb_CZ for the electricity and other modeled decarbonizing sectors

xdecarbonly_CZ <- as.data.frame(x2015_CZ[rownames(x2015_CZ) %in% c("CZ_i24.a","CZ_i24.a.w","CZ_i24.b","CZ_i26.d","CZ_i26.d.w","CZ_i27.a","CZ_i27.a.w","CZ_i34",
                                                                   "CZ_i40.11.a","CZ_i40.11.b","CZ_i40.11.c","CZ_i40.11.d","CZ_i40.11.e.1","CZ_i40.11.e.2","CZ_i40.11.f","CZ_i40.11.g","CZ_i40.11.h.1","CZ_i40.11.h.2","CZ_i40.11.i","CZ_i40.11.j","CZ_i40.11.k"), , drop = FALSE])

save(xdecarbonly_CZ, file = paste0(intpath,"xdecarb_CZ.RData")) ## Note, xdecarbonly_CZ saved as xdecarb_CZ
fwrite(xdecarbonly_CZ, file = paste0(intpath,"xdecarb_CZ.csv"), sep=";", dec=",", row.names=TRUE) ## Note, xdecarbonly_CZ saved as xdecarb_CZ


##########################################################################
# 1.2. Adjust employment values for the modeled decarb sectors from CZSO

## Extract E_CZ_decarb
E_CZ_decarb <- as.data.frame(E_CZ[Q.codes$Compartment %in% c("Value.added","Employment.persons") & Q.codes$Stressor != "Employment: Vulnerable employment",
                                  colnames(E_CZ) %in% c("CZ_i24.a","CZ_i24.a.w","CZ_i24.b","CZ_i26.d","CZ_i26.d.w","CZ_i27.a","CZ_i27.a.w","CZ_i34")])
E_CZ_decarb$Name <- Q_CZ.codes$Stressor[1:15]
E_CZ_decarb <- E_CZ_decarb[,c(9,1:8)]

save(E_CZ_decarb, file = paste0(intpath,"E_CZ_decarb.RData"))
fwrite(E_CZ_decarb, file = paste0(intpath,"E_CZ_decarb.csv"), sep=";", dec=",")

## Read E_CZ_decarb_repl for employment total in 1000 persons
E_CZ_decarb_repl <- readxl::read_excel(paste0(datapath,"labor/empfactors_decarb_cz.xlsx"), sheet = "emp_factors")
E_CZ_decarb_repl <- as.data.frame(E_CZ_decarb_repl[1:8,])
rownames(E_CZ_decarb_repl) <- paste0(E_CZ_decarb_repl$Country.Code,"_",E_CZ_decarb_repl$Industry.Code)
E_CZ_decarb_repl <- E_CZ_decarb_repl$`Employment: Total (1000p)`

## Extract shares of different skill levels
Ext_CZ_agg <- as.data.frame(Ext_CZ_agg[Q_CZ.codes$Compartment %in% c("Value.added","Employment.persons")&Q_CZ.codes$Stressor != "Employment: Vulnerable employment",])
Ext_CZ_agg_decarb <- Ext_CZ_agg[,colnames(Ext_CZ_agg) %in% c("CZ_i24.a","CZ_i24.a.w","CZ_i24.b","CZ_i26.d","CZ_i26.d.w","CZ_i27.a","CZ_i27.a.w","CZ_i34")]

emp_low_m <- Ext_CZ_agg_decarb[Q.codes$Stressor=="Employment: Low-skilled male",]
emp_low_f <- Ext_CZ_agg_decarb[Q.codes$Stressor=="Employment: Low-skilled female",]
emp_medium_m <- Ext_CZ_agg_decarb[Q.codes$Stressor=="Employment: Medium-skilled male",]
emp_medium_f <- Ext_CZ_agg_decarb[Q.codes$Stressor=="Employment: Medium-skilled female",]
emp_high_m <- Ext_CZ_agg_decarb[Q.codes$Stressor=="Employment: High-skilled male",]
emp_high_f <- Ext_CZ_agg_decarb[Q.codes$Stressor=="Employment: High-skilled female",]

## Calculate emp_tot (total employment intensity)
emp_tot <- emp_low_m + emp_low_f + emp_medium_m + emp_medium_f + emp_high_m + emp_high_f

share_low_m <- emp_low_m / emp_tot
share_low_f <- emp_low_f / emp_tot
share_medium_m <- emp_medium_m / emp_tot
share_medium_f <- emp_medium_f / emp_tot
share_high_m <- emp_high_m / emp_tot
share_high_f <- emp_high_f / emp_tot

## Calculate the adjusted employment intensity
xdecarbmodeled_CZ <- xdecarbonly_CZ[rownames(xdecarbonly_CZ) %in% c("CZ_i24.a","CZ_i24.a.w","CZ_i24.b","CZ_i26.d","CZ_i26.d.w","CZ_i27.a","CZ_i27.a.w","CZ_i34"),]
Ext_CZ_decarb_repl <- t(E_CZ_decarb_repl / as.data.frame(xdecarbmodeled_CZ))

Ext_CZ_agg_decarb[Q.codes$Stressor=="Employment: Low-skilled male", c(1:ncol(Ext_CZ_decarb_repl))] <- share_low_m * Ext_CZ_decarb_repl
Ext_CZ_agg_decarb[Q.codes$Stressor=="Employment: Low-skilled female", c(1:ncol(Ext_CZ_decarb_repl))] <- share_low_f * Ext_CZ_decarb_repl
Ext_CZ_agg_decarb[Q.codes$Stressor=="Employment: Medium-skilled male", c(1:ncol(Ext_CZ_decarb_repl))] <- share_medium_m * Ext_CZ_decarb_repl
Ext_CZ_agg_decarb[Q.codes$Stressor=="Employment: Medium-skilled female", c(1:ncol(Ext_CZ_decarb_repl))] <- share_medium_f * Ext_CZ_decarb_repl
Ext_CZ_agg_decarb[Q.codes$Stressor=="Employment: High-skilled male", c(1:ncol(Ext_CZ_decarb_repl))] <- share_high_m * Ext_CZ_decarb_repl
Ext_CZ_agg_decarb[Q.codes$Stressor=="Employment: High-skilled female", c(1:ncol(Ext_CZ_decarb_repl))] <- share_high_f * Ext_CZ_decarb_repl

## Implement into Ext_CZ_agg
Ext_CZ_agg[,colnames(Ext_CZ_agg) %in% c("CZ_i24.a","CZ_i24.a.w","CZ_i24.b","CZ_i26.d","CZ_i26.d.w","CZ_i27.a","CZ_i27.a.w","CZ_i34")] <- Ext_CZ_agg_decarb[,colnames(Ext_CZ_agg_decarb) %in% c("CZ_i24.a","CZ_i24.a.w","CZ_i24.b","CZ_i26.d","CZ_i26.d.w","CZ_i27.a","CZ_i27.a.w","CZ_i34")]


##########################################################################
# 1.3. Separate columns from A_CZ for the electricity and other modeled decarbonizing sectors

opex_decarb_int_CZ <- as.data.frame(A_CZ)
opex_decarb_int_CZ <- opex_decarb_int_CZ[,colnames(opex_decarb_int_CZ) %in% c("CZ_i24.a","CZ_i24.a.w","CZ_i24.b","CZ_i26.d","CZ_i26.d.w","CZ_i27.a","CZ_i27.a.w","CZ_i34",
                                                                              "CZ_i40.11.a","CZ_i40.11.b","CZ_i40.11.c","CZ_i40.11.d","CZ_i40.11.e.1","CZ_i40.11.e.2","CZ_i40.11.f","CZ_i40.11.g","CZ_i40.11.h.1","CZ_i40.11.h.2","CZ_i40.11.i","CZ_i40.11.j","CZ_i40.11.k")]
opex_decarb_int_CZ$Name <- IO_CZ.codes$Industry.Name
opex_decarb_int_CZ$Name <- factor(opex_decarb_int_CZ$Name, levels = unique(opex_decarb_int_CZ$Name), ordered = TRUE)
opex_decarb_int_CZ <- opex_decarb_int_CZ %>% 
  group_by(Name) %>%
  summarize(CZ_i24.a = sum(CZ_i24.a),
            CZ_i24.a.w = sum(CZ_i24.a.w),
            CZ_i24.b = sum(CZ_i24.b),
            CZ_i26.d = sum(CZ_i26.d),
            CZ_i26.d.w = sum(CZ_i26.d.w),
            CZ_i27.a = sum(CZ_i27.a),
            CZ_i27.a.w = sum(CZ_i27.a.w),
            CZ_i34 = sum(CZ_i34),
            CZ_i40.11.a = sum(CZ_i40.11.a),
            CZ_i40.11.b = sum(CZ_i40.11.b),
            CZ_i40.11.c = sum(CZ_i40.11.c),
            CZ_i40.11.d = sum(CZ_i40.11.d),
            CZ_i40.11.e.1 = sum(CZ_i40.11.e.1),
            CZ_i40.11.e.2 = sum(CZ_i40.11.e.2),
            CZ_i40.11.f = sum(CZ_i40.11.f),
            CZ_i40.11.g = sum(CZ_i40.11.g),
            CZ_i40.11.h.1 = sum(CZ_i40.11.h.1),
            CZ_i40.11.h.2 = sum(CZ_i40.11.h.2),
            CZ_i40.11.i = sum(CZ_i40.11.i),
            CZ_i40.11.j = sum(CZ_i40.11.j),
            CZ_i40.11.k = sum(CZ_i40.11.k))


##########################################################################
# 1.4. Separate columns from Ext_CZ_agg for the electricity and other modeled decarbonizing sectors

## Unify Ext_CZ_agg and opex_decarb_ext_CZ (also for later use of Ext_CZ_agg)
opex_decarb_ext_CZ <- Ext_CZ_agg[,colnames(Ext_CZ_agg) %in% c("CZ_i24.a","CZ_i24.a.w","CZ_i24.b","CZ_i26.d","CZ_i26.d.w","CZ_i27.a","CZ_i27.a.w","CZ_i34",
                                                              "CZ_i40.11.a","CZ_i40.11.b","CZ_i40.11.c","CZ_i40.11.d","CZ_i40.11.e.1","CZ_i40.11.e.2","CZ_i40.11.f","CZ_i40.11.g","CZ_i40.11.h.1","CZ_i40.11.h.2","CZ_i40.11.i","CZ_i40.11.j","CZ_i40.11.k")]
opex_decarb_ext_CZ$Name <- Q_CZ.codes$Stressor[1:15]
opex_decarb_ext_CZ <- opex_decarb_ext_CZ[,c(22,1:21)]


##########################################################################
# 1.5. rbind opex_decarb_int_CZ and opex_decarb_ext_CZ for the electricity and other modeled decarbonizing sectors, export and save

CZ_decarb_opex <- bind_rows(opex_decarb_int_CZ, opex_decarb_ext_CZ)
colnames(CZ_decarb_opex) <- c("Industry.Name","CZ_i24.a","CZ_i24.a.w","CZ_i24.b","CZ_i26.d","CZ_i26.d.w","CZ_i27.a","CZ_i27.a.w","CZ_i34",
                              "CZ_i40.11.a","CZ_i40.11.b","CZ_i40.11.c","CZ_i40.11.d","CZ_i40.11.e.1","CZ_i40.11.e.2","CZ_i40.11.f","CZ_i40.11.g","CZ_i40.11.h.1","CZ_i40.11.h.2","CZ_i40.11.i","CZ_i40.11.j","CZ_i40.11.k")

save(CZ_decarb_opex, file = paste0(intpath,"CZ_decarb_opex.RData"))
fwrite(CZ_decarb_opex, file = paste0(intpath,"CZ_decarb_opex.csv"), sep=";", dec=",")


##########################################################################
# 1.6. Separate columns from A_CZ for the rest of the industrial sectors

opex_decarbplus_int_CZ <- as.data.frame(A_CZ)
opex_decarbplus_int_CZ <- opex_decarbplus_int_CZ[,colnames(opex_decarbplus_int_CZ) %in% industrial_CZ]
opex_decarbplus_int_CZ$Name <- IO_CZ.codes$Industry.Name
opex_decarbplus_int_CZ$Name <- factor(opex_decarbplus_int_CZ$Name, levels = unique(opex_decarbplus_int_CZ$Name), ordered = TRUE)
opex_decarbplus_int_CZ <- opex_decarbplus_int_CZ %>%
  tibble::as_tibble(opex_decarbplus_int_CZ) %>%
  group_by(Name) %>%
  summarise_at(vars(CZ_i15.a:CZ_i45.w), sum, na.rm = TRUE)


##########################################################################
# 1.7. Separate columns from Ext_CZ_agg for the electricity and other modeled decarbonizing sectors

## Create opex_decarbplus_ext_CZ from adapted Ext_CZ_agg
opex_decarbplus_ext_CZ <- Ext_CZ_agg[,colnames(Ext_CZ_agg) %in% industrial_CZ]
opex_decarbplus_ext_CZ$Name <- Q_CZ.codes$Stressor[1:15]
opex_decarbplus_ext_CZ <- opex_decarbplus_ext_CZ[,c(62,1:61)]


##########################################################################
# 1.8. rbind opex_decarbplus_int_CZ and opex_decarbplus_ext_CZ for the electricity and other modeled decarbonizing sectors, export and save

CZ_decarbplus_opex <- bind_rows(opex_decarbplus_int_CZ, opex_decarbplus_ext_CZ)
colnames(CZ_decarbplus_opex) <- c("Industry.Name",industrial_CZ)

save(CZ_decarbplus_opex, file = paste0(intpath,"CZ_decarbplus_opex.RData"))
fwrite(CZ_decarbplus_opex, file = paste0(intpath,"CZ_decarbplus_opex.csv"), sep=";", dec=",")



##########################################################################
# 2. Split modeled sectors in the IO elements used in the analysis
##########################################################################

##########################################################################
# 2.1. Create IOdecarb.codes by adding new rows

IOdecarb_CZ.codes <- IO_CZ.codes

plastics.2 <- sort(which(IOdecarb_CZ.codes$Industry.Code == "i24.a"), decreasing = TRUE)
for(i in plastics.2) {
  IOdecarb_CZ.codes <- IOdecarb_CZ.codes %>% 
    tibble::add_row(
      Index = NA_integer_,
      Industry.Name = "Plastics, basic - (Electric) steam cracking (+ (End of Life) CCS)",
      Industry.Code = "i24.a.2",
      Country.Code = NA_character_,
      Region.Code = NA_character_,
      Industry.Group = NA_character_,
      .after = i
    )
}

plastics.3 <- sort(which(IOdecarb_CZ.codes$Industry.Code == "i24.a.2"), decreasing = TRUE)
for(i in plastics.3) {
  IOdecarb_CZ.codes <- IOdecarb_CZ.codes %>% 
    tibble::add_row(
      Index = NA_integer_,
      Industry.Name = "Plastics, basic - Biomass feedstock",
      Industry.Code = "i24.a.3",
      Country.Code = NA_character_,
      Region.Code = NA_character_,
      Industry.Group = NA_character_,
      .after = i
    )
}

nfertiliser.2 <- sort(which(IOdecarb_CZ.codes$Industry.Code == "i24.b"), decreasing = TRUE)
for(i in nfertiliser.2) {
  IOdecarb_CZ.codes <- IOdecarb_CZ.codes %>% 
    tibble::add_row(
      Index = NA_integer_,
      Industry.Name = "N-fertiliser - Steam methane reforming (SMR) with CCS",
      Industry.Code = "i24.b.2",
      Country.Code = NA_character_,
      Region.Code = NA_character_,
      Industry.Group = NA_character_,
      .after = i
    )
}

nfertiliser.3 <- sort(which(IOdecarb_CZ.codes$Industry.Code == "i24.b.2"), decreasing = TRUE)
for(i in nfertiliser.3) {
  IOdecarb_CZ.codes <- IOdecarb_CZ.codes %>% 
    tibble::add_row(
      Index = NA_integer_,
      Industry.Name = "N-fertiliser - Water electrolysis",
      Industry.Code = "i24.b.3",
      Country.Code = NA_character_,
      Region.Code = NA_character_,
      Industry.Group = NA_character_,
      .after = i
    )
}

cement.2 <- sort(which(IOdecarb_CZ.codes$Industry.Code == "i26.d"), decreasing = TRUE)
for(i in cement.2) {
  IOdecarb_CZ.codes <- IOdecarb_CZ.codes %>% 
    tibble::add_row(
      Index = NA_integer_,
      Industry.Name = "Manufacture of cement, lime and plaster - Electrification of kiln (+ direct separation (CCS))",
      Industry.Code = "i26.d.2",
      Country.Code = NA_character_,
      Region.Code = NA_character_,
      Industry.Group = NA_character_,
      .after = i
    )
}

cement.3 <- sort(which(IOdecarb_CZ.codes$Industry.Code == "i26.d.2"), decreasing = TRUE)
for(i in cement.3) {
  IOdecarb_CZ.codes <- IOdecarb_CZ.codes %>% 
    tibble::add_row(
      Index = NA_integer_,
      Industry.Name = "Manufacture of cement, lime and plaster - Oxyfuel CCS",
      Industry.Code = "i26.d.3",
      Country.Code = NA_character_,
      Region.Code = NA_character_,
      Industry.Group = NA_character_,
      .after = i
    )
}

steel.2 <- sort(which(IOdecarb_CZ.codes$Industry.Code == "i27.a"), decreasing = TRUE)
for(i in steel.2) {
  IOdecarb_CZ.codes <- IOdecarb_CZ.codes %>% 
    tibble::add_row(
      Index = NA_integer_,
      Industry.Name = "Manufacture of basic iron and steel and of ferro-alloys and first products thereof - Hydrogen Direct Reduction (H-DR)",
      Industry.Code = "i27.a.2",
      Country.Code = NA_character_,
      Region.Code = NA_character_,
      Industry.Group = NA_character_,
      .after = i
    )
}

steel.3 <- sort(which(IOdecarb_CZ.codes$Industry.Code == "i27.a.2"), decreasing = TRUE)
for(i in steel.3) {
  IOdecarb_CZ.codes <- IOdecarb_CZ.codes %>% 
    tibble::add_row(
      Index = NA_integer_,
      Industry.Name = "Manufacture of basic iron and steel and of ferro-alloys and first products thereof - Smelting reduction with CCS",
      Industry.Code = "i27.a.3",
      Country.Code = NA_character_,
      Region.Code = NA_character_,
      Industry.Group = NA_character_,
      .after = i
    )
}

vehicles.2 <- sort(which(IOdecarb_CZ.codes$Industry.Code == "i34"), decreasing = TRUE)
for(i in vehicles.2) {
  IOdecarb_CZ.codes <- IOdecarb_CZ.codes %>% 
    tibble::add_row(
      Index = NA_integer_,
      Industry.Name = "Manufacture of motor vehicles, trailers and semi-trailers - Electric Vehicle",
      Industry.Code = "i34.2",
      Country.Code = NA_character_,
      Region.Code = NA_character_,
      Industry.Group = NA_character_,
      .after = i
    )
}

## Adjust NAs in the new rows, names of the 'old' rows (sectors) and the rest of the table
IOdecarb_CZ.codes <- IOdecarb_CZ.codes %>% do(na.locf(.)) ## Replace NA values with values for the previous row
IOdecarb_CZ.codes$Index <- 1:nrow(IOdecarb_CZ.codes) ## Adjust numbers in 'Index' column

IOdecarb_CZ.codes$Industry.Code[IOdecarb_CZ.codes$Industry.Code == "i24.a"] <- "i24.a.1"
IOdecarb_CZ.codes$Industry.Code[IOdecarb_CZ.codes$Industry.Code == "i24.b"] <- "i24.b.1"
IOdecarb_CZ.codes$Industry.Code[IOdecarb_CZ.codes$Industry.Code == "i26.d"] <- "i26.d.1"
IOdecarb_CZ.codes$Industry.Code[IOdecarb_CZ.codes$Industry.Code == "i27.a"] <- "i27.a.1"
IOdecarb_CZ.codes$Industry.Code[IOdecarb_CZ.codes$Industry.Code == "i34"] <- "i34.1"

IOdecarb_CZ.codes$Industry.Name[IOdecarb_CZ.codes$Industry.Name == "Plastics, basic"] <- "Plastics, basic - Steam cracking (unabated route)"
IOdecarb_CZ.codes$Industry.Name[IOdecarb_CZ.codes$Industry.Name == "N-fertiliser"] <- "N-fertiliser - Steam methane reforming (SMR) for ammonia production"
IOdecarb_CZ.codes$Industry.Name[IOdecarb_CZ.codes$Industry.Name == "Manufacture of cement, lime and plaster"] <- "Manufacture of cement, lime and plaster - Ordinary Portland Cement (OPC)"
IOdecarb_CZ.codes$Industry.Name[IOdecarb_CZ.codes$Industry.Name == "Manufacture of basic iron and steel and of ferro-alloys and first products thereof"] <- "Manufacture of basic iron and steel and of ferro-alloys and first products thereof - Integrated mill (BF-BOF)"
IOdecarb_CZ.codes$Industry.Name[IOdecarb_CZ.codes$Industry.Name == "Manufacture of motor vehicles, trailers and semi-trailers (34)"] <- "Manufacture of motor vehicles, trailers and semi-trailers - Internal Combustion Engine"


##########################################################################
# 2.2. Calculate shares for the base year of the detailed sectors to disaggregate the rows or columns of x, Y, Z and E

## Read the external source (from decarb_emp_cz)
gen <- list()

for (scenario in scenarios) {
  gen[[scenario]] <- readxl::read_excel(paste0(datapath, "/decarb_emp_cz.xlsx"), sheet = paste0("scen_",scenario), range = cell_limits(c(1,1),c(NA,19)), col_types = c("text","text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
  gen[[scenario]] <- as.data.frame(gen[[scenario]])
  rownames(gen[[scenario]]) <- paste0(gen[[scenario]]$Country.Code,"_",gen[[scenario]]$Industry.Code)
}

## Extract shares on total production of each sector for the disaggregated (sub)sectors
## Shares (aka "shares_full") for 2030 and 2050 have to be both year- and scenario- specific
shares_full <- list()

for (scenario in scenarios) {
  for (year in years) {
    shares_full[[scenario]][[year]] <- as.data.frame(gen[[scenario]][rownames(gen[[scenario]]) %in% decarb_modeled,paste0("shares",year)])
    rownames(shares_full[[scenario]][[year]]) <- decarb_modeled
    colnames(shares_full[[scenario]][[year]]) <- c("Shares")
    shares_full[[scenario]][[year]] <- as.matrix(shares_full[[scenario]][[year]])
  }
}

shares <- shares_full[["bau"]][["2015"]] ## doesn't matter which scenario, the shares are the same for all for 2015


##########################################################################
# 2.3. Create x2015decarb_CZ by adding new rows

## Disaggregate x2015_CZ to x2015decarb_CZ
x2015decarb_CZ <- x2015_CZ
x2015decarb_CZ <- as.data.frame(x2015decarb_CZ)
rownames(x2015decarb_CZ) <- paste0(IO_CZ.codes$Country.Code,"_",IO_CZ.codes$Industry.Code)

detailed.1 <- sort(cbind(which(endsWith(rownames(x2015decarb_CZ),"i24.a")),
                         which(endsWith(rownames(x2015decarb_CZ),"i24.b")),
                         which(endsWith(rownames(x2015decarb_CZ),"i26.d")),
                         which(endsWith(rownames(x2015decarb_CZ),"i27.a")),
                         which(endsWith(rownames(x2015decarb_CZ),"i34"))), decreasing = TRUE)
detailed.2 <- sort(cbind(which(endsWith(rownames(x2015decarb_CZ),"CZ_i24.a")),
                         which(endsWith(rownames(x2015decarb_CZ),"CZ_i24.b"))+1,
                         which(endsWith(rownames(x2015decarb_CZ),"CZ_i26.d"))+2,
                         which(endsWith(rownames(x2015decarb_CZ),"CZ_i27.a"))+3,
                         which(endsWith(rownames(x2015decarb_CZ),"RoW_i24.a"))+5,
                         which(endsWith(rownames(x2015decarb_CZ),"RoW_i24.b"))+6,
                         which(endsWith(rownames(x2015decarb_CZ),"RoW_i26.d"))+7,
                         which(endsWith(rownames(x2015decarb_CZ),"RoW_i27.a"))+8), decreasing = TRUE)
x2015decarb_CZ <- as_tibble(x2015decarb_CZ)
for(i in detailed.1) {
  x2015decarb_CZ <- x2015decarb_CZ %>% 
    tibble::add_row(
      .after = i
    )
}
for(i in detailed.2) {
  x2015decarb_CZ <- x2015decarb_CZ %>% 
    tibble::add_row(
      .after = i
    )
}
rownames(x2015decarb_CZ) <- paste0(IOdecarb_CZ.codes$Country.Code,"_",IOdecarb_CZ.codes$Industry.Code)

## Distribute the right values over the new rows (by dividing the original sectors' production for each year considered)
## Implement "shares" into x2015decarb_CZ for the new rows (sectors)
x2015decarb_CZ <- as.matrix(x2015decarb_CZ)

## The "shares" are only for CZ, therefore the following code works only for CZ
## This is solved by inserting zeros to the newly created sectors in the RoW
x2015decarb_CZ[which(endsWith(rownames(x2015decarb_CZ), "CZ_i24.a.1"))] <- shares[which(endsWith(rownames(shares), "CZ_i24.a.1"))] * x2015_CZ[which(endsWith(rownames(x2015_CZ), "CZ_i24.a")),]
x2015decarb_CZ[which(endsWith(rownames(x2015decarb_CZ), "CZ_i24.a.2"))] <- shares[which(endsWith(rownames(shares), "CZ_i24.a.2"))] * x2015_CZ[which(endsWith(rownames(x2015_CZ), "CZ_i24.a")),]
x2015decarb_CZ[which(endsWith(rownames(x2015decarb_CZ), "CZ_i24.a.3"))] <- shares[which(endsWith(rownames(shares), "CZ_i24.a.3"))] * x2015_CZ[which(endsWith(rownames(x2015_CZ), "CZ_i24.a")),]
x2015decarb_CZ[which(endsWith(rownames(x2015decarb_CZ), "CZ_i24.b.1"))] <- shares[which(endsWith(rownames(shares), "CZ_i24.b.1"))] * x2015_CZ[which(endsWith(rownames(x2015_CZ), "CZ_i24.b")),]
x2015decarb_CZ[which(endsWith(rownames(x2015decarb_CZ), "CZ_i24.b.2"))] <- shares[which(endsWith(rownames(shares), "CZ_i24.b.2"))] * x2015_CZ[which(endsWith(rownames(x2015_CZ), "CZ_i24.b")),]
x2015decarb_CZ[which(endsWith(rownames(x2015decarb_CZ), "CZ_i24.b.3"))] <- shares[which(endsWith(rownames(shares), "CZ_i24.b.3"))] * x2015_CZ[which(endsWith(rownames(x2015_CZ), "CZ_i24.b")),]
x2015decarb_CZ[which(endsWith(rownames(x2015decarb_CZ), "CZ_i26.d.1"))] <- shares[which(endsWith(rownames(shares), "CZ_i26.d.1"))] * x2015_CZ[which(endsWith(rownames(x2015_CZ), "CZ_i26.d")),]
x2015decarb_CZ[which(endsWith(rownames(x2015decarb_CZ), "CZ_i26.d.2"))] <- shares[which(endsWith(rownames(shares), "CZ_i26.d.2"))] * x2015_CZ[which(endsWith(rownames(x2015_CZ), "CZ_i26.d")),]
x2015decarb_CZ[which(endsWith(rownames(x2015decarb_CZ), "CZ_i26.d.3"))] <- shares[which(endsWith(rownames(shares), "CZ_i26.d.3"))] * x2015_CZ[which(endsWith(rownames(x2015_CZ), "CZ_i26.d")),]
x2015decarb_CZ[which(endsWith(rownames(x2015decarb_CZ), "CZ_i27.a.1"))] <- shares[which(endsWith(rownames(shares), "CZ_i27.a.1"))] * x2015_CZ[which(endsWith(rownames(x2015_CZ), "CZ_i27.a")),]
x2015decarb_CZ[which(endsWith(rownames(x2015decarb_CZ), "CZ_i27.a.2"))] <- shares[which(endsWith(rownames(shares), "CZ_i27.a.2"))] * x2015_CZ[which(endsWith(rownames(x2015_CZ), "CZ_i27.a")),]
x2015decarb_CZ[which(endsWith(rownames(x2015decarb_CZ), "CZ_i27.a.3"))] <- shares[which(endsWith(rownames(shares), "CZ_i27.a.3"))] * x2015_CZ[which(endsWith(rownames(x2015_CZ), "CZ_i27.a")),]
x2015decarb_CZ[which(endsWith(rownames(x2015decarb_CZ), "CZ_i34.1"))] <- shares[which(endsWith(rownames(shares), "CZ_i34.1"))] * x2015_CZ[which(endsWith(rownames(x2015_CZ), "CZ_i34")),]
x2015decarb_CZ[which(endsWith(rownames(x2015decarb_CZ), "CZ_i34.2"))] <- shares[which(endsWith(rownames(shares), "CZ_i34.2"))] * x2015_CZ[which(endsWith(rownames(x2015_CZ), "CZ_i34")),]

x2015decarb_CZ[is.na(x2015decarb_CZ)] <- 0


##########################################################################
# 2.4. Create Ydecarb_CZ by adding new rows

## Disaggregate Y to Ydecarb_CZ
Ydecarb_CZ <- Y_CZ
Ydecarb_CZ <- as.data.frame(Ydecarb_CZ)
rownames(Ydecarb_CZ) <- paste0(IO_CZ.codes$Country.Code,"_",IO_CZ.codes$Industry.Code)

detailed.1 <- sort(cbind(which(endsWith(rownames(Ydecarb_CZ),"i24.a")),
                         which(endsWith(rownames(Ydecarb_CZ),"i24.b")),
                         which(endsWith(rownames(Ydecarb_CZ),"i26.d")),
                         which(endsWith(rownames(Ydecarb_CZ),"i27.a")),
                         which(endsWith(rownames(Ydecarb_CZ),"i34"))), decreasing = TRUE)
detailed.2 <- sort(cbind(which(endsWith(rownames(Ydecarb_CZ),"CZ_i24.a")),
                         which(endsWith(rownames(Ydecarb_CZ),"CZ_i24.b"))+1,
                         which(endsWith(rownames(Ydecarb_CZ),"CZ_i26.d"))+2,
                         which(endsWith(rownames(Ydecarb_CZ),"CZ_i27.a"))+3,
                         which(endsWith(rownames(Ydecarb_CZ),"RoW_i24.a"))+5,
                         which(endsWith(rownames(Ydecarb_CZ),"RoW_i24.b"))+6,
                         which(endsWith(rownames(Ydecarb_CZ),"RoW_i26.d"))+7,
                         which(endsWith(rownames(Ydecarb_CZ),"RoW_i27.a"))+8), decreasing = TRUE)
Ydecarb_CZ <- as_tibble(Ydecarb_CZ)
for(i in detailed.1) {
  Ydecarb_CZ <- Ydecarb_CZ %>% 
    tibble::add_row(
      .after = i
    )
}
for(i in detailed.2) {
  Ydecarb_CZ <- Ydecarb_CZ %>% 
    tibble::add_row(
      .after = i
    )
}
rownames(Ydecarb_CZ) <- paste0(IOdecarb_CZ.codes$Country.Code,"_",IOdecarb_CZ.codes$Industry.Code)

## Distribute the right values over the new rows (from external source by dividing the original sectors' production)
## Implement "shares" into Ydecarb_CZ for the new rows (sectors)
Ydecarb_CZ <- as.matrix(Ydecarb_CZ)

## The "shares" are only for CZ, therefore the following code works only for CZ
## This is solved by inserting zeros to the newly created sectors in the RoW
Ydecarb_CZ[which(endsWith(rownames(Ydecarb_CZ), "CZ_i24.a.1")),] <- shares[which(endsWith(rownames(shares), "CZ_i24.a.1"))] * Y_CZ[which(endsWith(rownames(Y_CZ), "CZ_i24.a")),]
Ydecarb_CZ[which(endsWith(rownames(Ydecarb_CZ), "CZ_i24.a.2")),] <- shares[which(endsWith(rownames(shares), "CZ_i24.a.2"))] * Y_CZ[which(endsWith(rownames(Y_CZ), "CZ_i24.a")),]
Ydecarb_CZ[which(endsWith(rownames(Ydecarb_CZ), "CZ_i24.a.3")),] <- shares[which(endsWith(rownames(shares), "CZ_i24.a.3"))] * Y_CZ[which(endsWith(rownames(Y_CZ), "CZ_i24.a")),]
Ydecarb_CZ[which(endsWith(rownames(Ydecarb_CZ), "CZ_i24.b.1")),] <- shares[which(endsWith(rownames(shares), "CZ_i24.b.1"))] * Y_CZ[which(endsWith(rownames(Y_CZ), "CZ_i24.b")),]
Ydecarb_CZ[which(endsWith(rownames(Ydecarb_CZ), "CZ_i24.b.2")),] <- shares[which(endsWith(rownames(shares), "CZ_i24.b.2"))] * Y_CZ[which(endsWith(rownames(Y_CZ), "CZ_i24.b")),]
Ydecarb_CZ[which(endsWith(rownames(Ydecarb_CZ), "CZ_i24.b.3")),] <- shares[which(endsWith(rownames(shares), "CZ_i24.b.3"))] * Y_CZ[which(endsWith(rownames(Y_CZ), "CZ_i24.b")),]
Ydecarb_CZ[which(endsWith(rownames(Ydecarb_CZ), "CZ_i26.d.1")),] <- shares[which(endsWith(rownames(shares), "CZ_i26.d.1"))] * Y_CZ[which(endsWith(rownames(Y_CZ), "CZ_i26.d")),]
Ydecarb_CZ[which(endsWith(rownames(Ydecarb_CZ), "CZ_i26.d.2")),] <- shares[which(endsWith(rownames(shares), "CZ_i26.d.2"))] * Y_CZ[which(endsWith(rownames(Y_CZ), "CZ_i26.d")),]
Ydecarb_CZ[which(endsWith(rownames(Ydecarb_CZ), "CZ_i26.d.3")),] <- shares[which(endsWith(rownames(shares), "CZ_i26.d.3"))] * Y_CZ[which(endsWith(rownames(Y_CZ), "CZ_i26.d")),]
Ydecarb_CZ[which(endsWith(rownames(Ydecarb_CZ), "CZ_i27.a.1")),] <- shares[which(endsWith(rownames(shares), "CZ_i27.a.1"))] * Y_CZ[which(endsWith(rownames(Y_CZ), "CZ_i27.a")),]
Ydecarb_CZ[which(endsWith(rownames(Ydecarb_CZ), "CZ_i27.a.2")),] <- shares[which(endsWith(rownames(shares), "CZ_i27.a.2"))] * Y_CZ[which(endsWith(rownames(Y_CZ), "CZ_i27.a")),]
Ydecarb_CZ[which(endsWith(rownames(Ydecarb_CZ), "CZ_i27.a.3")),] <- shares[which(endsWith(rownames(shares), "CZ_i27.a.3"))] * Y_CZ[which(endsWith(rownames(Y_CZ), "CZ_i27.a")),]
Ydecarb_CZ[which(endsWith(rownames(Ydecarb_CZ), "CZ_i34.1")),] <- shares[which(endsWith(rownames(shares), "CZ_i34.1"))] * Y_CZ[which(endsWith(rownames(Y_CZ), "CZ_i34")),]
Ydecarb_CZ[which(endsWith(rownames(Ydecarb_CZ), "CZ_i34.2")),] <- shares[which(endsWith(rownames(shares), "CZ_i34.2"))] * Y_CZ[which(endsWith(rownames(Y_CZ), "CZ_i34")),]

Ydecarb_CZ[is.na(Ydecarb_CZ)] <- 0


##########################################################################
# 2.5. Create Zdecarb_CZ by adding new rows and columns

## Disaggregate Z to Zdecarb_CZ
## By rows
Zdecarb_CZ <- Z_CZ
Zdecarb_CZ <- as.data.frame(Zdecarb_CZ)
rownames(Zdecarb_CZ) <- paste0(IO_CZ.codes$Country.Code,"_",IO_CZ.codes$Industry.Code)

detailed.1 <- sort(cbind(which(endsWith(rownames(Zdecarb_CZ),"i24.a")),
                         which(endsWith(rownames(Zdecarb_CZ),"i24.b")),
                         which(endsWith(rownames(Zdecarb_CZ),"i26.d")),
                         which(endsWith(rownames(Zdecarb_CZ),"i27.a")),
                         which(endsWith(rownames(Zdecarb_CZ),"i34"))), decreasing = TRUE)
detailed.2 <- sort(cbind(which(endsWith(rownames(Zdecarb_CZ),"CZ_i24.a")),
                         which(endsWith(rownames(Zdecarb_CZ),"CZ_i24.b"))+1,
                         which(endsWith(rownames(Zdecarb_CZ),"CZ_i26.d"))+2,
                         which(endsWith(rownames(Zdecarb_CZ),"CZ_i27.a"))+3,
                         which(endsWith(rownames(Zdecarb_CZ),"RoW_i24.a"))+5,
                         which(endsWith(rownames(Zdecarb_CZ),"RoW_i24.b"))+6,
                         which(endsWith(rownames(Zdecarb_CZ),"RoW_i26.d"))+7,
                         which(endsWith(rownames(Zdecarb_CZ),"RoW_i27.a"))+8), decreasing = TRUE)
Zdecarb_CZ <- as_tibble(Zdecarb_CZ)
for(i in detailed.1) {
  Zdecarb_CZ <- Zdecarb_CZ %>% 
    tibble::add_row(
      .after = i
    )
}
for(i in detailed.2) {
  Zdecarb_CZ <- Zdecarb_CZ %>% 
    tibble::add_row(
      .after = i
    )
}
rownames(Zdecarb_CZ) <- paste0(IOdecarb_CZ.codes$Country.Code,"_",IOdecarb_CZ.codes$Industry.Code)

## Distribute the right values over the new rows (from external source by dividing the original sectors' production)
## Implement "shares" into Zdecarb_CZ for the new rows (sectors)
Zdecarb_CZ <- as.matrix(Zdecarb_CZ)

## The "shares" are only for CZ, therefore the following code works only for CZ
## This is solved by inserting zeros to the newly created sectors in the RoW
Zdecarb_CZ[which(endsWith(rownames(Zdecarb_CZ), "CZ_i24.a.1")),] <- shares[which(endsWith(rownames(shares), "CZ_i24.a.1"))] * Z_CZ[which(endsWith(rownames(Z_CZ), "CZ_i24.a")),]
Zdecarb_CZ[which(endsWith(rownames(Zdecarb_CZ), "CZ_i24.a.2")),] <- shares[which(endsWith(rownames(shares), "CZ_i24.a.2"))] * Z_CZ[which(endsWith(rownames(Z_CZ), "CZ_i24.a")),]
Zdecarb_CZ[which(endsWith(rownames(Zdecarb_CZ), "CZ_i24.a.3")),] <- shares[which(endsWith(rownames(shares), "CZ_i24.a.3"))] * Z_CZ[which(endsWith(rownames(Z_CZ), "CZ_i24.a")),]
Zdecarb_CZ[which(endsWith(rownames(Zdecarb_CZ), "CZ_i24.b.1")),] <- shares[which(endsWith(rownames(shares), "CZ_i24.b.1"))] * Z_CZ[which(endsWith(rownames(Z_CZ), "CZ_i24.b")),]
Zdecarb_CZ[which(endsWith(rownames(Zdecarb_CZ), "CZ_i24.b.2")),] <- shares[which(endsWith(rownames(shares), "CZ_i24.b.2"))] * Z_CZ[which(endsWith(rownames(Z_CZ), "CZ_i24.b")),]
Zdecarb_CZ[which(endsWith(rownames(Zdecarb_CZ), "CZ_i24.b.3")),] <- shares[which(endsWith(rownames(shares), "CZ_i24.b.3"))] * Z_CZ[which(endsWith(rownames(Z_CZ), "CZ_i24.b")),]
Zdecarb_CZ[which(endsWith(rownames(Zdecarb_CZ), "CZ_i26.d.1")),] <- shares[which(endsWith(rownames(shares), "CZ_i26.d.1"))] * Z_CZ[which(endsWith(rownames(Z_CZ), "CZ_i26.d")),]
Zdecarb_CZ[which(endsWith(rownames(Zdecarb_CZ), "CZ_i26.d.2")),] <- shares[which(endsWith(rownames(shares), "CZ_i26.d.2"))] * Z_CZ[which(endsWith(rownames(Z_CZ), "CZ_i26.d")),]
Zdecarb_CZ[which(endsWith(rownames(Zdecarb_CZ), "CZ_i26.d.3")),] <- shares[which(endsWith(rownames(shares), "CZ_i26.d.3"))] * Z_CZ[which(endsWith(rownames(Z_CZ), "CZ_i26.d")),]
Zdecarb_CZ[which(endsWith(rownames(Zdecarb_CZ), "CZ_i27.a.1")),] <- shares[which(endsWith(rownames(shares), "CZ_i27.a.1"))] * Z_CZ[which(endsWith(rownames(Z_CZ), "CZ_i27.a")),]
Zdecarb_CZ[which(endsWith(rownames(Zdecarb_CZ), "CZ_i27.a.2")),] <- shares[which(endsWith(rownames(shares), "CZ_i27.a.2"))] * Z_CZ[which(endsWith(rownames(Z_CZ), "CZ_i27.a")),]
Zdecarb_CZ[which(endsWith(rownames(Zdecarb_CZ), "CZ_i27.a.3")),] <- shares[which(endsWith(rownames(shares), "CZ_i27.a.3"))] * Z_CZ[which(endsWith(rownames(Z_CZ), "CZ_i27.a")),]
Zdecarb_CZ[which(endsWith(rownames(Zdecarb_CZ), "CZ_i34.1")),] <- shares[which(endsWith(rownames(shares), "CZ_i34.1"))] * Z_CZ[which(endsWith(rownames(Z_CZ), "CZ_i34")),]
Zdecarb_CZ[which(endsWith(rownames(Zdecarb_CZ), "CZ_i34.2")),] <- shares[which(endsWith(rownames(shares), "CZ_i34.2"))] * Z_CZ[which(endsWith(rownames(Z_CZ), "CZ_i34")),]

Zdecarb_CZ[is.na(Zdecarb_CZ)] <- 0


## By columns
## Create temporary Zdecarb_CZ_temp
Zdecarb_CZ_temp <- t(Zdecarb_CZ)

Zdecarb_CZ <- t(Zdecarb_CZ)
Zdecarb_CZ <- as.data.frame(Zdecarb_CZ)

Zdecarb_CZ <- as_tibble(Zdecarb_CZ)
for(i in detailed.1) {
  Zdecarb_CZ <- Zdecarb_CZ %>% 
    tibble::add_row(
      .after = i
    )
}
for(i in detailed.2) {
  Zdecarb_CZ <- Zdecarb_CZ %>% 
    tibble::add_row(
      .after = i
    )
}
rownames(Zdecarb_CZ) <- paste0(IOdecarb_CZ.codes$Country.Code,"_",IOdecarb_CZ.codes$Industry.Code)

## Distribute the right values over the new columns (from external source by dividing the original sectors' production)
## Implement "shares" into Zdecarb_CZ for the new columns (sectors)
Zdecarb_CZ <- as.matrix(Zdecarb_CZ)

## The "shares" are only for CZ, therefore the following code works only for CZ
## This is solved by inserting zeros to the newly created sectors in the RoW
Zdecarb_CZ[which(endsWith(rownames(Zdecarb_CZ), "CZ_i24.a.1")),] <- shares[which(endsWith(rownames(shares), "CZ_i24.a.1"))] * Zdecarb_CZ_temp[which(endsWith(rownames(Zdecarb_CZ_temp), "CZ_i24.a")),]
Zdecarb_CZ[which(endsWith(rownames(Zdecarb_CZ), "CZ_i24.a.2")),] <- shares[which(endsWith(rownames(shares), "CZ_i24.a.2"))] * Zdecarb_CZ_temp[which(endsWith(rownames(Zdecarb_CZ_temp), "CZ_i24.a")),]
Zdecarb_CZ[which(endsWith(rownames(Zdecarb_CZ), "CZ_i24.a.3")),] <- shares[which(endsWith(rownames(shares), "CZ_i24.a.3"))] * Zdecarb_CZ_temp[which(endsWith(rownames(Zdecarb_CZ_temp), "CZ_i24.a")),]
Zdecarb_CZ[which(endsWith(rownames(Zdecarb_CZ), "CZ_i24.b.1")),] <- shares[which(endsWith(rownames(shares), "CZ_i24.b.1"))] * Zdecarb_CZ_temp[which(endsWith(rownames(Zdecarb_CZ_temp), "CZ_i24.b")),]
Zdecarb_CZ[which(endsWith(rownames(Zdecarb_CZ), "CZ_i24.b.2")),] <- shares[which(endsWith(rownames(shares), "CZ_i24.b.2"))] * Zdecarb_CZ_temp[which(endsWith(rownames(Zdecarb_CZ_temp), "CZ_i24.b")),]
Zdecarb_CZ[which(endsWith(rownames(Zdecarb_CZ), "CZ_i24.b.3")),] <- shares[which(endsWith(rownames(shares), "CZ_i24.b.3"))] * Zdecarb_CZ_temp[which(endsWith(rownames(Zdecarb_CZ_temp), "CZ_i24.b")),]
Zdecarb_CZ[which(endsWith(rownames(Zdecarb_CZ), "CZ_i26.d.1")),] <- shares[which(endsWith(rownames(shares), "CZ_i26.d.1"))] * Zdecarb_CZ_temp[which(endsWith(rownames(Zdecarb_CZ_temp), "CZ_i26.d")),]
Zdecarb_CZ[which(endsWith(rownames(Zdecarb_CZ), "CZ_i26.d.2")),] <- shares[which(endsWith(rownames(shares), "CZ_i26.d.2"))] * Zdecarb_CZ_temp[which(endsWith(rownames(Zdecarb_CZ_temp), "CZ_i26.d")),]
Zdecarb_CZ[which(endsWith(rownames(Zdecarb_CZ), "CZ_i26.d.3")),] <- shares[which(endsWith(rownames(shares), "CZ_i26.d.3"))] * Zdecarb_CZ_temp[which(endsWith(rownames(Zdecarb_CZ_temp), "CZ_i26.d")),]
Zdecarb_CZ[which(endsWith(rownames(Zdecarb_CZ), "CZ_i27.a.1")),] <- shares[which(endsWith(rownames(shares), "CZ_i27.a.1"))] * Zdecarb_CZ_temp[which(endsWith(rownames(Zdecarb_CZ_temp), "CZ_i27.a")),]
Zdecarb_CZ[which(endsWith(rownames(Zdecarb_CZ), "CZ_i27.a.2")),] <- shares[which(endsWith(rownames(shares), "CZ_i27.a.2"))] * Zdecarb_CZ_temp[which(endsWith(rownames(Zdecarb_CZ_temp), "CZ_i27.a")),]
Zdecarb_CZ[which(endsWith(rownames(Zdecarb_CZ), "CZ_i27.a.3")),] <- shares[which(endsWith(rownames(shares), "CZ_i27.a.3"))] * Zdecarb_CZ_temp[which(endsWith(rownames(Zdecarb_CZ_temp), "CZ_i27.a")),]
Zdecarb_CZ[which(endsWith(rownames(Zdecarb_CZ), "CZ_i34.1")),] <- shares[which(endsWith(rownames(shares), "CZ_i34.1"))] * Zdecarb_CZ_temp[which(endsWith(rownames(Zdecarb_CZ_temp), "CZ_i34")),]
Zdecarb_CZ[which(endsWith(rownames(Zdecarb_CZ), "CZ_i34.2")),] <- shares[which(endsWith(rownames(shares), "CZ_i34.2"))] * Zdecarb_CZ_temp[which(endsWith(rownames(Zdecarb_CZ_temp), "CZ_i34")),]

Zdecarb_CZ[is.na(Zdecarb_CZ)] <- 0

Zdecarb_CZ <- t(Zdecarb_CZ)


##########################################################################
# 2.7. Export

fwrite(IOdecarb_CZ.codes, file = paste0(intpath,"IOdecarb_CZ.codes.csv"), sep=";", dec=",")
fwrite(x2015decarb_CZ, file = paste0(intpath,"x2015decarb_CZ.csv"), sep=";", dec=",")
fwrite(Ydecarb_CZ, file = paste0(intpath,"Ydecarb_CZ.csv"), sep=";", dec=",")
fwrite(Zdecarb_CZ, file = paste0(intpath,"Zdecarb_CZ.csv"), sep=";", dec=",")



##########################################################################
# 3. Import adjusted opex
##########################################################################

# 1. IMPORT ADJUSTED OPEX FOR EACH YEAR
# 2. DISAGGREGATE A BY COLUMNS FOR EACH YEAR (LEAVE COLUMNS EMPTY - WILL BE FILLED IN WITH THE ADJUSTED OPEX VALUES)
# 3. DISAGGREGATE Ext BY COLUMNS FOR EACH YEAR (LEAVE COLUMNS EMPTY - WILL BE FILLED IN WITH THE ADJUSTED OPEX VALUES)
# 4. INTEGRATE ADJUSTED OPEX SHARES INTO A AND Ext FOR EACH YEAR (TO THE EMPTY COLUMNS)
# 5. DISAGGREGATE ADJUSTED A BY ROWS FOR EACH YEAR, USING (YEAR-SPECIFIC) shares


##########################################################################
# 3.1. Read opex disaggregations and projections

## Read concordance matrices for opex projections
opex_CZ <- list()

for (year in years) {
  opex_CZ[[year]] <- readxl::read_excel(paste0(datapath,"decarb_emp_cz.xlsx"), sheet = paste0("opex_det_",year))
  opex_CZ[[year]] <- as.data.frame(opex_CZ[[year]])
  rownames(opex_CZ[[year]]) <- opex_CZ[[year]]$Industry.Code
}


##########################################################################
# 3.2. Disaggregate A_CZ to Adecarb_CZ by columns before implementing the modeled changes from the imported (disaggregated) file

Adecarb_CZ <- t(A_CZ)
rownames(Adecarb_CZ) <- paste0(IO_CZ.codes$Country.Code,"_",IO_CZ.codes$Industry.Code)

detailed.1 <- sort(cbind(which(endsWith(rownames(Adecarb_CZ),"i24.a")),
                         which(endsWith(rownames(Adecarb_CZ),"i24.b")),
                         which(endsWith(rownames(Adecarb_CZ),"i26.d")),
                         which(endsWith(rownames(Adecarb_CZ),"i27.a")),
                         which(endsWith(rownames(Adecarb_CZ),"i34"))), decreasing = TRUE)
detailed.2 <- sort(cbind(which(endsWith(rownames(Adecarb_CZ),"CZ_i24.a")),
                         which(endsWith(rownames(Adecarb_CZ),"CZ_i24.b"))+1,
                         which(endsWith(rownames(Adecarb_CZ),"CZ_i26.d"))+2,
                         which(endsWith(rownames(Adecarb_CZ),"CZ_i27.a"))+3,
                         which(endsWith(rownames(Adecarb_CZ),"RoW_i24.a"))+5,
                         which(endsWith(rownames(Adecarb_CZ),"RoW_i24.b"))+6,
                         which(endsWith(rownames(Adecarb_CZ),"RoW_i26.d"))+7,
                         which(endsWith(rownames(Adecarb_CZ),"RoW_i27.a"))+8), decreasing = TRUE)
Adecarb_CZ <- as_tibble(Adecarb_CZ)
for(i in detailed.1) {
  Adecarb_CZ <- Adecarb_CZ %>% 
    tibble::add_row(
      .after = i
    )
}
for(i in detailed.2) {
  Adecarb_CZ <- Adecarb_CZ %>% 
    tibble::add_row(
      .after = i
    )
}

## The right values over the new columns will be distributed later after importing the adjusted (disaggregated) opex values
## The adjusted (disaggregated) opex values are only for CZ - zeros inserted to the newly created sectors in the RoW
Adecarb_CZ[is.na(Adecarb_CZ)] <- 0
Adecarb_CZ <- as.data.frame(t(Adecarb_CZ))
colnames(Adecarb_CZ) <- paste0(IOdecarb_CZ.codes$Country.Code,"_",IOdecarb_CZ.codes$Industry.Code)

## Create different Adecarb_CZ for each scenario and each modeled year
## For each scenario because of different shares of each disaggregated sector (as technical coefficients) in 2030 and 2050
A2015decarb_CZ <- Adecarb_CZ
Adecarb_CZ <- list()
for (scenario in scenarios) {
  for (year in years) {
    Adecarb_CZ[[scenario]][[year]] <- A2015decarb_CZ
  }
}


##########################################################################
# 3.3. Disaggregate Ext_CZ_agg to Extdecarb_CZ by columns before implementing the modeled changes from the imported file

Extdecarb_CZ <- t(Ext_CZ_agg)
rownames(Extdecarb_CZ) <- paste0(IO_CZ.codes$Country.Code,"_",IO_CZ.codes$Industry.Code)

detailed.1 <- sort(cbind(which(endsWith(rownames(Extdecarb_CZ),"i24.a")),
                         which(endsWith(rownames(Extdecarb_CZ),"i24.b")),
                         which(endsWith(rownames(Extdecarb_CZ),"i26.d")),
                         which(endsWith(rownames(Extdecarb_CZ),"i27.a")),
                         which(endsWith(rownames(Extdecarb_CZ),"i34"))), decreasing = TRUE)
detailed.2 <- sort(cbind(which(endsWith(rownames(Extdecarb_CZ),"CZ_i24.a")),
                         which(endsWith(rownames(Extdecarb_CZ),"CZ_i24.b"))+1,
                         which(endsWith(rownames(Extdecarb_CZ),"CZ_i26.d"))+2,
                         which(endsWith(rownames(Extdecarb_CZ),"CZ_i27.a"))+3,
                         which(endsWith(rownames(Extdecarb_CZ),"RoW_i24.a"))+5,
                         which(endsWith(rownames(Extdecarb_CZ),"RoW_i24.b"))+6,
                         which(endsWith(rownames(Extdecarb_CZ),"RoW_i26.d"))+7,
                         which(endsWith(rownames(Extdecarb_CZ),"RoW_i27.a"))+8), decreasing = TRUE)
Extdecarb_CZ <- as_tibble(Extdecarb_CZ)
for(i in detailed.1) {
  Extdecarb_CZ <- Extdecarb_CZ %>% 
    tibble::add_row(
      .after = i
    )
}
for(i in detailed.2) {
  Extdecarb_CZ <- Extdecarb_CZ %>% 
    tibble::add_row(
      .after = i
    )
}

## The right values over the new columns will be distributed later after importing the adjusted (disaggregated) opex values
## The adjusted (disaggregated) opex values are only for CZ - zeros insterted to the newly created sectors in the RoW
Extdecarb_CZ[is.na(Extdecarb_CZ)] <- 0
Extdecarb_CZ <- as.data.frame(t(Extdecarb_CZ))
colnames(Extdecarb_CZ) <- paste0(IOdecarb_CZ.codes$Country.Code,"_",IOdecarb_CZ.codes$Industry.Code)

## Create different Extdecarb_CZ for each modeled year
Ext2015decarb_CZ <- Extdecarb_CZ
Extdecarb_CZ <- list()
for (year in years) {
  Extdecarb_CZ[[year]] <- Ext2015decarb_CZ
}


##########################################################################
# 3.4. Import the adjusted (disaggregated by column) opex values to the disaggregated columns of the modeled sectors for each year

## Calculate summary rows and shares of each row's (sector's) input to the summary of CZ & RoW (before disaggregating Adecarb by rows)
Ashare_CZ <- A2015decarb_CZ[,colnames(A2015decarb_CZ) %in% decarb_CZ]
Asum_CZ <- matrix(0, ncol = 21, nrow = 165)
rownames(Asum_CZ) <- unique(IO_CZ.codes$Industry.Code)
colnames(Asum_CZ) <- decarb_CZ
sectors <- c(rownames(Asum_CZ))
for (sector in sectors) {
  # sector <- "i29"
  Asum_CZ[endsWith(rownames(Asum_CZ), sector),] <- colSums(Ashare_CZ[endsWith(rownames(Ashare_CZ), sector),])
  Ashare_CZ[which(endsWith(rownames(Ashare_CZ), sector)),] <- t(t(as.matrix(Ashare_CZ[which(endsWith(rownames(Ashare_CZ), sector)),])) / as.vector(Asum_CZ[which(endsWith(rownames(Asum_CZ), sector)),]))
}
Ashare_CZ[is.nan(Ashare_CZ)] <- 0

## Replace 0 in the newly created sectors with the shares from the original ones to get the proportions of inputs from CZ vs. RoW
Ashare_CZ[,c("CZ_i24.a.2","CZ_i24.a.3")] <- Ashare_CZ[,c("CZ_i24.a.1")]
Ashare_CZ[,c("CZ_i24.b.2","CZ_i24.b.3")] <- Ashare_CZ[,c("CZ_i24.b.1")]
Ashare_CZ[,c("CZ_i26.d.2","CZ_i26.d.3")] <- Ashare_CZ[,c("CZ_i26.d.1")]
Ashare_CZ[,c("CZ_i27.a.2","CZ_i27.a.3")] <- Ashare_CZ[,c("CZ_i27.a.1")]
Ashare_CZ[,c("CZ_i34.2")] <- Ashare_CZ[,c("CZ_i34.1")]

## Insert the adjusted opex values
## Adecarb_CZ
for (scenario in scenarios) {
  for (year in years) {
    for (d in decarb) {
      Adecarb_CZ[[scenario]][[year]][,endsWith(colnames(Adecarb_CZ[[scenario]][[year]]),d)] <- rep(opex_CZ[[year]][1:165,endsWith(colnames(opex_CZ[[year]]),d)], 2) * Ashare_CZ[,paste0("CZ_",d)]
    }
  }
}

## Extdecarb_CZ for the modeled decarb sectors
for (year in years) {
  for (d in decarb) {
    Extdecarb_CZ[[year]][,endsWith(colnames(Extdecarb_CZ[[year]]),d)] <- opex_CZ[[year]][166:180,endsWith(colnames(opex_CZ[[year]]),d)]
  }
}

## Extdecarb_CZ add the other industrial sectors
for (year in years) {
  for (i in industrial) {
    Extdecarb_CZ[[year]][,endsWith(colnames(Extdecarb_CZ[[year]]),i)] <- opex_CZ[[year]][166:180,endsWith(colnames(opex_CZ[[year]]),i)]
  }
}


##########################################################################
# 3.5. Disaggregate Adecarb_CZ by rows

## We use year- and scenario- specific production shares for the detailed (sub)sectors (shares_full)
## to split the technical coefficients accordingly.
## We do this just in case we don't use Adecarbzero_CZ, where the rows are zeroed anyway.

for (scenario in scenarios) {
  for (year in years) {
    detailed.1 <- sort(cbind(which(endsWith(rownames(Adecarb_CZ[[scenario]][[year]]),"i24.a")),
                             which(endsWith(rownames(Adecarb_CZ[[scenario]][[year]]),"i24.b")),
                             which(endsWith(rownames(Adecarb_CZ[[scenario]][[year]]),"i26.d")),
                             which(endsWith(rownames(Adecarb_CZ[[scenario]][[year]]),"i27.a")),
                             which(endsWith(rownames(Adecarb_CZ[[scenario]][[year]]),"i34"))), decreasing = TRUE)
    detailed.2 <- sort(cbind(which(endsWith(rownames(Adecarb_CZ[[scenario]][[year]]),"CZ_i24.a")),
                             which(endsWith(rownames(Adecarb_CZ[[scenario]][[year]]),"CZ_i24.b"))+1,
                             which(endsWith(rownames(Adecarb_CZ[[scenario]][[year]]),"CZ_i26.d"))+2,
                             which(endsWith(rownames(Adecarb_CZ[[scenario]][[year]]),"CZ_i27.a"))+3,
                             which(endsWith(rownames(Adecarb_CZ[[scenario]][[year]]),"RoW_i24.a"))+5,
                             which(endsWith(rownames(Adecarb_CZ[[scenario]][[year]]),"RoW_i24.b"))+6,
                             which(endsWith(rownames(Adecarb_CZ[[scenario]][[year]]),"RoW_i26.d"))+7,
                             which(endsWith(rownames(Adecarb_CZ[[scenario]][[year]]),"RoW_i27.a"))+8), decreasing = TRUE)
    Adecarb_CZ[[scenario]][[year]] <- as_tibble(Adecarb_CZ[[scenario]][[year]])
    for(i in detailed.1) {
      Adecarb_CZ[[scenario]][[year]] <- Adecarb_CZ[[scenario]][[year]] %>%
        tibble::add_row(
          .after = i
        )
    }
    for(i in detailed.2) {
      Adecarb_CZ[[scenario]][[year]] <- Adecarb_CZ[[scenario]][[year]] %>%
        tibble::add_row(
          .after = i
        )
    }
    rownames(Adecarb_CZ[[scenario]][[year]]) <- paste0(IOdecarb_CZ.codes$Country.Code,"_",IOdecarb_CZ.codes$Industry.Code)
    
    ## Distribute the right values over the new rows (from external source by dividing the original sectors' coefficients)
    ## Implement "shares_full" into Adecarb_CZ for the new rows (sectors)
    Adecarb_CZ[[scenario]][[year]] <- as.data.frame(Adecarb_CZ[[scenario]][[year]])
    
    Adecarb_CZ[[scenario]][[year]][which(endsWith(rownames(Adecarb_CZ[[scenario]][[year]]), "CZ_i24.a.1")),] <- shares_full[[scenario]][[year]][which(endsWith(rownames(shares_full[[scenario]][[year]]), "CZ_i24.a.1"))] * A2015decarb_CZ[which(endsWith(rownames(A2015decarb_CZ), "CZ_i24.a")),]
    Adecarb_CZ[[scenario]][[year]][which(endsWith(rownames(Adecarb_CZ[[scenario]][[year]]), "CZ_i24.a.2")),] <- shares_full[[scenario]][[year]][which(endsWith(rownames(shares_full[[scenario]][[year]]), "CZ_i24.a.2"))] * A2015decarb_CZ[which(endsWith(rownames(A2015decarb_CZ), "CZ_i24.a")),]
    Adecarb_CZ[[scenario]][[year]][which(endsWith(rownames(Adecarb_CZ[[scenario]][[year]]), "CZ_i24.a.3")),] <- shares_full[[scenario]][[year]][which(endsWith(rownames(shares_full[[scenario]][[year]]), "CZ_i24.a.3"))] * A2015decarb_CZ[which(endsWith(rownames(A2015decarb_CZ), "CZ_i24.a")),]
    Adecarb_CZ[[scenario]][[year]][which(endsWith(rownames(Adecarb_CZ[[scenario]][[year]]), "CZ_i24.b.1")),] <- shares_full[[scenario]][[year]][which(endsWith(rownames(shares_full[[scenario]][[year]]), "CZ_i24.b.1"))] * A2015decarb_CZ[which(endsWith(rownames(A2015decarb_CZ), "CZ_i24.b")),]
    Adecarb_CZ[[scenario]][[year]][which(endsWith(rownames(Adecarb_CZ[[scenario]][[year]]), "CZ_i24.b.2")),] <- shares_full[[scenario]][[year]][which(endsWith(rownames(shares_full[[scenario]][[year]]), "CZ_i24.b.2"))] * A2015decarb_CZ[which(endsWith(rownames(A2015decarb_CZ), "CZ_i24.b")),]
    Adecarb_CZ[[scenario]][[year]][which(endsWith(rownames(Adecarb_CZ[[scenario]][[year]]), "CZ_i24.b.3")),] <- shares_full[[scenario]][[year]][which(endsWith(rownames(shares_full[[scenario]][[year]]), "CZ_i24.b.3"))] * A2015decarb_CZ[which(endsWith(rownames(A2015decarb_CZ), "CZ_i24.b")),]
    Adecarb_CZ[[scenario]][[year]][which(endsWith(rownames(Adecarb_CZ[[scenario]][[year]]), "CZ_i26.d.1")),] <- shares_full[[scenario]][[year]][which(endsWith(rownames(shares_full[[scenario]][[year]]), "CZ_i26.d.1"))] * A2015decarb_CZ[which(endsWith(rownames(A2015decarb_CZ), "CZ_i26.d")),]
    Adecarb_CZ[[scenario]][[year]][which(endsWith(rownames(Adecarb_CZ[[scenario]][[year]]), "CZ_i26.d.2")),] <- shares_full[[scenario]][[year]][which(endsWith(rownames(shares_full[[scenario]][[year]]), "CZ_i26.d.2"))] * A2015decarb_CZ[which(endsWith(rownames(A2015decarb_CZ), "CZ_i26.d")),]
    Adecarb_CZ[[scenario]][[year]][which(endsWith(rownames(Adecarb_CZ[[scenario]][[year]]), "CZ_i26.d.3")),] <- shares_full[[scenario]][[year]][which(endsWith(rownames(shares_full[[scenario]][[year]]), "CZ_i26.d.3"))] * A2015decarb_CZ[which(endsWith(rownames(A2015decarb_CZ), "CZ_i26.d")),]
    Adecarb_CZ[[scenario]][[year]][which(endsWith(rownames(Adecarb_CZ[[scenario]][[year]]), "CZ_i27.a.1")),] <- shares_full[[scenario]][[year]][which(endsWith(rownames(shares_full[[scenario]][[year]]), "CZ_i27.a.1"))] * A2015decarb_CZ[which(endsWith(rownames(A2015decarb_CZ), "CZ_i27.a")),]
    Adecarb_CZ[[scenario]][[year]][which(endsWith(rownames(Adecarb_CZ[[scenario]][[year]]), "CZ_i27.a.2")),] <- shares_full[[scenario]][[year]][which(endsWith(rownames(shares_full[[scenario]][[year]]), "CZ_i27.a.2"))] * A2015decarb_CZ[which(endsWith(rownames(A2015decarb_CZ), "CZ_i27.a")),]
    Adecarb_CZ[[scenario]][[year]][which(endsWith(rownames(Adecarb_CZ[[scenario]][[year]]), "CZ_i27.a.3")),] <- shares_full[[scenario]][[year]][which(endsWith(rownames(shares_full[[scenario]][[year]]), "CZ_i27.a.3"))] * A2015decarb_CZ[which(endsWith(rownames(A2015decarb_CZ), "CZ_i27.a")),]
    Adecarb_CZ[[scenario]][[year]][which(endsWith(rownames(Adecarb_CZ[[scenario]][[year]]), "CZ_i34.1")),] <- shares_full[[scenario]][[year]][which(endsWith(rownames(shares_full[[scenario]][[year]]), "CZ_i34.1"))] * A2015decarb_CZ[which(endsWith(rownames(A2015decarb_CZ), "CZ_i34")),]
    Adecarb_CZ[[scenario]][[year]][which(endsWith(rownames(Adecarb_CZ[[scenario]][[year]]), "CZ_i34.2")),] <- shares_full[[scenario]][[year]][which(endsWith(rownames(shares_full[[scenario]][[year]]), "CZ_i34.2"))] * A2015decarb_CZ[which(endsWith(rownames(A2015decarb_CZ), "CZ_i34")),]
    
    Adecarb_CZ[[scenario]][[year]][is.na(Adecarb_CZ[[scenario]][[year]])] <- 0
  }
}



##########################################################################
# 4. Run scenarios
##########################################################################

## Define additional sets
decarb_all <- c("i24.a.1","i24.a.2","i24.a.3","i24.a.w","i24.b.1","i24.b.2","i24.b.3","i26.d.1","i26.d.2","i26.d.3","i26.d.w","i27.a.1","i27.a.2","i27.a.3","i27.a.w","i34.1","i34.2","i40.11.a","i40.11.b","i40.11.c","i40.11.d","i40.11.e.1","i40.11.e.2","i40.11.f","i40.11.g","i40.11.h.1","i40.11.h.2","i40.11.i","i40.11.j","i40.11.k")
decarb_all_null <- c("i24.a","i24.a.w","i24.b","i26.d","i26.d.w","i27.a","i27.a.w","i34","i40.11.a","i40.11.b","i40.11.c","i40.11.d","i40.11.e.1","i40.11.e.2","i40.11.f","i40.11.g","i40.11.h.1","i40.11.h.2","i40.11.i","i40.11.j","i40.11.k")
decarb_CZ_all <- c("CZ_i24.a.1","CZ_i24.a.2","CZ_i24.a.3","CZ_i24.a.w","CZ_i24.b.1","CZ_i24.b.2","CZ_i24.b.3","CZ_i26.d.1","CZ_i26.d.2","CZ_i26.d.3","CZ_i26.d.w","CZ_i27.a.1","CZ_i27.a.2","CZ_i27.a.3","CZ_i27.a.w","CZ_i34.1","CZ_i34.2","CZ_i40.11.a","CZ_i40.11.b","CZ_i40.11.c","CZ_i40.11.d","CZ_i40.11.e.1","CZ_i40.11.e.2","CZ_i40.11.f","CZ_i40.11.g","CZ_i40.11.h.1","CZ_i40.11.h.2","CZ_i40.11.i","CZ_i40.11.j","CZ_i40.11.k")
decarb_scen <- IOdecarb_CZ.codes$Index[grepl("i24.a.1|i24.a.2|i24.a.3|i24.a.w|i24.b.1|i24.b.2|i24.b.3|i26.d.1|i26.d.2|i26.d.3|i26.d.w|i27.a.1|i27.a.2|i27.a.3|i27.a.w|i34.1|i34.2|i40.11.a|i40.11.b|i40.11.c|i40.11.d|i40.11.e.1|i40.11.e.2|i40.11.f|i40.11.g|i40.11.h.1|i40.11.h.2|i40.11.i|i40.11.j|i40.11.k", IOdecarb_CZ.codes$Industry.Code) & grepl("CZ", IOdecarb_CZ.codes$Country.Code)]
decarb_scen_null <- IO_CZ.codes$Index[grepl("i24.a|i24.a.w|i24.b|i26.d|i26.d.w|i27.a|i27.a.w|i34|i40.11.a|i40.11.b|i40.11.c|i40.11.d|i40.11.e.1|i40.11.e.2|i40.11.f|i40.11.g|i40.11.h.1|i40.11.h.2|i40.11.i|i40.11.j|i40.11.k", IO_CZ.codes$Industry.Code) & grepl("CZ", IO_CZ.codes$Country.Code)]
# fuel_CZ <- IOdecarb_CZ.codes$Index[grepl("i01.w.2|i10|i23.1|i23.2|i24.d|i40.2|i40.3|i90.1|i90.2|i90.3", IOdecarb_CZ.codes$Industry.Code)&IOdecarb_CZ.codes$Country.Code=="CZ"]


##########################################################################
# 4.1. Insert changes in total output (xdecarb_CZ) for each modeled year based on the scenarios, create xdecarbnull_CZ for indirect effects

## Read scenarios for the electricity and other decarb sectors
scen <- list()

for (scenario in scenarios) {
  scen[[scenario]] <- readxl::read_excel(paste0(datapath, "decarb_emp_cz.xlsx"), sheet = paste0("scen_",scenario))
  scen[[scenario]] <- as.data.frame(scen[[scenario]])
  rownames(scen[[scenario]]) <- paste0(scen[[scenario]]$Country.Code,"_",scen[[scenario]]$Industry.Code)
}

## Insert changes for each scenario and each modeled year into xdecarb_CZ
xdecarb_CZ <- list()

for (scenario in scenarios) {
  for (year in years) {
    xdecarb_CZ[[scenario]][[year]] <- x2015decarb_CZ
    if(year>2015){
      xdecarb_CZ[[scenario]][[year]][decarb_scen] <- scen[[scenario]][,paste0("X",year)]
    }
  }
}

## Create xdecarbnull_CZ
xdecarbnull_CZ <- as.matrix(x2015_CZ)
# xdecarbnull_CZ[decarb_scen_null] <- 0


##########################################################################
# 4.2. Calculate Ldecarb_CZ and L_CZ for indirect effects

## Check memory
gc()

## Calculate Ldecarb_CZ
Ldecarb_CZ <- list()
for (scenario in scenarios) {
  for (year in years) {
    Ldecarb_CZ[[scenario]][[year]] <- solve(diag(nrow(IOdecarb_CZ.codes))-Adecarb_CZ[[scenario]][[year]])
  }
}

# ## OR
# 
# ## Creating separate A and L for each effect or each part (other decarb, electricity, and energy) is not necessary
# ## in the chosen model logic.
# ## We model scenarios where we assume approximate compatibility across all three parts (or ideally know it straight away).
# 
# ## Calcluate Adecarbzero_CZ with all modeled sector's rows in CZ set to zero to avoid changing the diagonal elements
# Adecarbzero_CZ <- list()
# for (scenario in scenarios) {
#   for (year in years) {
#     Adecarbzero_CZ[[scenario]][[year]] <- Adecarb_CZ[[scenario]][[year]]
#     
#     ## Set rows of the modeled decarb sectors to zero
#     Adecarbzero_CZ[[scenario]][[year]][rownames(Adecarbzero_CZ[[scenario]][[year]]) %in% decarb_CZ_all,] <- 0
#     
#     # ## Add fuels
#     # Adecarbzero_CZ[[scenario]][[year]][fuel_CZ,] <- 0
#   }
# }
# 
# ## Check memory
# gc()
# 
# ## Calculate Ldecarbzero_CZ with adapted off-diagonal elements of the modeled sectors
# Ldecarbzero_CZ <- list()
# for (scenario in scenarios) {
#   for (year in years) {
#     Ldecarbzero_CZ[[scenario]][[year]] <- solve(diag(nrow(IOdecarb_CZ.codes))-Adecarbzero_CZ[[scenario]][[year]])
#   }
# }

## Check memory
gc()

# ## Calculate Adecarbnull_CZ and Ldecarbnull_CZ
# Adecarbnull_CZ <- A_CZ
# Adecarbnull_CZ[decarb_scen_null,] <- 0
# 
# Ldecarbnull_CZ <- solve(diag(nrow(IO_CZ.codes))-Adecarbnull_CZ)

## OR
## Calculate L_CZ
L_CZ <- solve(diag(nrow(IO_CZ.codes))-A_CZ)



##########################################################################
# 5. Calculate footprint
##########################################################################

##########################################################################
# 5.1. Prepare Ext (extensions) by extracting indicators we are interested in, prepare Ext.names

Ext <- list()
Ext.names <- list()

for (year in years) {
  
  ## Employment by skill levels and gender, compensation of employees and consumption of capital
  Ext[[year]] <- as.data.frame(t(Extdecarb_CZ[[year]][Q_CZ.codes$Stressor %in% c("Compensation of employees; wages, salaries, & employers' social contributions: Low-skilled",
                                                                                 "Compensation of employees; wages, salaries, & employers' social contributions: Medium-skilled",
                                                                                 "Compensation of employees; wages, salaries, & employers' social contributions: High-skilled",
                                                                                 "Operating surplus: Consumption of fixed capital",
                                                                                 "Employment: Low-skilled male",
                                                                                 "Employment: Low-skilled female",
                                                                                 "Employment: Medium-skilled male",
                                                                                 "Employment: Medium-skilled female",
                                                                                 "Employment: High-skilled male",
                                                                                 "Employment: High-skilled female"),]))
  Ext.names <- as.character(Q_CZ.codes$Stressor[Q_CZ.codes$Stressor %in% c("Compensation of employees; wages, salaries, & employers' social contributions: Low-skilled",
                                                                           "Compensation of employees; wages, salaries, & employers' social contributions: Medium-skilled",
                                                                           "Compensation of employees; wages, salaries, & employers' social contributions: High-skilled",
                                                                           "Operating surplus: Consumption of fixed capital",
                                                                           "Employment: Low-skilled male",
                                                                           "Employment: Low-skilled female",
                                                                           "Employment: Medium-skilled male",
                                                                           "Employment: Medium-skilled female",
                                                                           "Employment: High-skilled male",
                                                                           "Employment: High-skilled female")])
  # Ext[[year]] <- as.data.frame(t(Extdecarb_CZ[[year]][Q_CZ.codes$Compartment=="Employment.persons"
  #                                                     & Q_CZ.codes$Stressor!="Employment: Vulnerable employment",]))
  # Ext.names <- as.character(Q_CZ.codes$Stressor[Q_CZ.codes$Compartment=="Employment.persons"
  #                                               & Q_CZ.codes$Stressor!="Employment: Vulnerable employment"])
  Ext.names <- gsub(": ", "_", Ext.names)
}


##########################################################################
# 5.2. Calculate employment footprint (requirements)

## Define footprint functions
footprint_om <- function(L,Ext,ee,results){
  
  ## Calculate Multiplier Matrix (MP)
  # L <- Ldecarbzero_CZ[[year]]
  # ee <- 1
  # MP <- L * Ext[[year]][,ee]
  MP <- L * Ext[,ee]
  
  ## Calculate Footprint (FP = MP * x). x = Total Output
  # x_data <- xdecarb_CZ[[scenario]][[year]]
  FP <- t(t(MP) * as.vector(x_data))
  FP <- cbind(IOdecarb_CZ.codes, data.frame(Year = rep(year,348), Indicator = rep(Ext.names[ee],348), Effect = rep("O&M",348)), FP)
  FP <- melt(FP, id=c(colnames(IOdecarb_CZ.codes),"Year","Indicator","Effect"), variable.name="Sector", value.name="Value")
  FP$Country.Destination <- substr(FP$Sector, 1, 2)
  FP$Sector <- substr(FP$Sector, 4, 13)
  FP <- FP[FP$Sector %in% decarb_all,]
  
  results <- rbind(results, FP)
}

## Check memory
gc()

## Calculate footprint
## scenario <- "bau"
for (scenario in scenarios) {
  print(paste0(scenario," scenario"))
  
  ## year <- 2015
  for(year in years){
    print(paste0("year ",year))
    
    x_data <- xdecarb_CZ[[scenario]][[year]]
    
    ## Calculate footprints
    ## ee <- 1 (ee = employment extension)
    for(ee in 1:ncol(Ext[[year]])){
      print(paste0("extension ",ee))
      
      ## Ldecarb_CZ is used for all effects/parts (other decarb and electricity) in the chosen model logic,
      ## since we model scenarios where we assume approximate compatibility across all three parts (or ideally know it straight away).
      results <- data.frame()
      results <- footprint_om(L=Ldecarb_CZ[[scenario]][[year]], Ext=Ext[[year]], ee=ee, results=results)
      
      results$Industry.Group <- results$Region.Code <- NULL
      results <- results %>%
        rename(Country.Origin = Country.Code)
      
      data.table::fwrite(results, file = paste0(respath,"/output_data/decarb_emp_cz_detailed/decarb_emp_cz_",scenario,"_",year,"_",ee,".csv"), sep=";", dec=".")
    }
  }
}


##########################################################################
# 5.3. Prepare Extnull (extensions) by extracting indicators we are interested in, prepare Extnull.names

Extdecarbnull_CZ <- as.data.frame(Ext_CZ[c(1:15,24),])

Extnull <- list()
Extnull.names <- list()

## Employment by skill levels and gender, compensation of employees and consumption of capital + CO2 emissions
Extnull <- as.data.frame(t(Extdecarbnull_CZ))
Extnull.names <- as.character(Q_CZ.codes[c(1:15,24),2])
Extnull.names[16] <- "CO2 - combustion - air"
Extnull.names <- gsub(": ", "_", Extnull.names)


##########################################################################
# 5.4. Calculate employment footprint for null (to see indirect employment and CO2 emissions for each modeled sector)

## Define footprint functions
footprint_null <- function(L,Extnull,ee,results_null){
  
  ## Calculate Multiplier Matrix (MP)
  # L <- L_CZ
  # ee <- 16
  # MP <- L * Extnull[,ee]
  MP <- L * Extnull[,ee]
  
  ## Calculate Footprint (FP = MP * x). x = Total Output
  # x_data <- xdecarbnull_CZ
  FP <- t(t(MP) * as.vector(x_data))
  FP <- cbind(IO_CZ.codes, data.frame(Year = rep("2015",330), Indicator = rep(Extnull.names[ee],330), Effect = rep("O&M",330)), FP)
  FP <- melt(FP, id=c(colnames(IO_CZ.codes),"Year","Indicator","Effect"), variable.name="Sector", value.name="Value")
  FP$Country.Destination <- substr(FP$Sector, 1, 2)
  FP$Sector <- substr(FP$Sector, 4, 13)
  FP <- FP[FP$Sector %in% decarb_all_null,]
  
  results_null <- rbind(results_null, FP)
}

## Check memory
gc()

## Calculate footprint
x_data <- xdecarbnull_CZ

## ee <- 16 (ee = extension)
for(ee in 1:ncol(Extnull)){
  print(paste0("extension ",ee))
  
  results_null <- data.frame()
  results_null <- footprint_null(L=L_CZ, Extnull=Extnull, ee=ee, results_null=results_null)
  
  results_null$Industry.Group <- results_null$Region.Code <- NULL
  results_null <- results_null %>%
    rename(Country.Origin = Country.Code)
  
  data.table::fwrite(results_null, file = paste0(respath,"/output_data/decarb_emp_cz_detailed/decarb_emp_cz_null_",ee,".csv"), sep=";", dec=".")
}


##########################################################################
# 5.5. Export CO2 emissions accounts from E_CZ

E_CZ_CO2 <- as.data.frame(t(E_CZ[24,]))
rownames(E_CZ_CO2) <- "CO2 - combustion - air"
fwrite(E_CZ_CO2, file = paste0(intpath,"/E_CZ_CO2.csv"), sep=";", dec=",")



##########################################################################
# 6. Organize results
##########################################################################

## RESULTS

## 1) EMP:
## a) Total
## b) By skill levels and gender
## c) By sectors
## d) By spheres (manufacturing, services, budgetary)
## e) By each modeled sector

## 2) LAB vs. CAP
## a) Share of compensation of employees vs. share of capital consumption

## Domestic: Country.Origin = CZ

## Note that we can only calculate "pure" domestic = where Country.Destination matches Country.Origin.
## The results thus omit effects related to Czechia's electricity industries that are induced outside CZ (Country.Destination = RoW).
## However, the technology mix outside CZ does not change in the model, so this employment would remain constant over time anyway.

# ## Define EUR to CZK average exchange rate for 2015
# exchange <- 27.283 ## 1 EUR = 27.283 CZK, source: https://www.kurzy.cz/kurzy-men/historie/rok-2015/


##########################################################################
# 6.1. Load results and separate employment in numbers of persons from "LabCap" (compensation of employees vs. consumption of capital)

## Load results
results <- data.frame()

for (scenario in scenarios) {
  print(paste0(scenario," scenario"))
  
  for (year in years) {
    print(paste0("year ",year))
    
    for (ee in 1:ncol(Ext[[year]])) {
      print(paste0("extension ",ee))
      
      ## Read data and remove NAs
      data <- read.csv(paste0(respath,"/output_data/decarb_emp_cz_detailed/decarb_emp_cz_",scenario,"_",year,"_",ee,".csv"), header=TRUE, sep=";")
      data[is.na(data)] <- 0
      
      ## Introduce "Scenario" column, filter only CZ results
      data$Scenario <- c(rep(scenario))
      data <- data %>% 
        filter(Country.Destination == "CZ")
      
      ## rbind to get results for each scenario, year and extension category below each other
      results <- bind_rows(results, data)
      
      ## Export results
      fwrite(results, file = paste0(respath,"/output_data/decarb_emp_cz.csv"), sep=";", dec=",")
    }
  }
}
