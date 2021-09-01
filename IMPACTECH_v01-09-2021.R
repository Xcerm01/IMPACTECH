##########################################################################
# IMPACTECH model
# Indicators: Employment, GDP, GVA and GHG effects of changes in the energy mix in electricity production and other fuels
# Modules: GFCF effects, expert projections of selected technological and socioeconomic parameters for modelled renewable energy sources
# Scope: CZ + aggregated Rest of World region (MRIO with 2 regions)
# Scenarios and expert projections: defined by the user
##########################################################################


##########################################################################
# 0. Define conditions, install packages and libraries, set path and load data
##########################################################################

##########################################################################
# 0.0. Define conditions, install packages and libraries

## Basic user options - 1 = yes, 0 = no:
install_libraries <- 0
p1 <- 0 # Does the user specify the expert projections (CAPEX and OPEX)?
p2 <- 0 # Does the user model the whole energy sector?
p3 <- 0 # Not yet implemented - Does the user specify inputs in percentage values (0 is for input in native values)?
p4 <- 0 # Not yet implemented - Save model outputs?

if (install_libraries==1) {

install.packages("readxl")
install.packages("tidyverse")
install.packages("tidyr")
install.packages("reshape2")
install.packages("data.table")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("stringr")
install.packages("backports")
install.packages("gridExtra")
install.packages("zoo")
install.packages("ggsci")
install.packages("ggthemes")
install.packages("viridis")
install.packages("shiny")
install.packages("rhandsontable")
install.packages("shinyjs")

}

library(readxl)
library(tidyverse)
library(dplyr)
library(tidyr)
library(data.table)
library(reshape2)
library(ggplot2)
library(stringr)
library(tibble)
library(gridExtra)
library(zoo)
library(ggsci)
library(ggthemes)
library(viridis)
library(shiny)
library(rhandsontable)
library(shinyjs)


##########################################################################
# 0.1. Set datapath (for loading inputs) and respath (for saving results)

## Definice cest; pozor, je potøeba psát opaènì lomítka, než je kopie cesty z prùzkumníka
## CHANGE ACCORDING TO THE USER'S DEPOSITORY

# example: datapath = "C:/Users/Cerny/Documents/Work/Projects/2018-2021 TAÈR Théta IMPACTECH/WP6 Model a web/Model/Data/"
# example: respath <- "C:/Users/Cerny/Documents/Work/Projects/2018-2021 TAÈR Théta IMPACTECH/WP6 Model a web/Model/Results/"


##########################################################################
# 0.2. Load data adjusted from EXIOBASE 3.6 and other objects necessary to run the scenarios and expert projections

## Load adjusted EXIOBASE 3.6 objects
load(paste0(datapath,"IO_CZ.codes.RData"))
load(paste0(datapath,"YGFCF_CZ.codes.RData"))
load(paste0(datapath,"Q_CZ.codes.RData"))
load(paste0(datapath,"YGFCF2015_CZ.RData"))
load(paste0(datapath,"Ext_CZ.RData"))
load(paste0(datapath,"A_CZ.RData"))
load(paste0(datapath,"x2015_CZ.RData"))
load(paste0(datapath,"x2015el_CZ.RData"))
load(paste0(datapath,"x2015el_repl.RData"))
load(paste0(datapath,"x2015fuel_CZ.RData"))
load(paste0(datapath,"EUSTi24.d_CZ.RData"))
load(paste0(datapath,"EUSTi40.2_CZ.RData"))

## Adjust
Ext_CZ <- as.matrix(Ext_CZ)
x2015_CZ <- x2015_CZ$x2015_CZ

## Load adjusted objects for integrating scenarios and expert projections
load(paste0(datapath,"concordance_opex.RData"))
load(paste0(datapath,"concordance_capex.RData"))
load(paste0(datapath,"GFCF_el.RData"))
load(paste0(datapath,"GFCF_CZ.RData"))
load(paste0(datapath,"lifespan_CZ.RData"))
load(paste0(datapath,"capex_CZ.RData"))
load(paste0(datapath,"opex_CZ.RData"))
load(paste0(datapath,"emp_intensity.RData"))
load(paste0(datapath,"capex_opex.RData"))
load(paste0(datapath,"concordance_el.RData"))
load(paste0(datapath,"concordance_fuel.RData"))
load(paste0(datapath,"CZ_RES_opex.RData"))
load(paste0(datapath,"inst_CZ.RData"))
load(paste0(datapath,"replacements_CZ.RData"))
load(paste0(datapath,"gen_CZ.RData"))
load(paste0(datapath,"cap_CZ.RData"))
load(paste0(datapath,"en_CZ.RData"))
load(paste0(datapath,"industries.RData"))


##########################################################################
# 0.3. Define sets and other necessary objects

years = c("2015","2020","2025","2030","2035","2040","2045","2050")  ## years considered in the analysis
extensions <- c("emp","gva","gdp","ghg") ## extensions (employment, gross value added, gross domestic product, greenhouse gas emissions)
electricity <- c("a","b","c","d","e.1","e.2","f","g","h.1","h.2","i","j","k") ## modelled electricity sectors
electricity_CZ <- IO_CZ.codes$Index[grepl("Production of electricity by", IO_CZ.codes$Industry.Name) & grepl("CZ", IO_CZ.codes$Country.Code)]
fuel_CZ <- IO_CZ.codes$Index[grepl("i01.w.2|i10|i23.1|i23.2|i24.d|i40.2|i40.3|i90.1|i90.2|i90.3", IO_CZ.codes$Industry.Code)&IO_CZ.codes$Country.Code=="CZ"]
res <- c("e.1","g","h.1","h.2") ## modelled renewable energy sources that are subject to expert projections
res_names <- data.frame(name = c("Wind Onshore","Bioenergy","Solar PV Utility","Solar PV Residential"),
                        code = c("i40.11.e.1","i40.11.g","i40.11.h.1","i40.11.h.2")) ## "concordance" of the modelled renewable energy sources names and their EXIOBASE codes
sources <- data.frame(cz = c("Vìtrná energie (onshore)",
                             "Biomasa, bioplyn a odpad",
                             "Velkoplošná fotovoltaika (>1MW)",
                             "Maloplošná fotovoltaika (<1MW)"),
                      en = c("Wind Onshore",
                             "Bioenergy",
                             "Solar PV Utility",
                             "Solar PV Residential")) ## "concordance" between Czech and English names of the modelled energy sources
categories <- data.frame(cz = c("Stavební a konstrukèní náklady","Náklady na dodávku a instalaci mechanického zaøízení",
                                "Dodávka a instalace elektrické a pøístrojové techniky a øízení","Nepøímé náklady projektu",
                                "Náklady vlastníka","Údržba: Servis a náhradní díly",
                                "Pravidelná údržba, opravy (stavební práce) a další rùzné náklady","Administrativní náklady",
                                "Pronájem pozemkù","Náklady na pojištìní","Náklady na napájení ze sítì","Vstupní suroviny","Práce",
                                "Zakoupený materiál","Skladování","Pøeprava","Údržba","Ostatní","Rùzné","Odpisy",
                                "Náklady na údržbu fotovoltaického systému","Náhradní díly","Pronájem pozemkù","Náklady na pojištìní",
                                "Náklady na zabezpeèení a údržbu lokality","Spotøební náklady (vèetnì nakoupené elektøiny)","Støídaè",
                                "Èištìní/vegetace","Fotovoltaické pole","Elektroinstalace","Dokumenty","Fotovoltaický modul",
                                "Zapojení stejnosmìrného proudu","Mechanika","Správa majetku","Zapojení støídavého proudu"),
                         en = c("Civil and structural costs","Mechanical equipment supply and installation costs",
                                "Electrical and I&C supply and installation","Project indirect costs","Owner's costs",
                                "Maintenance: Service and spare parts",
                                "Regular maintenance, repair (civil work) and other miscellaneous costs","Administration costs",
                                "Land rent","Insurance costs","Power from the grid costs","Feedstock","Labour","Purchased materials",
                                "Storage","Transport","Maintenance","Other","Miscellaneous","Depreciation","PV system maintenance costs",
                                "Spares","Land lease","Insurance","Site security and administration costs",
                                "Utilities (including purchased electricity)","Inverter","Cleaning/Veg","PV Array","Electrical",
                                "Documents","PV Module","DC Wiring","Mechanical","Asset Management",
                                "AC Wiring")) ## "concordance" between Czech and English names of the capex and opex categories
fuels <- data.frame(cz = c("Tuhá paliva",
                           "Ropa",
                           "Plyn",
                           "Teplo",
                           "Obnovitelné zdroje"),
                    en = c("Solids",
                           "Oil",
                           "Gas",
                           "Heat",
                           "Renewable energy forms")) ## "concordance" between Czech and English names of the fuels
countries <- c("CZ") ## modelled countries considered in the analysis
countries_all <- c("CZ","RoW") ## all countries considered in the analysis (RoW = Rest of World, aggregated countries and regions outside CZ)

## Define EUR to CZK average exchange rate for 2015 to give results in CZK
exchange <- 27.283 ## 1 EUR = 27.283 CZK, source: https://www.kurzy.cz/kurzy-men/historie/rok-2015/


##########################################################################
# 0.4. Load functions

## is.nan
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

## !in
`%!in%` = Negate(`%in%`)



##########################################################################
# 1. Read scenario of the electricity sector given by the user
##########################################################################

##########################################################################
# 1.1. Read and adjust scenario in percents

## RUN THIS PART (1.1.) ONLY IF WE LOAD THE SCENARIO IN PERCENTS
if (p3==1) {

## Read scenario in percents for the production of electricity by each source
scen_el_gen <- readxl::read_excel(paste0(datapath,"IMPACTECH_vstupy.xlsx"), sheet = "elektro_produkce_procenta")
scen_el_gen <- scen_el_gen[!grepl("Kontrolní",scen_el_gen$'Produkce elektøiny z daného zdroje (%)'),]
scen_el_gen <- as.data.frame(scen_el_gen)
for(i in c(2:9)){
  scen_el_gen[!grepl("celkem",scen_el_gen$'Produkce elektøiny z daného zdroje (%)'),i] <-
    scen_el_gen[!grepl("celkem",scen_el_gen$'Produkce elektøiny z daného zdroje (%)'),i] * scen_el_gen[grepl("celkem",scen_el_gen$'Produkce elektøiny z daného zdroje (%)'),i]
}
scen_el_gen <- scen_el_gen[!grepl("celkem",scen_el_gen$'Produkce elektøiny z daného zdroje (%)'),]

## Calculate relative change compared to 2015 values and replace NaN values with zeros
for (year in years) {
  scen_el_gen[,paste0("Change.",year)] <- scen_el_gen[,year] / scen_el_gen[,"2015"]
}
scen_el_gen[is.nan(scen_el_gen)] <- 0
scen_el_gen[,startsWith(colnames(scen_el_gen),"20")] <- NULL
colnames(scen_el_gen)[startsWith(colnames(scen_el_gen),"Change")] <- years

## Multiply the values in scen_el_gen with gen_CZ (baseline values in GWh) to get the absolute values
scen_el_gen[,startsWith(colnames(scen_el_gen),"20")] <- scen_el_gen[,startsWith(colnames(scen_el_gen),"20")] * gen_CZ$gen2015

## Read scenario in percents for the installed capacity of each electricity source
scen_el_cap <- readxl::read_excel(paste0(datapath,"IMPACTECH_vstupy.xlsx"), sheet = "elektro_kapacita_procenta")
scen_el_cap <- scen_el_cap[!grepl("Kontrolní",scen_el_cap$'Instalovaná kapacita daného zdroje (%)'),]
scen_el_cap <- as.data.frame(scen_el_cap)
for(i in c(2:9)){
  scen_el_cap[!grepl("celkem",scen_el_cap$'Instalovaná kapacita daného zdroje (%)'),i] <-
    scen_el_cap[!grepl("celkem",scen_el_cap$'Instalovaná kapacita daného zdroje (%)'),i] * scen_el_cap[grepl("celkem",scen_el_cap$'Instalovaná kapacita daného zdroje (%)'),i]
}
scen_el_cap <- scen_el_cap[!grepl("celkem",scen_el_cap$'Instalovaná kapacita daného zdroje (%)'),]

## Calculate relative change compared to 2015 values and replace NaN values with zeros
for (year in years) {
  scen_el_cap[,paste0("Change.",year)] <- scen_el_cap[,year] / scen_el_cap[,"2015"]
}
scen_el_cap[is.nan(scen_el_cap)] <- 0
scen_el_cap[,startsWith(colnames(scen_el_cap),"20")] <- NULL
colnames(scen_el_cap)[startsWith(colnames(scen_el_cap),"Change")] <- years

## Multiply the values in scen_el_cap with cap_CZ (baseline values in MW) to get the absolute values
scen_el_cap[,startsWith(colnames(scen_el_cap),"20")] <- scen_el_cap[,startsWith(colnames(scen_el_cap),"20")] * cap_CZ$cap2015

}


##########################################################################
# 1.2. Read scenario in absolute values

## RUN THIS PART (1.2.) ONLY IF WE LOAD THE SCENARIO IN ABSOLUTE VALUES
if (p3==0) {

## Read scenario in percents for the production of electricity by each source
scen_el_gen <- readxl::read_excel(paste0(datapath,"IMPACTECH_vstupy.xlsx"), sheet = "elektro_produkce_hodnoty")
scen_el_gen <- scen_el_gen[!grepl("Kontrolní",scen_el_gen$'Produkce elektøiny z daného zdroje (GWh)'),]
scen_el_gen <- as.data.frame(scen_el_gen)

## Read scenario in percents for the installed capacity of each electricity source
scen_el_cap <- readxl::read_excel(paste0(datapath,"IMPACTECH_vstupy.xlsx"), sheet = "elektro_kapacita_hodnoty")
scen_el_cap <- scen_el_cap[!grepl("Kontrolní",scen_el_cap$'Instalovaná kapacita daného zdroje (MW)'),]
scen_el_cap <- as.data.frame(scen_el_cap)

}


##########################################################################
# 2. Read scenario of the whole energy sector given by the user
##########################################################################

## RUN THIS PART (2.) ONLY IF WE MODEL THE WHOLE ENERGY SECTOR
if (p2==1) {

##########################################################################
# 2.1. Read scenario in percents

## RUN THIS PART (2.1.) ONLY IF WE LOAD THE SCENARIO IN PERCENTS
if (p3==1) {

## Read scenario in percents for the production of each of the other fuels
scen_en <- readxl::read_excel(paste0(datapath,"IMPACTECH_vstupy.xlsx"), sheet = "paliva_procenta")
scen_en <- scen_en[!grepl("Kontrolní",scen_en$'Produkce dle zdroje energie mimo elektøinu (%)'),]
scen_en <- as.data.frame(scen_en)
for(i in c(2:9)){
  scen_en[!grepl("celkem",scen_en$'Produkce dle zdroje energie mimo elektøinu (%)'),i] <-
    scen_en[!grepl("celkem",scen_en$'Produkce dle zdroje energie mimo elektøinu (%)'),i] * scen_en[grepl("celkem",scen_en$'Produkce dle zdroje energie mimo elektøinu (%)'),i]
}
scen_en <- scen_en[!grepl("celkem",scen_en$'Produkce dle zdroje energie mimo elektøinu (%)'),]

## Calculate relative change compared to 2015 values and replace NaN values with zeros
for (year in years) {
  scen_en[,paste0("Change.",year)] <- scen_en[,year] / scen_en[,"2015"]
}
scen_en[is.nan(scen_en)] <- 0
scen_en[,startsWith(colnames(scen_en),"20")] <- NULL
colnames(scen_en)[startsWith(colnames(scen_en),"Change")] <- years

## Multiply the values in scen_en with en_CZ (baseline values in ktoe) to get the absolute values
scen_en[,startsWith(colnames(scen_en),"20")] <- scen_en[,startsWith(colnames(scen_en),"20")] * en_CZ$'2015'

} # End of condition p3


##########################################################################
# 2.2. Read scenario in absolute values

## RUN THIS PART (2.2.) ONLY IF WE LOAD THE SCENARIO IN ABSOLUTE VALUES
if (p3==0) {

## Read scenario in percents for the production of each of the other fuels
scen_en <- readxl::read_excel(paste0(datapath,"IMPACTECH_vstupy.xlsx"), sheet = "paliva_hodnoty")
scen_en <- as.data.frame(scen_en)

} # End of condition p3
} # End of condition p2


##########################################################################
# 3. Read expert projections given by the user and adjust for further calculations
##########################################################################

## RUN THIS PART (3.) ONLY IF WE LOAD THE EXPERT PROJECTIONS
if (p1==1) {

##########################################################################
# 3.1. Read and adjust expert projections

## Read lifespan projections
projection_lifespan <- readxl::read_excel(paste0(datapath,"IMPACTECH_vstupy.xlsx"), sheet = "projekce_zivotnost")
projection_lifespan <- as.data.frame(projection_lifespan)
rownames(projection_lifespan) <- paste0("i40.11.",res)
projection_lifespan <- projection_lifespan[,startsWith(colnames(projection_lifespan),"20")]

## Read capex : opex projections and interpolate between 2015, 2030 and 2050
projection_capex_opex <- readxl::read_excel(paste0(datapath,"IMPACTECH_vstupy.xlsx"), sheet = "projekce_capex_opex")
projection_capex_opex <- as.data.frame(projection_capex_opex[,!grepl("Kategorie",colnames(projection_capex_opex))])
projection_capex_opex <- projection_capex_opex[!grepl("Celkem",projection_capex_opex$Položka),]
projection_capex_opex[,c("2020","2025","2035","2040","2045")] <- 0
projection_capex_opex <- projection_capex_opex[,c("Zdroj energie","Položka","2015","2020","2025","2030","2035","2040","2045","2050")]
projection_capex_opex$`2020` <- projection_capex_opex$`2015` + (projection_capex_opex$`2030` - projection_capex_opex$`2015`) / 3
projection_capex_opex$`2025` <- projection_capex_opex$`2015` + 2 * (projection_capex_opex$`2030` - projection_capex_opex$`2015`) / 3
projection_capex_opex$`2035` <- projection_capex_opex$`2030` + (projection_capex_opex$`2050` - projection_capex_opex$`2030`) / 4
projection_capex_opex$`2040` <- projection_capex_opex$`2030` + 2 * (projection_capex_opex$`2050` - projection_capex_opex$`2030`) / 4
projection_capex_opex$`2045` <- projection_capex_opex$`2030` + 3 * (projection_capex_opex$`2050` - projection_capex_opex$`2030`) / 4

## Read employment intesity projections and interpolate between 2015, 2030 and 2050
projection_emp_intensity <- readxl::read_excel(paste0(datapath,"IMPACTECH_vstupy.xlsx"), sheet = "projekce_prace")
projection_emp_intensity <- as.data.frame(projection_emp_intensity[,!grepl("Kategorie",colnames(projection_emp_intensity))])
projection_emp_intensity[,c("2020","2025","2035","2040","2045")] <- 0
projection_emp_intensity <- projection_emp_intensity[,c("Zdroj energie","Položka","2015","2020","2025","2030","2035","2040","2045","2050")]
projection_emp_intensity$`2020` <- projection_emp_intensity$`2015` + (projection_emp_intensity$`2030` - projection_emp_intensity$`2015`) / 3
projection_emp_intensity$`2025` <- projection_emp_intensity$`2015` + 2 * (projection_emp_intensity$`2030` - projection_emp_intensity$`2015`) / 3
projection_emp_intensity$`2035` <- projection_emp_intensity$`2030` + (projection_emp_intensity$`2050` - projection_emp_intensity$`2030`) / 4
projection_emp_intensity$`2040` <- projection_emp_intensity$`2030` + 2 * (projection_emp_intensity$`2050` - projection_emp_intensity$`2030`) / 4
projection_emp_intensity$`2045` <- projection_emp_intensity$`2030` + 3 * (projection_emp_intensity$`2050` - projection_emp_intensity$`2030`) / 4

## Read capex shares projections, interpolate between 2015, 2030 and 2050, add Source_Category column to match projections with the concordance
projection_capex <- readxl::read_excel(paste0(datapath,"IMPACTECH_vstupy.xlsx"), sheet = "projekce_capex")
projection_capex <- as.data.frame(projection_capex[,!grepl("Kategorie",colnames(projection_capex))])
projection_capex <- projection_capex[!grepl("Celkem",projection_capex$Položka),]
projection_capex[,c("2020","2025","2035","2040","2045")] <- 0
projection_capex <- projection_capex[,c("Zdroj energie","Položka","2015","2020","2025","2030","2035","2040","2045","2050")]
projection_capex$`2020` <- projection_capex$`2015` + (projection_capex$`2030` - projection_capex$`2015`) / 3
projection_capex$`2025` <- projection_capex$`2015` + 2 * (projection_capex$`2030` - projection_capex$`2015`) / 3
projection_capex$`2035` <- projection_capex$`2030` + (projection_capex$`2050` - projection_capex$`2030`) / 4
projection_capex$`2040` <- projection_capex$`2030` + 2 * (projection_capex$`2050` - projection_capex$`2030`) / 4
projection_capex$`2045` <- projection_capex$`2030` + 3 * (projection_capex$`2050` - projection_capex$`2030`) / 4
projection_capex$Source_Category <- paste0(sources$en[match(projection_capex$`Zdroj energie`, sources$cz)],
                                           "_",categories$en[match(projection_capex$`Položka`, categories$cz)])

## Read opex shares projections, interpolate between 2015, 2030 and 2050, add Source_Category column to match projections with the concordance
## Insert row for bioenergy depreciation (Operating surplus: Consumption of fixed capital) and fill it with default values from opex_CZ
projection_opex <- readxl::read_excel(paste0(datapath,"IMPACTECH_vstupy.xlsx"), sheet = "projekce_opex")
projection_opex <- as.data.frame(projection_opex[,!grepl("Kategorie",colnames(projection_opex))])
projection_opex <- projection_opex[!grepl("Celkem",projection_opex$Položka),]
projection_opex[,c("2020","2025","2035","2040","2045")] <- 0
projection_opex <- projection_opex[,c("Zdroj energie","Položka","2015","2020","2025","2030","2035","2040","2045","2050")]
projection_opex$`2020` <- projection_opex$`2015` + (projection_opex$`2030` - projection_opex$`2015`) / 3
projection_opex$`2025` <- projection_opex$`2015` + 2 * (projection_opex$`2030` - projection_opex$`2015`) / 3
projection_opex$`2035` <- projection_opex$`2030` + (projection_opex$`2050` - projection_opex$`2030`) / 4
projection_opex$`2040` <- projection_opex$`2030` + 2 * (projection_opex$`2050` - projection_opex$`2030`) / 4
projection_opex$`2045` <- projection_opex$`2030` + 3 * (projection_opex$`2050` - projection_opex$`2030`) / 4
projection_opex <- projection_opex %>% 
  tibble::add_row(
    `Zdroj energie` = "Biomasa, bioplyn a odpad",
    Položka = "Odpisy",
    .after = 14)
projection_opex[grepl("Odpisy",projection_opex$Položka),startsWith(colnames(projection_opex),"20")] <-
  opex_CZ[grepl("Depreciation",opex_CZ$Category),startsWith(colnames(opex_CZ),"20")]
projection_opex$Source_Category <- paste0(sources$en[match(projection_opex$`Zdroj energie`, sources$cz)],
                                          "_",categories$en[match(projection_opex$`Položka`, categories$cz)])


##########################################################################
# 3.2. Translate lifespan from the expert projections input file

## If no expert projections are given, the code runs further below with default values from the original lifespan_CZ
## Lifespan also influences directly the GFCF part of the scenarios (GFCF_CZ)

for (i in res) {
  lifespan_CZ[grepl(paste0("i40.11.",i),rownames(lifespan_CZ)),] <- projection_lifespan[grepl(paste0("i40.11.",i),rownames(projection_lifespan)),]
}


##########################################################################
# 3.3. Translate capex : opex shares from the expert projections input file

capex_opex[,startsWith(colnames(capex_opex),"Share")] <- projection_capex_opex[,startsWith(colnames(projection_capex_opex),"20")]


##########################################################################
# 3.4. Translate employment intensity from the expert projections input file

emp_intensity[,startsWith(colnames(emp_intensity),"20")] <- projection_emp_intensity[,startsWith(colnames(projection_emp_intensity),"20")]


##########################################################################
# 3.5. Translate capex shares from the expert projections input file

capex_CZ$Source_Category <- paste0(capex_CZ$Source,"_",capex_CZ$Category)
capex_CZ[,c(4:11)] <- projection_capex[match(capex_CZ$Source_Category,projection_capex$Source_Category),startsWith(colnames(projection_capex),"20")]
capex_CZ <- merge(capex_CZ, as.data.table(capex_CZ)[, .N, by = c("Source_Category")], by = "Source_Category")
capex_CZ[, grepl("20", colnames(capex_CZ))] <- capex_CZ[, grepl("20", colnames(capex_CZ))] / capex_CZ$N


##########################################################################
# 3.6. Translate opex shares from the expert projections input file

## Adjust values for compensation of employees with the original shares for each compensation of employees category from CZ_RES_opex
shares <- CZ_RES_opex[startsWith(CZ_RES_opex$Industry.Name,"Compensation"),]
shares[,startsWith(colnames(shares),"i40.11")] <- t(t(shares[,startsWith(colnames(shares),"i40.11")])/colSums(shares[,startsWith(colnames(shares),"i40.11")]))
shares[,startsWith(colnames(shares),"i40.11")] <- 1 / shares[,startsWith(colnames(shares),"i40.11")]
shares <- melt(shares, variable.name = "Source", value.name = "Share")
shares$Source <- res_names$name[match(shares$Source, res_names$code)]

## Translate shares from projections_CZ to opex_CZ
opex_CZ$Source_Category <- paste0(opex_CZ$Source,"_",opex_CZ$Category)
opex_CZ[,c(4:11)] <- projections_CZ[match(opex_CZ$Source_Category,projections_CZ$Source_Category),startsWith(colnames(projections_CZ),"20")]
opex_CZ <- merge(opex_CZ, as.data.table(opex_CZ)[, .N, by = c("Source_Category")], by = "Source_Category")

## Rewrite N for compensation of employees with values from shares
opex_CZ$N[startsWith(opex_CZ$Industry.Name,"Compensation")] <- shares$Share
opex_CZ[, grepl("20", colnames(opex_CZ))] <- opex_CZ[, grepl("20", colnames(opex_CZ))] / opex_CZ$N


##########################################################################
# 3.7. Compatibilize adjusted capex shares with the IO structure of the model

## Adjust capex_CZ
capex_CZ$Category <- NULL
capex_CZ <- melt(capex_CZ, variable.name="Year", value.name="Share")
capex_CZ$Source.code <- res_names$code[match(capex_CZ$Source, res_names$name)]

## Implement values from capex_CZ into concordance_capex
concordance2015_capex <- concordance_capex
concordance_capex <- list()

for (year in years) {
  temp <- capex_CZ[capex_CZ$Year==year, -1] %>%
    group_by(Industry.Name, Year, Source.code) %>%
    summarise(Share = sum(Share, na.rm = T)) %>%
    spread(Source.code, Share)

  concordance2015_capex$i40.11.e.1 <- temp$i40.11.e.1[match(concordance2015_capex$Industry.Name, temp$Industry.Name)]
  concordance2015_capex$i40.11.g <- temp$i40.11.g[match(concordance2015_capex$Industry.Name, temp$Industry.Name)]
  concordance2015_capex$i40.11.h.1 <- temp$i40.11.h.1[match(concordance2015_capex$Industry.Name, temp$Industry.Name)]
  concordance2015_capex$i40.11.h.2 <- temp$i40.11.h.2[match(concordance2015_capex$Industry.Name, temp$Industry.Name)]

  concordance2015_capex[is.na(concordance2015_capex)] <- 0
  concordance_capex[[year]] <- concordance2015_capex
}


##########################################################################
# 3.8. Compatibilize adjusted opex shares and other variables influencing opex shares with the IO structure of the model

## Adjust lifespan_CZ into lifespan_surplus
## (lifespan influences Operating surplus: Remaining net operating surplus - the longer the lifespan, the higher the surplus)
lifespan_surplus <- lifespan_CZ[grepl("i40.11.e.1|i40.11.g|i40.11.h",rownames(lifespan_CZ)),]
for (year in years) {
  colnames(lifespan_surplus)[startsWith(colnames(lifespan_surplus),year)] <- c(paste0("Lifespan.",year))
  lifespan_surplus[,year] <- lifespan_surplus[,paste0("Lifespan.",year)] / lifespan_surplus[,"Lifespan.2015"]
}
lifespan_surplus <- lifespan_surplus[,!startsWith(colnames(lifespan_surplus),"Lifespan")]
lifespan_surplus$Source.code <- res_names$code
lifespan_surplus$Industry.Name <- c(rep("Operating surplus: Remaining net operating surplus"))
lifespan_surplus <- melt(lifespan_surplus, variable.name="Year", value.name="Change")

## Adjust capex_opex
## (capex : opex - the share of capex to the sum of capex and opex - influences consumption of fixed capital)
capex_opex <- capex_opex[!grepl("Opex",capex_opex$Category),]
for (year in years) {
  capex_opex[,year] <- capex_opex[,paste0("Share.",year)] / capex_opex[,"Share.2015"]
}
capex_opex <- capex_opex[,!startsWith(colnames(capex_opex),"Share")]
capex_opex <- melt(capex_opex, variable.name="Year", value.name="Change")
capex_opex$Source.code <- res_names$code[match(capex_opex$Source, res_names$name)]
names(capex_opex)[names(capex_opex) == "Category"] <- "Industry.Name"
capex_opex[capex_opex$Industry.Name == "Capex",colnames(capex_opex) == "Industry.Name"] <- "Operating surplus: Consumption of fixed capital"

## Adjust opex_CZ
opex_CZ$Category <- opex_CZ$N <- NULL
opex_CZ <- melt(opex_CZ, variable.name="Year", value.name="Share")
opex_CZ$Source.code <- res_names$code[match(opex_CZ$Source, res_names$name)]

## Adjust emp_intensity
## (employment intensity influences employment in persons)
emp_intensity[emp_intensity$Category == "Employment: Low-skilled",colnames(emp_intensity) == "Category"] <- "Employment: Low-skilled male"
emp_intensity[emp_intensity$Category == "Employment: Medium-skilled",colnames(emp_intensity) == "Category"] <- "Employment: Medium-skilled male"
emp_intensity[emp_intensity$Category == "Employment: High-skilled",colnames(emp_intensity) == "Category"] <- "Employment: High-skilled male"

low_skilled <- sort(which(emp_intensity$Category == "Employment: Low-skilled male"), decreasing = TRUE)
for(i in low_skilled) {
  emp_intensity <- emp_intensity %>% 
    tibble::add_row(
      Category = "Employment: Low-skilled female",
      .after = i
    )
}
medium_skilled <- sort(which(emp_intensity$Category == "Employment: Medium-skilled male"), decreasing = TRUE)
for(i in medium_skilled) {
  emp_intensity <- emp_intensity %>% 
    tibble::add_row(
      Category = "Employment: Medium-skilled female",
      .after = i
    )
}
high_skilled <- sort(which(emp_intensity$Category == "Employment: High-skilled male"), decreasing = TRUE)
for(i in high_skilled) {
  emp_intensity <- emp_intensity %>% 
    tibble::add_row(
      Category = "Employment: High-skilled female",
      .after = i
    )
}

emp_intensity <- emp_intensity %>% do(na.locf(.)) ## Replace NA values with values for the previous row
emp_intensity <- melt(emp_intensity, variable.name="Year", value.name="Intensity")
emp_intensity$Source.code <- res_names$code[match(emp_intensity$Source, res_names$name)]


##########################################################################
# 3.9. Integrate values from capex_opex, lifespan_CZ, opex_CZ, CZ_RES_opex and emp_intensity into concordance_capex

concordance2015_opex <- concordance_opex
concordance_opex <- list()

for (year in years) {
  print(year)
  
  ## Integrate effects of capex : opex shares changes for wind and solar PV
  temp <- capex_opex[capex_opex$Year==year, -1] %>%
    spread(Source.code, Change)
  concordance2015_opex[grepl("capital",concordance2015_opex$Industry.Name),
                       grepl("i40.11",colnames(concordance2015_opex))
                       & !grepl("i40.11.g",colnames(concordance2015_opex))] <-
    CZ_RES_opex[grepl("capital",CZ_RES_opex$Industry.Name),
                grepl("i40.11",colnames(CZ_RES_opex))
                & !grepl("i40.11.g",colnames(CZ_RES_opex))] * temp[,grepl("i40.11",colnames(temp)) & !grepl("i40.11.g",colnames(temp))]
  
  ## Integrate effects of capex : opex shares changes for bioenergy - step 1
  ## (integrated separately, directly from the expert projections)
  temp_bioenergy <- opex_CZ[opex_CZ$Year==year & opex_CZ$Industry.Name=="Operating surplus: Consumption of fixed capital", -c(1,2)] %>%
    spread(Source.code, Share)
  concordance2015_opex[grepl("capital",concordance2015_opex$Industry.Name),
                       grepl("i40.11.g",colnames(concordance2015_opex))] <- 
    temp_bioenergy[,grepl("i40.11.g",colnames(temp_bioenergy))] * temp[,grepl("i40.11.g",colnames(temp))]
  
  ## Integrate effects of changes in lifespan
  temp <- lifespan_surplus[lifespan_surplus$Year==year,] %>%
    spread(Source.code, Change)
  concordance2015_opex[grepl("Remaining net operating surplus",concordance2015_opex$Industry.Name),
                       grepl("i40.11",colnames(concordance2015_opex))] <-
    CZ_RES_opex[grepl("Remaining net operating surplus",CZ_RES_opex$Industry.Name),
                grepl("i40.11",colnames(CZ_RES_opex))] * temp[,grepl("i40.11",colnames(temp))]
  
  ## Import other value added items from CZ_RES_opex that are not affected by the expert projections (taxes and royalties)
  ## (these remain unchanged over years)
  concordance2015_opex[grepl("Taxes|taxes|Royalties on resources",concordance2015_opex$Industry.Name),
                       grepl("i40.11",colnames(concordance2015_opex))] <-
    CZ_RES_opex[grepl("Taxes|taxes|Royalties on resources",CZ_RES_opex$Industry.Name),
                grepl("i40.11",colnames(CZ_RES_opex))]
  
  ## Calculate colsums of taxes, royalties and remaining net operating surplus for the bioenergy column
  sums <- as.data.frame(concordance2015_opex[grepl("Taxes|taxes|Royalties on resources|net operating surplus",concordance2015_opex$Industry.Name),
                                             startsWith(colnames(concordance2015_opex),"i40.11.g")])
  sums <- colSums(sums)
  
  ## Integrate effects of capex : opex shares changes for bioenergy - step 2
  ## (recalculate bioenergy capital consumption, using colsums of the rest of the value added items except for land rents and wages)
  concordance2015_opex[grepl("capital",concordance2015_opex$Industry.Name),
                       grepl("i40.11.g",colnames(concordance2015_opex))] <-
    concordance2015_opex[grepl("capital",concordance2015_opex$Industry.Name),
                         grepl("i40.11.g",colnames(concordance2015_opex))] * (1-sums)
  
  ## Delete the rest of the original opex shares (i.e. items not defined above)
  concordance2015_opex[!grepl("Taxes|taxes|capital|Royalties on resources|net operating surplus",concordance2015_opex$Industry.Name),
                       grepl("i40.11",colnames(concordance2015_opex))] <- 0
  
  ## Calculate colsums of the items defined above
  sums <- as.data.frame(t(colSums(concordance2015_opex[,startsWith(colnames(concordance2015_opex),"i40.11")])))
  sums <- 1-sums
  
  ## Import the rest of the opex shares from opex_CZ
  ## (without the bioenergy depreciation row from the bioenergy column in opex, otehrwise it pops up in the colsums of temp)
  temp <- opex_CZ[opex_CZ$Year==year & !grepl("Bioenergy_Depreciation",opex_CZ$Source_Category), -1] %>% 
    group_by(Industry.Name, Year, Source.code) %>% 
    summarise(Share = sum(Share, na.rm = T)) %>% 
    spread(Source.code, Share)
  # temp <- (as.data.frame(temp))
  # temp[is.na(temp)] <- 0
  # colSums(temp[,startsWith(colnames(temp),"i40.11")])
  
  ## Scale up the replacement values in temp to cover 1-colsums of the rest of the items defined and integrated above
  temp[,startsWith(colnames(temp),"i40.11")] <- t(t(temp[,startsWith(colnames(temp),"i40.11")]) * as.numeric(sums))
  
  ## Merge vlaues from temp (derived from opex_CZ) with concordance2015_opex
  temp <- plyr::join(concordance2015_opex[,1:3], temp)
  concordance2015_opex[,4:7][!is.na(temp[,5:8])] <- temp[,5:8][!is.na(temp[,5:8])]
  
  ## Check colsums if they add up to 1
  # colSums(concordance2015_opex[,startsWith(colnames(concordance2015_opex),"i40.11")])
  
  ## Integrate effects of employment intensity changes
  temp <- emp_intensity[emp_intensity$Year==year, -1] %>%
    spread(Source.code, Intensity)
  
  temp[startsWith(temp$Category,"Employment"),startsWith(colnames(temp),"i40.11")] <-
    temp[startsWith(temp$Category,"Employment"),startsWith(colnames(temp),"i40.11")] * CZ_RES_opex[startsWith(CZ_RES_opex$Industry.Name,"Employment"),startsWith(colnames(CZ_RES_opex),"i40.11")]
  concordance2015_opex[startsWith(concordance2015_opex$Industry.Name,"Employment"),startsWith(colnames(concordance2015_opex),"i40.11")] <-
    temp[startsWith(temp$Category,"Employment"),startsWith(colnames(temp),"i40.11")]
  
  concordance_opex[[year]] <- concordance2015_opex
}

} # End of condition p1



##########################################################################
# 4. Transform and calculate scenarios given by the user
##########################################################################

##########################################################################
# 4.1. Calculate installed capacity

## Adapt scen_el_cap, calculate the new installed capacity needed every modelled year
rownames(scen_el_cap) <- rownames(GFCF_CZ)
scen_el_cap[grepl("Instalovaná kapacita",colnames(scen_el_cap))] <- NULL
for (year in years) {
  if(year=="2015"){
    scen_el_cap[,paste0("Change.",year)] <- scen_el_cap[,year] - scen_el_cap[,"2015"]
  } else if(year=="2020"){
    scen_el_cap[,paste0("Change.",year)] <- scen_el_cap[,year] - scen_el_cap[,"2015"]
  } else if(year=="2025") {
    scen_el_cap[,paste0("Change.",year)] <- scen_el_cap[,year] - scen_el_cap[,"2020"]
  } else if(year=="2030") {
    scen_el_cap[,paste0("Change.",year)] <- scen_el_cap[,year] - scen_el_cap[,"2025"]
  } else if(year=="2035") {
    scen_el_cap[,paste0("Change.",year)] <- scen_el_cap[,year] - scen_el_cap[,"2030"]
  } else if(year=="2040") {
    scen_el_cap[,paste0("Change.",year)] <- scen_el_cap[,year] - scen_el_cap[,"2035"]
  } else if(year=="2045") {
    scen_el_cap[,paste0("Change.",year)] <- scen_el_cap[,year] - scen_el_cap[,"2040"]
  } else if(year=="2050") {
    scen_el_cap[,paste0("Change.",year)] <- scen_el_cap[,year] - scen_el_cap[,"2045"]
  }
}

## Replace negative values with zeros
scen_el_cap[scen_el_cap<0] <- 0

## Fill in values from inst_CZ into 2015, add effects of capital replacements and integrate effects of lifespan changes
## (lifespan changes have to be divided by 5 for 2020 onwards - otherwise they would capture effects for the whole 5 year intervals)
scen_el_cap[,grepl("Change.2015",colnames(scen_el_cap))] <- inst_CZ[,grepl("2015",colnames(inst_CZ))]
scen_el_cap[,startsWith(colnames(scen_el_cap),"Change")] <-
  scen_el_cap[,startsWith(colnames(scen_el_cap),"Change")] + scen_el_cap[,startsWith(colnames(scen_el_cap),"20")] / lifespan_CZ[,startsWith(colnames(lifespan_CZ),"20")]
scen_el_cap[,startsWith(colnames(scen_el_cap),"Change") & !grepl("2015",colnames(scen_el_cap))] <-
  scen_el_cap[,startsWith(colnames(scen_el_cap),"Change") & !grepl("2015",colnames(scen_el_cap))] / 5

## (scen_el_cap then directly translates into GFCF_CZ in part 5.1.)


##########################################################################
# 4.2. Calculate electricity generation

## Adapt scen_el_gen
rownames(scen_el_gen) <- rownames(GFCF_CZ)
scen_el_gen[grepl("Produkce elektøiny",colnames(scen_el_gen))] <- NULL

## Integrate replacement values for missing energy sources
scen_el_gen[grepl("i40.11.e.2|i40.11.i|i40.11.j|i40.11.k",rownames(scen_el_gen)),
            "2015"] <- replacements_CZ[,startsWith(colnames(replacements_CZ),"gen")]

## Calculate (proportional) changes for each year
for (year in years) {
  scen_el_gen[,paste0("Change.",year)] <- scen_el_gen[,year] / scen_el_gen[,"2015"]
}

## Replace NaN values with zeros
scen_el_gen[is.nan(scen_el_gen)] <- 0

## Calculate x, using replacement values from other countries
x2015el_repl <- x2015el_repl[c(19,37,51,13),]
xel_CZ <- x2015el_CZ
names(xel_CZ) <- c("xel_CZ")
xel_CZ[xel_CZ==0] <- x2015el_repl

for (year in years) {
  if(year=="2015"){
    scen_el_gen[,paste0("X",year)] <- xel_CZ
  } else {
    scen_el_gen[,paste0("X",year)] <- xel_CZ * scen_el_gen[,paste0("Change.",year)]
  }
}

## Replace back the original values for 2015 (replacements were used just to calculate the other years) and leave only columns with x
scen_el_gen[,"X2015"] <- x2015el_CZ
scen_el_gen <- scen_el_gen[,startsWith(colnames(scen_el_gen), "X")]


##########################################################################
# 4.3. Calculate production from other fuels

## RUN THIS PART (4.3.) ONLY IF WE MODEL THE WHOLE ENERGY SECTOR
if (p2==1) {

## Adjust colnames and translate fuels
colnames(scen_en)[startsWith(colnames(scen_en),"Produkce")] <- "Source"
scen_en$Source <- fuels$en

## Match and distribute values given by the user with the EXIOBASE sectors for the other fuels
concordance_fuel$Country.Code <- NULL
names(concordance_fuel)[names(concordance_fuel) == "EURef.Name"] <- "Source"
concordance_fuel[,c(years)] <- scen_en[match(concordance_fuel$Source,scen_en$Source),startsWith(colnames(scen_en),"20")]
concordance_fuel <- merge(concordance_fuel, as.data.table(concordance_fuel)[, .N, by = c("Source")], by = "Source")
scen_en <- concordance_fuel[order(concordance_fuel$Industry.Code),]

## Extract only the biofuels part from Chemicals nec from x2015fuel_CZ
x2015fuel_CZ[,c("Source","Industry.Name")] <- scen_en[,c("Source","Industry.Name")]
x2015fuel_CZ[grepl("Chemicals",x2015fuel_CZ$Industry.Name),"x2015fuel_CZ"] <-
  x2015fuel_CZ[grepl("Chemicals",x2015fuel_CZ$Industry.Name),"x2015fuel_CZ"] * EUSTi24.d_CZ[2,]

## Calculate shares of x for each of the EXIOBASE sectors on their main matching fuel category
shares_fuels <- x2015fuel_CZ
shares_fuels[grepl("Solids",shares_fuels$Source),"x2015fuel_CZ"] <-
  shares_fuels[grepl("Solids",shares_fuels$Source),"x2015fuel_CZ"] / colSums(as.data.frame(shares_fuels[grepl("Solids",shares_fuels$Source),"x2015fuel_CZ"]))
shares_fuels[grepl("Oil",shares_fuels$Source),"x2015fuel_CZ"] <-
  shares_fuels[grepl("Oil",shares_fuels$Source),"x2015fuel_CZ"] / colSums(as.data.frame(shares_fuels[grepl("Oil",shares_fuels$Source),"x2015fuel_CZ"]))
shares_fuels[grepl("Gas",shares_fuels$Source),"x2015fuel_CZ"] <-
  shares_fuels[grepl("Gas",shares_fuels$Source),"x2015fuel_CZ"] / colSums(as.data.frame(shares_fuels[grepl("Gas",shares_fuels$Source),"x2015fuel_CZ"]))
shares_fuels[grepl("Heat",shares_fuels$Source),"x2015fuel_CZ"] <-
  shares_fuels[grepl("Heat",shares_fuels$Source),"x2015fuel_CZ"] / colSums(as.data.frame(shares_fuels[grepl("Heat",shares_fuels$Source),"x2015fuel_CZ"]))
shares_fuels[grepl("Renewable energy forms",shares_fuels$Source),"x2015fuel_CZ"] <-
  shares_fuels[grepl("Renewable energy forms",shares_fuels$Source),"x2015fuel_CZ"] / colSums(as.data.frame(shares_fuels[grepl("Renewable energy forms",shares_fuels$Source),"x2015fuel_CZ"]))
shares_fuels$x2015fuel_CZ <- 1 / shares_fuels$x2015fuel_CZ
shares_fuels$x2015fuel_CZ[is.infinite(shares_fuels$x2015fuel_CZ)] <- 0

## Rewrite N for each of the sectors and their matching fuel category from shares_fuels
scen_en$N <- shares_fuels$x2015fuel_CZ
scen_en$N[is.nan(scen_en$N)] <- 0
scen_en[, grepl("20", colnames(scen_en))] <- scen_en[, grepl("20", colnames(scen_en))] / scen_en$N
scen_en <- scen_en %>% mutate_if(is.numeric, function(x) ifelse(is.infinite(x), 0, x))

## Calculate (proportional) changes for each year
## (adapt gas rows according to the share of gas vs. biogas from EUST for 2020+)
for (year in years) {
  scen_en[,paste0("Change.",year)] <- scen_en[,year] / scen_en[,"2015"]
}

## Replace NaN values with zeros
scen_en[is.nan(scen_en)] <- 0

## Calculate x, using just the respective share of gas vs. biogas from EUST for 2020+
for (year in years) {
  if(year=="2015"){
    scen_en[,paste0("X",year)] <- x2015fuel_CZ$x2015fuel_CZ
  } else {
    scen_en[grepl("i40.2",scen_en$Industry.Code),paste0("X",year)] <-
      EUSTi40.2_CZ[1,] * x2015fuel_CZ[grepl("Gas",x2015fuel_CZ$Source),"x2015fuel_CZ"] * scen_en[grepl("i40.2",scen_en$Industry.Code),paste0("Change.",year)] +
      EUSTi40.2_CZ[2,] * x2015fuel_CZ[grepl("Gas",x2015fuel_CZ$Source),"x2015fuel_CZ"] * scen_en[grepl("i90.2.c",scen_en$Industry.Code),paste0("Change.",year)]
    ## (change in biogas based on proportional changes of rows for Renewable energy forms, selected one randomly)
    
    scen_en[!grepl("i40.2",scen_en$Industry.Code),paste0("X",year)] <-
      x2015fuel_CZ[!grepl("Gas",x2015fuel_CZ$Source),"x2015fuel_CZ"] * scen_en[!grepl("i40.2",scen_en$Industry.Code),paste0("Change.",year)]
  }
}

## Add rownames and leave only columns with x
rownames(scen_en) <- paste0("CZ_",scen_en$Industry.Code)
scen_en <- scen_en[,startsWith(colnames(scen_en), "X")]

} # end of condition p2



##########################################################################
# 5. Implement capex (GFCF) parts into the model structure
##########################################################################

##########################################################################
# 5.0. Prepare concordance_capex for each year

## DO NOT RUN THIS PART (5.0.) IF WE RUN PART 3. - IF WE LOAD THE EXPERT PROJECTIONS (SKIP TO 5.1.)
if (p1==0) {

concordance2015_capex <- concordance_capex
concordance_capex <- list()

for (year in years) {
  concordance_capex[[year]] <- concordance2015_capex
}

}


##########################################################################
# 5.1. Replace the columns from the original GFCF capex concordance for all electricity sectors with capex projections for the modelled sectors

GFCF2015_el <- GFCF_el
GFCF_el <- list()

for (year in years) {
  GFCF_el[[year]] <- GFCF2015_el
  for (r in res) {
    GFCF_el[[year]][,endsWith(colnames(GFCF_el[[year]]),r)] <- concordance_capex[[year]][,endsWith(colnames(concordance_capex[[year]]),r)]
  }
}


##########################################################################
# 5.2. Calculate GFCF_CZ on the basis of the scenario for the installed capacity (scen_el_cap)

GFCF_CZ <- GFCF_CZ * scen_el_cap[,startsWith(colnames(scen_el_cap),"Change")]
colnames(GFCF_CZ) <- years


##########################################################################
# 5.3. Add summary GFCF rows for each sector and calculate shares of each row's input to the summary GFCF

GFCFshare_CZ <- select(YGFCF2015_CZ, contains("Gross fixed capital formation"))
rownames(GFCFshare_CZ) <- paste0(IO_CZ.codes$Country.Code, "_", IO_CZ.codes$Industry.Code)

GFCFsum_CZ <- matrix(0, ncol = 2, nrow = 165)
rownames(GFCFsum_CZ) <- unique(IO_CZ.codes$Industry.Code)
colnames(GFCFsum_CZ) <- paste0(unique(IO_CZ.codes$Country.Code), "_Gross fixed capital formation")
sectors <- c(rownames(GFCFsum_CZ))
for (sector in sectors) {
  GFCFsum_CZ[endsWith(rownames(GFCFsum_CZ), sector),] <- colSums(GFCFshare_CZ[endsWith(rownames(GFCFshare_CZ), sector),])
  GFCFshare_CZ[which(endsWith(rownames(GFCFshare_CZ), sector)),] <- t(t(as.matrix(GFCFshare_CZ[which(endsWith(rownames(GFCFshare_CZ), sector)),])) / as.vector(GFCFsum_CZ[which(endsWith(rownames(GFCFsum_CZ), sector)),]))
}

## Replace NaNs after dividing by zero
GFCFshare_CZ[is.nan(GFCFshare_CZ)] <- 0


##########################################################################
# 5.4. Paste GFCF from each sector of CZ vs. RoW going into the modelled sectors into YGFCF2015 and YGFCF for other modelled years

YGFCF_CZ <- list()

for (year in years) {
  YGFCF_CZ[[year]] <- YGFCF2015_CZ
  for (country in countries) {
    for (e in electricity) {
      YGFCF_CZ[[year]][,paste0(country, "_i40.11.", e)] <- rep(GFCF_el[[year]][,paste0("i40.11.", e)] * GFCF_CZ[paste0(country, "_i40.11.", e),year], 2) * GFCFshare_CZ[,which(startsWith(colnames(GFCFshare_CZ), country))]
    }
  }
}


##########################################################################
# 5.5. Extract only the detailed electricity GFCF columns from YGFCF

for (year in years) {
  YGFCF_CZ[[year]] <- YGFCF_CZ[[year]][,!grepl("i40.11.l", colnames(YGFCF_CZ[[year]]))&
                                         grepl("i40.11", colnames(YGFCF_CZ[[year]]))]
}



##########################################################################
# 6. Implement opex expert projections into A_CZ and Ext_CZ, calculate L
##########################################################################

## RUN THIS PART (6.) ONLY IF WE RUN PART 3. (IF WE LOAD THE EXPERT PROJECTIONS)
if (p1==1) {

#########################################################################
# 6.1. Calculate summary rows and shares of each row's input to the summary

Ashare_CZ <- A_CZ[,colnames(A_CZ) %in% c("CZ_i40.11.e.1","CZ_i40.11.g","CZ_i40.11.h.1","CZ_i40.11.h.2")]
rownames(Ashare_CZ) <- paste0(IO_CZ.codes$Country.Code, "_", IO_CZ.codes$Industry.Code)

Asum_CZ <- matrix(0, ncol = 4, nrow = 165)
rownames(Asum_CZ) <- unique(IO_CZ.codes$Industry.Code)
colnames(Asum_CZ) <- c("CZ_i40.11.e.1","CZ_i40.11.g","CZ_i40.11.h.1","CZ_i40.11.h.2")
sectors <- c(rownames(Asum_CZ))
for (sector in sectors) {
  Asum_CZ[endsWith(rownames(Asum_CZ), sector),] <- colSums(Ashare_CZ[endsWith(rownames(Ashare_CZ), sector),])
  Ashare_CZ[which(endsWith(rownames(Ashare_CZ), sector)),] <- t(t(as.matrix(Ashare_CZ[which(endsWith(rownames(Ashare_CZ), sector)),])) / as.vector(Asum_CZ[which(endsWith(rownames(Asum_CZ), sector)),]))
}

## Replace NaNs after dividing by zero
Ashare_CZ[is.nan(Ashare_CZ)] <- 0


##########################################################################
# 6.2. Replace original columns of the modelled renewable energy sectors for each year in CZ vs. RoW in A_CZ

A2015_CZ <- as.data.frame(A_CZ)
A_CZ <- list()

for (year in years) {
  A_CZ[[year]] <- A2015_CZ
  for (country in countries) {
    for (r in res) {
      A_CZ[[year]][,paste0("CZ_i40.11.",r)] <- rep(concordance_opex[[year]][1:165,paste0("i40.11.",r)], 2) * Ashare_CZ[,paste0(country,"_i40.11.",r)]
    }
  }
}


##########################################################################
# 6.3. Replace original columns of the modelled renewable energy sectors for each year in Ext_CZ, including employment.persons part

Ext2015_CZ <- as.data.frame(Ext_CZ)
Ext_CZ <- list()

for (year in years) {
  Ext_CZ[[year]] <- Ext2015_CZ
  for (country in countries) {
    for (r in res) {
      Ext_CZ[[year]][1:15,paste0("CZ_i40.11.",r)] <- concordance_opex[[year]][166:180,paste0("i40.11.",r)]
    }
  }
}

} # End of condition p1



##########################################################################
# 7. Adapt A_CZ to Anoel_CZ and calculate Lnoel_CZ for each year
##########################################################################

##########################################################################
# 7.0. Prepare A_CZ for each year

## DO NOT RUN THIS PART (7.0.) IF WE RUN PARTS 3. AND 6. - IF WE LOAD THE EXPERT PROJECTIONS (SKIP TO 7.1. OR 7.2.)
if (p1==0) {

A2015_CZ <- as.data.frame(A_CZ)
A_CZ <- list()

for (year in years) {
  A_CZ[[year]] <- A2015_CZ
}

}


##########################################################################
# 7.1. Calculate Anoel with zero electricity rows in CZ to avoid changing diagonal elements in L

## DO NOT RUN THIS PART (7.1.) IF WE MODEL THE WHOLE ENERGY SECTOR (SKIP TO 7.2.)
if (p2==0) {

Anoel_CZ <- list()

for (year in years) {
  Anoel_CZ[[year]] <- A_CZ[[year]]
  Anoel_CZ[[year]][c(electricity_CZ),] <- 0
  Anoel_CZ[[year]] <- as.matrix(Anoel_CZ[[year]])
}

}


##########################################################################
# 7.2. Calculate Anoel with zero electricity and other energy fuels rows in CZ to avoid changing diagonal elements in L

## RUN THIS PART (7.2.) ONLY IF WE MODEL THE WHOLE ENERGY SECTOR
if (p2==1) {

Anoel_CZ <- list()

for (year in years) {
  Anoel_CZ[[year]] <- A_CZ[[year]]
  Anoel_CZ[[year]][c(fuel_CZ,electricity_CZ),] <- 0
  Anoel_CZ[[year]] <- as.matrix(Anoel_CZ[[year]])
}

}


##########################################################################
# 7.3. Calculate Lnoel with adapted off-diagonal elements of the modelled sectors in CZ

Lnoel_CZ <- list()

for (year in years) {
  Lnoel_CZ[[year]] <- solve(diag(nrow(IO_CZ.codes))-Anoel_CZ[[year]])
  colnames(Lnoel_CZ[[year]]) <- paste0(IO_CZ.codes$Country.Code,"_",IO_CZ.codes$Industry.Code)
}



##########################################################################
# 8. Implement scenarios into x_CZ for each year
##########################################################################

##########################################################################
# 8.1. Insert changes in total output (x_CZ) for each modelled year based on the scenarios

## Insert changes for each modelled year into x_CZ
x_CZ <- list()

for (year in years) {
  x_CZ[[year]] <- x2015_CZ
  if(year>2015){
    x_CZ[[year]][electricity_CZ] <- scen_el_gen[,paste0("X", year)]
  }
}


##########################################################################
# 8.2. Insert changes in total output (x_CZ) of the other fuels for each modelled year based on the scenarios

## RUN THIS PART (8.2.) ONLY IF WE MODEL THE WHOLE ENERGY SECTOR
if (p2==1) {

## Insert changes for each scenario and each modelled year into x_CZ
for (year in years) {
  x_CZ[[year]][fuel_CZ] <- scen_en[,paste0("X", year)]
}

}



##########################################################################
# 9. Calculate footprint
##########################################################################

##########################################################################
# 9.0. Prepare Ext_CZ for each year

## DO NOT RUN THIS PART (9.0.) IF WE RUN PARTS 3. AND 6. - IF WE LOAD THE EXPERT PROJECTIONS (SKIP TO 9.1.)
if (p1==0) {

Ext2015_CZ <- as.data.frame(Ext_CZ)
Ext_CZ <- list()

for (year in years) {
  Ext_CZ[[year]] <- Ext2015_CZ
}

}


##########################################################################
# 9.1. Prepare Ext (extensions) by extracting indicators we are interested in, prepare Ext.names

Ext <- list()
Ext.names <- list()

for (year in years) {
  
  extension <- "emp"
  ## Employment by skill levels and gender (EMP)
  Ext[[year]][[extension]] <- t(Ext_CZ[[year]][Q_CZ.codes$Compartment=="Employment.persons" & Q_CZ.codes$Stressor!="Employment: Vulnerable employment",])
  Ext.names[[extension]] <- as.character(Q_CZ.codes$Stressor[Q_CZ.codes$Compartment=="Employment.persons" & Q_CZ.codes$Stressor!="Employment: Vulnerable employment"])
  Ext.names[[extension]] <- gsub(": ", "_", Ext.names[[extension]])
  
  extension <- "gva"
  ## Gross value added (GVA)
  Ext[[year]][[extension]] <- t(Ext_CZ[[year]][Q_CZ.codes$Compartment=="Value.added" & Q_CZ.codes$Stressor %!in% c("Taxes less subsidies on products purchased: Total","Other net taxes on production"),])
  Ext[[year]][[extension]] <- as.matrix(rowSums(Ext[[year]][[extension]]))
  Ext.names[[extension]] <- c("Gross value added")
  
  extension <- "gdp"
  ## Gross domestic product (GDP)
  Ext[[year]][[extension]] <- t(Ext_CZ[[year]][Q_CZ.codes$Compartment=="Value.added",])
  Ext[[year]][[extension]] <- as.matrix(rowSums(Ext[[year]][[extension]]))
  Ext.names[[extension]] <- c("Gross domestic product")
  
  extension <- "ghg"
  ## GHG emissions (GHG)
  Ext[[year]][[extension]] <- t(Ext_CZ[[year]][Q_CZ.codes$Compartment=="Emissions",])
  Ext.names[[extension]] <- c("GHG")
}


##########################################################################
# 9.2. Calculate footprint for the effects of the electricity sector

## DO NOT RUN THIS PART (9.2.) IF WE MODEL THE WHOLE ENERGY SECTOR (SKIP TO 9.3.)
if (p2==0) {

## Define footprint functions with electricity only
footprint_om <- function(L,Ext,ee,country,results){
  
  ## Calculate Multiplier Matrix (MP)
  MP <- L * Ext[,ee]
  
  ## Calculate Footprint (FP = MP * x). x = Total Output
  FP <- t(t(MP) * x_data)
  FP <- cbind(IO_CZ.codes, data.frame(Year = rep(year,330), Indicator = rep(Ext.names[[extension]][ee],330), Effect = rep("O&M",330)), 
              FP[, grepl("CZ_i40.11.", colnames(FP))])
  FP <- melt(FP, id=c(colnames(IO_CZ.codes),"Year","Indicator","Effect"), variable.name="Electricity", value.name="Value")
  FP <- FP[FP$Electricity %in% paste0("CZ_i40.11.",electricity),]
  FP$Country.Destination <- substr(FP$Electricity, 1, 2)
  FP$Electricity <- substr(FP$Electricity, 4, 13)
  
  results <- rbind(results, FP)
}

footprint_gfcf <- function(L,Ext,ee,country,results){
  
  ## Calculate Multiplier Matrix (MP)
  MP <- L * Ext[,ee]
  
  ## Calculate Footprint (FP = MP * FD). FD = Final Demand (Y)
  FP <- MP %*% as.matrix(Y_data)
  FP <- cbind(IO_CZ.codes, data.frame(Year = rep(year,330), Country.Destination = country, Indicator = rep(Ext.names[[extension]][ee],330), Effect = rep("GFCF",330)), FP)
  FP <- melt(FP, id=c(colnames(IO_CZ.codes),"Year","Country.Destination","Indicator","Effect"), variable.name="Electricity", value.name="Value")
  FP$Electricity <- substr(FP$Electricity, 4, 13)
  
  results <- rbind(results, FP)
}


## Check memory
gc()

## Calculate footprint
reslist <- list()

## extension <- "emp"
for (extension in extensions) {
  print(extension)
  
  results <- data.frame()
  
  ## year <- "2015"
  for(year in years){
    print(paste0("year ",year))
    
    x_data <- x_CZ[[year]]
    Y_data <- YGFCF_CZ[[year]]
    
    ## Calculate footprints
    ## ee <- 1 (ee = column in extension)
    for(ee in 1:ncol(Ext[[year]][[extension]])){
      print(paste0("extension ",ee))
      
      results <- footprint_om(L=Lnoel_CZ[[year]], Ext=Ext[[year]][[extension]], ee=ee, country=country, results=results)
      
      ## country <- "CZ"
      for(country in countries){
        results <- footprint_gfcf(L=Lnoel_CZ[[year]], Ext=Ext[[year]][[extension]], ee=ee, country=country, results=results)
      }
    }
  }
  results$Industry.Group <- results$Region.Code <- NULL
  results$Extension <- c(rep(extension))
  results <- results %>%
    rename(Country.Origin = Country.Code)
  
  reslist[[extension]] <- results
}

results = do.call(rbind, reslist)

}


##########################################################################
# 9.3. Calculate footprint for the effects of the whole energy sector

## RUN THIS PART (9.3.) ONLY IF WE MODEL THE WHOLE ENERGY SECTOR
if (p2==1) {

## Define footprint functions with all energy sectors
footprint_om <- function(L,Ext,ee,country,results){
  
  ## Calculate Multiplier Matrix (MP)
  MP <- L * Ext[,ee]
  
  ## Calculate Footprint (FP = MP * x). x = Total Output
  FP <- t(t(MP) * x_data)
  FP <- cbind(IO_CZ.codes, data.frame(Year = rep(year,330), Indicator = rep(Ext.names[[extension]][ee],330), Effect = rep("O&M",330)), 
              FP[, grepl("CZ_i40.11.|CZ_i01.w.2|CZ_i10|CZ_i23.1|CZ_i23.2|CZ_i24.d|CZ_i40.2|CZ_i40.3|CZ_i90.1|CZ_i90.2|CZ_i90.3", colnames(FP))])
  FP <- melt(FP, id=c(colnames(IO_CZ.codes),"Year","Indicator","Effect"), variable.name="Energy", value.name="Value")
  FP <- FP[grepl("CZ_i40.11.a|CZ_i40.11.b|CZ_i40.11.c|CZ_i40.11.d|CZ_i40.11.e.1|CZ_i40.11.e.2|CZ_i40.11.f|CZ_i40.11.g|CZ_i40.11.h.1|CZ_i40.11.h.2|CZ_i40.11.i|CZ_i40.11.j|CZ_i40.11.k|CZ_i01.w.2|CZ_i10|CZ_i23.1|CZ_i23.2|CZ_i24.d|CZ_i40.2|CZ_i40.3|CZ_i90.1|CZ_i90.2|CZ_i90.3", FP$Energy),]
  FP$Country.Destination <- substr(FP$Energy, 1, 2)
  FP$Energy <- substr(FP$Energy, 4, 13)
  
  results <- rbind(results, FP)
}

footprint_gfcf <- function(L,Ext,ee,country,results){
  
  ## Calculate Multiplier Matrix (MP)
  MP <- L * Ext[,ee]
  
  ## Calculate Footprint (FP = MP * FD). FD = Final Demand (Y)
  FP <- MP %*% as.matrix(Y_data)
  FP <- cbind(IO_CZ.codes, data.frame(Year = rep(year,330), Country.Destination = country, Indicator = rep(Ext.names[[extension]][ee],330), Effect = rep("GFCF",330)), FP)
  FP <- melt(FP, id=c(colnames(IO_CZ.codes),"Year","Country.Destination","Indicator","Effect"), variable.name="Energy", value.name="Value")
  FP$Energy <- substr(FP$Energy, 4, 13)
  
  results <- rbind(results, FP)
}

## Check memory
gc()

## Calculate footprint
reslist <- list()

## extension <- "emp"
for (extension in extensions) {
  print(extension)
  
  results <- data.frame()
  
  ## year <- "2015"
  for(year in years){
    print(paste0("year ",year))
    
    x_data <- x_CZ[[year]]
    Y_data <- YGFCF_CZ[[year]]
    
    ## Calculate footprints
    ## ee <- 1 (ee = column in extension)
    for(ee in 1:ncol(Ext[[year]][[extension]])){
      print(paste0("extension ",ee))
      
      results <- footprint_om(L=Lnoel_CZ[[year]], Ext=Ext[[year]][[extension]], ee=ee, country=country, results=results)
      
      ## country <- "CZ"
      for(country in countries){
        results <- footprint_gfcf(L=Lnoel_CZ[[year]], Ext=Ext[[year]][[extension]], ee=ee, country=country, results=results)
      }
    }
  }
  results$Industry.Group <- results$Region.Code <- NULL
  results$Extension <- c(rep(extension))
  results <- results %>%
    rename(Country.Origin = Country.Code)
  
  reslist[[extension]] <- results
}

results = do.call(rbind, reslist)

}



##########################################################################
# 10. Load results for the electricity sectors only, organise data and plot
##########################################################################

## DO NOT RUN THIS PART (10.) IF WE MODEL THE WHOLE ENERGY SECTOR (SKIP TO 11.)
if (p2==0) {

## RESULTS - ELECTRICITY ONLY:

## 1) EMP
## a) Total
## b) By effects (O&M vs. GFCF)
## c) By skill levels and gender
## d) By sectors
## e) By each electricity source

## 2) GVA, GDP
## a) Total
## b) By sectors
## c) By each electricity source

## 3) GHG
## a) Total
## b) By each electricity source


## Domestic: Country.Origin = CZ

## Note that I can only calculate "pure" domestic = where Country.Destination matches Country.Origin.
## The results thus omit effects related to Czechia's electricity industries that are induced outside CZ (Country.Destination = RoW).
## However, the electricity mix outside CZ does not change in the model, so this employment would remain constant over time anyway.


##########################################################################
# 10.1. Organise EMP total results and plot

## Aggregate by total values (and nothing else)
emp_total <- results
emp_total <- emp_total %>% 
  filter(Country.Origin %in% "CZ") %>%
  filter(Extension %in% "emp") %>%
  group_by(Country.Destination, Year) %>%
  summarize(Value = sum(Value))

## Export results
fwrite(emp_total, file = paste0(respath,"emp_total.csv"), sep=";", dec=",")

## Mutate to plot stacked bar chart with value labels and round numbers
emp_total <- emp_total %>%
  group_by(Year) %>%
  mutate(lab_ypos = cumsum(Value) - 0.5 * Value)
emp_total$Value <- round(emp_total$Value, digits = 1)
emp_total$lab_ypos <- round(emp_total$lab_ypos, digits = 1)

## Plot results
plot_total <- ggplot(emp_total) +
  geom_bar(aes(x = as.factor(Year), y = Value),
           stat = "identity",
           width = 0.8) + ## ?position_stack() - position of bars
  geom_text(aes(x = as.factor(Year), y = lab_ypos, label = Value),
            color = "white",
            size = 5,
            vjust = 0.3) +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  xlab(NULL) +
  ylab("Poèet pracovních míst (1000)") +
  ggtitle("Poptávka po práci - elektroenergetika a dodavatelské øetìzce",
          "Celkem") +
  theme(strip.text.x = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 16))

## Export plot
png(paste0(respath,"Plots/emp_total.png"), width = 800, height = 500)
print(plot_total)
dev.off()


##########################################################################
# 10.2. Organise EMP results by O&M vs. GFCF and plot

## Aggregate by O&M vs. GFCF effects (and nothing else)
emp_effects <- results
emp_effects <- emp_effects %>% 
  filter(Country.Origin %in% "CZ") %>%
  filter(Extension %in% "emp") %>%
  group_by(Country.Destination, Year, Effect) %>%
  summarize(Value = sum(Value))

## Reorder Effect to make the right order in the plot
neworder <- c("O&M","GFCF")
emp_effects <- emp_effects %>% slice(order(factor(Effect, levels = neworder)))

## Export results
fwrite(emp_effects, file = paste0(respath,"emp_effects.csv"), sep=";", dec=",")

## Mutate to plot stacked bar chart with value labels and round numbers
emp_effects <- emp_effects %>%
  group_by(Year) %>%
  mutate(lab_ypos = cumsum(Value) - 0.5 * Value)
emp_effects$Value <- round(emp_effects$Value, digits = 1)
emp_effects$lab_ypos <- round(emp_effects$lab_ypos, digits = 1)

## Make Effect ordered factor
emp_effects$Effect <- factor(emp_effects$Effect, levels = rev(unique(emp_effects$Effect)), ordered = TRUE)

## Plot results
plot_effects <- ggplot(emp_effects) +
  geom_bar(aes(x = as.factor(Year), y = Value, fill = Effect, group = Effect),
           stat = "identity",
           position = "stack",
           width = 0.8) + ## ?position_stack() - position of bars
  geom_text(aes(x = as.factor(Year), y = lab_ypos, label = Value, group = Effect),
            colour = "white",
            size = 5,
            vjust = 0.3) +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  scale_y_continuous(expand = c(0.1, 0.1)) +
  xlab(NULL) +
  ylab("Poèet pracovních míst (1000)") +
  ggtitle("Poptávka po práci - elektroenergetika a dodavatelské øetìzce",
          "Provoz a údržba vs. tvorba hrubého fixního kapitálu") +
  scale_fill_manual(values = c("#1473C3", "#0A1A8F"), ## colours
                    name = "Kategorie", ## label name
                    labels = c("Tvorba hrubého fixního kapitálu", "Provoz a údržba")) + ## label values
  theme(strip.text.x = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 16))

## Export plot
png(paste0(respath,"Plots/emp_effects.png"), width = 800, height = 500)
print(plot_effects)
dev.off()


##########################################################################
# 10.3. Organise EMP results by skill levels and gender and plot

## Aggregate by distributional employment categories (and nothing else)
emp_distr <- results
emp_distr <- emp_distr %>% 
  filter(Country.Origin %in% "CZ") %>%
  filter(Extension %in% "emp") %>%
  group_by(Country.Destination, Year, Indicator) %>%
  summarize(Value = sum(Value))

## Reorder Indicator to make the right order in the plot
neworder <- c("Employment_Low-skilled female","Employment_Medium-skilled female","Employment_High-skilled female",
              "Employment_Low-skilled male","Employment_Medium-skilled male","Employment_High-skilled male")
emp_distr <- emp_distr %>% slice(order(factor(Indicator, levels = neworder)))

## Export results
fwrite(emp_distr, file = paste0(respath,"emp_distr.csv"), sep=";", dec=",")

## Mutate to plot stacked bar chart with value labels and round numbers
emp_distr <- emp_distr %>%
  group_by(Year) %>%
  mutate(lab_ypos = cumsum(Value) - 0.5 * Value)
emp_distr$Value <- round(emp_distr$Value, digits = 1)
emp_distr$lab_ypos <- round(emp_distr$lab_ypos, digits = 1)

## Make Indicator ordered
emp_distr$Indicator <- factor(emp_distr$Indicator, levels = rev(unique(emp_distr$Indicator)), ordered = TRUE)

## Plot results
plot_distr <- ggplot(emp_distr) +
  geom_bar(aes(x = as.factor(Year), y = Value, fill = Indicator),
           stat = "identity",
           position = "stack",
           width = 0.8) + ## ?position_stack() - position of bars
  geom_text(aes(x = as.factor(Year), y = lab_ypos, label = Value, group = Indicator),
            color = "white",
            size = 5,
            vjust = 0.3) +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  scale_y_continuous(expand = c(0.1, 0.1)) +
  xlab(NULL) +
  ylab("Poèet pracovních míst (1000)") +
  scale_fill_manual(values = c("#632E4A","#763966","#894385","#117E18","#2A9617","#51AC1D"),
                    name = "Kategorie",
                    labels = c("Muži - vysoce kvalifikovaná", "Muži - støednì kvalifikovaná", "Muži - nízce kvalifikovaná",
                               "Ženy - vysoce kvalifikovaná", "Ženy - støednì kvalifikovaná", "Ženy - nízce kvalifikovaná")) +
  ggtitle("Poptávka po práci - elektroenergetika a dodavatelské øetìzce",
          "Èlenìní dle úrovnì kvalifikace a pohlaví") +
  theme(strip.text.x = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 15),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 16))

## Export plot
png(paste0(respath,"Plots/emp_distr.png"), width = 800, height = 500)
print(plot_distr)
dev.off()


##########################################################################
# 10.4. Organise EMP results by sectors (winners vs. losers sorted by 2050; average over 2015-2050) and plot

## Aggregate by sectors (and nothing else)
emp_sectors <- results
emp_sectors <- emp_sectors %>% 
  filter(Country.Origin %in% "CZ") %>%
  filter(Extension %in% "emp") %>%
  group_by(Country.Destination, Year, Industry.Code, Industry.Name) %>%
  summarize(Value = sum(Value))

## Rename sectors in Czech
emp_sectors$Industry.Name <- c(rep(industries$cz))

## Cast ("unmelt") to match values from different years next to each other, calculate Change columns (percentage change compared to 2015)
emp_sectors <- dcast(emp_sectors, Country.Destination + Industry.Name + Industry.Code~Year, value.var = "Value")
for (year in years) {
  if (year>2015) {
    emp_sectors[,paste0("Change.",year)] <- emp_sectors[,year] - emp_sectors[,"2015"]
  }
}

## Delete Value columns
for (year in years) {
  emp_sectors[,year] <- NULL
}

## Sort by Change.2050 values in a descending order and change colnames
emp_sectors <- emp_sectors %>% 
  as_tibble() %>% 
  arrange(desc(emp_sectors$Change.2050))
colnames(emp_sectors)[4:10] <- c("2020","2025","2030","2035","2040","2045","2050")

## Melt and adjust colnames
emp_sectors <- melt(emp_sectors)
colnames(emp_sectors)[4] <- "Year"
colnames(emp_sectors)[5] <- "Change"

## Export results
fwrite(emp_sectors, file = paste0(respath,"emp_sectors.csv"), sep=";", dec=",")

## Round numbers
emp_sectors$Change <- round(emp_sectors$Change, digits = 1)

## Plot results for 10 biggest winners
plot_sectors_win <- ggplot(emp_sectors[c(991:1000,826:835,661:670,496:505,331:340,166:175,1:10),], aes(x = Industry.Name, y = Change, fill = as.factor(Year))) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.9)) + ## ?position_dodge() - position of bars
  scale_x_discrete(limits = emp_sectors$Industry.Name[1:10], ## avoid automatically (alphabetically) sorting the x-axis
                   labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(expand = c(0.2, 0.2)) +
  xlab(NULL) +
  ylab("Poèet pracovních míst (1000)") +
  ggtitle("Poptávka po práci - elektroenergetika a dodavatelské øetìzce",
          "10 odvìtví s nejvìtším nárùstem oproti roku 2015 (seøazeno dle hodnot v roce 2050)") +
  scale_fill_manual(values = c(rep_len(c("#A6DF83","#8BD873","#6FD164","#56C95B","#48C15E","#3AB863","#2DAF6A"), length(unique(emp_sectors$Industry.Name))-1)), ## colours
                    name = "Rok",
                    labels = c("2020","2025","2030","2035","2040","2045","2050")) +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 16),
        legend.position = "bottom",
        legend.direction = "horizontal",
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 16)) +
  guides(fill = guide_legend(ncol = 7))

## Export plot
png(paste0(respath,"Plots/emp_sectors_win.png"), width = 800, height = 500)
print(plot_sectors_win)
dev.off()

## Plot results for 10 biggest losers
plot_sectors_los <- ggplot(emp_sectors[c(1155:1146,990:981,825:816,660:651,495:486,330:321,165:156),], aes(Industry.Name, Change, fill = as.factor(Year))) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.9)) + ## ?position_dodge() - position of bars
  scale_x_discrete(limits = emp_sectors$Industry.Name[1146:1155], ## avoid automatically (alphabetically) sorting the x-axis
                   labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(expand = c(0.2, 0.2)) +
  xlab(NULL) +
  ylab("Poèet pracovních míst (1000)") +
  ggtitle("Poptávka po práci - elektroenergetika a dodavatelské øetìzce",
          "10 odvìtví s nejvìtším poklesem oproti roku 2015 (seøazeno dle hodnot v roce 2050)") +
  scale_fill_manual(values = c(rep_len(c("#B969A1","#AC5080","#9F385B","#912032","#830B08","#770019","#68002B"), length(unique(emp_sectors$Industry.Name))-1)), ## colours
                    name = "Rok",
                    labels = c("2020","2025","2030","2035","2040","2045","2050")) +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 16),
        legend.position = "bottom",
        legend.direction = "horizontal",
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 16)) +
  guides(fill = guide_legend(ncol = 7))

## Export plot
png(paste0(respath,"Plots/emp_sectors_los.png"), width = 800, height = 500)
print(plot_sectors_los)
dev.off()

## Calculate average employment change over years
emp_sectors_av <- emp_sectors

## Cast ("unmelt"), calculate Average column, delete Change columns, sort by Average column and leave only 10 first and last rows
emp_sectors_av <- dcast(emp_sectors_av, Country.Destination + Industry.Name + Industry.Code~Year, value.var = "Change")
emp_sectors_av$Average <- round(rowMeans(emp_sectors_av[,4:10]), digits = 1)
emp_sectors_av[,4:10] <- NULL
emp_sectors_av <- emp_sectors_av[order(emp_sectors_av$Average, decreasing = TRUE),]
emp_sectors_av <- emp_sectors_av[c(1:10,156:165),]

## Make Industry.Name ordered factor
emp_sectors_av$Industry.Name <- factor(emp_sectors_av$Industry.Name, levels = rev(unique(emp_sectors_av$Industry.Name)), ordered = TRUE)

## Export results
fwrite(emp_sectors_av, file = paste0(respath,"emp_sectors_av.csv"), sep=";", dec=",")

## Plot results for average 10 biggest winners and 10 biggest losers
plot_sectors_av <- ggplot(emp_sectors_av, aes(Industry.Name, Average, fill = as.factor(desc(Industry.Name)))) +
  geom_bar(stat = "identity",
           width = 0.9) +
  scale_x_discrete(limits = emp_sectors_av$Industry.Name, ## avoid automatically (alphabetically) sorting the x-axis
                   labels = function(x) str_wrap(x, width = 60),
                   expand = c(0.23, 0.23)) +
  scale_y_continuous(expand = c(0.1, 0.1)) +
  xlab(NULL) +
  ylab("Poèet pracovních míst (1000)") +
  ggtitle("Poptávka po práci - elektroenergetika a dodavatelské øetìzce",
          "10 odvìtví s nejvìtším nárùstem a poklesem (prùmìrná hodnota mezi roky 2015-2050)") +
  scale_fill_manual(values = c(rep(c("#2DAF6A"), times = 10),rep(c("#68002B"), times = 10))) + ## colours
  geom_text(aes(y = Average + 0.2 * sign(Average), label = round(Average, digits = 1))) +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 11),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 16))

## Export plot
png(paste0(respath,"Plots/emp_sectors_av.png"), width = 1000, height = 600)
print(plot_sectors_av)
dev.off()


##########################################################################
# 10.5. Organise EMP results by electricity sources and plot

## Aggregate results by electricity sources (and nothing else)
emp_sources <- results
emp_sources <- emp_sources %>% 
  filter(Country.Origin %in% "CZ") %>%
  filter(Extension %in% "emp") %>%
  group_by(Country.Origin, Country.Destination, Year, Electricity) %>%
  summarize(Value = sum(Value))

## Reorder Electricity to group energy sources visually in the plot
neworder <- c("i40.11.a","i40.11.b","i40.11.f","i40.11.c","i40.11.g","i40.11.d","i40.11.e.1","i40.11.e.2","i40.11.h.1","i40.11.h.2","i40.11.i","i40.11.j","i40.11.k")
emp_sources <- emp_sources %>% slice(order(factor(Electricity, levels = neworder)))

## Export results
fwrite(emp_sources, file = paste0(respath,"emp_sources.csv"), sep=";", dec=",")

## Round numbers
emp_sources$Value <- round(emp_sources$Value, digits = 1)

## Make Electricity ordered factor
emp_sources$Electricity <- factor(emp_sources$Electricity, levels = rev(unique(emp_sources$Electricity)), ordered = TRUE)

## Plot results
plot_sources <- ggplot(emp_sources) +
  geom_bar(aes(x = as.factor(Year), y = Value, fill = Electricity),
           stat = "identity",
           position = "stack",
           width = 0.8) + ## ?position_stack() - position of bars
  scale_x_discrete(expand = c(0.1, 0.1)) +
  xlab(NULL) +
  ylab("Poèet pracovních míst (1000)") +
  scale_fill_tableau(palette = "Classic Cyclic",
                     direction = -1,
                     name = "Zdroje",
                     labels = c("Geotermální energie","Pøílivová energie","Solární termální energie","Maloplošná fotovoltaika (<1MW)",
                                "Velkoplošná fotovoltaika (>1MW)","Vìtrná energie (offshore)","Vìtrná energie (onshore)","Vodní energie",
                                "Biomasa, bioplyn a odpad","Jaderná energie","Ropa a další ropné deriváty","Plyn","Uhlí")) +
  ggtitle("Poptávka po práci - elektroenergetika a dodavatelské øetìzce",
          "Èlenìní dle jednotlivých zdrojù energie") +
  theme(strip.text.x = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 16))

## Export plot
png(paste0(respath,"Plots/emp_sources.png"), width = 800, height = 500)
print(plot_sources)
dev.off()


##########################################################################
# 10.6. Organise GVA total results and plot

## Aggregate by total values (and nothing else)
gva_total <- results
gva_total <- gva_total %>% 
  filter(Country.Origin %in% "CZ") %>%
  filter(Extension %in% "gva") %>%
  group_by(Country.Destination, Year) %>%
  summarize(Value = sum(Value))

## Convert values in EUR to CZK
gva_total$Value <- gva_total$Value * exchange

## Export results
fwrite(gva_total, file = paste0(respath,"gva_total.csv"), sep=";", dec=",")

## Mutate to plot stacked bar chart with value labels and round numbers
gva_total <- gva_total %>%
  group_by(Year) %>%
  mutate(lab_ypos = cumsum(Value) - 0.5 * Value)
gva_total$Value <- round(gva_total$Value, digits = 1)
gva_total$lab_ypos <- round(gva_total$lab_ypos, digits = 1)

## Plot results
plot_total <- ggplot(gva_total) +
  geom_bar(aes(x = as.factor(Year), y = Value),
           stat = "identity",
           width = 0.8) + ## ?position_stack() - position of bars
  geom_text(aes(x = as.factor(Year), y = lab_ypos, label = Value),
            color = "white",
            size = 5,
            vjust = 0.3) +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  xlab(NULL) +
  ylab("Milion Kè") +
  ggtitle("Hrubá pøidaná hodnota - elektroenergetika a dodavatelské øetìzce",
          "Celkem") +
  theme(strip.text.x = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 16))

## Export plot
png(paste0(respath,"Plots/gva_total.png"), width = 800, height = 500)
print(plot_total)
dev.off()


##########################################################################
# 10.7. Organise GVA results by sectors (winners vs. losers sorted by 2050; average over 2015-2050) and plot

## Aggregate by sectors (and nothing else)
gva_sectors <- results
gva_sectors <- gva_sectors %>% 
  filter(Country.Origin %in% "CZ") %>%
  filter(Extension %in% "gva") %>%
  group_by(Country.Destination, Year, Industry.Code, Industry.Name) %>%
  summarize(Value = sum(Value))

## Rename sectors in Czech
gva_sectors$Industry.Name <- c(rep(industries$cz))

## Convert values in EUR to CZK
gva_sectors$Value <- gva_sectors$Value * exchange

## Cast ("unmelt") to match values from different years next to each other, calculate Change columns (percentage change compared to 2015)
gva_sectors <- dcast(gva_sectors, Country.Destination + Industry.Name + Industry.Code~Year, value.var = "Value")
for (year in years) {
  if (year>2015) {
    gva_sectors[,paste0("Change.",year)] <- gva_sectors[,year] - gva_sectors[,"2015"]
  }
}

## Delete Value columns
for (year in years) {
  gva_sectors[,year] <- NULL
}

## Sort by Change.2050 values in a descending order and change colnames
gva_sectors <- gva_sectors %>% 
  as_tibble() %>% 
  arrange(desc(gva_sectors$Change.2050))
colnames(gva_sectors)[4:10] <- c("2020","2025","2030","2035","2040","2045","2050")

## Melt and adjust colnames
gva_sectors <- melt(gva_sectors)
colnames(gva_sectors)[4] <- "Year"
colnames(gva_sectors)[5] <- "Change"

## Export results
fwrite(gva_sectors, file = paste0(respath,"gva_sectors.csv"), sep=";", dec=",")

## Round numbers
gva_sectors$Change <- round(gva_sectors$Change, digits = 1)

## Plot results for 10 biggest winners
plot_sectors_win <- ggplot(gva_sectors[c(991:1000,826:835,661:670,496:505,331:340,166:175,1:10),], aes(x = Industry.Name, y = Change, fill = as.factor(Year))) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.9)) + ## ?position_dodge() - position of bars
  scale_x_discrete(limits = gva_sectors$Industry.Name[1:10], ## avoid automatically (alphabetically) sorting the x-axis
                   labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(expand = c(0.2, 0.2)) +
  xlab(NULL) +
  ylab("Milion Kè") +
  ggtitle("Hrubá pøidaná hodnota - elektroenergetika a dodavatelské øetìzce",
          "10 odvìtví s nejvìtším nárùstem oproti roku 2015 (seøazeno dle hodnot v roce 2050)") +
  scale_fill_manual(values = c(rep_len(c("#A6DF83","#8BD873","#6FD164","#56C95B","#48C15E","#3AB863","#2DAF6A"), length(unique(gva_sectors$Industry.Name))-1)), ## colours
                    name = "Rok",
                    labels = c("2020","2025","2030","2035","2040","2045","2050")) +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 16),
        legend.position = "bottom",
        legend.direction = "horizontal",
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 16)) +
  guides(fill = guide_legend(ncol = 7))

## Export plot
png(paste0(respath,"Plots/gva_sectors_win.png"), width = 800, height = 500)
print(plot_sectors_win)
dev.off()

## Plot results for 10 biggest losers
plot_sectors_los <- ggplot(gva_sectors[c(1155:1146,990:981,825:816,660:651,495:486,330:321,165:156),], aes(Industry.Name, Change, fill = as.factor(Year))) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.9)) + ## ?position_dodge() - position of bars
  scale_x_discrete(limits = gva_sectors$Industry.Name[1146:1155], ## avoid automatically (alphabetically) sorting the x-axis
                   labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(expand = c(0.2, 0.2)) +
  xlab(NULL) +
  ylab("Milion Kè") +
  ggtitle("Hrubá pøidaná hodnota - elektroenergetika a dodavatelské øetìzce",
          "10 odvìtví s nejvìtším poklesem oproti roku 2015 (seøazeno dle hodnot v roce 2050)") +
  scale_fill_manual(values = c(rep_len(c("#B969A1","#AC5080","#9F385B","#912032","#830B08","#770019","#68002B"), length(unique(gva_sectors$Industry.Name))-1)), ## colours
                    name = "Rok",
                    labels = c("2020","2025","2030","2035","2040","2045","2050")) +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 16),
        legend.position = "bottom",
        legend.direction = "horizontal",
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 16)) +
  guides(fill = guide_legend(ncol = 7))

## Export plot
png(paste0(respath,"Plots/gva_sectors_los.png"), width = 800, height = 500)
print(plot_sectors_los)
dev.off()

## Calculate average employment change over years
gva_sectors_av <- gva_sectors

## Cast ("unmelt"), calculate Average column, delete Change columns, sort by Average column and leave only 10 first and last rows
gva_sectors_av <- dcast(gva_sectors_av, Country.Destination + Industry.Name + Industry.Code~Year, value.var = "Change")
gva_sectors_av$Average <- round(rowMeans(gva_sectors_av[,4:10]), digits = 1)
gva_sectors_av[,4:10] <- NULL
gva_sectors_av <- gva_sectors_av[order(gva_sectors_av$Average, decreasing = TRUE),]
gva_sectors_av <- gva_sectors_av[c(1:10,156:165),]

## Make Industry.Name ordered factor
gva_sectors_av$Industry.Name <- factor(gva_sectors_av$Industry.Name, levels = rev(unique(gva_sectors_av$Industry.Name)), ordered = TRUE)

## Export results
fwrite(gva_sectors_av, file = paste0(respath,"gva_sectors_av.csv"), sep=";", dec=",")

## Plot results for average 10 biggest winners and 10 biggest losers
plot_sectors_av <- ggplot(gva_sectors_av, aes(Industry.Name, Average, fill = as.factor(desc(Industry.Name)))) +
  geom_bar(stat = "identity",
           width = 0.9) +
  scale_x_discrete(limits = gva_sectors_av$Industry.Name, ## avoid automatically (alphabetically) sorting the x-axis
                   labels = function(x) str_wrap(x, width = 60),
                   expand = c(0.23, 0.23)) +
  scale_y_continuous(expand = c(0.1, 0.1)) +
  xlab(NULL) +
  ylab("Milion Kè") +
  ggtitle("Hrubá pøidaná hodnota - elektroenergetika a dodavatelské øetìzce",
          "10 odvìtví s nejvìtším nárùstem a poklesem (prùmìrná hodnota mezi roky 2015-2050)") +
  scale_fill_manual(values = c(rep(c("#2DAF6A"), times = 10),rep(c("#68002B"), times = 10))) + ## colours
  geom_text(aes(y = Average + 0.1 * sign(Average), label = round(Average, digits = 1))) +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 11),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 16))

## Export plot
png(paste0(respath,"Plots/gva_sectors_av.png"), width = 1000, height = 600)
print(plot_sectors_av)
dev.off()


##########################################################################
# 10.8. Organise GVA results by electricity sources and plot

## Aggregate results by electricity sources (and nothing else)
gva_sources <- results
gva_sources <- gva_sources %>% 
  filter(Country.Origin %in% "CZ") %>%
  filter(Extension %in% "gva") %>%
  group_by(Country.Origin, Country.Destination, Year, Electricity) %>%
  summarize(Value = sum(Value))

## Convert values in EUR to CZK
gva_sources$Value <- gva_sources$Value * exchange

## Reorder Electricity to group energy sources visually in the plot
neworder <- c("i40.11.a","i40.11.b","i40.11.f","i40.11.c","i40.11.g","i40.11.d","i40.11.e.1","i40.11.e.2","i40.11.h.1","i40.11.h.2","i40.11.i","i40.11.j","i40.11.k")
gva_sources <- gva_sources %>% slice(order(factor(Electricity, levels = neworder)))

## Export results
fwrite(gva_sources, file = paste0(respath,"gva_sources.csv"), sep=";", dec=",")

## Round numbers
gva_sources$Value <- round(gva_sources$Value, digits = 1)

## Make Electricity ordered factor
gva_sources$Electricity <- factor(gva_sources$Electricity, levels = rev(unique(gva_sources$Electricity)), ordered = TRUE)

## Plot results
plot_sources <- ggplot(gva_sources) +
  geom_bar(aes(x = as.factor(Year), y = Value, fill = Electricity),
           stat = "identity",
           position = "stack",
           width = 0.8) + ## ?position_stack() - position of bars
  scale_x_discrete(expand = c(0.1, 0.1)) +
  xlab(NULL) +
  ylab("Milion Kè") +
  scale_fill_tableau(palette = "Classic Cyclic",
                     direction = -1,
                     name = "Zdroje",
                     labels = c("Geotermální energie","Pøílivová energie","Solární termální energie","Maloplošná fotovoltaika (<1MW)",
                                "Velkoplošná fotovoltaika (>1MW)","Vìtrná energie (offshore)","Vìtrná energie (onshore)","Vodní energie",
                                "Biomasa, bioplyn a odpad","Jaderná energie","Ropa a další ropné deriváty","Plyn","Uhlí")) +
  ggtitle("Hrubá pøidaná hodnota - elektroenergetika a dodavatelské øetìzce",
          "Èlenìní dle jednotlivých zdrojù energie") +
  theme(strip.text.x = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 16))

## Export plot
png(paste0(respath,"Plots/gva_sources.png"), width = 800, height = 500)
print(plot_sources)
dev.off()


##########################################################################
# 10.9. Organise GDP total results and plot

## Aggregate by total values (and nothing else)
gdp_total <- results
gdp_total <- gdp_total %>% 
  filter(Country.Origin %in% "CZ") %>%
  filter(Extension %in% "gdp") %>%
  group_by(Country.Destination, Year) %>%
  summarize(Value = sum(Value))

## Convert values in EUR to CZK
gdp_total$Value <- gdp_total$Value * exchange

## Export results
fwrite(gdp_total, file = paste0(respath,"gdp_total.csv"), sep=";", dec=",")

## Mutate to plot stacked bar chart with value labels and round numbers
gdp_total <- gdp_total %>%
  group_by(Year) %>%
  mutate(lab_ypos = cumsum(Value) - 0.5 * Value)
gdp_total$Value <- round(gdp_total$Value, digits = 1)
gdp_total$lab_ypos <- round(gdp_total$lab_ypos, digits = 1)

## Plot results
plot_total <- ggplot(gdp_total) +
  geom_bar(aes(x = as.factor(Year), y = Value),
           stat = "identity",
           width = 0.8) + ## ?position_stack() - position of bars
  geom_text(aes(x = as.factor(Year), y = lab_ypos, label = Value),
            color = "white",
            size = 5,
            vjust = 0.3) +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  xlab(NULL) +
  ylab("Milion Kè") +
  ggtitle("Hrubý domácí produkt - elektroenergetika a dodavatelské øetìzce",
          "Celkem") +
  theme(strip.text.x = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 16))

## Export plot
png(paste0(respath,"Plots/gdp_total.png"), width = 800, height = 500)
print(plot_total)
dev.off()


##########################################################################
# 10.10. Organise GDP results by sectors (winners vs. losers sorted by 2050; average over 2015-2050) and plot

## Aggregate by sectors (and nothing else)
gdp_sectors <- results
gdp_sectors <- gdp_sectors %>% 
  filter(Country.Origin %in% "CZ") %>%
  filter(Extension %in% "gdp") %>%
  group_by(Country.Destination, Year, Industry.Code, Industry.Name) %>%
  summarize(Value = sum(Value))

## Rename sectors in Czech
gdp_sectors$Industry.Name <- c(rep(industries$cz))

## Convert values in EUR to CZK
gdp_sectors$Value <- gdp_sectors$Value * exchange

## Cast ("unmelt") to match values from different years next to each other, calculate Change columns (percentage change compared to 2015)
gdp_sectors <- dcast(gdp_sectors, Country.Destination + Industry.Name + Industry.Code~Year, value.var = "Value")
for (year in years) {
  if (year>2015) {
    gdp_sectors[,paste0("Change.",year)] <- gdp_sectors[,year] - gdp_sectors[,"2015"]
  }
}

## Delete Value columns
for (year in years) {
  gdp_sectors[,year] <- NULL
}

## Sort by Change.2050 values in a descending order and change colnames
gdp_sectors <- gdp_sectors %>% 
  as_tibble() %>% 
  arrange(desc(gdp_sectors$Change.2050))
colnames(gdp_sectors)[4:10] <- c("2020","2025","2030","2035","2040","2045","2050")

## Melt and adjust colnames
gdp_sectors <- melt(gdp_sectors)
colnames(gdp_sectors)[4] <- "Year"
colnames(gdp_sectors)[5] <- "Change"

## Export results
fwrite(gdp_sectors, file = paste0(respath,"gdp_sectors.csv"), sep=";", dec=",")

## Round numbers
gdp_sectors$Change <- round(gdp_sectors$Change, digits = 1)

## Plot results for 10 biggest winners
plot_sectors_win <- ggplot(gdp_sectors[c(991:1000,826:835,661:670,496:505,331:340,166:175,1:10),], aes(x = Industry.Name, y = Change, fill = as.factor(Year))) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.9)) + ## ?position_dodge() - position of bars
  scale_x_discrete(limits = gdp_sectors$Industry.Name[1:10], ## avoid automatically (alphabetically) sorting the x-axis
                   labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(expand = c(0.2, 0.2)) +
  xlab(NULL) +
  ylab("Milion Kè") +
  ggtitle("Hrubý domácí produkt - elektroenergetika a dodavatelské øetìzce",
          "10 odvìtví s nejvìtším nárùstem oproti roku 2015 (seøazeno dle hodnot v roce 2050)") +
  scale_fill_manual(values = c(rep_len(c("#A6DF83","#8BD873","#6FD164","#56C95B","#48C15E","#3AB863","#2DAF6A"), length(unique(gdp_sectors$Industry.Name))-1)), ## colours
                    name = "Rok",
                    labels = c("2020","2025","2030","2035","2040","2045","2050")) +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 16),
        legend.position = "bottom",
        legend.direction = "horizontal",
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 16)) +
  guides(fill = guide_legend(ncol = 7))

## Export plot
png(paste0(respath,"Plots/gdp_sectors_win.png"), width = 800, height = 500)
print(plot_sectors_win)
dev.off()

## Plot results for 10 biggest losers
plot_sectors_los <- ggplot(gdp_sectors[c(1155:1146,990:981,825:816,660:651,495:486,330:321,165:156),], aes(Industry.Name, Change, fill = as.factor(Year))) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.9)) + ## ?position_dodge() - position of bars
  scale_x_discrete(limits = gdp_sectors$Industry.Name[1146:1155], ## avoid automatically (alphabetically) sorting the x-axis
                   labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(expand = c(0.2, 0.2)) +
  xlab(NULL) +
  ylab("Milion Kè") +
  ggtitle("Hrubá pøidaná hodnota - elektroenergetika a dodavatelské øetìzce",
          "10 odvìtví s nejvìtším poklesem oproti roku 2015 (seøazeno dle hodnot v roce 2050)") +
  scale_fill_manual(values = c(rep_len(c("#B969A1","#AC5080","#9F385B","#912032","#830B08","#770019","#68002B"), length(unique(gdp_sectors$Industry.Name))-1)), ## colours
                    name = "Rok",
                    labels = c("2020","2025","2030","2035","2040","2045","2050")) +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 16),
        legend.position = "bottom",
        legend.direction = "horizontal",
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 16)) +
  guides(fill = guide_legend(ncol = 7))

## Export plot
png(paste0(respath,"Plots/gdp_sectors_los.png"), width = 800, height = 500)
print(plot_sectors_los)
dev.off()

## Calculate average employment change over years
gdp_sectors_av <- gdp_sectors

## Cast ("unmelt"), calculate Average column, delete Change columns, sort by Average column and leave only 10 first and last rows
gdp_sectors_av <- dcast(gdp_sectors_av, Country.Destination + Industry.Name + Industry.Code~Year, value.var = "Change")
gdp_sectors_av$Average <- round(rowMeans(gdp_sectors_av[,4:10]), digits = 1)
gdp_sectors_av[,4:10] <- NULL
gdp_sectors_av <- gdp_sectors_av[order(gdp_sectors_av$Average, decreasing = TRUE),]
gdp_sectors_av <- gdp_sectors_av[c(1:10,156:165),]

## Make Industry.Name ordered factor
gdp_sectors_av$Industry.Name <- factor(gdp_sectors_av$Industry.Name, levels = rev(unique(gdp_sectors_av$Industry.Name)), ordered = TRUE)

## Export results
fwrite(gdp_sectors_av, file = paste0(respath,"gdp_sectors_av.csv"), sep=";", dec=",")

## Plot results for average 10 biggest winners and 10 biggest losers
plot_sectors_av <- ggplot(gdp_sectors_av, aes(Industry.Name, Average, fill = as.factor(desc(Industry.Name)))) +
  geom_bar(stat = "identity",
           width = 0.9) +
  scale_x_discrete(limits = gdp_sectors_av$Industry.Name, ## avoid automatically (alphabetically) sorting the x-axis
                   labels = function(x) str_wrap(x, width = 60),
                   expand = c(0.23, 0.23)) +
  scale_y_continuous(expand = c(0.1, 0.1)) +
  xlab(NULL) +
  ylab("Milion Kè") +
  ggtitle("Hrubý domácí produkt - elektroenergetika a dodavatelské øetìzce",
          "10 odvìtví s nejvìtším nárùstem a poklesem (prùmìrná hodnota mezi roky 2015-2050)") +
  scale_fill_manual(values = c(rep(c("#2DAF6A"), times = 10),rep(c("#68002B"), times = 10))) + ## colours
  geom_text(aes(y = Average + 0.1 * sign(Average), label = round(Average, digits = 1))) +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 11),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 16))

## Export plot
png(paste0(respath,"Plots/gdp_sectors_av.png"), width = 1000, height = 600)
print(plot_sectors_av)
dev.off()


##########################################################################
# 10.11. Organise GDP results by electricity sources and plot

## Aggregate results by electricity sources (and nothing else)
gdp_sources <- results
gdp_sources <- gdp_sources %>% 
  filter(Country.Origin %in% "CZ") %>%
  filter(Extension %in% "gdp") %>%
  group_by(Country.Origin, Country.Destination, Year, Electricity) %>%
  summarize(Value = sum(Value))

## Convert values in EUR to CZK
gdp_sources$Value <- gdp_sources$Value * exchange

## Reorder Electricity to group energy sources visually in the plot
neworder <- c("i40.11.a","i40.11.b","i40.11.f","i40.11.c","i40.11.g","i40.11.d","i40.11.e.1","i40.11.e.2","i40.11.h.1","i40.11.h.2","i40.11.i","i40.11.j","i40.11.k")
gdp_sources <- gdp_sources %>% slice(order(factor(Electricity, levels = neworder)))

## Export results
fwrite(gdp_sources, file = paste0(respath,"gdp_sources.csv"), sep=";", dec=",")

## Round numbers
gdp_sources$Value <- round(gdp_sources$Value, digits = 1)

## Make Electricity ordered factor
gdp_sources$Electricity <- factor(gdp_sources$Electricity, levels = rev(unique(gdp_sources$Electricity)), ordered = TRUE)

## Plot results
plot_sources <- ggplot(gdp_sources) +
  geom_bar(aes(x = as.factor(Year), y = Value, fill = Electricity),
           stat = "identity",
           position = "stack",
           width = 0.8) + ## ?position_stack() - position of bars
  scale_x_discrete(expand = c(0.1, 0.1)) +
  xlab(NULL) +
  ylab("Milion Kè") +
  scale_fill_tableau(palette = "Classic Cyclic",
                     direction = -1,
                     name = "Zdroje",
                     labels = c("Geotermální energie","Pøílivová energie","Solární termální energie","Maloplošná fotovoltaika (<1MW)",
                                "Velkoplošná fotovoltaika (>1MW)","Vìtrná energie (offshore)","Vìtrná energie (onshore)","Vodní energie",
                                "Biomasa, bioplyn a odpad","Jaderná energie","Ropa a další ropné deriváty","Plyn","Uhlí")) +
  ggtitle("Hrubý domácí produkt - elektroenergetika a dodavatelské øetìzce",
          "Èlenìní dle jednotlivých zdrojù energie") +
  theme(strip.text.x = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 16))

## Export plot
png(paste0(respath,"Plots/gdp_sources.png"), width = 800, height = 500)
print(plot_sources)
dev.off()


##########################################################################
# 10.12. Organise GHG total results and plot

## Aggregate by total values (and nothing else)
ghg_total <- results
ghg_total <- ghg_total %>%
  filter(Country.Origin %in% "CZ") %>%
  filter(Extension %in% "ghg") %>%
  group_by(Country.Destination, Year) %>%
  summarize(Value = sum(Value))

## Turn Kg into Tons
ghg_total$Value <- ghg_total$Value/1000

## Export results
fwrite(ghg_total, file = paste0(respath,"ghg_total.csv"), sep=";", dec=",")

## Mutate to plot stacked bar chart with value labels and round numbers
ghg_total <- ghg_total %>%
  mutate(lab_ypos = 0.5 * Value)
ghg_total$Value <- round(ghg_total$Value, digits = 0)
ghg_total$lab_ypos <- round(ghg_total$lab_ypos, digits = 0)

## Plot results
plot_total <- ggplot(ghg_total) +
  geom_bar(aes(x = as.factor(Year), y = Value),
           stat = "identity",
           width = 0.8) +
  geom_text(aes(x = as.factor(Year), y = lab_ypos, label = Value),
            colour = "white",
            size = 4.5,
            vjust = 0.3) +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  scale_y_continuous(expand = c(0.1, 0.1)) +
  xlab(NULL) +
  ylab("Kg CO2e (1000)") +
  ggtitle("Emise skleníkových plynù - elektroenergetika a dodavatelské øetìzce",
          "Celkem") +
  theme(strip.text.x = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 16))

## Export plot
png(paste0(respath,"Plots/ghg_total.png"), width = 800, height = 500)
print(plot_total)
dev.off()


##########################################################################
# 10.13. Organise GHG results by electricity sources and plot

## Aggregate results by electricity sources (and nothing else)
ghg_sources <- results
ghg_sources <- ghg_sources %>%
  filter(Country.Origin %in% "CZ") %>%
  filter(Extension %in% "ghg") %>%
  group_by(Country.Origin, Country.Destination, Year, Electricity) %>%
  summarize(Value = sum(Value))

## Turn Kg into Tons
ghg_sources$Value <- ghg_sources$Value/1000

## Reorder Electricity to group energy sources visually in the plot
neworder <- c("i40.11.a","i40.11.b","i40.11.f","i40.11.c","i40.11.g","i40.11.d","i40.11.e.1","i40.11.e.2","i40.11.h.1","i40.11.h.2","i40.11.i","i40.11.j","i40.11.k")
ghg_sources <- ghg_sources %>% slice(order(factor(Electricity, levels = neworder)))

## Export results
fwrite(ghg_sources, file = paste0(respath,"ghg_sources.csv"), sep=";", dec=",")

## Round numbers
ghg_sources$Value <- round(ghg_sources$Value, digits = 1)

## Make Electricity ordered factor
ghg_sources$Electricity <- factor(ghg_sources$Electricity, levels = rev(unique(ghg_sources$Electricity)), ordered = TRUE)

## Plot results
plot_sources <- ggplot(ghg_sources) +
  geom_bar(aes(x = as.factor(Year), y = Value, fill = Electricity),
           stat = "identity",
           position = "stack",
           width = 0.8) + ## ?position_stack() - position of bars
  scale_x_discrete(expand = c(0.1, 0.1)) +
  xlab(NULL) +
  ylab("Kg CO2e (1000)") +
  scale_fill_tableau(palette = "Classic Cyclic",
                     direction = -1,
                     name = "Zdroje",
                     labels = c("Geotermální energie","Pøílivová energie","Solární termální energie","Maloplošná fotovoltaika (<1MW)",
                                "Velkoplošná fotovoltaika (>1MW)","Vìtrná energie (offshore)","Vìtrná energie (onshore)","Vodní energie",
                                "Biomasa, bioplyn a odpad","Jaderná energie","Ropa a další ropné deriváty","Plyn","Uhlí")) +
  ggtitle("Emise skleníkových plynù - elektroenergetika a dodavatelské øetìzce",
          "Èlenìní dle jednotlivých zdrojù energie") +
  theme(strip.text.x = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 16))

## Export plot
png(paste0(respath,"Plots/ghg_sources.png"), width = 800, height = 500)
print(plot_sources)
dev.off()

} # end of condition p2



##########################################################################
# 11. Load results for the whole energy sector, organise data and plot
##########################################################################

## RUN THIS PART (11.) ONLY IF WE MODEL THE WHOLE ENERGY SECTOR
if (p2==1) {

## RESULTS - THE WHOLE ENERGY SECTOR:

## 1) EMP
## a) Total
## b) By skill levels and gender
## c) By sectors
## d) By each energy sector

## 2) GVA, GDP
## a) Total
## b) By sectors
## c) By each energy sector

## 3) GHG
## a) Total
## b) By each energy sector


## Domestic: Country.Origin = CZ

## Note that I can only calculate "pure" domestic = where Country.Destination matches Country.Origin.
## The results thus omit effects related to Czechia's electricity industries that are induced outside CZ (Country.Destination = RoW).
## However, the electricity mix outside CZ does not change in the model, so this employment would remain constant over time anyway.


##########################################################################
# 11.1. Organise EMP total results and plot

## Aggregate by total values (and nothing else)
emp_total <- results
emp_total <- emp_total %>% 
  filter(Country.Origin %in% "CZ") %>%
  filter(Extension %in% "emp") %>%
  group_by(Country.Destination, Year) %>%
  summarize(Value = sum(Value))

## Export results
fwrite(emp_total, file = paste0(respath,"emp_total.csv"), sep=";", dec=",")

## Mutate to plot stacked bar chart with value labels and round numbers
emp_total <- emp_total %>%
  group_by(Year) %>%
  mutate(lab_ypos = cumsum(Value) - 0.5 * Value)
emp_total$Value <- round(emp_total$Value, digits = 1)
emp_total$lab_ypos <- round(emp_total$lab_ypos, digits = 1)

## Plot results
plot_total <- ggplot(emp_total) +
  geom_bar(aes(x = as.factor(Year), y = Value),
           stat = "identity",
           width = 0.8) + ## ?position_stack() - position of bars
  geom_text(aes(x = as.factor(Year), y = lab_ypos, label = Value),
            color = "white",
            size = 5,
            vjust = 0.3) +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  xlab(NULL) +
  ylab("Poèet pracovních míst (1000)") +
  ggtitle("Poptávka po práci - energetický sektor a dodavatelské øetìzce",
          "Celkem") +
  theme(strip.text.x = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 16))

## Export plot
png(paste0(respath,"Plots/emp_total.png"), width = 800, height = 500)
print(plot_total)
dev.off()


##########################################################################
# 11.2. Organise EMP results by skill levels and gender and plot

## Aggregate by distributional employment categories (and nothing else)
emp_distr <- results
emp_distr <- emp_distr %>% 
  filter(Country.Origin %in% "CZ") %>%
  filter(Extension %in% "emp") %>%
  group_by(Country.Destination, Year, Indicator) %>%
  summarize(Value = sum(Value))

## Reorder Indicator to make the right order in the plot
neworder <- c("Employment_Low-skilled female","Employment_Medium-skilled female","Employment_High-skilled female",
              "Employment_Low-skilled male","Employment_Medium-skilled male","Employment_High-skilled male")
emp_distr <- emp_distr %>% slice(order(factor(Indicator, levels = neworder)))

## Export results
fwrite(emp_distr, file = paste0(respath,"emp_distr.csv"), sep=";", dec=",")

## Mutate to plot stacked bar chart with value labels and round numbers
emp_distr <- emp_distr %>%
  group_by(Year) %>%
  mutate(lab_ypos = cumsum(Value) - 0.5 * Value)
emp_distr$Value <- round(emp_distr$Value, digits = 1)
emp_distr$lab_ypos <- round(emp_distr$lab_ypos, digits = 1)

## Make Indicator ordered
emp_distr$Indicator <- factor(emp_distr$Indicator, levels = rev(unique(emp_distr$Indicator)), ordered = TRUE)

## Plot results
plot_distr <- ggplot(emp_distr) +
  geom_bar(aes(x = as.factor(Year), y = Value, fill = Indicator),
           stat = "identity",
           position = "stack",
           width = 0.8) + ## ?position_stack() - position of bars
  geom_text(aes(x = as.factor(Year), y = lab_ypos, label = Value, group = Indicator),
            color = "white",
            size = 5,
            vjust = 0.3) +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  scale_y_continuous(expand = c(0.1, 0.1)) +
  xlab(NULL) +
  ylab("Poèet pracovních míst (1000)") +
  scale_fill_manual(values = c("#632E4A","#763966","#894385","#117E18","#2A9617","#51AC1D"),
                    name = "Kategorie",
                    labels = c("Muži - vysoce kvalifikovaná", "Muži - støednì kvalifikovaná", "Muži - nízce kvalifikovaná",
                               "Ženy - vysoce kvalifikovaná", "Ženy - støednì kvalifikovaná", "Ženy - nízce kvalifikovaná")) +
  ggtitle("Poptávka po práci - energetický sektor a dodavatelské øetìzce",
          "Èlenìní dle úrovnì kvalifikace a pohlaví") +
  theme(strip.text.x = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 15),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 16))

## Export plot
png(paste0(respath,"Plots/emp_distr.png"), width = 800, height = 500)
print(plot_distr)
dev.off()


##########################################################################
# 11.3. Organise EMP results by sectors (winners vs. losers sorted by 2050; average over 2015-2050) and plot

## Aggregate by sectors (and nothing else)
emp_sectors <- results
emp_sectors <- emp_sectors %>% 
  filter(Country.Origin %in% "CZ") %>%
  filter(Extension %in% "emp") %>%
  group_by(Country.Destination, Year, Industry.Code, Industry.Name) %>%
  summarize(Value = sum(Value))

## Rename sectors in Czech
emp_sectors$Industry.Name <- c(rep(industries$cz))

## Cast ("unmelt") to match values from different years next to each other, calculate Change columns (percentage change compared to 2015)
emp_sectors <- dcast(emp_sectors, Country.Destination + Industry.Name + Industry.Code~Year, value.var = "Value")
for (year in years) {
  if (year>2015) {
    emp_sectors[,paste0("Change.",year)] <- emp_sectors[,year] - emp_sectors[,"2015"]
  }
}

## Delete Value columns
for (year in years) {
  emp_sectors[,year] <- NULL
}

## Sort by Change.2050 values in a descending order and change colnames
emp_sectors <- emp_sectors %>% 
  as_tibble() %>% 
  arrange(desc(emp_sectors$Change.2050))
colnames(emp_sectors)[4:10] <- c("2020","2025","2030","2035","2040","2045","2050")

## Melt and adjust colnames
emp_sectors <- melt(emp_sectors)
colnames(emp_sectors)[4] <- "Year"
colnames(emp_sectors)[5] <- "Change"

## Export results
fwrite(emp_sectors, file = paste0(respath,"emp_sectors.csv"), sep=";", dec=",")

## Round numbers
emp_sectors$Change <- round(emp_sectors$Change, digits = 1)

## Plot results for 10 biggest winners
plot_sectors_win <- ggplot(emp_sectors[c(991:1000,826:835,661:670,496:505,331:340,166:175,1:10),], aes(x = Industry.Name, y = Change, fill = as.factor(Year))) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.9)) + ## ?position_dodge() - position of bars
  scale_x_discrete(limits = emp_sectors$Industry.Name[1:10], ## avoid automatically (alphabetically) sorting the x-axis
                   labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(expand = c(0.2, 0.2)) +
  xlab(NULL) +
  ylab("Poèet pracovních míst (1000)") +
  ggtitle("Poptávka po práci - energetický sektor a dodavatelské øetìzce",
          "10 odvìtví s nejvìtším nárùstem oproti roku 2015 (seøazeno dle hodnot v roce 2050)") +
  scale_fill_manual(values = c(rep_len(c("#A6DF83","#8BD873","#6FD164","#56C95B","#48C15E","#3AB863","#2DAF6A"), length(unique(emp_sectors$Industry.Name))-1)), ## colours
                    name = "Rok",
                    labels = c("2020","2025","2030","2035","2040","2045","2050")) +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 16),
        legend.position = "bottom",
        legend.direction = "horizontal",
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 16)) +
  guides(fill = guide_legend(ncol = 7))

## Export plot
png(paste0(respath,"Plots/emp_sectors_win.png"), width = 800, height = 500)
print(plot_sectors_win)
dev.off()

## Plot results for 10 biggest losers
plot_sectors_los <- ggplot(emp_sectors[c(1155:1146,990:981,825:816,660:651,495:486,330:321,165:156),], aes(Industry.Name, Change, fill = as.factor(Year))) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.9)) + ## ?position_dodge() - position of bars
  scale_x_discrete(limits = emp_sectors$Industry.Name[1146:1155], ## avoid automatically (alphabetically) sorting the x-axis
                   labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(expand = c(0.2, 0.2)) +
  xlab(NULL) +
  ylab("Poèet pracovních míst (1000)") +
  ggtitle("Poptávka po práci - energetický sektor a dodavatelské øetìzce",
          "10 odvìtví s nejvìtším poklesem oproti roku 2015 (seøazeno dle hodnot v roce 2050)") +
  scale_fill_manual(values = c(rep_len(c("#B969A1","#AC5080","#9F385B","#912032","#830B08","#770019","#68002B"), length(unique(emp_sectors$Industry.Name))-1)), ## colours
                    name = "Rok",
                    labels = c("2020","2025","2030","2035","2040","2045","2050")) +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 16),
        legend.position = "bottom",
        legend.direction = "horizontal",
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 16)) +
  guides(fill = guide_legend(ncol = 7))

## Export plot
png(paste0(respath,"Plots/emp_sectors_los.png"), width = 800, height = 500)
print(plot_sectors_los)
dev.off()

## Calculate average employment change over years
emp_sectors_av <- emp_sectors

## Cast ("unmelt"), calculate Average column, delete Change columns, sort by Average column and leave only 10 first and last rows
emp_sectors_av <- dcast(emp_sectors_av, Country.Destination + Industry.Name + Industry.Code~Year, value.var = "Change")
emp_sectors_av$Average <- round(rowMeans(emp_sectors_av[,4:10]), digits = 1)
emp_sectors_av[,4:10] <- NULL
emp_sectors_av <- emp_sectors_av[order(emp_sectors_av$Average, decreasing = TRUE),]
emp_sectors_av <- emp_sectors_av[c(1:10,156:165),]

## Make Industry.Name ordered factor
emp_sectors_av$Industry.Name <- factor(emp_sectors_av$Industry.Name, levels = rev(unique(emp_sectors_av$Industry.Name)), ordered = TRUE)

## Export results
fwrite(emp_sectors_av, file = paste0(respath,"emp_sectors_av.csv"), sep=";", dec=",")

## Plot results for average 10 biggest winners and 10 biggest losers
plot_sectors_av <- ggplot(emp_sectors_av, aes(Industry.Name, Average, fill = as.factor(desc(Industry.Name)))) +
  geom_bar(stat = "identity",
           width = 0.9) +
  scale_x_discrete(limits = emp_sectors_av$Industry.Name, ## avoid automatically (alphabetically) sorting the x-axis
                   labels = function(x) str_wrap(x, width = 60),
                   expand = c(0.23, 0.23)) +
  scale_y_continuous(expand = c(0.1, 0.1)) +
  xlab(NULL) +
  ylab("Poèet pracovních míst (1000)") +
  ggtitle("Poptávka po práci - energetický sektor a dodavatelské øetìzce",
          "10 odvìtví s nejvìtším nárùstem a poklesem (prùmìrná hodnota mezi roky 2015-2050)") +
  scale_fill_manual(values = c(rep(c("#2DAF6A"), times = 10),rep(c("#68002B"), times = 10))) + ## colours
  geom_text(aes(y = Average + 0.2 * sign(Average), label = round(Average, digits = 1))) +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 11),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 16))

## Export plot
png(paste0(respath,"Plots/emp_sectors_av.png"), width = 1000, height = 600)
print(plot_sectors_av)
dev.off()


##########################################################################
# 11.4. Organise EMP results by each energy sector and plot

## Aggregate results by each energy sector (and nothing else)
emp_fuel <- results
emp_fuel <- emp_fuel %>% 
  filter(Country.Origin %in% "CZ") %>%
  filter(Extension %in% "emp") %>%
  group_by(Country.Origin, Country.Destination, Year, Energy) %>%
  summarize(Value = sum(Value))

## Adapt Energy codes to group by energy sources (and not EXIOBASE sectors)
emp_fuel$Energy[grepl("i40.11", emp_fuel$Energy)] <- c("Elektøina")
emp_fuel$Energy[grepl("i90|i01.w.2|i24.d", emp_fuel$Energy)] <- c("Obnovitelné zdroje (biomasa, bioplyn, odpad)")
emp_fuel$Energy[grepl("i10|i23.1", emp_fuel$Energy)] <- c("Tuhá paliva")
emp_fuel$Energy[grepl("i23.2", emp_fuel$Energy)] <- c("Ropa")
emp_fuel$Energy[grepl("i40.3", emp_fuel$Energy)] <- c("Teplo")
emp_fuel$Energy[grepl("i40.2", emp_fuel$Energy)] <- c("Plyn")
emp_fuel <- emp_fuel %>% 
  group_by(Country.Origin, Country.Destination, Year, Energy) %>%
  summarize(Value = sum(Value))

## Reorder Energy to group the sectors visually in the plot
neworder <- c("Tuhá paliva","Ropa","Plyn","Teplo","Obnovitelné zdroje","Elektøina")
emp_fuel <- emp_fuel %>% slice(order(factor(Energy, levels = neworder)))

## Export results
fwrite(emp_fuel, file = paste0(respath,"emp_fuel.csv"), sep=";", dec=",")

## Round numbers
emp_fuel$Value <- round(emp_fuel$Value, digits = 1)

## Make Energy ordered factor
emp_fuel$Energy <- factor(emp_fuel$Energy, levels = rev(unique(emp_fuel$Energy)), ordered = TRUE)

## Plot results
plot_fuel <- ggplot(emp_fuel) +
  geom_bar(aes(x = as.factor(Year), y = Value, fill = Energy),
           stat = "identity",
           position = "stack",
           width = 0.8) + ## ?position_stack() - position of bars
  scale_x_discrete(expand = c(0.1, 0.1)) +
  xlab(NULL) +
  ylab("Poèet pracovních míst (1000)") +
  scale_fill_tableau(palette = "Classic Cyclic",
                     direction = -1,
                     name = "Zdroje") +
  ggtitle("Poptávka po práci - energetický sektor a dodavatelské øetìzce",
          "Èlenìní dle jednotlivých zdrojù energie") +
  theme(strip.text.x = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 16))

## Export plot
png(paste0(respath,"Plots/emp_fuel.png"), width = 800, height = 500)
print(plot_fuel)
dev.off()


##########################################################################
# 11.5. Organise GVA total results and plot

## Aggregate by total values (and nothing else)
gva_total <- results
gva_total <- gva_total %>% 
  filter(Country.Origin %in% "CZ") %>%
  filter(Extension %in% "gva") %>%
  group_by(Country.Destination, Year) %>%
  summarize(Value = sum(Value))

## Convert values in EUR to CZK
gva_total$Value <- gva_total$Value * exchange

## Export results
fwrite(gva_total, file = paste0(respath,"gva_total.csv"), sep=";", dec=",")

## Mutate to plot stacked bar chart with value labels and round numbers
gva_total <- gva_total %>%
  group_by(Year) %>%
  mutate(lab_ypos = cumsum(Value) - 0.5 * Value)
gva_total$Value <- round(gva_total$Value, digits = 1)
gva_total$lab_ypos <- round(gva_total$lab_ypos, digits = 1)

## Plot results
plot_total <- ggplot(gva_total) +
  geom_bar(aes(x = as.factor(Year), y = Value),
           stat = "identity",
           width = 0.8) + ## ?position_stack() - position of bars
  geom_text(aes(x = as.factor(Year), y = lab_ypos, label = Value),
            color = "white",
            size = 5,
            vjust = 0.3) +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  xlab(NULL) +
  ylab("Milion Kè") +
  ggtitle("Hrubá pøidaná hodnota - energetický sektor a dodavatelské øetìzce",
          "Celkem") +
  theme(strip.text.x = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 16))

## Export plot
png(paste0(respath,"Plots/gva_total.png"), width = 800, height = 500)
print(plot_total)
dev.off()


##########################################################################
# 11.6. Organise GVA results by sectors (winners vs. losers sorted by 2050; average over 2015-2050) and plot

## Aggregate by sectors (and nothing else)
gva_sectors <- results
gva_sectors <- gva_sectors %>% 
  filter(Country.Origin %in% "CZ") %>%
  filter(Extension %in% "gva") %>%
  group_by(Country.Destination, Year, Industry.Code, Industry.Name) %>%
  summarize(Value = sum(Value))

## Rename sectors in Czech
gva_sectors$Industry.Name <- c(rep(industries$cz))

## Convert values in EUR to CZK
gva_sectors$Value <- gva_sectors$Value * exchange

## Cast ("unmelt") to match values from different years next to each other, calculate Change columns (percentage change compared to 2015)
gva_sectors <- dcast(gva_sectors, Country.Destination + Industry.Name + Industry.Code~Year, value.var = "Value")
for (year in years) {
  if (year>2015) {
    gva_sectors[,paste0("Change.",year)] <- gva_sectors[,year] - gva_sectors[,"2015"]
  }
}

## Delete Value columns
for (year in years) {
  gva_sectors[,year] <- NULL
}

## Sort by Change.2050 values in a descending order and change colnames
gva_sectors <- gva_sectors %>% 
  as_tibble() %>% 
  arrange(desc(gva_sectors$Change.2050))
colnames(gva_sectors)[4:10] <- c("2020","2025","2030","2035","2040","2045","2050")

## Melt and adjust colnames
gva_sectors <- melt(gva_sectors)
colnames(gva_sectors)[4] <- "Year"
colnames(gva_sectors)[5] <- "Change"

## Export results
fwrite(gva_sectors, file = paste0(respath,"gva_sectors.csv"), sep=";", dec=",")

## Round numbers
gva_sectors$Change <- round(gva_sectors$Change, digits = 1)

## Plot results for 10 biggest winners
plot_sectors_win <- ggplot(gva_sectors[c(991:1000,826:835,661:670,496:505,331:340,166:175,1:10),], aes(x = Industry.Name, y = Change, fill = as.factor(Year))) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.9)) + ## ?position_dodge() - position of bars
  scale_x_discrete(limits = gva_sectors$Industry.Name[1:10], ## avoid automatically (alphabetically) sorting the x-axis
                   labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(expand = c(0.2, 0.2)) +
  xlab(NULL) +
  ylab("Milion Kè") +
  ggtitle("Hrubá pøidaná hodnota - energetický sektor a dodavatelské øetìzce",
          "10 odvìtví s nejvìtším nárùstem oproti roku 2015 (seøazeno dle hodnot v roce 2050)") +
  scale_fill_manual(values = c(rep_len(c("#A6DF83","#8BD873","#6FD164","#56C95B","#48C15E","#3AB863","#2DAF6A"), length(unique(gva_sectors$Industry.Name))-1)), ## colours
                    name = "Rok",
                    labels = c("2020","2025","2030","2035","2040","2045","2050")) +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 16),
        legend.position = "bottom",
        legend.direction = "horizontal",
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 16)) +
  guides(fill = guide_legend(ncol = 7))

## Export plot
png(paste0(respath,"Plots/gva_sectors_win.png"), width = 800, height = 500)
print(plot_sectors_win)
dev.off()

## Plot results for 10 biggest losers
plot_sectors_los <- ggplot(gva_sectors[c(1155:1146,990:981,825:816,660:651,495:486,330:321,165:156),], aes(Industry.Name, Change, fill = as.factor(Year))) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.9)) + ## ?position_dodge() - position of bars
  scale_x_discrete(limits = gva_sectors$Industry.Name[1146:1155], ## avoid automatically (alphabetically) sorting the x-axis
                   labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(expand = c(0.2, 0.2)) +
  xlab(NULL) +
  ylab("Milion Kè") +
  ggtitle("Hrubá pøidaná hodnota - energetický sektor a dodavatelské øetìzce",
          "10 odvìtví s nejvìtším poklesem oproti roku 2015 (seøazeno dle hodnot v roce 2050)") +
  scale_fill_manual(values = c(rep_len(c("#B969A1","#AC5080","#9F385B","#912032","#830B08","#770019","#68002B"), length(unique(gva_sectors$Industry.Name))-1)), ## colours
                    name = "Rok",
                    labels = c("2020","2025","2030","2035","2040","2045","2050")) +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 16),
        legend.position = "bottom",
        legend.direction = "horizontal",
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 16)) +
  guides(fill = guide_legend(ncol = 7))

## Export plot
png(paste0(respath,"Plots/gva_sectors_los.png"), width = 800, height = 500)
print(plot_sectors_los)
dev.off()

## Calculate average employment change over years
gva_sectors_av <- gva_sectors

## Cast ("unmelt"), calculate Average column, delete Change columns, sort by Average column and leave only 10 first and last rows
gva_sectors_av <- dcast(gva_sectors_av, Country.Destination + Industry.Name + Industry.Code~Year, value.var = "Change")
gva_sectors_av$Average <- round(rowMeans(gva_sectors_av[,4:10]), digits = 1)
gva_sectors_av[,4:10] <- NULL
gva_sectors_av <- gva_sectors_av[order(gva_sectors_av$Average, decreasing = TRUE),]
gva_sectors_av <- gva_sectors_av[c(1:10,156:165),]

## Make Industry.Name ordered factor
gva_sectors_av$Industry.Name <- factor(gva_sectors_av$Industry.Name, levels = rev(unique(gva_sectors_av$Industry.Name)), ordered = TRUE)

## Export results
fwrite(gva_sectors_av, file = paste0(respath,"gva_sectors_av.csv"), sep=";", dec=",")

## Plot results for average 10 biggest winners and 10 biggest losers
plot_sectors_av <- ggplot(gva_sectors_av, aes(Industry.Name, Average, fill = as.factor(desc(Industry.Name)))) +
  geom_bar(stat = "identity",
           width = 0.9) +
  scale_x_discrete(limits = gva_sectors_av$Industry.Name, ## avoid automatically (alphabetically) sorting the x-axis
                   labels = function(x) str_wrap(x, width = 60),
                   expand = c(0.23, 0.23)) +
  scale_y_continuous(expand = c(0.1, 0.1)) +
  xlab(NULL) +
  ylab("Milion Kè") +
  ggtitle("Hrubá pøidaná hodnota - energetický sektor a dodavatelské øetìzce",
          "10 odvìtví s nejvìtším nárùstem a poklesem (prùmìrná hodnota mezi roky 2015-2050)") +
  scale_fill_manual(values = c(rep(c("#2DAF6A"), times = 10),rep(c("#68002B"), times = 10))) + ## colours
  geom_text(aes(y = Average + 0.1 * sign(Average), label = round(Average, digits = 1))) +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 11),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 16))

## Export plot
png(paste0(respath,"Plots/gva_sectors_av.png"), width = 1000, height = 600)
print(plot_sectors_av)
dev.off()


##########################################################################
# 11.7. Organise GVA results by each energy sector and plot

## Aggregate results by each energy sector (and nothing else)
gva_fuel <- results
gva_fuel <- gva_fuel %>% 
  filter(Country.Origin %in% "CZ") %>%
  filter(Extension %in% "gva") %>%
  group_by(Country.Origin, Country.Destination, Year, Energy) %>%
  summarize(Value = sum(Value))

## Convert values in EUR to CZK
gva_fuel$Value <- gva_fuel$Value * exchange

## Adapt Energy codes to group by energy sources (and not EXIOBASE sectors)
gva_fuel$Energy[grepl("i40.11", gva_fuel$Energy)] <- c("Elektøina")
gva_fuel$Energy[grepl("i90|i01.w.2|i24.d", gva_fuel$Energy)] <- c("Obnovitelné zdroje (biomasa, bioplyn, odpad)")
gva_fuel$Energy[grepl("i10|i23.1", gva_fuel$Energy)] <- c("Tuhá paliva")
gva_fuel$Energy[grepl("i23.2", gva_fuel$Energy)] <- c("Ropa")
gva_fuel$Energy[grepl("i40.3", gva_fuel$Energy)] <- c("Teplo")
gva_fuel$Energy[grepl("i40.2", gva_fuel$Energy)] <- c("Plyn")
gva_fuel <- gva_fuel %>% 
  group_by(Country.Origin, Country.Destination, Year, Energy) %>%
  summarize(Value = sum(Value))

## Reorder Energy to group the sectors visually in the plot
neworder <- c("Tuhá paliva","Ropa","Plyn","Teplo","Obnovitelné zdroje","Elektøina")
gva_fuel <- gva_fuel %>% slice(order(factor(Energy, levels = neworder)))

## Export results
fwrite(gva_fuel, file = paste0(respath,"gva_fuel.csv"), sep=";", dec=",")

## Round numbers
gva_fuel$Value <- round(gva_fuel$Value, digits = 1)

## Make Energy ordered factor
gva_fuel$Energy <- factor(gva_fuel$Energy, levels = rev(unique(gva_fuel$Energy)), ordered = TRUE)

## Plot results
plot_fuel <- ggplot(gva_fuel) +
  geom_bar(aes(x = as.factor(Year), y = Value, fill = Energy),
           stat = "identity",
           position = "stack",
           width = 0.8) + ## ?position_stack() - position of bars
  scale_x_discrete(expand = c(0.1, 0.1)) +
  xlab(NULL) +
  ylab("Poèet pracovních míst (1000)") +
  scale_fill_tableau(palette = "Classic Cyclic",
                     direction = -1,
                     name = "Zdroje") +
  ggtitle("Hrubá pøidaná hodnota - energetický sektor a dodavatelské øetìzce",
          "Èlenìní dle jednotlivých zdrojù energie") +
  theme(strip.text.x = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 16))

## Export plot
png(paste0(respath,"Plots/gva_fuel.png"), width = 800, height = 500)
print(plot_fuel)
dev.off()


##########################################################################
# 11.8. Organise GDP total results and plot

## Aggregate by total values (and nothing else)
gdp_total <- results
gdp_total <- gdp_total %>% 
  filter(Country.Origin %in% "CZ") %>%
  filter(Extension %in% "gdp") %>%
  group_by(Country.Destination, Year) %>%
  summarize(Value = sum(Value))

## Convert values in EUR to CZK
gdp_total$Value <- gdp_total$Value * exchange

## Export results
fwrite(gdp_total, file = paste0(respath,"gdp_total.csv"), sep=";", dec=",")

## Mutate to plot stacked bar chart with value labels and round numbers
gdp_total <- gdp_total %>%
  group_by(Year) %>%
  mutate(lab_ypos = cumsum(Value) - 0.5 * Value)
gdp_total$Value <- round(gdp_total$Value, digits = 1)
gdp_total$lab_ypos <- round(gdp_total$lab_ypos, digits = 1)

## Plot results
plot_total <- ggplot(gdp_total) +
  geom_bar(aes(x = as.factor(Year), y = Value),
           stat = "identity",
           width = 0.8) + ## ?position_stack() - position of bars
  geom_text(aes(x = as.factor(Year), y = lab_ypos, label = Value),
            color = "white",
            size = 5,
            vjust = 0.3) +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  xlab(NULL) +
  ylab("Milion Kè") +
  ggtitle("Hrubý domácí produkt - energetický sektor a dodavatelské øetìzce",
          "Celkem") +
  theme(strip.text.x = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 16))

## Export plot
png(paste0(respath,"Plots/gdp_total.png"), width = 800, height = 500)
print(plot_total)
dev.off()


##########################################################################
# 11.9. Organise GDP results by sectors (winners vs. losers sorted by 2050; average over 2015-2050) and plot

## Aggregate by sectors (and nothing else)
gdp_sectors <- results
gdp_sectors <- gdp_sectors %>% 
  filter(Country.Origin %in% "CZ") %>%
  filter(Extension %in% "gdp") %>%
  group_by(Country.Destination, Year, Industry.Code, Industry.Name) %>%
  summarize(Value = sum(Value))

## Rename sectors in Czech
gdp_sectors$Industry.Name <- c(rep(industries$cz))

## Convert values in EUR to CZK
gdp_sectors$Value <- gdp_sectors$Value * exchange

## Cast ("unmelt") to match values from different years next to each other, calculate Change columns (percentage change compared to 2015)
gdp_sectors <- dcast(gdp_sectors, Country.Destination + Industry.Name + Industry.Code~Year, value.var = "Value")
for (year in years) {
  if (year>2015) {
    gdp_sectors[,paste0("Change.",year)] <- gdp_sectors[,year] - gdp_sectors[,"2015"]
  }
}

## Delete Value columns
for (year in years) {
  gdp_sectors[,year] <- NULL
}

## Sort by Change.2050 values in a descending order and change colnames
gdp_sectors <- gdp_sectors %>% 
  as_tibble() %>% 
  arrange(desc(gdp_sectors$Change.2050))
colnames(gdp_sectors)[4:10] <- c("2020","2025","2030","2035","2040","2045","2050")

## Melt and adjust colnames
gdp_sectors <- melt(gdp_sectors)
colnames(gdp_sectors)[4] <- "Year"
colnames(gdp_sectors)[5] <- "Change"

## Export results
fwrite(gdp_sectors, file = paste0(respath,"gdp_sectors.csv"), sep=";", dec=",")

## Round numbers
gdp_sectors$Change <- round(gdp_sectors$Change, digits = 1)

## Plot results for 10 biggest winners
plot_sectors_win <- ggplot(gdp_sectors[c(991:1000,826:835,661:670,496:505,331:340,166:175,1:10),], aes(x = Industry.Name, y = Change, fill = as.factor(Year))) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.9)) + ## ?position_dodge() - position of bars
  scale_x_discrete(limits = gdp_sectors$Industry.Name[1:10], ## avoid automatically (alphabetically) sorting the x-axis
                   labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(expand = c(0.2, 0.2)) +
  xlab(NULL) +
  ylab("Milion Kè") +
  ggtitle("Hrubý domácí produkt - energetický sektor a dodavatelské øetìzce",
          "10 odvìtví s nejvìtším nárùstem oproti roku 2015 (seøazeno dle hodnot v roce 2050)") +
  scale_fill_manual(values = c(rep_len(c("#A6DF83","#8BD873","#6FD164","#56C95B","#48C15E","#3AB863","#2DAF6A"), length(unique(gdp_sectors$Industry.Name))-1)), ## colours
                    name = "Rok",
                    labels = c("2020","2025","2030","2035","2040","2045","2050")) +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 16),
        legend.position = "bottom",
        legend.direction = "horizontal",
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 16)) +
  guides(fill = guide_legend(ncol = 7))

## Export plot
png(paste0(respath,"Plots/gdp_sectors_win.png"), width = 800, height = 500)
print(plot_sectors_win)
dev.off()

## Plot results for 10 biggest losers
plot_sectors_los <- ggplot(gdp_sectors[c(1155:1146,990:981,825:816,660:651,495:486,330:321,165:156),], aes(Industry.Name, Change, fill = as.factor(Year))) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.9)) + ## ?position_dodge() - position of bars
  scale_x_discrete(limits = gdp_sectors$Industry.Name[1146:1155], ## avoid automatically (alphabetically) sorting the x-axis
                   labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(expand = c(0.2, 0.2)) +
  xlab(NULL) +
  ylab("Milion Kè") +
  ggtitle("Hrubý domácí produkt - energetický sektor a dodavatelské øetìzce",
          "10 odvìtví s nejvìtším poklesem oproti roku 2015 (seøazeno dle hodnot v roce 2050)") +
  scale_fill_manual(values = c(rep_len(c("#B969A1","#AC5080","#9F385B","#912032","#830B08","#770019","#68002B"), length(unique(gdp_sectors$Industry.Name))-1)), ## colours
                    name = "Rok",
                    labels = c("2020","2025","2030","2035","2040","2045","2050")) +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 16),
        legend.position = "bottom",
        legend.direction = "horizontal",
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 16)) +
  guides(fill = guide_legend(ncol = 7))

## Export plot
png(paste0(respath,"Plots/gdp_sectors_los.png"), width = 800, height = 500)
print(plot_sectors_los)
dev.off()

## Calculate average employment change over years
gdp_sectors_av <- gdp_sectors

## Cast ("unmelt"), calculate Average column, delete Change columns, sort by Average column and leave only 10 first and last rows
gdp_sectors_av <- dcast(gdp_sectors_av, Country.Destination + Industry.Name + Industry.Code~Year, value.var = "Change")
gdp_sectors_av$Average <- round(rowMeans(gdp_sectors_av[,4:10]), digits = 1)
gdp_sectors_av[,4:10] <- NULL
gdp_sectors_av <- gdp_sectors_av[order(gdp_sectors_av$Average, decreasing = TRUE),]
gdp_sectors_av <- gdp_sectors_av[c(1:10,156:165),]

## Make Industry.Name ordered factor
gdp_sectors_av$Industry.Name <- factor(gdp_sectors_av$Industry.Name, levels = rev(unique(gdp_sectors_av$Industry.Name)), ordered = TRUE)

## Export results
fwrite(gdp_sectors_av, file = paste0(respath,"gdp_sectors_av.csv"), sep=";", dec=",")

## Plot results for average 10 biggest winners and 10 biggest losers
plot_sectors_av <- ggplot(gdp_sectors_av, aes(Industry.Name, Average, fill = as.factor(desc(Industry.Name)))) +
  geom_bar(stat = "identity",
           width = 0.9) +
  scale_x_discrete(limits = gdp_sectors_av$Industry.Name, ## avoid automatically (alphabetically) sorting the x-axis
                   labels = function(x) str_wrap(x, width = 60),
                   expand = c(0.23, 0.23)) +
  scale_y_continuous(expand = c(0.1, 0.1)) +
  xlab(NULL) +
  ylab("Milion Kè") +
  ggtitle("Hrubý domácí produkt - energetický sektor a dodavatelské øetìzce",
          "10 odvìtví s nejvìtším nárùstem a poklesem (prùmìrná hodnota mezi roky 2015-2050)") +
  scale_fill_manual(values = c(rep(c("#2DAF6A"), times = 10),rep(c("#68002B"), times = 10))) + ## colours
  geom_text(aes(y = Average + 0.1 * sign(Average), label = round(Average, digits = 1))) +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 11),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 16))

## Export plot
png(paste0(respath,"Plots/gdp_sectors_av.png"), width = 1000, height = 600)
print(plot_sectors_av)
dev.off()


##########################################################################
# 11.10. Organise GDP results by each energy sector and plot

## Aggregate results by each energy sector (and nothing else)
gdp_fuel <- results
gdp_fuel <- gdp_fuel %>% 
  filter(Country.Origin %in% "CZ") %>%
  filter(Extension %in% "gdp") %>%
  group_by(Country.Origin, Country.Destination, Year, Energy) %>%
  summarize(Value = sum(Value))

## Convert values in EUR to CZK
gdp_fuel$Value <- gdp_fuel$Value * exchange

## Adapt Energy codes to group by energy sources (and not EXIOBASE sectors)
gdp_fuel$Energy[grepl("i40.11", gdp_fuel$Energy)] <- c("Elektøina")
gdp_fuel$Energy[grepl("i90|i01.w.2|i24.d", gdp_fuel$Energy)] <- c("Obnovitelné zdroje (biomasa, bioplyn, odpad)")
gdp_fuel$Energy[grepl("i10|i23.1", gdp_fuel$Energy)] <- c("Tuhá paliva")
gdp_fuel$Energy[grepl("i23.2", gdp_fuel$Energy)] <- c("Ropa")
gdp_fuel$Energy[grepl("i40.3", gdp_fuel$Energy)] <- c("Teplo")
gdp_fuel$Energy[grepl("i40.2", gdp_fuel$Energy)] <- c("Plyn")
gdp_fuel <- gdp_fuel %>% 
  group_by(Country.Origin, Country.Destination, Year, Energy) %>%
  summarize(Value = sum(Value))

## Reorder Energy to group the sectors visually in the plot
neworder <- c("Tuhá paliva","Ropa","Plyn","Teplo","Obnovitelné zdroje","Elektøina")
gdp_fuel <- gdp_fuel %>% slice(order(factor(Energy, levels = neworder)))

## Export results
fwrite(gdp_fuel, file = paste0(respath,"gdp_fuel.csv"), sep=";", dec=",")

## Round numbers
gdp_fuel$Value <- round(gdp_fuel$Value, digits = 1)

## Make Energy ordered factor
gdp_fuel$Energy <- factor(gdp_fuel$Energy, levels = rev(unique(gdp_fuel$Energy)), ordered = TRUE)

## Plot results
plot_fuel <- ggplot(gdp_fuel) +
  geom_bar(aes(x = as.factor(Year), y = Value, fill = Energy),
           stat = "identity",
           position = "stack",
           width = 0.8) + ## ?position_stack() - position of bars
  scale_x_discrete(expand = c(0.1, 0.1)) +
  xlab(NULL) +
  ylab("Poèet pracovních míst (1000)") +
  scale_fill_tableau(palette = "Classic Cyclic",
                     direction = -1,
                     name = "Zdroje") +
  ggtitle("Hrubý domácí produkt - energetický sektor a dodavatelské øetìzce",
          "Èlenìní dle jednotlivých zdrojù energie") +
  theme(strip.text.x = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 16))

## Export plot
png(paste0(respath,"Plots/gdp_fuel.png"), width = 800, height = 500)
print(plot_fuel)
dev.off()


##########################################################################
# 11.11. Organise GHG total results and plot

## Aggregate by total values (and nothing else)
ghg_total <- results
ghg_total <- ghg_total %>%
  filter(Country.Origin %in% "CZ") %>%
  filter(Extension %in% "ghg") %>%
  group_by(Country.Destination, Year) %>%
  summarize(Value = sum(Value))

## Turn Kg into Tons
ghg_total$Value <- ghg_total$Value/1000

## Export results
fwrite(ghg_total, file = paste0(respath,"ghg_total.csv"), sep=";", dec=",")

## Mutate to plot stacked bar chart with value labels and round numbers
ghg_total <- ghg_total %>%
  mutate(lab_ypos = 0.5 * Value)
ghg_total$Value <- round(ghg_total$Value, digits = 0)
ghg_total$lab_ypos <- round(ghg_total$lab_ypos, digits = 0)

## Plot results
plot_total <- ggplot(ghg_total) +
  geom_bar(aes(x = as.factor(Year), y = Value),
           stat = "identity",
           width = 0.8) +
  geom_text(aes(x = as.factor(Year), y = lab_ypos, label = Value),
            colour = "white",
            size = 4.5,
            vjust = 0.3) +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  scale_y_continuous(expand = c(0.1, 0.1)) +
  xlab(NULL) +
  ylab("Kg CO2e (1000)") +
  ggtitle("Emise skleníkových plynù - energetický sektor a dodavatelské øetìzce",
          "Celkem") +
  theme(strip.text.x = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 16))

## Export plot
png(paste0(respath,"Plots/ghg_total.png"), width = 800, height = 500)
print(plot_total)
dev.off()


##########################################################################
# 11.12. Organise GHG results by each energy sector and plot

## Aggregate results by each energy sector (and nothing else)
ghg_fuel <- results
ghg_fuel <- ghg_fuel %>%
  filter(Country.Origin %in% "CZ") %>%
  filter(Extension %in% "ghg") %>%
  group_by(Country.Origin, Country.Destination, Year, Energy) %>%
  summarize(Value = sum(Value))

## Turn Kg into Tons
ghg_fuel$Value <- ghg_fuel$Value/1000

## Adapt Energy codes to group by energy sources (and not EXIOBASE sectors)
ghg_fuel$Energy[grepl("i40.11", ghg_fuel$Energy)] <- c("Elektøina")
ghg_fuel$Energy[grepl("i90|i01.w.2|i24.d", ghg_fuel$Energy)] <- c("Obnovitelné zdroje (biomasa, bioplyn, odpad)")
ghg_fuel$Energy[grepl("i10|i23.1", ghg_fuel$Energy)] <- c("Tuhá paliva")
ghg_fuel$Energy[grepl("i23.2", ghg_fuel$Energy)] <- c("Ropa")
ghg_fuel$Energy[grepl("i40.3", ghg_fuel$Energy)] <- c("Teplo")
ghg_fuel$Energy[grepl("i40.2", ghg_fuel$Energy)] <- c("Plyn")
ghg_fuel <- ghg_fuel %>%
  group_by(Country.Origin, Country.Destination, Year, Energy) %>%
  summarize(Value = sum(Value))

## Reorder Energy to group the sectors visually in the plot
neworder <- c("Tuhá paliva","Ropa","Plyn","Teplo","Obnovitelné zdroje","Elektøina")
ghg_fuel <- ghg_fuel %>% slice(order(factor(Energy, levels = neworder)))

## Export results
fwrite(ghg_fuel, file = paste0(respath,"ghg_fuel.csv"), sep=";", dec=",")

## Round numbers
ghg_fuel$Value <- round(ghg_fuel$Value, digits = 1)

## Make Energy ordered factor
ghg_fuel$Energy <- factor(ghg_fuel$Energy, levels = rev(unique(ghg_fuel$Energy)), ordered = TRUE)

## Plot results
plot_fuel <- ggplot(ghg_fuel) +
  geom_bar(aes(x = as.factor(Year), y = Value, fill = Energy),
           stat = "identity",
           position = "stack",
           width = 0.8) + ## ?position_stack() - position of bars
  scale_x_discrete(expand = c(0.1, 0.1)) +
  xlab(NULL) +
  ylab("Kg CO2e (1000)") +
  scale_fill_tableau(palette = "Classic Cyclic",
                     direction = -1,
                     name = "Zdroje") +
  ggtitle("Emise skleníkových plynù - energetický sektor a dodavatelské øetìzce",
          "Èlenìní dle jednotlivých zdrojù energie") +
  theme(strip.text.x = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 16))

## Export plot
png(paste0(respath,"Plots/ghg_fuel.png"), width = 800, height = 500)
print(plot_fuel)
dev.off()

} # end of condition p2


