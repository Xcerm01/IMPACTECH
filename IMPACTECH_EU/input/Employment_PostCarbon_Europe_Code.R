##########################################################################
# Global employment and skill levele requirements for 'Post-Carbon Europe'
# Scope: MRIO, EU27+UK by countries
# Scenarios: 100% Renewables, Reference
##########################################################################

## Use with compatible EXIOBASE 3.6 industry-by-industry 2015 data: https://zenodo.org/record/3583071#.YhqMUujMKUk

install.packages("readxl")
install.packages("openxlsx")
install.packages("tidyverse")
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
update.packages(ask = FALSE)

library(readxl)
library(openxlsx)
library(tidyverse)
library(dplyr)
library(data.table)
library(reshape2)
library(ggplot2)
library(stringr)
library(tibble)
library(gridExtra)
library(zoo)
library(ggsci)
library(ggthemes)



##########################################################################
# 0. Set path and load data
##########################################################################

##########################################################################
# 0.1. Set path (change accordingly)

## Source input-output data
IOpath <- "D:/EXIOBASE 3.6/parsed/"

## Other data
datapath <- "C:/Users/Cerny/Documents/Work/Papers/2022 Employment and renewable energy/Article/Data and codes/"

## Raw results for employment requirements
emppath <- "D:/EXIOBASE 3.6/emp_results/"

## Results
respath <- "C:/Users/Cerny/Documents/Work/Papers/2022 Employment and renewable energy/Article/Results"


##########################################################################
# 0.2. Load original EXIOBASE 3.6 data

load(paste0(IOpath,"/Q.codes.RData"))
load(paste0(IOpath,"/Y.codes.RData"))
load(paste0(IOpath,"/ixi/IO.codes.RData"))
load(paste0(IOpath,"/ixi/2015_Z.RData"))
load(paste0(IOpath,"/ixi/2015_Y.RData"))
load(paste0(IOpath,"/ixi/2015_E.RData"))
load(paste0(IOpath,"/ixi/2015_x.RData"))


##########################################################################
# 0.3. Define basic sets

scenarios <- c("euref", "stanford") ## modelled scenarios (euref = reference, stanford = 100% renewables)
years = c("2015", "2020", "2025", "2030", "2035", "2040", "2045", "2050")  ## years considered in the analysis
electricity_det <- c("a", "b", "c", "d", "e.1", "e.2", "f", "g", "h.1", "h.2", "i", "j", "k") ## modelled electricity sectors (detailed)
countries <- c("AT","BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", "FR", "GR", "HR", "HU", "IE", "IT", "LT",
               "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK", "GB")
countries_all <- c("AT","BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", "FR", "GR", "HR", "HU", "IE", "IT", "LT",
                   "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK", "GB", "US", "JP", "CN", "CA", "KR", "BR",
                   "IN", "MX", "RU", "AU", "CH", "TR", "TW", "NO", "ID", "ZA", "WA", "WL", "WE", "WF", "WM")
countries_names <- c("Austria", "Belgium", "Bulgaria", "Cyprus", "Czechia", "Germany", "Denmark", "Estonia", "Spain",
                     "Finland", "France", "Greece", "Croatia", "Hungary", "Ireland", "Italy", "Lithuania", "Luxembourg", "Latvia",
                     "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Sweden", "Slovenia", "Slovakia", "United Kingdom")
countries_names_all <- c("Austria", "Belgium", "Bulgaria", "Cyprus", "Czechia", "Germany", "Denmark", "Estonia", "Spain",
                         "Finland", "France", "Greece", "Croatia", "Hungary", "Ireland", "Italy", "Lithuania", "Luxembourg", "Latvia",
                         "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Sweden", "Slovenia", "Slovakia", "United Kingdom",
                         "Australia", "Brazil", "Canada", "China", "Chile", "Indonesia", "India", "Japan", "South Korea", "Mexico",
                         "Norway", "Russia", "Turkey", "Taiwan", "United States", "Asia and Pacific", "Rest of Europe", "Africa", "America", 
                         "Middle East", "South Africa")


##########################################################################
# 0.4. Load functions

## agg
agg <- function(x)
{
  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
  return(x)
}

## is.nan
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

## !in
`%!in%` = Negate(`%in%`)


##########################################################################
# 0.5. Run and load the EXIOBASE disaggregations with IO.codes (->IOdet.codes), x (->xdet) Y (->Ydet), Z (->Zdet) and E (->Edet)

## Create IOdet.codes by adding new rows for the detailed sectors
IOdet.codes <- IO.codes

wind <- sort(which(IOdet.codes$Industry.Code == "i40.11.e"), decreasing = TRUE)
for(i in wind) {
  IOdet.codes <- IOdet.codes %>% 
    tibble::add_row(
      Index = NA_integer_,
      Industry.Name = "Production of electricity by wind offshore",
      Industry.Code = "i40.11.e.2",
      Country.Code = NA_character_,
      Region.Code = NA_character_,
      Industry.Group = NA_character_,
      .after = i
    )
}

pv <- sort(which(IOdet.codes$Industry.Code == "i40.11.h"), decreasing = TRUE)
for(i in pv) {
  IOdet.codes <- IOdet.codes %>% 
    tibble::add_row(
      Index = NA_integer_,
      Industry.Name = "Production of electricity by solar photovoltaic residential",
      Industry.Code = "i40.11.h.2",
      Country.Code = NA_character_,
      Region.Code = NA_character_,
      Industry.Group = NA_character_,
      .after = i
    )
}

## Adjust the NAs in the new rows, names of the 'old' rows (sectors) and the rest of the table
IOdet.codes <- IOdet.codes %>% do(na.locf(.)) ## Replace NA values with values for the previous row
IOdet.codes$Index <- 1:nrow(IOdet.codes) ## Adjust numbers in 'Index' column
IOdet.codes$Industry.Code[IOdet.codes$Industry.Code == "i40.11.e"] <- "i40.11.e.1"
IOdet.codes$Industry.Code[IOdet.codes$Industry.Code == "i40.11.h"] <- "i40.11.h.1"
IOdet.codes$Industry.Name[IOdet.codes$Industry.Name == "Production of electricity by wind"] <- "Production of electricity by wind onshore"
IOdet.codes$Industry.Name[IOdet.codes$Industry.Name == "Production of electricity by solar photovoltaic"] <- "Production of electricity by solar photovoltaic utility"

## Calculate shares of the detailed sectors to disaggregate the rows or columns of X, Y, Z and E
gen <- readxl::read_excel(paste0(datapath, "Employment_PostCarbon_Europe_Data.xlsx"), sheet = "generation", col_types = c("text", "text", "text", "skip", "skip", "skip", "skip", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
gen <- as.data.frame(gen)
rownames(gen) <- paste0(gen$Country.Code,"_",gen$Industry.Code)
gen <- gen[,"gen2015",drop = FALSE]

## Calculate shares on total production for the disaggregated sectors (based on physical units) for each country
windon_share <- list()
windoff_share <- list()
pvutil_share <- list()
pvres_share <- list()

for (country in countries_all) {
  windon <- gen$gen2015[which(endsWith(rownames(gen), paste0(country,"_i40.11.e.1")))]
  windoff <- gen$gen2015[which(endsWith(rownames(gen), paste0(country,"_i40.11.e.2")))]
  pvutil <- gen$gen2015[which(endsWith(rownames(gen), paste0(country,"_i40.11.h.1")))]
  pvres <- gen$gen2015[which(endsWith(rownames(gen), paste0(country,"_i40.11.h.2")))]
  windon_share[[country]] <- windon / (windon + windoff)
  windoff_share[[country]] <- windoff / (windon + windoff)
  pvutil_share[[country]] <- pvutil / (pvutil + pvres)
  pvres_share[[country]] <- pvres / (pvutil + pvres)
}

## Unlist, rename rows and bind together
windon_share <- as.data.frame(unlist(windon_share))
rownames(windon_share) <- paste0(rownames(windon_share), "_i40.11.e.1")
colnames(windon_share) <- c("Share")

windoff_share <- as.data.frame(unlist(windoff_share))
rownames(windoff_share) <- paste0(rownames(windoff_share), "_i40.11.e.2")
colnames(windoff_share) <- c("Share")

pvutil_share <- as.data.frame(unlist(pvutil_share))
rownames(pvutil_share) <- paste0(rownames(pvutil_share), "_i40.11.h.1")
colnames(pvutil_share) <- c("Share")

pvres_share <- as.data.frame(unlist(pvres_share))
rownames(pvres_share) <- paste0(rownames(pvres_share), "_i40.11.h.2")
colnames(pvres_share) <- c("Share")

shares <- rbind(windon_share, windoff_share, pvutil_share, pvres_share)
shares <- as.data.frame(shares[order(rownames(shares)), 1, drop = FALSE])
shares <- as.matrix(shares)
shares[is.nan(shares)] <- 0

## Create xdet by adding new rows (disaggregate x to xdet)
xdet <- x
xdet <- as.data.frame(xdet)
rownames(xdet) <- paste0(IO.codes$Country.Code,"_",IO.codes$Industry.Code)

detailed <- sort(cbind(which(endsWith(rownames(xdet),"i40.11.e")),
                       which(endsWith(rownames(xdet),"i40.11.h"))), decreasing = TRUE)
xdet <- as_tibble(xdet)
for(i in detailed) {
  xdet <- xdet %>% 
    tibble::add_row(
      .after = i
    )
}
rownames(xdet) <- paste0(IOdet.codes$Country.Code,"_",IOdet.codes$Industry.Code)

## Distribute the right values over the new rows (from external source by dividing the original sectors' production)
xdet <- as.matrix(xdet)

## Implement "shares" into xdet for the new rows (sectors)
## The "shares" are only for (EU27+UK) "countries", therefore the following code works only for "countries"
## This is solved by inserting zeros to the newly created sectors in the rest of the countries (outside EU27+UK)
for (country in countries) {
  xdet[which(endsWith(rownames(xdet), paste0(country,"_i40.11.e.2")))] <- shares[which(endsWith(rownames(shares), paste0(country,"_i40.11.e.2")))] * xdet[which(endsWith(rownames(xdet), paste0(country,"_i40.11.e.1")))]
  xdet[which(endsWith(rownames(xdet), paste0(country,"_i40.11.e.1")))] <- shares[which(endsWith(rownames(shares), paste0(country,"_i40.11.e.1")))] * xdet[which(endsWith(rownames(xdet), paste0(country,"_i40.11.e.1")))]
  xdet[which(endsWith(rownames(xdet), paste0(country,"_i40.11.h.2")))] <- shares[which(endsWith(rownames(shares), paste0(country,"_i40.11.h.2")))] * xdet[which(endsWith(rownames(xdet), paste0(country,"_i40.11.h.1")))]
  xdet[which(endsWith(rownames(xdet), paste0(country,"_i40.11.h.1")))] <- shares[which(endsWith(rownames(shares), paste0(country,"_i40.11.h.1")))] * xdet[which(endsWith(rownames(xdet), paste0(country,"_i40.11.h.1")))]
}

xdet[is.na(xdet)] <- 0

## Create Ydet by adding new rows (disaggregate Y to Ydet)
Ydet <- Y
Ydet <- as.data.frame(Ydet)
rownames(Ydet) <- paste0(IO.codes$Country.Code,"_",IO.codes$Industry.Code)

detailed <- sort(cbind(which(endsWith(rownames(Ydet),"i40.11.e")),
                       which(endsWith(rownames(Ydet),"i40.11.h"))), decreasing = TRUE)
for(i in detailed) {
  Ydet <- Ydet %>% 
    tibble::add_row(
      .after = i
    )
}
rownames(Ydet) <- paste0(IOdet.codes$Country.Code,"_",IOdet.codes$Industry.Code)

## Distribute the right values over the new rows (from external source by dividing the original sectors' production)
Ydet <- as.matrix(Ydet)

## Implement "shares" into Ydet for the new rows (sectors)
## The "shares" are only for (EU27+UK) "countries", therefore the following code works only for "countries"
## This is solved by inserting zeros to the newly created sectors in the rest of the countries (outside EU27+UK)
for (country in countries) {
  Ydet[which(endsWith(rownames(Ydet), paste0(country,"_i40.11.e.2"))),] <- shares[which(endsWith(rownames(shares), paste0(country,"_i40.11.e.2"))),] * Ydet[which(endsWith(rownames(Ydet), paste0(country,"_i40.11.e.1"))),]
  Ydet[which(endsWith(rownames(Ydet), paste0(country,"_i40.11.e.1"))),] <- shares[which(endsWith(rownames(shares), paste0(country,"_i40.11.e.1"))),] * Ydet[which(endsWith(rownames(Ydet), paste0(country,"_i40.11.e.1"))),]
  Ydet[which(endsWith(rownames(Ydet), paste0(country,"_i40.11.h.2"))),] <- shares[which(endsWith(rownames(shares), paste0(country,"_i40.11.h.2"))),] * Ydet[which(endsWith(rownames(Ydet), paste0(country,"_i40.11.h.1"))),]
  Ydet[which(endsWith(rownames(Ydet), paste0(country,"_i40.11.h.1"))),] <- shares[which(endsWith(rownames(shares), paste0(country,"_i40.11.h.1"))),] * Ydet[which(endsWith(rownames(Ydet), paste0(country,"_i40.11.h.1"))),]
}

Ydet[is.na(Ydet)] <- 0

## Create Zdet by adding new rows and columns (Disaggregate Z to Zdet)
## By rows
Zdet <- Z
Zdet <- as.data.frame(Zdet)
rownames(Zdet) <- paste0(IO.codes$Country.Code,"_",IO.codes$Industry.Code)

detailed <- sort(cbind(which(endsWith(rownames(Zdet),"i40.11.e")),
                       which(endsWith(rownames(Zdet),"i40.11.h"))), decreasing = TRUE)
for(i in detailed) {
  Zdet <- Zdet %>% 
    tibble::add_row(
      .after = i
    )
}

## By columns
Zdet <- t(Zdet)
Zdet <- as.data.frame(Zdet)
rownames(Zdet) <- paste0(IO.codes$Country.Code,"_",IO.codes$Industry.Code)

detailed <- sort(cbind(which(endsWith(rownames(Zdet),"i40.11.e")),
                       which(endsWith(rownames(Zdet),"i40.11.h"))), decreasing = TRUE)
for(i in detailed) {
  Zdet <- Zdet %>% 
    tibble::add_row(
      .after = i
    )
}
Zdet <- t(Zdet)
rownames(Zdet) <- paste0(IOdet.codes$Country.Code,"_",IOdet.codes$Industry.Code)

## Distribute the right values over the new rows and columns (from external source by dividing the original sectors' production)
## Implement "shares" into Zdet for the new rows (sectors)
## The "shares" are only for (EU27+UK) "countries", therefore the following code works only for "countries"
## This is solved by inserting zeros to the newly created sectors in the rest of the countries (outside EU27+UK)
for (country in countries) {
  Zdet[which(endsWith(rownames(Zdet), paste0(country,"_i40.11.e.2"))),] <- shares[which(endsWith(rownames(shares), paste0(country,"_i40.11.e.2"))),]*Zdet[which(endsWith(rownames(Zdet), paste0(country,"_i40.11.e.1"))),]
  Zdet[which(endsWith(rownames(Zdet), paste0(country,"_i40.11.e.1"))),] <- shares[which(endsWith(rownames(shares), paste0(country,"_i40.11.e.1"))),]*Zdet[which(endsWith(rownames(Zdet), paste0(country,"_i40.11.e.1"))),]
  Zdet[which(endsWith(rownames(Zdet), paste0(country,"_i40.11.h.2"))),] <- shares[which(endsWith(rownames(shares), paste0(country,"_i40.11.h.2"))),]*Zdet[which(endsWith(rownames(Zdet), paste0(country,"_i40.11.h.1"))),]
  Zdet[which(endsWith(rownames(Zdet), paste0(country,"_i40.11.h.1"))),] <- shares[which(endsWith(rownames(shares), paste0(country,"_i40.11.h.1"))),]*Zdet[which(endsWith(rownames(Zdet), paste0(country,"_i40.11.h.1"))),]
}
Zdet[is.na(Zdet)] <- 0

## Implement "shares" into Zdet for the new columns (sectors)
## Note: Zdet disaggregation by columns should be ideally based on different OPEX input cost shares
## Here we assume the same technology of production for both disaggregated sectors as for the aggregated
Zdet <- t(Zdet)
rownames(Zdet) <- paste0(IOdet.codes$Country.Code,"_",IOdet.codes$Industry.Code)

for (country in countries) {
  Zdet[which(endsWith(rownames(Zdet), paste0(country,"_i40.11.e.2"))),] <- shares[which(endsWith(rownames(shares), paste0(country,"_i40.11.e.2"))),]*Zdet[which(endsWith(rownames(Zdet), paste0(country,"_i40.11.e.1"))),]
  Zdet[which(endsWith(rownames(Zdet), paste0(country,"_i40.11.e.1"))),] <- shares[which(endsWith(rownames(shares), paste0(country,"_i40.11.e.1"))),]*Zdet[which(endsWith(rownames(Zdet), paste0(country,"_i40.11.e.1"))),]
  Zdet[which(endsWith(rownames(Zdet), paste0(country,"_i40.11.h.2"))),] <- shares[which(endsWith(rownames(shares), paste0(country,"_i40.11.h.2"))),]*Zdet[which(endsWith(rownames(Zdet), paste0(country,"_i40.11.h.1"))),]
  Zdet[which(endsWith(rownames(Zdet), paste0(country,"_i40.11.h.1"))),] <- shares[which(endsWith(rownames(shares), paste0(country,"_i40.11.h.1"))),]*Zdet[which(endsWith(rownames(Zdet), paste0(country,"_i40.11.h.1"))),]
}
Zdet[is.na(Zdet)] <- 0

Zdet <- t(Zdet)

## Create Edet by adding new columns (disaggregate E to Edet)
Edet <- E
Edet <- t(Edet)
Edet <- as.data.frame(Edet)
rownames(Edet) <- paste0(IO.codes$Country.Code,"_",IO.codes$Industry.Code)

detailed <- sort(cbind(which(endsWith(rownames(Edet),"i40.11.e")),
                       which(endsWith(rownames(Edet),"i40.11.h"))), decreasing = TRUE)
for(i in detailed) {
  Edet <- Edet %>% 
    tibble::add_row(
      .after = i
    )
}
Edet <- t(Edet)
colnames(Edet) <- paste0(IOdet.codes$Country.Code,"_",IOdet.codes$Industry.Code)

## Distribute the right values over the new columns (from external source by dividing the original sectors' production)
## The "shares" are only for (EU27+UK) "countries", therefore the following code works only for "countries"
## This is solved by inserting zeros to the newly created sectors in the rest of the countries (outside EU27+UK)
## Note: Edet disaggregation should be ideally based on different OPEX input cost shares
## Here we assume the same technology of production for the disaggregated sectors as for the aggregated
Edet <- t(Edet)

for (country in countries) {
  Edet[which(endsWith(rownames(Edet), paste0(country,"_i40.11.e.2"))),] <- shares[which(endsWith(rownames(shares), paste0(country,"_i40.11.e.2"))),]*Edet[which(endsWith(rownames(Zdet), paste0(country,"_i40.11.e.1"))),]
  Edet[which(endsWith(rownames(Edet), paste0(country,"_i40.11.e.1"))),] <- shares[which(endsWith(rownames(shares), paste0(country,"_i40.11.e.1"))),]*Edet[which(endsWith(rownames(Zdet), paste0(country,"_i40.11.e.1"))),]
  Edet[which(endsWith(rownames(Edet), paste0(country,"_i40.11.h.2"))),] <- shares[which(endsWith(rownames(shares), paste0(country,"_i40.11.h.2"))),]*Edet[which(endsWith(rownames(Zdet), paste0(country,"_i40.11.h.1"))),]
  Edet[which(endsWith(rownames(Edet), paste0(country,"_i40.11.h.1"))),] <- shares[which(endsWith(rownames(shares), paste0(country,"_i40.11.h.1"))),]*Edet[which(endsWith(rownames(Zdet), paste0(country,"_i40.11.h.1"))),]
}
Edet[is.na(Edet)] <- 0

Edet <- t(Edet)



##########################################################################
# 1. Prepare GFCF data and implement into Y (Ydet)
##########################################################################

##########################################################################
# 1.1. Prepare adjusted Y (with GFCF for the modelled sectors separate) for 2015

YGFCF2015 <- Ydet
colnames(YGFCF2015) <- paste0(Y.codes$`Region Name`, "_", Y.codes$`Final Demand Category`)
rownames(YGFCF2015) <- paste0(IOdet.codes$Country.Code, "_", IOdet.codes$Industry.Code)
YGFCF2015 <- as.data.frame(YGFCF2015)

for(country in countries){
  a <- paste0(country, "_i40.11.a")
  b <- paste0(country, "_i40.11.b")
  c <- paste0(country, "_i40.11.c")
  d <- paste0(country, "_i40.11.d")
  e1 <- paste0(country, "_i40.11.e.1")
  e2 <- paste0(country, "_i40.11.e.2")
  f <- paste0(country, "_i40.11.f")
  g <- paste0(country, "_i40.11.g")
  h1 <- paste0(country, "_i40.11.h.1")
  h2 <- paste0(country, "_i40.11.h.2")
  i <- paste0(country, "_i40.11.i")
  j <- paste0(country, "_i40.11.j")
  k <- paste0(country, "_i40.11.k")
  l <- paste0(country, "_i40.11.l")
  YGFCF2015 <- add_column(YGFCF2015,
                          !!a := 0, !!b := 0, !!c := 0, !!d := 0, !!e1 := 0, !!e2 := 0, !!f := 0, !!g := 0, !!h1 := 0, !!h2 := 0, !!i := 0, !!j := 0, !!k := 0, !!l := 0,
                          .before = paste0(country, "_Gross fixed capital formation"))
}


##########################################################################
# 1.2. Read GFCF for the disaggregated electricity sectors and the concordance matrix

## Read GFCF parts of the scenarios
GFCF <- list()

for (scenario in scenarios) {
  GFCF[[scenario]] <- readxl::read_excel(paste0(datapath,"Employment_PostCarbon_Europe_Data.xlsx"), sheet = paste0("gfcf_", scenario, "_det"))
  GFCF[[scenario]] <- as.data.frame(GFCF[[scenario]])
  rownames(GFCF[[scenario]]) <- paste0(GFCF[[scenario]]$Country.Code, "_", GFCF[[scenario]]$Industry.Code)
  GFCF[[scenario]] <- select(GFCF[[scenario]], contains("GFCF"))
}

## Read concordance matrix
concordance <- readxl::read_excel(paste0(datapath,"Employment_PostCarbon_Europe_Data.xlsx"), sheet = "gfcf_concordance_det")
concordance <- as.data.frame(concordance)
rownames(concordance) <- concordance$Industry.Code
concordance <- select(concordance, contains("i40.11"))


##########################################################################
# 1.3. Add summary GFCF rows for each sector (rows) for each country (columns) and calculate shares of each row's input to the summary GFCF

GFCFshare <- select(YGFCF2015, contains("Gross fixed capital formation"))
rownames(GFCFshare) <- paste0(IOdet.codes$Country.Code, "_", IOdet.codes$Industry.Code)

GFCFsum <- matrix(0, ncol = 49, nrow = 165)
rownames(GFCFsum) <- unique(IOdet.codes$Industry.Code)
colnames(GFCFsum) <- paste0(unique(IOdet.codes$Country.Code), "_Gross fixed capital formation")
sectors <- c(rownames(GFCFsum))
for (sector in sectors) {
  GFCFsum[endsWith(rownames(GFCFsum), sector),] <- colSums(GFCFshare[endsWith(rownames(GFCFshare), sector),])
  GFCFshare[which(endsWith(rownames(GFCFshare), sector)),] <- t(t(as.matrix(GFCFshare[which(endsWith(rownames(GFCFshare), sector)),])) / as.vector(GFCFsum[which(endsWith(rownames(GFCFsum), sector)),]))
}

## Replace NaNs after dividing by zero
GFCFshare[is.nan(GFCFshare)] <- 0


##########################################################################
# 1.4. Paste GFCF from each sector of each country going into the modelled sectors into YGFCF2015 and YGFCF for other modelled years

YGFCF <- list()

for (scenario in scenarios) {
  for (year in years) {
    YGFCF[[scenario]][[year]] <- YGFCF2015
    for (country in countries) {
      for (e in electricity_det) {
        YGFCF[[scenario]][[year]][,paste0(country, "_i40.11.", e)] <- rep(concordance[,paste0("i40.11.", e)] * GFCF[[scenario]][paste0(country, "_i40.11.", e),paste0("GFCF", year)], 49) * GFCFshare[,which(startsWith(colnames(GFCFshare), country))]
      }
    }
  }
}

## Decrease the original GFCF vector in YGFCF accordingly
## For 2015 we need to decrease the GFCF vector, but for the other years, we need the decreased one from 2015
for (scenario in scenarios) {
  for (year in years) {
    for (country in countries) {
      if(year==2015) {
        YGFCF[[scenario]][[year]][,paste0(country, "_Gross fixed capital formation")] <- YGFCF[[scenario]][[year]][,paste0(country, "_Gross fixed capital formation")] - rowSums(YGFCF[[scenario]][[year]][,which(startsWith(colnames(YGFCF[[scenario]][[year]]), paste0(country,"_i40.11")))])
      } else if(year!=2015) {
        YGFCF[[scenario]][[year]][,paste0(country, "_Gross fixed capital formation")] <- YGFCF[[scenario]][["2015"]][,paste0(country, "_Gross fixed capital formation")]
      }
    }
  }
  YGFCF2015 <- YGFCF[[scenario]][["2015"]]
}


##########################################################################
# 1.5. Extract only the detailed electricity GFCF columns from YGFCF and create YGFCF.codes

## Extract detailed electricity GFCF columns from YGFCF
for (scenario in scenarios) {
  for (year in years) {
    YGFCF[[scenario]][[year]] <- YGFCF[[scenario]][[year]][,!grepl("i40.11.l", colnames(YGFCF[[scenario]][[year]]))&
                                                             grepl("i40.11", colnames(YGFCF[[scenario]][[year]]))]
  }
}

## Create YGFCF.codes for the footprint (employment requirements) calculation
YGFCF.codes <- data.frame(`Region Name` = rep(countries_all, each = 13),
                          `Final Demand Category` = rep(c("Gross fixed capital formation - electricity by coal",
                                                          "Gross fixed capital formation - electricity by gas",
                                                          "Gross fixed capital formation - electricity by nuclear",
                                                          "Gross fixed capital formation - electricity by hydro",
                                                          "Gross fixed capital formation - electricity by wind onshore",
                                                          "Gross fixed capital formation - electricity by wind offshore",
                                                          "Gross fixed capital formation - electricity by petroleum and other oil derivatives",
                                                          "Gross fixed capital formation - electricity by biomass and waste",
                                                          "Gross fixed capital formation - electricity by solar photovoltaic utility",
                                                          "Gross fixed capital formation - electricity by solar photovoltaic residential",
                                                          "Gross fixed capital formation - electricity by solar thermal",
                                                          "Gross fixed capital formation - electricity by tide, wave, ocean",
                                                          "Gross fixed capital formation - electricity by Geothermal")))
YGFCF.codes$Key <- paste0(YGFCF.codes$Region.Name,"-",YGFCF.codes$Final.Demand.Category)


##########################################################################
# 1.6. Prepare xdet for 2015, prepare Adet (A remains the same over time)

## Prepare xdet for 2015
xdet2015 <- xdet
xdet2015 <- as.vector(xdet2015)

## Calculate Adet
Adet <- t(t(Zdet) / xdet2015)
Adet[!is.finite(Adet)] <- 0


##########################################################################
# 1.7. Identify modelled sectors in the EU27+UK countries and export for external calculations

## Identify modelled electricity sectors in the EU27+UK countries
EUelectricity_det <- IOdet.codes$Index[grepl("Production of electricity by", IOdet.codes$Industry.Name)&IOdet.codes$Region.Code=="EU"]

## Export xdet2015_el - the electricity rows - to calculate changes in total output and compare employment intensity (externally)
## Note: check current directory
xdet2015_el <- xdet2015[EUelectricity_det]
xdet2015_el <- as.data.table(xdet2015_el)
data <- loadWorkbook("Employment_PostCarbon_Europe_Data.xlsx")
writeData(data, sheet = "xdet2015_el", xdet2015_el)
saveWorkbook(data, "Employment_PostCarbon_Europe_Data.xlsx" , overwrite = T)

## Identify other modelled energy fuels sectors in the EU27+UK countries
EUfuel <- IOdet.codes$Index[grepl("i01.w.2|i10|i23.1|i23.2|i24.d|i40.2|i40.3|i90.2|i90.3", IOdet.codes$Industry.Code)&IOdet.codes$Region.Code=="EU"]


##########################################################################
# 1.8. Run and load the EXIOBASE Adet and extension (Extdet) adjustments

## Load Adet (intermediate inputs in the electricity sectors) and create replacements (A_el)
## There are zeros when there is no production from the sector
## When electricity would start to be produced from the empty source in the future, it would distort the results

## Identify the electricity sectors
All_electricity_det <- IOdet.codes$Index[grepl("Production of electricity by", IOdet.codes$Industry.Name)]

## Read Adet and create A_el
A_el <- Adet
colnames(A_el) <- paste0(IOdet.codes$Country.Code,"_",IOdet.codes$Industry.Code)
A_el <- A_el[,All_electricity_det]
rownames(A_el) <- paste0(IOdet.codes$Country.Code,"_",IOdet.codes$Industry.Code)

## Replace empty columns in A_el with values from other countries and shift the domestic supply rows to the right positions
## Domestic supply is shifted because we assume that if there would be electricity production from a given (so far empty) source,
## it would follow the same pattern as in the country from which the replacement comes - i.e. probably majority of inputs would come
## from domestic inputs

## AT
domestic <- A_el[startsWith(rownames(A_el), "CZ"), "CZ_i40.11.c"]
imported <- A_el[startsWith(rownames(A_el), "AT"), "CZ_i40.11.c"]
A_el[,"AT_i40.11.c"] <- A_el[,"CZ_i40.11.c"]
A_el[startsWith(rownames(A_el), "AT"), "AT_i40.11.c"] <- domestic
A_el[startsWith(rownames(A_el), "CZ"), "AT_i40.11.c"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "DE"), "DE_i40.11.e.2"]
imported <- A_el[startsWith(rownames(A_el), "AT"), "DE_i40.11.e.2"]
A_el[,"AT_i40.11.e.2"] <- A_el[,"DE_i40.11.e.2"]
A_el[startsWith(rownames(A_el), "AT"), "AT_i40.11.e.2"] <- domestic
A_el[startsWith(rownames(A_el), "DE"), "AT_i40.11.e.2"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "ES"), "ES_i40.11.i"]
imported <- A_el[startsWith(rownames(A_el), "AT"), "ES_i40.11.i"]
A_el[,"AT_i40.11.i"] <- A_el[,"ES_i40.11.i"]
A_el[startsWith(rownames(A_el), "AT"), "AT_i40.11.i"] <- domestic
A_el[startsWith(rownames(A_el), "ES"), "AT_i40.11.i"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "FR"), "FR_i40.11.j"]
imported <- A_el[startsWith(rownames(A_el), "AT"), "FR_i40.11.j"]
A_el[,"AT_i40.11.j"] <- A_el[,"FR_i40.11.j"]
A_el[startsWith(rownames(A_el), "AT"), "AT_i40.11.j"] <- domestic
A_el[startsWith(rownames(A_el), "FR"), "AT_i40.11.j"] <- imported

## BE
domestic <- A_el[startsWith(rownames(A_el), "ES"), "ES_i40.11.i"]
imported <- A_el[startsWith(rownames(A_el), "BE"), "ES_i40.11.i"]
A_el[,"BE_i40.11.i"] <- A_el[,"ES_i40.11.i"]
A_el[startsWith(rownames(A_el), "BE"), "BE_i40.11.i"] <- domestic
A_el[startsWith(rownames(A_el), "ES"), "BE_i40.11.i"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "FR"), "FR_i40.11.j"]
imported <- A_el[startsWith(rownames(A_el), "BE"), "FR_i40.11.j"]
A_el[,"BE_i40.11.j"] <- A_el[,"FR_i40.11.j"]
A_el[startsWith(rownames(A_el), "BE"), "BE_i40.11.j"] <- domestic
A_el[startsWith(rownames(A_el), "FR"), "BE_i40.11.j"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "AT"), "AT_i40.11.k"]
imported <- A_el[startsWith(rownames(A_el), "BE"), "AT_i40.11.k"]
A_el[,"BE_i40.11.k"] <- A_el[,"AT_i40.11.k"]
A_el[startsWith(rownames(A_el), "BE"), "BE_i40.11.k"] <- domestic
A_el[startsWith(rownames(A_el), "AT"), "BE_i40.11.k"] <- imported

## BG
domestic <- A_el[startsWith(rownames(A_el), "PT"), "PT_i40.11.e.2"]
imported <- A_el[startsWith(rownames(A_el), "BG"), "PT_i40.11.e.2"]
A_el[,"BG_i40.11.e.2"] <- A_el[,"PT_i40.11.e.2"]
A_el[startsWith(rownames(A_el), "BG"), "BG_i40.11.e.2"] <- domestic
A_el[startsWith(rownames(A_el), "PT"), "BG_i40.11.e.2"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "ES"), "ES_i40.11.i"]
imported <- A_el[startsWith(rownames(A_el), "BG"), "ES_i40.11.i"]
A_el[,"BG_i40.11.i"] <- A_el[,"ES_i40.11.i"]
A_el[startsWith(rownames(A_el), "BG"), "BG_i40.11.i"] <- domestic
A_el[startsWith(rownames(A_el), "ES"), "BG_i40.11.i"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "FR"), "FR_i40.11.j"]
imported <- A_el[startsWith(rownames(A_el), "BG"), "FR_i40.11.j"]
A_el[,"BG_i40.11.j"] <- A_el[,"FR_i40.11.j"]
A_el[startsWith(rownames(A_el), "BG"), "BG_i40.11.j"] <- domestic
A_el[startsWith(rownames(A_el), "FR"), "BG_i40.11.j"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "IT"), "IT_i40.11.k"]
imported <- A_el[startsWith(rownames(A_el), "BG"), "IT_i40.11.k"]
A_el[,"BG_i40.11.k"] <- A_el[,"IT_i40.11.k"]
A_el[startsWith(rownames(A_el), "BG"), "BG_i40.11.k"] <- domestic
A_el[startsWith(rownames(A_el), "IT"), "BG_i40.11.k"] <- imported

## CY
domestic <- A_el[startsWith(rownames(A_el), "GR"), "GR_i40.11.a"]
imported <- A_el[startsWith(rownames(A_el), "CY"), "GR_i40.11.a"]
A_el[,"CY_i40.11.a"] <- A_el[,"GR_i40.11.a"]
A_el[startsWith(rownames(A_el), "CY"), "CY_i40.11.a"] <- domestic
A_el[startsWith(rownames(A_el), "GR"), "CY_i40.11.a"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "GR"), "GR_i40.11.b"]
imported <- A_el[startsWith(rownames(A_el), "CY"), "GR_i40.11.b"]
A_el[,"CY_i40.11.b"] <- A_el[,"GR_i40.11.b"]
A_el[startsWith(rownames(A_el), "CY"), "CY_i40.11.b"] <- domestic
A_el[startsWith(rownames(A_el), "GR"), "CY_i40.11.b"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "BG"), "BG_i40.11.c"]
imported <- A_el[startsWith(rownames(A_el), "CY"), "BG_i40.11.c"]
A_el[,"CY_i40.11.c"] <- A_el[,"BG_i40.11.c"]
A_el[startsWith(rownames(A_el), "CY"), "CY_i40.11.c"] <- domestic
A_el[startsWith(rownames(A_el), "BG"), "CY_i40.11.c"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "GR"), "GR_i40.11.d"]
imported <- A_el[startsWith(rownames(A_el), "CY"), "GR_i40.11.d"]
A_el[,"CY_i40.11.d"] <- A_el[,"GR_i40.11.d"]
A_el[startsWith(rownames(A_el), "CY"), "CY_i40.11.d"] <- domestic
A_el[startsWith(rownames(A_el), "GR"), "CY_i40.11.d"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "PT"), "PT_i40.11.e.2"]
imported <- A_el[startsWith(rownames(A_el), "CY"), "PT_i40.11.e.2"]
A_el[,"CY_i40.11.e.2"] <- A_el[,"PT_i40.11.e.2"]
A_el[startsWith(rownames(A_el), "CY"), "CY_i40.11.e.2"] <- domestic
A_el[startsWith(rownames(A_el), "PT"), "CY_i40.11.e.2"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "ES"), "ES_i40.11.i"]
imported <- A_el[startsWith(rownames(A_el), "CY"), "ES_i40.11.i"]
A_el[,"CY_i40.11.i"] <- A_el[,"ES_i40.11.i"]
A_el[startsWith(rownames(A_el), "CY"), "CY_i40.11.i"] <- domestic
A_el[startsWith(rownames(A_el), "ES"), "CY_i40.11.i"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "FR"), "FR_i40.11.j"]
imported <- A_el[startsWith(rownames(A_el), "CY"), "FR_i40.11.j"]
A_el[,"CY_i40.11.j"] <- A_el[,"FR_i40.11.j"]
A_el[startsWith(rownames(A_el), "CY"), "CY_i40.11.j"] <- domestic
A_el[startsWith(rownames(A_el), "FR"), "CY_i40.11.j"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "IT"), "IT_i40.11.k"]
imported <- A_el[startsWith(rownames(A_el), "CY"), "IT_i40.11.k"]
A_el[,"CY_i40.11.k"] <- A_el[,"IT_i40.11.k"]
A_el[startsWith(rownames(A_el), "CY"), "CY_i40.11.k"] <- domestic
A_el[startsWith(rownames(A_el), "IT"), "CY_i40.11.k"] <- imported

## CZ
domestic <- A_el[startsWith(rownames(A_el), "DE"), "DE_i40.11.e.2"]
imported <- A_el[startsWith(rownames(A_el), "CZ"), "DE_i40.11.e.2"]
A_el[,"CZ_i40.11.e.2"] <- A_el[,"DE_i40.11.e.2"]
A_el[startsWith(rownames(A_el), "CZ"), "CZ_i40.11.e.2"] <- domestic
A_el[startsWith(rownames(A_el), "DE"), "CZ_i40.11.e.2"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "ES"), "ES_i40.11.i"]
imported <- A_el[startsWith(rownames(A_el), "CZ"), "ES_i40.11.i"]
A_el[,"CZ_i40.11.i"] <- A_el[,"ES_i40.11.i"]
A_el[startsWith(rownames(A_el), "CZ"), "CZ_i40.11.i"] <- domestic
A_el[startsWith(rownames(A_el), "ES"), "CZ_i40.11.i"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "FR"), "FR_i40.11.j"]
imported <- A_el[startsWith(rownames(A_el), "CZ"), "FR_i40.11.j"]
A_el[,"CZ_i40.11.j"] <- A_el[,"FR_i40.11.j"]
A_el[startsWith(rownames(A_el), "CZ"), "CZ_i40.11.j"] <- domestic
A_el[startsWith(rownames(A_el), "FR"), "CZ_i40.11.j"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "AT"), "AT_i40.11.k"]
imported <- A_el[startsWith(rownames(A_el), "CZ"), "AT_i40.11.k"]
A_el[,"CZ_i40.11.k"] <- A_el[,"AT_i40.11.k"]
A_el[startsWith(rownames(A_el), "CZ"), "CZ_i40.11.k"] <- domestic
A_el[startsWith(rownames(A_el), "AT"), "CZ_i40.11.k"] <- imported

## DE
domestic <- A_el[startsWith(rownames(A_el), "ES"), "ES_i40.11.i"]
imported <- A_el[startsWith(rownames(A_el), "DE"), "ES_i40.11.i"]
A_el[,"DE_i40.11.i"] <- A_el[,"ES_i40.11.i"]
A_el[startsWith(rownames(A_el), "DE"), "DE_i40.11.i"] <- domestic
A_el[startsWith(rownames(A_el), "ES"), "DE_i40.11.i"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "FR"), "FR_i40.11.j"]
imported <- A_el[startsWith(rownames(A_el), "DE"), "FR_i40.11.j"]
A_el[,"DE_i40.11.j"] <- A_el[,"FR_i40.11.j"]
A_el[startsWith(rownames(A_el), "DE"), "DE_i40.11.j"] <- domestic
A_el[startsWith(rownames(A_el), "FR"), "DE_i40.11.j"] <- imported

## DK
domestic <- A_el[startsWith(rownames(A_el), "SE"), "SE_i40.11.c"]
imported <- A_el[startsWith(rownames(A_el), "DK"), "SE_i40.11.c"]
A_el[,"DK_i40.11.c"] <- A_el[,"SE_i40.11.c"]
A_el[startsWith(rownames(A_el), "DK"), "DK_i40.11.c"] <- domestic
A_el[startsWith(rownames(A_el), "SE"), "DK_i40.11.c"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "ES"), "ES_i40.11.i"]
imported <- A_el[startsWith(rownames(A_el), "DK"), "ES_i40.11.i"]
A_el[,"DK_i40.11.i"] <- A_el[,"ES_i40.11.i"]
A_el[startsWith(rownames(A_el), "DK"), "DK_i40.11.i"] <- domestic
A_el[startsWith(rownames(A_el), "ES"), "DK_i40.11.i"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "FR"), "FR_i40.11.j"]
imported <- A_el[startsWith(rownames(A_el), "DK"), "FR_i40.11.j"]
A_el[,"DK_i40.11.j"] <- A_el[,"FR_i40.11.j"]
A_el[startsWith(rownames(A_el), "DK"), "DK_i40.11.j"] <- domestic
A_el[startsWith(rownames(A_el), "FR"), "DK_i40.11.j"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "AT"), "AT_i40.11.k"]
imported <- A_el[startsWith(rownames(A_el), "DK"), "AT_i40.11.k"]
A_el[,"DK_i40.11.k"] <- A_el[,"AT_i40.11.k"]
A_el[startsWith(rownames(A_el), "DK"), "DK_i40.11.k"] <- domestic
A_el[startsWith(rownames(A_el), "AT"), "DK_i40.11.k"] <- imported

## EE
domestic <- A_el[startsWith(rownames(A_el), "SE"), "SE_i40.11.c"]
imported <- A_el[startsWith(rownames(A_el), "EE"), "SE_i40.11.c"]
A_el[,"EE_i40.11.c"] <- A_el[,"SE_i40.11.c"]
A_el[startsWith(rownames(A_el), "EE"), "EE_i40.11.c"] <- domestic
A_el[startsWith(rownames(A_el), "SE"), "EE_i40.11.c"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "DE"), "DE_i40.11.e.2"]
imported <- A_el[startsWith(rownames(A_el), "EE"), "DE_i40.11.e.2"]
A_el[,"EE_i40.11.e.2"] <- A_el[,"DE_i40.11.e.2"]
A_el[startsWith(rownames(A_el), "EE"), "EE_i40.11.e.2"] <- domestic
A_el[startsWith(rownames(A_el), "DE"), "EE_i40.11.e.2"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "SE"), "SE_i40.11.h.1"]
imported <- A_el[startsWith(rownames(A_el), "EE"), "SE_i40.11.h.1"]
A_el[,"EE_i40.11.h.1"] <- A_el[,"SE_i40.11.h.1"]
A_el[startsWith(rownames(A_el), "EE"), "EE_i40.11.h.1"] <- domestic
A_el[startsWith(rownames(A_el), "SE"), "EE_i40.11.h.1"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "FI"), "FI_i40.11.h.2"]
imported <- A_el[startsWith(rownames(A_el), "EE"), "FI_i40.11.h.2"]
A_el[,"EE_i40.11.h.2"] <- A_el[,"FI_i40.11.h.2"]
A_el[startsWith(rownames(A_el), "EE"), "EE_i40.11.h.2"] <- domestic
A_el[startsWith(rownames(A_el), "FI"), "EE_i40.11.h.2"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "ES"), "ES_i40.11.i"]
imported <- A_el[startsWith(rownames(A_el), "EE"), "ES_i40.11.i"]
A_el[,"EE_i40.11.i"] <- A_el[,"ES_i40.11.i"]
A_el[startsWith(rownames(A_el), "EE"), "EE_i40.11.i"] <- domestic
A_el[startsWith(rownames(A_el), "ES"), "EE_i40.11.i"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "FR"), "FR_i40.11.j"]
imported <- A_el[startsWith(rownames(A_el), "EE"), "FR_i40.11.j"]
A_el[,"EE_i40.11.j"] <- A_el[,"FR_i40.11.j"]
A_el[startsWith(rownames(A_el), "EE"), "EE_i40.11.j"] <- domestic
A_el[startsWith(rownames(A_el), "FR"), "EE_i40.11.j"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "AT"), "AT_i40.11.k"]
imported <- A_el[startsWith(rownames(A_el), "EE"), "AT_i40.11.k"]
A_el[,"EE_i40.11.k"] <- A_el[,"AT_i40.11.k"]
A_el[startsWith(rownames(A_el), "EE"), "EE_i40.11.k"] <- domestic
A_el[startsWith(rownames(A_el), "AT"), "EE_i40.11.k"] <- imported

## ES
domestic <- A_el[startsWith(rownames(A_el), "PT"), "PT_i40.11.e.2"]
imported <- A_el[startsWith(rownames(A_el), "ES"), "PT_i40.11.e.2"]
A_el[,"ES_i40.11.e.2"] <- A_el[,"PT_i40.11.e.2"]
A_el[startsWith(rownames(A_el), "ES"), "ES_i40.11.e.2"] <- domestic
A_el[startsWith(rownames(A_el), "PT"), "ES_i40.11.e.2"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "FR"), "FR_i40.11.j"]
imported <- A_el[startsWith(rownames(A_el), "ES"), "FR_i40.11.j"]
A_el[,"ES_i40.11.j"] <- A_el[,"FR_i40.11.j"]
A_el[startsWith(rownames(A_el), "ES"), "ES_i40.11.j"] <- domestic
A_el[startsWith(rownames(A_el), "FR"), "ES_i40.11.j"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "PT"), "PT_i40.11.k"]
imported <- A_el[startsWith(rownames(A_el), "ES"), "PT_i40.11.k"]
A_el[,"ES_i40.11.k"] <- A_el[,"PT_i40.11.k"]
A_el[startsWith(rownames(A_el), "ES"), "ES_i40.11.k"] <- domestic
A_el[startsWith(rownames(A_el), "PT"), "ES_i40.11.k"] <- imported

## FI
domestic <- A_el[startsWith(rownames(A_el), "SE"), "SE_i40.11.h.1"]
imported <- A_el[startsWith(rownames(A_el), "FI"), "SE_i40.11.h.1"]
A_el[,"FI_i40.11.h.1"] <- A_el[,"SE_i40.11.h.1"]
A_el[startsWith(rownames(A_el), "FI"), "FI_i40.11.h.1"] <- domestic
A_el[startsWith(rownames(A_el), "SE"), "FI_i40.11.h.1"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "ES"), "ES_i40.11.i"]
imported <- A_el[startsWith(rownames(A_el), "FI"), "ES_i40.11.i"]
A_el[,"FI_i40.11.i"] <- A_el[,"ES_i40.11.i"]
A_el[startsWith(rownames(A_el), "FI"), "FI_i40.11.i"] <- domestic
A_el[startsWith(rownames(A_el), "ES"), "FI_i40.11.i"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "FR"), "FR_i40.11.j"]
imported <- A_el[startsWith(rownames(A_el), "FI"), "FR_i40.11.j"]
A_el[,"FI_i40.11.j"] <- A_el[,"FR_i40.11.j"]
A_el[startsWith(rownames(A_el), "FI"), "FI_i40.11.j"] <- domestic
A_el[startsWith(rownames(A_el), "FR"), "FI_i40.11.j"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "AT"), "AT_i40.11.k"]
imported <- A_el[startsWith(rownames(A_el), "FI"), "AT_i40.11.k"]
A_el[,"FI_i40.11.k"] <- A_el[,"AT_i40.11.k"]
A_el[startsWith(rownames(A_el), "FI"), "FI_i40.11.k"] <- domestic
A_el[startsWith(rownames(A_el), "AT"), "FI_i40.11.k"] <- imported

## FR
domestic <- A_el[startsWith(rownames(A_el), "PT"), "PT_i40.11.e.2"]
imported <- A_el[startsWith(rownames(A_el), "FR"), "PT_i40.11.e.2"]
A_el[,"FR_i40.11.e.2"] <- A_el[,"PT_i40.11.e.2"]
A_el[startsWith(rownames(A_el), "FR"), "FR_i40.11.e.2"] <- domestic
A_el[startsWith(rownames(A_el), "PT"), "FR_i40.11.e.2"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "ES"), "ES_i40.11.i"]
imported <- A_el[startsWith(rownames(A_el), "FR"), "ES_i40.11.i"]
A_el[,"FR_i40.11.i"] <- A_el[,"ES_i40.11.i"]
A_el[startsWith(rownames(A_el), "FR"), "FR_i40.11.i"] <- domestic
A_el[startsWith(rownames(A_el), "ES"), "FR_i40.11.i"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "IT"), "IT_i40.11.k"]
imported <- A_el[startsWith(rownames(A_el), "FR"), "IT_i40.11.k"]
A_el[,"FR_i40.11.k"] <- A_el[,"IT_i40.11.k"]
A_el[startsWith(rownames(A_el), "FR"), "FR_i40.11.k"] <- domestic
A_el[startsWith(rownames(A_el), "IT"), "FR_i40.11.k"] <- imported

## GR
domestic <- A_el[startsWith(rownames(A_el), "BG"), "BG_i40.11.c"]
imported <- A_el[startsWith(rownames(A_el), "GR"), "BG_i40.11.c"]
A_el[,"GR_i40.11.c"] <- A_el[,"BG_i40.11.c"]
A_el[startsWith(rownames(A_el), "GR"), "GR_i40.11.c"] <- domestic
A_el[startsWith(rownames(A_el), "BG"), "GR_i40.11.c"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "PT"), "PT_i40.11.e.2"]
imported <- A_el[startsWith(rownames(A_el), "GR"), "PT_i40.11.e.2"]
A_el[,"GR_i40.11.e.2"] <- A_el[,"PT_i40.11.e.2"]
A_el[startsWith(rownames(A_el), "GR"), "GR_i40.11.e.2"] <- domestic
A_el[startsWith(rownames(A_el), "PT"), "GR_i40.11.e.2"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "ES"), "ES_i40.11.i"]
imported <- A_el[startsWith(rownames(A_el), "GR"), "ES_i40.11.i"]
A_el[,"GR_i40.11.i"] <- A_el[,"ES_i40.11.i"]
A_el[startsWith(rownames(A_el), "GR"), "GR_i40.11.i"] <- domestic
A_el[startsWith(rownames(A_el), "ES"), "GR_i40.11.i"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "FR"), "FR_i40.11.j"]
imported <- A_el[startsWith(rownames(A_el), "GR"), "FR_i40.11.j"]
A_el[,"GR_i40.11.j"] <- A_el[,"FR_i40.11.j"]
A_el[startsWith(rownames(A_el), "GR"), "GR_i40.11.j"] <- domestic
A_el[startsWith(rownames(A_el), "FR"), "GR_i40.11.j"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "IT"), "IT_i40.11.k"]
imported <- A_el[startsWith(rownames(A_el), "GR"), "IT_i40.11.k"]
A_el[,"GR_i40.11.k"] <- A_el[,"IT_i40.11.k"]
A_el[startsWith(rownames(A_el), "GR"), "GR_i40.11.k"] <- domestic
A_el[startsWith(rownames(A_el), "IT"), "GR_i40.11.k"] <- imported

## HR
domestic <- A_el[startsWith(rownames(A_el), "BG"), "BG_i40.11.c"]
imported <- A_el[startsWith(rownames(A_el), "HR"), "BG_i40.11.c"]
A_el[,"HR_i40.11.c"] <- A_el[,"BG_i40.11.c"]
A_el[startsWith(rownames(A_el), "HR"), "HR_i40.11.c"] <- domestic
A_el[startsWith(rownames(A_el), "BG"), "HR_i40.11.c"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "PT"), "PT_i40.11.e.2"]
imported <- A_el[startsWith(rownames(A_el), "HR"), "PT_i40.11.e.2"]
A_el[,"HR_i40.11.e.2"] <- A_el[,"PT_i40.11.e.2"]
A_el[startsWith(rownames(A_el), "HR"), "HR_i40.11.e.2"] <- domestic
A_el[startsWith(rownames(A_el), "PT"), "HR_i40.11.e.2"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "IT"), "IT_i40.11.h.1"]
imported <- A_el[startsWith(rownames(A_el), "HR"), "IT_i40.11.h.1"]
A_el[,"HR_i40.11.h.1"] <- A_el[,"IT_i40.11.h.1"]
A_el[startsWith(rownames(A_el), "HR"), "HR_i40.11.h.1"] <- domestic
A_el[startsWith(rownames(A_el), "IT"), "HR_i40.11.h.1"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "IT"), "IT_i40.11.h.2"]
imported <- A_el[startsWith(rownames(A_el), "HR"), "IT_i40.11.h.2"]
A_el[,"HR_i40.11.h.2"] <- A_el[,"IT_i40.11.h.2"]
A_el[startsWith(rownames(A_el), "HR"), "HR_i40.11.h.2"] <- domestic
A_el[startsWith(rownames(A_el), "IT"), "HR_i40.11.h.2"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "ES"), "ES_i40.11.i"]
imported <- A_el[startsWith(rownames(A_el), "HR"), "ES_i40.11.i"]
A_el[,"HR_i40.11.i"] <- A_el[,"ES_i40.11.i"]
A_el[startsWith(rownames(A_el), "HR"), "HR_i40.11.i"] <- domestic
A_el[startsWith(rownames(A_el), "ES"), "HR_i40.11.i"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "FR"), "FR_i40.11.j"]
imported <- A_el[startsWith(rownames(A_el), "HR"), "FR_i40.11.j"]
A_el[,"HR_i40.11.j"] <- A_el[,"FR_i40.11.j"]
A_el[startsWith(rownames(A_el), "HR"), "HR_i40.11.j"] <- domestic
A_el[startsWith(rownames(A_el), "FR"), "HR_i40.11.j"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "IT"), "IT_i40.11.k"]
imported <- A_el[startsWith(rownames(A_el), "HR"), "IT_i40.11.k"]
A_el[,"HR_i40.11.k"] <- A_el[,"IT_i40.11.k"]
A_el[startsWith(rownames(A_el), "HR"), "HR_i40.11.k"] <- domestic
A_el[startsWith(rownames(A_el), "IT"), "HR_i40.11.k"] <- imported

## HU
domestic <- A_el[startsWith(rownames(A_el), "DE"), "DE_i40.11.e.2"]
imported <- A_el[startsWith(rownames(A_el), "HU"), "DE_i40.11.e.2"]
A_el[,"HU_i40.11.e.2"] <- A_el[,"DE_i40.11.e.2"]
A_el[startsWith(rownames(A_el), "HU"), "HU_i40.11.e.2"] <- domestic
A_el[startsWith(rownames(A_el), "DE"), "HU_i40.11.e.2"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "ES"), "ES_i40.11.i"]
imported <- A_el[startsWith(rownames(A_el), "HU"), "ES_i40.11.i"]
A_el[,"HU_i40.11.i"] <- A_el[,"ES_i40.11.i"]
A_el[startsWith(rownames(A_el), "HU"), "HU_i40.11.i"] <- domestic
A_el[startsWith(rownames(A_el), "ES"), "HU_i40.11.i"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "FR"), "FR_i40.11.j"]
imported <- A_el[startsWith(rownames(A_el), "HU"), "FR_i40.11.j"]
A_el[,"HU_i40.11.j"] <- A_el[,"FR_i40.11.j"]
A_el[startsWith(rownames(A_el), "HU"), "HU_i40.11.j"] <- domestic
A_el[startsWith(rownames(A_el), "FR"), "HU_i40.11.j"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "AT"), "AT_i40.11.k"]
imported <- A_el[startsWith(rownames(A_el), "HU"), "AT_i40.11.k"]
A_el[,"HU_i40.11.k"] <- A_el[,"AT_i40.11.k"]
A_el[startsWith(rownames(A_el), "HU"), "HU_i40.11.k"] <- domestic
A_el[startsWith(rownames(A_el), "AT"), "HU_i40.11.k"] <- imported

## IE
domestic <- A_el[startsWith(rownames(A_el), "GB"), "GB_i40.11.c"]
imported <- A_el[startsWith(rownames(A_el), "IE"), "GB_i40.11.c"]
A_el[,"IE_i40.11.c"] <- A_el[,"GB_i40.11.c"]
A_el[startsWith(rownames(A_el), "IE"), "IE_i40.11.c"] <- domestic
A_el[startsWith(rownames(A_el), "GB"), "IE_i40.11.c"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "GB"), "GB_i40.11.e.2"]
imported <- A_el[startsWith(rownames(A_el), "IE"), "GB_i40.11.e.2"]
A_el[,"IE_i40.11.e.2"] <- A_el[,"GB_i40.11.e.2"]
A_el[startsWith(rownames(A_el), "IE"), "IE_i40.11.e.2"] <- domestic
A_el[startsWith(rownames(A_el), "GB"), "IE_i40.11.e.2"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "GB"), "GB_i40.11.h.1"]
imported <- A_el[startsWith(rownames(A_el), "IE"), "GB_i40.11.h.1"]
A_el[,"IE_i40.11.h.1"] <- A_el[,"GB_i40.11.h.1"]
A_el[startsWith(rownames(A_el), "IE"), "IE_i40.11.h.1"] <- domestic
A_el[startsWith(rownames(A_el), "GB"), "IE_i40.11.h.1"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "GB"), "GB_i40.11.h.2"]
imported <- A_el[startsWith(rownames(A_el), "IE"), "GB_i40.11.h.2"]
A_el[,"IE_i40.11.h.2"] <- A_el[,"GB_i40.11.h.2"]
A_el[startsWith(rownames(A_el), "IE"), "IE_i40.11.h.2"] <- domestic
A_el[startsWith(rownames(A_el), "GB"), "IE_i40.11.h.2"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "ES"), "ES_i40.11.i"]
imported <- A_el[startsWith(rownames(A_el), "IE"), "ES_i40.11.i"]
A_el[,"IE_i40.11.i"] <- A_el[,"ES_i40.11.i"]
A_el[startsWith(rownames(A_el), "IE"), "IE_i40.11.i"] <- domestic
A_el[startsWith(rownames(A_el), "ES"), "IE_i40.11.i"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "FR"), "FR_i40.11.j"]
imported <- A_el[startsWith(rownames(A_el), "IE"), "FR_i40.11.j"]
A_el[,"IE_i40.11.j"] <- A_el[,"FR_i40.11.j"]
A_el[startsWith(rownames(A_el), "IE"), "IE_i40.11.j"] <- domestic
A_el[startsWith(rownames(A_el), "FR"), "IE_i40.11.j"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "AT"), "AT_i40.11.k"]
imported <- A_el[startsWith(rownames(A_el), "IE"), "AT_i40.11.k"]
A_el[,"IE_i40.11.k"] <- A_el[,"AT_i40.11.k"]
A_el[startsWith(rownames(A_el), "IE"), "IE_i40.11.k"] <- domestic
A_el[startsWith(rownames(A_el), "AT"), "IE_i40.11.k"] <- imported

## IT
domestic <- A_el[startsWith(rownames(A_el), "FR"), "FR_i40.11.c"]
imported <- A_el[startsWith(rownames(A_el), "IT"), "FR_i40.11.c"]
A_el[,"IT_i40.11.c"] <- A_el[,"FR_i40.11.c"]
A_el[startsWith(rownames(A_el), "IT"), "IT_i40.11.c"] <- domestic
A_el[startsWith(rownames(A_el), "FR"), "IT_i40.11.c"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "PT"), "PT_i40.11.e.2"]
imported <- A_el[startsWith(rownames(A_el), "IT"), "PT_i40.11.e.2"]
A_el[,"IT_i40.11.e.2"] <- A_el[,"PT_i40.11.e.2"]
A_el[startsWith(rownames(A_el), "IT"), "IT_i40.11.e.2"] <- domestic
A_el[startsWith(rownames(A_el), "PT"), "IT_i40.11.e.2"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "ES"), "ES_i40.11.i"]
imported <- A_el[startsWith(rownames(A_el), "IT"), "ES_i40.11.i"]
A_el[,"IT_i40.11.i"] <- A_el[,"ES_i40.11.i"]
A_el[startsWith(rownames(A_el), "IT"), "IT_i40.11.i"] <- domestic
A_el[startsWith(rownames(A_el), "ES"), "IT_i40.11.i"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "FR"), "FR_i40.11.j"]
imported <- A_el[startsWith(rownames(A_el), "IT"), "FR_i40.11.j"]
A_el[,"IT_i40.11.j"] <- A_el[,"FR_i40.11.j"]
A_el[startsWith(rownames(A_el), "IT"), "IT_i40.11.j"] <- domestic
A_el[startsWith(rownames(A_el), "FR"), "IT_i40.11.j"] <- imported

## LT
domestic <- A_el[startsWith(rownames(A_el), "EE"), "EE_i40.11.a"]
imported <- A_el[startsWith(rownames(A_el), "LT"), "EE_i40.11.a"]
A_el[,"LT_i40.11.a"] <- A_el[,"EE_i40.11.a"]
A_el[startsWith(rownames(A_el), "LT"), "LT_i40.11.a"] <- domestic
A_el[startsWith(rownames(A_el), "EE"), "LT_i40.11.a"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "SE"), "SE_i40.11.c"]
imported <- A_el[startsWith(rownames(A_el), "LT"), "SE_i40.11.c"]
A_el[,"LT_i40.11.c"] <- A_el[,"SE_i40.11.c"]
A_el[startsWith(rownames(A_el), "LT"), "LT_i40.11.c"] <- domestic
A_el[startsWith(rownames(A_el), "SE"), "LT_i40.11.c"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "DE"), "DE_i40.11.e.2"]
imported <- A_el[startsWith(rownames(A_el), "LT"), "DE_i40.11.e.2"]
A_el[,"LT_i40.11.e.2"] <- A_el[,"DE_i40.11.e.2"]
A_el[startsWith(rownames(A_el), "LT"), "LT_i40.11.e.2"] <- domestic
A_el[startsWith(rownames(A_el), "DE"), "LT_i40.11.e.2"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "SE"), "SE_i40.11.h.1"]
imported <- A_el[startsWith(rownames(A_el), "LT"), "SE_i40.11.h.1"]
A_el[,"LT_i40.11.h.1"] <- A_el[,"SE_i40.11.h.1"]
A_el[startsWith(rownames(A_el), "LT"), "LT_i40.11.h.1"] <- domestic
A_el[startsWith(rownames(A_el), "SE"), "LT_i40.11.h.1"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "FI"), "FI_i40.11.h.2"]
imported <- A_el[startsWith(rownames(A_el), "LT"), "FI_i40.11.h.2"]
A_el[,"LT_i40.11.h.2"] <- A_el[,"FI_i40.11.h.2"]
A_el[startsWith(rownames(A_el), "LT"), "LT_i40.11.h.2"] <- domestic
A_el[startsWith(rownames(A_el), "FI"), "LT_i40.11.h.2"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "ES"), "ES_i40.11.i"]
imported <- A_el[startsWith(rownames(A_el), "LT"), "ES_i40.11.i"]
A_el[,"LT_i40.11.i"] <- A_el[,"ES_i40.11.i"]
A_el[startsWith(rownames(A_el), "LT"), "LT_i40.11.i"] <- domestic
A_el[startsWith(rownames(A_el), "ES"), "LT_i40.11.i"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "FR"), "FR_i40.11.j"]
imported <- A_el[startsWith(rownames(A_el), "LT"), "FR_i40.11.j"]
A_el[,"LT_i40.11.j"] <- A_el[,"FR_i40.11.j"]
A_el[startsWith(rownames(A_el), "LT"), "LT_i40.11.j"] <- domestic
A_el[startsWith(rownames(A_el), "FR"), "LT_i40.11.j"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "AT"), "AT_i40.11.k"]
imported <- A_el[startsWith(rownames(A_el), "LT"), "AT_i40.11.k"]
A_el[,"LT_i40.11.k"] <- A_el[,"AT_i40.11.k"]
A_el[startsWith(rownames(A_el), "LT"), "LT_i40.11.k"] <- domestic
A_el[startsWith(rownames(A_el), "AT"), "LT_i40.11.k"] <- imported

## LU
domestic <- A_el[startsWith(rownames(A_el), "DE"), "DE_i40.11.a"]
imported <- A_el[startsWith(rownames(A_el), "LU"), "DE_i40.11.a"]
A_el[,"LU_i40.11.a"] <- A_el[,"DE_i40.11.a"]
A_el[startsWith(rownames(A_el), "LU"), "LU_i40.11.a"] <- domestic
A_el[startsWith(rownames(A_el), "DE"), "LU_i40.11.a"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "DE"), "DE_i40.11.c"]
imported <- A_el[startsWith(rownames(A_el), "LU"), "DE_i40.11.c"]
A_el[,"LU_i40.11.c"] <- A_el[,"DE_i40.11.c"]
A_el[startsWith(rownames(A_el), "LU"), "LU_i40.11.c"] <- domestic
A_el[startsWith(rownames(A_el), "DE"), "LU_i40.11.c"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "DE"), "DE_i40.11.e.2"]
imported <- A_el[startsWith(rownames(A_el), "LU"), "DE_i40.11.e.2"]
A_el[,"LU_i40.11.e.2"] <- A_el[,"DE_i40.11.e.2"]
A_el[startsWith(rownames(A_el), "LU"), "LU_i40.11.e.2"] <- domestic
A_el[startsWith(rownames(A_el), "DE"), "LU_i40.11.e.2"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "DE"), "DE_i40.11.h.1"]
imported <- A_el[startsWith(rownames(A_el), "LU"), "DE_i40.11.h.1"]
A_el[,"LU_i40.11.h.1"] <- A_el[,"DE_i40.11.h.1"]
A_el[startsWith(rownames(A_el), "LU"), "LU_i40.11.h.1"] <- domestic
A_el[startsWith(rownames(A_el), "DE"), "LU_i40.11.h.1"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "ES"), "ES_i40.11.i"]
imported <- A_el[startsWith(rownames(A_el), "LU"), "ES_i40.11.i"]
A_el[,"LU_i40.11.i"] <- A_el[,"ES_i40.11.i"]
A_el[startsWith(rownames(A_el), "LU"), "LU_i40.11.i"] <- domestic
A_el[startsWith(rownames(A_el), "ES"), "LU_i40.11.i"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "FR"), "FR_i40.11.j"]
imported <- A_el[startsWith(rownames(A_el), "LU"), "FR_i40.11.j"]
A_el[,"LU_i40.11.j"] <- A_el[,"FR_i40.11.j"]
A_el[startsWith(rownames(A_el), "LU"), "LU_i40.11.j"] <- domestic
A_el[startsWith(rownames(A_el), "FR"), "LU_i40.11.j"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "AT"), "AT_i40.11.k"]
imported <- A_el[startsWith(rownames(A_el), "LU"), "AT_i40.11.k"]
A_el[,"LU_i40.11.k"] <- A_el[,"AT_i40.11.k"]
A_el[startsWith(rownames(A_el), "LU"), "LU_i40.11.k"] <- domestic
A_el[startsWith(rownames(A_el), "AT"), "LU_i40.11.k"] <- imported

## LV
domestic <- A_el[startsWith(rownames(A_el), "SE"), "SE_i40.11.c"]
imported <- A_el[startsWith(rownames(A_el), "LV"), "SE_i40.11.c"]
A_el[,"LV_i40.11.c"] <- A_el[,"SE_i40.11.c"]
A_el[startsWith(rownames(A_el), "LV"), "LV_i40.11.c"] <- domestic
A_el[startsWith(rownames(A_el), "SE"), "LV_i40.11.c"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "DE"), "DE_i40.11.e.2"]
imported <- A_el[startsWith(rownames(A_el), "LV"), "DE_i40.11.e.2"]
A_el[,"LV_i40.11.e.2"] <- A_el[,"DE_i40.11.e.2"]
A_el[startsWith(rownames(A_el), "LV"), "LV_i40.11.e.2"] <- domestic
A_el[startsWith(rownames(A_el), "DE"), "LV_i40.11.e.2"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "EE"), "EE_i40.11.f"]
imported <- A_el[startsWith(rownames(A_el), "LV"), "EE_i40.11.f"]
A_el[,"LV_i40.11.f"] <- A_el[,"EE_i40.11.f"]
A_el[startsWith(rownames(A_el), "LV"), "LV_i40.11.f"] <- domestic
A_el[startsWith(rownames(A_el), "EE"), "LV_i40.11.f"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "SE"), "SE_i40.11.h.1"]
imported <- A_el[startsWith(rownames(A_el), "LV"), "SE_i40.11.h.1"]
A_el[,"LV_i40.11.h.1"] <- A_el[,"SE_i40.11.h.1"]
A_el[startsWith(rownames(A_el), "LV"), "LV_i40.11.h.1"] <- domestic
A_el[startsWith(rownames(A_el), "SE"), "LV_i40.11.h.1"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "FI"), "FI_i40.11.h.2"]
imported <- A_el[startsWith(rownames(A_el), "LV"), "FI_i40.11.h.2"]
A_el[,"LV_i40.11.h.2"] <- A_el[,"FI_i40.11.h.2"]
A_el[startsWith(rownames(A_el), "LV"), "LV_i40.11.h.2"] <- domestic
A_el[startsWith(rownames(A_el), "FI"), "LV_i40.11.h.2"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "ES"), "ES_i40.11.i"]
imported <- A_el[startsWith(rownames(A_el), "LV"), "ES_i40.11.i"]
A_el[,"LV_i40.11.i"] <- A_el[,"ES_i40.11.i"]
A_el[startsWith(rownames(A_el), "LV"), "LV_i40.11.i"] <- domestic
A_el[startsWith(rownames(A_el), "ES"), "LV_i40.11.i"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "FR"), "FR_i40.11.j"]
imported <- A_el[startsWith(rownames(A_el), "LV"), "FR_i40.11.j"]
A_el[,"LV_i40.11.j"] <- A_el[,"FR_i40.11.j"]
A_el[startsWith(rownames(A_el), "LV"), "LV_i40.11.j"] <- domestic
A_el[startsWith(rownames(A_el), "FR"), "LV_i40.11.j"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "AT"), "AT_i40.11.k"]
imported <- A_el[startsWith(rownames(A_el), "LV"), "AT_i40.11.k"]
A_el[,"LV_i40.11.k"] <- A_el[,"AT_i40.11.k"]
A_el[startsWith(rownames(A_el), "LV"), "LV_i40.11.k"] <- domestic
A_el[startsWith(rownames(A_el), "AT"), "LV_i40.11.k"] <- imported

## MT
domestic <- A_el[startsWith(rownames(A_el), "IT"), "IT_i40.11.a"]
imported <- A_el[startsWith(rownames(A_el), "MT"), "IT_i40.11.a"]
A_el[,"MT_i40.11.a"] <- A_el[,"IT_i40.11.a"]
A_el[startsWith(rownames(A_el), "MT"), "MT_i40.11.a"] <- domestic
A_el[startsWith(rownames(A_el), "IT"), "MT_i40.11.a"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "IT"), "IT_i40.11.b"]
imported <- A_el[startsWith(rownames(A_el), "MT"), "IT_i40.11.b"]
A_el[,"MT_i40.11.b"] <- A_el[,"IT_i40.11.b"]
A_el[startsWith(rownames(A_el), "MT"), "MT_i40.11.b"] <- domestic
A_el[startsWith(rownames(A_el), "IT"), "MT_i40.11.b"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "FR"), "FR_i40.11.c"]
imported <- A_el[startsWith(rownames(A_el), "MT"), "FR_i40.11.c"]
A_el[,"MT_i40.11.c"] <- A_el[,"FR_i40.11.c"]
A_el[startsWith(rownames(A_el), "MT"), "MT_i40.11.c"] <- domestic
A_el[startsWith(rownames(A_el), "FR"), "MT_i40.11.c"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "IT"), "IT_i40.11.d"]
imported <- A_el[startsWith(rownames(A_el), "MT"), "IT_i40.11.d"]
A_el[,"MT_i40.11.d"] <- A_el[,"IT_i40.11.d"]
A_el[startsWith(rownames(A_el), "MT"), "MT_i40.11.d"] <- domestic
A_el[startsWith(rownames(A_el), "IT"), "MT_i40.11.d"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "IT"), "IT_i40.11.e.1"]
imported <- A_el[startsWith(rownames(A_el), "MT"), "IT_i40.11.e.1"]
A_el[,"MT_i40.11.e.1"] <- A_el[,"IT_i40.11.e.1"]
A_el[startsWith(rownames(A_el), "MT"), "MT_i40.11.e.1"] <- domestic
A_el[startsWith(rownames(A_el), "IT"), "MT_i40.11.e.1"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "PT"), "PT_i40.11.e.2"]
imported <- A_el[startsWith(rownames(A_el), "MT"), "PT_i40.11.e.2"]
A_el[,"MT_i40.11.e.2"] <- A_el[,"PT_i40.11.e.2"]
A_el[startsWith(rownames(A_el), "MT"), "MT_i40.11.e.2"] <- domestic
A_el[startsWith(rownames(A_el), "PT"), "MT_i40.11.e.2"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "IT"), "IT_i40.11.f"]
imported <- A_el[startsWith(rownames(A_el), "MT"), "IT_i40.11.f"]
A_el[,"MT_i40.11.f"] <- A_el[,"IT_i40.11.f"]
A_el[startsWith(rownames(A_el), "MT"), "MT_i40.11.f"] <- domestic
A_el[startsWith(rownames(A_el), "IT"), "MT_i40.11.f"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "IT"), "IT_i40.11.g"]
imported <- A_el[startsWith(rownames(A_el), "MT"), "IT_i40.11.g"]
A_el[,"MT_i40.11.g"] <- A_el[,"IT_i40.11.g"]
A_el[startsWith(rownames(A_el), "MT"), "MT_i40.11.g"] <- domestic
A_el[startsWith(rownames(A_el), "IT"), "MT_i40.11.g"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "IT"), "IT_i40.11.h.1"]
imported <- A_el[startsWith(rownames(A_el), "MT"), "IT_i40.11.h.1"]
A_el[,"MT_i40.11.h.1"] <- A_el[,"IT_i40.11.h.1"]
A_el[startsWith(rownames(A_el), "MT"), "MT_i40.11.h.1"] <- domestic
A_el[startsWith(rownames(A_el), "IT"), "MT_i40.11.h.1"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "IT"), "IT_i40.11.h.2"]
imported <- A_el[startsWith(rownames(A_el), "MT"), "IT_i40.11.h.2"]
A_el[,"MT_i40.11.h.2"] <- A_el[,"IT_i40.11.h.2"]
A_el[startsWith(rownames(A_el), "MT"), "MT_i40.11.h.2"] <- domestic
A_el[startsWith(rownames(A_el), "IT"), "MT_i40.11.h.2"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "ES"), "ES_i40.11.i"]
imported <- A_el[startsWith(rownames(A_el), "MT"), "ES_i40.11.i"]
A_el[,"MT_i40.11.i"] <- A_el[,"ES_i40.11.i"]
A_el[startsWith(rownames(A_el), "MT"), "MT_i40.11.i"] <- domestic
A_el[startsWith(rownames(A_el), "ES"), "MT_i40.11.i"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "FR"), "FR_i40.11.j"]
imported <- A_el[startsWith(rownames(A_el), "MT"), "FR_i40.11.j"]
A_el[,"MT_i40.11.j"] <- A_el[,"FR_i40.11.j"]
A_el[startsWith(rownames(A_el), "MT"), "MT_i40.11.j"] <- domestic
A_el[startsWith(rownames(A_el), "FR"), "MT_i40.11.j"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "IT"), "IT_i40.11.k"]
imported <- A_el[startsWith(rownames(A_el), "MT"), "IT_i40.11.k"]
A_el[,"MT_i40.11.k"] <- A_el[,"IT_i40.11.k"]
A_el[startsWith(rownames(A_el), "MT"), "MT_i40.11.k"] <- domestic
A_el[startsWith(rownames(A_el), "IT"), "MT_i40.11.k"] <- imported

## NL
domestic <- A_el[startsWith(rownames(A_el), "DE"), "DE_i40.11.a"]
imported <- A_el[startsWith(rownames(A_el), "NL"), "DE_i40.11.a"]
A_el[,"NL_i40.11.a"] <- A_el[,"DE_i40.11.a"]
A_el[startsWith(rownames(A_el), "NL"), "NL_i40.11.a"] <- domestic
A_el[startsWith(rownames(A_el), "DE"), "NL_i40.11.a"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "DE"), "DE_i40.11.c"]
imported <- A_el[startsWith(rownames(A_el), "NL"), "DE_i40.11.c"]
A_el[,"NL_i40.11.c"] <- A_el[,"DE_i40.11.c"]
A_el[startsWith(rownames(A_el), "NL"), "NL_i40.11.c"] <- domestic
A_el[startsWith(rownames(A_el), "DE"), "NL_i40.11.c"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "ES"), "ES_i40.11.i"]
imported <- A_el[startsWith(rownames(A_el), "NL"), "ES_i40.11.i"]
A_el[,"NL_i40.11.i"] <- A_el[,"ES_i40.11.i"]
A_el[startsWith(rownames(A_el), "NL"), "NL_i40.11.i"] <- domestic
A_el[startsWith(rownames(A_el), "ES"), "NL_i40.11.i"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "FR"), "FR_i40.11.j"]
imported <- A_el[startsWith(rownames(A_el), "NL"), "FR_i40.11.j"]
A_el[,"NL_i40.11.j"] <- A_el[,"FR_i40.11.j"]
A_el[startsWith(rownames(A_el), "NL"), "NL_i40.11.j"] <- domestic
A_el[startsWith(rownames(A_el), "FR"), "NL_i40.11.j"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "AT"), "AT_i40.11.k"]
imported <- A_el[startsWith(rownames(A_el), "NL"), "AT_i40.11.k"]
A_el[,"NL_i40.11.k"] <- A_el[,"AT_i40.11.k"]
A_el[startsWith(rownames(A_el), "NL"), "NL_i40.11.k"] <- domestic
A_el[startsWith(rownames(A_el), "AT"), "NL_i40.11.k"] <- imported

## PL
domestic <- A_el[startsWith(rownames(A_el), "DE"), "DE_i40.11.c"]
imported <- A_el[startsWith(rownames(A_el), "PL"), "DE_i40.11.c"]
A_el[,"PL_i40.11.c"] <- A_el[,"DE_i40.11.c"]
A_el[startsWith(rownames(A_el), "PL"), "PL_i40.11.c"] <- domestic
A_el[startsWith(rownames(A_el), "DE"), "PL_i40.11.c"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "DE"), "DE_i40.11.e.2"]
imported <- A_el[startsWith(rownames(A_el), "PL"), "DE_i40.11.e.2"]
A_el[,"PL_i40.11.e.2"] <- A_el[,"DE_i40.11.e.2"]
A_el[startsWith(rownames(A_el), "PL"), "PL_i40.11.e.2"] <- domestic
A_el[startsWith(rownames(A_el), "DE"), "PL_i40.11.e.2"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "CZ"), "CZ_i40.11.h.1"]
imported <- A_el[startsWith(rownames(A_el), "PL"), "CZ_i40.11.h.1"]
A_el[,"PL_i40.11.h.1"] <- A_el[,"CZ_i40.11.h.1"]
A_el[startsWith(rownames(A_el), "PL"), "PL_i40.11.h.1"] <- domestic
A_el[startsWith(rownames(A_el), "CZ"), "PL_i40.11.h.1"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "CZ"), "CZ_i40.11.h.2"]
imported <- A_el[startsWith(rownames(A_el), "PL"), "CZ_i40.11.h.2"]
A_el[,"PL_i40.11.h.2"] <- A_el[,"CZ_i40.11.h.2"]
A_el[startsWith(rownames(A_el), "PL"), "PL_i40.11.h.2"] <- domestic
A_el[startsWith(rownames(A_el), "CZ"), "PL_i40.11.h.2"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "ES"), "ES_i40.11.i"]
imported <- A_el[startsWith(rownames(A_el), "PL"), "ES_i40.11.i"]
A_el[,"PL_i40.11.i"] <- A_el[,"ES_i40.11.i"]
A_el[startsWith(rownames(A_el), "PL"), "PL_i40.11.i"] <- domestic
A_el[startsWith(rownames(A_el), "ES"), "PL_i40.11.i"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "FR"), "FR_i40.11.j"]
imported <- A_el[startsWith(rownames(A_el), "PL"), "FR_i40.11.j"]
A_el[,"PL_i40.11.j"] <- A_el[,"FR_i40.11.j"]
A_el[startsWith(rownames(A_el), "PL"), "PL_i40.11.j"] <- domestic
A_el[startsWith(rownames(A_el), "FR"), "PL_i40.11.j"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "AT"), "AT_i40.11.k"]
imported <- A_el[startsWith(rownames(A_el), "PL"), "AT_i40.11.k"]
A_el[,"PL_i40.11.k"] <- A_el[,"AT_i40.11.k"]
A_el[startsWith(rownames(A_el), "PL"), "PL_i40.11.k"] <- domestic
A_el[startsWith(rownames(A_el), "AT"), "PL_i40.11.k"] <- imported

## PT
domestic <- A_el[startsWith(rownames(A_el), "ES"), "ES_i40.11.c"]
imported <- A_el[startsWith(rownames(A_el), "PT"), "ES_i40.11.c"]
A_el[,"PT_i40.11.c"] <- A_el[,"ES_i40.11.c"]
A_el[startsWith(rownames(A_el), "PT"), "PT_i40.11.c"] <- domestic
A_el[startsWith(rownames(A_el), "ES"), "PT_i40.11.c"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "ES"), "ES_i40.11.i"]
imported <- A_el[startsWith(rownames(A_el), "PT"), "ES_i40.11.i"]
A_el[,"PT_i40.11.i"] <- A_el[,"ES_i40.11.i"]
A_el[startsWith(rownames(A_el), "PT"), "PT_i40.11.i"] <- domestic
A_el[startsWith(rownames(A_el), "ES"), "PT_i40.11.i"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "FR"), "FR_i40.11.j"]
imported <- A_el[startsWith(rownames(A_el), "PT"), "FR_i40.11.j"]
A_el[,"PT_i40.11.j"] <- A_el[,"FR_i40.11.j"]
A_el[startsWith(rownames(A_el), "PT"), "PT_i40.11.j"] <- domestic
A_el[startsWith(rownames(A_el), "FR"), "PT_i40.11.j"] <- imported

## RO
domestic <- A_el[startsWith(rownames(A_el), "PT"), "PT_i40.11.e.2"]
imported <- A_el[startsWith(rownames(A_el), "RO"), "PT_i40.11.e.2"]
A_el[,"RO_i40.11.e.2"] <- A_el[,"PT_i40.11.e.2"]
A_el[startsWith(rownames(A_el), "RO"), "RO_i40.11.e.2"] <- domestic
A_el[startsWith(rownames(A_el), "PT"), "RO_i40.11.e.2"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "HU"), "HU_i40.11.h.2"]
imported <- A_el[startsWith(rownames(A_el), "RO"), "HU_i40.11.h.2"]
A_el[,"RO_i40.11.h.2"] <- A_el[,"HU_i40.11.h.2"]
A_el[startsWith(rownames(A_el), "RO"), "RO_i40.11.h.2"] <- domestic
A_el[startsWith(rownames(A_el), "HU"), "RO_i40.11.h.2"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "ES"), "ES_i40.11.i"]
imported <- A_el[startsWith(rownames(A_el), "RO"), "ES_i40.11.i"]
A_el[,"RO_i40.11.i"] <- A_el[,"ES_i40.11.i"]
A_el[startsWith(rownames(A_el), "RO"), "RO_i40.11.i"] <- domestic
A_el[startsWith(rownames(A_el), "ES"), "RO_i40.11.i"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "FR"), "FR_i40.11.j"]
imported <- A_el[startsWith(rownames(A_el), "RO"), "FR_i40.11.j"]
A_el[,"RO_i40.11.j"] <- A_el[,"FR_i40.11.j"]
A_el[startsWith(rownames(A_el), "RO"), "RO_i40.11.j"] <- domestic
A_el[startsWith(rownames(A_el), "FR"), "RO_i40.11.j"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "IT"), "IT_i40.11.k"]
imported <- A_el[startsWith(rownames(A_el), "RO"), "IT_i40.11.k"]
A_el[,"RO_i40.11.k"] <- A_el[,"IT_i40.11.k"]
A_el[startsWith(rownames(A_el), "RO"), "RO_i40.11.k"] <- domestic
A_el[startsWith(rownames(A_el), "IT"), "RO_i40.11.k"] <- imported

## SE
domestic <- A_el[startsWith(rownames(A_el), "ES"), "ES_i40.11.i"]
imported <- A_el[startsWith(rownames(A_el), "SE"), "ES_i40.11.i"]
A_el[,"SE_i40.11.i"] <- A_el[,"ES_i40.11.i"]
A_el[startsWith(rownames(A_el), "SE"), "SE_i40.11.i"] <- domestic
A_el[startsWith(rownames(A_el), "ES"), "SE_i40.11.i"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "FR"), "FR_i40.11.j"]
imported <- A_el[startsWith(rownames(A_el), "SE"), "FR_i40.11.j"]
A_el[,"SE_i40.11.j"] <- A_el[,"FR_i40.11.j"]
A_el[startsWith(rownames(A_el), "SE"), "SE_i40.11.j"] <- domestic
A_el[startsWith(rownames(A_el), "FR"), "SE_i40.11.j"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "AT"), "AT_i40.11.k"]
imported <- A_el[startsWith(rownames(A_el), "SE"), "AT_i40.11.k"]
A_el[,"SE_i40.11.k"] <- A_el[,"AT_i40.11.k"]
A_el[startsWith(rownames(A_el), "SE"), "SE_i40.11.k"] <- domestic
A_el[startsWith(rownames(A_el), "AT"), "SE_i40.11.k"] <- imported

## SI
domestic <- A_el[startsWith(rownames(A_el), "IT"), "IT_i40.11.e.1"]
imported <- A_el[startsWith(rownames(A_el), "SI"), "IT_i40.11.e.1"]
A_el[,"SI_i40.11.e.1"] <- A_el[,"IT_i40.11.e.1"]
A_el[startsWith(rownames(A_el), "SI"), "SI_i40.11.e.1"] <- domestic
A_el[startsWith(rownames(A_el), "IT"), "SI_i40.11.e.1"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "PT"), "PT_i40.11.e.2"]
imported <- A_el[startsWith(rownames(A_el), "SI"), "PT_i40.11.e.2"]
A_el[,"SI_i40.11.e.2"] <- A_el[,"PT_i40.11.e.2"]
A_el[startsWith(rownames(A_el), "SI"), "SI_i40.11.e.2"] <- domestic
A_el[startsWith(rownames(A_el), "PT"), "SI_i40.11.e.2"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "AT"), "AT_i40.11.h.2"]
imported <- A_el[startsWith(rownames(A_el), "SI"), "AT_i40.11.h.2"]
A_el[,"SI_i40.11.h.2"] <- A_el[,"AT_i40.11.h.2"]
A_el[startsWith(rownames(A_el), "SI"), "SI_i40.11.h.2"] <- domestic
A_el[startsWith(rownames(A_el), "AT"), "SI_i40.11.h.2"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "ES"), "ES_i40.11.i"]
imported <- A_el[startsWith(rownames(A_el), "SI"), "ES_i40.11.i"]
A_el[,"SI_i40.11.i"] <- A_el[,"ES_i40.11.i"]
A_el[startsWith(rownames(A_el), "SI"), "SI_i40.11.i"] <- domestic
A_el[startsWith(rownames(A_el), "ES"), "SI_i40.11.i"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "FR"), "FR_i40.11.j"]
imported <- A_el[startsWith(rownames(A_el), "SI"), "FR_i40.11.j"]
A_el[,"SI_i40.11.j"] <- A_el[,"FR_i40.11.j"]
A_el[startsWith(rownames(A_el), "SI"), "SI_i40.11.j"] <- domestic
A_el[startsWith(rownames(A_el), "FR"), "SI_i40.11.j"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "IT"), "IT_i40.11.k"]
imported <- A_el[startsWith(rownames(A_el), "SI"), "IT_i40.11.k"]
A_el[,"SI_i40.11.k"] <- A_el[,"IT_i40.11.k"]
A_el[startsWith(rownames(A_el), "SI"), "SI_i40.11.k"] <- domestic
A_el[startsWith(rownames(A_el), "IT"), "SI_i40.11.k"] <- imported

## SK
domestic <- A_el[startsWith(rownames(A_el), "DE"), "DE_i40.11.e.2"]
imported <- A_el[startsWith(rownames(A_el), "SK"), "DE_i40.11.e.2"]
A_el[,"SK_i40.11.e.2"] <- A_el[,"DE_i40.11.e.2"]
A_el[startsWith(rownames(A_el), "SK"), "SK_i40.11.e.2"] <- domestic
A_el[startsWith(rownames(A_el), "DE"), "SK_i40.11.e.2"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "ES"), "ES_i40.11.i"]
imported <- A_el[startsWith(rownames(A_el), "SK"), "ES_i40.11.i"]
A_el[,"SK_i40.11.i"] <- A_el[,"ES_i40.11.i"]
A_el[startsWith(rownames(A_el), "SK"), "SK_i40.11.i"] <- domestic
A_el[startsWith(rownames(A_el), "ES"), "SK_i40.11.i"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "FR"), "FR_i40.11.j"]
imported <- A_el[startsWith(rownames(A_el), "SK"), "FR_i40.11.j"]
A_el[,"SK_i40.11.j"] <- A_el[,"FR_i40.11.j"]
A_el[startsWith(rownames(A_el), "SK"), "SK_i40.11.j"] <- domestic
A_el[startsWith(rownames(A_el), "FR"), "SK_i40.11.j"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "AT"), "AT_i40.11.k"]
imported <- A_el[startsWith(rownames(A_el), "SK"), "AT_i40.11.k"]
A_el[,"SK_i40.11.k"] <- A_el[,"AT_i40.11.k"]
A_el[startsWith(rownames(A_el), "SK"), "SK_i40.11.k"] <- domestic
A_el[startsWith(rownames(A_el), "AT"), "SK_i40.11.k"] <- imported

## GB
domestic <- A_el[startsWith(rownames(A_el), "ES"), "ES_i40.11.i"]
imported <- A_el[startsWith(rownames(A_el), "GB"), "ES_i40.11.i"]
A_el[,"GB_i40.11.i"] <- A_el[,"ES_i40.11.i"]
A_el[startsWith(rownames(A_el), "GB"), "GB_i40.11.i"] <- domestic
A_el[startsWith(rownames(A_el), "ES"), "GB_i40.11.i"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "FR"), "FR_i40.11.j"]
imported <- A_el[startsWith(rownames(A_el), "GB"), "FR_i40.11.j"]
A_el[,"GB_i40.11.j"] <- A_el[,"FR_i40.11.j"]
A_el[startsWith(rownames(A_el), "GB"), "GB_i40.11.j"] <- domestic
A_el[startsWith(rownames(A_el), "FR"), "GB_i40.11.j"] <- imported
domestic <- A_el[startsWith(rownames(A_el), "AT"), "AT_i40.11.k"]
imported <- A_el[startsWith(rownames(A_el), "GB"), "AT_i40.11.k"]
A_el[,"GB_i40.11.k"] <- A_el[,"AT_i40.11.k"]
A_el[startsWith(rownames(A_el), "GB"), "GB_i40.11.k"] <- domestic
A_el[startsWith(rownames(A_el), "AT"), "GB_i40.11.k"] <- imported

## Rewrite Adet with values from A_el in the electricity sectors in EU27+UK
for (country in countries_all) {
  for (e in electricity_det) {
    Adet[,paste0(country, "_i40.11.", e)] <- A_el[,paste0(country, "_i40.11.", e)]
  }
}

## Load Edet (value added and employment accounts in the electricity sectors) and create replacements (E_el and Ext_el), replace empty columns with values from other countries
## There are zeros when there is no production from the sector
## When electricity would start to be produced from the empty source in the future, it would distort the results

## Read Edet and create E_el
E_el <- as.data.frame(Edet[Q.codes$Compartment %in% c("Value.added", "Employment.persons"),])
colnames(E_el) <- paste0(IOdet.codes$Country.Code,"_",IOdet.codes$Industry.Code)
E_el <- E_el[,All_electricity_det]

## Read Edet and create Ext_el (extension)
Ext_el <- as.data.frame(Edet[Q.codes$Compartment %in% c("Value.added", "Employment.persons"),])
colnames(Ext_el) <- paste0(IOdet.codes$Country.Code,"_",IOdet.codes$Industry.Code)
Ext_el <- t(t(Ext_el) / xdet2015)
Ext_el[!is.finite(Ext_el)] <- 0
Ext_el <- as.data.frame(Ext_el)
Ext_el <- Ext_el[,All_electricity_det]

## Replace columns with some production of electricity, but mistakenly zero employment (EXIOBASE errors)

## These need to be replaced first, so they are later below included in the changes (replacements) in Ext_el
Ext_el[c(10:15),"AT_i40.11.h.1"] <- Ext_el[c(10:15),"AT_i40.11.h.2"]
Ext_el[c(10:15),"AT_i40.11.k"] <- Ext_el[c(10:15),"IT_i40.11.k"]
Ext_el[c(10:15),"CY_i40.11.h.2"] <- Ext_el[c(10:15),"CY_i40.11.h.1"]
Ext_el[c(10:15),"CZ_i40.11.g"] <- Ext_el[c(10:15),"AT_i40.11.g"]
Ext_el[c(10:15),"DE_i40.11.k"] <- Ext_el[c(10:15),"IT_i40.11.k"]
Ext_el[c(10:15),"DK_i40.11.h.1"] <- Ext_el[c(10:15),"DK_i40.11.h.2"]
Ext_el[c(10:15),"FI_i40.11.e.2"] <- Ext_el[c(10:15),"FI_i40.11.e.1"]
Ext_el[c(10:15),"HU_i40.11.h.1"] <- Ext_el[c(10:15),"SK_i40.11.h.1"]
Ext_el[c(10:15),"HU_i40.11.h.2"] <- Ext_el[c(10:15),"SK_i40.11.h.2"]
Ext_el[c(10:15),"LU_i40.11.f"] <- Ext_el[c(10:15),"DE_i40.11.f"]
Ext_el[c(10:15),"LU_i40.11.g"] <- Ext_el[c(10:15),"DE_i40.11.g"]
Ext_el[c(10:15),"NL_i40.11.h.1"] <- Ext_el[c(10:15),"DE_i40.11.h.1"]
Ext_el[c(10:15),"RO_i40.11.h.1"] <- Ext_el[c(10:15),"BG_i40.11.h.1"]
Ext_el[c(10:15),"SE_i40.11.h.1"] <- Ext_el[c(10:15),"DK_i40.11.h.2"] ## yes, really h.2
Ext_el[c(10:15),"SE_i40.11.h.2"] <- Ext_el[c(10:15),"DK_i40.11.h.2"]

## Replace empty columns in Ext_el with values from other countries for the years from 2020 onwards (depending on the scenarios)

## AT
Ext_el[,"AT_i40.11.c"] <- Ext_el[,"CZ_i40.11.c"]
Ext_el[,"AT_i40.11.e.2"] <- Ext_el[,"DE_i40.11.e.2"]
Ext_el[,"AT_i40.11.i"] <- Ext_el[,"ES_i40.11.i"]
Ext_el[,"AT_i40.11.j"] <- Ext_el[,"FR_i40.11.j"]

## BE
Ext_el[,"BE_i40.11.i"] <- Ext_el[,"ES_i40.11.i"]
Ext_el[,"BE_i40.11.j"] <- Ext_el[,"FR_i40.11.j"]
Ext_el[,"BE_i40.11.k"] <- Ext_el[,"AT_i40.11.k"]

## BG
Ext_el[,"BG_i40.11.e.2"] <- Ext_el[,"PT_i40.11.e.2"]
Ext_el[,"BG_i40.11.i"] <- Ext_el[,"ES_i40.11.i"]
Ext_el[,"BG_i40.11.j"] <- Ext_el[,"FR_i40.11.j"]
Ext_el[,"BG_i40.11.k"] <- Ext_el[,"IT_i40.11.k"]

## CY
Ext_el[,"CY_i40.11.a"] <- Ext_el[,"GR_i40.11.a"]
Ext_el[,"CY_i40.11.b"] <- Ext_el[,"GR_i40.11.b"]
Ext_el[,"CY_i40.11.c"] <- Ext_el[,"BG_i40.11.c"]
Ext_el[,"CY_i40.11.d"] <- Ext_el[,"GR_i40.11.d"]
Ext_el[,"CY_i40.11.e.2"] <- Ext_el[,"PT_i40.11.e.2"]
Ext_el[,"CY_i40.11.i"] <- Ext_el[,"ES_i40.11.i"]
Ext_el[,"CY_i40.11.j"] <- Ext_el[,"FR_i40.11.j"]
Ext_el[,"CY_i40.11.k"] <- Ext_el[,"IT_i40.11.k"]

## CZ
Ext_el[,"CZ_i40.11.e.2"] <- Ext_el[,"DE_i40.11.e.2"]
Ext_el[,"CZ_i40.11.i"] <- Ext_el[,"ES_i40.11.i"]
Ext_el[,"CZ_i40.11.j"] <- Ext_el[,"FR_i40.11.j"]
Ext_el[,"CZ_i40.11.k"] <- Ext_el[,"AT_i40.11.k"]

## DE
Ext_el[,"DE_i40.11.i"] <- Ext_el[,"ES_i40.11.i"]
Ext_el[,"DE_i40.11.j"] <- Ext_el[,"FR_i40.11.j"]

## DK
Ext_el[,"DK_i40.11.c"] <- Ext_el[,"SE_i40.11.c"]
Ext_el[,"DK_i40.11.i"] <- Ext_el[,"ES_i40.11.i"]
Ext_el[,"DK_i40.11.j"] <- Ext_el[,"FR_i40.11.j"]
Ext_el[,"DK_i40.11.k"] <- Ext_el[,"AT_i40.11.k"]

## EE
Ext_el[,"EE_i40.11.c"] <- Ext_el[,"SE_i40.11.c"]
Ext_el[,"EE_i40.11.e.2"] <- Ext_el[,"DE_i40.11.e.2"]
Ext_el[,"EE_i40.11.h.1"] <- Ext_el[,"SE_i40.11.h.1"]
Ext_el[,"EE_i40.11.h.2"] <- Ext_el[,"FI_i40.11.h.2"]
Ext_el[,"EE_i40.11.i"] <- Ext_el[,"ES_i40.11.i"]
Ext_el[,"EE_i40.11.j"] <- Ext_el[,"FR_i40.11.j"]
Ext_el[,"EE_i40.11.k"] <- Ext_el[,"AT_i40.11.k"]

## ES
Ext_el[,"ES_i40.11.e.2"] <- Ext_el[,"PT_i40.11.e.2"]
Ext_el[,"ES_i40.11.j"] <- Ext_el[,"FR_i40.11.j"]
Ext_el[,"ES_i40.11.k"] <- Ext_el[,"PT_i40.11.k"]

## FI
Ext_el[,"FI_i40.11.h.1"] <- Ext_el[,"SE_i40.11.h.1"]
Ext_el[,"FI_i40.11.i"] <- Ext_el[,"ES_i40.11.i"]
Ext_el[,"FI_i40.11.j"] <- Ext_el[,"FR_i40.11.j"]
Ext_el[,"FI_i40.11.k"] <- Ext_el[,"AT_i40.11.k"]

## FR
Ext_el[,"FR_i40.11.e.2"] <- Ext_el[,"PT_i40.11.e.2"]
Ext_el[,"FR_i40.11.i"] <- Ext_el[,"ES_i40.11.i"]
Ext_el[,"FR_i40.11.k"] <- Ext_el[,"IT_i40.11.k"]

## GR
Ext_el[,"GR_i40.11.c"] <- Ext_el[,"BG_i40.11.c"]
Ext_el[,"GR_i40.11.e.2"] <- Ext_el[,"PT_i40.11.e.2"]
Ext_el[,"GR_i40.11.i"] <- Ext_el[,"ES_i40.11.i"]
Ext_el[,"GR_i40.11.j"] <- Ext_el[,"FR_i40.11.j"]
Ext_el[,"GR_i40.11.k"] <- Ext_el[,"IT_i40.11.k"]

## HR
Ext_el[,"HR_i40.11.c"] <- Ext_el[,"BG_i40.11.c"]
Ext_el[,"HR_i40.11.e.2"] <- Ext_el[,"PT_i40.11.e.2"]
Ext_el[,"HR_i40.11.h.1"] <- Ext_el[,"IT_i40.11.h.1"]
Ext_el[,"HR_i40.11.h.2"] <- Ext_el[,"IT_i40.11.h.2"]
Ext_el[,"HR_i40.11.i"] <- Ext_el[,"ES_i40.11.i"]
Ext_el[,"HR_i40.11.j"] <- Ext_el[,"FR_i40.11.j"]
Ext_el[,"HR_i40.11.k"] <- Ext_el[,"IT_i40.11.k"]

## HU
Ext_el[,"HU_i40.11.e.2"] <- Ext_el[,"DE_i40.11.e.2"]
Ext_el[,"HU_i40.11.i"] <- Ext_el[,"ES_i40.11.i"]
Ext_el[,"HU_i40.11.j"] <- Ext_el[,"FR_i40.11.j"]
Ext_el[,"HU_i40.11.k"] <- Ext_el[,"AT_i40.11.k"]

## IE
Ext_el[,"IE_i40.11.c"] <- Ext_el[,"GB_i40.11.c"]
Ext_el[,"IE_i40.11.e.2"] <- Ext_el[,"GB_i40.11.e.2"]
Ext_el[,"IE_i40.11.h.1"] <- Ext_el[,"GB_i40.11.h.1"]
Ext_el[,"IE_i40.11.h.2"] <- Ext_el[,"GB_i40.11.h.2"]
Ext_el[,"IE_i40.11.i"] <- Ext_el[,"ES_i40.11.i"]
Ext_el[,"IE_i40.11.j"] <- Ext_el[,"FR_i40.11.j"]
Ext_el[,"IE_i40.11.k"] <- Ext_el[,"AT_i40.11.k"]

## IT
Ext_el[,"IT_i40.11.c"] <- Ext_el[,"FR_i40.11.c"]
Ext_el[,"IT_i40.11.e.2"] <- Ext_el[,"PT_i40.11.e.2"]
Ext_el[,"IT_i40.11.i"] <- Ext_el[,"ES_i40.11.i"]
Ext_el[,"IT_i40.11.j"] <- Ext_el[,"FR_i40.11.j"]

## LT
Ext_el[,"LT_i40.11.a"] <- Ext_el[,"EE_i40.11.a"]
Ext_el[,"LT_i40.11.c"] <- Ext_el[,"SE_i40.11.c"]
Ext_el[,"LT_i40.11.e.2"] <- Ext_el[,"DE_i40.11.e.2"]
Ext_el[,"LT_i40.11.h.1"] <- Ext_el[,"SE_i40.11.h.1"]
Ext_el[,"LT_i40.11.h.2"] <- Ext_el[,"FI_i40.11.h.2"]
Ext_el[,"LT_i40.11.i"] <- Ext_el[,"ES_i40.11.i"]
Ext_el[,"LT_i40.11.j"] <- Ext_el[,"FR_i40.11.j"]
Ext_el[,"LT_i40.11.k"] <- Ext_el[,"AT_i40.11.k"]

## LU
Ext_el[,"LU_i40.11.a"] <- Ext_el[,"DE_i40.11.a"]
Ext_el[,"LU_i40.11.c"] <- Ext_el[,"DE_i40.11.c"]
Ext_el[,"LU_i40.11.e.2"] <- Ext_el[,"DE_i40.11.e.2"]
Ext_el[,"LU_i40.11.h.1"] <- Ext_el[,"DE_i40.11.h.1"]
Ext_el[,"LU_i40.11.i"] <- Ext_el[,"ES_i40.11.i"]
Ext_el[,"LU_i40.11.j"] <- Ext_el[,"FR_i40.11.j"]
Ext_el[,"LU_i40.11.k"] <- Ext_el[,"AT_i40.11.k"]

## LV
Ext_el[,"LV_i40.11.c"] <- Ext_el[,"SE_i40.11.c"]
Ext_el[,"LV_i40.11.e.2"] <- Ext_el[,"DE_i40.11.e.2"]
Ext_el[,"LV_i40.11.f"] <- Ext_el[,"EE_i40.11.f"]
Ext_el[,"LV_i40.11.h.1"] <- Ext_el[,"SE_i40.11.h.1"]
Ext_el[,"LV_i40.11.h.2"] <- Ext_el[,"FI_i40.11.h.2"]
Ext_el[,"LV_i40.11.i"] <- Ext_el[,"ES_i40.11.i"]
Ext_el[,"LV_i40.11.j"] <- Ext_el[,"FR_i40.11.j"]
Ext_el[,"LV_i40.11.k"] <- Ext_el[,"AT_i40.11.k"]

## MT
Ext_el[,"MT_i40.11.a"] <- Ext_el[,"IT_i40.11.a"]
Ext_el[,"MT_i40.11.b"] <- Ext_el[,"IT_i40.11.b"]
Ext_el[,"MT_i40.11.c"] <- Ext_el[,"FR_i40.11.c"]
Ext_el[,"MT_i40.11.d"] <- Ext_el[,"IT_i40.11.d"]
Ext_el[,"MT_i40.11.e.1"] <- Ext_el[,"IT_i40.11.e.1"]
Ext_el[,"MT_i40.11.e.2"] <- Ext_el[,"PT_i40.11.e.2"]
Ext_el[,"MT_i40.11.f"] <- Ext_el[,"IT_i40.11.f"]
Ext_el[,"MT_i40.11.g"] <- Ext_el[,"IT_i40.11.g"]
Ext_el[,"MT_i40.11.h.1"] <- Ext_el[,"IT_i40.11.h.1"]
Ext_el[,"MT_i40.11.h.2"] <- Ext_el[,"IT_i40.11.h.2"]
Ext_el[,"MT_i40.11.i"] <- Ext_el[,"ES_i40.11.i"]
Ext_el[,"MT_i40.11.j"] <- Ext_el[,"FR_i40.11.j"]
Ext_el[,"MT_i40.11.k"] <- Ext_el[,"IT_i40.11.k"]

## NL
Ext_el[,"NL_i40.11.a"] <- Ext_el[,"DE_i40.11.a"]
Ext_el[,"NL_i40.11.c"] <- Ext_el[,"DE_i40.11.c"]
Ext_el[,"NL_i40.11.i"] <- Ext_el[,"ES_i40.11.i"]
Ext_el[,"NL_i40.11.j"] <- Ext_el[,"FR_i40.11.j"]
Ext_el[,"NL_i40.11.k"] <- Ext_el[,"AT_i40.11.k"]

## PL
Ext_el[,"PL_i40.11.c"] <- Ext_el[,"DE_i40.11.c"]
Ext_el[,"PL_i40.11.e.2"] <- Ext_el[,"DE_i40.11.e.2"]
Ext_el[,"PL_i40.11.h.1"] <- Ext_el[,"CZ_i40.11.h.1"]
Ext_el[,"PL_i40.11.h.2"] <- Ext_el[,"CZ_i40.11.h.2"]
Ext_el[,"PL_i40.11.i"] <- Ext_el[,"ES_i40.11.i"]
Ext_el[,"PL_i40.11.j"] <- Ext_el[,"FR_i40.11.j"]
Ext_el[,"PL_i40.11.k"] <- Ext_el[,"AT_i40.11.k"]

## PT
Ext_el[,"PT_i40.11.c"] <- Ext_el[,"ES_i40.11.c"]
Ext_el[,"PT_i40.11.i"] <- Ext_el[,"ES_i40.11.i"]
Ext_el[,"PT_i40.11.j"] <- Ext_el[,"FR_i40.11.j"]

## RO
Ext_el[,"RO_i40.11.e.2"] <- Ext_el[,"PT_i40.11.e.2"]
Ext_el[,"RO_i40.11.h.2"] <- Ext_el[,"HU_i40.11.h.2"]
Ext_el[,"RO_i40.11.i"] <- Ext_el[,"ES_i40.11.i"]
Ext_el[,"RO_i40.11.j"] <- Ext_el[,"FR_i40.11.j"]
Ext_el[,"RO_i40.11.k"] <- Ext_el[,"IT_i40.11.k"]

## SE
Ext_el[,"SE_i40.11.i"] <- Ext_el[,"ES_i40.11.i"]
Ext_el[,"SE_i40.11.j"] <- Ext_el[,"FR_i40.11.j"]
Ext_el[,"SE_i40.11.k"] <- Ext_el[,"AT_i40.11.k"]

# SI
Ext_el[,"SI_i40.11.e.1"] <- Ext_el[,"IT_i40.11.e.1"]
Ext_el[,"SI_i40.11.e.2"] <- Ext_el[,"PT_i40.11.e.2"]
Ext_el[,"SI_i40.11.h.2"] <- Ext_el[,"AT_i40.11.h.2"]
Ext_el[,"SI_i40.11.i"] <- Ext_el[,"ES_i40.11.i"]
Ext_el[,"SI_i40.11.j"] <- Ext_el[,"FR_i40.11.j"]
Ext_el[,"SI_i40.11.k"] <- Ext_el[,"IT_i40.11.k"]

## SK
Ext_el[,"SK_i40.11.e.2"] <- Ext_el[,"DE_i40.11.e.2"]
Ext_el[,"SK_i40.11.i"] <- Ext_el[,"ES_i40.11.i"]
Ext_el[,"SK_i40.11.j"] <- Ext_el[,"FR_i40.11.j"]
Ext_el[,"SK_i40.11.k"] <- Ext_el[,"AT_i40.11.k"]

## GB
Ext_el[,"GB_i40.11.i"] <- Ext_el[,"ES_i40.11.i"]
Ext_el[,"GB_i40.11.j"] <- Ext_el[,"FR_i40.11.j"]
Ext_el[,"GB_i40.11.k"] <- Ext_el[,"AT_i40.11.k"]

## Check
check <- colSums(A_el) + colSums(Ext_el[1:9,])
check <- t(as.data.frame(check))

## Extract shares of different skill levels to the original total employment intensity (with replaced values for the empty columns)
emp_low_m <- Ext_el[Q.codes$Stressor=="Employment: Low-skilled male",]
emp_low_f <- Ext_el[Q.codes$Stressor=="Employment: Low-skilled female",]
emp_medium_m <- Ext_el[Q.codes$Stressor=="Employment: Medium-skilled male",]
emp_medium_f <- Ext_el[Q.codes$Stressor=="Employment: Medium-skilled female",]
emp_high_m <- Ext_el[Q.codes$Stressor=="Employment: High-skilled male",]
emp_high_f <- Ext_el[Q.codes$Stressor=="Employment: High-skilled female",]

## Calculate emp_tot (total employment intensity)
emp_tot <- emp_low_m + emp_low_f + emp_medium_m + emp_medium_f + emp_high_m + emp_high_f

share_low_m <- emp_low_m / emp_tot
share_low_f <- emp_low_f / emp_tot
share_medium_m <- emp_medium_m / emp_tot
share_medium_f <- emp_medium_f / emp_tot
share_high_m <- emp_high_m / emp_tot
share_high_f <- emp_high_f / emp_tot

## Load adjusted E_emp (adjusted employment by source and by country) and calculate new employment intensities, using Xdet_el
E_emp <- readxl::read_excel(paste0(datapath,"Employment_PostCarbon_Europe_Data.xlsx"), sheet = "emp_det")
E_emp <- as.data.frame(E_emp)
rownames(E_emp) <- paste0(E_emp$Country.Code,"_",E_emp$Industry.Code)
E_emp[,c(1,2,3)] <- NULL

## Replace empty rows in E_emp, create xdet_el and replace empty rows in xdet_el

## AT
E_emp["AT_i40.11.c",] <- E_emp["CZ_i40.11.c",]
E_emp["AT_i40.11.e.2",] <- E_emp["DE_i40.11.e.2",]
E_emp["AT_i40.11.i",] <- E_emp["ES_i40.11.i",]
E_emp["AT_i40.11.j",] <- E_emp["FR_i40.11.j",]

## BE
E_emp["BE_i40.11.i",] <- E_emp["ES_i40.11.i",]
E_emp["BE_i40.11.j",] <- E_emp["FR_i40.11.j",]
E_emp["BE_i40.11.k",] <- E_emp["AT_i40.11.k",]

## BG
E_emp["BG_i40.11.e.2",] <- E_emp["PT_i40.11.e.2",]
E_emp["BG_i40.11.i",] <- E_emp["ES_i40.11.i",]
E_emp["BG_i40.11.j",] <- E_emp["FR_i40.11.j",]
E_emp["BG_i40.11.k",] <- E_emp["IT_i40.11.k",]

## CY
E_emp["CY_i40.11.a",] <- E_emp["GR_i40.11.a",]
E_emp["CY_i40.11.b",] <- E_emp["GR_i40.11.b",]
E_emp["CY_i40.11.c",] <- E_emp["BG_i40.11.c",]
E_emp["CY_i40.11.d",] <- E_emp["GR_i40.11.d",]
E_emp["CY_i40.11.e.2",] <- E_emp["PT_i40.11.e.2",]
E_emp["CY_i40.11.i",] <- E_emp["ES_i40.11.i",]
E_emp["CY_i40.11.j",] <- E_emp["FR_i40.11.j",]
E_emp["CY_i40.11.k",] <- E_emp["IT_i40.11.k",]

## CZ
E_emp["CZ_i40.11.e.2",] <- E_emp["DE_i40.11.e.2",]
E_emp["CZ_i40.11.i",] <- E_emp["ES_i40.11.i",]
E_emp["CZ_i40.11.j",] <- E_emp["FR_i40.11.j",]
E_emp["CZ_i40.11.k",] <- E_emp["AT_i40.11.k",]

## DE
E_emp["DE_i40.11.i",] <- E_emp["ES_i40.11.i",]
E_emp["DE_i40.11.j",] <- E_emp["FR_i40.11.j",]

## DK
E_emp["DK_i40.11.c",] <- E_emp["SE_i40.11.c",]
E_emp["DK_i40.11.i",] <- E_emp["ES_i40.11.i",]
E_emp["DK_i40.11.j",] <- E_emp["FR_i40.11.j",]
E_emp["DK_i40.11.k",] <- E_emp["AT_i40.11.k",]

## EE
E_emp["EE_i40.11.c",] <- E_emp["SE_i40.11.c",]
E_emp["EE_i40.11.e.2",] <- E_emp["DE_i40.11.e.2",]
E_emp["EE_i40.11.h.1",] <- E_emp["SE_i40.11.h.1",]
E_emp["EE_i40.11.h.2",] <- E_emp["FI_i40.11.h.2",]
E_emp["EE_i40.11.i",] <- E_emp["ES_i40.11.i",]
E_emp["EE_i40.11.j",] <- E_emp["FR_i40.11.j",]
E_emp["EE_i40.11.k",] <- E_emp["AT_i40.11.k",]

## ES
E_emp["ES_i40.11.e.2",] <- E_emp["PT_i40.11.e.2",]
E_emp["ES_i40.11.j",] <- E_emp["FR_i40.11.j",]
E_emp["ES_i40.11.k",] <- E_emp["PT_i40.11.k",]

## FI
E_emp["FI_i40.11.h.1",] <- E_emp["SE_i40.11.h.1",]
E_emp["FI_i40.11.i",] <- E_emp["ES_i40.11.i",]
E_emp["FI_i40.11.j",] <- E_emp["FR_i40.11.j",]
E_emp["FI_i40.11.k",] <- E_emp["AT_i40.11.k",]

## FR
E_emp["FR_i40.11.e.2",] <- E_emp["PT_i40.11.e.2",]
E_emp["FR_i40.11.i",] <- E_emp["ES_i40.11.i",]
E_emp["FR_i40.11.k",] <- E_emp["IT_i40.11.k",]

## GR
E_emp["GR_i40.11.c",] <- E_emp["BG_i40.11.c",]
E_emp["GR_i40.11.e.2",] <- E_emp["PT_i40.11.e.2",]
E_emp["GR_i40.11.i",] <- E_emp["ES_i40.11.i",]
E_emp["GR_i40.11.j",] <- E_emp["FR_i40.11.j",]
E_emp["GR_i40.11.k",] <- E_emp["IT_i40.11.k",]

## HR
E_emp["HR_i40.11.c",] <- E_emp["BG_i40.11.c",]
E_emp["HR_i40.11.e.2",] <- E_emp["PT_i40.11.e.2",]
E_emp["HR_i40.11.h.1",] <- E_emp["IT_i40.11.h.1",]
E_emp["HR_i40.11.h.2",] <- E_emp["IT_i40.11.h.2",]
E_emp["HR_i40.11.i",] <- E_emp["ES_i40.11.i",]
E_emp["HR_i40.11.j",] <- E_emp["FR_i40.11.j",]
E_emp["HR_i40.11.k",] <- E_emp["IT_i40.11.k",]

## HU
E_emp["HU_i40.11.e.2",] <- E_emp["DE_i40.11.e.2",]
E_emp["HU_i40.11.i",] <- E_emp["ES_i40.11.i",]
E_emp["HU_i40.11.j",] <- E_emp["FR_i40.11.j",]
E_emp["HU_i40.11.k",] <- E_emp["AT_i40.11.k",]

## IE
E_emp["IE_i40.11.c",] <- E_emp["GB_i40.11.c",]
E_emp["IE_i40.11.e.2",] <- E_emp["GB_i40.11.e.2",]
E_emp["IE_i40.11.h.1",] <- E_emp["GB_i40.11.h.1",]
E_emp["IE_i40.11.h.2",] <- E_emp["GB_i40.11.h.2",]
E_emp["IE_i40.11.i",] <- E_emp["ES_i40.11.i",]
E_emp["IE_i40.11.j",] <- E_emp["FR_i40.11.j",]
E_emp["IE_i40.11.k",] <- E_emp["AT_i40.11.k",]

## IT
E_emp["IT_i40.11.c",] <- E_emp["FR_i40.11.c",]
E_emp["IT_i40.11.e.2",] <- E_emp["PT_i40.11.e.2",]
E_emp["IT_i40.11.i",] <- E_emp["ES_i40.11.i",]
E_emp["IT_i40.11.j",] <- E_emp["FR_i40.11.j",]

## LT
E_emp["LT_i40.11.a",] <- E_emp["EE_i40.11.a",]
E_emp["LT_i40.11.c",] <- E_emp["SE_i40.11.c",]
E_emp["LT_i40.11.e.2",] <- E_emp["DE_i40.11.e.2",]
E_emp["LT_i40.11.h.1",] <- E_emp["SE_i40.11.h.1",]
E_emp["LT_i40.11.h.2",] <- E_emp["FI_i40.11.h.2",]
E_emp["LT_i40.11.i",] <- E_emp["ES_i40.11.i",]
E_emp["LT_i40.11.j",] <- E_emp["FR_i40.11.j",]
E_emp["LT_i40.11.k",] <- E_emp["AT_i40.11.k",]

## LU
E_emp["LU_i40.11.a",] <- E_emp["DE_i40.11.a",]
E_emp["LU_i40.11.c",] <- E_emp["DE_i40.11.c",]
E_emp["LU_i40.11.e.2",] <- E_emp["DE_i40.11.e.2",]
E_emp["LU_i40.11.h.1",] <- E_emp["DE_i40.11.h.1",]
E_emp["LU_i40.11.i",] <- E_emp["ES_i40.11.i",]
E_emp["LU_i40.11.j",] <- E_emp["FR_i40.11.j",]
E_emp["LU_i40.11.k",] <- E_emp["AT_i40.11.k",]

## LV
E_emp["LV_i40.11.c",] <- E_emp["SE_i40.11.c",]
E_emp["LV_i40.11.e.2",] <- E_emp["DE_i40.11.e.2",]
E_emp["LV_i40.11.f",] <- E_emp["EE_i40.11.f",]
E_emp["LV_i40.11.h.1",] <- E_emp["SE_i40.11.h.1",]
E_emp["LV_i40.11.h.2",] <- E_emp["FI_i40.11.h.2",]
E_emp["LV_i40.11.i",] <- E_emp["ES_i40.11.i",]
E_emp["LV_i40.11.j",] <- E_emp["FR_i40.11.j",]
E_emp["LV_i40.11.k",] <- E_emp["AT_i40.11.k",]

## MT
E_emp["MT_i40.11.a",] <- E_emp["IT_i40.11.a",]
E_emp["MT_i40.11.b",] <- E_emp["IT_i40.11.b",]
E_emp["MT_i40.11.c",] <- E_emp["FR_i40.11.c",]
E_emp["MT_i40.11.d",] <- E_emp["IT_i40.11.d",]
E_emp["MT_i40.11.e.1",] <- E_emp["IT_i40.11.e.1",]
E_emp["MT_i40.11.e.2",] <- E_emp["PT_i40.11.e.2",]
E_emp["MT_i40.11.f",] <- E_emp["IT_i40.11.f",]
E_emp["MT_i40.11.g",] <- E_emp["IT_i40.11.g",]
E_emp["MT_i40.11.h.1",] <- E_emp["IT_i40.11.h.1",]
E_emp["MT_i40.11.h.2",] <- E_emp["IT_i40.11.h.2",]
E_emp["MT_i40.11.i",] <- E_emp["ES_i40.11.i",]
E_emp["MT_i40.11.j",] <- E_emp["FR_i40.11.j",]
E_emp["MT_i40.11.k",] <- E_emp["IT_i40.11.k",]

## NL
E_emp["NL_i40.11.a",] <- E_emp["DE_i40.11.a",]
E_emp["NL_i40.11.c",] <- E_emp["DE_i40.11.c",]
E_emp["NL_i40.11.i",] <- E_emp["ES_i40.11.i",]
E_emp["NL_i40.11.j",] <- E_emp["FR_i40.11.j",]
E_emp["NL_i40.11.k",] <- E_emp["AT_i40.11.k",]

## PL
E_emp["PL_i40.11.c",] <- E_emp["DE_i40.11.c",]
E_emp["PL_i40.11.e.2",] <- E_emp["DE_i40.11.e.2",]
E_emp["PL_i40.11.h.1",] <- E_emp["CZ_i40.11.h.1",]
E_emp["PL_i40.11.h.2",] <- E_emp["CZ_i40.11.h.2",]
E_emp["PL_i40.11.i",] <- E_emp["ES_i40.11.i",]
E_emp["PL_i40.11.j",] <- E_emp["FR_i40.11.j",]
E_emp["PL_i40.11.k",] <- E_emp["AT_i40.11.k",]

## PT
E_emp["PT_i40.11.c",] <- E_emp["ES_i40.11.c",]
E_emp["PT_i40.11.i",] <- E_emp["ES_i40.11.i",]
E_emp["PT_i40.11.j",] <- E_emp["FR_i40.11.j",]

## RO
E_emp["RO_i40.11.e.2",] <- E_emp["PT_i40.11.e.2",]
E_emp["RO_i40.11.h.2",] <- E_emp["HU_i40.11.h.2",]
E_emp["RO_i40.11.i",] <- E_emp["ES_i40.11.i",]
E_emp["RO_i40.11.j",] <- E_emp["FR_i40.11.j",]
E_emp["RO_i40.11.k",] <- E_emp["IT_i40.11.k",]

## SE
E_emp["SE_i40.11.i",] <- E_emp["ES_i40.11.i",]
E_emp["SE_i40.11.j",] <- E_emp["FR_i40.11.j",]
E_emp["SE_i40.11.k",] <- E_emp["AT_i40.11.k",]

# SI
E_emp["SI_i40.11.e.1",] <- E_emp["IT_i40.11.e.1",]
E_emp["SI_i40.11.e.2",] <- E_emp["PT_i40.11.e.2",]
E_emp["SI_i40.11.h.2",] <- E_emp["AT_i40.11.h.2",]
E_emp["SI_i40.11.i",] <- E_emp["ES_i40.11.i",]
E_emp["SI_i40.11.j",] <- E_emp["FR_i40.11.j",]
E_emp["SI_i40.11.k",] <- E_emp["IT_i40.11.k",]

## SK
E_emp["SK_i40.11.e.2",] <- E_emp["DE_i40.11.e.2",]
E_emp["SK_i40.11.i",] <- E_emp["ES_i40.11.i",]
E_emp["SK_i40.11.j",] <- E_emp["FR_i40.11.j",]
E_emp["SK_i40.11.k",] <- E_emp["AT_i40.11.k",]

## GB
E_emp["GB_i40.11.i",] <- E_emp["ES_i40.11.i",]
E_emp["GB_i40.11.j",] <- E_emp["FR_i40.11.j",]
E_emp["GB_i40.11.k",] <- E_emp["AT_i40.11.k",]

## Replace the empty rows in x (xdet_el)
## xdet_el is created to calculate the adjusted intensities in Ext_el, so that the empty columns in Ext_el do not remain with zeros
xdet_el <- xdet2015
xdet_el <- as.data.frame(xdet_el)
rownames(xdet_el) <- paste0(IOdet.codes$Country.Code,"_",IOdet.codes$Industry.Code)

## AT
xdet_el["AT_i40.11.c",] <- xdet_el["CZ_i40.11.c",]
xdet_el["AT_i40.11.e.2",] <- xdet_el["DE_i40.11.e.2",]
xdet_el["AT_i40.11.i",] <- xdet_el["ES_i40.11.i",]
xdet_el["AT_i40.11.j",] <- xdet_el["FR_i40.11.j",]

## BE
xdet_el["BE_i40.11.i",] <- xdet_el["ES_i40.11.i",]
xdet_el["BE_i40.11.j",] <- xdet_el["FR_i40.11.j",]
xdet_el["BE_i40.11.k",] <- xdet_el["AT_i40.11.k",]

## BG
xdet_el["BG_i40.11.e.2",] <- xdet_el["PT_i40.11.e.2",]
xdet_el["BG_i40.11.i",] <- xdet_el["ES_i40.11.i",]
xdet_el["BG_i40.11.j",] <- xdet_el["FR_i40.11.j",]
xdet_el["BG_i40.11.k",] <- xdet_el["IT_i40.11.k",]

## CY
xdet_el["CY_i40.11.a",] <- xdet_el["GR_i40.11.a",]
xdet_el["CY_i40.11.b",] <- xdet_el["GR_i40.11.b",]
xdet_el["CY_i40.11.c",] <- xdet_el["BG_i40.11.c",]
xdet_el["CY_i40.11.d",] <- xdet_el["GR_i40.11.d",]
xdet_el["CY_i40.11.e.2",] <- xdet_el["PT_i40.11.e.2",]
xdet_el["CY_i40.11.i",] <- xdet_el["ES_i40.11.i",]
xdet_el["CY_i40.11.j",] <- xdet_el["FR_i40.11.j",]
xdet_el["CY_i40.11.k",] <- xdet_el["IT_i40.11.k",]

## CZ
xdet_el["CZ_i40.11.e.2",] <- xdet_el["DE_i40.11.e.2",]
xdet_el["CZ_i40.11.i",] <- xdet_el["ES_i40.11.i",]
xdet_el["CZ_i40.11.j",] <- xdet_el["FR_i40.11.j",]
xdet_el["CZ_i40.11.k",] <- xdet_el["AT_i40.11.k",]

## DE
xdet_el["DE_i40.11.i",] <- xdet_el["ES_i40.11.i",]
xdet_el["DE_i40.11.j",] <- xdet_el["FR_i40.11.j",]

## DK
xdet_el["DK_i40.11.c",] <- xdet_el["SE_i40.11.c",]
xdet_el["DK_i40.11.i",] <- xdet_el["ES_i40.11.i",]
xdet_el["DK_i40.11.j",] <- xdet_el["FR_i40.11.j",]
xdet_el["DK_i40.11.k",] <- xdet_el["AT_i40.11.k",]

## EE
xdet_el["EE_i40.11.c",] <- xdet_el["SE_i40.11.c",]
xdet_el["EE_i40.11.e.2",] <- xdet_el["DE_i40.11.e.2",]
xdet_el["EE_i40.11.h.1",] <- xdet_el["SE_i40.11.h.1",]
xdet_el["EE_i40.11.h.2",] <- xdet_el["FI_i40.11.h.2",]
xdet_el["EE_i40.11.i",] <- xdet_el["ES_i40.11.i",]
xdet_el["EE_i40.11.j",] <- xdet_el["FR_i40.11.j",]
xdet_el["EE_i40.11.k",] <- xdet_el["AT_i40.11.k",]

## ES
xdet_el["ES_i40.11.e.2",] <- xdet_el["PT_i40.11.e.2",]
xdet_el["ES_i40.11.j",] <- xdet_el["FR_i40.11.j",]
xdet_el["ES_i40.11.k",] <- xdet_el["PT_i40.11.k",]

## FI
xdet_el["FI_i40.11.h.1",] <- xdet_el["SE_i40.11.h.1",]
xdet_el["FI_i40.11.i",] <- xdet_el["ES_i40.11.i",]
xdet_el["FI_i40.11.j",] <- xdet_el["FR_i40.11.j",]
xdet_el["FI_i40.11.k",] <- xdet_el["AT_i40.11.k",]

## FR
xdet_el["FR_i40.11.e.2",] <- xdet_el["PT_i40.11.e.2",]
xdet_el["FR_i40.11.i",] <- xdet_el["ES_i40.11.i",]
xdet_el["FR_i40.11.k",] <- xdet_el["IT_i40.11.k",]

## GR
xdet_el["GR_i40.11.c",] <- xdet_el["BG_i40.11.c",]
xdet_el["GR_i40.11.e.2",] <- xdet_el["PT_i40.11.e.2",]
xdet_el["GR_i40.11.i",] <- xdet_el["ES_i40.11.i",]
xdet_el["GR_i40.11.j",] <- xdet_el["FR_i40.11.j",]
xdet_el["GR_i40.11.k",] <- xdet_el["IT_i40.11.k",]

## HR
xdet_el["HR_i40.11.c",] <- xdet_el["BG_i40.11.c",]
xdet_el["HR_i40.11.e.2",] <- xdet_el["PT_i40.11.e.2",]
xdet_el["HR_i40.11.h.1",] <- xdet_el["IT_i40.11.h.1",]
xdet_el["HR_i40.11.h.2",] <- xdet_el["IT_i40.11.h.2",]
xdet_el["HR_i40.11.i",] <- xdet_el["ES_i40.11.i",]
xdet_el["HR_i40.11.j",] <- xdet_el["FR_i40.11.j",]
xdet_el["HR_i40.11.k",] <- xdet_el["IT_i40.11.k",]

## HU
xdet_el["HU_i40.11.e.2",] <- xdet_el["DE_i40.11.e.2",]
xdet_el["HU_i40.11.i",] <- xdet_el["ES_i40.11.i",]
xdet_el["HU_i40.11.j",] <- xdet_el["FR_i40.11.j",]
xdet_el["HU_i40.11.k",] <- xdet_el["AT_i40.11.k",]

## IE
xdet_el["IE_i40.11.c",] <- xdet_el["GB_i40.11.c",]
xdet_el["IE_i40.11.e.2",] <- xdet_el["GB_i40.11.e.2",]
xdet_el["IE_i40.11.h.1",] <- xdet_el["GB_i40.11.h.1",]
xdet_el["IE_i40.11.h.2",] <- xdet_el["GB_i40.11.h.2",]
xdet_el["IE_i40.11.i",] <- xdet_el["ES_i40.11.i",]
xdet_el["IE_i40.11.j",] <- xdet_el["FR_i40.11.j",]
xdet_el["IE_i40.11.k",] <- xdet_el["AT_i40.11.k",]

## IT
xdet_el["IT_i40.11.c",] <- xdet_el["FR_i40.11.c",]
xdet_el["IT_i40.11.e.2",] <- xdet_el["PT_i40.11.e.2",]
xdet_el["IT_i40.11.i",] <- xdet_el["ES_i40.11.i",]
xdet_el["IT_i40.11.j",] <- xdet_el["FR_i40.11.j",]

## LT
xdet_el["LT_i40.11.a",] <- xdet_el["EE_i40.11.a",]
xdet_el["LT_i40.11.c",] <- xdet_el["SE_i40.11.c",]
xdet_el["LT_i40.11.e.2",] <- xdet_el["DE_i40.11.e.2",]
xdet_el["LT_i40.11.h.1",] <- xdet_el["SE_i40.11.h.1",]
xdet_el["LT_i40.11.h.2",] <- xdet_el["FI_i40.11.h.2",]
xdet_el["LT_i40.11.i",] <- xdet_el["ES_i40.11.i",]
xdet_el["LT_i40.11.j",] <- xdet_el["FR_i40.11.j",]
xdet_el["LT_i40.11.k",] <- xdet_el["AT_i40.11.k",]

## LU
xdet_el["LU_i40.11.a",] <- xdet_el["DE_i40.11.a",]
xdet_el["LU_i40.11.c",] <- xdet_el["DE_i40.11.c",]
xdet_el["LU_i40.11.e.2",] <- xdet_el["DE_i40.11.e.2",]
xdet_el["LU_i40.11.h.1",] <- xdet_el["DE_i40.11.h.1",]
xdet_el["LU_i40.11.i",] <- xdet_el["ES_i40.11.i",]
xdet_el["LU_i40.11.j",] <- xdet_el["FR_i40.11.j",]
xdet_el["LU_i40.11.k",] <- xdet_el["AT_i40.11.k",]

## LV
xdet_el["LV_i40.11.c",] <- xdet_el["SE_i40.11.c",]
xdet_el["LV_i40.11.e.2",] <- xdet_el["DE_i40.11.e.2",]
xdet_el["LV_i40.11.f",] <- xdet_el["EE_i40.11.f",]
xdet_el["LV_i40.11.h.1",] <- xdet_el["SE_i40.11.h.1",]
xdet_el["LV_i40.11.h.2",] <- xdet_el["FI_i40.11.h.2",]
xdet_el["LV_i40.11.i",] <- xdet_el["ES_i40.11.i",]
xdet_el["LV_i40.11.j",] <- xdet_el["FR_i40.11.j",]
xdet_el["LV_i40.11.k",] <- xdet_el["AT_i40.11.k",]

## MT
xdet_el["MT_i40.11.a",] <- xdet_el["IT_i40.11.a",]
xdet_el["MT_i40.11.b",] <- xdet_el["IT_i40.11.b",]
xdet_el["MT_i40.11.c",] <- xdet_el["FR_i40.11.c",]
xdet_el["MT_i40.11.d",] <- xdet_el["IT_i40.11.d",]
xdet_el["MT_i40.11.e.1",] <- xdet_el["IT_i40.11.e.1",]
xdet_el["MT_i40.11.e.2",] <- xdet_el["PT_i40.11.e.2",]
xdet_el["MT_i40.11.f",] <- xdet_el["IT_i40.11.f",]
xdet_el["MT_i40.11.g",] <- xdet_el["IT_i40.11.g",]
xdet_el["MT_i40.11.h.1",] <- xdet_el["IT_i40.11.h.1",]
xdet_el["MT_i40.11.h.2",] <- xdet_el["IT_i40.11.h.2",]
xdet_el["MT_i40.11.i",] <- xdet_el["ES_i40.11.i",]
xdet_el["MT_i40.11.j",] <- xdet_el["FR_i40.11.j",]
xdet_el["MT_i40.11.k",] <- xdet_el["IT_i40.11.k",]

## NL
xdet_el["NL_i40.11.a",] <- xdet_el["DE_i40.11.a",]
xdet_el["NL_i40.11.c",] <- xdet_el["DE_i40.11.c",]
xdet_el["NL_i40.11.i",] <- xdet_el["ES_i40.11.i",]
xdet_el["NL_i40.11.j",] <- xdet_el["FR_i40.11.j",]
xdet_el["NL_i40.11.k",] <- xdet_el["AT_i40.11.k",]

## PL
xdet_el["PL_i40.11.c",] <- xdet_el["DE_i40.11.c",]
xdet_el["PL_i40.11.e.2",] <- xdet_el["DE_i40.11.e.2",]
xdet_el["PL_i40.11.h.1",] <- xdet_el["CZ_i40.11.h.1",]
xdet_el["PL_i40.11.h.2",] <- xdet_el["CZ_i40.11.h.2",]
xdet_el["PL_i40.11.i",] <- xdet_el["ES_i40.11.i",]
xdet_el["PL_i40.11.j",] <- xdet_el["FR_i40.11.j",]
xdet_el["PL_i40.11.k",] <- xdet_el["AT_i40.11.k",]

## PT
xdet_el["PT_i40.11.c",] <- xdet_el["ES_i40.11.c",]
xdet_el["PT_i40.11.i",] <- xdet_el["ES_i40.11.i",]
xdet_el["PT_i40.11.j",] <- xdet_el["FR_i40.11.j",]

## RO
xdet_el["RO_i40.11.e.2",] <- xdet_el["PT_i40.11.e.2",]
xdet_el["RO_i40.11.h.2",] <- xdet_el["HU_i40.11.h.2",]
xdet_el["RO_i40.11.i",] <- xdet_el["ES_i40.11.i",]
xdet_el["RO_i40.11.j",] <- xdet_el["FR_i40.11.j",]
xdet_el["RO_i40.11.k",] <- xdet_el["IT_i40.11.k",]

## SE
xdet_el["SE_i40.11.i",] <- xdet_el["ES_i40.11.i",]
xdet_el["SE_i40.11.j",] <- xdet_el["FR_i40.11.j",]
xdet_el["SE_i40.11.k",] <- xdet_el["AT_i40.11.k",]

# SI
xdet_el["SI_i40.11.e.1",] <- xdet_el["IT_i40.11.e.1",]
xdet_el["SI_i40.11.e.2",] <- xdet_el["PT_i40.11.e.2",]
xdet_el["SI_i40.11.h.2",] <- xdet_el["AT_i40.11.h.2",]
xdet_el["SI_i40.11.i",] <- xdet_el["ES_i40.11.i",]
xdet_el["SI_i40.11.j",] <- xdet_el["FR_i40.11.j",]
xdet_el["SI_i40.11.k",] <- xdet_el["IT_i40.11.k",]

## SK
xdet_el["SK_i40.11.e.2",] <- xdet_el["DE_i40.11.e.2",]
xdet_el["SK_i40.11.i",] <- xdet_el["ES_i40.11.i",]
xdet_el["SK_i40.11.j",] <- xdet_el["FR_i40.11.j",]
xdet_el["SK_i40.11.k",] <- xdet_el["AT_i40.11.k",]

## GB
xdet_el["GB_i40.11.i",] <- xdet_el["ES_i40.11.i",]
xdet_el["GB_i40.11.j",] <- xdet_el["FR_i40.11.j",]
xdet_el["GB_i40.11.k",] <- xdet_el["AT_i40.11.k",]

xdet_el <- as.vector(t(xdet_el))

## Calculate the adjusted employment intensity, using xdet_el, and implement into Ext_el
E_emp <- as.data.frame(t(E_emp))
E_emp <- E_emp / xdet_el[All_electricity_det]
E_emp[is.nan(E_emp)] <- 0

Ext_el[Q.codes$Stressor=="Employment: Low-skilled male", c(1:ncol(E_emp))] <- share_low_m[,c(1:ncol(E_emp))] * E_emp
Ext_el[Q.codes$Stressor=="Employment: Low-skilled female", c(1:ncol(E_emp))] <- share_low_f[,c(1:ncol(E_emp))] * E_emp
Ext_el[Q.codes$Stressor=="Employment: Medium-skilled male", c(1:ncol(E_emp))] <- share_medium_m[,c(1:ncol(E_emp))] * E_emp
Ext_el[Q.codes$Stressor=="Employment: Medium-skilled female", c(1:ncol(E_emp))] <- share_medium_f[,c(1:ncol(E_emp))] * E_emp
Ext_el[Q.codes$Stressor=="Employment: High-skilled male", c(1:ncol(E_emp))] <- share_high_m[,c(1:ncol(E_emp))] * E_emp
Ext_el[Q.codes$Stressor=="Employment: High-skilled female", c(1:ncol(E_emp))] <- share_high_f[,c(1:ncol(E_emp))] * E_emp

Ext_el[is.nan(Ext_el)] <- 0

## Match the adapted extension values (employment intensities) into the original extensions (Extdet)
Extdet <- as.data.frame(Edet)
Extdet <- t(t(Extdet) / xdet2015) ## Or xdet_el (doesn't matter because values for the empty sectors are replaced below with Ext_el)
Extdet[!is.finite(Extdet)] <- 0
colnames(Extdet) <- paste0(IOdet.codes$Country.Code,"_",IOdet.codes$Industry.Code)

for (country in countries_all) {
  for (e in electricity_det) {
    Extdet[Q.codes$Compartment %in% c("Value.added","Employment.persons"),paste0(country, "_i40.11.", e)] <- Ext_el[,paste0(country, "_i40.11.", e)]
  }
}



##########################################################################
# 2. Implement scenarios into xdet, calculate Anoel, Anofuel and Lnoel and Lnofuel
##########################################################################

##########################################################################
# 2.1. Insert changes in electricity total output (xdet) for each modelled year based on the scenarios

## Read scenarios for the electricity sectors
scen <- list()

for (scenario in scenarios) {
  scen[[scenario]] <- readxl::read_excel(paste0(datapath, "Employment_PostCarbon_Europe_Data.xlsx"), sheet = paste0(scenario,"_det"))
  scen[[scenario]] <- as.data.frame(scen[[scenario]])
}

## Insert changes for each scenario and each modelled year into xdet
xdet <- list()

for (scenario in scenarios) {
  for (year in years) {
    xdet[[scenario]][[year]] <- xdet2015
    if(year>2015){
      xdet[[scenario]][[year]][EUelectricity_det] <- scen[[scenario]][,paste0("X", year)]
    }
  }
}


##########################################################################
# 2.2. Insert changes in demand (final use) for the other fuels for each modelled year based on the scenarios

Ddet <- matrix(0, nrow(Zdet), length(countries))
colnames(Ddet) <- countries
for (country in countries) {
  Ddet[, country] <- rowSums(Zdet[,IOdet.codes$Country.Code == country]) + rowSums(Ydet[,Y.codes$Region.Name == country])
}

## Read scenarios for the other fuels sectors
Ddet_scen <- list()

for (scenario in scenarios) {
  scen_fuel <- readxl::read_excel(paste0(datapath, "Employment_PostCarbon_Europe_Data.xlsx"), sheet = paste0(scenario,"_fuel"))
  scen_fuel[sapply(scen_fuel, is.character)] <- lapply(scen_fuel[sapply(scen_fuel, is.character)],as.factor)
  Ddet_years <- list()
  for(year in years){
    scen_fuel_year <- scen_fuel[, c("Country.Code", "Industry.Code", year)] %>% 
      tidyr::spread("Country.Code", value = year)
    scen_fuel_year <- scen_fuel_year[match(IOdet.codes$Industry.Code, scen_fuel_year$Industry.Code),]
    scen_fuel_year[, countries][is.na(scen_fuel_year[, countries])] <- 0
    Ddet_years[[year]] <- Ddet * scen_fuel_year[, countries]
  }
  Ddet_scen[[scenario]] <- Ddet_years
}


##########################################################################
# 2.3. Adapt Adet to Anoel and Anofuel and calculate Lnoel and Lnofuel

## Calcluate Anoel with all electricity rows in the EU set to zero to avoid changing the diagonal elements
Anoel <- Adet
Anofuel <- Adet

## Calcluate Anofuel with all electricity and other fuels rows in the EU set to zero to avoid changing the diagonal elements
Anoel[c(EUelectricity_det),] <- 0
Anofuel[c(EUfuel,EUelectricity_det),] <- 0

## Check memory
gc()

## Calculate Lnoel with adapted off-diagonal elements of the modelled electricity sectors in the EU countries
Lnoel <- solve(diag(nrow(Zdet))-Anoel)

## Calculate Lnofuel with adapted off-diagonal elements of the modelled electricity and other fuels sectors in the EU countries
Lnofuel <- solve(diag(nrow(Zdet))-Anofuel)



##########################################################################
# 3. Calculate footprint (employment requirements)
##########################################################################

##########################################################################
# 3.1. Prepare Ext by extracting indicators we are interested from Extdet, prepare Ext.names

Extdet <- as.data.frame(Extdet[Q.codes$Compartment=="Employment.persons" & Q.codes$Stressor!="Employment: Vulnerable employment",])
Ext <- t(Extdet)

Ext.names <- Q.codes$Stressor[Q.codes$Compartment=="Employment.persons" & Q.codes$Stressor!="Employment: Vulnerable employment"]
Ext.names <- gsub(": ", "_", Ext.names)


##########################################################################
# 3.2. Calculate employment footprint (requirements)

## Define footprint functions
footprint_om <- function(L,Ext,ee,results){
  
  ## Calculate Multiplier Matrix (MP)
  MP <- L * Ext[,ee]
  
  ## Calculate Footprint (FP = MP * x). x = Total Output
  FP <- t(t(MP) * x_data)
  FP <- cbind(IOdet.codes, data.frame(Year = rep(year,8085), Indicator = rep(Ext.names[ee],8085), Effect = rep("O&M",8085)), FP)
  FP <- melt(FP, id=c(colnames(IOdet.codes),"Year","Indicator","Effect"), variable.name="Electricity", value.name="Value")
  FP$Country.Destination <- substr(FP$Electricity, 1, 2)
  FP$Electricity <- substr(FP$Electricity, 4, 13)
  FP <- FP[FP$Electricity %in% paste0("i40.11.",electricity_det),]
  
  results <- rbindlist(list(results, FP), use.names = TRUE)
}

footprint_gfcf <- function(L,Ext,ee,country,results){
  
  ## Calculate Multiplier Matrix (MP)
  MP <- L * Ext[,ee]
  
  ## Calculate Footprint (FP = MP * YGFCF). YGFCF = GFCF of the modelled electricity sectors
  FP <- MP %*% as.matrix(Y_data[,YGFCF.codes$Region.Name==country])
  FP <- cbind(IOdet.codes, data.frame(Year = rep(year,8085), Country.Destination = country, Indicator = rep(Ext.names[ee],8085), Effect = rep("GFCF",8085)), FP)
  FP <- melt(FP, id=c(colnames(IOdet.codes),"Year","Country.Destination","Indicator","Effect"), variable.name="Electricity", value.name="Value")
  FP$Electricity <- substr(FP$Electricity, 4, 13)
  
  results <- rbindlist(list(results, FP), use.names = TRUE)
}

footprint_subst <- function(L,Ext,ee,X,results){
  
  ## Calculate Multiplier Matrix (MP)
  MP <- L * Ext[,ee]
  
  ## Calculate Footprint (FP = MP * X). X = Total Output matrix by modelled countries
  FP <- MP %*% as.matrix(X)
  FP <- cbind(IOdet.codes, data.frame(Year = rep(year,8085), Indicator = rep(Ext.names[ee],8085), Effect = rep("Subst",8085), Electricity = rep(NA,8085)), FP)
  FP <- melt(FP, id=c(colnames(IOdet.codes),"Year","Indicator","Effect","Electricity"), variable.name="Country.Destination", value.name="Value")
  
  results <- rbindlist(list(results, FP), use.names = TRUE)
}

## Check memory
gc()

## Calculate footprint
## scenario <- "stanford"
for (scenario in scenarios) {
  print(paste0(scenario," scenario"))
  
  ## year <- 2015
  for(year in years){
    print(paste0("year ",year))
    
    x_data <- xdet[[scenario]][[year]]
    Y_data <- YGFCF[[scenario]][[year]]
    
    ## Calculate footprints
    ## ee <- 1 (ee = employment extension)
    for(ee in 1:ncol(Ext)){
      print(paste0("extension ",ee))
      
      results <- data.frame()
      results <- footprint_om(L=Lnoel, Ext=Ext, ee=ee, results=results)
      results <- footprint_subst(L=Lnofuel, Ext=Ext, ee=ee, X=Ddet_scen[[scenario]][[year]], results=results)
      
      ## country <- "AT"
      for(country in countries){
        
        results <- footprint_gfcf(L=Lnoel, Ext=Ext, ee=ee, country=country, results=results)
        
      }
      data.table::fwrite(results, file = paste0(emppath,scenario,"_",year,"_",ee,".csv"), sep=";", dec=".")
    }
  }
}



##########################################################################
# 4. Load results and organise data
##########################################################################

## All results for the EU27+UK as a whole and by countries
## 1) Total (domestic vs. in the RoW)
## 2) By effects (O&M vs. GFCF) (domestic vs. in the RoW)
## 3) By regions of origin and sectors
## 3) By skill levels and gender (domestic)
## 5) By sectors and skill levels (domestic)
## 6) By each energy source (domestic vs. in the Row)
## 7) By each of the EU27+UK countries (domestic)

## Domestic = "country" is Country.Origin (note that I can only calculate such domestic effects for the EU27+UK area, mostly because I don't calculate GFCF effects in the RoW)
## Aggregated for EU27+UK is a "pure domestic" = where Country.Destination matches Country.Origin (this is what I do here below except for 4.6 where I distinguish between the EU27+UK countries)
## Foreign (abroad, RoW) = where Country.Destination is "country" and Country.Origin is anything else but "country"

## Note that the results omit the effects induced by possible changes outside the EU27+UK area
## (we leave out all RoW countries of destination).
## However, the energy mix outside the EU27+UK does not change, so it would only add up small extra employment
## that would remain constant over time anyway.


##########################################################################
# 4.0. Load results

## Load results for the EU27+UK
results <- data.frame()

for (scenario in scenarios) {
  print(paste0(scenario," scenario"))
  
  for (year in years) {
    print(paste0("year ",year))
    
    for (ee in 1:ncol(Ext)) {
      print(paste0("extension ",ee))
      
      ## Read data and remove NAs
      data <- read.csv(paste0(emppath,scenario,"_",year,"_",ee,".csv"), header=TRUE, sep=";")
      data[is.na(data)] <- 0
      
      ## Delete Industry.Group column, introduce "Scenario" column, rename Country.Code to Country.Origin, filter only EU27+UK results
      data$Industry.Group <- NULL
      data$Scenario <- c(rep(scenario))
      data <- data %>% 
        rename(Country.Origin = Country.Code) %>%
        filter(Country.Destination %in% countries)
      
      ## Adjust country names
      data$Country.Destination <- as.character(data$Country.Destination)
      data[grepl("O&M|GFCF",data$Effect),"Country.Destination"] <- rep(countries_names, each = 105105, times = 2) ## 2 = 2 effects
      data[grepl("Subst",data$Effect),"Country.Destination"] <- rep(countries_names, each = 8085)
      
      data$Country.Origin <- as.character(data$Country.Origin)
      data[grepl("O&M|GFCF",data$Effect),"Country.Origin"] <- rep(countries_names_all, each = 165, times = 728) ## 728 = 2 effects * 13 electricity sources * 28 countries of destination
      data[grepl("Subst",data$Effect),"Country.Origin"] <- rep(countries_names_all, each = 165, times = 28) ## 28 = 28 countries of destination
      
      ## Aggregate EU27+UK data into one region (Country.Destination, Country.Origin)
      data$Country.Destination[data$Country.Destination %in% c(countries_names)] <- "EU27+UK"
      data$Country.Origin[data$Country.Origin %in% c(countries_names)] <- "EU27+UK"
      data <- data %>%
        group_by(Industry.Code, Industry.Name, Country.Origin, Region.Code, Year, Indicator, Effect, Electricity, Country.Destination, Scenario) %>%
        summarize(Value = sum(Value))
      
      ## rbind to get results for each scenario, year and extension category below each other
      results <- bind_rows(results, data)
    }
  }
}

## Load results by countries
results_countries <- data.frame()

for (scenario in scenarios) {
  print(paste0(scenario," scenario"))
  
  for (year in years) {
    print(paste0("year ",year))
    
    for (ee in 1:ncol(Ext)) {
      print(paste0("extension ",ee))
      
      ## Read data and remove NAs
      data <- read.csv(paste0(emppath,scenario,"_",year,"_",ee,".csv"), header=TRUE, sep=";")
      data[is.na(data)] <- 0
      
      ## Delete Industry.Group column, introduce "Scenario" column, rename Country.Code to Country.Origin, filter only EU27+UK results
      data$Industry.Group <- NULL
      data$Scenario <- c(rep(scenario))
      data <- data %>% 
        rename(Country.Origin = Country.Code) %>%
        filter(Country.Destination %in% countries)
      
      ## Adjust country names
      data$Country.Destination <- as.character(data$Country.Destination)
      data[grepl("O&M|GFCF",data$Effect),"Country.Destination"] <- rep(countries_names, each = 105105, times = 2) ## 2 = 2 effects
      data[grepl("Subst",data$Effect),"Country.Destination"] <- rep(countries_names, each = 8085)
      
      data$Country.Origin <- as.character(data$Country.Origin)
      data[grepl("O&M|GFCF",data$Effect),"Country.Origin"] <- rep(countries_names_all, each = 165, times = 728) ## 728 = 2 effects * 13 electricity sources * 28 countries of destination
      data[grepl("Subst",data$Effect),"Country.Origin"] <- rep(countries_names_all, each = 165, times = 28) ## 28 = 28 countries of destination
      
      ## Aggregate data by countries of origin and effects (and nothing else)
      data <- data %>% 
        group_by(Country.Origin, Country.Destination, Year, Effect, Scenario) %>%
        summarize(Value = sum(Value))
      
      ## rbind to get results for each scenario, year and extension category below each other
      results_countries <- bind_rows(results_countries, data)
    }
  }
}


##########################################################################
# 4.1. Organise results by effects (O&M vs. GFCF vs. Subst, domestic vs. foreign) and plot

## Organise domestic (EU27+UK) versus foreign (RoW) effects
results_effects <- results
results_effects$Country.Origin[results_effects$Country.Origin %!in% c("EU27+UK")] <- "RoW"
results_effects <- results_effects %>% 
  group_by(Scenario, Country.Origin, Country.Destination, Year, Effect) %>%
  summarize(Value = sum(Value))

## Export results
fwrite(results_effects, file = paste0(respath,"EU_effects.csv"), sep=";", dec=",")

## Round numbers and split by Country.Origin
results_effects$Value <- round(results_effects$Value, digits = 1)
results_effects <- split(results_effects, f = results_effects$Country.Origin)

## Make Scenario and Effect ordered factors (to give Stanford scenario and O&M first)
origin <- c("EU27+UK","RoW")
for (region in origin) {
  results_effects[[region]]$Scenario <- factor(results_effects[[region]]$Scenario, levels = rev(unique(results_effects[[region]]$Scenario)), ordered = TRUE)
  results_effects[[region]]$Effect <- factor(results_effects[[region]]$Effect, levels = c("O&M","GFCF","Subst"), ordered = TRUE)
}

## Plot results
plot_effects <- list()

for (region in origin) {
  Scenario.labs <- c("100% Renewables Scenario", "Reference Scenario")
  names(Scenario.labs) <- c("stanford", "euref")
  
  plot_effects[[region]] <- ggplot(results_effects[[region]]) +
    geom_bar(aes(x = as.factor(Year), y = Value, fill = as.factor(Effect), group = as.factor(Effect)),
             stat = "identity",
             position = "dodge") + ## ?position_dodge() - position of bars
    geom_text(if(region=="EU27+UK"){
      aes(x = as.factor(Year), y = Value + 450 * sign(Value), group = as.factor(Effect), label = round(Value, digits = 0))
      } else aes(x = as.factor(Year), y = Value + 550 * sign(Value), group = as.factor(Effect), label = round(Value, digits = 0)),
      position = position_dodge(width = 0.9),
      size = 5,
      angle = 90) +
    facet_grid(~Scenario,
               labeller = labeller(Scenario = Scenario.labs)) + ## apply change of facet labels
    scale_x_discrete(expand = c(0.1, 0.1)) +
    scale_y_continuous(expand = c(0.1, 0.1)) +
    xlab(NULL) +
    ylab("Numbers of persons (1000)") +
    ggtitle(if(region=="EU27+UK"){
      "Employment requirements within the EU27+UK by operation and maintenance, capital formation and substitution \nof other fuels"
    } else "Employment requirements outside the EU27+UK by operation and maintenance, capital formation and substitution \nof other fuels") +
    scale_fill_manual(values = c("#a1d99b","#31a354","#1f6617"), ## colours
                      name = "Effect", ## label name
                      labels = c("Operation and maintenance","Capital formation","Substitution of other fuels")) + ## label values
    theme(strip.text.x = element_text(size = 16, face = "bold"),
          axis.title.x = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(size = 14, face = "bold"),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.ticks = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 16),
          legend.position = "bottom",
          legend.direction = "horizontal",
          plot.title = element_text(size = 20, face = "bold"),
          plot.caption = element_text(size = 16)) +
    guides(fill = guide_legend(nrow = 1))
  }

## Export plots
for (region in origin) {
  png(paste0(respath,"plots/EU_effects.png"), width = 1200, height = 1200)
  print(grid.arrange(plot_effects[["EU27+UK"]], plot_effects[["RoW"]], nrow = 2))
  dev.off()
}

for (region in origin) {
  ggsave(filename = paste0(respath,"plots/EU_effects.pdf"),
         plot = grid.arrange(plot_effects[["EU27+UK"]], plot_effects[["RoW"]], nrow = 2),
         width = 5000, height = 5000, units = "px")
}

for (region in origin) {
  ggsave(filename = paste0(respath,"plots/EU_effects.eps"),
         plot = grid.arrange(plot_effects[["EU27+UK"]], plot_effects[["RoW"]], nrow = 2),
         width = 5000, height = 5000, units = "px",
         device = cairo_ps)
}


##########################################################################
# 4.2. Organise results by country/region of origin and sectors and plot

## Aggregate domestic (EU27+UK) effects
results_origin <- results
results_origin <- results_origin %>% 
  group_by(Scenario, Country.Destination, Year, Region.Code, Industry.Code, Industry.Name) %>%
  summarize(Value = sum(Value))

## Calculate effects by sectors for each scenario and region of origin
results_origin_sectors <- split(results_origin, f = results_origin$Scenario)
for (scenario in scenarios) {
  results_origin_sectors[[scenario]] <- split(results_origin_sectors[[scenario]], f = results_origin_sectors[[scenario]]$Region.Code)
}

## Cast ("unmelt") to match values from different years next to each other
origin_names <- c("Africa","Asia","Australia","EU","Latin America","Middle East","North America","Rest of Europe")

for (scenario in scenarios) {
  for (region in origin_names) {
    results_origin_sectors[[scenario]][[region]] <- dcast(results_origin_sectors[[scenario]][[region]], Scenario + Country.Destination + Region.Code + Industry.Code + Industry.Name~Year, value.var = "Value")
  }
}

## Unlist/rbind (twice since it is a nested list)
results_origin_sectors <- do.call("rbind", results_origin_sectors)
results_origin_sectors <- do.call("rbind", results_origin_sectors)

## Export results with the sectors
fwrite(results_origin_sectors, file = paste0(respath,"EU_origin_sectors.csv"), sep=";", dec=",")

## Cluster sectors into NACE rev.1 divisions
results_origin_sectors$Industry.Code <- as.character(results_origin_sectors$Industry.Code)
results_origin_sectors$Industry.Code[!startsWith(results_origin_sectors$Industry.Code, "i40.11")] <-
  substr(results_origin_sectors$Industry.Code[!startsWith(results_origin_sectors$Industry.Code, "i40.11")], 1, 3)
results_origin_sectors$Industry.Code[startsWith(results_origin_sectors$Industry.Code, "i40.11.l")] <-
  substr(results_origin_sectors$Industry.Code[startsWith(results_origin_sectors$Industry.Code, "i40.11.l")], 1, 3)

divisions <- data.frame(codes = c(unique(results_origin_sectors$Industry.Code)),
                        names = c("Agriculture, hunting and related service activities","Forestry, logging and related service activities",
                                  "Fishing, operation of fish hatcheries and fish farms; service activities incidental to fishing",
                                  "Mining of coal and lignite; extraction of peat",
                                  "Extraction of crude petroleum and natural gas; service activities incidental to oil and gas extraction, excluding surveying",
                                  "Mining of uranium and thorium ores","Mining of metal ores","Other mining and quarrying",
                                  "Manufacture of food products and beverages","Manufacture of tobacco products","Manufacture of textiles",
                                  "Manufacture of wearing apparel; dressing and dyeing of fur",
                                  "Tanning and dressing of leather; manufacture of luggage, handbags, saddlery, harnes and footwear",
                                  "Manufacture of wood and of products of wood and cork, except furniture; manufacture of articles of straw and plaiting materials",
                                  "Manufacture of pulp, paper and paper products","Publishing, printing and reproduction of recorded media",
                                  "Manufacture of coke, refined petroleum products and nuclear fuel","Manufacture of chemicals and chemical products",
                                  "Manufacture of rubber and plastic products","Manufacture of other non-metallic mineral products",
                                  "Manufacture of basic metals","Manufacture of fabricated metal products, except machinery and equipment",
                                  "Manufacture of machinery and equipment n.e.c.","Manufacture of office machinery and computers",
                                  "Manufacture of electrical machinery and apparatus n.e.c.",
                                  "Manufacture of radio, television and communication equipment and apparatus",
                                  "Manufacture of medical, precision and optical instruments, watches and clocks",
                                  "Manufacture of motor vehicles, trailers and semi-trailers","Manufacture of other transport equipment",
                                  "Manufacture of furniture; manufacturing n.e.c.","Recycling",
                                  "Production of electricity by coal",
                                  "Production of electricity by gas",
                                  "Production of electricity by nuclear",
                                  "Production of electricity by hydro",
                                  "Production of electricity by wind onshore",
                                  "Production of electricity by wind offshore",
                                  "Production of electricity by petroleum and other oil derivatives",
                                  "Production of electricity by biomass and waste",
                                  "Production of electricity by solar photovoltaic utility",
                                  "Production of electricity by solar photovoltaic residential",
                                  "Production of electricity by solar thermal",
                                  "Production of electricity by tide, wave, ocean",
                                  "Production of electricity by Geothermal",
                                  "Other electricity, gas, steam and hot water supply",
                                  "Collection, purification and distribution of water","Construction",
                                  "Sale, maintenance and repair of motor vehicles and motorcycles; retail sale of automotive fuel",
                                  "Wholesale trade and commission trade, except of motor vehicles and motorcycles",
                                  "Retail trade, except of motor vehicles and motorcycles; repair of personal and household goods",
                                  "Hotels and restaurants","Land transport; transport via pipelines","Water transport","Air transport",
                                  "Supporting and auxiliary transport activities; activities of travel agencies",
                                  "Post and telecommunications","Financial intermediation, except insurance and pension funding",
                                  "Insurance and pension funding, except compulsory social security","Activities auxiliary to financial intermediation",
                                  "Real estate activities","Renting of machinery and equipment without operator and of personal and household goods",
                                  "Computer and related activities","Research and development","Other business activities",
                                  "Public administration and defence; compulsory social security","Education","Health and social work",
                                  "Sewage and refuse disposal, sanitation and similar activities","Activities of membership organizations n.e.c.",
                                  "Recreational, cultural and sporting activities","Other service activities","Private households with employed persons",
                                  "Extra-territorial organizations and bodies")) ## "concordance" between NACE rev. 1 divisions names and codes

results_origin_sectors$Industry.Name <- divisions$names[match(results_origin_sectors$Industry.Code, divisions$codes)]
results_origin_sectors <- results_origin_sectors %>% 
  group_by(Scenario, Country.Destination, Region.Code, Industry.Code, Industry.Name) %>%
  summarize_at(vars("2015":"2050"), sum, na.rm = TRUE)

## Export results with the divisions
fwrite(results_origin_sectors, file = paste0(respath,"EU_origin_divisions.csv"), sep=";", dec=",")

## Remove effects by sectors in the original results_origin for the plots and export results only by regions
results_origin <- results_origin %>% 
  group_by(Scenario, Country.Destination, Year, Region.Code) %>%
  summarize(Value = sum(Value))
fwrite(results_origin, file = paste0(respath,"EU_origin.csv"), sep=";", dec=",")

## Switch rows to sort regions of origin, mutate to plot stacked bar chart with value labels and round numbers
regionorder <- c("Australia","Latin America","North America","Asia","Africa","Middle East","Rest of Europe","EU")
results_origin <- results_origin %>% 
  as_tibble() %>% 
  group_by(Scenario, Country.Destination, Year) %>% 
  slice(order(factor(Region.Code, levels = regionorder)))
results_origin <- results_origin %>%
  group_by(Scenario, Year) %>%
  mutate(lab_ypos = cumsum(Value) - 0.5 * Value)
results_origin$Value <- round(results_origin$Value, digits = 1)
results_origin$lab_ypos <- round(results_origin$lab_ypos, digits = 1)

## Make Scenario and Region.Code ordered factors
results_origin$Scenario <- factor(results_origin$Scenario, levels = rev(unique(results_origin$Scenario)), ordered = TRUE)
results_origin$Region.Code <- factor(results_origin$Region.Code, levels = unique(results_origin$Region.Code), ordered = TRUE)

## Plot results
plot_origin <- list()

Scenario.labs <- c("100% Renewables Scenario", "Reference Scenario")
names(Scenario.labs) <- c("stanford", "euref")

plot_origin <- ggplot(results_origin) +
  geom_bar(aes(x = as.factor(Year), y = Value, fill = Region.Code),
           stat = "identity",
           position = "stack",
           width = 0.5) + ## ?position_stack() - position of bars
  facet_grid(~Scenario,
             labeller = labeller(Scenario = Scenario.labs)) + ## apply change of facet labels
  scale_x_discrete(expand = c(0.1, 0.1)) +
  scale_y_continuous(expand = c(0.1, 0.1)) +
  xlab(NULL) +
  ylab("Numbers of persons (1000)") +
  ggtitle("Employment requirements of the EU27+UK electricity sector by world region") +
  scale_fill_discrete(name = "World region", ## label name
                      labels = c("Australia","Latin America","North America","Asia","Africa","Middle East","Rest of Europe","EU27+UK")) + ## label values
  theme(strip.text.x = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.position = "bottom",
        legend.direction = "horizontal",
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.caption = element_text(size = 16)) +
  guides(fill = guide_legend(nrow = 1))

## Export plot
png(paste0(respath,"plots/EU_origin.png"), width = 1200, height = 800)
print(plot_origin)
dev.off()

ggsave(filename = paste0(respath,"plots/EU_origin.pdf"),
       plot = plot_origin,
       width = 5000, height = 3500, units = "px")

ggsave(filename = paste0(respath,"plots/EU_origin.eps"),
       plot = plot_origin,
       width = 5000, height = 3500, units = "px",
       device = cairo_ps)


##########################################################################
# 4.3. Organise results by skill levels and gender and plot

## Aggregate domestic (EU27+UK) versus foreign (RoW) effects
results_distr <- results
results_distr$Country.Origin[results_distr$Country.Origin %!in% c("EU27+UK")] <- "RoW"
results_distr <- results_distr %>% 
  group_by(Scenario, Country.Origin, Country.Destination, Year, Indicator) %>%
  summarize(Value = sum(Value))

## Switch rows to sort the distributional employment categories (Indicator) and export results
neworder <- c("Employment_High-skilled male",
              "Employment_Medium-skilled male",
              "Employment_Low-skilled male",
              "Employment_High-skilled female",
              "Employment_Medium-skilled female",
              "Employment_Low-skilled female")

results_distr <- results_distr %>% 
  as_tibble() %>% 
  group_by(Scenario, Country.Origin, Year) %>% 
  slice(order(factor(Indicator, levels = neworder)))
fwrite(results_distr, file = paste0(respath,"EU_distr.csv"), sep=";", dec=",")

## Split by Country.Origin
results_distr <- split(results_distr, f = results_distr$Country.Origin)

## Mutate to plot stacked bar chart with value labels and round numbers
origin <- c("EU27+UK","RoW")
for (region in origin) {
  results_distr[[region]] <- results_distr[[region]] %>%
    group_by(Scenario, Year) %>%
    mutate(lab_ypos = cumsum(Value) - 0.5 * Value)
  results_distr[[region]]$Value <- round(results_distr[[region]]$Value, digits = 1)
  results_distr[[region]]$lab_ypos <- round(results_distr[[region]]$lab_ypos, digits = 1)
}

## Make Scenario and Indicator ordered factors
for (region in origin) {
  results_distr[[region]]$Scenario <- factor(results_distr[[region]]$Scenario, levels = rev(unique(results_distr[[region]]$Scenario)), ordered = TRUE)
  results_distr[[region]]$Indicator <- factor(results_distr[[region]]$Indicator, levels = rev(unique(results_distr[[region]]$Indicator)), ordered = TRUE)
}

## Plot results
plot_distr <- list()

for (region in origin) {
  Scenario.labs <- c("100% Renewables Scenario", "Reference Scenario")
  names(Scenario.labs) <- c("stanford", "euref")
  
  plot_distr[[region]] <- ggplot(results_distr[[region]]) +
    geom_bar(aes(x = as.factor(Year), y = Value, fill = as.factor(desc(Indicator))),
             stat = "identity",
             position = "stack",
             width = 0.5) + ## ?position_stack() - position of bars
    facet_grid(~Scenario,
               labeller = labeller(Scenario = Scenario.labs)) + ## apply change of facet labels
    scale_x_discrete(expand = c(0.1, 0.1)) +
    scale_y_continuous(breaks=seq(0,15000,2500)) +
    xlab(NULL) +
    ylab("Numbers of persons (1000)") +
    scale_fill_manual(values = c("#2675f7","#5393fd","#679ffa","#28981d","#2bb03e","#3bc56f"),
                      name = "Skill levels and gender",
                      labels = c("High-skilled male", "Medium-skilled male", "Low-skilled male", "High-skilled female", "Medium-skilled female", "Low-skilled female")) +
    ggtitle(if(region=="EU27+UK"){
      "Employment requirements within the EU27+UK by skill level and gender"
      } else "Employment requirements outside the EU27+UK by skill level and gender") +
    theme(strip.text.x = element_text(size = 16, face = "bold"),
          axis.title.x = element_text(size = 14, face = "bold"),
          axis.title.y = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 16, face = "bold"),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.ticks = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 15),
          legend.position = "bottom",
          legend.direction = "horizontal",
          plot.title = element_text(size = 20, face = "bold"),
          plot.caption = element_text(size = 16)) +
    guides(fill = guide_legend(nrow = 1))
  }

## Export plots
for (region in origin) {
  png(paste0(respath,"plots/EU_distr.png"), width = 1200, height = 1200)
  print(grid.arrange(plot_distr[["EU27+UK"]], plot_distr[["RoW"]], nrow = 2))
  dev.off()
}

for (region in origin) {
  ggsave(filename = paste0(respath,"plots/EU_distr.pdf"),
         plot = grid.arrange(plot_distr[["EU27+UK"]], plot_distr[["RoW"]], nrow = 2),
         width = 5000, height = 5000, units = "px")
}

for (region in origin) {
  ggsave(filename = paste0(respath,"plots/EU_distr.eps"),
         plot = grid.arrange(plot_distr[["EU27+UK"]], plot_distr[["RoW"]], nrow = 2),
         width = 5000, height = 5000, units = "px",
         device = cairo_ps)
}


##########################################################################
# 4.4. Organise results by sectors, skill levels and gender (domestic and spill-over; winners vs. losers and average) and plot

## Calculate highest absolute changes over the whole period by skill levels
## Aggregate domestic (EU27+UK) effects
results_sectors_domestic_abs <- results %>% 
  filter(Country.Origin %in% "EU27+UK") %>%
  group_by(Scenario, Country.Destination, Year, Industry.Code, Industry.Name, Indicator) %>%
  summarize(Value = sum(Value))

## Cluster sectors into NACE rev.1 divisions
results_sectors_domestic_abs$Industry.Code <- as.character(results_sectors_domestic_abs$Industry.Code)
results_sectors_domestic_abs$Industry.Code[!startsWith(results_sectors_domestic_abs$Industry.Code, "i40.11")] <-
  substr(results_sectors_domestic_abs$Industry.Code[!startsWith(results_sectors_domestic_abs$Industry.Code, "i40.11")], 1, 3)
results_sectors_domestic_abs$Industry.Code[startsWith(results_sectors_domestic_abs$Industry.Code, "i40.11.l")] <-
  substr(results_sectors_domestic_abs$Industry.Code[startsWith(results_sectors_domestic_abs$Industry.Code, "i40.11.l")], 1, 3)
results_sectors_domestic_abs$Industry.Name <- divisions$names[match(results_sectors_domestic_abs$Industry.Code, divisions$codes)]
results_sectors_domestic_abs <- results_sectors_domestic_abs %>% 
  group_by(Scenario, Country.Destination, Year, Industry.Code, Industry.Name, Indicator) %>%
  summarize(Value = sum(Value))

## Split by Scenario
results_sectors_domestic_abs <- split(results_sectors_domestic_abs, f = results_sectors_domestic_abs$Scenario)

## Cast ("unmelt") to match values from different years next to each other, calculate Change and absolute Change columns (year-2015)
for (scenario in scenarios) {
  results_sectors_domestic_abs[[scenario]] <- dcast(results_sectors_domestic_abs[[scenario]], Scenario + Country.Destination + Industry.Name + Industry.Code + Indicator~Year, value.var = "Value")
  for (year in years) {
    if (year>2015) {
      results_sectors_domestic_abs[[scenario]][,paste0("Change.",year)] <- results_sectors_domestic_abs[[scenario]][,year] - results_sectors_domestic_abs[[scenario]][,"2015"]
      results_sectors_domestic_abs[[scenario]][,paste0("Changeabs.",year)] <- abs(results_sectors_domestic_abs[[scenario]][,year] - results_sectors_domestic_abs[[scenario]][,"2015"])
    }
  }
}

## Delete Value columns
for (scenario in scenarios) {
  for (year in years) {
    results_sectors_domestic_abs[[scenario]][,year] <- NULL
  }
}

## Calculate absolute maximum value across years for each row
for (scenario in scenarios) {
  results_sectors_domestic_abs[[scenario]][,"Changeabs.max"] <- do.call(pmax, results_sectors_domestic_abs[[scenario]][,startsWith(colnames(results_sectors_domestic_abs[[scenario]]),"Changeabs.")])
}

## Reorder skill levels and gender (Indicator), sort by highest absolute change values in a descending order and change colnames
neworder <- c("Employment_High-skilled male",
              "Employment_Medium-skilled male",
              "Employment_Low-skilled male",
              "Employment_High-skilled female",
              "Employment_Medium-skilled female",
              "Employment_Low-skilled female")

for (scenario in scenarios) {
  results_sectors_domestic_abs[[scenario]] <- results_sectors_domestic_abs[[scenario]] %>% 
    as_tibble() %>% 
    slice(order(factor(Indicator, levels = neworder))) %>% 
    group_by(Industry.Name, Industry.Code) %>% 
    mutate(Change.sum = sum(Changeabs.max)) %>% 
    arrange(desc(Change.sum)) %>% 
    ungroup()
}

## Delete Changeabs columns and change colnames
for (scenario in scenarios) {
  for (year in years) {
    results_sectors_domestic_abs[[scenario]][,startsWith(colnames(results_sectors_domestic_abs[[scenario]]),"Changeabs.")] <- NULL
  }
  colnames(results_sectors_domestic_abs[[scenario]])[6:12] <- c("2020","2025","2030","2035","2040","2045","2050")
  results_sectors_domestic_abs[[scenario]] <- results_sectors_domestic_abs[[scenario]][,1:12]
}

## Melt and adjust colnames
for (scenario in scenarios) {
  results_sectors_domestic_abs[[scenario]] <- melt(results_sectors_domestic_abs[[scenario]])
  colnames(results_sectors_domestic_abs[[scenario]])[6] <- "Year"
  colnames(results_sectors_domestic_abs[[scenario]])[7] <- "Change"
}

## Export results
for (scenario in scenarios) {
  fwrite(results_sectors_domestic_abs[[scenario]], file = paste0(respath,"EU_sectors_domestic_abs_",scenario,".csv"), sep=";", dec=",")
}

## Round numbers
for (scenario in scenarios) {
  results_sectors_domestic_abs[[scenario]]$Change <- round(results_sectors_domestic_abs[[scenario]]$Change, digits = 1)
}

## Leave only 12 sectors with the largest absolute change
for (scenario in scenarios) {
  results_sectors_domestic_abs[[scenario]] <- results_sectors_domestic_abs[[scenario]][c(2700:2629,2262:2191,1824:1753,1386:1315,948:877,510:439,72:1),]
  
  ## Make Industry.Name and Indicator ordered factors
  results_sectors_domestic_abs[[scenario]]$Industry.Name <- factor(results_sectors_domestic_abs[[scenario]]$Industry.Name, levels = rev(unique(results_sectors_domestic_abs[[scenario]]$Industry.Name)), ordered = TRUE)
  results_sectors_domestic_abs[[scenario]]$Indicator <- factor(results_sectors_domestic_abs[[scenario]]$Indicator, levels = rev(unique(results_sectors_domestic_abs[[scenario]]$Indicator)), ordered = TRUE)
}

## Plot results for 12 sectors with the largest absolute change, divided by skill levels and gender
plot_sectors_domestic_abs_skills <- list()

for (scenario in scenarios) {
  plot_sectors_domestic_abs_skills[[scenario]] <- ggplot(results_sectors_domestic_abs[[scenario]], aes(x = Year, y = Change, fill = as.factor(Indicator))) +
    geom_bar(position = "stack", stat = "identity", width = 0.95) +
    facet_wrap( ~ Industry.Name, nrow = 1, labeller = labeller(Industry.Name = label_wrap_gen(20))) +
    scale_x_discrete(labels = c("2020","","2030","","2040","","2050"), expand = c(0.15, 0.15)) +
    scale_y_continuous(expand = c(0.15, 0.15)) +
    xlab(NULL) +
    ylab("Numbers of persons (1000)") +
    scale_fill_manual(values = c("#2675f7","#5393fd","#679ffa","#28981d","#2bb03e","#3bc56f"),
                      name = "Skill levels and gender",
                      labels = c("High-skilled male", "Medium-skilled male", "Low-skilled male", "High-skilled female", "Medium-skilled female", "Low-skilled female")) +
    ggtitle(if(scenario=="euref"){"Change in employment requirements within the EU27+UK by sector, skill level and gender: Reference Scenario"} else "Change in employment requirements within the EU27+UK by sector, skill level and gender: 100% Renewables Scenario",
            "12 sectors with the largest absolute changes between 2015 and 2050") +
    theme(panel.spacing = grid::unit(0, "lines"),
          strip.text.x = element_text(size = 12),
          axis.title.x = element_text(face = "bold"),
          axis.title = element_text(size = 16, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 14),
          axis.text.y = element_text(size = 14),
          axis.ticks = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 16),
          legend.position = "bottom",
          legend.direction = "horizontal",
          plot.title = element_text(size = 20, face = "bold"),
          plot.subtitle = element_text(size = 18),
          plot.caption = element_text(size = 16)) +
    guides(fill = guide_legend(ncol = 7))
  }

## Export plots
for (scenario in scenarios) {
  png(paste0(respath,"plots/EU_sectors_domestic_abs_skills.png"), width = 1300, height = 1300)
  print(grid.arrange(plot_sectors_domestic_abs_skills[["stanford"]], plot_sectors_domestic_abs_skills[["euref"]], nrow = 2))
  dev.off()
}

for (scenario in scenarios) {
  ggsave(filename = paste0(respath,"plots/EU_sectors_domestic_abs_skills.pdf"),
         plot = grid.arrange(plot_sectors_domestic_abs_skills[["stanford"]], plot_sectors_domestic_abs_skills[["euref"]], nrow = 2),
         width = 5500, height = 5500, units = "px")
}

for (scenario in scenarios) {
  ggsave(filename = paste0(respath,"plots/EU_sectors_domestic_abs_skills.eps"),
         plot = grid.arrange(plot_sectors_domestic_abs_skills[["stanford"]], plot_sectors_domestic_abs_skills[["euref"]], nrow = 2),
         width = 5500, height = 5500, units = "px",
         device = cairo_ps)
}


## Aggregate spill-over (RoW) effects
results_sectors_spillover_abs <- results %>% 
  filter(Country.Origin %!in% "EU27+UK") %>%
  group_by(Scenario, Country.Destination, Year, Industry.Code, Industry.Name, Indicator) %>%
  summarize(Value = sum(Value))

## Cluster sectors into NACE rev.1 divisions
results_sectors_spillover_abs$Industry.Code <- as.character(results_sectors_spillover_abs$Industry.Code)
results_sectors_spillover_abs$Industry.Code[!startsWith(results_sectors_spillover_abs$Industry.Code, "i40.11")] <-
  substr(results_sectors_spillover_abs$Industry.Code[!startsWith(results_sectors_spillover_abs$Industry.Code, "i40.11")], 1, 3)
results_sectors_spillover_abs$Industry.Code[startsWith(results_sectors_spillover_abs$Industry.Code, "i40.11.l")] <-
  substr(results_sectors_spillover_abs$Industry.Code[startsWith(results_sectors_spillover_abs$Industry.Code, "i40.11.l")], 1, 3)
results_sectors_spillover_abs$Industry.Name <- divisions$names[match(results_sectors_spillover_abs$Industry.Code, divisions$codes)]
results_sectors_spillover_abs <- results_sectors_spillover_abs %>% 
  group_by(Scenario, Country.Destination, Year, Industry.Code, Industry.Name, Indicator) %>%
  summarize(Value = sum(Value))

## Split by Scenario
results_sectors_spillover_abs <- split(results_sectors_spillover_abs, f = results_sectors_spillover_abs$Scenario)

## Cast ("unmelt") to match values from different years next to each other, calculate Change and absolute Change columns (year-2015)
for (scenario in scenarios) {
  results_sectors_spillover_abs[[scenario]] <- dcast(results_sectors_spillover_abs[[scenario]], Scenario + Country.Destination + Industry.Name + Industry.Code + Indicator~Year, value.var = "Value")
  for (year in years) {
    if (year>2015) {
      results_sectors_spillover_abs[[scenario]][,paste0("Change.",year)] <- results_sectors_spillover_abs[[scenario]][,year] - results_sectors_spillover_abs[[scenario]][,"2015"]
      results_sectors_spillover_abs[[scenario]][,paste0("Changeabs.",year)] <- abs(results_sectors_spillover_abs[[scenario]][,year] - results_sectors_spillover_abs[[scenario]][,"2015"])
    }
  }
}

## Delete Value columns
for (scenario in scenarios) {
  for (year in years) {
    results_sectors_spillover_abs[[scenario]][,year] <- NULL
  }
}

## Calculate absolute maximum value across years for each row
for (scenario in scenarios) {
  results_sectors_spillover_abs[[scenario]][,"Changeabs.max"] <- do.call(pmax, results_sectors_spillover_abs[[scenario]][,startsWith(colnames(results_sectors_spillover_abs[[scenario]]),"Changeabs.")])
}

## Reorder skill levels and gender (Indicator), sort by highest absolute change values in a descending order and change colnames
neworder <- c("Employment_High-skilled male",
              "Employment_Medium-skilled male",
              "Employment_Low-skilled male",
              "Employment_High-skilled female",
              "Employment_Medium-skilled female",
              "Employment_Low-skilled female")

for (scenario in scenarios) {
  results_sectors_spillover_abs[[scenario]] <- results_sectors_spillover_abs[[scenario]] %>% 
    as_tibble() %>% 
    slice(order(factor(Indicator, levels = neworder))) %>% 
    group_by(Industry.Name, Industry.Code) %>% 
    mutate(Change.sum = sum(Changeabs.max)) %>% 
    arrange(desc(Change.sum)) %>% 
    ungroup()
}

## Delete Changeabs columns and change colnames
for (scenario in scenarios) {
  for (year in years) {
    results_sectors_spillover_abs[[scenario]][,startsWith(colnames(results_sectors_spillover_abs[[scenario]]),"Changeabs.")] <- NULL
  }
  colnames(results_sectors_spillover_abs[[scenario]])[6:12] <- c("2020","2025","2030","2035","2040","2045","2050")
  results_sectors_spillover_abs[[scenario]] <- results_sectors_spillover_abs[[scenario]][,1:12]
}

## Melt and adjust colnames
for (scenario in scenarios) {
  results_sectors_spillover_abs[[scenario]] <- melt(results_sectors_spillover_abs[[scenario]])
  colnames(results_sectors_spillover_abs[[scenario]])[6] <- "Year"
  colnames(results_sectors_spillover_abs[[scenario]])[7] <- "Change"
}

## Export results
for (scenario in scenarios) {
  fwrite(results_sectors_spillover_abs[[scenario]], file = paste0(respath,"EU_sectors_spillover_abs_",scenario,".csv"), sep=";", dec=",")
}

## Round numbers
for (scenario in scenarios) {
  results_sectors_spillover_abs[[scenario]]$Change <- round(results_sectors_spillover_abs[[scenario]]$Change, digits = 1)
}

## Leave only 12 sectors with the largest absolute change
for (scenario in scenarios) {
  results_sectors_spillover_abs[[scenario]] <- results_sectors_spillover_abs[[scenario]][c(2700:2629,2262:2191,1824:1753,1386:1315,948:877,510:439,72:1),]
  
  ## Make Industry.Name and Indicator ordered factors
  results_sectors_spillover_abs[[scenario]]$Industry.Name <- factor(results_sectors_spillover_abs[[scenario]]$Industry.Name, levels = rev(unique(results_sectors_spillover_abs[[scenario]]$Industry.Name)), ordered = TRUE)
  results_sectors_spillover_abs[[scenario]]$Indicator <- factor(results_sectors_spillover_abs[[scenario]]$Indicator, levels = rev(unique(results_sectors_spillover_abs[[scenario]]$Indicator)), ordered = TRUE)
}

## Plot results for 12 sectors with the largest absolute change, divided by skill levels and gender
plot_sectors_spillover_abs_skills <- list()

for (scenario in scenarios) {
  plot_sectors_spillover_abs_skills[[scenario]] <- ggplot(results_sectors_spillover_abs[[scenario]], aes(x = Year, y = Change, fill = as.factor(Indicator))) +
    geom_bar(position = "stack", stat = "identity", width = 0.95) +
    facet_wrap( ~ Industry.Name, nrow = 1, labeller = labeller(Industry.Name = label_wrap_gen(20))) +
    scale_x_discrete(labels = c("2020","","2030","","2040","","2050"), expand = c(0.15, 0.15)) +
    scale_y_continuous(expand = c(0.15, 0.15)) +
    xlab(NULL) +
    ylab("Numbers of persons (1000)") +
    scale_fill_manual(values = c("#2675f7","#5393fd","#679ffa","#28981d","#2bb03e","#3bc56f"),
                      name = "Skill levels and gender",
                      labels = c("High-skilled male", "Medium-skilled male", "Low-skilled male", "High-skilled female", "Medium-skilled female", "Low-skilled female")) +
    ggtitle(if(scenario=="euref"){"Change in employment requirements outside the EU27+UK by sector, skill level and gender: Reference Scenario"} else "Change in employment requirements outside the EU27+UK by sector, skill level and gender: 100% Renewables Scenario",
            "12 sectors with the largest absolute changes between 2015 and 2050") +
    theme(panel.spacing = grid::unit(0, "lines"),
          strip.text.x = element_text(size = 12),
          axis.title.x = element_text(face = "bold"),
          axis.title = element_text(size = 16, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 14),
          axis.text.y = element_text(size = 14),
          axis.ticks = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 16),
          legend.position = "bottom",
          legend.direction = "horizontal",
          plot.title = element_text(size = 20, face = "bold"),
          plot.subtitle = element_text(size = 18),
          plot.caption = element_text(size = 16)) +
    guides(fill = guide_legend(ncol = 7))
  }

## Export plots
for (scenario in scenarios) {
  png(paste0(respath,"plots/EU_sectors_spillover_abs_skills.png"), width = 1300, height = 1300)
  print(grid.arrange(plot_sectors_spillover_abs_skills[["stanford"]], plot_sectors_spillover_abs_skills[["euref"]], nrow = 2))
  dev.off()
}

for (scenario in scenarios) {
  ggsave(filename = paste0(respath,"plots/EU_sectors_spillover_abs_skills.pdf"),
         plot = grid.arrange(plot_sectors_spillover_abs_skills[["stanford"]], plot_sectors_spillover_abs_skills[["euref"]], nrow = 2),
         width = 5500, height = 5500, units = "px")
}

for (scenario in scenarios) {
  ggsave(filename = paste0(respath,"plots/EU_sectors_spillover_abs_skills.eps"),
         plot = grid.arrange(plot_sectors_spillover_abs_skills[["stanford"]], plot_sectors_spillover_abs_skills[["euref"]], nrow = 2),
         width = 5500, height = 5500, units = "px",
         device = cairo_ps)
}


##########################################################################
# 4.5. Organise results by electricity sources (domestic vs. foreign) and plot

## Aggregate domestic (EU27+UK) versus foreign (RoW) effects
results_sources <- results
results_sources$Country.Origin[results_sources$Country.Origin %!in% c("EU27+UK")] <- "RoW"
results_sources <- results_sources %>% 
  group_by(Scenario, Country.Origin, Country.Destination, Year, Electricity) %>%
  summarize(Value = sum(Value))

## Delete the substitution effects - we cannot really allocate the job losses associated with each technology, they produce all the same electricity
results_sources <- results_sources %>% filter(Electricity %in% c(paste0("i40.11.",electricity_det)))

## Export results
fwrite(results_sources, file = paste0(respath,"EU_sources.csv"), sep=";", dec=",")

## Split by Country.Origin
origin <- c("EU27+UK","RoW")
results_sources <- split(results_sources, f = results_sources$Country.Origin)

## Sort by Scenario, Year and Electricity and round numbers
for (region in origin) {
  results_sources[[region]] <- results_sources[[region]][order(results_sources[[region]]$Scenario,results_sources[[region]]$Year,results_sources[[region]]$Electricity),]
  results_sources[[region]]$Value <- round(results_sources[[region]]$Value, digits = 1)
}

## Reorder energy sources (Electricity) to group them visually in the plot
neworder <- c("i40.11.a","i40.11.b","i40.11.f","i40.11.c","i40.11.g","i40.11.d","i40.11.e.1","i40.11.e.2","i40.11.h.1","i40.11.h.2","i40.11.i","i40.11.j","i40.11.k")
for (region in origin) {
  results_sources[[region]] <- results_sources[[region]] %>% slice(order(factor(Electricity, levels = neworder)))
}

## Make Scenario and Electricity ordered factors
for (region in origin) {
  results_sources[[region]]$Scenario <- factor(results_sources[[region]]$Scenario, levels = rev(unique(results_sources[[region]]$Scenario)), ordered = TRUE)
  results_sources[[region]]$Electricity <- factor(results_sources[[region]]$Electricity, levels = rev(unique(results_sources[[region]]$Electricity)), ordered = TRUE)
}

## Plot results
plot_sources <- list()

for (region in origin) {
  Scenario.labs <- c("100% Renewables Scenario", "Reference Scenario")
  names(Scenario.labs) <- c("stanford", "euref")
  
  plot_sources[[region]] <- ggplot(results_sources[[region]]) +
    geom_bar(aes(x = as.factor(Year), y = Value, fill = Electricity),
             stat = "identity",
             position = "stack",
             width = 0.5) + ## ?position_stack() - position of bars
    facet_grid(~Scenario,
               labeller = labeller(Scenario = Scenario.labs)) + ## apply change of facet labels
    scale_x_discrete(expand = c(0.1, 0.1)) +
    xlab(NULL) +
    ylab("Numbers of persons (1000)") +
    scale_fill_tableau(palette = "Classic Cyclic",
                       direction = -1,
                       name = "Electricity source",
                       labels = c("Geothermal","Tide, wave, ocean","Solar thermal","Solar PV residential","Solar PV utility",
                                  "Wind offshore","Wind onshore","Hydro","Biomass and waste",
                                  "Nuclear","Petroleum and other oil derivatives","Gas","Coal")) +
    ggtitle(if(region=="EU27+UK"){
      "Employment requirements within the EU27+UK by electricity source"
      } else "Employment requirements outside the EU27+UK by electricity source") +
    theme(strip.text.x = element_text(size = 16, face = "bold"),
          axis.title.x = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 16, face = "bold"),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.ticks = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 14),
          plot.title = element_text(size = 20, face = "bold"),
          plot.caption = element_text(size = 16))
  }

## Export plots
png(paste0(respath,"plots/EU_sources.png"), width = 1200, height = 1200)
print(grid.arrange(plot_sources[["EU27+UK"]], plot_sources[["RoW"]], nrow = 2))
dev.off()

ggsave(filename = paste0(respath,"plots/EU_sources.pdf"),
       plot = grid.arrange(plot_sources[["EU27+UK"]], plot_sources[["RoW"]], nrow = 2),
       width = 5000, height = 5000, units = "px")

ggsave(filename = paste0(respath,"plots/EU_sources.eps"),
       plot = grid.arrange(plot_sources[["EU27+UK"]], plot_sources[["RoW"]], nrow = 2),
       width = 5000, height = 5000, units = "px",
       device = cairo_ps)


##########################################################################
# 4.6. Organise results by countries of origin (domestic) and plot

## Select only EU27+UK countries, match extension categories together by Country.Origin and split by Scenario
results_countries_orig <- results_countries %>% filter(Country.Origin %in% c(countries_names))
results_countries_orig <- results_countries_orig %>% 
  group_by(Scenario, Year, Country.Origin) %>%
  summarize(Value = sum(Value))
results_countries_orig <- split(results_countries_orig, f = results_countries_orig$Scenario)

## Cast ("unmelt") to match values from different years next to each other, calculate Change columns (percentage change compared to 2015)
for (scenario in scenarios) {
  results_countries_orig[[scenario]] <- dcast(results_countries_orig[[scenario]], Scenario + Country.Origin~Year, value.var = "Value")
  for (year in years) {
    if (year>2015) {
      ## Make sure if I really want -1, because then I don't get the percentage change compared to the 2015 value
      results_countries_orig[[scenario]][,paste0("Change.",year)] <-
        results_countries_orig[[scenario]][,year] / results_countries_orig[[scenario]][,"2015"] - 1 ## -1 to get the percentage share compared to 2015 (2015 = 100%)
    }
  }
}

## Separate Value columns to a new file, sort in alphabetical order, melt and export
results_countries_orig_val <- list()
for (scenario in scenarios) {
  results_countries_orig_val[[scenario]] <- results_countries_orig[[scenario]][,1:10]
  results_countries_orig_val[[scenario]] <- melt(results_countries_orig_val[[scenario]])
  colnames(results_countries_orig_val[[scenario]])[3] <- "Year"
  colnames(results_countries_orig_val[[scenario]])[4] <- "Value"
  results_countries_orig_val[[scenario]] <- results_countries_orig_val[[scenario]][order(results_countries_orig_val[[scenario]]$Year, results_countries_orig_val[[scenario]]$Country.Origin),]
  fwrite(results_countries_orig_val[[scenario]], file = paste0(respath,"EU_destination_val_",scenario,".csv"), sep=";", dec=",")
}

## Recalculate to get values for employment in the electricity sectors per employment (labour force) of each country
emp <- as.matrix(Edet[Q.codes$Stressor=="Employment: Low-skilled male",] + Edet[Q.codes$Stressor=="Employment: Low-skilled female",] + Edet[Q.codes$Stressor=="Employment: Medium-skilled male",] + Edet[Q.codes$Stressor=="Employment: Medium-skilled female",] + Edet[Q.codes$Stressor=="Employment: High-skilled male",] + Edet[Q.codes$Stressor=="Employment: High-skilled female",])
rownames(emp) <- substr(rownames(emp), 1, 2)
emp <- t(agg(t(emp)))
emp <- as.matrix(emp[rownames(emp) %in% countries])
rownames(emp) <- c(countries_names)
emp <- as.matrix(emp[order(rownames(emp)),])

for (scenario in scenarios) {
  results_countries_orig_val[[scenario]]$Value <- (results_countries_orig_val[[scenario]]$Value / rep.int(emp, times = 8))
}

## Cast ("unmelt") to match values from different years next to each other
for (scenario in scenarios) {
  results_countries_orig_val[[scenario]] <- dcast(results_countries_orig_val[[scenario]], Scenario + Country.Origin~Year, value.var = "Value")
}

## Sort by 2050 values in a descending order
for (scenario in scenarios) {
  results_countries_orig_val[[scenario]] <- results_countries_orig_val[[scenario]] %>% 
    as_tibble() %>% 
    arrange(desc(results_countries_orig_val[[scenario]][,"2050"]))
}

## Melt and adjust colnames
for (scenario in scenarios) {
  results_countries_orig_val[[scenario]] <- melt(results_countries_orig_val[[scenario]])
  colnames(results_countries_orig_val[[scenario]])[3] <- "Year"
  colnames(results_countries_orig_val[[scenario]])[4] <- "Value"
}

## Sort alphabetically (for exporting) and export results
results_countries_orig_val_alph <- list()

for (scenario in scenarios) {
  results_countries_orig_val_alph[[scenario]] <- results_countries_orig_val[[scenario]][order(results_countries_orig_val[[scenario]]$Year, results_countries_orig_val[[scenario]]$Country.Origin),]
  fwrite(results_countries_orig_val_alph[[scenario]], file = paste0(respath,"EU_countries_val_",scenario,".csv"), sep=";", dec=",")
}

## Make Scenario, Year and Country.Origin ordered factors
for (scenario in scenarios) {
  results_countries_orig_val[[scenario]]$Year <- factor(results_countries_orig_val[[scenario]]$Year, levels = rev(unique(results_countries_orig_val[[scenario]]$Year)), ordered = TRUE)
  results_countries_orig_val[[scenario]]$Scenario <- factor(results_countries_orig_val[[scenario]]$Scenario, levels = rev(unique(results_countries_orig_val[[scenario]]$Scenario)), ordered = TRUE)
  results_countries_orig_val[[scenario]]$Country.Origin <- factor(results_countries_orig_val[[scenario]]$Country.Origin, levels = rev(unique(results_countries_orig_val[[scenario]]$Country.Origin)), ordered = TRUE)
}

## Plot results
plot_countries_orig_val <- list()

for (scenario in scenarios) {
  plot_countries_orig_val[[scenario]] <- ggplot(results_countries_orig_val[[scenario]], aes(x = Country.Origin, y = Value, fill = as.factor(desc(Year)))) +
    geom_bar(stat = "identity",
             position = position_dodge(width = 0.9)) + ## ?position_dodge() - position of bars
    scale_x_discrete(limits = results_countries_orig_val[[scenario]]$Country.Origin[1:28], ## avoid automatically (alphabetically) sorting the x-axis
                     labels = function(x) str_wrap(x, width = 15),
                     expand = c(0.05, 0.05)) +
    xlab(NULL) +
    ylab("Number of persons per total employment") +
    ggtitle(if(scenario=="euref"){"Employment requirements of the EU27+UK electricity sector by country: Reference Scenario"} else "Employment requirements of the EU27+UK electricity sector by country: 100% Renewables Scenario") +
    scale_fill_manual(values = c(rep_len(c("#186872","#1E877C","#259B76","#2DAF6A","#3AB863","#48C15E","#56C95B","#6FD164"), length(unique(results_countries_orig_pop[[scenario]]$Country.Origin))-1)), ## colours
                      labels = c("2015","2020","2025","2030","2035","2040","2045","2050")) +
    theme(axis.title.x = element_text(face = "bold"),
          axis.title = element_text(size = 14, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 16),
          axis.text.y = element_text(size = 14),
          axis.ticks = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 16),
          legend.position = "bottom",
          legend.direction = "horizontal",
          plot.title = element_text(size = 20, face = "bold"),
          plot.caption = element_text(size = 16)) +
    guides(fill = guide_legend(ncol = 8))
  }

## Export plots
for (scenario in scenarios) {
  png(paste0(respath,"plots/EU_countries_orig_val.png"), width = 1200, height = 1200)
  print(grid.arrange(plot_countries_orig_val[["stanford"]], plot_countries_orig_val[["euref"]], nrow = 2))
  dev.off()
}

for (scenario in scenarios) {
  ggsave(filename = paste0(respath,"plots/EU_countries_orig_val.pdf"),
         plot = grid.arrange(plot_countries_orig_val[["stanford"]], plot_countries_orig_val[["euref"]], nrow = 2),
         width = 5000, height = 5000, units = "px")
}

for (scenario in scenarios) {
  ggsave(filename = paste0(respath,"plots/EU_countries_orig_val.eps"),
         plot = grid.arrange(plot_countries_orig_val[["stanford"]], plot_countries_orig_val[["euref"]], nrow = 2),
         width = 5000, height = 5000, units = "px",
         device = cairo_ps)
}



##########################################################################
# 5. Load results for each country and organise data
##########################################################################

##########################################################################
# 5.1. Organise results by effects (O&M vs. GFCF, domestic) and plot

## Match results together by Country.Origin (the calculation above gives them separate) and filter only EU27+UK results in Country.Origin
results_effects_cntr <- results_countries
results_effects_cntr <- results_effects_cntr %>% 
  filter(Country.Origin %in% countries_names) %>%
  group_by(Scenario, Country.Origin, Year, Effect) %>%
  summarize(Value = sum(Value))

## Export results
fwrite(results_effects_cntr, file = paste0(respath,"countries_effects.csv"), sep=";", dec=",")

## Round numbers
results_effects_cntr$Value <- round(results_effects_cntr$Value, digits = 1)

## Make Scenario and Effect ordered factors (to give Stanford scenario and O&M first) and split by Country.Origin
results_effects_cntr$Scenario <- factor(results_effects_cntr$Scenario, levels = rev(unique(results_effects_cntr$Scenario)), ordered = TRUE)
results_effects_cntr$Effect <- factor(results_effects_cntr$Effect, levels = c("O&M","GFCF","Subst"), ordered = TRUE)
results_effects_cntr <- split(results_effects_cntr, f = results_effects_cntr$Country.Origin)

## Plot results
countries_names_1 <- c("Austria","Belgium","Bulgaria","Cyprus")
countries_names_2 <- c("Czechia","Germany","Denmark","Estonia")
countries_names_3 <- c("Croatia","Hungary","Spain","Finland")
countries_names_4 <- c("France","Greece","Ireland","Italy")
countries_names_5 <- c("Lithuania","Luxembourg","Latvia","Malta")
countries_names_6 <- c("Netherlands","Poland","Portugal","Romania")
countries_names_7 <- c("Sweden","Slovenia","Slovakia","United Kingdom")

## 1
plot_effects_cntr_1 <- list()

for (country in countries_names_1) {
  Scenario.labs <- c("100% Renewables Scenario", "Reference Scenario")
  names(Scenario.labs) <- c("stanford", "euref")
  
  plot_effects_cntr_1[[country]] <- ggplot(results_effects_cntr[[country]]) +
    geom_bar(aes(x = as.factor(Year), y = Value, fill = as.factor(Effect), group = as.factor(Effect)),
             stat = "identity",
             position = "dodge") + ## ?position_dodge() - position of bars
    facet_grid(~Scenario,
               labeller = labeller(Scenario = Scenario.labs)) + ## apply change of facet labels
    scale_x_discrete(expand = c(0.1, 0.1)) +
    scale_y_continuous(expand = c(0.1, 0.1)) +
    xlab(NULL) +
    ylab("Number of persons (1000)") +
    ggtitle(country,
            "Employment requirements by operation and maintenance, capital formation and\nsubstitution of other fuels") +
    scale_fill_manual(values = c("#a1d99b","#31a354","#1f6617"), ## colours
                      name = "Effect", ## label name
                      labels = c("Operation and maintenance", "Capital formation", "Substitution of other fuels")) + ## label values
    theme(strip.text.x = element_text(size = 16, face = "bold"),
          axis.title.x = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(size = 14, face = "bold"),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.ticks = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 16),
          legend.position = "bottom",
          legend.direction = "horizontal",
          plot.title = element_text(size = 20, face = "bold"),
          plot.subtitle = element_text(size = 15),
          plot.caption = element_text(size = 16)) +
    guides(fill = guide_legend(nrow = 1))
}

## Export plots
png(paste0(respath,"plots/countries_effects_1.png"), width = 1600, height = 1200)
do.call(grid.arrange, plot_effects_cntr_1)
dev.off()

for (country in countries_names_1) {
  ggsave(filename = paste0(respath,"plots/countries_effects_1.pdf"),
       plot = grid.arrange(plot_effects_cntr_1[["Austria"]],
                           plot_effects_cntr_1[["Belgium"]],
                           plot_effects_cntr_1[["Bulgaria"]],
                           plot_effects_cntr_1[["Cyprus"]],  nrow = 2, ncol = 2),
       width = 7500, height = 5000, units = "px")
}

for (country in countries_names_1) {
  ggsave(filename = paste0(respath,"plots/countries_effects_1.eps"),
       plot = grid.arrange(plot_effects_cntr_1[["Austria"]],
                           plot_effects_cntr_1[["Belgium"]],
                           plot_effects_cntr_1[["Bulgaria"]],
                           plot_effects_cntr_1[["Cyprus"]],  nrow = 2, ncol = 2),
       width = 7500, height = 5000, units = "px",
       device = cairo_ps)
}


## 2
plot_effects_cntr_2 <- list()

for (country in countries_names_2) {
  Scenario.labs <- c("100% Renewables Scenario", "Reference Scenario")
  names(Scenario.labs) <- c("stanford", "euref")
  
  plot_effects_cntr_2[[country]] <- ggplot(results_effects_cntr[[country]]) +
    geom_bar(aes(x = as.factor(Year), y = Value, fill = as.factor(Effect), group = as.factor(Effect)),
             stat = "identity",
             position = "dodge") + ## ?position_dodge() - position of bars
    facet_grid(~Scenario,
               labeller = labeller(Scenario = Scenario.labs)) + ## apply change of facet labels
    scale_x_discrete(expand = c(0.1, 0.1)) +
    scale_y_continuous(expand = c(0.1, 0.1)) +
    xlab(NULL) +
    ylab("Number of persons (1000)") +
    ggtitle(country,
            "Employment requirements by operation and maintenance, capital formation and\nsubstitution of other fuels") +
    scale_fill_manual(values = c("#a1d99b","#31a354","#1f6617"), ## colours
                      name = "Effect", ## label name
                      labels = c("Operation and maintenance", "Capital formation", "Substitution of other fuels")) + ## label values
    theme(strip.text.x = element_text(size = 16, face = "bold"),
          axis.title.x = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(size = 14, face = "bold"),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.ticks = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 16),
          legend.position = "bottom",
          legend.direction = "horizontal",
          plot.title = element_text(size = 20, face = "bold"),
          plot.subtitle = element_text(size = 15),
          plot.caption = element_text(size = 16)) +
    guides(fill = guide_legend(nrow = 1))
}

## Export plots
png(paste0(respath,"plots/countries_effects_2.png"), width = 1600, height = 1200)
do.call(grid.arrange, plot_effects_cntr_2)
dev.off()

for (country in countries_names_2) {
  ggsave(filename = paste0(respath,"plots/countries_effects_2.pdf"),
         plot = grid.arrange(plot_effects_cntr_2[["Czechia"]],
                             plot_effects_cntr_2[["Germany"]],
                             plot_effects_cntr_2[["Denmark"]],
                             plot_effects_cntr_2[["Estonia"]],  nrow = 2, ncol = 2),
         width = 7500, height = 5000, units = "px")
}

for (country in countries_names_2) {
  ggsave(filename = paste0(respath,"plots/countries_effects_2.eps"),
         plot = grid.arrange(plot_effects_cntr_2[["Czechia"]],
                             plot_effects_cntr_2[["Germany"]],
                             plot_effects_cntr_2[["Denmark"]],
                             plot_effects_cntr_2[["Estonia"]],  nrow = 2, ncol = 2),
         width = 7500, height = 5000, units = "px",
         device = cairo_ps)
}


## 3
plot_effects_cntr_3 <- list()

for (country in countries_names_3) {
  Scenario.labs <- c("100% Renewables Scenario", "Reference Scenario")
  names(Scenario.labs) <- c("stanford", "euref")
  
  plot_effects_cntr_3[[country]] <- ggplot(results_effects_cntr[[country]]) +
    geom_bar(aes(x = as.factor(Year), y = Value, fill = as.factor(Effect), group = as.factor(Effect)),
             stat = "identity",
             position = "dodge") + ## ?position_dodge() - position of bars
    facet_grid(~Scenario,
               labeller = labeller(Scenario = Scenario.labs)) + ## apply change of facet labels
    scale_x_discrete(expand = c(0.1, 0.1)) +
    scale_y_continuous(expand = c(0.1, 0.1)) +
    xlab(NULL) +
    ylab("Number of persons (1000)") +
    ggtitle(country,
            "Employment requirements by operation and maintenance, capital formation and\nsubstitution of other fuels") +
    scale_fill_manual(values = c("#a1d99b","#31a354","#1f6617"), ## colours
                      name = "Effect", ## label name
                      labels = c("Operation and maintenance", "Capital formation", "Substitution of other fuels")) + ## label values
    theme(strip.text.x = element_text(size = 16, face = "bold"),
          axis.title.x = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(size = 14, face = "bold"),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.ticks = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 16),
          legend.position = "bottom",
          legend.direction = "horizontal",
          plot.title = element_text(size = 20, face = "bold"),
          plot.subtitle = element_text(size = 15),
          plot.caption = element_text(size = 16)) +
    guides(fill = guide_legend(nrow = 1))
}

## Export plots
png(paste0(respath,"plots/countries_effects_3.png"), width = 1600, height = 1200)
do.call(grid.arrange, plot_effects_cntr_3)
dev.off()

for (country in countries_names_3) {
  ggsave(filename = paste0(respath,"plots/countries_effects_3.pdf"),
         plot = grid.arrange(plot_effects_cntr_3[["Croatia"]],
                             plot_effects_cntr_3[["Hungary"]],
                             plot_effects_cntr_3[["Spain"]],
                             plot_effects_cntr_3[["Finland"]],  nrow = 2, ncol = 2),
         width = 7500, height = 5000, units = "px")
}

for (country in countries_names_3) {
  ggsave(filename = paste0(respath,"plots/countries_effects_3.eps"),
         plot = grid.arrange(plot_effects_cntr_3[["Croatia"]],
                             plot_effects_cntr_3[["Hungary"]],
                             plot_effects_cntr_3[["Spain"]],
                             plot_effects_cntr_3[["Finland"]],  nrow = 2, ncol = 2),
         width = 7500, height = 5000, units = "px",
         device = cairo_ps)
}


## 4
plot_effects_cntr_4 <- list()

for (country in countries_names_4) {
  Scenario.labs <- c("100% Renewables Scenario", "Reference Scenario")
  names(Scenario.labs) <- c("stanford", "euref")
  
  plot_effects_cntr_4[[country]] <- ggplot(results_effects_cntr[[country]]) +
    geom_bar(aes(x = as.factor(Year), y = Value, fill = as.factor(Effect), group = as.factor(Effect)),
             stat = "identity",
             position = "dodge") + ## ?position_dodge() - position of bars
    facet_grid(~Scenario,
               labeller = labeller(Scenario = Scenario.labs)) + ## apply change of facet labels
    scale_x_discrete(expand = c(0.1, 0.1)) +
    scale_y_continuous(expand = c(0.1, 0.1)) +
    xlab(NULL) +
    ylab("Number of persons (1000)") +
    ggtitle(country,
            "Employment requirements by operation and maintenance, capital formation and\nsubstitution of other fuels") +
    scale_fill_manual(values = c("#a1d99b","#31a354","#1f6617"), ## colours
                      name = "Effect", ## label name
                      labels = c("Operation and maintenance", "Capital formation", "Substitution of other fuels")) + ## label values
    theme(strip.text.x = element_text(size = 16, face = "bold"),
          axis.title.x = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(size = 14, face = "bold"),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.ticks = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 16),
          legend.position = "bottom",
          legend.direction = "horizontal",
          plot.title = element_text(size = 20, face = "bold"),
          plot.subtitle = element_text(size = 15),
          plot.caption = element_text(size = 16)) +
    guides(fill = guide_legend(nrow = 1))
}

## Export plots
png(paste0(respath,"plots/countries_effects_4.png"), width = 1600, height = 1200)
do.call(grid.arrange, plot_effects_cntr_4)
dev.off()

for (country in countries_names_4) {
  ggsave(filename = paste0(respath,"plots/countries_effects_4.pdf"),
         plot = grid.arrange(plot_effects_cntr_4[["France"]],
                             plot_effects_cntr_4[["Greece"]],
                             plot_effects_cntr_4[["Ireland"]],
                             plot_effects_cntr_4[["Italy"]],  nrow = 2, ncol = 2),
         width = 7500, height = 5000, units = "px")
}

for (country in countries_names_4) {
  ggsave(filename = paste0(respath,"plots/countries_effects_4.eps"),
         plot = grid.arrange(plot_effects_cntr_4[["France"]],
                             plot_effects_cntr_4[["Greece"]],
                             plot_effects_cntr_4[["Ireland"]],
                             plot_effects_cntr_4[["Italy"]],  nrow = 2, ncol = 2),
         width = 7500, height = 5000, units = "px",
         device = cairo_ps)
}


## 5
plot_effects_cntr_5 <- list()

for (country in countries_names_5) {
  Scenario.labs <- c("100% Renewables Scenario", "Reference Scenario")
  names(Scenario.labs) <- c("stanford", "euref")
  
  plot_effects_cntr_5[[country]] <- ggplot(results_effects_cntr[[country]]) +
    geom_bar(aes(x = as.factor(Year), y = Value, fill = as.factor(Effect), group = as.factor(Effect)),
             stat = "identity",
             position = "dodge") + ## ?position_dodge() - position of bars
    facet_grid(~Scenario,
               labeller = labeller(Scenario = Scenario.labs)) + ## apply change of facet labels
    scale_x_discrete(expand = c(0.1, 0.1)) +
    scale_y_continuous(expand = c(0.1, 0.1)) +
    xlab(NULL) +
    ylab("Number of persons (1000)") +
    ggtitle(country,
            "Employment requirements by operation and maintenance, capital formation and\nsubstitution of other fuels") +
    scale_fill_manual(values = c("#a1d99b","#31a354","#1f6617"), ## colours
                      name = "Effect", ## label name
                      labels = c("Operation and maintenance", "Capital formation", "Substitution of other fuels")) + ## label values
    theme(strip.text.x = element_text(size = 16, face = "bold"),
          axis.title.x = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(size = 14, face = "bold"),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.ticks = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 16),
          legend.position = "bottom",
          legend.direction = "horizontal",
          plot.title = element_text(size = 20, face = "bold"),
          plot.subtitle = element_text(size = 15),
          plot.caption = element_text(size = 16)) +
    guides(fill = guide_legend(nrow = 1))
}

## Export plots
png(paste0(respath,"plots/countries_effects_5.png"), width = 1600, height = 1200)
do.call(grid.arrange, plot_effects_cntr_5)
dev.off()

for (country in countries_names_5) {
  ggsave(filename = paste0(respath,"plots/countries_effects_5.pdf"),
         plot = grid.arrange(plot_effects_cntr_5[["Lithuania"]],
                             plot_effects_cntr_5[["Luxembourg"]],
                             plot_effects_cntr_5[["Latvia"]],
                             plot_effects_cntr_5[["Malta"]],  nrow = 2, ncol = 2),
         width = 7500, height = 5000, units = "px")
}

for (country in countries_names_5) {
  ggsave(filename = paste0(respath,"plots/countries_effects_5.eps"),
         plot = grid.arrange(plot_effects_cntr_5[["Lithuania"]],
                             plot_effects_cntr_5[["Luxembourg"]],
                             plot_effects_cntr_5[["Latvia"]],
                             plot_effects_cntr_5[["Malta"]],  nrow = 2, ncol = 2),
         width = 7500, height = 5000, units = "px",
         device = cairo_ps)
}


## 6
plot_effects_cntr_6 <- list()

for (country in countries_names_6) {
  Scenario.labs <- c("100% Renewables Scenario", "Reference Scenario")
  names(Scenario.labs) <- c("stanford", "euref")
  
  plot_effects_cntr_6[[country]] <- ggplot(results_effects_cntr[[country]]) +
    geom_bar(aes(x = as.factor(Year), y = Value, fill = as.factor(Effect), group = as.factor(Effect)),
             stat = "identity",
             position = "dodge") + ## ?position_dodge() - position of bars
    facet_grid(~Scenario,
               labeller = labeller(Scenario = Scenario.labs)) + ## apply change of facet labels
    scale_x_discrete(expand = c(0.1, 0.1)) +
    scale_y_continuous(expand = c(0.1, 0.1)) +
    xlab(NULL) +
    ylab("Number of persons (1000)") +
    ggtitle(country,
            "Employment requirements by operation and maintenance, capital formation and\nsubstitution of other fuels") +
    scale_fill_manual(values = c("#a1d99b","#31a354","#1f6617"), ## colours
                      name = "Effect", ## label name
                      labels = c("Operation and maintenance", "Capital formation", "Substitution of other fuels")) + ## label values
    theme(strip.text.x = element_text(size = 16, face = "bold"),
          axis.title.x = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(size = 14, face = "bold"),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.ticks = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 16),
          legend.position = "bottom",
          legend.direction = "horizontal",
          plot.title = element_text(size = 20, face = "bold"),
          plot.subtitle = element_text(size = 15),
          plot.caption = element_text(size = 16)) +
    guides(fill = guide_legend(nrow = 1))
}

## Export plots
png(paste0(respath,"plots/countries_effects_6.png"), width = 1600, height = 1200)
do.call(grid.arrange, plot_effects_cntr_6)
dev.off()

for (country in countries_names_6) {
  ggsave(filename = paste0(respath,"plots/countries_effects_6.pdf"),
         plot = grid.arrange(plot_effects_cntr_6[["Netherlands"]],
                             plot_effects_cntr_6[["Poland"]],
                             plot_effects_cntr_6[["Portugal"]],
                             plot_effects_cntr_6[["Romania"]],  nrow = 2, ncol = 2),
         width = 7500, height = 5000, units = "px")
}

for (country in countries_names_6) {
  ggsave(filename = paste0(respath,"plots/countries_effects_6.eps"),
         plot = grid.arrange(plot_effects_cntr_6[["Netherlands"]],
                             plot_effects_cntr_6[["Poland"]],
                             plot_effects_cntr_6[["Portugal"]],
                             plot_effects_cntr_6[["Romania"]],  nrow = 2, ncol = 2),
         width = 7500, height = 5000, units = "px",
         device = cairo_ps)
}


## 7
plot_effects_cntr_7 <- list()

for (country in countries_names_7) {
  Scenario.labs <- c("100% Renewables Scenario", "Reference Scenario")
  names(Scenario.labs) <- c("stanford", "euref")
  
  plot_effects_cntr_7[[country]] <- ggplot(results_effects_cntr[[country]]) +
    geom_bar(aes(x = as.factor(Year), y = Value, fill = as.factor(Effect), group = as.factor(Effect)),
             stat = "identity",
             position = "dodge") + ## ?position_dodge() - position of bars
    facet_grid(~Scenario,
               labeller = labeller(Scenario = Scenario.labs)) + ## apply change of facet labels
    scale_x_discrete(expand = c(0.1, 0.1)) +
    scale_y_continuous(expand = c(0.1, 0.1)) +
    xlab(NULL) +
    ylab("Number of persons (1000)") +
    ggtitle(country,
            "Employment requirements by operation and maintenance, capital formation and\nsubstitution of other fuels") +
    scale_fill_manual(values = c("#a1d99b","#31a354","#1f6617"), ## colours
                      name = "Effect", ## label name
                      labels = c("Operation and maintenance", "Capital formation", "Substitution of other fuels")) + ## label values
    theme(strip.text.x = element_text(size = 16, face = "bold"),
          axis.title.x = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(size = 14, face = "bold"),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.ticks = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 16),
          legend.position = "bottom",
          legend.direction = "horizontal",
          plot.title = element_text(size = 20, face = "bold"),
          plot.subtitle = element_text(size = 15),
          plot.caption = element_text(size = 16)) +
    guides(fill = guide_legend(nrow = 1))
}

## Export plots
png(paste0(respath,"plots/countries_effects_7.png"), width = 1600, height = 1200)
do.call(grid.arrange, plot_effects_cntr_7)
dev.off()

for (country in countries_names_7) {
  ggsave(filename = paste0(respath,"plots/countries_effects_7.pdf"),
         plot = grid.arrange(plot_effects_cntr_7[["Sweden"]],
                             plot_effects_cntr_7[["Slovenia"]],
                             plot_effects_cntr_7[["Slovakia"]],
                             plot_effects_cntr_7[["United Kingdom"]],  nrow = 2, ncol = 2),
         width = 7500, height = 5000, units = "px")
}

for (country in countries_names_7) {
  ggsave(filename = paste0(respath,"plots/countries_effects_7.eps"),
         plot = grid.arrange(plot_effects_cntr_7[["Sweden"]],
                             plot_effects_cntr_7[["Slovenia"]],
                             plot_effects_cntr_7[["Slovakia"]],
                             plot_effects_cntr_7[["United Kingdom"]],  nrow = 2, ncol = 2),
         width = 7500, height = 5000, units = "px",
         device = cairo_ps)
}



##########################################################################
# 6. Load and visualise scenarios incl. nowcasting until 2019
##########################################################################

##########################################################################
# 6.0. Load and adapt scenarios

## Summarize EU27+UK values for each scenario
scen_EU <- list()

for (scenario in scenarios) {
  scen_EU[[scenario]] <- scen[[scenario]]
  
  ## Make Industry.Name and Industry.Code ordered factors
  scen_EU[[scenario]]$Industry.Name <- factor(scen_EU[[scenario]]$Industry.Name, levels = unique(scen_EU[[scenario]]$Industry.Name), ordered = TRUE)
  scen_EU[[scenario]]$Industry.Code <- factor(scen_EU[[scenario]]$Industry.Code, levels = unique(scen_EU[[scenario]]$Industry.Code), ordered = TRUE)
  scen_EU[[scenario]] <- scen_EU[[scenario]] %>% 
    group_by(Industry.Name, Industry.Code) %>%
    summarise_at(vars(2:33), sum)
}

## Split into scenarios for electricity generation and installed capacity
scen_EU_gen <- list()
scen_EU_inst <- list()

for (scenario in scenarios) {
  scen_EU_gen[[scenario]] <- scen_EU[[scenario]][,grepl("Industry|gen", colnames(scen_EU[[scenario]]))]
  scen_EU_inst[[scenario]] <- scen_EU[[scenario]][,grepl("Industry|inst", colnames(scen_EU[[scenario]]))]
}

## rbind and export the original scen_EU
scen_EU <- bind_rows(scen_EU, .id = "Scenario")
fwrite(scen_EU, file = paste0(respath,"EU_scen.csv"), sep=";", dec=",")


##########################################################################
# 6.1. Plot scenarios for electricity generation

## Change order of the electricity sources for plotting, adjust colnames, melt and adjust colnames again
neworder <- c("i40.11.a","i40.11.b","i40.11.f","i40.11.c","i40.11.g","i40.11.d","i40.11.e.1","i40.11.e.2","i40.11.h.1","i40.11.h.2","i40.11.i","i40.11.j","i40.11.k")
for (scenario in scenarios) {
  scen_EU_gen[[scenario]] <- as.data.frame(scen_EU_gen[[scenario]])
  scen_EU_gen[[scenario]] <- scen_EU_gen[[scenario]] %>% slice(order(factor(Industry.Code, levels = neworder)))
  colnames(scen_EU_gen[[scenario]])[3:10] <- c("2015","2020","2025","2030","2035","2040","2045","2050")
  scen_EU_gen[[scenario]] <- scen_EU_gen[[scenario]] %>% slice(order(factor(Industry.Code, levels = neworder)))
  scen_EU_gen[[scenario]] <- melt(scen_EU_gen[[scenario]])
  colnames(scen_EU_gen[[scenario]])[1] <- "Electricity sources"
  colnames(scen_EU_gen[[scenario]])[3] <- "Year"
  colnames(scen_EU_gen[[scenario]])[4] <- "Value"
}

## Change names of the electricity sources
for (scenario in scenarios) {
  scen_EU_gen[[scenario]]$`Electricity sources` <- rep.int(c("Coal","Gas","Petroleum and other oil derivatives","Nuclear",
                                                             "Biomass and waste","Hydro","Wind onshore","Wind offshore",
                                                             "Solar PV utility","Solar PV residential","Solar thermal",
                                                             "Tide, wave, ocean","Geothermal"), times = 8)
  scen_EU_gen[[scenario]]$`Electricity sources` <- factor(scen_EU_gen[[scenario]]$`Electricity sources`, levels = rev(unique(scen_EU_gen[[scenario]]$`Electricity sources`)), ordered = TRUE)
}

## Match together by scenarios and make Scenario an ordered factor
scen_EU_gen <- bind_rows(scen_EU_gen, .id = "Scenario")
scen_EU_gen$Scenario <- factor(scen_EU_gen$Scenario, levels = unique(rev(scen_EU_gen$Scenario)), ordered = TRUE)

## Adapt values (GWh to TWh)
scen_EU_gen$Value <- scen_EU_gen$Value / 1000

## Plot scenarios
plot_scen_EU_gen <- list()

Scenario.labs <- c("100% Renewables Scenario", "Reference Scenario")
names(Scenario.labs) <- c("stanford", "euref")

plot_scen_EU_gen <- ggplot(scen_EU_gen, aes(x = Year, y = Value, group = `Electricity sources`)) +
  geom_area(aes(fill = `Electricity sources`), position = "stack") +
  xlab(NULL) +
  ylab("TWh") +
  facet_grid(~Scenario,
             labeller = labeller(Scenario = Scenario.labs)) + ## apply change of facet labels
  scale_y_continuous(limits = c(0, 7500)) +
  scale_fill_tableau(palette = "Classic Cyclic",
                     direction = -1,
                     name = "Electricity source",
                     labels = c("Geothermal","Tide, wave, ocean","Solar thermal","Solar PV residential","Solar PV utility",
                                "Wind offshore","Wind onshore","Hydro","Biomass and waste",
                                "Nuclear","Petroleum and other oil derivatives","Gas","Coal")) +
  ggtitle("Electricity generation within the EU27+UK by source (TWh)") +
  theme(strip.text.x = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.caption = element_text(size = 16)) +
  labs(fill = "Electricity source")
  # labs(caption = "Source: EXIOBASE 3.6, EU 2016 Reference Scenario, Stanford WWS Scenario") ## caption

## Export plot
png(paste0(respath,"plots/EU_scen_gen.png"), width = 1200, height = 600)
print(plot_scen_EU_gen)
dev.off()

ggsave(filename = paste0(respath,"plots/EU_scen_gen.pdf"),
       plot = plot_scen_EU_gen,
       width = 5000, height = 2500, units = "px")

ggsave(filename = paste0(respath,"plots/EU_scen_gen.eps"),
       plot = plot_scen_EU_gen,
       width = 5000, height = 2500, units = "px",
       device = cairo_ps)


##########################################################################
# 6.2. Plot scenarios for installed capacity

## Change order of the electricity sources for plotting, adjust colnames, melt and adjust colnames again
neworder <- c("i40.11.a","i40.11.b","i40.11.f","i40.11.c","i40.11.g","i40.11.d","i40.11.e.1","i40.11.e.2","i40.11.h.1","i40.11.h.2","i40.11.i","i40.11.j","i40.11.k")
for (scenario in scenarios) {
  scen_EU_inst[[scenario]] <- as.data.frame(scen_EU_inst[[scenario]])
  scen_EU_inst[[scenario]] <- scen_EU_inst[[scenario]] %>% slice(order(factor(Industry.Code, levels = neworder)))
  colnames(scen_EU_inst[[scenario]])[3:10] <- c("2015","2020","2025","2030","2035","2040","2045","2050")
  scen_EU_inst[[scenario]] <- melt(scen_EU_inst[[scenario]])
  colnames(scen_EU_inst[[scenario]])[1] <- "Electricity sources"
  colnames(scen_EU_inst[[scenario]])[3] <- "Year"
  colnames(scen_EU_inst[[scenario]])[4] <- "Value"
}

## Change names of the electricity sources
for (scenario in scenarios) {
  scen_EU_inst[[scenario]]$`Electricity sources` <- rep.int(c("Coal","Gas","Petroleum and other oil derivatives","Nuclear",
                                                              "Biomass and waste","Hydro","Wind onshore","Wind offshore",
                                                              "Solar PV utility","Solar PV residential","Solar thermal",
                                                              "Tide, wave, ocean","Geothermal"), times = 8)
  scen_EU_inst[[scenario]]$`Electricity sources` <- factor(scen_EU_inst[[scenario]]$`Electricity sources`, levels = rev(unique(scen_EU_inst[[scenario]]$`Electricity sources`)), ordered = TRUE)
}

## Match together by scenarios and make Scenario an ordered factor
scen_EU_inst <- bind_rows(scen_EU_inst, .id = "Scenario")
scen_EU_inst$Scenario <- factor(scen_EU_inst$Scenario, levels = unique(rev(scen_EU_inst$Scenario)), ordered = TRUE)

## Adapt values (MW to GW)
scen_EU_inst$Value <- scen_EU_inst$Value / 1000

## Plot scenarios
plot_scen_EU_inst <- list()

Scenario.labs <- c("100% Renewables Scenario", "Reference Scenario")
names(Scenario.labs) <- c("stanford", "euref")

plot_scen_EU_inst <- ggplot(scen_EU_inst, aes(x = Year, y = Value, group = `Electricity sources`)) +
  geom_area(aes(fill = `Electricity sources`), position = "stack") +
  xlab(NULL) +
  ylab("GW") +
  facet_grid(~Scenario,
             labeller = labeller(Scenario = Scenario.labs)) + ## apply change of facet labels
  scale_y_continuous(limits = c(0, 300)) +
  scale_fill_tableau(palette = "Classic Cyclic",
                     direction = -1,
                     name = "Electricity source",
                     labels = c("Geothermal","Tide, wave, ocean","Solar thermal","Solar PV residential","Solar PV utility",
                                "Wind offshore","Wind onshore","Hydro","Biomass and waste",
                                "Nuclear","Petroleum and other oil derivatives","Gas","Coal")) +
  ggtitle("Yearly new installed and replaced capacity within the EU27+UK by source (GW)") +
  theme(strip.text.x = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.caption = element_text(size = 16)) +
  labs(fill = "Electricity source")
# labs(caption = "Source: EXIOBASE 3.6, EU 2016 Reference Scenario, Stanford WWS Scenario") ## caption

## Export plot
png(paste0(respath,"plots/EU_scen_inst.png"), width = 1200, height = 600)
print(plot_scen_EU_inst)
dev.off()

ggsave(filename = paste0(respath,"plots/EU_scen_inst.pdf"),
       plot = plot_scen_EU_inst,
       width = 5000, height = 2500, units = "px")

ggsave(filename = paste0(respath,"plots/EU_scen_inst.eps"),
       plot = plot_scen_EU_inst,
       width = 5000, height = 2500, units = "px",
       device = cairo_ps)


##########################################################################
# 6.3. Load, adapt and plot scenarios for electricity substitution

## Load scen_fuel_EU
scen_fuel_EU <- list()

for (scenario in scenarios) {
  scen_fuel_EU[["euref"]] <- readxl::read_excel(paste0(datapath, "Scenarios/Scenario_fuel_EURef.xlsx"), sheet = "subst_EU")
  scen_fuel_EU[["stanford"]] <- readxl::read_excel(paste0(datapath, "Scenarios/Scenario_fuel_Stanford.xlsx"), sheet = "subst_EU")
}

## Make category (fuels) ordered factors
for (scenario in scenarios) {
  scen_fuel_EU[[scenario]]$`Electricity substitution by fuel type (ktoe)` <- factor(scen_fuel_EU[[scenario]]$`Electricity substitution by fuel type (ktoe)`, levels = unique(scen_fuel_EU[[scenario]]$`Electricity substitution by fuel type (ktoe)`), ordered = TRUE)
}

## rbind and export (creating extra object to avoid overwriting scen_EU_fuel)
scen_fuel_EU_exp <- bind_rows(scen_fuel_EU, .id = "Scenario")
fwrite(scen_fuel_EU_exp, file = paste0(respath,"EU_scen_fuel.csv"), sep=";", dec=",")

## Melt and adjust colnames
for (scenario in scenarios) {
  scen_fuel_EU[[scenario]] <- melt(scen_fuel_EU[[scenario]])
  colnames(scen_fuel_EU[[scenario]])[2] <- "Year"
  colnames(scen_fuel_EU[[scenario]])[3] <- "Value"
}

## Match together by scenarios and make scenario ordered factor
scen_fuel_EU <- bind_rows(scen_fuel_EU, .id = "Scenario")
scen_fuel_EU$Scenario <- factor(scen_fuel_EU$Scenario, levels = unique(rev(scen_fuel_EU$Scenario)), ordered = TRUE)

## Adapt values (ktoe to mtoe and multiply by -1 to get the replacements by electricity)
scen_fuel_EU$Value <- scen_fuel_EU$Value / -1000
names(scen_fuel_EU)[names(scen_fuel_EU) == "Electricity substitution by fuel type (ktoe)"] <- "Electricity substitution by fuel type (mtoe)"

## Plot scenarios
plot_scen_fuel_EU <- list()

Scenario.labs <- c("100% Renewables Scenario", "Reference Scenario")
names(Scenario.labs) <- c("stanford", "euref")

plot_scen_fuel_EU <- ggplot(scen_fuel_EU, aes(x = Year, y = Value, group = rev(`Electricity substitution by fuel type (mtoe)`))) +
  geom_area(aes(fill = `Electricity substitution by fuel type (mtoe)`), position = "stack") +
  xlab(NULL) +
  ylab("mtoe") +
  facet_grid(~Scenario,
             labeller = labeller(Scenario = Scenario.labs)) + ## apply change of facet labels
  ggtitle("Fuels replaced by electricity within the EU27+UK (mtoe)") +
  theme(strip.text.x = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.caption = element_text(size = 16)) +
  labs(fill = "Fuel type")
# labs(caption = "Source: EXIOBASE 3.6, EU 2016 Reference Scenario, Stanford WWS Scenario") ## caption

## Export plot
png(paste0(respath,"plots/EU_scen_fuel.png"), width = 1200, height = 400)
print(plot_scen_fuel_EU)
dev.off()

ggsave(filename = paste0(respath,"plots/EU_scen_fuel.pdf"),
       plot = plot_scen_fuel_EU,
       width = 5000, height = 1700, units = "px")

ggsave(filename = paste0(respath,"plots/EU_scen_fuel.eps"),
       plot = plot_scen_fuel_EU,
       width = 5000, height = 1700, units = "px",
       device = cairo_ps)


##########################################################################
# 6.4. Recalculate total results for employment per TWh

## Organise total (domestic and spill-over addded together), delete the substitution effects and summarize
results_twh <- results
results_twh <- results_twh[grepl("GFCF|O&M", results_twh$Effect),] %>% 
  group_by(Scenario, Effect, Year) %>%
  summarize(Value = sum(Value))

results_twh_om <- results_twh[grepl("O&M", results_twh$Effect),]
results_twh_om <- results_twh_om %>% 
  group_by(Scenario, Year) %>%
  summarize(Value = sum(Value))

results_twh_gfcf <- results_twh[grepl("GFCF", results_twh$Effect),]
results_twh_gfcf <- results_twh_gfcf %>% 
  group_by(Scenario, Year) %>%
  summarize(Value = sum(Value))

## Recalculate per TWh of generated electricity
scen_twh <- list()

for (scenario in scenarios) {
  scen_twh[[scenario]] <- scen[[scenario]]
}

## Match together by scenarios, select only values for electricity generation and summarize for each scenario
scen_twh <- bind_rows(scen_twh, .id = "Scenario")
scen_twh <- scen_twh[,grepl("Scenario|Industry|gen", colnames(scen_twh))]
scen_twh <- scen_twh %>% 
  group_by(Scenario) %>%
  summarise_at(vars(3:10), sum)

## Change colnames, melt and adapt values (GWh to TWh)
colnames(scen_twh)[2:9] <- c("2015","2020","2025","2030","2035","2040","2045","2050")
scen_twh <- melt(scen_twh, variable.name="Year", value.name="Value")
scen_twh <- scen_twh[order(scen_twh$Scenario),]
scen_twh$Value <- scen_twh$Value / 1000

## Divide the total employment (in 1000 persons) by the electricity generation (in TWh)
results_twh_om$Value <- results_twh_om$Value / scen_twh$Value

## Recalculate per TW of installed capacity
scen_inst <- list()

for (scenario in scenarios) {
  scen_inst[[scenario]] <- scen[[scenario]]
}

## Match together by scenarios, select only values for installed capacity and summarize for each scenario
scen_inst <- bind_rows(scen_inst, .id = "Scenario")
scen_inst <- scen_inst[,grepl("Scenario|Industry|inst", colnames(scen_inst))]
scen_inst <- scen_inst %>% 
  group_by(Scenario) %>%
  summarise_at(vars(3:10), sum)

## Change colnames and melt
colnames(scen_inst)[2:9] <- c("2015","2020","2025","2030","2035","2040","2045","2050")
scen_inst <- melt(scen_inst, variable.name="Year", value.name="Value")
scen_inst <- scen_inst[order(scen_inst$Scenario),]

## Divide the total employment (in 1000 persons) by the installed capacity (in TW)
results_twh_gfcf$Value <- results_twh_gfcf$Value / scen_inst$Value

## Export results
fwrite(results_twh_om, file = paste0(respath,"EU_twh_om.csv"), sep=";", dec=",")
fwrite(results_twh_gfcf, file = paste0(respath,"EU_twh_gfcf.csv"), sep=";", dec=",")


