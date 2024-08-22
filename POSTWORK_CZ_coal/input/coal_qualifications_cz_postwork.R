##########################################################################
# impactech coal_qualifications_cz

# Objective: 

# Modules (in order of integration):
# (1) qualification requirements of the coal mining occupations
# (2) qualification requirements of other occupations
# (3) clustering green, mixed, and brown jobs

# Sectors:
# (1) coal mining

# Indicators:
# (1) kkovs education categories
# (2) nsp competences
# (3) nsp generic skills
# (4) nsp soft skills
# (5) worker preferences

# Geographic scope: CZ

# Input data:
# (1) qualifications data: MPSV (API)

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
install.packages("ggsci")
install.packages("ggthemes")
install.packages("xlsx")
install.packages("plotly")
install.packages("RColorBrewer")
install.packages("tidytext")
install.packages(c("httr", "jsonlite"))
install.packages("ggVennDiagram")
install.packages("sf")
install.packages("RCzechia")
install.packages("ggthemes")
install.packages("showtext")
install.packages("sysfonts")
install.packages("ggplotify")
install.packages("gridtext")
install.packages("cowplot")
install.packages("ggrepel")

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
library(ggsci)
library(ggthemes)
library(xlsx)
library(plotly)
library(RColorBrewer)
library(tidytext)
library(httr)
library(jsonlite)
library(ggVennDiagram)
library(RCzechia)
library(ggthemes)
library(showtext)
library(sysfonts)
library(ggplotify)
library(grid)
library(gtable)
library(gridtext)
library(cowplot)
library(ggrepel)

## Add fonts
# font_families()
font_add_google("Roboto", "Roboto")
showtext_auto()



##########################################################################
# 0. Set path and functions
##########################################################################

## https://www.dataquest.io/blog/r-api-tutorial/

##########################################################################
# 0.1. Set path

cdkpath <- "C:/Users/Cerny/Documents/Work/Models/IMPACTECH/data/input_data/labor/cdk/" ## storage of original o*net data
datapath <- "C:/Users/Cerny/Documents/Work/Models/IMPACTECH/data/input_data/" ## storage of (other) input files
# intpath <- "C:/Users/Cerny/Documents/Work/Models/IMPACTECH/data/intermediate_data/" ## storage of intermediate data
respath <- "C:/Users/Cerny/Documents/Work/Models/IMPACTECH/results/output_data/coal_qualifications_cz/" ## storage of results
imgpath <- "C:/Users/Cerny/Documents/Work/Models/IMPACTECH/results/figures/coal_qualifications_cz/" ## storage of figures


##########################################################################
# 0.2. Load necessary functions

agg <- function(x)
{
  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
  return(x)
}

`%!in%` = Negate(`%in%`)

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))



##########################################################################
# 1. Get MPSV API data and adjust
##########################################################################

## Source: https://nsp.cz/api/doc/v1.2; https://www.nsp.cz/napoveda/api

##########################################################################
# 1.1. Get MPSV API data for professional directions, work units, isco categories, competences, generic skills and soft skills

## Work units
res1 = GET("http://nsp.cz/api/v1.2/workUnit?limit=100&offset=0&includePublic=true&includeSuggestion=false", accept_json())
res2 = GET("http://nsp.cz/api/v1.2/workUnit?limit=100&offset=100&includePublic=true&includeSuggestion=false", accept_json())
res3 = GET("http://nsp.cz/api/v1.2/workUnit?limit=100&offset=200&includePublic=true&includeSuggestion=false", accept_json())
res4 = GET("http://nsp.cz/api/v1.2/workUnit?limit=100&offset=300&includePublic=true&includeSuggestion=false", accept_json())
res5 = GET("http://nsp.cz/api/v1.2/workUnit?limit=100&offset=400&includePublic=true&includeSuggestion=false", accept_json())
res6 = GET("http://nsp.cz/api/v1.2/workUnit?limit=100&offset=500&includePublic=true&includeSuggestion=false", accept_json())
res7 = GET("http://nsp.cz/api/v1.2/workUnit?limit=100&offset=600&includePublic=true&includeSuggestion=false", accept_json())
res8 = GET("http://nsp.cz/api/v1.2/workUnit?limit=100&offset=700&includePublic=true&includeSuggestion=false", accept_json())
res9 = GET("http://nsp.cz/api/v1.2/workUnit?limit=100&offset=800&includePublic=true&includeSuggestion=false", accept_json())
res10 = GET("http://nsp.cz/api/v1.2/workUnit?limit=100&offset=900&includePublic=true&includeSuggestion=false", accept_json())
res11 = GET("http://nsp.cz/api/v1.2/workUnit?limit=100&offset=1000&includePublic=true&includeSuggestion=false", accept_json())
res12 = GET("http://nsp.cz/api/v1.2/workUnit?limit=100&offset=1100&includePublic=true&includeSuggestion=false", accept_json())
res13 = GET("http://nsp.cz/api/v1.2/workUnit?limit=100&offset=1200&includePublic=true&includeSuggestion=false", accept_json())
res14 = GET("http://nsp.cz/api/v1.2/workUnit?limit=100&offset=1300&includePublic=true&includeSuggestion=false", accept_json())
res15 = GET("http://nsp.cz/api/v1.2/workUnit?limit=100&offset=1400&includePublic=true&includeSuggestion=false", accept_json())
res16 = GET("http://nsp.cz/api/v1.2/workUnit?limit=100&offset=1500&includePublic=true&includeSuggestion=false", accept_json())
res17 = GET("http://nsp.cz/api/v1.2/workUnit?limit=100&offset=1600&includePublic=true&includeSuggestion=false", accept_json())
res18 = GET("http://nsp.cz/api/v1.2/workUnit?limit=100&offset=1700&includePublic=true&includeSuggestion=false", accept_json())
res19 = GET("http://nsp.cz/api/v1.2/workUnit?limit=100&offset=1800&includePublic=true&includeSuggestion=false", accept_json())
res20 = GET("http://nsp.cz/api/v1.2/workUnit?limit=100&offset=1900&includePublic=true&includeSuggestion=false", accept_json())
res21 = GET("http://nsp.cz/api/v1.2/workUnit?limit=100&offset=2000&includePublic=true&includeSuggestion=false", accept_json())
res22 = GET("http://nsp.cz/api/v1.2/workUnit?limit=100&offset=2100&includePublic=true&includeSuggestion=false", accept_json())
res23 = GET("http://nsp.cz/api/v1.2/workUnit?limit=100&offset=2200&includePublic=true&includeSuggestion=false", accept_json())
res24 = GET("http://nsp.cz/api/v1.2/workUnit?limit=100&offset=2300&includePublic=true&includeSuggestion=false", accept_json())
res25 = GET("http://nsp.cz/api/v1.2/workUnit?limit=100&offset=2400&includePublic=true&includeSuggestion=false", accept_json())
res26 = GET("http://nsp.cz/api/v1.2/workUnit?limit=100&offset=2500&includePublic=true&includeSuggestion=false", accept_json())
res27 = GET("http://nsp.cz/api/v1.2/workUnit?limit=100&offset=2600&includePublic=true&includeSuggestion=false", accept_json())

reslist <- list(res1, res2, res3, res4, res5, res6, res7, res8, res9, res10,
                res11, res12, res13, res14, res15, res16, res17, res18, res19, res20,
                res21, res22, res23, res24, res25, res26, res27)
rescount <- c(1:27)
workunits <- list()

for (i in rescount) {
  workunits[[i]] = fromJSON(rawToChar(reslist[[i]]$content))
  workunits[[i]] <- workunits[[i]]$data
  workunits[[i]]$eqf <- workunits[[i]]$professionalDirectionPrimary <- workunits[[i]]$professionalDirectionSecondary <- 
    workunits[[i]]$professionalDirectionAdditions <- workunits[[i]]$alternativeNames <- workunits[[i]]$parent <- workunits[[i]]$children <- NULL
}

workunits <- do.call("rbind", workunits)
fwrite(workunits, file = paste0(cdkpath,"workunits.csv"), sep=";", dec=",", bom = TRUE)


## Get combination of work units and professional directions
workunits_directions <- list()

for (i in 1:26) { # 27 from reslist is empty
  workunits_directions[[i]] = fromJSON(rawToChar(reslist[[i]]$content))
  workunits_directions[[i]] <- workunits_directions[[i]]$data
  workunits_directions[[i]] <- cbind(workunits_directions[[i]]$urlSlug, workunits_directions[[i]]$title, 
                                     workunits_directions[[i]]$professionalDirectionPrimary)
  colnames(workunits_directions[[i]])[which(names(workunits_directions[[i]]) == "id")] <- "direction.id"
  colnames(workunits_directions[[i]])[which(names(workunits_directions[[i]]) == "title")] <- "direction.title"
  colnames(workunits_directions[[i]])[which(names(workunits_directions[[i]]) == "urlSlug")] <- "direction.urlSlug"
  colnames(workunits_directions[[i]]) <- gsub(x = colnames(workunits_directions[[i]]), pattern = "workunits_directions[[i]]$", replacement = "", fixed = T)
}

workunits_directions <- do.call("rbind", workunits_directions)
fwrite(workunits_directions, file = paste0(cdkpath,"workunits_directions.csv"), sep=";", dec=",", bom = TRUE)


## ISCO categories
res1 = GET("http://nsp.cz/api/v1.2/lists/iSCO", accept_json())
isco = fromJSON(rawToChar(res1$content))
isco <- isco$data
isco <- isco %>%
  select(title, code)

fwrite(isco, file = paste0(cdkpath,"isco.csv"), sep=";", dec=",", bom = TRUE)


## Competences
res1 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=0", accept_json())
res2 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=100", accept_json())
res3 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=200", accept_json())
res4 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=300", accept_json())
res5 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=400", accept_json())
res6 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=500", accept_json())
res7 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=600", accept_json())
res8 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=700", accept_json())
res9 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=800", accept_json())
res10 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=900", accept_json())
res11 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=1000", accept_json())
res12 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=1100", accept_json())
res13 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=1200", accept_json())
res14 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=1300", accept_json())
res15 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=1400", accept_json())
res16 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=1500", accept_json())
res17 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=1600", accept_json())
res18 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=1700", accept_json())
res19 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=1800", accept_json())
res20 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=1900", accept_json())
res21 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=2000", accept_json())
res22 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=2100", accept_json())
res23 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=2200", accept_json())
res24 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=2300", accept_json())
res25 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=2400", accept_json())
res26 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=2500", accept_json())
res27 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=2600", accept_json())
res28 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=2700", accept_json())
res29 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=2800", accept_json())
res30 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=2900", accept_json())
res31 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=3000", accept_json())
res32 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=3100", accept_json())
res33 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=3200", accept_json())
res34 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=3300", accept_json())
res35 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=3400", accept_json())
res36 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=3500", accept_json())
res37 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=3600", accept_json())
res38 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=3700", accept_json())
res39 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=3800", accept_json())
res40 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=3900", accept_json())
res41 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=4000", accept_json())
res42 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=4100", accept_json())
res43 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=4200", accept_json())
res44 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=4300", accept_json())
res45 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=4400", accept_json())
res46 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=4500", accept_json())
res47 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=4600", accept_json())
res48 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=4700", accept_json())
res49 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=4800", accept_json())
res50 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=4900", accept_json())
res51 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=5000", accept_json())
res52 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=5100", accept_json())
res53 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=5200", accept_json())
res54 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=5300", accept_json())
res55 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=5400", accept_json())
res56 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=5500", accept_json())
res57 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=5600", accept_json())
res58 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=5700", accept_json())
res59 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=5800", accept_json())
res60 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=5900", accept_json())
res61 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=6000", accept_json())
res62 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=6100", accept_json())
res63 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=6200", accept_json())
res64 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=6300", accept_json())
res65 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=6400", accept_json())
res66 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=6500", accept_json())
res67 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=6600", accept_json())
res68 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=6700", accept_json())
res69 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=6800", accept_json())
res70 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=6900", accept_json())
res71 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=7000", accept_json())
res72 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=7100", accept_json())
res73 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=7200", accept_json())
res74 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=7300", accept_json())
res75 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=7400", accept_json())
res76 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=7500", accept_json())
res77 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=7600", accept_json())
res78 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=7700", accept_json())
res79 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=7800", accept_json())
res80 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=7900", accept_json())
res81 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=8000", accept_json())
res82 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=8100", accept_json())
res83 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=8200", accept_json())
res84 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=8300", accept_json())
res85 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=8400", accept_json())
res86 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=8500", accept_json())
res87 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=8600", accept_json())
res88 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=8700", accept_json())
res89 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=8800", accept_json())
res90 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=8900", accept_json())
res91 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=9000", accept_json())
res92 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=9100", accept_json())
res93 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=9200", accept_json())
res94 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=9300", accept_json())
res95 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=9400", accept_json())
res96 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=9500", accept_json())
res97 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=9600", accept_json())
res98 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=9700", accept_json())
res99 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=9800", accept_json())
res100 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=9900", accept_json())
res101 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=10000", accept_json())
res102 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=10100", accept_json())
res103 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=10200", accept_json())
res104 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=10300", accept_json())
res105 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=10400", accept_json())
res106 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=10500", accept_json())
res107 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=10600", accept_json())
res108 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=10700", accept_json())
res109 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=10800", accept_json())
res110 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=10900", accept_json())
res111 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=11000", accept_json())
res112 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=11100", accept_json())
res113 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=11200", accept_json())
res114 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=11300", accept_json())
res115 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=11400", accept_json())
res116 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=11500", accept_json())
res117 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=11600", accept_json())
res118 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=11700", accept_json())
res119 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=11800", accept_json())
res120 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=11900", accept_json())
res121 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=12000", accept_json())
res122 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=12100", accept_json())
res123 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=12200", accept_json())
res124 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=12300", accept_json())
res125 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=12400", accept_json())
res126 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=12500", accept_json())
res127 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=12600", accept_json())
res128 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=12700", accept_json())
res129 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=12800", accept_json())
res130 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=12900", accept_json())
res131 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=13000", accept_json())
res132 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=13100", accept_json())
res133 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=13200", accept_json())
res134 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=13300", accept_json())
res135 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=13400", accept_json())
res136 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=13500", accept_json())
res137 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=13600", accept_json())
res138 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=13700", accept_json())
res139 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=13800", accept_json())
res140 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=13900", accept_json())
res141 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=14000", accept_json())
res142 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=14100", accept_json())
res143 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=14200", accept_json())
res144 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=14300", accept_json())
res145 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=14400", accept_json())
res146 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=14500", accept_json())
res147 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=14600", accept_json())
res148 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=14700", accept_json())
res149 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=14800", accept_json())
res150 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=14900", accept_json())
res151 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=15000", accept_json())
res152 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=15100", accept_json())
res153 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=15200", accept_json())
res154 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=15300", accept_json())
res155 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=15400", accept_json())
res156 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=15500", accept_json())
res157 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=15600", accept_json())
res158 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=15700", accept_json())
res159 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=15800", accept_json())
res160 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=15900", accept_json())
res161 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=16000", accept_json())
res162 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=16100", accept_json())
res163 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=16200", accept_json())
res164 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=16300", accept_json())
res165 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=16400", accept_json())
res166 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=16500", accept_json())
res167 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=16600", accept_json())
res168 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=16700", accept_json())
res169 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=16800", accept_json())
res170 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=16900", accept_json())
res171 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=17000", accept_json())
res172 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=17100", accept_json())
res173 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=17200", accept_json())
res174 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=17300", accept_json())
res175 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=17400", accept_json())
res176 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=17500", accept_json())
res177 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=17600", accept_json())
res178 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=17700", accept_json())
res179 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=17800", accept_json())
res180 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=17900", accept_json())
res181 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=18000", accept_json())
res182 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=18100", accept_json())
res183 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=18200", accept_json())
res184 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=18300", accept_json())
res185 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=18400", accept_json())
res186 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=18500", accept_json())
res187 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=18600", accept_json())
res188 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=18700", accept_json())
res189 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=18800", accept_json())
res190 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=18900", accept_json())
res191 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=19000", accept_json())
res192 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=19100", accept_json())
res193 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=19200", accept_json())
res194 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=19300", accept_json())
res195 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=19400", accept_json())
res196 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=19500", accept_json())
res197 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=19600", accept_json())
res198 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=19700", accept_json())
res199 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=19800", accept_json())
res200 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=19900", accept_json())
res201 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=20000", accept_json())
res202 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=20100", accept_json())
res203 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=20200", accept_json())
res204 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=20300", accept_json())
res205 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=20400", accept_json())
res206 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=20500", accept_json())
res207 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=20600", accept_json())
res208 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=20700", accept_json())
res209 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=20800", accept_json())
res210 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=20900", accept_json())
res211 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=21000", accept_json())
res212 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=21100", accept_json())
res213 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=21200", accept_json())
res214 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=21300", accept_json())
res215 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=21400", accept_json())
res216 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=21500", accept_json())
res217 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=21600", accept_json())
res218 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=21700", accept_json())
res219 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=21800", accept_json())
res220 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=21900", accept_json())
res221 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=22000", accept_json())
res222 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=22100", accept_json())
res223 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=22200", accept_json())
res224 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=22300", accept_json())
res225 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=22400", accept_json())
res226 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=22500", accept_json())
res227 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=22600", accept_json())
res228 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=22700", accept_json())
res229 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=22800", accept_json())
res230 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=22900", accept_json())
res231 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=23000", accept_json())
res232 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=23100", accept_json())
res233 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=23200", accept_json())
res234 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=23300", accept_json())
res235 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=23400", accept_json())
res236 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=23500", accept_json())
res237 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=23600", accept_json())
res238 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=23700", accept_json())
res239 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=23800", accept_json())
res240 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=23900", accept_json())
res241 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=24000", accept_json())
res242 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=24100", accept_json())
res243 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=24200", accept_json())
res244 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=24300", accept_json())
res245 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=24400", accept_json())
res246 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=24500", accept_json())
res247 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=24600", accept_json())
res248 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=24700", accept_json())
res249 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=24800", accept_json())
res250 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=24900", accept_json())
res251 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=25000", accept_json())
res252 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=25100", accept_json())
res253 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=25200", accept_json())
res254 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=25300", accept_json())
res255 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=25400", accept_json())
res256 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=25500", accept_json())
res257 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=25600", accept_json())
res258 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=25700", accept_json())
res259 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=25800", accept_json())
res260 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=25900", accept_json())
res261 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=26000", accept_json())
res262 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=26100", accept_json())
res263 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=26200", accept_json())
res264 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=26300", accept_json())
res265 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=26400", accept_json())
res266 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=26600", accept_json())
res267 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=26600", accept_json())
res268 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=26700", accept_json())
res269 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=26800", accept_json())
res270 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=26900", accept_json())
res271 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=27000", accept_json())
res272 = GET("http://nsp.cz/api/v1.2/cdk/competence?limit=100&offset=27100", accept_json())

reslist <- list(res1, res2, res3, res4, res5, res6, res7, res8, res9, res10,
                res11, res12, res13, res14, res15, res16, res17, res18, res19, res20,
                res21, res22, res23, res24, res25, res26, res27, res28, res29, res30,
                res31, res32, res33, res34, res35, res36, res37, res38, res39, res40,
                res41, res42, res43, res44, res45, res46, res47, res48, res49, res50,
                res51, res52, res53, res54, res55, res56, res57, res58, res59, res60,
                res61, res62, res63, res64, res65, res66, res67, res68, res69, res70,
                res71, res72, res73, res74, res75, res76, res77, res78, res79, res80,
                res81, res82, res83, res84, res85, res86, res87, res88, res89, res90,
                res91, res92, res93, res94, res95, res96, res97, res98, res99, res100,
                res101, res102, res103, res104, res105, res106, res107, res108, res109, res110,
                res111, res112, res113, res114, res115, res116, res117, res118, res119, res120,
                res121, res122, res123, res124, res125, res126, res127, res128, res129, res130,
                res131, res132, res133, res134, res135, res136, res137, res138, res139, res140,
                res141, res142, res143, res144, res145, res146, res147, res148, res149, res150,
                res151, res152, res153, res154, res155, res156, res157, res158, res159, res160,
                res161, res162, res163, res164, res165, res166, res167, res168, res169, res170,
                res171, res172, res173, res174, res175, res176, res177, res178, res179, res180,
                res181, res182, res183, res184, res185, res186, res187, res188, res189, res190,
                res191, res192, res193, res194, res195, res196, res197, res198, res199, res200,
                res201, res202, res203, res204, res205, res206, res207, res208, res209, res210,
                res211, res212, res213, res214, res215, res216, res217, res218, res219, res220,
                res221, res222, res223, res224, res225, res226, res227, res228, res229, res230,
                res231, res232, res233, res234, res235, res236, res237, res238, res239, res240,
                res241, res242, res243, res244, res245, res246, res247, res248, res249, res250,
                res251, res252, res253, res254, res255, res256, res257, res258, res259, res260,
                res261, res262, res263, res264, res265, res266, res267, res268, res269, res270,
                res271, res272)
rescount <- c(1:272)
competences <- list()

for (i in rescount) {
  competences[[i]] = fromJSON(rawToChar(reslist[[i]]$content))
  competences[[i]] <- competences[[i]]$data
  competences[[i]] <- cbind(competences[[i]]$id, competences[[i]]$title, competences[[i]]$legacyId, competences[[i]]$code,
                            competences[[i]]$fullCode, competences[[i]]$type, competences[[i]]$updatedAt,
                            competences[[i]]$parent)
  colnames(competences[[i]])[which(names(competences[[i]]) == "id")] <- "mainId"
  colnames(competences[[i]])[which(names(competences[[i]]) == "title")] <- "mainTitle"
  colnames(competences[[i]])[which(names(competences[[i]]) == "legacyId")] <- "mainLegacyId"
  colnames(competences[[i]])[which(names(competences[[i]]) == "code")] <- "mainCode"
  colnames(competences[[i]])[which(names(competences[[i]]) == "fullCode")] <- "mainFullCode"
  colnames(competences[[i]]) <- gsub(x = colnames(competences[[i]]), pattern = "competences[[i]]$", replacement = "", fixed = T)
  colnames(competences[[i]]) <- gsub(x = colnames(competences[[i]]), pattern = "`", replacement = "", fixed = T)
}

competences <- do.call("rbind", competences)
fwrite(competences, file = paste0(cdkpath,"competences.csv"), sep=";", dec=",", bom = TRUE)


## Generic skills
res1 = GET("http://nsp.cz/api/v1.2/cdk/generic-skill", accept_json())
generic_skills = fromJSON(rawToChar(res1$content))
generic_skills <- generic_skills$data
generic_skills$descriptionExamples <- NULL

fwrite(generic_skills, file = paste0(cdkpath,"generic_skills.csv"), sep=";", dec=",", bom = TRUE)


## Soft skills
res1 = GET("http://nsp.cz/api/v1.2/cdk/soft-skill", accept_json())
soft_skills = fromJSON(rawToChar(res1$content))
soft_skills <- soft_skills$data

fwrite(soft_skills, file = paste0(cdkpath,"soft_skills.csv"), sep=";", dec=",", bom = TRUE)


## Education KKOV
res1 = GET("http://nsp.cz/api/v1.2/lists/kKOV", accept_json())
education = fromJSON(rawToChar(res1$content))
education <- education$data
education <- education %>%
  select(title, code)

fwrite(education, file = paste0(cdkpath,"education.csv"), sep=";", dec=",", bom = TRUE)


## Remove res files to clean up
remove(res1, res2, res3, res4, res5, res6, res7, res8, res9, res10,
       res11, res12, res13, res14, res15, res16, res17, res18, res19, res20,
       res21, res22, res23, res24, res25, res26, res27, res28, res29, res30,
       res31, res32, res33, res34, res35, res36, res37, res38, res39, res40,
       res41, res42, res43, res44, res45, res46, res47, res48, res49, res50,
       res51, res52, res53, res54, res55, res56, res57, res58, res59, res60,
       res61, res62, res63, res64, res65, res66, res67, res68, res69, res70,
       res71, res72, res73, res74, res75, res76, res77, res78, res79, res80,
       res81, res82, res83, res84, res85, res86, res87, res88, res89, res90,
       res91, res92, res93, res94, res95, res96, res97, res98, res99, res100,
       res101, res102, res103, res104, res105, res106, res107, res108, res109, res110,
       res111, res112, res113, res114, res115, res116, res117, res118, res119, res120,
       res121, res122, res123, res124, res125, res126, res127, res128, res129, res130,
       res131, res132, res133, res134, res135, res136, res137, res138, res139, res140,
       res141, res142, res143, res144, res145, res146, res147, res148, res149, res150,
       res151, res152, res153, res154, res155, res156, res157, res158, res159, res160,
       res161, res162, res163, res164, res165, res166, res167, res168, res169, res170,
       res171, res172, res173, res174, res175, res176, res177, res178, res179, res180,
       res181, res182, res183, res184, res185, res186, res187, res188, res189, res190,
       res191, res192, res193, res194, res195, res196, res197, res198, res199, res200,
       res201, res202, res203, res204, res205, res206, res207, res208, res209, res210,
       res211, res212, res213, res214, res215, res216, res217, res218, res219, res220,
       res221, res222, res223, res224, res225, res226, res227, res228, res229, res230,
       res231, res232, res233, res234, res235, res236, res237, res238, res239, res240,
       res241, res242, res243, res244, res245, res246, res247, res248, res249, res250,
       res251, res252, res253, res254, res255, res256, res257, res258, res259, res260,
       res261, res262, res263, res264, res265, res266, res267, res268, res269, res270,
       res271, res272, reslist)


##########################################################################
# 1.2. Create a vector from urlSlug (simplified job names) and a converter from urlSlug to job titles and define additional sets

jobs_nsp_all <- c(workunits$urlSlug)
jobs_converter <- data.frame("urlSlug" = workunits$urlSlug, "title" = workunits$title)

## Problematic jobs that do not match the length of datasets for competences, generic skills and soft skills
problematic_jobs <- c("bezpecnostni-technolog",
                      "monter-mechanickych-zabra",
                      "obsluha-stavebnich-stroju-2366",
                      "pomocnik-podkovare",
                      "ridic-kolesoveho-nakladac",
                      "referent-specialista-stat-5f1d")


##########################################################################
# 1.3. Combine data for ISCO for all jobs

## Get information for all jobs (as urlSlug) together in a loop
res <- data_isco <- list()

# job <- "advokat"

for (job in jobs_nsp_all) {
  print(job)
  
  res[[job]] = GET(paste0("http://nsp.cz/api/v1.2/workUnit/",job,"/isco"), accept_json())
  data_isco[[job]] = fromJSON(rawToChar(res[[job]]$content))
  data_isco[[job]] <- data_isco[[job]]$data
  
  data_isco[[job]]$job <- rep(job)
  
  ## Remove jobs with no information
  if(is.null(ncol(data_isco[[job]]))) {data_isco[[job]] <- NULL}
  
  ## Possibly also get rid of the jobs that do not have any information for the competences, generic skills and soft skills?
  
  ## Get rid of the problematic jobs
  # data_isco[names(data_isco) %in% problematic_jobs] <- NULL
}

## Remove res file to clean up
remove(res)

## Update the set of jobs after removing those with no information and the problematic jobs with extra duplications
jobs_isco <- names(data_isco)

## Cluster ISCO categories to 4 digits
isco_full <- isco
isco <- isco_full[nchar(as.character(isco_full$code)) == 4,]
isco <- isco[!grepl("Neza?aditeln?", isco$title),]

## Harmonize data
# job <- "advokat"

for (job in jobs_isco) {
  print(job)
  
  ## Harmonize columns
  data_isco[[job]] <- do.call("cbind", data_isco[[job]])
  data_isco[[job]] <- data_isco[[job]] %>% select(job, isco.title, isco.code)
  colnames(data_isco[[job]])[colnames(data_isco[[job]]) == "isco.title"] <- "title"
  colnames(data_isco[[job]])[colnames(data_isco[[job]]) == "isco.code"] <- "code"
  data_isco[[job]]$code <- substr(data_isco[[job]]$code, 1, 4) ## leave only 4 digits category
  data_isco[[job]] <- data_isco[[job]] %>%
    group_by(job, code) %>%
    select(job, code) %>%
    unique() ## cluster according to the 4 digits category (note - this leaves out the title, but this is added later again)
  data_isco[[job]]$level <- 1 ## add "level" (1 or 0) to indicate if the job fits to this ISCO category (later zeros will be inserted everywhere else)
  data_isco[[job]]$job <- jobs_converter$title[match(data_isco[[job]]$job, jobs_converter$urlSlug)]
  
  ## Add all ISCOs to each job to harmonize the data (create empty rows)
  data_isco[[job]] <- left_join(isco[,c("title", "code")], data_isco[[job]][,c("job", "code", "level")], by = "code")
  data_isco[[job]]$job <- job
  data_isco[[job]]$job <- jobs_converter$title[match(data_isco[[job]]$job, jobs_converter$urlSlug)]
  data_isco[[job]][is.na(data_isco[[job]])] <- 0
}

## Check if list length is identical
data_isco_length <- list()
for (job in jobs_isco) {
  data_isco_length[[job]] <- nrow(data_isco[[job]])
}
data_isco_length <- as.data.frame(unlist(data_isco_length))
data_isco_length <- data_isco_length %>% filter_all(any_vars(.!=435))

## rbind jobs by each category
isco_categories <- as.data.frame(data.table::rbindlist(data_isco))
isco_categories$job.simple <- jobs_converter$urlSlug[match(isco_categories$job, jobs_converter$title)]

## Export
fwrite(isco_categories, file = paste0(cdkpath,"isco_categories.csv"), sep=";", dec=",", bom = TRUE)

## Cast ("unmelt") to match values next to each other for each category
gc()

isco_categories <- as.data.table(isco_categories)
isco_categories <- dcast.data.table(isco_categories, title + code ~ job, value.var = "level", fun.aggregate = sum)

## Export casted
fwrite(isco_categories, file = paste0(cdkpath,"isco_categories_casted.csv"), sep=";", dec=",", bom = TRUE)


##########################################################################
# 1.4. Combine data for competences (competences, generic skills and soft skills) for all jobs

## Get information for all jobs (as urlSlug) together in a loop
res <- data <- data_competences <- data_generic <- data_soft <- list()

# job <- "advokat"

for (job in jobs_nsp_all) {
  print(job)
  
  res[[job]] = GET(paste0("http://nsp.cz/api/v1.2/workUnit/",job,"/competence"), accept_json())
  data[[job]] = fromJSON(rawToChar(res[[job]]$content))
  data[[job]] <- data[[job]]$data
  
  data_competences[[job]] <- data[[job]]$competences
  data_generic[[job]] <- data[[job]]$genericSkills
  data_soft[[job]] <- data[[job]]$softSkills
  
  data_competences[[job]]$job <- rep(job)
  data_generic[[job]]$job <- rep(job)
  data_soft[[job]]$job <- rep(job)
  
  ## Remove jobs with no information
  if(is.null(ncol(data_competences[[job]])) | 
     is.null(ncol(data_generic[[job]])) | 
     is.null(ncol(data_soft[[job]]))) {data_competences[[job]] <- data_generic[[job]] <- data_soft[[job]] <- NULL}
  
  ## Get rid of the problematic jobs
  data_competences[names(data_competences) %in% problematic_jobs] <- NULL
  data_generic[names(data_generic) %in% problematic_jobs] <- NULL
  data_soft[names(data_soft) %in% problematic_jobs] <- NULL
}

## Remove res file to clean up
remove(res)

## Update the set of jobs after removing those with no information and the problematic jobs with extra duplicated competences
jobs_nsp <- names(data_competences)

## Cluster competences into larger groups (according to mainTitle)
competences_full <- competences
competences <- competences_full %>%
  group_by(mainTitle, mainFullCode) %>%
  summarise(count = n(), mainTitle = mainTitle) %>%
  select(mainTitle, mainFullCode) %>%
  unique()
colnames(competences)[colnames(competences) == "mainFullCode"] <- "code"
colnames(competences)[colnames(competences) == "mainTitle"] <- "title"

## Harmonize data
# job <- "advokat"

for (job in jobs_nsp) {
  print(job)
  
  ## Harmonize columns
  data_competences[[job]] <- cbind(data_competences[[job]]$competence, data_competences[[job]]$id, data_competences[[job]]$level,
                                   data_competences[[job]]$suitability, data_competences[[job]]$job)
  data_competences[[job]] <- cbind(data_competences[[job]]$treeGroupParent,
                                   data_competences[[job]]$fullCode, data_competences[[job]]$legacyId, data_competences[[job]]$title,
                                   data_competences[[job]]$code, data_competences[[job]]$`data_competences[[job]]$id`,
                                   data_competences[[job]]$`data_competences[[job]]$level`, data_competences[[job]]$`data_competences[[job]]$suitability`,
                                   data_competences[[job]]$`data_competences[[job]]$job`)
  colnames(data_competences[[job]])[which(names(data_competences[[job]]) == "title")] <- "mainTitle"
  colnames(data_competences[[job]])[which(names(data_competences[[job]]) == "code")] <- "mainCode"
  colnames(data_competences[[job]])[which(names(data_competences[[job]]) == "id")] <- "mainId"
  colnames(data_competences[[job]]) <- gsub(x = colnames(data_competences[[job]]), pattern = "data_competences[[job]]$", replacement = "", fixed = T)
  colnames(data_competences[[job]]) <- gsub(x = colnames(data_competences[[job]]), pattern = "`", replacement = "", fixed = T)
  data_competences[[job]] <- data_competences[[job]] %>% select(job, mainTitle, mainCode, level)
  data_competences[[job]] <- data_competences[[job]] %>%
    group_by(job, mainTitle, mainCode) %>%
    summarise(level = mean(level)) %>%
    select(job, mainTitle, mainCode, level) ## cluster competences and average their levels from the more detailed competences
  data_competences[[job]]$mainFullCode <- competences$code[match(data_competences[[job]]$mainTitle, competences$title)] ## match mainFullCode
  data_competences[[job]]$job <- jobs_converter$title[match(data_competences[[job]]$job, jobs_converter$urlSlug)] ## match job name
  data_competences[[job]]$mainCode <- NULL ## delete "mainCode" and turn "mainFullCode" to "code" to make it compatible with Generic skills and Soft skills where fullCode is missing
  colnames(data_competences[[job]])[colnames(data_competences[[job]]) == "mainFullCode"] <- "code"
  colnames(data_competences[[job]])[colnames(data_competences[[job]]) == "mainTitle"] <- "title"
  data_competences[[job]] <- data_competences[[job]][, c("job","title","code","level")] ## reorder columns to make it compatible with Generic skills and Soft skills
  
  data_generic[[job]] <- cbind(data_generic[[job]]$genericSkill, data_generic[[job]]$id, data_generic[[job]]$level,
                               data_generic[[job]]$suitability, data_generic[[job]]$job)
  colnames(data_generic[[job]])[which(names(data_generic[[job]]) == "id")] <- "mainId"
  colnames(data_generic[[job]]) <- gsub(x = colnames(data_generic[[job]]), pattern = "data_generic[[job]]$", replacement = "", fixed = T)
  colnames(data_generic[[job]]) <- gsub(x = colnames(data_generic[[job]]), pattern = "`", replacement = "", fixed = T)
  data_generic[[job]] <- data_generic[[job]] %>% select(job, title, code, fullCode, level)
  data_generic[[job]]$job <- jobs_converter$title[match(data_generic[[job]]$job, jobs_converter$urlSlug)]
  data_generic[[job]]$fullCode <- NULL ## delete "fullCode" to make it compatible with Competences and Soft skills where fullCode is missing
  
  data_soft[[job]] <- cbind(data_soft[[job]]$softSkill, data_soft[[job]]$id, data_soft[[job]]$level, data_soft[[job]]$job)
  colnames(data_soft[[job]])[which(names(data_soft[[job]]) == "id")] <- "mainId"
  colnames(data_soft[[job]]) <- gsub(x = colnames(data_soft[[job]]), pattern = "data_soft[[job]]$", replacement = "", fixed = T)
  colnames(data_soft[[job]]) <- gsub(x = colnames(data_soft[[job]]), pattern = "`", replacement = "", fixed = T)
  data_soft[[job]] <- data_soft[[job]] %>% select(job, title, code, level)
  data_soft[[job]]$job <- jobs_converter$title[match(data_soft[[job]]$job, jobs_converter$urlSlug)]
  
  ## Add all competences, generic skills and soft skills to each job to harmonize the data (create empty rows)
  data_competences[[job]] <- left_join(competences[,c("title", "code")], data_competences[[job]][,c("job", "title", "level")], by = "title")
  data_competences[[job]]$job <- job
  data_competences[[job]]$job <- jobs_converter$title[match(data_competences[[job]]$job, jobs_converter$urlSlug)]
  data_competences[[job]][is.na(data_competences[[job]])] <- 0
  
  data_generic[[job]] <- left_join(generic_skills[,c("title", "code")], data_generic[[job]][,c("job", "title", "level")], by = "title")
  data_generic[[job]]$job <- job
  data_generic[[job]]$job <- jobs_converter$title[match(data_generic[[job]]$job, jobs_converter$urlSlug)]
  data_generic[[job]][is.na(data_generic[[job]])] <- 0
  
  data_soft[[job]] <- left_join(soft_skills[,c("title", "code")], data_soft[[job]][,c("job", "title", "level")], by = "title")
  data_soft[[job]]$job <- job
  data_soft[[job]]$job <- jobs_converter$title[match(data_soft[[job]]$job, jobs_converter$urlSlug)]
  data_soft[[job]][is.na(data_soft[[job]])] <- 0
}

## Check if list length is identical
data_competences_length <- list()
for (job in jobs_nsp) {
  data_competences_length[[job]] <- nrow(data_competences[[job]])
}
data_competences_length <- as.data.frame(unlist(data_competences_length))
data_competences_length <- data_competences_length %>% filter_all(any_vars(.!=668))

data_generic_length <- list()
for (job in jobs_nsp) {
  data_generic_length[[job]] <- nrow(data_generic[[job]])
}
data_generic_length <- as.data.frame(unlist(data_generic_length))
data_generic_length <- data_generic_length %>% filter_all(any_vars(.!=8))

data_soft_length <- list()
for (job in jobs_nsp) {
  data_soft_length[[job]] <- nrow(data_soft[[job]])
}
data_soft_length <- as.data.frame(unlist(data_soft_length))
data_soft_length <- data_soft_length %>% filter_all(any_vars(.!=19))

## Define problematic jobs that do not match
# problematic_jobs <- unique(c(rownames(data_competences_length), rownames(data_generic_length), rownames(data_soft_length)))

## rbind jobs by each category
nsp_competences <- as.data.frame(data.table::rbindlist(data_competences))
nsp_competences$job.simple <- jobs_converter$urlSlug[match(nsp_competences$job, jobs_converter$title)]

nsp_generic <- as.data.frame(data.table::rbindlist(data_generic))
nsp_generic$job.simple <- jobs_converter$urlSlug[match(nsp_generic$job, jobs_converter$title)]

nsp_soft <- as.data.frame(data.table::rbindlist(data_soft))
nsp_soft$job.simple <- jobs_converter$urlSlug[match(nsp_soft$job, jobs_converter$title)]

## Export
fwrite(nsp_competences, file = paste0(cdkpath,"nsp_competences.csv"), sep=";", dec=",", bom = TRUE)
fwrite(nsp_generic, file = paste0(cdkpath,"nsp_generic.csv"), sep=";", dec=",", bom = TRUE)
fwrite(nsp_soft, file = paste0(cdkpath,"nsp_soft.csv"), sep=";", dec=",", bom = TRUE)

## Cast ("unmelt") to match values next to each other for each category
gc()

nsp_competences <- as.data.table(nsp_competences)
nsp_competences <- dcast.data.table(nsp_competences, title + code ~ job, value.var = "level", fun.aggregate = sum)

nsp_generic <- as.data.table(nsp_generic)
nsp_generic <- dcast.data.table(nsp_generic, title + code ~ job, value.var = "level")

nsp_soft <- as.data.table(nsp_soft)
nsp_soft <- dcast.data.table(nsp_soft, title + code ~ job, value.var = "level")

## Export casted
fwrite(nsp_competences, file = paste0(cdkpath,"nsp_competences_casted.csv"), sep=";", dec=",", bom = TRUE)
fwrite(nsp_generic, file = paste0(cdkpath,"nsp_generic_casted.csv"), sep=";", dec=",", bom = TRUE)
fwrite(nsp_soft, file = paste0(cdkpath,"nsp_soft_casted.csv"), sep=";", dec=",", bom = TRUE)


##########################################################################
# 1.5. Combine data for education (kkovs only) for all jobs

## Get information for all jobs (as urlSlug) together in a loop
res <- data <- data_kkovs <- list()

# job <- "advokat"

for (job in jobs_nsp_all) {
  print(job)
  
  res[[job]] = GET(paste0("http://nsp.cz/api/v1.2/workUnit/",job,"/education"), accept_json())
  data[[job]] = fromJSON(rawToChar(res[[job]]$content))
  data[[job]] <- data[[job]]$data
  
  data_kkovs[[job]] <- data[[job]]$kkovs
  
  data_kkovs[[job]]$job <- rep(job)
  
  ## Remove jobs with no information
  if(is.null(ncol(data_kkovs[[job]]))) {data_kkovs[[job]] <- NULL}
  
  ## Possibly also get rid of the jobs that do not have any information for the competences, generic skills and soft skills?
  
  ## Get rid of the problematic jobs
  # data_kkovs[names(data_kkovs) %in% problematic_jobs] <- NULL
}

## Remove res file to clean up
remove(res)

## Update the set of jobs after removing those with no information and the problematic jobs with extra duplications
jobs_education <- names(data_kkovs)

## Reduce kkovs to 2 digits only (more is too specific)
education_full <- education
education <- education_full[nchar(as.character(education_full$code)) == 2,]

## Harmonize data
# job <- "advokat"

for (job in jobs_education) {
  print(job)
  
  ## Harmonize columns
  data_kkovs[[job]] <- do.call("cbind", data_kkovs[[job]])
  data_kkovs[[job]] <- data_kkovs[[job]] %>% select(job, kkov.title, kkov.code, kkovSuitabilityLevel)
  colnames(data_kkovs[[job]])[colnames(data_kkovs[[job]]) == "kkov.title"] <- "title"
  colnames(data_kkovs[[job]])[colnames(data_kkovs[[job]]) == "kkov.code"] <- "code"
  data_kkovs[[job]]$code <- substr(data_kkovs[[job]]$code, 1, 2) ## leave only 2 digits category
  data_kkovs[[job]] <- data_kkovs[[job]] %>% drop_na()  ## delete rows with NAs in the code column
  data_kkovs[[job]] <- data_kkovs[[job]] %>%
    group_by(job, code) %>%
    select(job, code) %>%
    unique() ## cluster according to the 2 digits category (note - this leaves out the title and the suitability level, but the title is added later again and the suitability level is replaced by simply a 1 or 0 value (have or does not have education in the field))
  data_kkovs[[job]]$level <- 1 ## add "level" (1 or 0) to indicate if the job fits to this category (later zeros will be inserted everywhere else)
  data_kkovs[[job]]$job <- jobs_converter$title[match(data_kkovs[[job]]$job, jobs_converter$urlSlug)]
  
  ## Add all kkovs to each job to harmonize the data (create empty rows)
  data_kkovs[[job]] <- left_join(education[,c("title", "code")], data_kkovs[[job]][,c("job", "code", "level")], by = "code")
  data_kkovs[[job]]$job <- job
  data_kkovs[[job]]$job <- jobs_converter$title[match(data_kkovs[[job]]$job, jobs_converter$urlSlug)]
  data_kkovs[[job]][is.na(data_kkovs[[job]])] <- 0
}

## Check if list length is identical
data_kkovs_length <- list()
for (job in jobs_education) {
  data_kkovs_length[[job]] <- nrow(data_kkovs[[job]])
}
data_kkovs_length <- as.data.frame(unlist(data_kkovs_length))
data_kkovs_length <- data_kkovs_length %>% filter_all(any_vars(.!=48))

## rbind jobs by each category
kkovs_education <- as.data.frame(data.table::rbindlist(data_kkovs))
kkovs_education$job.simple <- jobs_converter$urlSlug[match(kkovs_education$job, jobs_converter$title)]

## Export
fwrite(kkovs_education, file = paste0(cdkpath,"kkovs_education.csv"), sep=";", dec=",", bom = TRUE)

## Cast ("unmelt") to match values next to each other for each category
gc()

kkovs_education <- as.data.table(kkovs_education)
kkovs_education <- dcast.data.table(kkovs_education, title + code ~ job, value.var = "level", fun.aggregate = sum)

## Export casted
fwrite(kkovs_education, file = paste0(cdkpath,"kkovs_education_casted.csv"), sep=";", dec=",", bom = TRUE)



##########################################################################
# 2. Load the selected jobs at risk (workunits) and the jobs preferred by the employees from the survey
##########################################################################

## Encoding csv in UTF-8: https://stackoverflow.com/questions/18693139/how-to-convert-csv-files-encoding-to-utf-8

##########################################################################
# 2.0. Load adapted CDK files (only casted in case casted exist)

# competences <- fread(paste0(cdkpath,"competences.csv"), header=TRUE, sep=";", dec=",")
# education <- fread(paste0(cdkpath,"education.csv"), header=TRUE, sep=";", dec=",")
# generic_skills <- fread(paste0(cdkpath,"generic_skills.csv"), header=TRUE, sep=";", dec=",")
# isco <- fread(paste0(cdkpath,"isco.csv"), header=TRUE, sep=";", dec=",")
# isco_categories <- fread(paste0(cdkpath,"isco_categories_casted.csv"), header=TRUE, sep=";", dec=",")
# kkovs_education <- fread(paste0(cdkpath,"kkovs_education_casted.csv"), header=TRUE, sep=";", dec=",")
# nsp_competences <- fread(paste0(cdkpath,"nsp_competences_casted.csv"), header=TRUE, sep=";", dec=",")
# nsp_generic <- fread(paste0(cdkpath,"nsp_generic_casted.csv"), header=TRUE, sep=";", dec=",")
# nsp_soft <- fread(paste0(cdkpath,"nsp_soft_casted.csv"), header=TRUE, sep=";", dec=",")
# soft_skills <- fread(paste0(cdkpath,"soft_skills.csv"), header=TRUE, sep=";", dec=",")
# workunits <- fread(paste0(cdkpath,"workunits.csv"), header=TRUE, sep=";", dec=",")


##########################################################################
# 2.1. Load the jobs preferred by the employees from the survey and adapt

## Load the jobs preferred by the employees
preferences <- fread(paste0(cdkpath,"preferences.csv"), header=TRUE, sep=";", dec=",", encoding = "UTF-8")

## Adapt
colnames(preferences) <- paste0(colnames(preferences),"_",preferences[1,])
preferences <- preferences[!1]
colnames(preferences)[c(1:2)] <- c("sector.title","position.title")
preferences <- as.data.frame(preferences)
rownames(preferences) <- paste0(preferences[,"sector.title"],"_",preferences[,"position.title"])


##########################################################################
# 2.2. Load the selected jobs at risk from workunits and create a selection of jobs at risk

## Load workunits_selection and adjust
workunits_selection <- fread(paste0(cdkpath,"workunits_selection.csv"), header=TRUE, sep=";", dec=",", encoding = "UTF-8")
workunits_selection[,c("ohrozeni","V16")] <- NULL
workunits_selection$nspCode[workunits_selection$nspCode==""] <- NA

## Define jobs at risk and jobs with no risk
jobsatrisk <- c("title","code",workunits_selection$title)
jobsnorisk <- c("title","code",workunits$title[!workunits$title %in% jobsatrisk])

## Create workunits_nonselection to proceed with the other jobs
workunits_nonselection <- workunits[!workunits$title %in% jobsatrisk,]

## See overlaps between jobsatrisk and jobsnorisk
job_overlaps <- match(jobsatrisk,jobsnorisk)
jobsatrisk[!is.na(job_overlaps)]
jobsnorisk[job_overlaps[!is.na(job_overlaps)]]



##########################################################################
# 3. Run the job proximity analysis
##########################################################################

##########################################################################
# 3.1. Adjust the analyzed datasets (kkovs_education, nsp_competences, nsp_generic, nsp_soft) for jobs at risk

## Extract only jobs at risk, melt and add other characteristics for each job at risk from workunits_selection
kkovs_education_names_risk <- names(kkovs_education)[(names(kkovs_education) %in% jobsatrisk)]
kkovs_education_risk <- kkovs_education[,kkovs_education_names_risk,with = FALSE]
kkovs_education_risk <- melt(kkovs_education_risk, id.vars = c("title","code"), variable.name = "job", value.name = "value")
kkovs_education_risk <- merge(kkovs_education_risk, workunits_selection, by.x = "job", by.y = "title")
colnames(kkovs_education_risk)[colnames(kkovs_education_risk) == "code.x"] <- "kkovs.code"
colnames(kkovs_education_risk)[colnames(kkovs_education_risk) == "code.y"] <- "job.code"
colnames(kkovs_education_risk)[colnames(kkovs_education_risk) == "title"] <- "kkovs.title"
colnames(kkovs_education_risk)[colnames(kkovs_education_risk) == "job"] <- "job.title"
kkovs_education_risk$legacyId <- kkovs_education_risk$nspCode <- kkovs_education_risk[,10:17] <- NULL
kkovs_education_risk <- setcolorder(kkovs_education_risk, c("job.code","urlSlug","job.title","nace","kkovs.code","kkovs.title","value"))

nsp_competences_names_risk <- names(nsp_competences)[(names(nsp_competences) %in% jobsatrisk)]
nsp_competences_risk <- nsp_competences[,nsp_competences_names_risk,with = FALSE]
nsp_competences_risk <- melt(nsp_competences_risk, id.vars = c("title","code"), variable.name = "job", value.name = "value")
nsp_competences_risk <- merge(nsp_competences_risk, workunits_selection, by.x = "job", by.y = "title")
colnames(nsp_competences_risk)[colnames(nsp_competences_risk) == "code.x"] <- "nsp.competence.code"
colnames(nsp_competences_risk)[colnames(nsp_competences_risk) == "code.y"] <- "job.code"
colnames(nsp_competences_risk)[colnames(nsp_competences_risk) == "title"] <- "nsp.competence.title"
colnames(nsp_competences_risk)[colnames(nsp_competences_risk) == "job"] <- "job.title"
nsp_competences_risk$legacyId <- nsp_competences_risk$nspCode <- nsp_competences_risk[,10:17] <- NULL
nsp_competences_risk <- setcolorder(nsp_competences_risk, c("job.code","urlSlug","job.title","nace","nsp.competence.code","nsp.competence.title","value"))

nsp_generic_names_risk <- names(nsp_generic)[(names(nsp_generic) %in% jobsatrisk)]
nsp_generic_risk <- nsp_generic[,nsp_generic_names_risk,with = FALSE]
nsp_generic_risk <- melt(nsp_generic_risk, id.vars = c("title","code"), variable.name = "job", value.name = "value")
nsp_generic_risk <- merge(nsp_generic_risk, workunits_selection, by.x = "job", by.y = "title")
colnames(nsp_generic_risk)[colnames(nsp_generic_risk) == "code.x"] <- "nsp.generic.code"
colnames(nsp_generic_risk)[colnames(nsp_generic_risk) == "code.y"] <- "job.code"
colnames(nsp_generic_risk)[colnames(nsp_generic_risk) == "title"] <- "nsp.generic.title"
colnames(nsp_generic_risk)[colnames(nsp_generic_risk) == "job"] <- "job.title"
nsp_generic_risk$legacyId <- nsp_generic_risk$nspCode <- nsp_generic_risk[,10:17] <- NULL
nsp_generic_risk <- setcolorder(nsp_generic_risk, c("job.code","urlSlug","job.title","nace","nsp.generic.code","nsp.generic.title","value"))

nsp_soft_names_risk <- names(nsp_soft)[(names(nsp_soft) %in% jobsatrisk)]
nsp_soft_risk <- nsp_soft[,nsp_soft_names_risk,with = FALSE]
nsp_soft_risk <- melt(nsp_soft_risk, id.vars = c("title","code"), variable.name = "job", value.name = "value")
nsp_soft_risk <- merge(nsp_soft_risk, workunits_selection, by.x = "job", by.y = "title")
colnames(nsp_soft_risk)[colnames(nsp_soft_risk) == "code.x"] <- "nsp.soft.code"
colnames(nsp_soft_risk)[colnames(nsp_soft_risk) == "code.y"] <- "job.code"
colnames(nsp_soft_risk)[colnames(nsp_soft_risk) == "title"] <- "nsp.soft.title"
colnames(nsp_soft_risk)[colnames(nsp_soft_risk) == "job"] <- "job.title"
nsp_soft_risk$legacyId <- nsp_soft_risk$nspCode <- nsp_soft_risk[,10:17] <- NULL
nsp_soft_risk <- setcolorder(nsp_soft_risk, c("job.code","urlSlug","job.title","nace","nsp.soft.code","nsp.soft.title","value"))

## Split the jobs at risk by each job
kkovs_education_risk <- split(kkovs_education_risk, f = kkovs_education_risk$urlSlug)
nsp_competences_risk <- split(nsp_competences_risk, f = nsp_competences_risk$urlSlug)
nsp_generic_risk <- split(nsp_generic_risk, f = nsp_generic_risk$urlSlug)
nsp_soft_risk <- split(nsp_soft_risk, f = nsp_soft_risk$urlSlug)

kkovs_education_risk <- do.call(cbind.data.frame, kkovs_education_risk)
nsp_competences_risk <- do.call(cbind.data.frame, nsp_competences_risk)
nsp_generic_risk <- do.call(cbind.data.frame, nsp_generic_risk)
nsp_soft_risk <- do.call(cbind.data.frame, nsp_soft_risk)

## Make separate dataframe for each job at risk and category
kkovs_education_risk_jobs <- nsp_competences_risk_jobs <- nsp_generic_risk_jobs <- nsp_soft_risk_jobs <- list()

jobs_risk <- workunits_selection$urlSlug
jobs_education_risk <- jobs_risk[which(jobs_risk %in% jobs_education)]
jobs_nsp_risk <- jobs_risk[which(jobs_risk %in% jobs_nsp)]

## Note - startsWith (as well as grepl) merge some jobs together, it is necessary to manually remove the doubled jobs from there
for (job in jobs_education_risk) {
  kkovs_education_risk_jobs[[job]] <- kkovs_education_risk[,startsWith(colnames(kkovs_education_risk),job)]
  kkovs_education_risk_jobs[[job]] <- kkovs_education_risk_jobs[[job]][,1:7]
}
for (job in jobs_nsp_risk) {
  nsp_competences_risk_jobs[[job]] <- nsp_competences_risk[,startsWith(colnames(nsp_competences_risk),job)]
  nsp_generic_risk_jobs[[job]] <- nsp_generic_risk[,startsWith(colnames(nsp_generic_risk),job)]
  nsp_soft_risk_jobs[[job]] <- nsp_soft_risk[,startsWith(colnames(nsp_soft_risk),job)]
  nsp_competences_risk_jobs[[job]] <- nsp_competences_risk_jobs[[job]][,1:7]
  nsp_generic_risk_jobs[[job]] <- nsp_generic_risk_jobs[[job]][,1:7]
  nsp_soft_risk_jobs[[job]] <- nsp_soft_risk_jobs[[job]][,1:7]
}


##########################################################################
# 3.2. Adjust the analyzed datasets (kkovs_education, nsp_competences, nsp_generic, nsp_soft) for other jobs

## Extract only other jobs, melt and add other characteristics for each job at risk from workunits_selection
kkovs_education_names_norisk <- names(kkovs_education)[(names(kkovs_education) %in% jobsnorisk)]
kkovs_education_norisk <- kkovs_education[,kkovs_education_names_norisk,with = FALSE]
kkovs_education_norisk <- melt(kkovs_education_norisk, id.vars = c("title","code"), variable.name = "job", value.name = "value")
kkovs_education_norisk <- merge(kkovs_education_norisk, workunits_nonselection, by.x = "job", by.y = "title")
colnames(kkovs_education_norisk)[colnames(kkovs_education_norisk) == "code.x"] <- "kkovs.code"
colnames(kkovs_education_norisk)[colnames(kkovs_education_norisk) == "code.y"] <- "job.code"
colnames(kkovs_education_norisk)[colnames(kkovs_education_norisk) == "title"] <- "kkovs.title"
colnames(kkovs_education_norisk)[colnames(kkovs_education_norisk) == "job"] <- "job.title"
kkovs_education_norisk$legacyId <- kkovs_education_norisk$nspCode <- kkovs_education_norisk[,9:17] <- NULL
kkovs_education_norisk <- setcolorder(kkovs_education_norisk, c("job.code","urlSlug","job.title","kkovs.code","kkovs.title","value"))

nsp_competences_names_norisk <- names(nsp_competences)[(names(nsp_competences) %in% jobsnorisk)]
nsp_competences_norisk <- nsp_competences[,nsp_competences_names_norisk,with = FALSE]
nsp_competences_norisk <- melt(nsp_competences_norisk, id.vars = c("title","code"), variable.name = "job", value.name = "value")
nsp_competences_norisk <- merge(nsp_competences_norisk, workunits_nonselection, by.x = "job", by.y = "title")
colnames(nsp_competences_norisk)[colnames(nsp_competences_norisk) == "code.x"] <- "nsp.competence.code"
colnames(nsp_competences_norisk)[colnames(nsp_competences_norisk) == "code.y"] <- "job.code"
colnames(nsp_competences_norisk)[colnames(nsp_competences_norisk) == "title"] <- "nsp.competence.title"
colnames(nsp_competences_norisk)[colnames(nsp_competences_norisk) == "job"] <- "job.title"
nsp_competences_norisk$legacyId <- nsp_competences_norisk$nspCode <- nsp_competences_norisk[,9:17] <- NULL
nsp_competences_norisk <- setcolorder(nsp_competences_norisk, c("job.code","urlSlug","job.title","nsp.competence.code","nsp.competence.title","value"))

nsp_generic_names_norisk <- names(nsp_generic)[(names(nsp_generic) %in% jobsnorisk)]
nsp_generic_norisk <- nsp_generic[,nsp_generic_names_norisk,with = FALSE]
nsp_generic_norisk <- melt(nsp_generic_norisk, id.vars = c("title","code"), variable.name = "job", value.name = "value")
nsp_generic_norisk <- merge(nsp_generic_norisk, workunits_nonselection, by.x = "job", by.y = "title")
colnames(nsp_generic_norisk)[colnames(nsp_generic_norisk) == "code.x"] <- "nsp.generic.code"
colnames(nsp_generic_norisk)[colnames(nsp_generic_norisk) == "code.y"] <- "job.code"
colnames(nsp_generic_norisk)[colnames(nsp_generic_norisk) == "title"] <- "nsp.generic.title"
colnames(nsp_generic_norisk)[colnames(nsp_generic_norisk) == "job"] <- "job.title"
nsp_generic_norisk$legacyId <- nsp_generic_norisk$nspCode <- nsp_generic_norisk[,9:17] <- NULL
nsp_generic_norisk <- setcolorder(nsp_generic_norisk, c("job.code","urlSlug","job.title","nsp.generic.code","nsp.generic.title","value"))

nsp_soft_names_norisk <- names(nsp_soft)[(names(nsp_soft) %in% jobsnorisk)]
nsp_soft_norisk <- nsp_soft[,nsp_soft_names_norisk,with = FALSE]
nsp_soft_norisk <- melt(nsp_soft_norisk, id.vars = c("title","code"), variable.name = "job", value.name = "value")
nsp_soft_norisk <- merge(nsp_soft_norisk, workunits_nonselection, by.x = "job", by.y = "title")
colnames(nsp_soft_norisk)[colnames(nsp_soft_norisk) == "code.x"] <- "nsp.soft.code"
colnames(nsp_soft_norisk)[colnames(nsp_soft_norisk) == "code.y"] <- "job.code"
colnames(nsp_soft_norisk)[colnames(nsp_soft_norisk) == "title"] <- "nsp.soft.title"
colnames(nsp_soft_norisk)[colnames(nsp_soft_norisk) == "job"] <- "job.title"
nsp_soft_norisk$legacyId <- nsp_soft_norisk$nspCode <- nsp_soft_norisk[,9:17] <- NULL
nsp_soft_norisk <- setcolorder(nsp_soft_norisk, c("job.code","urlSlug","job.title","nsp.soft.code","nsp.soft.title","value"))

## Split the other jobs by each job
kkovs_education_norisk <- split(kkovs_education_norisk, f = kkovs_education_norisk$urlSlug)
nsp_competences_norisk <- split(nsp_competences_norisk, f = nsp_competences_norisk$urlSlug)
nsp_generic_norisk <- split(nsp_generic_norisk, f = nsp_generic_norisk$urlSlug)
nsp_soft_norisk <- split(nsp_soft_norisk, f = nsp_soft_norisk$urlSlug)

kkovs_education_norisk <- do.call(cbind.data.frame, kkovs_education_norisk)
nsp_competences_norisk <- do.call(cbind.data.frame, nsp_competences_norisk)
nsp_generic_norisk <- do.call(cbind.data.frame, nsp_generic_norisk)
nsp_soft_norisk <- do.call(cbind.data.frame, nsp_soft_norisk)


##########################################################################
# 3.3. Join (cbind) the jobs at risk with the other jobs (as additional columns)

kkovs_education_jobs <- nsp_competences_jobs <- nsp_generic_jobs <- nsp_soft_jobs <- list()

for (job in jobs_education_risk) {
  kkovs_education_jobs[[job]] <- cbind(kkovs_education_risk_jobs[[job]],kkovs_education_norisk[,endsWith(colnames(kkovs_education_norisk),"value")])
}
for (job in jobs_nsp_risk) {
  nsp_competences_jobs[[job]] <- cbind(nsp_competences_risk_jobs[[job]],nsp_competences_norisk[,endsWith(colnames(nsp_competences_norisk),"value")])
  nsp_generic_jobs[[job]] <- cbind(nsp_generic_risk_jobs[[job]],nsp_generic_norisk[,endsWith(colnames(nsp_generic_norisk),"value")])
  nsp_soft_jobs[[job]] <- cbind(nsp_soft_risk_jobs[[job]],nsp_soft_norisk[,endsWith(colnames(nsp_soft_norisk),"value")])
}


##########################################################################
# 3.4. Split preferences by each job at risk (so that we get similar structure to the other datasets)

jobs_pref_risk <- rownames(preferences)
jobs_pref_norisk <- colnames(preferences[-c(1:2)])

preferences_jobs <- list()
preferences_jobs <- split(preferences, f = rownames(preferences))
for (job in jobs_pref_risk) {
  colnames(preferences_jobs[[job]])[-c(1:2)] <- paste0(colnames(preferences_jobs[[job]])[-c(1:2)],".value")
  preferences_jobs[[job]][-c(1:2)] <- as.numeric(preferences_jobs[[job]][-c(1:2)])
  # preferences_jobs[[job]][,paste0(job,".value")] <- 127 ## 127 = number of respondents of the survey
  # preferences_jobs[[job]] <- preferences_jobs[[job]] %>% relocate(paste0(job,".value"), .after = "position.title")
}



##########################################################################
# 4. Calculate proximity (derivations) of the values for jobs at risk with other jobs
##########################################################################

## Proximity = distance/deviation of the value for each category.
## Separately for kkovs_education, nsp_competences, nsp_generic, nsp_soft, preferences.
## Each job has a specific combination of values of the elements (competences etc.) in each of the categories
## For each, we calculate the distance from those of the 75 jobs at risk concerned,
## and then sum up to get one number for each job (deviation) for kkovs_education, nsp_competences, nsp_generic, nsp_soft.
## The lower the number (distance/deviation) is, the closer the qualification requirements to the worker are,
## and therefore the more suitable (or low-cost in terms of retraining) that job is as a replacement to the original job at risk.


##########################################################################
# 4.1. Calculate the combination of the weighted values for each job by Element combination

## For each job and element I have to calculate the difference from the value for the jobs at risk (for each job at risk separately).
## "value.dif.job" column is the difference between the value for the respective job at risk and other jobs
## (see the column name for the code of the job)

## Note - different logic is applied to the preferences, where the distance denotes how much is the other job preferred.
## Therefore, we stay with the ".value" columns as they indicate how many respondents would prefer the given retraining option.

jobs_norisk <- workunits_nonselection$urlSlug
jobs_education_norisk <- jobs_norisk[which(jobs_norisk %in% jobs_education)]
jobs_nsp_norisk <- jobs_norisk[which(jobs_norisk %in% jobs_nsp)]

for (job in jobs_education_risk) {
  print(job)
  for (code in jobs_education_norisk) {
    print(code)
    kkovs_education_jobs[[job]][,paste0("distance.",code)] <- kkovs_education_jobs[[job]][,paste0(job,".value")] - kkovs_education_jobs[[job]][,paste0(code,".value")]
  }
}
for (job in jobs_nsp_risk) {
  print(job)
  for (code in jobs_nsp_norisk) {
    print(code)
    nsp_competences_jobs[[job]][,paste0("distance.",code)] <- nsp_competences_jobs[[job]][,paste0(job,".value")] - nsp_competences_jobs[[job]][,paste0(code,".value")]
    nsp_generic_jobs[[job]][,paste0("distance.",code)] <- nsp_generic_jobs[[job]][,paste0(job,".value")] - nsp_generic_jobs[[job]][,paste0(code,".value")]
    nsp_soft_jobs[[job]][,paste0("distance.",code)] <- nsp_soft_jobs[[job]][,paste0(job,".value")] - nsp_soft_jobs[[job]][,paste0(code,".value")]
  }
}
# for (job in jobs_pref_risk) {
#   print(job)
#   for (pref in jobs_pref_norisk) {
#     print(pref)
#     preferences_jobs[[job]][,paste0("distance.",pref)] <- preferences_jobs[[job]][,paste0(job,".value")] - preferences_jobs[[job]][,paste0(pref,".value")]
#   }
# }

## Delete the value columns, including the one for the actual job at risk
## Not done for the preferences dataset (see above)
for (job in jobs_education_risk) {
  print(job)
  kkovs_education_jobs[[job]][,endsWith(colnames(kkovs_education_jobs[[job]]),".value")] <- NULL
}
for (job in jobs_nsp_risk) {
  print(job)
  nsp_competences_jobs[[job]][,endsWith(colnames(nsp_competences_jobs[[job]]),".value")] <- NULL
  nsp_generic_jobs[[job]][,endsWith(colnames(nsp_generic_jobs[[job]]),".value")] <- NULL
  nsp_soft_jobs[[job]][,endsWith(colnames(nsp_soft_jobs[[job]]),".value")] <- NULL
}
# for (job in jobs_pref_risk) {
#   print(job)
#   preferences_jobs[[job]][,endsWith(colnames(preferences_jobs[[job]]),".value")] <- NULL
# }

## Create transposed version for further processing
kkovs_education_jobs_t <- list()
nsp_competences_jobs_t <- list()
nsp_generic_jobs_t <- list()
nsp_soft_jobs_t <- list()
preferences_jobs_t <- list()

for (job in jobs_education_risk) {
  print(job)
  kkovs_education_jobs_t[[job]] <- kkovs_education_jobs[[job]]
  rownames(kkovs_education_jobs_t[[job]]) <- paste0(kkovs_education_jobs_t[[job]][,3],"_",kkovs_education_jobs_t[[job]][,6])
  kkovs_education_jobs_t[[job]] <- kkovs_education_jobs_t[[job]][,c(7:2413)]
  kkovs_education_jobs_t[[job]] <- as.data.frame(t(kkovs_education_jobs_t[[job]]))
}
for (job in jobs_nsp_risk) {
  print(job)
  
  nsp_competences_jobs_t[[job]] <- nsp_competences_jobs[[job]]
  rownames(nsp_competences_jobs_t[[job]]) <- paste0(nsp_competences_jobs_t[[job]][,3],"_",nsp_competences_jobs_t[[job]][,6])
  nsp_competences_jobs_t[[job]] <- nsp_competences_jobs_t[[job]][,c(7:2001)]
  nsp_competences_jobs_t[[job]] <- as.data.frame(t(nsp_competences_jobs_t[[job]]))
  
  nsp_generic_jobs_t[[job]] <- nsp_generic_jobs[[job]]
  rownames(nsp_generic_jobs_t[[job]]) <- paste0(nsp_generic_jobs_t[[job]][,3],"_",nsp_generic_jobs_t[[job]][,6])
  nsp_generic_jobs_t[[job]] <- nsp_generic_jobs_t[[job]][,c(7:2001)]
  nsp_generic_jobs_t[[job]] <- as.data.frame(t(nsp_generic_jobs_t[[job]]))
  
  nsp_soft_jobs_t[[job]] <- nsp_soft_jobs[[job]]
  rownames(nsp_soft_jobs_t[[job]]) <- paste0(nsp_soft_jobs_t[[job]][,3],"_",nsp_soft_jobs_t[[job]][,6])
  nsp_soft_jobs_t[[job]] <- nsp_soft_jobs_t[[job]][,c(7:2001)]
  nsp_soft_jobs_t[[job]] <- as.data.frame(t(nsp_soft_jobs_t[[job]]))
}
for (job in jobs_pref_risk) {
  print(job)
  preferences_jobs_t[[job]] <- preferences_jobs[[job]]
  preferences_jobs_t[[job]] <- preferences_jobs_t[[job]][,c(3:266)]
  preferences_jobs_t[[job]] <- as.data.frame(t(preferences_jobs_t[[job]]))
}


##########################################################################
# 4.2. Calculate distance for each of the other jobs (not at risk) across all elements in each category with RMSD (Root-Mean-Square Deviation)

## We need to calculate the closest jobs with the minimal distance (difference in value) across all the qualifications/elements in each category
## Calculate the squares (RMSD):
## 1) Calculate the square values for each value
## 2) Sum the square values for each job (row-wise in the _t dataframes, i.e. across columns)
## 3) Divide the sums by the number of columns (to get the average square distance from the "0" value, i.e. the respective job at risk)

## Not done for the preferences dataset (see above) - we only create the "rank" object here (instead of "rmsd")

## Create the dataframes and rename columns
kkovs_education_jobs_rmsd <- list()
nsp_competences_jobs_rmsd <- list()
nsp_generic_jobs_rmsd <- list()
nsp_soft_jobs_rmsd <- list()
preferences_jobs_rank <- list() # Different name because of different approach

for (job in jobs_education_risk) {
  print(job)
  kkovs_education_jobs_rmsd[[job]] <- kkovs_education_jobs_t[[job]]
  colnames(kkovs_education_jobs_rmsd[[job]]) <- c(paste0("Distance_",colnames(kkovs_education_jobs_t[[job]])))
}
for (job in jobs_nsp_risk) {
  print(job)
  nsp_competences_jobs_rmsd[[job]] <- nsp_competences_jobs_t[[job]]
  nsp_generic_jobs_rmsd[[job]] <- nsp_generic_jobs_t[[job]]
  nsp_soft_jobs_rmsd[[job]] <- nsp_soft_jobs_t[[job]]
  colnames(nsp_competences_jobs_rmsd[[job]]) <- c(paste0("Distance_",colnames(nsp_competences_jobs_t[[job]])))
  colnames(nsp_generic_jobs_rmsd[[job]]) <- c(paste0("Distance_",colnames(nsp_generic_jobs_t[[job]])))
  colnames(nsp_soft_jobs_rmsd[[job]]) <- c(paste0("Distance_",colnames(nsp_soft_jobs_t[[job]])))
}
for (job in jobs_pref_risk) {
  print(job)
  preferences_jobs_rank[[job]] <- preferences_jobs_t[[job]]
  colnames(preferences_jobs_rank[[job]]) <- c(paste0("Value_",colnames(preferences_jobs_t[[job]])))
}

## Calculate square values (RMSD)
## Not done for the preferences dataset (see above)
for (job in jobs_education_risk) {
  print(job)
  kkovs_education_jobs_rmsd[[job]] <- kkovs_education_jobs_rmsd[[job]] ^ 2
}
for (job in jobs_nsp_risk) {
  print(job)
  nsp_competences_jobs_rmsd[[job]] <- nsp_competences_jobs_rmsd[[job]] ^ 2
  nsp_generic_jobs_rmsd[[job]] <- nsp_generic_jobs_rmsd[[job]] ^ 2
  nsp_soft_jobs_rmsd[[job]] <- nsp_soft_jobs_rmsd[[job]] ^ 2
}

## Calculate rowsums (of the rmsd, resp. of the values for the preferences) for each job
## Divide them by the number of columns (to get average)
## Rank the dataframe according to the rowsums
## The larger the rowsum for rmsd is, the worse (see above - the greater the distance/fit is), therefore we rank in an ascending order
## The calculation has to be done for each category separately, since there are different numbers of jobs (and elements) in each

## The rowsum for the preferences gives simply the sum of respondents who prefer this retraining option
## The larger the rowsum for the preferences is, the better (opposite to the logic of the rmsd calculation)
## Therefore we apply reversed logic to the preferences dataset and rank it in descending order

jobs_education_norisk_names <- jobs_education_names[which(jobs_education_names %in% jobsnorisk)]
jobs_nsp_norisk_names <- jobs_nsp_names[which(jobs_nsp_names %in% jobsnorisk)]

## Change non-unique jobs_education_norisk_names - (Zahradnický specialista is there twice, while the urlSlug is different, using the urlSlug ending to give a unique name)
# education_norisk_names_occur <- data.frame(table(jobs_education_norisk_names))
# education_norisk_names_occur[education_norisk_names_occur$Freq > 1,]
# 
# education_norisk_occur <- data.frame(table(jobs_education_norisk))
# education_norisk_occur[education_norisk_occur$Freq > 1,]
# 
# education_norisk_comp <- data.frame(jobs_education_norisk,jobs_education_norisk_names)

jobs_education_norisk_names[2376] <- "Zahradnický specialista DD1D"

for (job in jobs_education_risk) {
  print(job)
  kkovs_education_jobs_rmsd[[job]] <- kkovs_education_jobs_rmsd[[job]]
  kkovs_education_jobs_rmsd[[job]]$AverageRMSD <- rowSums(kkovs_education_jobs_rmsd[[job]]) / ncol(kkovs_education_jobs_rmsd[[job]]) ## -1 because of the Rowsum column
  kkovs_education_jobs_rmsd[[job]]$Rank <- NA
  kkovs_education_jobs_rmsd[[job]][,"Rank"][order(kkovs_education_jobs_rmsd[[job]][,"AverageRMSD"])] <- 1:nrow(kkovs_education_jobs_rmsd[[job]])
  rownames(kkovs_education_jobs_rmsd[[job]]) <- jobs_education_norisk_names
  kkovs_education_jobs_rmsd[[job]]$RepJobNames <- jobs_education_norisk_names
  kkovs_education_jobs_rmsd[[job]] <- kkovs_education_jobs_rmsd[[job]][order(kkovs_education_jobs_rmsd[[job]][,"Rank"]),]
  kkovs_education_jobs_rmsd[[job]] <- kkovs_education_jobs_rmsd[[job]][,c(51,50,49,1:48)]
}

for (job in jobs_nsp_risk) {
  print(job)
  
  nsp_competences_jobs_rmsd[[job]] <- nsp_competences_jobs_rmsd[[job]]
  nsp_competences_jobs_rmsd[[job]]$AverageRMSD <- rowSums(nsp_competences_jobs_rmsd[[job]]) / ncol(nsp_competences_jobs_rmsd[[job]]) ## -1 because of the Rowsum column
  nsp_competences_jobs_rmsd[[job]]$Rank <- NA
  nsp_competences_jobs_rmsd[[job]][,"Rank"][order(nsp_competences_jobs_rmsd[[job]][,"AverageRMSD"])] <- 1:nrow(nsp_competences_jobs_rmsd[[job]])
  rownames(nsp_competences_jobs_rmsd[[job]]) <- jobs_nsp_norisk_names
  nsp_competences_jobs_rmsd[[job]]$RepJobNames <- jobs_nsp_norisk_names
  nsp_competences_jobs_rmsd[[job]] <- nsp_competences_jobs_rmsd[[job]][order(nsp_competences_jobs_rmsd[[job]][,"Rank"]),]
  nsp_competences_jobs_rmsd[[job]] <- nsp_competences_jobs_rmsd[[job]][,c(670,669,668,1:667)]
  
  nsp_generic_jobs_rmsd[[job]] <- nsp_generic_jobs_rmsd[[job]]
  nsp_generic_jobs_rmsd[[job]]$AverageRMSD <- rowSums(nsp_generic_jobs_rmsd[[job]]) / ncol(nsp_generic_jobs_rmsd[[job]]) ## -1 because of the Rowsum column
  nsp_generic_jobs_rmsd[[job]]$Rank <- NA
  nsp_generic_jobs_rmsd[[job]][,"Rank"][order(nsp_generic_jobs_rmsd[[job]][,"AverageRMSD"])] <- 1:nrow(nsp_generic_jobs_rmsd[[job]])
  rownames(nsp_generic_jobs_rmsd[[job]]) <- jobs_nsp_norisk_names
  nsp_generic_jobs_rmsd[[job]]$RepJobNames <- jobs_nsp_norisk_names
  nsp_generic_jobs_rmsd[[job]] <- nsp_generic_jobs_rmsd[[job]][order(nsp_generic_jobs_rmsd[[job]][,"Rank"]),]
  nsp_generic_jobs_rmsd[[job]] <- nsp_generic_jobs_rmsd[[job]][,c(11,10,9,1:8)]
  
  nsp_soft_jobs_rmsd[[job]] <- nsp_soft_jobs_rmsd[[job]]
  nsp_soft_jobs_rmsd[[job]]$AverageRMSD <- rowSums(nsp_soft_jobs_rmsd[[job]]) / ncol(nsp_soft_jobs_rmsd[[job]]) ## -1 because of the Rowsum column
  nsp_soft_jobs_rmsd[[job]]$Rank <- NA
  nsp_soft_jobs_rmsd[[job]][,"Rank"][order(nsp_soft_jobs_rmsd[[job]][,"AverageRMSD"])] <- 1:nrow(nsp_soft_jobs_rmsd[[job]])
  rownames(nsp_soft_jobs_rmsd[[job]]) <- jobs_nsp_norisk_names
  nsp_soft_jobs_rmsd[[job]]$RepJobNames <- jobs_nsp_norisk_names
  nsp_soft_jobs_rmsd[[job]] <- nsp_soft_jobs_rmsd[[job]][order(nsp_soft_jobs_rmsd[[job]][,"Rank"]),]
  nsp_soft_jobs_rmsd[[job]] <- nsp_soft_jobs_rmsd[[job]][,c(22,21,20,1:19)]
}

for (job in jobs_pref_risk) {
  print(job)
  preferences_jobs_rank[[job]] <- preferences_jobs_rank[[job]]
  preferences_jobs_rank[[job]]$Sum <- rowSums(preferences_jobs_rank[[job]]) / colSums(preferences_jobs_rank[[job]]) # calculate shares for each preferred retraining option (percentage values)
  preferences_jobs_rank[[job]]$Sum[is.nan(preferences_jobs_rank[[job]]$Sum)] <- 0 # remove NaNs
  preferences_jobs_rank[[job]]$Rank <- NA
  preferences_jobs_rank[[job]][,"Rank"][order(preferences_jobs_rank[[job]][,"Sum"], decreasing = TRUE)] <- 1:nrow(preferences_jobs_rank[[job]])
  rownames(preferences_jobs_rank[[job]]) <- jobs_pref_norisk
  preferences_jobs_rank[[job]]$RepJobNames <- jobs_pref_norisk
  preferences_jobs_rank[[job]] <- preferences_jobs_rank[[job]][order(preferences_jobs_rank[[job]][,"Rank"]),]
  preferences_jobs_rank[[job]] <- preferences_jobs_rank[[job]][,c(4,3,2,1)]
}

## Export full results
for (job in jobs_education_risk) {
  fwrite(kkovs_education_jobs_rmsd[[job]], file = paste0(respath,"coal_qualifications_cz_kkovs_education_rmsd_",job,".csv"), sep=";", dec=",", row.names=TRUE)
}
for (job in jobs_nsp_risk) {
  fwrite(nsp_competences_jobs_rmsd[[job]], file = paste0(respath,"coal_qualifications_cz_nsp_competences_rmsd_",job,".csv"), sep=";", dec=",", row.names=TRUE)
  fwrite(nsp_generic_jobs_rmsd[[job]], file = paste0(respath,"coal_qualifications_cz_nsp_generic_rmsd_",job,".csv"), sep=";", dec=",", row.names=TRUE)
  fwrite(nsp_soft_jobs_rmsd[[job]], file = paste0(respath,"coal_qualifications_cz_nsp_soft_rmsd_",job,".csv"), sep=";", dec=",", row.names=TRUE)
}
for (job in jobs_pref_risk) {
  fwrite(preferences_jobs_rank[[job]], file = paste0(respath,"coal_qualifications_cz_preferences_rank_",job,".csv"), sep=";", dec=",", row.names=TRUE)
}



##########################################################################
## 5. Create CDK workuits x directions to NACE x ISCO converter
##########################################################################

##########################################################################
# 5.1. Adapt data_isco to isco_converter, add workunits directions

## rbind jobs from data_isco
isco_converter <- as.data.frame(data.table::rbindlist(data_isco))
isco_converter$job.simple <- jobs_converter$urlSlug[match(isco_converter$job, jobs_converter$title)]

## Delete rows with zeros and rename columns
isco_converter <- isco_converter %>% filter(!if_any(starts_with("level"), ~ . == 0))
isco_converter_names <- c(isco.title = "title", isco.code = "code", workunit = "job")
isco_converter <- rename(isco_converter, all_of(isco_converter_names))
isco_converter$level <- isco_converter$job.simple <- NULL

## Assign ISCO 1-digit codes from isco_full
isco_converter$isco.code.major <- sub("^(\\w).*$","\\1", isco_converter$isco.code)
isco_converter$isco.title.major <- isco_full$title[match(isco_converter$isco.code.major, isco_full$code)]
isco_converter <- isco_converter[,c(3,2,1,4,5)]

## Add sectors from workunits_directions
isco_converter$direction.id <- workunits_directions$direction.id[match(isco_converter$workunit, workunits_directions$title)]
isco_converter$direction.title <- workunits_directions$direction.title[match(isco_converter$workunit, workunits_directions$title)]
isco_converter$direction.urlSlug <- workunits_directions$direction.urlSlug[match(isco_converter$workunit, workunits_directions$title)]
isco_converter$urlSlug <- workunits$urlSlug[match(isco_converter$workunit, workunits$title)]
isco_converter <- isco_converter[,c(1,9,2:8)]


##########################################################################
# 5.2. Create converter between CZ NACE and CDK work directions

## The classification is only indicative, multiple interpretations of the NACE sector classification are possible in some cases!

## Get CZ NACE classification at the level of sections (https://apl.czso.cz/iSMS/klasdata.jsp?kodcis=80004)
cz_nace <- fread(paste0(datapath,"labor/occupation_data/cz_nace_sections.csv"), header=TRUE, sep=";", dec=",")

## Create separate "directions" object with a list of professional directions according to CDK classification
directions <- unique.data.frame(isco_converter[,c(7:9)])

## Add (estimate) NACE sectors after creating concordance matrix between CZ NACE and CDK professional directions
isco_converter$nace.code <- NA
isco_converter$nace.code[which(isco_converter$direction.id %in% c(2,5))] <- "N"
isco_converter$nace.code[which(isco_converter$direction.id %in% c(3,8))] <- "M" ## 8 is later adjusted
isco_converter$nace.code[which(isco_converter$direction.id %in% 4)] <- "J"
isco_converter$nace.code[which(isco_converter$direction.id %in% c(6,7,40))] <- "O"
isco_converter$nace.code[which(isco_converter$direction.id %in% c(9,10))] <- "Q"
isco_converter$nace.code[which(isco_converter$direction.id %in% c(11,12))] <- "H"
isco_converter$nace.code[which(isco_converter$direction.id %in% c(13,23))] <- "A"
isco_converter$nace.code[which(isco_converter$direction.id %in% 14)] <- "G"
isco_converter$nace.code[which(isco_converter$direction.id %in% 15)] <- "I"
isco_converter$nace.code[which(isco_converter$direction.id %in% 16)] <- "K"
isco_converter$nace.code[which(isco_converter$direction.id %in% 17)] <- "S"
isco_converter$nace.code[which(isco_converter$direction.id %in% 18)] <- "R"
isco_converter$nace.code[which(isco_converter$direction.id %in% 19)] <- "E"
isco_converter$nace.code[which(isco_converter$direction.id %in% 20)] <- "J"
isco_converter$nace.code[which(isco_converter$direction.id %in% 24)] <- "B"
isco_converter$nace.code[which(isco_converter$direction.id %in% c(22,25:36))] <- "C"
isco_converter$nace.code[which(isco_converter$direction.id %in% 37)] <- "D"
isco_converter$nace.code[which(isco_converter$direction.id %in% 38)] <- "F"

## Add (estimate) NACE sectors after creating concordance matrix between CZ NACE and CDK work directions - special cases
## Management
isco_converter$nace.code[which(isco_converter$direction.id %in% 1 & grepl("1211|1346", isco_converter$isco.code))] <- "K"
isco_converter$nace.code[which(isco_converter$direction.id %in% 1 & 
                                 grepl("1213|1221|1222|1223|1120|1212|2422", 
                                       isco_converter$isco.code))] <- "M" # "Činnosti vedení podniků"
isco_converter$nace.code[which(isco_converter$direction.id %in% 1 & grepl("1219|1439|3341", isco_converter$isco.code))] <- "N"
isco_converter$nace.code[which(isco_converter$direction.id %in% 1 & grepl("1420", isco_converter$isco.code))] <- "G"
isco_converter$nace.code[which(isco_converter$direction.id %in% 1 & grepl("1321|3122", isco_converter$isco.code))] <- "C"
isco_converter$nace.code[which(isco_converter$direction.id %in% 1 & grepl("2141|1323|3123", isco_converter$isco.code))] <- "F"
isco_converter$nace.code[which(isco_converter$direction.id %in% 1 & 
                                 grepl("1312|1311", isco_converter$isco.code))] <- "A"
isco_converter$nace.code[which(isco_converter$direction.id %in% 1 & grepl("1324", isco_converter$isco.code))] <- "H"
isco_converter$nace.code[which(isco_converter$direction.id %in% 1 & grepl("1322|3121", isco_converter$isco.code))] <- "B"
isco_converter$nace.code[which(isco_converter$direction.id %in% 1 & grepl("1330", isco_converter$isco.code))] <- "J"
isco_converter$nace.code[which(isco_converter$direction.id %in% 1 & 
                                 grepl("1341|1342|1343|1344", isco_converter$isco.code))] <- "Q"
isco_converter$nace.code[which(isco_converter$direction.id %in% 1 & grepl("1349|1431", isco_converter$isco.code))] <- "R"
isco_converter$nace.code[which(isco_converter$direction.id %in% 1 & grepl("1345", isco_converter$isco.code))] <- "P"
isco_converter$nace.code[which(isco_converter$direction.id %in% 1 & grepl("1411|1412", isco_converter$isco.code))] <- "I"

## Science, educations, sports
isco_converter$nace.code[which(isco_converter$direction.id %in% 8 & 
                                 grepl("5312|2359|2424|2320|2353|2356|2359|2355|2310|2352|2330|2354|2342", 
                                       isco_converter$isco.code))] <- "P"

## Environment and waste management
isco_converter$nace.code[which(isco_converter$direction.id %in% 21 & 
                                 grepl("2143|3111|2114|2112|3141", isco_converter$isco.code))] <- "M"
isco_converter$nace.code[which(isco_converter$direction.id %in% 21 & grepl("8160", isco_converter$isco.code))] <- "E"
isco_converter$nace.code[which(isco_converter$direction.id %in% 21 & grepl("5113", isco_converter$isco.code))] <- "R"
isco_converter$nace.code[which(isco_converter$direction.id %in% 21 & grepl("2320", isco_converter$isco.code))] <- "P"
isco_converter$nace.code[which(isco_converter$direction.id %in% 21 & grepl("2133", isco_converter$isco.code))] <- "O"

## Non-disciplinary and interdisciplinary professions
isco_converter$nace.code[which(isco_converter$direction.id %in% 39 & grepl("2141", isco_converter$isco.code))] <- "M"
isco_converter$nace.code[which(isco_converter$direction.id %in% 39 & grepl("9611|9612|8189|3141", isco_converter$isco.code))] <- "E"
isco_converter$nace.code[which(isco_converter$direction.id %in% 39 & grepl("3333|2635|2423", isco_converter$isco.code))] <- "N"
isco_converter$nace.code[which(isco_converter$direction.id %in% 39 & 
                                 grepl("3119|3114|3115|3116", isco_converter$isco.code))] <- "C"
isco_converter$nace.code[which(isco_converter$direction.id %in% 39 & grepl("3112|7119", isco_converter$isco.code))] <- "F"
isco_converter$nace.code[which(isco_converter$direction.id %in% 39 & grepl("3113", isco_converter$isco.code))] <- "D"
isco_converter$nace.code[which(isco_converter$direction.id %in% 39 & grepl("3117", isco_converter$isco.code))] <- "B"
isco_converter$nace.code[which(isco_converter$direction.id %in% 39 & grepl("3343", isco_converter$isco.code))] <- "N"
isco_converter$nace.code[which(isco_converter$direction.id %in% 39 & grepl("3111|2421", isco_converter$isco.code))] <- "M"
isco_converter$nace.code[which(isco_converter$direction.id %in% 39 & grepl("2263", isco_converter$isco.code))] <- "Q"
isco_converter$nace.code[which(isco_converter$direction.id %in% 39 & grepl("7133", isco_converter$isco.code))] <- "S"

## NAs in professional directions - add direction.id
isco_converter$direction.id[which(isco_converter$direction.id %in% NA & 
                                    isco_converter$urlSlug %in% c("asistentka"))] <- 2
isco_converter$direction.id[which(isco_converter$direction.id %in% NA & 
                                    isco_converter$urlSlug %in% c("bezpecnostni-technik-pro-",
                                                                  "bansky-zachranar-potapec",
                                                                  "bansky-zachranar-cetar-po",
                                                                  "bansky-zachranar-lezec",
                                                                  "bezpecnostni-technik-v-po",
                                                                  "bansky-technolog-speciali",
                                                                  "bansky-zachranar-cetar",
                                                                  "bansky-projektant-special",
                                                                  "bansky-zachranar-cetar-le",
                                                                  "dispecer-dulni-vyroby",
                                                                  "ekolog-specialista-v-tezb",
                                                                  "hornik-predak-rubani-a-ra",
                                                                  "hornik-v-ostatnich-cinnos",
                                                                  "hlavni-energetik-v-tezbe-",
                                                                  "hlavni-mechanik-banske-za",
                                                                  "hornik-kombajner-rubani-a",
                                                                  "klapkar",
                                                                  "mechanik-dulni-degazace",
                                                                  "obsluha-stroju-a-zarizeni",
                                                                  "obsluha-tezebniho-zarizen",
                                                                  "provozni-tezar-ropy-a-ply",
                                                                  "predak-na-povrchu",
                                                                  "pracovnik-vyplachoveho-se",
                                                                  "pomocny-pracovnik-v-dole",
                                                                  "pomocny-pracovnik-v-povrc",
                                                                  "ridic-pasoveho-vozu",
                                                                  "ridic-kolesoveho-nakladac",
                                                                  "ridic-rypadla",
                                                                  "ridic-velkostroje",
                                                                  "strelmistr-pro-vrtne-a-ge",
                                                                  "strojnik-tezniho-stroje",
                                                                  "strelmistr-v-dole",
                                                                  "servisni-technik-karotazn",
                                                                  "strelmistr-pro-povrchove-",
                                                                  "strelmistr-pro-zvlastni-d",
                                                                  "strelmistr-pro-stavebni-p",
                                                                  "technicky-dozor-na-povrch",
                                                                  "technik-technickeho-rozvo",
                                                                  "technik-realizace-investi",
                                                                  "technicky-dozor-v-podzemi",
                                                                  "technolog-tezby-ropy-a-ze",
                                                                  "technik-pripravy-vyroby-p",
                                                                  "technicky-vedouci-odstrel",
                                                                  "technik-rizeni-udrzby-vel",
                                                                  "technik-rizeni-jakosti-v-",
                                                                  "technik-udrzby-tezni-jamy",
                                                                  "technik-pripravy-dulni-vy",
                                                                  "technolog-dulniho-dila",
                                                                  "vedouci-zavodni-banske-st",
                                                                  "vedouci-strediska-karotaz",
                                                                  "vedouci-tezebniho-stredis",
                                                                  "vrtac-v-dole",
                                                                  "vedouci-useku-karotaze-a-",
                                                                  "vedouci-tezebniho-a-uprav",
                                                                  "vrtac-pruzkumnych-vrtu",
                                                                  "vedouci-dulni-klimatizace",
                                                                  "zavodni",
                                                                  "zavodni-lomu-2",
                                                                  "zavodni-dolu"))] <- 24
isco_converter$direction.id[which(isco_converter$direction.id %in% NA & 
                                    isco_converter$urlSlug %in% c("dentalni-hygienistka",
                                                                  "porodni-asistentka-9f9f",
                                                                  "zubni-instrumentarka-91d9"))] <- 9
isco_converter$direction.id[which(isco_converter$direction.id %in% NA & 
                                    isco_converter$urlSlug %in% c("drevomodelar-4997",
                                                                  "vyrobce-forem-ze-dreva"))] <- 22
isco_converter$direction.id[which(isco_converter$direction.id %in% NA & 
                                    isco_converter$urlSlug %in% c("expedient-metalurgickych-",
                                                                  "formir-a-jadrar",
                                                                  "hutni-technik-mistr",
                                                                  "hutnik-tavic-nezeleznych-",
                                                                  "hutni-technik-operator",
                                                                  "hutni-inzenyr-vyzkumny-a-",
                                                                  "hutni-technik-normovac",
                                                                  "hutni-technik-technolog",
                                                                  "hutni-technik-metalurg",
                                                                  "hutnik-v-recyklaci-nezele",
                                                                  "hutnik-pripravar-vsazky-a",
                                                                  "hutnik-vysokopecar",
                                                                  "hutni-inzenyr-projektant",
                                                                  "hutni-inzenyr-technolog",
                                                                  "hutni-inzenyr-technik-riz",
                                                                  "hutnik-tavic-oceli",
                                                                  "hutni-technik-projektant",
                                                                  "hutni-inzenyr-dispecer",
                                                                  "hutnik-valcir-kovu",
                                                                  "hutni-technik-rizeni-jako",
                                                                  "hutni-inzenyr-metalurg",
                                                                  "hutni-technik-dispecer",
                                                                  "hutnik-tazec-kovu",
                                                                  "kovar-v-hutnim-provozu",
                                                                  "lisar-na-protlacovacich-l-8e37",
                                                                  "lisar-serizovac-na-automa-6a8d",
                                                                  "lisar-na-strojich-s-manua-692fu",
                                                                  "laborant-v-metalurgii",
                                                                  "modelar-ve-slevarenstvi",
                                                                  "pomocny-pracovnik-v-hutni",
                                                                  "slevarensky-technik-mistr",
                                                                  "samostatny-hutni-technik-",
                                                                  "slevarensky-technik-dispe",
                                                                  "slevarensky-inzenyr-techn",
                                                                  "samostatny-hutni-technik-1ff8",
                                                                  "slevarensky-technik-normo",
                                                                  "samostatny-slevarensky-te",
                                                                  "slevarensky-inzenyr-vyzku",
                                                                  "slevarensky-technik-metal",
                                                                  "samostatny-hutni-technik-b7f9",
                                                                  "slevarensky-inzenyr-proje",
                                                                  "slevarensky-inzenyr-metal",
                                                                  "slevarensky-technik-techn-6045",
                                                                  "slevarensky-technik-rizen",
                                                                  "samostatny-slevarensky-te-aa74",
                                                                  "samostatny-slevarensky-te-d680",
                                                                  "samostatny-hutni-technik-2ef4",
                                                                  "slevarensky-technik-model",
                                                                  "samostatny-slevarensky-te-a2d6",
                                                                  "slevarensky-technik-proje",
                                                                  "slevarensky-inzenyr-techn-1005",
                                                                  "samostatny-hutni-technik-7332"))] <- 33
isco_converter$direction.id[which(isco_converter$direction.id %in% NA & 
                                    isco_converter$urlSlug %in% c("hodnotitel-rizik-ukladani"))] <- 21
isco_converter$direction.id[which(isco_converter$direction.id %in% NA & 
                                    isco_converter$urlSlug %in% c("hospodyne"))] <- 15
isco_converter$direction.id[which(isco_converter$direction.id %in% NA & 
                                    isco_converter$urlSlug %in% c("keramik-modelar",
                                                                  "vyrobce-sadrovych-forem"))] <- 32
isco_converter$direction.id[which(isco_converter$direction.id %in% NA & 
                                    isco_converter$urlSlug %in% c("kovarensky-technik-techno",
                                                                  "kovarensky-technik-techno-9b18",
                                                                  "kovarensky-inzenyr-techno",
                                                                  "kovarensky-inzenyr-techno-d1d7",
                                                                  "mistr-lisovny",
                                                                  "mistr-kovarny",
                                                                  "vyrobce-kovovych-forem",
                                                                  "technolog-lisovny",
                                                                  "tavic",
                                                                  "technolog-lisovny-special"))] <- 34
isco_converter$direction.id[which(isco_converter$direction.id %in% NA & 
                                    isco_converter$urlSlug %in% c("koksar-obsluha-baterii",
                                                                  "koksar-obsluha-chemicke-c",
                                                                  "vyplachovy-technik"))] <- 31
isco_converter$direction.id[which(isco_converter$direction.id %in% NA & 
                                    isco_converter$urlSlug %in% c("kosmeticka",
                                                                  "manikerka-pedikerka",
                                                                  "uklidovy-pracovnik-v-ubyt"))] <- 17
isco_converter$direction.id[which(isco_converter$direction.id %in% NA & 
                                    isco_converter$urlSlug %in% c("mechatronik"))] <- 35
isco_converter$direction.id[which(isco_converter$direction.id %in% NA & 
                                    isco_converter$urlSlug %in% c("zpracovatel-zivocisnych-t"))] <- 25
isco_converter$direction.id[which(isco_converter$direction.id %in% NA & 
                                    isco_converter$urlSlug %in% c("specialista-pro-oblast-vy-4e19"))] <- 6

## NAs in professional directions - classify
isco_converter$nace.code[which(isco_converter$direction.id %in% 2 & 
                                 isco_converter$urlSlug %in% c("asistentka"))] <- "N"
isco_converter$nace.code[which(isco_converter$direction.id %in% 24 & 
                                 isco_converter$urlSlug %in% c("bezpecnostni-technik-pro-",
                                                               "bansky-zachranar-potapec",
                                                               "bansky-zachranar-cetar-po",
                                                               "bansky-zachranar-lezec",
                                                               "bezpecnostni-technik-v-po",
                                                               "bansky-technolog-speciali",
                                                               "bansky-zachranar-cetar",
                                                               "bansky-projektant-special",
                                                               "bansky-zachranar-cetar-le",
                                                               "dispecer-dulni-vyroby",
                                                               "ekolog-specialista-v-tezb",
                                                               "hornik-predak-rubani-a-ra",
                                                               "hornik-v-ostatnich-cinnos",
                                                               "hlavni-energetik-v-tezbe-",
                                                               "hlavni-mechanik-banske-za",
                                                               "hornik-kombajner-rubani-a",
                                                               "klapkar",
                                                               "mechanik-dulni-degazace",
                                                               "obsluha-stroju-a-zarizeni",
                                                               "obsluha-tezebniho-zarizen",
                                                               "provozni-tezar-ropy-a-ply",
                                                               "predak-na-povrchu",
                                                               "pracovnik-vyplachoveho-se",
                                                               "pomocny-pracovnik-v-dole",
                                                               "pomocny-pracovnik-v-povrc",
                                                               "ridic-pasoveho-vozu",
                                                               "ridic-kolesoveho-nakladac",
                                                               "ridic-rypadla",
                                                               "ridic-velkostroje",
                                                               "strelmistr-pro-vrtne-a-ge",
                                                               "strojnik-tezniho-stroje",
                                                               "strelmistr-v-dole",
                                                               "servisni-technik-karotazn",
                                                               "strelmistr-pro-povrchove-",
                                                               "strelmistr-pro-zvlastni-d",
                                                               "strelmistr-pro-stavebni-p",
                                                               "technicky-dozor-na-povrch",
                                                               "technik-technickeho-rozvo",
                                                               "technik-realizace-investi",
                                                               "technicky-dozor-v-podzemi",
                                                               "technolog-tezby-ropy-a-ze",
                                                               "technik-pripravy-vyroby-p",
                                                               "technicky-vedouci-odstrel",
                                                               "technik-rizeni-udrzby-vel",
                                                               "technik-rizeni-jakosti-v-",
                                                               "technik-udrzby-tezni-jamy",
                                                               "technik-pripravy-dulni-vy",
                                                               "technolog-dulniho-dila",
                                                               "vedouci-zavodni-banske-st",
                                                               "vedouci-strediska-karotaz",
                                                               "vedouci-tezebniho-stredis",
                                                               "vrtac-v-dole",
                                                               "vedouci-useku-karotaze-a-",
                                                               "vedouci-tezebniho-a-uprav",
                                                               "vrtac-pruzkumnych-vrtu",
                                                               "vedouci-dulni-klimatizace",
                                                               "zavodni",
                                                               "zavodni-lomu-2",
                                                               "zavodni-dolu"))] <- "B"
isco_converter$nace.code[which(isco_converter$direction.id %in% 9 & 
                                 isco_converter$urlSlug %in% c("dentalni-hygienistka",
                                                               "porodni-asistentka-9f9f",
                                                               "zubni-instrumentarka-91d9"))] <- "Q"
isco_converter$nace.code[which(isco_converter$direction.id %in% 22 & 
                                 isco_converter$urlSlug %in% c("drevomodelar-4997",
                                                               "vyrobce-forem-ze-dreva"))] <- "C"
isco_converter$nace.code[which(isco_converter$direction.id %in% 33 & 
                                 isco_converter$urlSlug %in% c("expedient-metalurgickych-",
                                                               "formir-a-jadrar",
                                                               "hutni-technik-mistr",
                                                               "hutnik-tavic-nezeleznych-",
                                                               "hutni-technik-operator",
                                                               "hutni-inzenyr-vyzkumny-a-",
                                                               "hutni-technik-normovac",
                                                               "hutni-technik-technolog",
                                                               "hutni-technik-metalurg",
                                                               "hutnik-v-recyklaci-nezele",
                                                               "hutnik-pripravar-vsazky-a",
                                                               "hutnik-vysokopecar",
                                                               "hutni-inzenyr-projektant",
                                                               "hutni-inzenyr-technolog",
                                                               "hutni-inzenyr-technik-riz",
                                                               "hutnik-tavic-oceli",
                                                               "hutni-technik-projektant",
                                                               "hutni-inzenyr-dispecer",
                                                               "hutnik-valcir-kovu",
                                                               "hutni-technik-rizeni-jako",
                                                               "hutni-inzenyr-metalurg",
                                                               "hutni-technik-dispecer",
                                                               "hutnik-tazec-kovu",
                                                               "kovar-v-hutnim-provozu",
                                                               "lisar-na-protlacovacich-l-8e37",
                                                               "lisar-serizovac-na-automa-6a8d",
                                                               "lisar-na-strojich-s-manua-692fu",
                                                               "laborant-v-metalurgii",
                                                               "modelar-ve-slevarenstvi",
                                                               "pomocny-pracovnik-v-hutni",
                                                               "slevarensky-technik-mistr",
                                                               "samostatny-hutni-technik-",
                                                               "slevarensky-technik-dispe",
                                                               "slevarensky-inzenyr-techn",
                                                               "samostatny-hutni-technik-1ff8",
                                                               "slevarensky-technik-normo",
                                                               "samostatny-slevarensky-te",
                                                               "slevarensky-inzenyr-vyzku",
                                                               "slevarensky-technik-metal",
                                                               "samostatny-hutni-technik-b7f9",
                                                               "slevarensky-inzenyr-proje",
                                                               "slevarensky-inzenyr-metal",
                                                               "slevarensky-technik-techn-6045",
                                                               "slevarensky-technik-rizen",
                                                               "samostatny-slevarensky-te-aa74",
                                                               "samostatny-slevarensky-te-d680",
                                                               "samostatny-hutni-technik-2ef4",
                                                               "slevarensky-technik-model",
                                                               "samostatny-slevarensky-te-a2d6",
                                                               "slevarensky-technik-proje",
                                                               "slevarensky-inzenyr-techn-1005",
                                                               "samostatny-hutni-technik-7332"))] <- "C"
isco_converter$nace.code[which(isco_converter$direction.id %in% 21 & 
                                 isco_converter$urlSlug %in% c("hodnotitel-rizik-ukladani"))] <- "E"
isco_converter$nace.code[which(isco_converter$direction.id %in% 15 & 
                                 isco_converter$urlSlug %in% c("hospodyne"))] <- "I"
isco_converter$nace.code[which(isco_converter$direction.id %in% 32 & 
                                 isco_converter$urlSlug %in% c("keramik-modelar",
                                                               "vyrobce-sadrovych-forem"))] <- "C"
isco_converter$nace.code[which(isco_converter$direction.id %in% 34 & 
                                 isco_converter$urlSlug %in% c("kovarensky-technik-techno",
                                                               "kovarensky-technik-techno-9b18",
                                                               "kovarensky-inzenyr-techno",
                                                               "kovarensky-inzenyr-techno-d1d7",
                                                               "mistr-lisovny",
                                                               "mistr-kovarny",
                                                               "vyrobce-kovovych-forem",
                                                               "technolog-lisovny",
                                                               "tavic",
                                                               "technolog-lisovny-special"))] <- "C"
isco_converter$nace.code[which(isco_converter$direction.id %in% 31 & 
                                 isco_converter$urlSlug %in% c("koksar-obsluha-baterii",
                                                               "koksar-obsluha-chemicke-c",
                                                               "vyplachovy-technik"))] <- "C"
isco_converter$nace.code[which(isco_converter$direction.id %in% 17 & 
                                 isco_converter$urlSlug %in% c("kosmeticka",
                                                               "manikerka-pedikerka",
                                                               "uklidovy-pracovnik-v-ubyt"))] <- "S"
isco_converter$nace.code[which(isco_converter$direction.id %in% 35 & 
                                 isco_converter$urlSlug %in% c("mechatronik"))] <- "C"
isco_converter$nace.code[which(isco_converter$direction.id %in% 25 & 
                                 isco_converter$urlSlug %in% c("zpracovatel-zivocisnych-t"))] <- "C"
isco_converter$nace.code[which(isco_converter$direction.id %in% 6 & 
                                 isco_converter$urlSlug %in% c("specialista-pro-oblast-vy-4e19"))] <- "O"

## Add NACE and professional direction names, sort by NACE codes
isco_converter$nace.title <- cz_nace$text2[match(isco_converter$nace.code, cz_nace$chodnota2)]
isco_converter$direction.title <- directions$direction.title[match(isco_converter$direction.id, directions$direction.id)]
isco_converter$direction.urlSlug <- directions$direction.urlSlug[match(isco_converter$direction.id, directions$direction.id)]
isco_converter <- isco_converter[order(isco_converter$nace.code),]
isco_converter <- isco_converter[,c(10,11,1:9)]

## Export
fwrite(isco_converter, file = paste0(datapath,"labor/occupation_data/isco_nace_converter.csv"), sep=";", dec=",", bom=TRUE)

## Subset only for jobs at risk and export
isco_converter_selection <- isco_converter %>% 
  inner_join(workunits_selection, by = c("workunit" = "title"))
fwrite(isco_converter_selection, file = paste0(datapath,"labor/occupation_data/isco_nace_selection_converter.csv"), sep=";", dec=",", bom=TRUE)


##########################################################################
# 5.3. Assign ISCO major and NACE categories to each dataset except for 'preferences', add direction.id to later remove obsolete jobs

kkovs_education_jobs_rmsd_clust <- list()
nsp_competences_jobs_rmsd_clust <- list()
nsp_generic_jobs_rmsd_clust <- list()
nsp_soft_jobs_rmsd_clust <- list()

for (job in jobs_education_risk) {
  kkovs_education_jobs_rmsd_clust[[job]] <- kkovs_education_jobs_rmsd[[job]]
  kkovs_education_jobs_rmsd_clust[[job]]$IscoCode <- isco_converter$isco.code.major[match(kkovs_education_jobs_rmsd_clust[[job]]$RepJobNames, isco_converter$workunit)]
  kkovs_education_jobs_rmsd_clust[[job]]$IscoTitle <- isco_converter$isco.title.major[match(kkovs_education_jobs_rmsd_clust[[job]]$RepJobNames, isco_converter$workunit)]
  kkovs_education_jobs_rmsd_clust[[job]]$NaceCode <- isco_converter$nace.code[match(kkovs_education_jobs_rmsd_clust[[job]]$RepJobNames, isco_converter$workunit)]
  kkovs_education_jobs_rmsd_clust[[job]]$NaceTitle <- isco_converter$nace.title[match(kkovs_education_jobs_rmsd_clust[[job]]$RepJobNames, isco_converter$workunit)]
  kkovs_education_jobs_rmsd_clust[[job]]$DirectionId <- isco_converter$direction.id[match(kkovs_education_jobs_rmsd_clust[[job]]$RepJobNames, isco_converter$workunit)]
}
for (job in jobs_nsp_risk) {
  nsp_competences_jobs_rmsd_clust[[job]] <- nsp_competences_jobs_rmsd[[job]]
  nsp_competences_jobs_rmsd_clust[[job]]$IscoCode <- isco_converter$isco.code.major[match(nsp_competences_jobs_rmsd_clust[[job]]$RepJobNames, isco_converter$workunit)]
  nsp_competences_jobs_rmsd_clust[[job]]$IscoTitle <- isco_converter$isco.title.major[match(nsp_competences_jobs_rmsd_clust[[job]]$RepJobNames, isco_converter$workunit)]
  nsp_competences_jobs_rmsd_clust[[job]]$NaceCode <- isco_converter$nace.code[match(nsp_competences_jobs_rmsd_clust[[job]]$RepJobNames, isco_converter$workunit)]
  nsp_competences_jobs_rmsd_clust[[job]]$NaceTitle <- isco_converter$nace.title[match(nsp_competences_jobs_rmsd_clust[[job]]$RepJobNames, isco_converter$workunit)]
  nsp_competences_jobs_rmsd_clust[[job]]$DirectionId <- isco_converter$direction.id[match(nsp_competences_jobs_rmsd_clust[[job]]$RepJobNames, isco_converter$workunit)]
  
  nsp_generic_jobs_rmsd_clust[[job]] <- nsp_generic_jobs_rmsd[[job]]
  nsp_generic_jobs_rmsd_clust[[job]]$IscoCode <- isco_converter$isco.code.major[match(nsp_generic_jobs_rmsd_clust[[job]]$RepJobNames, isco_converter$workunit)]
  nsp_generic_jobs_rmsd_clust[[job]]$IscoTitle <- isco_converter$isco.title.major[match(nsp_generic_jobs_rmsd_clust[[job]]$RepJobNames, isco_converter$workunit)]
  nsp_generic_jobs_rmsd_clust[[job]]$NaceCode <- isco_converter$nace.code[match(nsp_generic_jobs_rmsd_clust[[job]]$RepJobNames, isco_converter$workunit)]
  nsp_generic_jobs_rmsd_clust[[job]]$NaceTitle <- isco_converter$nace.title[match(nsp_generic_jobs_rmsd_clust[[job]]$RepJobNames, isco_converter$workunit)]
  nsp_generic_jobs_rmsd_clust[[job]]$DirectionId <- isco_converter$direction.id[match(nsp_generic_jobs_rmsd_clust[[job]]$RepJobNames, isco_converter$workunit)]
  
  nsp_soft_jobs_rmsd_clust[[job]] <- nsp_soft_jobs_rmsd[[job]]
  nsp_soft_jobs_rmsd_clust[[job]]$IscoCode <- isco_converter$isco.code.major[match(nsp_soft_jobs_rmsd_clust[[job]]$RepJobNames, isco_converter$workunit)]
  nsp_soft_jobs_rmsd_clust[[job]]$IscoTitle <- isco_converter$isco.title.major[match(nsp_soft_jobs_rmsd_clust[[job]]$RepJobNames, isco_converter$workunit)]
  nsp_soft_jobs_rmsd_clust[[job]]$NaceCode <- isco_converter$nace.code[match(nsp_soft_jobs_rmsd_clust[[job]]$RepJobNames, isco_converter$workunit)]
  nsp_soft_jobs_rmsd_clust[[job]]$NaceTitle <- isco_converter$nace.title[match(nsp_soft_jobs_rmsd_clust[[job]]$RepJobNames, isco_converter$workunit)]
  nsp_soft_jobs_rmsd_clust[[job]]$DirectionId <- isco_converter$direction.id[match(nsp_soft_jobs_rmsd_clust[[job]]$RepJobNames, isco_converter$workunit)]
}


##########################################################################
# 5.4. Cluster each dataset (except for 'preferences') by ISCO major and NACE combinations worked with in the 'preferences' dataset

## We work with average values, averaged over the number of positions falling into each ISCO major x NACE combination

## Remove "Occupations removed from the categorization due to their obsolescence" (DirectionId = 41) and rows with NAs
for (job in jobs_education_risk) {
  kkovs_education_jobs_rmsd_clust[[job]] <- kkovs_education_jobs_rmsd_clust[[job]][!grepl("41", kkovs_education_jobs_rmsd_clust[[job]]$DirectionId),]
  kkovs_education_jobs_rmsd_clust[[job]] <- kkovs_education_jobs_rmsd_clust[[job]][!is.na(kkovs_education_jobs_rmsd_clust[[job]]$DirectionId),]
}
for (job in jobs_nsp_risk) {
  nsp_competences_jobs_rmsd_clust[[job]] <- nsp_competences_jobs_rmsd_clust[[job]][!grepl("41", nsp_competences_jobs_rmsd_clust[[job]]$DirectionId),]
  nsp_competences_jobs_rmsd_clust[[job]] <- nsp_competences_jobs_rmsd_clust[[job]][!is.na(nsp_competences_jobs_rmsd_clust[[job]]$DirectionId),]
  
  nsp_generic_jobs_rmsd_clust[[job]] <- nsp_generic_jobs_rmsd_clust[[job]][!grepl("41", nsp_generic_jobs_rmsd_clust[[job]]$DirectionId),]
  nsp_generic_jobs_rmsd_clust[[job]] <- nsp_generic_jobs_rmsd_clust[[job]][!is.na(nsp_generic_jobs_rmsd_clust[[job]]$DirectionId),]
  
  nsp_soft_jobs_rmsd_clust[[job]] <- nsp_soft_jobs_rmsd_clust[[job]][!grepl("41", nsp_soft_jobs_rmsd_clust[[job]]$DirectionId),]
  nsp_soft_jobs_rmsd_clust[[job]] <- nsp_soft_jobs_rmsd_clust[[job]][!is.na(nsp_soft_jobs_rmsd_clust[[job]]$DirectionId),]
}

## Cluster by ISCO major and NACE combinations by averaging the RMSD value over the number of jobs belonging to each such combination
for (job in jobs_education_risk) {
  kkovs_education_jobs_rmsd_clust[[job]] <- kkovs_education_jobs_rmsd_clust[[job]] %>%
    group_by(IscoTitle, NaceTitle) %>%
    summarize(AverageRMSD = mean(AverageRMSD))
  kkovs_education_jobs_rmsd_clust[[job]]$RepJobNames <- paste0(kkovs_education_jobs_rmsd_clust[[job]]$NaceTitle,"_",kkovs_education_jobs_rmsd_clust[[job]]$IscoTitle)
  kkovs_education_jobs_rmsd_clust[[job]]$NaceTitle <- kkovs_education_jobs_rmsd_clust[[job]]$IscoTitle <- NULL
  kkovs_education_jobs_rmsd_clust[[job]]$Rank <- NA
  kkovs_education_jobs_rmsd_clust[[job]] <- as.data.frame(kkovs_education_jobs_rmsd_clust[[job]][order(kkovs_education_jobs_rmsd_clust[[job]][,"AverageRMSD"]),])
  kkovs_education_jobs_rmsd_clust[[job]][,"Rank"] <- 1:nrow(kkovs_education_jobs_rmsd_clust[[job]])
  rownames(kkovs_education_jobs_rmsd_clust[[job]]) <- kkovs_education_jobs_rmsd_clust[[job]]$RepJobNames
  kkovs_education_jobs_rmsd_clust[[job]] <- kkovs_education_jobs_rmsd_clust[[job]][,c(2,1,3)]
}
for (job in jobs_nsp_risk) {
  nsp_competences_jobs_rmsd_clust[[job]] <- nsp_competences_jobs_rmsd_clust[[job]] %>%
    group_by(IscoTitle, NaceTitle) %>%
    summarize(AverageRMSD = mean(AverageRMSD))
  nsp_competences_jobs_rmsd_clust[[job]]$RepJobNames <- paste0(nsp_competences_jobs_rmsd_clust[[job]]$NaceTitle,"_",nsp_competences_jobs_rmsd_clust[[job]]$IscoTitle)
  nsp_competences_jobs_rmsd_clust[[job]]$NaceTitle <- nsp_competences_jobs_rmsd_clust[[job]]$IscoTitle <- NULL
  nsp_competences_jobs_rmsd_clust[[job]]$Rank <- NA
  nsp_competences_jobs_rmsd_clust[[job]] <- as.data.frame(nsp_competences_jobs_rmsd_clust[[job]][order(nsp_competences_jobs_rmsd_clust[[job]][,"AverageRMSD"]),])
  nsp_competences_jobs_rmsd_clust[[job]][,"Rank"] <- 1:nrow(nsp_competences_jobs_rmsd_clust[[job]])
  rownames(nsp_competences_jobs_rmsd_clust[[job]]) <- nsp_competences_jobs_rmsd_clust[[job]]$RepJobNames
  nsp_competences_jobs_rmsd_clust[[job]] <- nsp_competences_jobs_rmsd_clust[[job]][,c(2,1,3)]
  
  nsp_generic_jobs_rmsd_clust[[job]] <- nsp_generic_jobs_rmsd_clust[[job]] %>%
    group_by(IscoTitle, NaceTitle) %>%
    summarize(AverageRMSD = mean(AverageRMSD))
  nsp_generic_jobs_rmsd_clust[[job]]$RepJobNames <- paste0(nsp_generic_jobs_rmsd_clust[[job]]$NaceTitle,"_",nsp_generic_jobs_rmsd_clust[[job]]$IscoTitle)
  nsp_generic_jobs_rmsd_clust[[job]]$NaceTitle <- nsp_generic_jobs_rmsd_clust[[job]]$IscoTitle <- NULL
  nsp_generic_jobs_rmsd_clust[[job]]$Rank <- NA
  nsp_generic_jobs_rmsd_clust[[job]] <- as.data.frame(nsp_generic_jobs_rmsd_clust[[job]][order(nsp_generic_jobs_rmsd_clust[[job]][,"AverageRMSD"]),])
  nsp_generic_jobs_rmsd_clust[[job]][,"Rank"] <- 1:nrow(nsp_generic_jobs_rmsd_clust[[job]])
  rownames(nsp_generic_jobs_rmsd_clust[[job]]) <- nsp_generic_jobs_rmsd_clust[[job]]$RepJobNames
  nsp_generic_jobs_rmsd_clust[[job]] <- nsp_generic_jobs_rmsd_clust[[job]][,c(2,1,3)]
  
  nsp_soft_jobs_rmsd_clust[[job]] <- nsp_soft_jobs_rmsd_clust[[job]] %>%
    group_by(IscoTitle, NaceTitle) %>%
    summarize(AverageRMSD = mean(AverageRMSD))
  nsp_soft_jobs_rmsd_clust[[job]]$RepJobNames <- paste0(nsp_soft_jobs_rmsd_clust[[job]]$NaceTitle,"_",nsp_soft_jobs_rmsd_clust[[job]]$IscoTitle)
  nsp_soft_jobs_rmsd_clust[[job]]$NaceTitle <- nsp_soft_jobs_rmsd_clust[[job]]$IscoTitle <- NULL
  nsp_soft_jobs_rmsd_clust[[job]]$Rank <- NA
  nsp_soft_jobs_rmsd_clust[[job]] <- as.data.frame(nsp_soft_jobs_rmsd_clust[[job]][order(nsp_soft_jobs_rmsd_clust[[job]][,"AverageRMSD"]),])
  nsp_soft_jobs_rmsd_clust[[job]][,"Rank"] <- 1:nrow(nsp_soft_jobs_rmsd_clust[[job]])
  rownames(nsp_soft_jobs_rmsd_clust[[job]]) <- nsp_soft_jobs_rmsd_clust[[job]]$RepJobNames
  nsp_soft_jobs_rmsd_clust[[job]] <- nsp_soft_jobs_rmsd_clust[[job]][,c(2,1,3)]
}



##########################################################################
## 6. Adjust datasets for plotting
##########################################################################

##########################################################################
# 6.1. Organize the detailed (non-clustered) datasets (including 'preferences')

kkovs_education_jobs_rmsd_adj <- list()
nsp_competences_jobs_rmsd_adj <- list()
nsp_generic_jobs_rmsd_adj <- list()
nsp_soft_jobs_rmsd_adj <- list()
preferences_jobs_rank_adj <- list()

# jobs_education_risk_names <- jobs_education_names[which(jobs_education_names %in% jobsatrisk)]
# jobs_nsp_risk_names <- jobs_nsp_names[which(jobs_nsp_names %in% jobsatrisk)]

for (job in jobs_education_risk) {
  kkovs_education_jobs_rmsd_adj[[job]] <- kkovs_education_jobs_rmsd[[job]][c(1:100),c(1:3)]
  kkovs_education_jobs_rmsd_adj[[job]]$Job <- c(rep(job))
  kkovs_education_jobs_rmsd_adj[[job]]$Job <- jobs_converter$title[which(jobs_converter$urlSlug == job)]
}
for (job in jobs_education_risk) {
  kkovs_education_jobs_rmsd_adj[[job]]$RepJobNames <- factor(kkovs_education_jobs_rmsd_adj[[job]]$RepJobNames, levels = rev(unique(kkovs_education_jobs_rmsd_adj[[job]]$RepJobNames)), ordered = TRUE)
}

for (job in jobs_nsp_risk) {
  nsp_competences_jobs_rmsd_adj[[job]] <- nsp_competences_jobs_rmsd[[job]][c(1:100),c(1:3)]
  nsp_competences_jobs_rmsd_adj[[job]]$Job <- c(rep(job))
  nsp_competences_jobs_rmsd_adj[[job]]$Job <- jobs_converter$title[which(jobs_converter$urlSlug == job)]
  
  nsp_generic_jobs_rmsd_adj[[job]] <- nsp_generic_jobs_rmsd[[job]][c(1:100),c(1:3)]
  nsp_generic_jobs_rmsd_adj[[job]]$Job <- c(rep(job))
  nsp_generic_jobs_rmsd_adj[[job]]$Job <- jobs_converter$title[which(jobs_converter$urlSlug == job)]
  
  nsp_soft_jobs_rmsd_adj[[job]] <- nsp_soft_jobs_rmsd[[job]][c(1:100),c(1:3)]
  nsp_soft_jobs_rmsd_adj[[job]]$Job <- c(rep(job))
  nsp_soft_jobs_rmsd_adj[[job]]$Job <- jobs_converter$title[which(jobs_converter$urlSlug == job)]
}
for (job in jobs_nsp_risk) {
  nsp_competences_jobs_rmsd_adj[[job]]$RepJobNames <- factor(nsp_competences_jobs_rmsd_adj[[job]]$RepJobNames, levels = rev(unique(nsp_competences_jobs_rmsd_adj[[job]]$RepJobNames)), ordered = TRUE)
  nsp_generic_jobs_rmsd_adj[[job]]$RepJobNames <- factor(nsp_generic_jobs_rmsd_adj[[job]]$RepJobNames, levels = rev(unique(nsp_generic_jobs_rmsd_adj[[job]]$RepJobNames)), ordered = TRUE)
  nsp_soft_jobs_rmsd_adj[[job]]$RepJobNames <- factor(nsp_soft_jobs_rmsd_adj[[job]]$RepJobNames, levels = rev(unique(nsp_soft_jobs_rmsd_adj[[job]]$RepJobNames)), ordered = TRUE)
}

## Introduce 'Others' category before cutting off the 16th and further positions (using group_by and sum) in the 'preferences' dataset
for (job in jobs_pref_risk) {
  preferences_jobs_rank_adj[[job]] <- preferences_jobs_rank[[job]][,c(1:3)]
  preferences_jobs_rank_adj[[job]][c(16:nrow(preferences_jobs_rank_adj[[job]])),"RepJobNames"] <- c("Ostatní")
  preferences_jobs_rank_adj[[job]][c(16:nrow(preferences_jobs_rank_adj[[job]])),"Rank"] <- 16
  preferences_jobs_rank_adj[[job]][c(16:nrow(preferences_jobs_rank_adj[[job]])),] <- preferences_jobs_rank_adj[[job]][c(16:nrow(preferences_jobs_rank_adj[[job]])),] %>%
    group_by(RepJobNames, Rank) %>%
    summarize(Sum = sum(Sum))
  preferences_jobs_rank_adj[[job]] <- preferences_jobs_rank_adj[[job]][c(1:16),] # Only 15 + 'Others' as there are rarely more
  rownames(preferences_jobs_rank_adj[[job]]) <- preferences_jobs_rank_adj[[job]]$RepJobNames
  preferences_jobs_rank_adj[[job]]$Job <- c(rep(job))
  preferences_jobs_rank_adj[[job]]$Job <- gsub('_', ' - ', preferences_jobs_rank_adj[[job]]$Job)
}
for (job in jobs_pref_risk) {
  preferences_jobs_rank_adj[[job]]$RepJobNames <- factor(preferences_jobs_rank_adj[[job]]$RepJobNames, levels = rev(unique(preferences_jobs_rank_adj[[job]]$RepJobNames)), ordered = TRUE)
}


##########################################################################
# 6.2. rbind and adjust the detailed (non-clustered) datasets (including 'preferences') for plotting

kkovs_education_jobs_rmsd_adj <- do.call(rbind, kkovs_education_jobs_rmsd_adj)
nsp_competences_jobs_rmsd_adj <- do.call(rbind, nsp_competences_jobs_rmsd_adj)
nsp_generic_jobs_rmsd_adj <- do.call(rbind, nsp_generic_jobs_rmsd_adj)
nsp_soft_jobs_rmsd_adj <- do.call(rbind, nsp_soft_jobs_rmsd_adj)
preferences_jobs_rank_adj <- do.call(rbind, preferences_jobs_rank_adj)

## Fix order of the jobs
kkovs_education_jobs_rmsd_adj <- kkovs_education_jobs_rmsd_adj %>%
  mutate(RepJobNames = reorder_within(RepJobNames, AverageRMSD, Job))
nsp_competences_jobs_rmsd_adj <- nsp_competences_jobs_rmsd_adj %>%
  mutate(RepJobNames = reorder_within(RepJobNames, AverageRMSD, Job))
nsp_generic_jobs_rmsd_adj <- nsp_generic_jobs_rmsd_adj %>%
  mutate(RepJobNames = reorder_within(RepJobNames, AverageRMSD, Job))
nsp_soft_jobs_rmsd_adj <- nsp_soft_jobs_rmsd_adj %>%
  mutate(RepJobNames = reorder_within(RepJobNames, AverageRMSD, Job))
preferences_jobs_rank_adj <- preferences_jobs_rank_adj %>%
  mutate(RepJobNames = reorder_within(RepJobNames, Sum, Job))

## Adjust rownames, colnames, job names, and order of columns
rownames(kkovs_education_jobs_rmsd_adj) <- NULL
colnames(kkovs_education_jobs_rmsd_adj)[which(names(kkovs_education_jobs_rmsd_adj) == "RepJobNames")] <- "Replacement.Job"
colnames(kkovs_education_jobs_rmsd_adj)[which(names(kkovs_education_jobs_rmsd_adj) == "AverageRMSD")] <- "Average.RMSD"
kkovs_education_jobs_rmsd_adj$Replacement.Job <- gsub("\\_.*","", kkovs_education_jobs_rmsd_adj$Replacement.Job)
kkovs_education_jobs_rmsd_adj <- kkovs_education_jobs_rmsd_adj[,c(4,1,2,3)]

rownames(nsp_competences_jobs_rmsd_adj) <- NULL
colnames(nsp_competences_jobs_rmsd_adj)[which(names(nsp_competences_jobs_rmsd_adj) == "RepJobNames")] <- "Replacement.Job"
colnames(nsp_competences_jobs_rmsd_adj)[which(names(nsp_competences_jobs_rmsd_adj) == "AverageRMSD")] <- "Average.RMSD"
nsp_competences_jobs_rmsd_adj$Replacement.Job <- gsub("\\_.*","", nsp_competences_jobs_rmsd_adj$Replacement.Job)
nsp_competences_jobs_rmsd_adj <- nsp_competences_jobs_rmsd_adj[,c(4,1,3,2)]

rownames(nsp_generic_jobs_rmsd_adj) <- NULL
colnames(nsp_generic_jobs_rmsd_adj)[which(names(nsp_generic_jobs_rmsd_adj) == "RepJobNames")] <- "Replacement.Job"
colnames(nsp_generic_jobs_rmsd_adj)[which(names(nsp_generic_jobs_rmsd_adj) == "AverageRMSD")] <- "Average.RMSD"
nsp_generic_jobs_rmsd_adj$Replacement.Job <- gsub("\\_.*","", nsp_generic_jobs_rmsd_adj$Replacement.Job)
nsp_generic_jobs_rmsd_adj <- nsp_generic_jobs_rmsd_adj[,c(4,1,3,2)]

rownames(nsp_soft_jobs_rmsd_adj) <- NULL
colnames(nsp_soft_jobs_rmsd_adj)[which(names(nsp_soft_jobs_rmsd_adj) == "RepJobNames")] <- "Replacement.Job"
colnames(nsp_soft_jobs_rmsd_adj)[which(names(nsp_soft_jobs_rmsd_adj) == "AverageRMSD")] <- "Average.RMSD"
nsp_soft_jobs_rmsd_adj$Replacement.Job <- gsub("\\_.*","", nsp_soft_jobs_rmsd_adj$Replacement.Job)
nsp_soft_jobs_rmsd_adj <- nsp_soft_jobs_rmsd_adj[,c(4,1,3,2)]

rownames(preferences_jobs_rank_adj) <- NULL
colnames(preferences_jobs_rank_adj)[which(names(preferences_jobs_rank_adj) == "RepJobNames")] <- "Replacement.Job"
colnames(preferences_jobs_rank_adj)[which(names(preferences_jobs_rank_adj) == "Sum")] <- "Share"
preferences_jobs_rank_adj$Replacement.Job <- gsub("\\___.*","", preferences_jobs_rank_adj$Replacement.Job)
preferences_jobs_rank_adj$Replacement.Job <- gsub("\\_"," - ", preferences_jobs_rank_adj$Replacement.Job)
preferences_jobs_rank_adj <- preferences_jobs_rank_adj[,c(4,1,3,2)]

## Encode selected columns in UTF-8
kkovs_education_jobs_rmsd_adj$Replacement.Job <- enc2utf8(kkovs_education_jobs_rmsd_adj$Replacement.Job) 
kkovs_education_jobs_rmsd_adj$Job <- enc2utf8(kkovs_education_jobs_rmsd_adj$Job)

nsp_competences_jobs_rmsd_adj$Replacement.Job <- enc2utf8(nsp_competences_jobs_rmsd_adj$Replacement.Job) 
nsp_competences_jobs_rmsd_adj$Job <- enc2utf8(nsp_competences_jobs_rmsd_adj$Job)

nsp_generic_jobs_rmsd_adj$Replacement.Job <- enc2utf8(nsp_generic_jobs_rmsd_adj$Replacement.Job) 
nsp_generic_jobs_rmsd_adj$Job <- enc2utf8(nsp_generic_jobs_rmsd_adj$Job)

nsp_soft_jobs_rmsd_adj$Replacement.Job <- enc2utf8(nsp_soft_jobs_rmsd_adj$Replacement.Job) 
nsp_soft_jobs_rmsd_adj$Job <- enc2utf8(nsp_soft_jobs_rmsd_adj$Job)

preferences_jobs_rank_adj$Replacement.Job <- enc2utf8(preferences_jobs_rank_adj$Replacement.Job) 
preferences_jobs_rank_adj$Job <- enc2utf8(preferences_jobs_rank_adj$Job)

## Export
fwrite(kkovs_education_jobs_rmsd_adj, file = paste0(respath,"coal_qualifications_cz_kkovs_education_rmsd_adj.csv"), sep=";", dec=",", bom=TRUE)
fwrite(nsp_competences_jobs_rmsd_adj, file = paste0(respath,"coal_qualifications_cz_nsp_competences_rmsd_adj.csv"), sep=";", dec=",", bom=TRUE)
fwrite(nsp_generic_jobs_rmsd_adj, file = paste0(respath,"coal_qualifications_cz_nsp_generic_rmsd_adj.csv"), sep=";", dec=",", bom=TRUE)
fwrite(nsp_soft_jobs_rmsd_adj, file = paste0(respath,"coal_qualifications_cz_nsp_soft_rmsd_adj.csv"), sep=";", dec=",", bom=TRUE)
fwrite(preferences_jobs_rank_adj, file = paste0(respath,"coal_qualifications_cz_preferences_rank_adj.csv"), sep=";", dec=",", bom=TRUE)


##########################################################################
# 6.3. Organize the clustered datasets to harmonize them with the 'preferences'

kkovs_education_jobs_rmsd_clust_adj <- list()
nsp_competences_jobs_rmsd_clust_adj <- list()
nsp_generic_jobs_rmsd_clust_adj <- list()
nsp_soft_jobs_rmsd_clust_adj <- list()

## Cluster the 'job' by the NACE x ISCO major categories (including the 'preferences' dataset)
for (job in jobs_education_risk) {
  kkovs_education_jobs_rmsd_clust_adj[[job]] <- kkovs_education_jobs_rmsd_clust[[job]]
  kkovs_education_jobs_rmsd_clust_adj[[job]]$Job <- c(rep(job))
  kkovs_education_jobs_rmsd_clust_adj[[job]]$Job <- jobs_converter$title[which(jobs_converter$urlSlug == job)]
}
for (job in jobs_education_risk) {
  kkovs_education_jobs_rmsd_clust_adj[[job]]$RepJobNames <- factor(kkovs_education_jobs_rmsd_clust_adj[[job]]$RepJobNames, levels = rev(unique(kkovs_education_jobs_rmsd_clust_adj[[job]]$RepJobNames)), ordered = TRUE)
}

for (job in jobs_nsp_risk) {
  nsp_competences_jobs_rmsd_clust_adj[[job]] <- nsp_competences_jobs_rmsd_clust[[job]]
  nsp_competences_jobs_rmsd_clust_adj[[job]]$Job <- c(rep(job))
  nsp_competences_jobs_rmsd_clust_adj[[job]]$Job <- jobs_converter$title[which(jobs_converter$urlSlug == job)]
  
  nsp_generic_jobs_rmsd_clust_adj[[job]] <- nsp_generic_jobs_rmsd_clust[[job]]
  nsp_generic_jobs_rmsd_clust_adj[[job]]$Job <- c(rep(job))
  nsp_generic_jobs_rmsd_clust_adj[[job]]$Job <- jobs_converter$title[which(jobs_converter$urlSlug == job)]
  
  nsp_soft_jobs_rmsd_clust_adj[[job]] <- nsp_soft_jobs_rmsd_clust[[job]]
  nsp_soft_jobs_rmsd_clust_adj[[job]]$Job <- c(rep(job))
  nsp_soft_jobs_rmsd_clust_adj[[job]]$Job <- jobs_converter$title[which(jobs_converter$urlSlug == job)]
}
for (job in jobs_nsp_risk) {
  nsp_competences_jobs_rmsd_clust_adj[[job]]$RepJobNames <- factor(nsp_competences_jobs_rmsd_clust_adj[[job]]$RepJobNames, 
                                                                   levels = rev(unique(nsp_competences_jobs_rmsd_clust_adj[[job]]$RepJobNames)), 
                                                                   ordered = TRUE)
  nsp_generic_jobs_rmsd_clust_adj[[job]]$RepJobNames <- factor(nsp_generic_jobs_rmsd_clust_adj[[job]]$RepJobNames, 
                                                                   levels = rev(unique(nsp_generic_jobs_rmsd_clust_adj[[job]]$RepJobNames)), 
                                                                   ordered = TRUE)
  nsp_soft_jobs_rmsd_clust_adj[[job]]$RepJobNames <- factor(nsp_soft_jobs_rmsd_clust_adj[[job]]$RepJobNames, 
                                                                   levels = rev(unique(nsp_soft_jobs_rmsd_clust_adj[[job]]$RepJobNames)), 
                                                                   ordered = TRUE)
}

## rbind to further adjust
kkovs_education_jobs_rmsd_clust_adj <- do.call(rbind, kkovs_education_jobs_rmsd_clust_adj)
nsp_competences_jobs_rmsd_clust_adj <- do.call(rbind, nsp_competences_jobs_rmsd_clust_adj)
nsp_generic_jobs_rmsd_clust_adj <- do.call(rbind, nsp_generic_jobs_rmsd_clust_adj)
nsp_soft_jobs_rmsd_clust_adj <- do.call(rbind, nsp_soft_jobs_rmsd_clust_adj)

# nsp_soft_jobs_rmsd_clust_adj_check <- do.call(rbind, nsp_soft_jobs_rmsd_clust_adj_check)

## Create a converter of the jobs (workunits) at risk with the NACE x ISCO major categories used in the 'preferences' dataset
jobs_risk_converter <- as.data.frame(unique(preferences_jobs_rank_adj$Job))
colnames(jobs_risk_converter) <- c("Job.Preferences")
jobs_risk_converter$NACE.Name <- sub(" - .*$", "", jobs_risk_converter$Job.Preferences)
jobs_risk_converter$ISCO.Major.Name <- sub("^.* - ", "", jobs_risk_converter$Job.Preferences)
jobs_risk_converter$NACE <- NA
jobs_risk_converter$NACE[which(jobs_risk_converter$NACE.Name %in% "Těžba hnědého a černého uhlí")] <- "B"
jobs_risk_converter$NACE[which(jobs_risk_converter$NACE.Name %in% "Výroba elektřiny z uhlí")] <- "D"
jobs_risk_converter$NACE[which(jobs_risk_converter$NACE.Name %in% c("Výroba koksárenských výrobků",
                                                                    "Výroba železa a oceli, feroslitin"))] <- "C"
jobs_risk_converter$NACE[which(jobs_risk_converter$NACE.Name %in% c("Manipulace s nákladem; Skladování",
                                                                    "Železniční doprava"))] <- "H"
jobs_risk_converter$ISCO.Major <- isco_converter$isco.code.major[match(jobs_risk_converter$ISCO.Major.Name, isco_converter$isco.title.major)]
jobs_risk_converter$ISCO.Major[which(jobs_risk_converter$ISCO.Major.Name %in% "Řídící pracovníci a zákonodárci")] <- "1"
jobs_risk_converter$ISCO.Major[which(jobs_risk_converter$ISCO.Major.Name %in% "Pomocní pracovníci")] <- "9"
jobs_risk_converter$NACE.ISCO <- paste0(jobs_risk_converter$NACE,jobs_risk_converter$ISCO.Major)

jobs_risk_converter_workunits <- as.data.frame(unique(kkovs_education_jobs_rmsd_clust_adj$Job)) # doesn't matter which dataset is used to derive the unique jobs
colnames(jobs_risk_converter_workunits) <- c("Job.Workunits")
jobs_risk_converter_workunits$NACE <- isco_converter$nace.code[match(jobs_risk_converter_workunits$Job.Workunits, isco_converter$workunit)]
jobs_risk_converter_workunits$NACE[which(jobs_risk_converter_workunits$Job.Workunits %in% c("Báňský úpravář uhlí",
                                                                                            "Důlní elektrikář silnoproudých zařízení",
                                                                                            "Horník v dole",
                                                                                            "Horník rubání a ražení",
                                                                                            "Řidič závěsné lokomotivy v dole",
                                                                                            "Technik dozoru elektrického zařízení TZ v těžbě",
                                                                                            "Revizní technik velkostroje",
                                                                                            "Důlní zámečník",
                                                                                            "Elektromechanik v dole",
                                                                                            "Řidič důlní kolejové lokomotivy",
                                                                                            "Technik přípravy lomové těžby",
                                                                                            "Dispečer lomové těžby",
                                                                                            "Báňský technik pro povrchovou těžbu",
                                                                                            "Báňský technik pro hlubinnou těžbu",
                                                                                            "Projektant instalací elektrických zařízení v těžbě"))] <- "B"
jobs_risk_converter_workunits$NACE[which(jobs_risk_converter_workunits$Job.Workunits %in% c("Koksař",
                                                                                            "Hutník železa",
                                                                                            "Hutník neželezných kovů"))] <- "C"
jobs_risk_converter_workunits$ISCO.Major <- isco_converter$isco.code.major[match(jobs_risk_converter_workunits$Job.Workunits, isco_converter$workunit)]
jobs_risk_converter_workunits$ISCO.Major[which(jobs_risk_converter_workunits$Job.Workunits %in% c("Báňský úpravář uhlí",
                                                                                                  "Revizní technik velkostroje",
                                                                                                  "Dispečer lomové těžby"))] <- "3"
jobs_risk_converter_workunits$ISCO.Major[which(jobs_risk_converter_workunits$Job.Workunits %in% c("Horník v dole",
                                                                                                  "Řidič závěsné lokomotivy v dole",
                                                                                                  "Řidič důlní kolejové lokomotivy",
                                                                                                  "Koksař",
                                                                                                  "Hutník železa",
                                                                                                  "Hutník neželezných kovů"))] <- "8"
jobs_risk_converter_workunits$ISCO.Major[which(jobs_risk_converter_workunits$Job.Workunits %in% c("Technik přípravy lomové těžby",
                                                                                                  "Báňský technik pro povrchovou těžbu",
                                                                                                  "Báňský technik pro hlubinnou těžbu"))] <- "2"
jobs_risk_converter_workunits$NACE.ISCO <- paste0(jobs_risk_converter_workunits$NACE,jobs_risk_converter_workunits$ISCO.Major)
jobs_risk_converter_workunits$NACE.Major.Name <- isco_converter$nace.title[match(jobs_risk_converter_workunits$NACE, isco_converter$nace.code)]
jobs_risk_converter_workunits$ISCO.Major.Name <- isco_converter$isco.title.major[match(jobs_risk_converter_workunits$ISCO.Major, isco_converter$isco.code.major)]
jobs_risk_converter_workunits$Job.Preferences <- jobs_risk_converter$Job.Preferences[match(jobs_risk_converter_workunits$NACE.ISCO, jobs_risk_converter$NACE.ISCO)]

## Check and manually correct the sector allocation of the workunits besides the coal mining sector
jobs_risk_converter_workunits$Job.Preferences[59:65] <- jobs_risk_converter$Job.Preferences[31]
jobs_risk_converter_workunits$Job.Preferences[66] <- jobs_risk_converter$Job.Preferences[32]
jobs_risk_converter_workunits$Job.Preferences[67] <- jobs_risk_converter$Job.Preferences[27]

## Export
fwrite(jobs_risk_converter_workunits, file = paste0(datapath,"labor/occupation_data/jobs_risk_converter_workunits.csv"), sep=";", dec=",", bom=TRUE)

## Assign a NACE x ISCO major combination (from Job.Preferences) to each job at risk in each dataset and cluster
kkovs_education_jobs_rmsd_clust_adj$Job <- jobs_risk_converter_workunits$Job.Preferences[match(kkovs_education_jobs_rmsd_clust_adj$Job, jobs_risk_converter_workunits$Job.Workunits)]
kkovs_education_jobs_rmsd_clust_adj <- kkovs_education_jobs_rmsd_clust_adj %>%
  group_by(RepJobNames, Job) %>%
  summarize(AverageRMSD = mean(AverageRMSD))
kkovs_education_jobs_rmsd_clust_adj <- kkovs_education_jobs_rmsd_clust_adj %>%
  group_by(Job) %>%
  mutate(Rank = order(order(AverageRMSD)))
kkovs_education_jobs_rmsd_clust_adj <- kkovs_education_jobs_rmsd_clust_adj %>%
  arrange(Job, Rank)

nsp_competences_jobs_rmsd_clust_adj$Job <- jobs_risk_converter_workunits$Job.Preferences[match(nsp_competences_jobs_rmsd_clust_adj$Job, jobs_risk_converter_workunits$Job.Workunits)]
nsp_competences_jobs_rmsd_clust_adj <- nsp_competences_jobs_rmsd_clust_adj %>%
  group_by(RepJobNames, Job) %>%
  summarize(AverageRMSD = mean(AverageRMSD))
nsp_competences_jobs_rmsd_clust_adj <- nsp_competences_jobs_rmsd_clust_adj %>%
  group_by(Job) %>%
  mutate(Rank = order(order(AverageRMSD)))
nsp_competences_jobs_rmsd_clust_adj <- nsp_competences_jobs_rmsd_clust_adj %>%
  arrange(Job, Rank)

nsp_generic_jobs_rmsd_clust_adj$Job <- jobs_risk_converter_workunits$Job.Preferences[match(nsp_generic_jobs_rmsd_clust_adj$Job, jobs_risk_converter_workunits$Job.Workunits)]
nsp_generic_jobs_rmsd_clust_adj <- nsp_generic_jobs_rmsd_clust_adj %>%
  group_by(RepJobNames, Job) %>%
  summarize(AverageRMSD = mean(AverageRMSD))
nsp_generic_jobs_rmsd_clust_adj <- nsp_generic_jobs_rmsd_clust_adj %>%
  group_by(Job) %>%
  mutate(Rank = order(order(AverageRMSD)))
nsp_generic_jobs_rmsd_clust_adj <- nsp_generic_jobs_rmsd_clust_adj %>%
  arrange(Job, Rank)

nsp_soft_jobs_rmsd_clust_adj$Job <- jobs_risk_converter_workunits$Job.Preferences[match(nsp_soft_jobs_rmsd_clust_adj$Job, jobs_risk_converter_workunits$Job.Workunits)]
nsp_soft_jobs_rmsd_clust_adj <- nsp_soft_jobs_rmsd_clust_adj %>%
  group_by(RepJobNames, Job) %>%
  summarize(AverageRMSD = mean(AverageRMSD))
nsp_soft_jobs_rmsd_clust_adj <- nsp_soft_jobs_rmsd_clust_adj %>%
  group_by(Job) %>%
  mutate(Rank = order(order(AverageRMSD)))
nsp_soft_jobs_rmsd_clust_adj <- nsp_soft_jobs_rmsd_clust_adj %>%
  arrange(Job, Rank)

## Adjust rownames, colnames, job names, and order of columns
colnames(kkovs_education_jobs_rmsd_clust_adj)[which(names(kkovs_education_jobs_rmsd_clust_adj) == "RepJobNames")] <- "Replacement.Job"
colnames(kkovs_education_jobs_rmsd_clust_adj)[which(names(kkovs_education_jobs_rmsd_clust_adj) == "AverageRMSD")] <- "Average.RMSD"
kkovs_education_jobs_rmsd_clust_adj$Replacement.Job <- gsub("\\_"," - ", kkovs_education_jobs_rmsd_clust_adj$Replacement.Job)
kkovs_education_jobs_rmsd_clust_adj <- kkovs_education_jobs_rmsd_clust_adj[,c(2,1,3,4)]

colnames(nsp_competences_jobs_rmsd_clust_adj)[which(names(nsp_competences_jobs_rmsd_clust_adj) == "RepJobNames")] <- "Replacement.Job"
colnames(nsp_competences_jobs_rmsd_clust_adj)[which(names(nsp_competences_jobs_rmsd_clust_adj) == "AverageRMSD")] <- "Average.RMSD"
nsp_competences_jobs_rmsd_clust_adj$Replacement.Job <- gsub("\\_"," - ", nsp_competences_jobs_rmsd_clust_adj$Replacement.Job)
nsp_competences_jobs_rmsd_clust_adj <- nsp_competences_jobs_rmsd_clust_adj[,c(2,1,3,4)]

colnames(nsp_generic_jobs_rmsd_clust_adj)[which(names(nsp_generic_jobs_rmsd_clust_adj) == "RepJobNames")] <- "Replacement.Job"
colnames(nsp_generic_jobs_rmsd_clust_adj)[which(names(nsp_generic_jobs_rmsd_clust_adj) == "AverageRMSD")] <- "Average.RMSD"
nsp_generic_jobs_rmsd_clust_adj$Replacement.Job <- gsub("\\_"," - ", nsp_generic_jobs_rmsd_clust_adj$Replacement.Job)
nsp_generic_jobs_rmsd_clust_adj <- nsp_generic_jobs_rmsd_clust_adj[,c(2,1,3,4)]

colnames(nsp_soft_jobs_rmsd_clust_adj)[which(names(nsp_soft_jobs_rmsd_clust_adj) == "RepJobNames")] <- "Replacement.Job"
colnames(nsp_soft_jobs_rmsd_clust_adj)[which(names(nsp_soft_jobs_rmsd_clust_adj) == "AverageRMSD")] <- "Average.RMSD"
nsp_soft_jobs_rmsd_clust_adj$Replacement.Job <- gsub("\\_"," - ", nsp_soft_jobs_rmsd_clust_adj$Replacement.Job)
nsp_soft_jobs_rmsd_clust_adj <- nsp_soft_jobs_rmsd_clust_adj[,c(2,1,3,4)]

## Split into lists again, introduce 'Others' category before cutting off the 16th and further positions (using group_by and mean)
kkovs_education_jobs_rmsd_clust_adj_list <- list()
nsp_competences_jobs_rmsd_clust_adj_list <- list()
nsp_generic_jobs_rmsd_clust_adj_list <- list()
nsp_soft_jobs_rmsd_clust_adj_list <- list()

kkovs_education_jobs_rmsd_clust_adj_list <- split(kkovs_education_jobs_rmsd_clust_adj, f = kkovs_education_jobs_rmsd_clust_adj$Job)
nsp_competences_jobs_rmsd_clust_adj_list <- split(nsp_competences_jobs_rmsd_clust_adj, f = nsp_competences_jobs_rmsd_clust_adj$Job)
nsp_generic_jobs_rmsd_clust_adj_list <- split(nsp_generic_jobs_rmsd_clust_adj, f = nsp_generic_jobs_rmsd_clust_adj$Job)
nsp_soft_jobs_rmsd_clust_adj_list <- split(nsp_soft_jobs_rmsd_clust_adj, f = nsp_soft_jobs_rmsd_clust_adj$Job)

jobs_clust <- names(kkovs_education_jobs_rmsd_clust_adj_list) # doesn't matter which dataset is used to derive the job names (all are the same list)

for (job in jobs_clust) {
  kkovs_education_jobs_rmsd_clust_adj_list[[job]] <- as.data.frame(kkovs_education_jobs_rmsd_clust_adj_list[[job]][,c(1,2,4,3)])
  kkovs_education_jobs_rmsd_clust_adj_list[[job]]$Replacement.Job[which(kkovs_education_jobs_rmsd_clust_adj_list[[job]]$Rank > 15)] <- c("Ostatní")
  kkovs_education_jobs_rmsd_clust_adj_list[[job]]$Rank[which(kkovs_education_jobs_rmsd_clust_adj_list[[job]]$Rank > 15)] <- 16
  kkovs_education_jobs_rmsd_clust_adj_list[[job]][which(kkovs_education_jobs_rmsd_clust_adj_list[[job]]$Rank == 16),] <- kkovs_education_jobs_rmsd_clust_adj_list[[job]][which(kkovs_education_jobs_rmsd_clust_adj_list[[job]]$Rank == 16),] %>%
    group_by(Job, Replacement.Job, Rank) %>%
    summarize(Average.RMSD = mean(Average.RMSD))
  kkovs_education_jobs_rmsd_clust_adj_list[[job]] <- kkovs_education_jobs_rmsd_clust_adj_list[[job]][c(1:16),] # Only 15 + 'Others' as there are rarely more
  kkovs_education_jobs_rmsd_clust_adj_list[[job]] <- as.data.frame(kkovs_education_jobs_rmsd_clust_adj_list[[job]][,c(1,2,4,3)])
  
  nsp_competences_jobs_rmsd_clust_adj_list[[job]] <- as.data.frame(nsp_competences_jobs_rmsd_clust_adj_list[[job]][,c(1,2,4,3)])
  nsp_competences_jobs_rmsd_clust_adj_list[[job]]$Replacement.Job[which(nsp_competences_jobs_rmsd_clust_adj_list[[job]]$Rank > 15)] <- c("Ostatní")
  nsp_competences_jobs_rmsd_clust_adj_list[[job]]$Rank[which(nsp_competences_jobs_rmsd_clust_adj_list[[job]]$Rank > 15)] <- 16
  nsp_competences_jobs_rmsd_clust_adj_list[[job]][which(nsp_competences_jobs_rmsd_clust_adj_list[[job]]$Rank == 16),] <- nsp_competences_jobs_rmsd_clust_adj_list[[job]][which(nsp_competences_jobs_rmsd_clust_adj_list[[job]]$Rank == 16),] %>%
    group_by(Job, Replacement.Job, Rank) %>%
    summarize(Average.RMSD = mean(Average.RMSD))
  nsp_competences_jobs_rmsd_clust_adj_list[[job]] <- nsp_competences_jobs_rmsd_clust_adj_list[[job]][c(1:16),] # Only 15 + 'Others' as there are rarely more
  nsp_competences_jobs_rmsd_clust_adj_list[[job]] <- as.data.frame(nsp_competences_jobs_rmsd_clust_adj_list[[job]][,c(1,2,4,3)])
  
  nsp_generic_jobs_rmsd_clust_adj_list[[job]] <- as.data.frame(nsp_generic_jobs_rmsd_clust_adj_list[[job]][,c(1,2,4,3)])
  nsp_generic_jobs_rmsd_clust_adj_list[[job]]$Replacement.Job[which(nsp_generic_jobs_rmsd_clust_adj_list[[job]]$Rank > 15)] <- c("Ostatní")
  nsp_generic_jobs_rmsd_clust_adj_list[[job]]$Rank[which(nsp_generic_jobs_rmsd_clust_adj_list[[job]]$Rank > 15)] <- 16
  nsp_generic_jobs_rmsd_clust_adj_list[[job]][which(nsp_generic_jobs_rmsd_clust_adj_list[[job]]$Rank == 16),] <- nsp_generic_jobs_rmsd_clust_adj_list[[job]][which(nsp_generic_jobs_rmsd_clust_adj_list[[job]]$Rank == 16),] %>%
    group_by(Job, Replacement.Job, Rank) %>%
    summarize(Average.RMSD = mean(Average.RMSD))
  nsp_generic_jobs_rmsd_clust_adj_list[[job]] <- nsp_generic_jobs_rmsd_clust_adj_list[[job]][c(1:16),] # Only 15 + 'Others' as there are rarely more
  nsp_generic_jobs_rmsd_clust_adj_list[[job]] <- as.data.frame(nsp_generic_jobs_rmsd_clust_adj_list[[job]][,c(1,2,4,3)])
  
  nsp_soft_jobs_rmsd_clust_adj_list[[job]] <- as.data.frame(nsp_soft_jobs_rmsd_clust_adj_list[[job]][,c(1,2,4,3)])
  nsp_soft_jobs_rmsd_clust_adj_list[[job]]$Replacement.Job[which(nsp_soft_jobs_rmsd_clust_adj_list[[job]]$Rank > 15)] <- c("Ostatní")
  nsp_soft_jobs_rmsd_clust_adj_list[[job]]$Rank[which(nsp_soft_jobs_rmsd_clust_adj_list[[job]]$Rank > 15)] <- 16
  nsp_soft_jobs_rmsd_clust_adj_list[[job]][which(nsp_soft_jobs_rmsd_clust_adj_list[[job]]$Rank == 16),] <- nsp_soft_jobs_rmsd_clust_adj_list[[job]][which(nsp_soft_jobs_rmsd_clust_adj_list[[job]]$Rank == 16),] %>%
    group_by(Job, Replacement.Job, Rank) %>%
    summarize(Average.RMSD = mean(Average.RMSD))
  nsp_soft_jobs_rmsd_clust_adj_list[[job]] <- nsp_soft_jobs_rmsd_clust_adj_list[[job]][c(1:16),] # Only 15 + 'Others' as there are rarely more
  nsp_soft_jobs_rmsd_clust_adj_list[[job]] <- as.data.frame(nsp_soft_jobs_rmsd_clust_adj_list[[job]][,c(1,2,4,3)])
}

## Fix order of the jobs
for (job in jobs_clust) {
  kkovs_education_jobs_rmsd_clust_adj_list[[job]]$Replacement.Job <- factor(kkovs_education_jobs_rmsd_clust_adj_list[[job]]$Replacement.Job, 
                                                                            levels = rev(unique(kkovs_education_jobs_rmsd_clust_adj_list[[job]]$Replacement.Job)), 
                                                                            ordered = TRUE)
  nsp_competences_jobs_rmsd_clust_adj_list[[job]]$Replacement.Job <- factor(nsp_competences_jobs_rmsd_clust_adj_list[[job]]$Replacement.Job, 
                                                                            levels = rev(unique(nsp_competences_jobs_rmsd_clust_adj_list[[job]]$Replacement.Job)), 
                                                                            ordered = TRUE)
  nsp_generic_jobs_rmsd_clust_adj_list[[job]]$Replacement.Job <- factor(nsp_generic_jobs_rmsd_clust_adj_list[[job]]$Replacement.Job, 
                                                                        levels = rev(unique(nsp_generic_jobs_rmsd_clust_adj_list[[job]]$Replacement.Job)), 
                                                                        ordered = TRUE)
  nsp_soft_jobs_rmsd_clust_adj_list[[job]]$Replacement.Job <- factor(nsp_soft_jobs_rmsd_clust_adj_list[[job]]$Replacement.Job, 
                                                                     levels = rev(unique(nsp_soft_jobs_rmsd_clust_adj_list[[job]]$Replacement.Job)), 
                                                                     ordered = TRUE)
}


##########################################################################
# 6.4. rbind and adjust the clustered datasets (harmonized with the 'preferences')

## rbind again
kkovs_education_jobs_rmsd_clust_adj <- do.call(rbind, kkovs_education_jobs_rmsd_clust_adj_list)
nsp_competences_jobs_rmsd_clust_adj <- do.call(rbind, nsp_competences_jobs_rmsd_clust_adj_list)
nsp_generic_jobs_rmsd_clust_adj <- do.call(rbind, nsp_generic_jobs_rmsd_clust_adj_list)
nsp_soft_jobs_rmsd_clust_adj <- do.call(rbind, nsp_soft_jobs_rmsd_clust_adj_list)

## Adjust rownames
rownames(kkovs_education_jobs_rmsd_clust_adj) <- 
  rownames(nsp_competences_jobs_rmsd_clust_adj) <- 
  rownames(nsp_generic_jobs_rmsd_clust_adj) <- 
  rownames(nsp_soft_jobs_rmsd_clust_adj) <- NULL

## Adjust job names ("Pomocní pracovníci" to "Pomocní a nekvalifikovaní pracovníci")
kkovs_education_jobs_rmsd_clust_adj$Job <- gsub("\\Pomocní pracovníci","Pomocní a nekvalifikovaní pracovníci", kkovs_education_jobs_rmsd_clust_adj$Job)
nsp_competences_jobs_rmsd_clust_adj$Job <- gsub("\\Pomocní pracovníci","Pomocní a nekvalifikovaní pracovníci", nsp_competences_jobs_rmsd_clust_adj$Job)
nsp_generic_jobs_rmsd_clust_adj$Job <- gsub("\\Pomocní pracovníci","Pomocní a nekvalifikovaní pracovníci", nsp_generic_jobs_rmsd_clust_adj$Job)
nsp_soft_jobs_rmsd_clust_adj$Job <- gsub("\\Pomocní pracovníci","Pomocní a nekvalifikovaní pracovníci", nsp_soft_jobs_rmsd_clust_adj$Job)

## Turn Replacement.Job into character vector to enable encoding
kkovs_education_jobs_rmsd_clust_adj$Replacement.Job <- as.character(kkovs_education_jobs_rmsd_clust_adj$Replacement.Job)
nsp_competences_jobs_rmsd_clust_adj$Replacement.Job <- as.character(nsp_competences_jobs_rmsd_clust_adj$Replacement.Job)
nsp_generic_jobs_rmsd_clust_adj$Replacement.Job <- as.character(nsp_generic_jobs_rmsd_clust_adj$Replacement.Job)
nsp_soft_jobs_rmsd_clust_adj$Replacement.Job <- as.character(nsp_soft_jobs_rmsd_clust_adj$Replacement.Job)

## Encode selected columns in UTF-8
kkovs_education_jobs_rmsd_clust_adj$Replacement.Job <- enc2utf8(kkovs_education_jobs_rmsd_clust_adj$Replacement.Job) 
kkovs_education_jobs_rmsd_clust_adj$Job <- enc2utf8(kkovs_education_jobs_rmsd_clust_adj$Job)

nsp_competences_jobs_rmsd_clust_adj$Replacement.Job <- enc2utf8(nsp_competences_jobs_rmsd_clust_adj$Replacement.Job) 
nsp_competences_jobs_rmsd_clust_adj$Job <- enc2utf8(nsp_competences_jobs_rmsd_clust_adj$Job)

nsp_generic_jobs_rmsd_clust_adj$Replacement.Job <- enc2utf8(nsp_generic_jobs_rmsd_clust_adj$Replacement.Job) 
nsp_generic_jobs_rmsd_clust_adj$Job <- enc2utf8(nsp_generic_jobs_rmsd_clust_adj$Job)

nsp_soft_jobs_rmsd_clust_adj$Replacement.Job <- enc2utf8(nsp_soft_jobs_rmsd_clust_adj$Replacement.Job) 
nsp_soft_jobs_rmsd_clust_adj$Job <- enc2utf8(nsp_soft_jobs_rmsd_clust_adj$Job)

## Export
fwrite(kkovs_education_jobs_rmsd_clust_adj, file = paste0(respath,"coal_qualifications_cz_kkovs_education_rmsd_clust_adj.csv"), sep=";", dec=",", bom=TRUE)
fwrite(nsp_competences_jobs_rmsd_clust_adj, file = paste0(respath,"coal_qualifications_cz_nsp_competences_rmsd_clust_adj.csv"), sep=";", dec=",", bom=TRUE)
fwrite(nsp_generic_jobs_rmsd_clust_adj, file = paste0(respath,"coal_qualifications_cz_nsp_generic_rmsd_clust_adj.csv"), sep=";", dec=",", bom=TRUE)
fwrite(nsp_soft_jobs_rmsd_clust_adj, file = paste0(respath,"coal_qualifications_cz_nsp_soft_rmsd_clust_adj.csv"), sep=";", dec=",", bom=TRUE)

## Adjust job names also in the preferences dataset ("Pomocní pracovníci" to "Pomocní a nekvalifikovaní pracovníci")
preferences_jobs_rank_adj$Job <- gsub("\\Pomocní pracovníci","Pomocní a nekvalifikovaní pracovníci", preferences_jobs_rank_adj$Job)
preferences_jobs_rank_adj$Replacement.Job <- gsub("\\Pomocní pracovníci","Pomocní a nekvalifikovaní pracovníci", preferences_jobs_rank_adj$Replacement.Job)


