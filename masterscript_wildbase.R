##############################################################################################################
#------------------------------------------------------------------------------------------------------------# 
# WILDbase
#------------------------------------------------------------------------------------------------------------# 
##############################################################################################################
# Clean environment 
rm(list=ls())

##############################################################################################################
# Load packages
##############################################################################################################

# Which packages are being used?
gebruikte_packages <- c(
  "tidyverse", "readxl", "writexl", "stars", "MASS", "DescTools", "PropCIs", "mapview", "RColorBrewer", 
  "sf", "foreign", "parallel", "janitor", "terra", "raster", "lubridate", "ggpubr", "lme4", "knitr")

# Which packages have already been installed?
geinstalleerde_packages <- rownames(installed.packages())

# Install packages 
installeren_packages <- gebruikte_packages[!gebruikte_packages %in% geinstalleerde_packages]
if (length(installeren_packages) > 0) install.packages(pkgs = installeren_packages)

# Load unattached packages
if (!all(gebruikte_packages %in% .packages())) {
  suppressMessages(invisible(lapply(
    X              = gebruikte_packages,
    FUN            = library,
    character.only = TRUE)))}

options(scipen=999) # Remove scientific notation
select <- dplyr::select

##############################################################################################################
# Import / Clean
##############################################################################################################

# -------------- Import 
data_wildbase_org <- "Data/" %>% 
  list.files(full.names = TRUE) %>%
  str_subset(pattern = "data_WILDbase_") %>% 
  str_subset(pattern = ".xlsx") %>% 
  max() %>% # choose most recent version
  read_xlsx() 

# -------------- Clean 
data_wildbase <- data_wildbase_org %>% 
  clean_names() %>% 
  mutate(pathogen_group = if_else(pathogen_group == "viruses", "virus", pathogen_group), 
         pathogen_tested = pathogen_tested %>% str_to_lower, 
         pathogen_tested = pathogen_tested %>% str_replace("spp.", "spp"),
         pathogen_tested_c = case_when(
           pathogen_tested == "yersina pseudotuberculosis" ~ "yersinia pseudotuberculosis", 
           pathogen_tested == "borrelia burgdorferi sensu lato" ~ "borrelia burgdorferi s.l.",
           pathogen_tested == "borrelia burgdorferi sensu stricto" ~ "borrelia burgdorferi s.s.",
           pathogen_tested == "crimean-congo hemorragic fever virus" ~ "crimean-congo haemorrhagic fever virus",
           pathogen_tested == "borna virus" ~ "borna disease virus", 
           pathogen_tested == "dobrova-belgrade virus" ~ "dobrava-belgrade orthohantavirus", 
           pathogen_tested %>% str_detect("lymphocytic choriomeningitis") ~ "lymphocytic choriomeningitis mammarenavirus", 
           pathogen_tested == "influenza a" ~ "influenza a virus",
           pathogen_tested == "orthopox virus" ~ "orthopoxvirus",
           pathogen_tested == "tick-borne enchephalitis virus" ~ "tick-borne encephalitis virus", 
           pathogen_tested == "taenia taeniaformis" ~ "taenia taeniaeformis", 
           TRUE ~ pathogen_tested)) 

##############################################################################################################
# Functions
##############################################################################################################
function_reference_search <- function(x, y, grp) {
  if (nrow(y) < 1) { # if y is empty return data
    return(x)
  } else {
    similar <- agrepl(y$reference[1], y$reference) # find similar occurring strings
    x <- rbind(x, y[similar,] %>% 
                 mutate(reference=head(reference,1)) %>% 
                 mutate(grp=grp))
    
    y <- setdiff(y, y[similar,])
    special(x, y, grp+1)
  }
}