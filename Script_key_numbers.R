##############################################################################################################
#------------------------------------------------------------------------------------------------------------# 
# title: WILDbase numbers
#------------------------------------------------------------------------------------------------------------# 
##############################################################################################################

# - Unique number of articles per subgroup and in total 

desired <- NULL
grp     <- 1

# --------------- Total number of unique articles  
tab <- data_wildbase %>% 
  distinct(reference)

tab_c <- function_reference_search(x = desired, y = tab, grp)

tab_c %>% 
  distinct(grp) %>% 
  group_by(grp) %>%
  count %>% 
  ungroup 

# --------------- Total number of unique articles per species  
tab <- data_wildbase %>% 
  distinct(species, reference)

tab_c <- function_reference_search(x = desired, y = tab, grp)

tab_c %>% 
  distinct(species, grp) %>% 
  group_by(species) %>% 
  count %>% 
  ungroup

# --------------- Total number of unique articles per pathogen
# --------------- Fungi 
tab_fungi <- data_wildbase %>% 
  filter(pathogen_group == "fungi") %>% 
  distinct(pathogen_group, reference)  

tab_fungi_c <- function_reference_search(x = desired, y = tab_fungi, grp)
tab_fungi_c <- tab_fungi_c %>% distinct(pathogen_group, grp)

# --------------- Virus 
tab_virus <- data_wildbase %>%
  filter(pathogen_group == "virus") %>% 
  distinct(pathogen_group, reference)  

tab_virus_c <- function_reference_search(x = desired, y = tab_virus, grp)
tab_virus_c <- tab_virus_c %>% distinct(pathogen_group, grp)

# --------------- helminths 
tab_helminths <- data_wildbase %>% 
  filter(pathogen_group == "helminths") %>% 
  distinct(pathogen_group, reference)  

tab_helminths_c <- function_reference_search(x = desired, y = tab_helminths, grp)
tab_helminths_c <- tab_helminths_c %>% distinct(pathogen_group, grp)

# --------------- Protozoa 
tab_Protozoa <- data_wildbase %>% 
  filter(pathogen_group == "protozoa") %>% 
  distinct(pathogen_group, reference)  

tab_Protozoa_c <- function_reference_search(x = desired, y = tab_Protozoa, grp)
tab_Protozoa_c <- tab_Protozoa_c %>% distinct(pathogen_group, grp)

# --------------- Bacteria 
tab_Bacteria <- data_wildbase %>% 
  filter(pathogen_group == "bacteria") %>% 
  distinct(pathogen_group, reference)  

tab_Bacteria_c <- function_reference_search(x = desired, y = tab_Bacteria, grp)
tab_Bacteria_c <- tab_Bacteria_c %>% distinct(pathogen_group, grp)

# --------------- Overall 
tab_Overall <- tab_fungi_c %>% 
  bind_rows(tab_virus_c) %>% 
  bind_rows(tab_helminths_c) %>% 
  bind_rows(tab_Protozoa_c) %>% 
  bind_rows(tab_Bacteria_c) %>% 
  group_by(pathogen_group) %>% 
  count

# --------------- Clean
rm(tab_Overall, tab_Bacteria, tab_Bacteria_c, tab_fungi, tab_fungi_c, tab_helminths, tab_helminths_c, 
   tab_Protozoa, tab_Protozoa_c, tab_virus, tab_virus_c, tab, tab_c); gc()
