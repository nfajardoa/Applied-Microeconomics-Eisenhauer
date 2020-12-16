### PACKAGES LOADING AND INSTALLATION

#Set directory
setwd("Documents/MSc Bonn/Applied Micro/SOEP")

# Package names

packages <- c("haven", "dplyr", "DBI", "dbplyr", "purrr", "readxl", "glue", "stringr", "magrittr", "tidyr", "lubridate", "tidyverse", "bit64", "stringi", "sjmisc", "stringdist", "textshape", "textclean", "phonics") 
# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# Options
options(future.globals.maxSize = 891289600)

### Data
# Bio information
ppath <- read_csv("ppath.csv") %>%
  select(cid, pid, sex, gebjahr, todjahr, immiyear, germborn, corigin, migback, birthregion, loc1989) %>%
  group_by(cid, pid) %>%
  filter(sex > 0, gebjahr > 0) %>%
  rowwise() %>%
  mutate(east = case_when(loc1989 == 1 ~ 1,
                          loc1989 > 1 ~ 0,
                          (loc1989 < 1) & (birthregion %in% c(11, 12, 13, 14, 15, 16)) ~ 1,
                          (loc1989 < 1) & (birthregion %in% c(1,2,3,4,5,6,7,8,9,10)) ~ 0,
                          loc1989 < 1 ~ -2),
         culture = case_when(corigin %in% c(1, 10, 19, 69) ~ 1L,     #German related
                             corigin %in% c(2, 130) ~ 2L,            #Turkey
                             corigin %in% c(3, 21, 29, 73, 75, 106, 119, 120, 
                                            121, 122, 140, 165, 168) ~ 3L,            #Balkans
                             corigin %in% c(4, 5, 6, 28, 58) ~ 4L,                #South Europe
                             corigin %in% c(11, 12, 62, 116, 117, 118) ~ 5L,          #France and Benelux
                             corigin %in% c(13, 15, 16, 17, 70, 101, 103, 146) ~ 6L,  #Scandinavia
                             corigin %in% c(14, 18, 41, 55, 56, 63, 71, 93, 107, 112, 134, 164, 175) ~ 7L,  #Anglo
                             corigin %in% c(20, 27, 34, 35, 45, 48, 51, 59, 61, 64, 
                                            72, 88, 92, 96, 99, 108, 109, 114, 115, 
                                            124, 133, 157, 159, 167, 170, 171) ~ 8L,           #Latin America & Caribbean
                             corigin %in% c(22, 31, 32, 78, 123, 132, 153, 222) ~ 9L,          #East Europe
                             corigin %in% c(23, 25, 38, 40, 42, 43, 44, 50, 65, 66, 68, 74, 
                                            77, 82, 83, 85, 91, 97, 100, 104, 128, 137, 145,
                                            141, 148, 154, 155, 163, 169, 172, 177, 181, 182) ~ 10L,     #Asia
                             corigin %in% c(24, 30, 33, 39, 46, 52, 60, 67, 76, 79, 81, 87, 90,
                                            111, 126, 136, 149, 152, 161, 180, 193) ~ 11L,               #Middle East
                             corigin %in% c(36, 37, 47, 49, 53, 54, 57, 80, 84, 86, 89, 94, 95,
                                            102, 105, 110, 113, 125, 127, 129, 131, 135, 138, 139,
                                            142, 143, 144, 147, 150, 151, 156, 158, 160, 162, 166,
                                            173, 174, 176, 178, 179, 183, 190) ~ 12L,                    #Africa
                             TRUE ~ NA_integer_))

ppath %<>% select(-c(loc1989, birthregion, corigin))
ppath %<>% mutate(germborn = if_else(germborn == 2, 0, germborn))
ppath %<>% mutate(immiyear = ifelse(immiyear %in% c(-1,-2), NA, immiyear))
ppath %<>% mutate(todjahr = ifelse(todjahr %in% c(-1,-2), NA, todjahr))
ppath %<>% filter(!(migback == 2 & is.na(immiyear)))

bioimmig <- read_csv("bioimmig.csv")
bioparen <- read_csv("bioparen.csv") %>%
  select(cid, pid, fsedu, msedu, freli, mreli, locchildh, fnat, mnat, morigin, forigin)
biobirth <- read_csv("biobirth.csv") %>%
  select(cid, pid, sumkids)
biojob <- read_csv("biojob.csv") %>%
  select(cid, pid, agefjob)
bioage <- read_csv("bioage17.csv")

biosoc <- read_csv("biosoc.csv") %>%
  select(cid, pid, syear, bsschwo, bsschla, bsklausl, bsschzuk) %>% 
  group_by(cid, pid) %>% 
  distinct()

pgen <- read_csv("pgen.csv") %>%
  select(
    cid, pid, syear, pgfamstd, pglabnet, pgstib, pgemplst,
    pglfs, pgjobch, pgisco88, pgerljob, pgausb, pgerwzeit, pgtatzeit,
    pgexpft, pgexppt, pgexpue, pgbilzeit, pgpsbil, pgpsbil,
    pgpbbil01, pgpbbil02, pgpbbil03, pgpsbilo, pgpbbilo, pgpsbila, pgpbbila
  ) %>%
  group_by(cid, pid) %>%
  mutate(
    occupation = case_when(
      pgstib %in% c(11, 110, 120, 130, 140, 150) ~ 1L, # Student
      pgstib %in% c(210, 220, 310, 510, 520, 521, 522, 610) ~ 2L, # Unskilled
      pgstib %in% c(230, 240, 250, 320, 330, 340, 420, 421, 422, 423, 530, 540, 550, 620, 630, 640) ~ 3L, # Skilled
      pgstib %in% c(410, 411, 412, 413, 430, 431, 432, 433) ~ 4L, # Self-employed
      pgstib %in% c(10, 12, 440) ~ 5L, # Home
      pgstib %in% c(15) ~ 6L, # Military
      pgstib %in% c(13) ~ 7L
    ), # Retired
    across(where(is.numeric), ~ ifelse(.x < 0, NA, .x)),
    education_east = ifelse(pgpsbilo > 0 & pgpbbilo > 0, 1, 0),
    education_abroad = ifelse(pgpsbila > 0 & pgpbbila > 0, 1, 0),
    education_east = ifelse(is.na(education_east), 0, education_east),
    education_abroad = ifelse(is.na(education_abroad), 0, education_abroad)
  ) %>%
  rename(
    marital_status = pgfamstd,
    net_earnings = pglabnet,
    employment = pgemplst,
    participation = pglfs,
    job_change = pgjobch,
    isco = pgisco88,
    trained = pgerljob,
    education = pgbilzeit,
    need_edu = pgausb,
    weekly_hours = pgtatzeit,
    experience_firm = pgerwzeit,
    experience_fulltime = pgexpft,
    experience_parttime = pgexppt,
    experience_unemp = pgexpue,
    year = syear
  ) %>%
  select(-starts_with("pg")) %>%
  filter(!is.na(isco) | !is.na(occupation))

#Duration of stay
stay <- bioimmig %>% select(pid, cid, syear, biresper) %>% filter(biresper > 0) %>%
  pivot_wider(id_cols = c(pid, cid), names_from = syear, values_from = biresper) %>%
  rowwise() %>% mutate(stay = c_across(where(is.numeric)) %>% unique() %>% na.omit() %>% list()) %>%
  select(cid, pid, stay) %>%
  mutate(stay = ifelse(length(stay) > 1, 3, unlist(stay)))

#Joins
dataset <- left_join(ppath, stay, by = c("pid", "cid")) %>%
  left_join(bioparen, by = c("cid", "pid")) %>% 
  left_join(biobirth, by = c("cid", "pid")) %>%
  left_join(biojob, by = c("cid", "pid")) %>%
  left_join(biosoc, by = c("cid", "pid")) %>%
  rename(education_expectations = bsschzuk,
         education_interculturality = bsklausl) %>%
  mutate(across(where(is.numeric), ~ ifelse(.x < 0, NA, .x))) %>%
  right_join(pgen, by = c("cid", "pid")) %>%
  mutate(education_east = if_else(bsschwo == 2 | bsschla %in% c(11, 12, 13, 14, 15, 16, 18), 1, education_east, education_east),
         education_abroad = if_else(bsschwo == 3, 1, education_abroad, education_abroad))

# Choose only time-invariant variables
info <- dataset %>% select(0:22) %>% distinct()

#Filter
dataset %<>% filter(!is.na(sex), !is.na(gebjahr), !is.na(occupation))
dataset %<>% select(-starts_with("bss"))
dataset %<>% mutate(east = if_else(is.na(east), 0, east))
dataset %<>% group_by(cid, pid)
dataset %<>% mutate(
  years_germany = year - immiyear,
  age = year - gebjahr
)

heatmap <- dataset %>% group_by(occupation, migback) %>% count() 

ggplot(info) + geom_bar(aes(x=gebjahr))
ggplot(info) + geom_bar(aes(x=gebjahr, fill = factor(sex)))
ggplot(info) + geom_bar(aes(x=gebjahr, fill = factor(migback)))
ggplot(info %>% drop_na(immiyear)) + geom_bar(aes(x=gebjahr, fill = factor(immiyear)))
ggplot(info) + geom_bar(aes(x=gebjahr, fill = factor(east)))
ggplot(info) + geom_bar(aes(x=gebjahr, fill = factor(culture)))
ggplot(info) + geom_bar(aes(x=gebjahr, fill = factor(agefjob)))
ggplot(info) + geom_bar(aes(x=agefjob)) + facet_grid(~ migback, )
ggplot(info) + geom_bar(aes(x=gebjahr, fill = factor(sex))) + facet_grid(~ migback, )

ggplot(heatmap) + geom_tile(aes(x=migback, y=occupation, fill = n)) +
  scale_fill_gradient(name = "Count",
                      low = "#FFFFFF",
                      high = "#012345")

ggplot(dataset) + geom_bar(aes(x=year, fill = factor(occupation))) + facet_grid(~ migback, )
ggplot(dataset) + geom_bar(aes(x=year, fill = factor(education))) + facet_grid(~ migback, )
ggplot(dataset) + geom_bar(aes(x=age, fill = factor(job_change))) + facet_grid(~ migback, )
ggplot(dataset ) + geom_bar(aes(x=age, fill = factor(occupation)))
