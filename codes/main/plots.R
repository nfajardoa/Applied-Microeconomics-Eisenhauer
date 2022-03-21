### PACKAGES LOADING AND INSTALLATION

## Set directory
setwd("Documents/MSc Bonn/Applied Micro/SOEP")

## Package names
packages <- c("haven", "dplyr", "DBI", "dbplyr", "purrr", "readxl", "glue", 
              "stringr", "magrittr", "tidyr", "lubridate", "tidyverse", "bit64", 
              "stringi", "sjmisc", "stringdist", "textshape", "textclean", "statar")

## Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

## Packages loading
invisible(lapply(packages, library, character.only = TRUE))

## Options
options(future.globals.maxSize = 891289600)

### Custom functions
step_imputation <- function(x) {
  if (x %>% length() %>% `==`(1)) {
    return(x)
  } else {
    value <- x %>% median(na.rm = TRUE)
    x[is.na(x)] <- value
    return(x)
  }
}

### Data
## Classifications
# Language/Culture distances
culture_language <- read_csv("language_distance.csv") %>%
  select(-variable, -label) %>%
  rename(
    corigin = value,
    language_distance = ldnd
  ) %>%
  mutate(across(c(1, 3, 4), as.integer),
    x10_language_distance = xtile(language_distance, 10),
    x5_language_distance = xtile(language_distance, 5),
    x4_language_distance = xtile(language_distance, 4)
  )

# O*NET Job Skills
language_writing <- read_csv("Writing.csv") %>%
  rename(writing_importance = Importance, writing_level = Level)
language_speaking <- read_csv("Speaking.csv") %>%
  rename(speaking_importance = Importance, speaking_level = Level)
language_reading <- read_csv("Reading_Comprehension.csv") %>%
  rename(reading_importance = Importance, reading_level = Level)
language_listening <- read_csv("Active_Listening.csv") %>%
  rename(listening_importance = Importance, listening_level = Level)
language_proficiency <- read_csv("English_Language.csv") %>%
  rename(language_importance = Importance, language_level = Level) %>%
  mutate(across(matches("_"), as.numeric))

# SOC-10 -> ISCO Crosswalk
crosswalk <- read_dta("onetsoc_to_isco_cws_ibs/soc10_isco08.dta")

# ISCO-88 -> ISCO-08
isco_reclassification <- read_excel("index08-draft.xlsx") %>%
  mutate(across(1:2, as.numeric)) %>%
  group_by(isco88) %>%
  summarise(across(everything(), list)) %>%
  mutate(isco08 = map_dbl(isco08, min))

soc_skills <- ls(pattern = "language_") %>%
  map(~ parse(text = .x) %>% eval()) %>%
  reduce(full_join, by = c("Code", "Occupation")) %>%
  mutate(soc10 = str_remove_all(Code, "-") %>% as.numeric()) %>%
  select(-c(Code, Occupation))

## ISCO Language Skills
isco_skills <- left_join(crosswalk, soc_skills) %>%
  group_by(isco08) %>%
  summarise(across(matches("_"), median)) %>%
  rowwise() %>%
  mutate(isco3 = str_sub(isco08, 1, 3)) %>%
  group_by(isco3) %>%
  mutate(across(matches("_"), step_imputation)) %>%
  rowwise() %>%
  mutate(isco2 = str_sub(isco08, 1, 2)) %>%
  group_by(isco2) %>%
  mutate(
    across(matches("_"), step_imputation),
    isco_skill = case_when(
      between(isco08, 110, 199) | between(isco08, 1100, 1399) | between(isco08, 1500, 2999) ~ 4,
      between(isco08, 1400, 1499) | between(isco08, 3000, 3999) ~ 3,
      between(isco08, 210, 299) | between(isco08, 4000, 8999) ~ 2,
      between(isco08, 310, 399) | between(isco08, 9000, 9999) ~ 1
    )
  ) %>%
  group_by(isco_skill) %>%
  mutate(across(matches("_"), step_imputation)) %>%
  rowwise() %>%
  mutate(
    skill1_importance = c_across(matches("_importance")) %>% list(),
    skill1_level = c_across(matches("_level")) %>% list(),
    skill1 = (skill1_importance %*% skill1_level) / 500,
    skill2 = (language_level*language_importance) / 100
  ) %>%
  ungroup() %>%
  mutate(
    skill1 = (skill1 - min(skill1)) / (max(skill1) - min(skill1)) * 100,
    skill2 = (skill2 - min(skill2)) / (max(skill2) - min(skill2)) * 100,
    x4_skill1 = xtile(skill1, n = 4),
    x4_skill2 = xtile(skill2, n = 4)) %>%
  select(isco08, matches("skill"), -matches("(importance)|(level)")) %>%
  rename(isco = isco08)

## Household information
# Location
hbrutto <- read_csv("hbrutto.csv") %>%
  select(cid, syear, bula) %>%
  rename(year = syear) %>%
  mutate(across(where(is.numeric), ~ ifelse(.x < 0, NA, .x))) %>%
  distinct()

## Bio/Migrant information
# Parental information
bioparen <- read_csv("bioparen.csv") %>%
  select(cid, pid, locchildh, fnat, mnat, morigin, forigin) %>%
  mutate(across(where(is.numeric), ~ ifelse(.x < 0, NA, .x)))

# Tracking information
ppath <- read_csv("ppath.csv") %>%
  select(cid, pid, sex, gebjahr, todjahr, immiyear, germborn, corigin, migback, birthregion, loc1989, arefback) %>%
  group_by(pid) %>%
  filter(sex > 0, gebjahr >= 1950) %>%
  mutate(across(where(is.numeric), ~ ifelse(.x < 0, NA, .x))) %>%
  mutate(
    germborn = if_else(germborn == 2, 0L, 1L),
    female = if_else(sex == 2, 1L, 0L),
    migration_age = immiyear - gebjahr
  ) %>%
  select(-sex) %>%
  left_join(bioparen, by = c("cid", "pid")) %>% #Join with parental info
  filter(!(migback == 2 & is.na(immiyear))) %>% #
  mutate(
    missing_nat = if_else(is.na(fnat) & is.na(mnat), 1, 0),
    missing_origin = if_else(is.na(forigin) & is.na(morigin), 1, 0)
  ) %>%
  filter(!(migback == 3 & missing_nat == 1 & missing_origin == 1)) %>%
  filter(!(migback == 3 & missing_origin == 1)) %>%
  mutate(
    german_birth = case_when(
      fnat == 1 | mnat == 1 ~ 1, # Either of parents is german
      is.na(fnat) ~ mnat, # Missing fnat, then mnat
      is.na(mnat) ~ fnat, # Missing mnat, then fnat
      TRUE ~ 0
    ),
    german_birth = if_else(german_birth == 2, 0, german_birth) # Normalize to 0/1 dummy
  ) %>%
  left_join(culture_language %>% rename(morigin = corigin), by = "morigin") %>%
  left_join(culture_language %>% rename(forigin = corigin), suffix = c(".mother", ".father"), by = "forigin") %>%
  left_join(culture_language, by = "corigin") %>%
  mutate(hybrid = if_else(culture.father == culture.mother, 0, 1)) %>%
  filter(!is.na(corigin), !is.na(culture)) %>%
  mutate(
    culture = case_when(
      culture.mother == 1 & culture.father == 1 ~ 1L, # Either parents is German
      is.na(culture.mother) & is.na(culture.father) ~ culture, # Missing parental Info, then personal Info
      is.na(culture.mother) ~ culture.father, # Missing mothers Culture, then fathers Culture
      is.na(culture.father) ~ culture.mother, # Missing fathers Culture, then mothers Culture
      culture.mother == culture.father ~ culture.mother, # Same parental Culture
      culture.mother != culture.father ~ 0L # Hybrid id
    ),
    language_cost = if_else(migback %in% c(1, 2), min(language_distance.mother, language_distance.father, language_distance, na.rm = TRUE), language_distance),
    language_cost = if_else(migback == 3, min(language_distance.mother, language_distance.father, na.rm = TRUE), language_distance),
    migback = case_when(
      migback == 2 & migration_age <= 7 ~ 3L,
      migback == 1 & culture != 1 ~ 3L,
      TRUE ~ as.integer(migback)
    ),
    east_birth = case_when(
      loc1989 == 1 ~ 1L,
      loc1989 > 1 ~ 0L,
      is.na(loc1989) & (birthregion %in% c(11, 12, 13, 14, 15, 16)) ~ 1L,
      is.na(loc1989) & (birthregion %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) ~ 0L,
      is.na(loc1989) ~ NA_integer_
    )
  ) %>%
  select(cid, pid, female, gebjahr, todjahr, immiyear, migback, arefback, migration_age, 
         germborn, east_birth, german_birth, culture, culture2, hybrid, matches("language"), locchildh)

# Immigration information
bioimmig <- read_csv("bioimmig.csv") %>%
  select(cid, pid, biimgrp, biresper) %>%
  group_by(pid) %>%
  summarise(across(where(is.numeric), max, na.rm = TRUE), .groups = "keep") %>%
  rowwise() %>%
  mutate(across(where(is.numeric), ~ ifelse(.x < 0, NA, .x))) %>%
  filter(!(is.na(biimgrp) & is.na(biresper))) %>%
  mutate(germannat = if_else(between(biimgrp, 1, 2), 1, 0))

# Number of children
biobirth <- read_csv("biobirth.csv") %>%
  select(cid, pid, sumkids, kidgeb01) %>%
  mutate(across(where(is.numeric), ~ ifelse(.x < 0, NA, .x))) %>%
  rename(year_child = kidgeb01)

# Age of first job
biojob <- read_csv("biojob.csv") %>%
  select(cid, pid, agefjob) %>%
  mutate(across(where(is.numeric), ~ ifelse(.x < 0, NA, .x)))

# Expectations
biosoc <- read_csv("biosoc.csv") %>%
  select(pid, syear, bsschwo, bsschla, bsklausl, bsschzuk) %>%
  group_by(pid) %>%
  distinct() %>%
  mutate(across(where(is.numeric), ~ ifelse(.x < 0, NA, .x))) %>%
  rename(
    year = syear,
    education_expectations = bsschzuk,
    education_interculturality = bsklausl
  )

#Time-varying data
pgen <- read_csv("pgen.csv") %>%
  select(
    cid, pid, syear, pgnation, pgpartnr, pgfamstd, pglabnet, pgstib, pgemplst,
    pgisco88, pgisco08, pgisced97, pgisced11, pgbilzeit, pgerwzeit, pgtatzeit, pgexpft,
    pgexppt, pgexpue, pgpsbil, pgpsbilo, pgpbbilo, pgpsbila, pgpbbila
  ) %>%
  mutate(across(where(is.numeric), ~ ifelse(.x < 0, NA, .x))) %>%
  group_by(pid) %>%
  mutate(
    occupation_group = case_when(
      pgstib %in% c(11, 110, 120, 130, 140, 150) ~ 1L, # Student
      pgstib %in% c(210, 220, 310, 510, 520, 521, 522, 610) ~ 2L, # Unskilled
      pgstib %in% c(230, 240, 250, 320, 330, 340, 420, 421, 422, 423, 530, 540, 550, 620, 630, 640) ~ 3L, # Skilled
      pgstib %in% c(410, 411, 412, 413, 430, 431, 432, 433) ~ 4L, # Self-employed
      pgstib %in% c(10, 12, 440) ~ 5L, # Home
      pgstib %in% c(15) ~ 6L, # Military
      pgstib %in% c(13) ~ 7L # Retired
    ),
    across(where(is.numeric), ~ ifelse(.x < 0, NA, .x)),
    education_east = ifelse(pgpsbilo > 0 | pgpbbilo > 0, 1L, 0L),
    education_abroad = ifelse(pgpsbila > 0 | pgpbbila > 0, 1L, 0L),
  ) %>%
  rename(
    year = syear,
    german = pgnation,
    ppid = pgpartnr,
    marital_status = pgfamstd,
    net_earnings = pglabnet,
    employment = pgemplst,
    occupation = pgstib,
    isco08 = pgisco08,
    isco88 = pgisco88,
    isced97 = pgisced97,
    isced11 = pgisced11,
    years_education = pgbilzeit,
    weekly_hours = pgtatzeit,
    experience_firm = pgerwzeit,
    experience_fulltime = pgexpft,
    experience_parttime = pgexppt,
    experience_unemp = pgexpue,
  ) %>%
  select(-matches("pg")) %>%
  ungroup() %>%
  rename(isco08_original = isco08) %>%
  left_join(isco_reclassification %>% select(-name), by = "isco88") %>%
  mutate(isco = case_when(
    !is.na(isco08_original) ~ as.integer(isco08_original),
    is.na(isco08_original) & !is.na(isco08) ~ as.integer(isco08),
    occupation %in% c(10, 11, 12, 13, 15, 110, 120, 130, 140, 150) ~ 0L,
    TRUE ~ NA_integer_
  )) %>%
  select(-c(isco08, isco08_original, isco88)) %>%
  left_join(ppath %>% select(-cid)) %>%
  group_by(pid) %>%
  nest() %>%
  mutate(check = map_lgl(data, ~ .x$isco %>%`==`(-1) %>% all())) %>%
  filter(check == FALSE) %>%
  select(-check) %>%
  unnest(data) %>%
  left_join(isco_skills) %>%
  filter(!is.na(isco), !is.na(female)) %>%
  mutate(age = year - gebjahr)

# #Duration of stay
# stay <- bioimmig %>% select(pid, cid, syear, biresper) %>% filter(biresper > 0) %>%
#   pivot_wider(id_cols = c(pid, cid), names_from = syear, values_from = biresper) %>%
#   rowwise() %>% mutate(stay = c_across(where(is.numeric)) %>% unique() %>% na.omit() %>% list()) %>%
#   select(cid, pid, stay) %>%
#   mutate(stay = ifelse(length(stay) > 1, 3, unlist(stay)))

#Joins
dataset <- pgen %>%
  left_join(biobirth, by = c("cid", "pid")) %>%
  left_join(biojob, by = c("cid", "pid")) %>%
  left_join(pgen %>% select(cid, pid, year, matches("(culture)|(language)")) %>% rename(ppid = pid), 
            by = c("cid", "ppid", "year"),
            suffix = c("", ".p")) %>%
  left_join(biosoc, by = c("pid", "year")) %>%
  left_join(hbrutto, by = c("cid", "year")) %>%
  filter(!is.na(female), !is.na(gebjahr), !is.na(occupation)) %>%
  mutate(
    education_east = if_else(bsschwo == 2 | bsschla %in% c(0, 11, 12, 13, 14, 15, 16, 18) | education_east == 1, 1L, education_east),
    education_abroad = if_else(bsschwo == 3 | education_abroad == 1, 1L, education_abroad),
    years_germany = year - immiyear,
    year_child = year_child >= year,
    married_german = if_else(marital_status != 3 & culture.p == 1, 1L, 0L),
    east = case_when(
      bula %in% c(11, 12, 13, 14, 15, 16) ~ 1L,
      bula %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) ~ 0L)
  ) %>%
  select(-starts_with("bssch")) %>%
  nest()

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
ggplot(dataset ) + geom_bar(aes(y = (..count..)/sum(..count..), x=age, fill = factor(occupation)))

write.csv(dataset, "full_data_0401.csv")
write.csv(names, "language_distance.csv")

write_csv(info, "time_invariant_data.csv")

dataset %<>% filter(age >= 17)

dataset <- read_csv("full_data_0401.csv", guess_max = 400000) %>%
  select(-X1) %>%
  group_by(cid, pid) %>%
  filter(between(age, 17, 80))

dataset %>% 
  select(cid, pid, culture) %>%
  distinct() %>%
  group_by(culture) %>%
  count()

ggplot(dataset) + geom_point(aes(y=skill1, x=language_cost, color = factor(migback)))
ggplot(dataset) + geom_col(aes(x=age, y=..count.., fill = isco)) + facet_grid(. ~ migback)
ggplot(dataset) + geom_point(aes(x=isco, y=occupation))

dataset %>% group_by(isco, occupation) %>% count() %>% View()

occupations <- test3 %>% 
  group_by(year, skill_group, language_group, migback) %>%
  count(name = "obs") %>% 
  group_by(year, language_group, migback) %>%
  mutate(percentage = (obs/sum(obs))*100)

%>%
  ungroup() %>% mutate(occupation = factor(occupation, levels = 1:7, labels = c("Student", "Unskilled", "Skilled", "Self-Empoyed", "Home", "Military", "Retired")))
  
ggplot(occupations) + geom_col(aes(percentage, x=year, fill = skill_group)) + facet_grid(language_group ~ migback) + theme_minimal()
ggplot(info) + geom_bar(aes(x=culture)) + facet_grid(.~ migback)

language_skill <- read_excel("ISCO-occ choice language skills.xlsx")
costs_language <- read_excel("ld_bilateral.xlsx") %>% 
  filter(cty1name == "GERMANY") %>%
  mutate(across(where(is.character), str_to_lower)) %>%
  rename(label = cty2name) %>%
  select(label, ldnd) %>%
  mutate(ldnd = as.numeric(ldnd),
         ldnd = (ldnd - min(ldnd))/(max(ldnd) - min(ldnd))*100)

  
names <- read_csv("ppathl_values.csv") %>% 
  filter(variable == "morigin", value > 0) %>%
  mutate(across(where(is.character), str_to_lower)) %>%
  select(variable, value, label) %>%
  left_join(language_costs, by = "label")v

names_m <- read_csv("bioparen_values.csv") %>% 
  filter(variable == "morigin", value > 0) %>%
  mutate(across(where(is.character), str_to_lower))

names_f <- read_csv("bioparen_values.csv") %>% 
  filter(variable == "forigin", value > 0) %>%
  mutate(across(where(is.character), str_to_lower))

names %>% filter(is.na(ldnd)) %>% View()
names <- read_csv("language_distance.csv")

test <- bioparen %>% select(cid, pid, morigin, forigin) %>%
  mutate(across(where(is.numeric), ~ ifelse(.x < 0, NA, .x))) %>%
  left_join(language_culture %>% rename(morigin = corigin), by = "morigin") %>%
  left_join(language_culture %>% rename(forigin = corigin), suffix = c(".mother", ".father"), by = "forigin") %>% 
  group_by(culture.father, culture.mother) %>%
  count() %>%
  rowwise() %>%
  mutate(culture.mother = if_else(is.na(culture.mother), culture.father, culture.mother),
         culture.father = if_else(is.na(culture.father), culture.mother, culture.father)) %>%
  drop_na() %>% 
  group_by(culture.mother, culture.father) %>%
  summarise(n = sum(n)) %>%
  tail(-1)

ggplot(test) + geom_tile(aes(x=culture.mother, y=culture.father, fill = n))

ppath %>% group_by(migback) %>% count()
