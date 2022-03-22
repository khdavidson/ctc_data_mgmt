# Catch data collation to update catch database
# started Mar 2022 

setwd("~/ANALYSIS/data/ctc_data_mgmt")

library(tidyverse)
library(readxl)
library(openxlsx)


# DATA SOURCE OUTLINE -----------------------------

#### FOS (full and split data): 
#   NBC commercial & test
#   CBC commercial & test
#   Fraser commercial & test
#   SBC commercial & test
comm.test.raw <- read_excel("FOS_SAS_In-Season Catch - Comm & Test 2021 - DL17dec2021.xlsx", sheet="Comm & Test 2021")
seineLSL.raw <- read_excel("FOSp_DAVIDSONKA_SEINE_legal-sublegal_DL5jan2022.xlsx", sheet="fos_DAVIDSONKA_fosprod_commsein", skip=2)
trollLSL.raw <- read_excel("FOSp_DAVIDSONKA_TROLL_legal-sublegal_DL5jan2022.xlsx", sheet="FOSp_DAVIDSONKA_TROLL_legal-sub")

#### Post-season report: 
#   Transboundary commercial & FN (& rec, but none) 
tb.raw <- read_excel("Individual Catch Tables.xlsx", sheet="2. TB", skip=1)
#   NBC/CBC FN & rec
nbc.raw <- read_excel("Individual Catch Tables.xlsx", sheet="3. NBC", skip=1)
#   SBC FN 
sbc.raw <- read_excel("Individual Catch Tables.xlsx", sheet="4. Southern BC", skip=1)
#   Fraser FN
fraser.fn.raw <- read_excel("Individual Catch Tables.xlsx", sheet="5. Fraser", skip=1)

#### Creel/iREC:
#   SBC rec
creel.irec.raw <- read_excel("2021 Combined Creel & iREC Catch.xlsx", sheet="Summary")   #<-- export from creel_irec_merge.R

#### External:
#   Fraser River f/w sport Chuck
fraser.rec.raw <- read_excel("Summary Kept and Released CN in Fraser River Fisheries 2015-2021 (2022-02-18) for CTC IM.xlsx",
                             sheet="Kept-Released CNCO")
#   NBC/CBC Chelsea (confirm post-season report), Antonio (Atnarko, if decide to do regression)
#   SBC Joan Bennett 
sbc.rec.raw <- read_excel("SOG_CN_rec_catch_template2021jb(Feb0222).xlsx", sheet="2021 season", n_max=1)
# ------------------------------------------------


# TEMPLATE:
template.raw <- read_excel("catch_database_template.xlsx", sheet="temp")




################################################################################################################################################

#                                                                 CLEAN 


# =============== FOS DATA PREP AND EXTRATION =============== 

# Kept and total released ---------------------------------
comm.test <- comm.test.raw %>%
  mutate(Region_not_used = case_when(M_AREA%in%c(1,2,3,4,5,101,102,103,104,105,142)~"Northern British Columbia",
                                     M_AREA%in%c(6,7,8,9,10,106,107,108,109)~"Central British Columbia",
                                     M_AREA%in%c(11,12,13)~"Johnstone Strait",
                                     M_AREA%in%c(13,14,15,16,17,18,"19A",28)~"Georgia Strait",
                                     M_AREA%in%c("19B",20)~"Juan de Fuca",
                                     M_AREA%in%c(21,22,23,24,25,26,27,123,124,125,126,127)~"West Coast Vancouver Island",
                                     M_AREA%in%c(29)&grepl("Fraser", DESCRIPTION)|grepl("Fraser", MRP_CR_NAME)~"Fraser River",
                                     M_AREA%in%c(29)&!grepl("Fraser", DESCRIPTION)~"Georgia Strait")) %>%
  group_by(FISHERY_TYPE, Region_not_used, GEAR, CNR) %>%
  summarize(ck_kept = sum(CN_K), ck_rel = sum(CN_R)) %>%
  print()


# Seine release split data ---------------------------------
seine.releases <- seineLSL.raw %>% 
  mutate(CNR = case_when(TARGETS_CHINOOK=="YES"~1,
                         TARGETS_CHINOOK=="NO"~0)) %>%
  group_by(MGMT_AREA, AREA_NAME, OPNG_DESC, CNR) %>%
  summarize(adult_ck_kept=sum(ADULT_CHINOOK_KEPT), jack_ck_kept=sum(JACK_CHINOOK_KEPT), 
            adult_ck_reld = sum(ADULT_CHINOOK_RELD), jack_ck_reld = sum(JACK_CHINOOK_RELD)) %>%
  mutate(Region_not_used = case_when(MGMT_AREA%in%c(1,2,3,4,5,101,102,103,104,105,142)~"Northern British Columbia",
                                     MGMT_AREA%in%c(6,7,8,9,10,106,107,108,109)~"Central British Columbia",
                                     MGMT_AREA%in%c(11,12,13)~"Johnstone Strait",
                                     MGMT_AREA%in%c(13,14,15,16,17,18,"19A",28)~"Georgia Strait",
                                     MGMT_AREA%in%c("19B",20)~"Juan de Fuca",
                                     MGMT_AREA%in%c(21,22,23,24,25,26,27,123,124,125,126,127)~"West Coast Vancouver Island",
                                     MGMT_AREA%in%c(29)~"Fraser River")) %>%
  group_by(CNR, Region_not_used) %>%
  summarize(legal_ck_reld = sum(adult_ck_reld), sublegal_ck_reld = sum(jack_ck_reld)) %>%
  mutate(GEAR = "SN", FISHERY_TYPE="COMMERCIAL") %>%
  print()


# TROLL release split data ------------------------
troll.releases <- trollLSL.raw %>%
  mutate(CNR = case_when(TARGETS_CHINOOK=="YES"~1,
                         TARGETS_CHINOOK=="NO"~0),
         Region_not_used = case_when(MGMT_AREA%in%c(1,2,3,4,5,101,102,103,104,105,142)~"Northern British Columbia",
                                     MGMT_AREA%in%c(6,7,8,9,10,106,107,108,109)~"Central British Columbia",
                                     MGMT_AREA%in%c(11,12,13)~"Johnstone Strait",
                                     MGMT_AREA%in%c(13,14,15,16,17,18,"19A",28)~"Georgia Strait",
                                     MGMT_AREA%in%c("19B",20)~"Juan de Fuca",
                                     MGMT_AREA%in%c(21,22,23,24,25,26,27,123,124,125,126,127)~"West Coast Vancouver Island",
                                     MGMT_AREA%in%c(29)&grepl("Fraser", OPNG_DESC)~"Fraser River",
                                     MGMT_AREA%in%c(29)&!grepl("Fraser", OPNG_DESC)~"Georgia Strait")) %>%
  group_by(CNR, Region_not_used) %>%
  summarize(legal_ck_reld = sum(LEGAL_CHINOOK_RELD), sublegal_ck_reld=sum(SUB_LEGAL_CHINOOK_RELD)) %>%
  mutate(GEAR="TR", FISHERY_TYPE="COMMERCIAL") %>%
  print()






# =============== POST-SEASON REPORT DATA ===============
# be aware: if there is zero catch in a fishery type (e.g., TB recreational), will remove the placeholder row

# Transboundary ---------------------------
tb <- tb.raw %>%
  filter(!grepl("Total", `Licence Group`, ignore.case=TRUE)) %>%
  fill(`Licence Group`) %>%
  mutate(fishery_type = `Licence Group`,
         `Licence Group` = NA) %>%
  filter(!is.na(`Fishing Area`)) %>% 
  mutate(across(c("Sockeye Kept":"Chinook Released"), ~case_when(.=="-"~0, TRUE~as.numeric(.))),
         across(c("Sockeye Kept":"Chinook Released"), ~case_when(is.na(.)~0, TRUE~as.numeric(.))),
         Region_not_used = "Transboundary Rivers",
         fishery_type = case_when(grepl("First Nations", fishery_type)~"FN",
                                  fishery_type=="Commercial"~"commercial",
                                  fishery_type=="Recreational"~"recreational"),
         gear = case_when(fishery_type%in%c("commercial","FN")~"Gillnet",
                          fishery_type=="recreational"~"Sport"),
         catch_reported_comment = ifelse(fishery_type=="commercial", "From preliminary post-season report Appendix 2, Commercial",
                                    ifelse(fishery_type=="FN", "From preliminary post-season report Appendix 2, FSC & Treaty", NA)),
         release_legals_comment = ifelse(gear=="Gillnet" & fishery_type%in%c("FN", "commercial"), 
                                 "From preliminary post-season report Appendix 2, Commercial. Assumption that all releases are sublegals for GN.",
                                 NA),
         release_sublegals_comment = ifelse(gear=="Gillnet" & fishery_type%in%c("FN", "commercial"), 
                                    "From preliminary post-season report Appendix 2, Commercial. Assumption that all releases are sublegals for GN.",
                                    NA),
         release_sublegals = ifelse(gear=="Gillnet" & fishery_type%in%c("FN", "commercial"), `Chinook Released`, NA),
         release_legals = `Chinook Released`-release_sublegals) %>%
  select(-c(`Licence Group`, `Chinook Released`, `Sockeye Kept`:`Chum Released`)) %>%
  rename(sector=fishery_type,
         Area_group=`Fishing Area`,
         catch_reported=`Chinook Kept`) %>%
  print()



# Northern BC ---------------------------
nbc <- nbc.raw %>%
  filter(!grepl("Total", `Licence Group`, ignore.case=TRUE)) %>%
  fill(`Licence Group`) %>%
  filter(!is.na(`Fishing Area`)) %>% 
  mutate(fishery_type = case_when(grepl("Seine|Gillnet|Troll", `Licence Group`)~"commercial", 
                                  grepl("First Nations", `Licence Group`)~"FN",
                                  `Licence Group`=="Recreational"~"recreational"),
         `Licence Group` = ifelse(grepl("Area", `Licence Group`), `Licence Group`, NA),
         across(c("Sockeye Kept":"Chinook Released"), ~case_when(.=="-"~0, TRUE~as.numeric(.))),
         across(c("Sockeye Kept":"Chinook Released"), ~case_when(is.na(.)~0, TRUE~as.numeric(.))),
         Region_not_used = ifelse(`Fishing Area`=="Central Coast","Central British Columbia", "Northern British Columbia"),
         gear = case_when(fishery_type=="FN"~"Gillnet",
                          fishery_type=="recreational"~"Sport"),
         Year=2021,
         catch_reported_comment = ifelse(fishery_type=="recreational", 
                                         "From preliminary post-season report Appendix 3, Recreational **REQUIRES NC STAD CONFIRMAITON.**",
                                    ifelse(fishery_type=="FN", 
                                           "From preliminary post-season report Appendix 3, FSC & Treaty. **REQUIRES NC STAD CONFIRMAITON.**", 
                                           NA)),
         release_legals_comment = ifelse(gear=="Gillnet" & fishery_type=="FN", 
                                         "From preliminary post-season report Appendix 3, FSC & Treaty. Assumption that all releases are sublegals for GN.",
                                    ifelse(fishery_type=="recreational",
                                           "From preliminary post-season report Appendix 3, Recreational. No split data available; release legals may include sublegals.",NA)),
         release_sublegals_comment = ifelse(gear=="Gillnet" & fishery_type=="FN", 
                                            "From preliminary post-season report Appendix 3, FSC & Treaty. Assumption that all releases are sublegals for GN.",
                                       ifelse(fishery_type=="recreational",
                                              "From preliminary post-season report Appendix 3, Recreational. No split data available; release legals may include sublegals.",NA)),
         release_sublegals = ifelse(gear=="Gillnet" & fishery_type=="FN", `Chinook Released`, NA),
         release_legals = ifelse(gear=="Gillnet" & fishery_type=="FN", `Chinook Released`-release_sublegals,
                            ifelse(fishery_type=="recreational", `Chinook Released`, NA))) %>%
  filter(fishery_type!="commercial") %>%
  select(-c(`Licence Group`, `Chinook Released`, `Sockeye Kept`:`Chum Released`)) %>%
  rename(sector=fishery_type,
         Description=`Fishing Area`,
         catch_reported=`Chinook Kept`) %>%
  print()

# Extract NBC FN and rollup the Skeena+Nass estimates 
nbc.fn <- nbc %>%
  filter(sector=="FN") %>%
  group_by(Region_not_used) %>%
  summarize(catch_reported = sum(catch_reported), 
            release_legals=sum(release_legals), 
            release_sublegals=sum(release_sublegals),
            gear=unique(gear),
            catch_reported_comment=unique(catch_reported_comment),
            release_legals_comment=unique(release_legals_comment),
            release_sublegals_comment=unique(release_sublegals_comment),
            sector="FN") %>%
  print()



# Southern BC ---------------------------
sbc <- sbc.raw %>%
  filter(!grepl("Total", `Licence Group`, ignore.case=TRUE), !grepl("*reported", `Licence Group`)) %>%
  fill(`Licence Group`) %>%
  filter(if_any(c(`Fishing Area`, `Chinook Released`), ~ !is.na(.))) %>%   
  mutate(fishery_type = case_when(grepl("Area", `Licence Group`)~"commercial",
                                  `Licence Group`=="EO"~"FN", 
                                  grepl("Nations", `Licence Group`)~"FN",
                                  `Licence Group`=="Recreational"~"recreational")) %>%
  filter(fishery_type=="FN") %>%
  mutate(across(c("Sockeye Kept":"Chinook Released"), ~case_when(.=="-"~0, TRUE~as.numeric(.))),
         across(c("Sockeye Kept":"Chinook Released"), ~case_when(is.na(.)~0, TRUE~as.numeric(.))),
         `Fishing Area` = case_when(grepl("AABM", `Fishing Area`)~"WCVI - AABM",
                                    grepl("ISBM", `Fishing Area`)~"WCVI - ISBM",
                                    `Fishing Area`=="Johnstone Strait"~"Areas 11-12",
                                    `Fishing Area`=="Strait of Georgia"~"Areas 13-18, 19A, 28, 29",
                                    `Fishing Area`=="Juan de Fuca"~"Areas 19B, 20") , 
         Region_not_used = case_when(grepl("WCVI", `Fishing Area`)~"West Coast Vancouver Island",
                                     `Fishing Area`=="Areas 11-12"~"Johnstone Strait", 
                                     `Fishing Area`=="Areas 19B, 20"~"Juan de Fuca",
                                     `Fishing Area`=="Areas 13-18, 19A, 28, 29"~"Georgia Strait",
                                     TRUE~as.character(`Fishing Area`)),
         gear = ifelse(`Fishing Area`=="WCVI - AABM", "Troll", "Gillnet"),
         catch_reported_comment = "From preliminary post-season report Appendix 4, FSC & Treaty.", 
         release_legals_comment = ifelse(gear=="Gillnet", 
                                         "From preliminary post-season report Appendix 4, FSC & Treaty. Assumption that all releases are sublegals for GN.",
                                    ifelse(gear=="Troll",
                                           "From preliminary post-season report Appendix 4, FSC & Treaty. No split data available; release legals may include sublegals.",
                                           NA)),
         release_sublegals_comment = ifelse(gear=="Gillnet", 
                                            "From preliminary post-season report Appendix 4, FSC & Treaty. Assumption that all releases are sublegals for GN.",
                                       ifelse(gear=="Troll",
                                              "From preliminary post-season report Appendix 4, FSC & Treaty. No split data available; release legals may include sublegals.",
                                              NA)),
         release_sublegals = ifelse(gear=="Gillnet", `Chinook Released`, NA),
         release_legals = ifelse(gear=="Gillnet", `Chinook Released`-release_sublegals, `Chinook Released`),
         Fishery2 = ifelse(`Licence Group`=="Five Nations*", "Taaqwiihak",
                           ifelse(Region_not_used=="West Coast Vancouver Island" & 
                                    `Fishing Area`=="WCVI - AABM" & 
                                    `Licence Group`=="First Nations FSC and Treaty", "Maanulth",
                                  ifelse(Region_not_used=="West Coast Vancouver Island" & 
                                           `Fishing Area`=="WCVI - ISBM" & 
                                           `Licence Group`=="EO", "Hup/TSE",
                                         ifelse(Region_not_used=="West Coast Vancouver Island" & 
                                                  `Fishing Area`=="WCVI - ISBM" & 
                                                  `Licence Group`=="First Nations FSC and Treaty", "MNA/ntc/hup/tse", NA))))) %>%
  select(-c(`Licence Group`, `Chinook Released`, `Sockeye Kept`:`Chum Released`)) %>%
  rename(sector=fishery_type,
         Area_group=`Fishing Area`,
         catch_reported=`Chinook Kept`) %>%
  print()
  

# Fraser ---------------------------
fraser.fn <- fraser.fn.raw %>%
  filter(!grepl("Total", `Licence Group`, ignore.case=TRUE)) %>%
  fill(`Licence Group`) %>%
  filter(if_any(c(`Fishing Area`, `Chinook Released`), ~ !is.na(.))) %>%  
  mutate(fishery_type = case_when(`Licence Group`=="Commercial"~"commercial",
                                  grepl("Nations", `Licence Group`)~"FN",
                                  `Licence Group`=="Recreational"~"recreational")) %>%
  filter(fishery_type=="FN") %>%
  mutate(across(c("Sockeye Kept":"Chinook Released"), ~case_when(.=="-"~0, TRUE~as.numeric(.))),
         across(c("Sockeye Kept":"Chinook Released"), ~case_when(is.na(.)~0, TRUE~as.numeric(.))),
         Region_not_used = "Fraser River",
         FN_fishery = case_when(`Licence Group`=="First Nations FSC and Treaty"~"FSC",
                                `Licence Group`=="First Nations Commercial"~"EO"),
         gear="Gillnet",
         catch_reported_comment = "From preliminary post-season report Appendix 5, First Nations FSC & Treaty/Commercial.", 
         release_legals_comment = "From preliminary post-season report Appendix 5, First Nations FSC & Treaty/Commercial. Assumption that all releases are sublegals for GN.",
         release_sublegals_comment = "From preliminary post-season report Appendix 5, First Nations FSC & Treaty/Commercial. Assumption that all releases are sublegals for GN.",
         release_sublegals = ifelse(gear=="Gillnet", `Chinook Released`, NA),
         release_legals = ifelse(gear=="Gillnet", `Chinook Released`-release_sublegals, `Chinook Released`)) %>%
  select(-c(`Fishing Area`, `Licence Group`, `Chinook Released`, `Sockeye Kept`:`Chum Released`)) %>%
  rename(sector=fishery_type,
         catch_reported=`Chinook Kept`) %>%
  print()



# =============== CREEL/iREC DATA ===============
creel.irec <- creel.irec.raw %>% 
  unite(TYPE:SUB_TYPE, col="type", sep="-") %>%
  pivot_wider(names_from=type, values_from = total) %>%
  rename(Region_not_used=region_rollup,
         catch_reported=`Kept-LEGAL`,
         release_legals=`Released-LEGAL`,
         release_sublegals=`Released-SUB-LEGAL`,
         Description=group) %>%
  mutate(Description = case_when(Description=="iREC"~"iREC out of creel estimates",
                                 Description=="creel"~"from CREST", TRUE~as.character(Description)),
         Area_group = case_when(Region_not_used=="GST"~"Areas 13-18, 19A, 28, 29",
                                Region_not_used=="JDF"~"Areas 19B, 20",
                                Region_not_used=="JST"~"Areas 11-12",
                                Region_not_used=="WCVI AABM"~"WCVI - AABM",
                                Region_not_used=="WCVI ISBM"~"WCVI - ISBM", TRUE~as.character(Region_not_used)),
         Region_not_used = case_when(Region_not_used=="GST"~"Georgia Strait",
                                     Region_not_used=="JDF"~"Juan de Fuca",
                                     Region_not_used=="JST"~"Johnstone Strait",
                                     grepl("WCVI", Region_not_used)~"West Coast Vancouver Island", TRUE~as.character(Region_not_used)),
         sector="recreational", Year=2021, gear="Sport",
         catch_reported_comment = ifelse(grepl("CREST", Description), "From Nick/Rob's creel", 
                                         ifelse(grepl("iREC", Description), "From Nick/Rob's calibrated iREC", NA)), 
         release_legals_comment = ifelse(grepl("CREST", Description), "From Nick/Rob's creel. Used creel split data.", 
                                         ifelse(grepl("iREC", Description), "From Nick/Rob's calibrated iREC. Used iREC split data", NA)), 
         release_sublegals_comment = ifelse(grepl("CREST", Description), "From Nick/Rob's creel. Used creel split data.", 
                                            ifelse(grepl("iREC", Description), "From Nick/Rob's calibrated iREC. Used iREC split data", NA))) %>%
  print()




# =============== EXTERNAL DATA ===============

# Fraser rec (Chuck) ---------------------------
fraser.rec <- fraser.rec.raw %>%
  filter(Species=="Chinook", `Fishery Type`=="REC") %>%
  mutate(group = case_when(Location=="Fraser" ~ "Mainstem", 
                           grepl("Fraser River", Area) ~ "Mainstem",
                           TRUE ~ "Trib")) %>%
  print()

fraser.rec.2021 <- 
  full_join(
  fraser.rec %>% 
    group_by(Year, group) %>%
    summarize(catch_reported = sum(`Total Kept`), group=unique(group)),
  fraser.rec %>% 
    group_by(Year, group, `Jack or Adult`) %>%
    summarize(total_released = sum(`Total Released`), group=unique(group)) %>%
    pivot_wider(names_from = "Jack or Adult", values_from=total_released) %>%
    rename(release_legals = Adult, 
           release_sublegals = Jack)) %>%
  filter(Year==2021) %>%
  rename(Fishery = group) %>%
  mutate(Fishery = case_when(Fishery=="Mainstem"~"Mainstem Catch",
                           Fishery=="Trib"~"Trib Catch"),
         Region_not_used = "Fraser River",
         gear = "Sport",
         catch_reported_comment = ifelse(Fishery=="Mainstem Catch", 
                                         "From Chuck Parken/Lauren Weir. Mainstem considered Location=='Fraser' & grepl('Fraser River', Area).",
                                         "From Chuck Parken/Lauren Weir. Tribs considered Location!='Fraser' & !grepl('Fraser River', Area)."),
         release_legals_comment = ifelse(Fishery=="Mainstem Catch", 
                                         "From Chuck Parken/Lauren Weir. Mainstem considered Location=='Fraser' & grepl('Fraser River', Area). Legal releases = Adult releases.",
                                         "From Chuck Parken/Lauren Weir. Tribs considered Location!='Fraser' & !grepl('Fraser River', Area). Legal releases = Adult releases."),
         release_sublegals_comment = ifelse(Fishery=="Mainstem Catch", 
                                         "From Chuck Parken/Lauren Weir. Mainstem considered Location=='Fraser' & grepl('Fraser River', Area). Sublegal releases = Jack releases.",
                                         "From Chuck Parken/Lauren Weir. Tribs considered Location!='Fraser' & !grepl('Fraser River', Area). Sublegal releases = Jack releases."))


# South Coast (Joan) ---------------------------
sbc.rec <- sbc.rec.raw %>%
  mutate(catch_reported = NA) %>%
  mutate_at("catch_reported", as.numeric) %>%
  select(-c(water_type, Fishery)) %>%
  print()


     
# =============== TEMPLATE BY SECTOR ===============
temp.comm <- template.raw %>% 
  filter(sector=="commercial")
temp.test <- template.raw %>%
  filter(sector=="test")
temp.fn <- template.raw %>% 
  filter(sector=="FN")
temp.rec <- template.raw %>% 
  filter(sector=="recreational")
  
  

# =============== DATA EXPLORATION/CHECKS ===============

# 1. Are there any fisheries with blank CNR periods that need to be fixed/assessed?
comm.test.full %>% filter(is.na(CNR))




################################################################################################################################################


#                                                     JOIN BY FISHERY SECTOR TYPES 



# START HERE NEXT DAY!! 
# then join all the rec data, 
# THEN add those into the template, sector by sector

# COMMERCIAL + TEST JOIN ----------------
# Note Georgia Strait/Fraser Troll issues - Area 29 is called GST in this, but sometimes Fraser River. Should clarify
comm.test.format <- full_join(troll.releases, seine.releases) %>%
  full_join(., comm.test.summary) %>%
  select(FISHERY_TYPE, GEAR, CNR, Region_not_used, ck_kept, ck_rel, legal_ck_reld, sublegal_ck_reld) %>%
  mutate(release_legals_comment = ifelse(FISHERY_TYPE=="TEST", "Release legals may include sublegals",
                                    ifelse(GEAR=="GN", "Assumption that all releases are sublegals for GN", 
                                      ifelse(FISHERY_TYPE=="COMMERCIAL" & GEAR%in%c("TR", "SN"), "Split data from FOS", NA))) ,
         release_sublegals_comment = ifelse(FISHERY_TYPE=="TEST", "Release legals may include sublegals",
                                       ifelse(GEAR=="GN", "Assumption that all releases are sublegals for GN",  
                                         ifelse(FISHERY_TYPE=="COMMERCIAL" & GEAR%in%c("TR", "SN"), "Split data from FOS", NA))),
         catch_reported_comment = ifelse(FISHERY_TYPE%in%c("COMMERCIAL", "TEST"), "From FOS in-season", NA),
         FISHERY_TYPE = tolower(FISHERY_TYPE),
         GEAR = case_when(GEAR=="SN"~"Seine", GEAR=="GN"~"Gillnet", GEAR=="TR"~"Troll", TRUE~as.character(GEAR)),
         sublegal_ck_reld = ifelse(is.na(legal_ck_reld) & is.na(sublegal_ck_reld) & GEAR=="Gillnet"& FISHERY_TYPE=="commercial", 
                                   ck_rel, 
                                   sublegal_ck_reld),
         legal_ck_reld = ifelse(is.na(legal_ck_reld) & is.na(sublegal_ck_reld) & FISHERY_TYPE=="test", ck_rel, legal_ck_reld),
         QAQC_RELD = ifelse(!is.na(legal_ck_reld) & !is.na(sublegal_ck_reld), ck_rel-(legal_ck_reld+sublegal_ck_reld),
                      ifelse(is.na(legal_ck_reld), ck_rel-sublegal_ck_reld, ck_rel-legal_ck_reld))) %>%
  rename(sector=FISHERY_TYPE,
         gear=GEAR,
         catch_reported=ck_kept,
         release_legals=legal_ck_reld,
         release_sublegals=sublegal_ck_reld) %>%
  select(-c(ck_rel, QAQC_RELD)) %>%
  full_join(., tb%>%filter(sector=="commercial")) %>%
  mutate(Year=2021,
         Fishery = ifelse(Region_not_used=="Northern British Columbia" & gear=="Troll" & CNR==0, "CNR Troll",
                          ifelse(gear=="Troll", "Troll", 
                                 ifelse(gear%in%c("Seine", "Gillnet"), "Net", NA)))) %>%
  print()




# FIRST NATIONS JOIN ----------------
fn.format <- full_join(tb%>%filter(sector=="FN"), nbc.fn) %>%
  full_join(., sbc%>%filter(sector=="FN")) %>%
  full_join(., fraser.fn) %>%
  print()



# REC JOIN ----------------
rec.format <- full_join(creel.irec, nbc%>%filter(sector=="recreational")) %>%
  full_join(., fraser.rec.2021) %>%
  full_join(., sbc.rec) %>%
  print()





################################################################################################################################################


#                                                       POPULATE TEMPLATE and watch the world explode

all.join <- full_join(comm.test.format, fn.format) %>%
  full_join(., rec.format) %>%
  select(sector, FN_fishery, gear, Area_group, Fishery, Fishery2, Description, Year, CNR,
         catch_reported,catch_reported_comment,release_legals, release_legals_comment, release_sublegals, release_sublegals_comment,
         Region_not_used) %>%
  arrange(sector)


catch.data.export <- createWorkbook()
addWorksheet(catch.data.export, "2021")
writeData(catch.data.export, sheet="2021", x=all.join)
saveWorkbook(catch.data.export, "2021 Prelim catch data for db - R export.xlsx", overwrite = T)



## need to add "confirm NC STAD" flag to CBC commercial data 



