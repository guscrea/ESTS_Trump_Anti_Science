### Script for working with US OPM
### personnel data, full dataset.
### (1998-2018)
# Chris Rea
# Last Modified: May 26, 2019

###### INSTALL PACKAGES ######
my_packages <- c("tidyverse","broom","skimr","sp",
                 "reshape2","data.table","ggalluvial",
                 "ggmap","ggthemes","ggrepel",
                 "directlabels","scales",#"raster",
                 "ggpubr","stringr",#"zoo",
                 #"grid","tools","stringi",
                 "tidyselect")

install.packages(my_packages)

#detach("package:raster", unload=TRUE)

###### LOAD PACKAGES ######
lapply(my_packages, require, character.only = TRUE)

###### SET WD ######
setwd("your_working_directory")

###### BUILD YEAR-WISE AGY CNTS ######
# function make dataframe by agency-year

# Note: this is the primary function used to build counts
# from september OPM data.  I use a separate function to
# build counts from March data below; this is used to 
# supplement missing data on the Department of State
# in September 2015. Using data from a consistent time
# period is critial becuase of the seasonality of work
# in some agencies---particularly resource agencies, 
# which hire large populations of workers in the summer
# tourist and fire seasons.

make_df <- function(yyyy){
  path = paste("FedScope/",yyyy,"_Employment_Cube/FACTDATA_SEP",
               yyyy,".txt",sep = "")
  path_agy = paste("FedScope/",yyyy,"_Employment_Cube/DTagy.txt",
                   sep = "")
  emp <- read.csv(path, header = TRUE)
  agy <- read.csv(path_agy, header = TRUE)
#  emp <- get(df1)
#  agy <- get(df2)
  # list of unique values of agency abbr.
  agy_abr <- unique(agy$AGYSUB)
  # list of unique agency full names
  agy_nms <- unique(agy$AGYSUBT)
  # counts of employees by agency
  empnum <- emp %>%
    count(AGYSUB)
  empnum[is.na(empnum)] <- 0
  # counts of employees by agency and length of service
  emplos <- emp %>%
    group_by(AGYSUB) %>%
    count(LOSLVL) %>%
    spread(LOSLVL, n)
  emplos[is.na(emplos)] <- 0
  # counts of employees by agency and educational attainmnet
  empedu <- emp %>%
    group_by(AGYSUB) %>%
    count(EDLVL) %>%
    spread(EDLVL, n)
  empedu[is.na(empedu)] <- 0
  # (append "ed" to var names)
  l = length(colnames(empedu))
  colnames(empedu)[2:l] <- paste(colnames(empedu)[2:l],
                                  "ed",sep = "")
  # counts of employees by agency and GS level
  empgse <- emp %>%
    group_by(AGYSUB) %>%
    count(GSEGRD)
  # (add leading 0 if necessary)
  empgse$GSEGRD <- formatC(empgse$GSEGRD, width = 2,
                           format = "d", flag = "0")
  empgse <- empgse %>%
    spread(GSEGRD, n)
  empgse[is.na(empgse)] <- 0
  # (append "gs" to var names)
  l = length(colnames(empgse))
  colnames(empgse)[2:l] <- paste(colnames(empgse)[2:l],
                                  "gs",sep = "")
  # counts of employees by profession
  empstm <- emp %>%
    group_by(AGYSUB) %>%
    count(OCC) %>%
    spread(OCC, n)
  empstm[is.na(empstm)] <- 0
  # (append "prof" to var names)
  l = length(colnames(empstm))
  colnames(empstm)[2:l] <- paste(colnames(empstm)[2:l],
                                 "prof",sep = "")
  # counts of employees by location
  emploc <- emp %>%
    group_by(AGYSUB) %>%
    count(LOC) %>%
    spread(LOC, n)
  emploc[is.na(emploc)] <- 0
  # (drop columns past US states)
  l = length(colnames(emploc))
  emploc <- emploc[-c(2, 54:l)]
  # (append state abbr. to var names)
  state_nms <- c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL",
                 "GA","HI","ID","IL","IN","IA","KS","KY","LA","ME",
                 "MD","MA","MI","MN","MS","MO","MT","NE","NV","NH",
                 "NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI",
                 "SC","SD","TN","TX","UT","VT","VA","WA","WV","WI",
                 "WY")
  l = length(colnames(emploc))
  colnames(emploc)[2:l] <- paste(colnames(emploc)[2:l],"st",
                                 state_nms,sep = "_")
  # join dataframes by AGYSUB
  emp <- list(empnum,emplos,empedu,empgse,empstm,emploc) %>%
    Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by="AGYSUB"), .)
  # calculate column sums for each column (this gives totals
  # by year across agencies/ agency sub-categories)
  totals <- emp %>%
    summarise_if(is.numeric, sum, na.rm = TRUE)
  # add names to totals row
  totals["AGYSUB"] <- "TOTL"
  totals
  # append totals to emp
  emp <- bind_rows(emp,totals)
  # calculate average length of service
  emp <- emp %>%
    mutate(LOSavg = .5*(A/n)+1.5*(B/n)+2.5*(C/n)+7*(D/n)+
             12*(E/n)+17*(F/n)+22*(G/n)+27*(H/n)+32*(I/n)+
             37*(J/n))
  # add year column
  emp$yr <- yyyy
  # assign agency names by abbr
  emp$agy_full <- factor(emp$AGYSUB,
                         levels = agy_abr,
                         labels = agy_nms)
  # add full name for totals category
  totals["agy_full"] <- "TOTL-ALL AGENCIES"
  return(emp)
}

make_df.mar <- function(yyyy){
  path = paste("FedScope/",yyyy,"_Employment_Cube/FACTDATA_MAR",
               yyyy,".txt",sep = "")
  path_agy = paste("FedScope/",yyyy,"_Employment_Cube/DTagy.txt",
                   sep = "")
  emp <- read.csv(path, header = TRUE)
  agy <- read.csv(path_agy, header = TRUE)
  #  emp <- get(df1)
  #  agy <- get(df2)
  # list of unique values of agency abbr.
  agy_abr <- unique(agy$AGYSUB)
  # list of unique agency full names
  agy_nms <- unique(agy$AGYSUBT)
  # counts of employees by agency
  empnum <- emp %>%
    count(AGYSUB)
  empnum[is.na(empnum)] <- 0
  # counts of employees by agency and length of service
  emplos <- emp %>%
    group_by(AGYSUB) %>%
    count(LOSLVL) %>%
    spread(LOSLVL, n)
  emplos[is.na(emplos)] <- 0
  # counts of employees by agency and educational attainmnet
  empedu <- emp %>%
    group_by(AGYSUB) %>%
    count(EDLVL) %>%
    spread(EDLVL, n)
  empedu[is.na(empedu)] <- 0
  # (append "ed" to var names)
  l = length(colnames(empedu))
  colnames(empedu)[2:l] <- paste(colnames(empedu)[2:l],
                                 "ed",sep = "")
  # counts of employees by agency and GS level
  empgse <- emp %>%
    group_by(AGYSUB) %>%
    count(GSEGRD)
  # (add leading 0 if necessary)
  empgse$GSEGRD <- formatC(empgse$GSEGRD, width = 2,
                           format = "d", flag = "0")
  empgse <- empgse %>%
    spread(GSEGRD, n)
  empgse[is.na(empgse)] <- 0
  # (append "gs" to var names)
  l = length(colnames(empgse))
  colnames(empgse)[2:l] <- paste(colnames(empgse)[2:l],
                                 "gs",sep = "")
  # counts of employees by profession
  empstm <- emp %>%
    group_by(AGYSUB) %>%
    count(OCC) %>%
    spread(OCC, n)
  empstm[is.na(empstm)] <- 0
  # (append "prof" to var names)
  l = length(colnames(empstm))
  colnames(empstm)[2:l] <- paste(colnames(empstm)[2:l],
                                 "prof",sep = "")
  # counts of employees by location
  emploc <- emp %>%
    group_by(AGYSUB) %>%
    count(LOC) %>%
    spread(LOC, n)
  emploc[is.na(emploc)] <- 0
  # (drop columns past US states)
  l = length(colnames(emploc))
  emploc <- emploc[-c(2, 54:l)]
  # (append state abbr. to var names)
  state_nms <- c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL",
                 "GA","HI","ID","IL","IN","IA","KS","KY","LA","ME",
                 "MD","MA","MI","MN","MS","MO","MT","NE","NV","NH",
                 "NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI",
                 "SC","SD","TN","TX","UT","VT","VA","WA","WV","WI",
                 "WY")
  l = length(colnames(emploc))
  colnames(emploc)[2:l] <- paste(colnames(emploc)[2:l],"st",
                                 state_nms,sep = "_")
  # join dataframes by AGYSUB
  emp <- list(empnum,emplos,empedu,empgse,empstm,emploc) %>%
    Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by="AGYSUB"), .)
  # calculate column sums for each column (this gives totals
  # by year across agencies/ agency sub-categories)
  totals <- emp %>%
    summarise_if(is.numeric, sum, na.rm = TRUE)
  # add names to totals row
  totals["AGYSUB"] <- "TOTL"
  totals
  # append totals to emp
  emp <- bind_rows(emp,totals)
  # calculate average length of service
  emp <- emp %>%
    mutate(LOSavg = .5*(A/n)+1.5*(B/n)+2.5*(C/n)+7*(D/n)+
             12*(E/n)+17*(F/n)+22*(G/n)+27*(H/n)+32*(I/n)+
             37*(J/n))
  # add year column
  emp$yr <- yyyy
  # assign agency names by abbr
  emp$agy_full <- factor(emp$AGYSUB,
                         levels = agy_abr,
                         labels = agy_nms)
  # add full name for totals category
  totals["agy_full"] <- "TOTL-ALL AGENCIES"
  return(emp)
}

e98 <- make_df(1998)
e99 <- make_df(1999)
e00 <- make_df(2000)
e01 <- make_df(2001)
e02 <- make_df(2002)
e03 <- make_df(2003)
e04 <- make_df(2004)
e05 <- make_df(2005)
e06 <- make_df(2006)
e07 <- make_df(2007)
e08 <- make_df(2008)
e09 <- make_df(2009)
e10 <- make_df(2010)
e11 <- make_df(2011)
e12 <- make_df(2012)
e13 <- make_df(2013)
e14 <- make_df(2014)
e15 <- make_df(2015)
e15_mar <- make_df.mar(2015) # to supplement missing Dept. of State data in 2015.
e16 <- make_df(2016)
e17 <- make_df(2017)
e18 <- make_df(2018)

# make df of just department of state employees in 2015.
e15_mar_DoS <- e15_mar[e15_mar$AGYSUB=="ST00", ]
# append this row to e15, since otherwise e15 is missing DoS
e15 <- bind_rows(e15,e15_mar_DoS)

###### COMBINE ALL YEARS ######
e98_17 <- bind_rows(e98,e99,e00,e01,e02,e03,
                e04,e05,e06,e07,e08,e09,
                e10,e11,e12,e13,e14,e15,
                e16,e17,e18)
e98_17[is.na(e98_17)] <- 0

###### LIST AND LABEL ALL AGENCIES ######
agy_list <- function(yyyy){
path_agy = paste("FedScope/",yyyy,"_Employment_Cube/DTagy.txt",
                 sep = "")
agy <- read.csv(path_agy, header = TRUE)
}
a98 <- agy_list(1998)
a99 <- agy_list(1999)
a00 <- agy_list(2000)
a01 <- agy_list(2001)
a02 <- agy_list(2002)
a03 <- agy_list(2003)
a04 <- agy_list(2004)
a05 <- agy_list(2005)
a06 <- agy_list(2006)
a07 <- agy_list(2007)
a08 <- agy_list(2008)
a09 <- agy_list(2009)
a10 <- agy_list(2010)
a11 <- agy_list(2011)
a12 <- agy_list(2012)
a13 <- agy_list(2013)
a14 <- agy_list(2014)
a15 <- agy_list(2015)
a16 <- agy_list(2016)
a17 <- agy_list(2017)
a18 <- agy_list(2018)
# bind all agency-years together
a98_17 <- bind_rows(a98,a99,a00,a01,a02,a03,
                    a04,a05,a06,a07,a08,a09,
                    a10,a11,a12,a13,a14,a15,
                    a16,a17,a18)
#  retain only unique rows
a98_17 <- unique(a98_17)
# remove individual years
rm(a98,a99,a00,a01,a02,a03,
   a04,a05,a06,a07,a08,a09,
   a10,a11,a12,a13,a14,a15,
   a16,a17,a18)
# Begin coding by agecny type
# 1. create new variable for agency type
a98_17$agy_typ <- 0
# 2. create list of unique agency categories
agy_list <- unique(a98_17$AGY)
# 2a. append "TO" for total to list
agy_list <- append(agy_list,"TO")
# 2b. Display list
agy_list
# 3. create list of agency codes to map to agency categories
# (Currently at 11   unique codes)
agy_codes <- c("Defense","Nature and Resources","Defense",
               "Commerce and Information","Defense","Crime and Law",
               "Work and Labor","Research, Arts, and Sciences","Education",
               "Health and Social Welfare","Health and Social Welfare",
               "Nature and Resources","Defense","Foreign Affairs",
               "Commerce and Information","Money, Finance, and Banking",
               "Health and Social Welfare","Foreign Affairs",
               "Work and Labor","Nature and Resources","Nature and Resources",
               "Commerce and Information","Money, Finance, and Banking",
               "Commerce and Information","Management and Administration",
               "Foreign Affairs","Management and Administration",
               "Research, Arts, and Sciences","Work and Labor",
               "Research, Arts, and Sciences","Management and Administration",
               "Research, Arts, and Sciences","Management and Administration","Health and Social Welfare",
               "Commerce and Information","Money, Finance, and Banking",
               "Research, Arts, and Sciences","Health and Social Welfare",
               "Defense","Research, Arts, and Sciences","Work and Labor",
               "Work and Labor","Defense","Health and Social Welfare",
               "Management and Administration","Commerce and Information",
               "Money, Finance, and Banking","Commerce and Information",
               "Management and Administration","Money, Finance, and Banking",
               "Work and Labor","Money, Finance, and Banking",
               "Foreign Affairs","Research, Arts, and Sciences",
               "Health and Social Welfare","Management and Administration","Crime and Law",
               "Commerce and Information","Commerce and Information",
               "Foreign Affairs","Crime and Law",
               "Health and Social Welfare","Health and Social Welfare",
               "Health and Social Welfare","Defense",
               "Health and Social Welfare","Commerce and Information",
               "Commerce and Information","Defense",
               "Management and Administration","Foreign Affairs",
               "Foreign Affairs","Health and Social Welfare",
               "Research, Arts, and Sciences","Education",
               "Health and Social Welfare","Research, Arts, and Sciences","Education",
               "Health and Social Welfare","Commerce and Information",
               "Research, Arts, and Sciences","Crime and Law",
               "Education","Research, Arts, and Sciences",
               "Education","Nature and Resources",
               "Education","Foreign Affairs","Management and Administration",
               "Money, Finance, and Banking","Health and Social Welfare",
               "Money, Finance, and Banking","Crime and Law","Education",
               "Crime and Law","Foreign Affairs","Foreign Affairs",
               "Health and Social Welfare","Research, Arts, and Sciences",
               "Education","Foreign Affairs","Nature and Resources",
               "Health and Social Welfare","Work and Labor",
               "Research, Arts, and Sciences","Defense",
               "Work and Labor","Health and Social Welfare",
               "Health and Social Welfare","Research, Arts, and Sciences",
               "Foreign Affairs","Nature and Resources",
               "Commerce and Information","Crime and Law",
               "Health and Social Welfare","Nature and Resources",
               "Crime and Law","Foreign Affairs","Commerce and Information",
               "Foreign Affairs","Health and Social Welfare",
               "Foreign Affairs","Health and Social Welfare",
               "Health and Social Welfare","Education","Nature and Resources",
               "Crime and Law","Foreign Affairs","Crime and Law",
               "Nature and Resources","Management and Administration",
               "Health and Social Welfare","Defense","Foreign Affairs",
               "Defense","Commerce and Information",
               "Research, Arts, and Sciences","Foreign Affairs",
               "Commerce and Information","Nature and Resources",
               "Nature and Resources","Defense","Research, Arts, and Sciences",
               "Money, Finance, and Banking","Commerce and Information",
               "Money, Finance, and Banking","Management and Administration",
               "Health and Social Welfare","Health and Social Welfare",
               "Money, Finance, and Banking","Management and Administration",
               "Research, Arts, and Sciences","Crime and Law","Crime and Law",
               "Health and Social Welfare","Research, Arts, and Sciences",
               "Nature and Resources","Totals")
# 4. Count unique categories 
un <- unique(agy_codes)
# 5. Create agency code variable based on first 2 digits
# of AGYSUB
e98_17$agy_code <- substr(e98_17$AGYSUB, 1, 2)
# 6. Apply agency codes to agencies
e98_17$agy_typ <- factor(e98_17$agy_code,
                            levels = agy_list,
                            labels = agy_codes)
a98_17$agy_typ <- factor(a98_17$AGY,
                         levels = agy_list,
                         labels = agy_codes)

# write out agency list for manual coding
write.csv(a98_17, file = "a98_17.csv")

# Note: the above .csv file, a98_17.csv, provides a starting
# place for coding offices by type. I then re-code the agencies
# manually, slotting each into one of the 11 codes listed above
# (Nature and Resources; Management and Adminstration; Foreign
# Affairs; Education; Commerce and Information; Work and Labor;
# Defense; Money, Finance and Banking; Crime and Law; Health and
# Social Welfare; Research, Arts, and Sciences.) A few notes
# about that coding: 

# 1. All leadership and management-type (Secretary, Under Secretary,
#    Assistant Secretay, etc. offices are coded as "Management
#    and Adminstration." Arguably, the secretary offices thmselves
#    could be slotted into the respective categories where their
#    substantive focus is (e.g. the Office of the Secretary of 
#    Defense could be categorized as Defense; the Office of the
#    Secretary of Agriculture could be in Commerce and Information;
#    the Office of the Secretary of the Department of Interior could
#    be in Nature and Resources), but all that argument seems much
#    less strong in the case of subordinate positions (Assistant
#    Secretaries and so on). Becuase these offices are relatively
#    small, this does not make a tremendous difference to the larger
#    analysis. One possible exception is that the Office of the 
#    Secretary of Interior has grown subsantially---the *only* arguable
#    increase in the Nature and Resources category from 1998 t0 2018---
#    and a largley *ADMINISTRATIVE* one.
#
# 2. All Inspector General offices are categorized as Management and
#    Adminstration, not Crime and Law, although the latter is also
#    justifiable.
#
# 3. All Civil Rights offices are coded as Crime and Law, since they
#    are focused on enforcing Civil Rights statures as they pertain
#    to the domain governed by a given dpeartment or agency. The
#    same applies to the Civil Rights Commission.
#
# 4. See the coded file itself, a98_17_coded_update.csv, for the
#    specific designations. Codes can easily be changed and the 
#    script re-run to see how that changes the output (plots).


# read-in agency list after manual coding
agy2 <- read.csv("a98_17_coded_update.csv", header = TRUE)

# re-extract and apply agency codes to data
agy_list2 <- as.vector(agy2$AGYSUB)
agy_codes2 <- as.vector(agy2$agy_typ2)

e98_17$agy_typ2 <- factor(e98_17$AGYSUB,
                         levels = agy_list2,
                         labels = agy_codes2)

# make variable with sum of only domestic employees
# (sum counts of employess in each state, excluding 
# territories and non-U.S.-based locations.
e98_17$n_dom <- rowSums(select(e98_17,`01_st_AL`:`56_st_WY`))

# re-order columns for easy viewing
e98_17 <- e98_17 %>%
  select(AGYSUB,n,n_dom,everything())

###### REPLACE GENERIC NAMES WITH SPECIFIC ONES ######
e98_17$agy_full <- gsub("EDEF-OFFICE OF INSPECTOR GENERAL",
                        "EDEF-DEPT. OF EDUCATION OFFICE OF INSPECTOR GENERAL",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("HUGG-OFFICE OF INSPECTOR GENERAL",
                        "HUGG-HUD OFFICE OF INSPECTOR GENERAL",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("IN24-OFFICE OF THE INSPECTOR GENERAL",
                        "IN24-DEPT. OF INTERIOR OFFICE OF THE INSPECTOR GENERAL",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("TD12-OFFICE OF INSPECTOR GENERAL",
                        "TD12-DEPT. OF TRANSPORTATION OFFICE OF INSPECTOR GENERAL",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("TR95-OFFICE OF INSPECTOR GENERAL",
                        "TR95-DEPT. OF TREASURY OFFICE OF INSPECTOR GENERAL",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("VAAF-INSPECTOR GENERAL",
                        "VAAF-DEPT. OF VETERANS AFFAIRS INSPECTOR GENERAL",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("GS15-OFFICE OF INSPECTOR GENERAL",
                        "GS15-GENERAL SERVICES ADMIN. OFFICE OF INSPECTOR GENERAL",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("EM02-OFFICE OF INSPECTOR GENERAL",
                        "EM02-FEMA OFFICE OF INSPECTOR GENERAL",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("HSAE-OFFICE OF THE INSPECTOR GENERAL",
                        "HSAE-DEPT. OF HOMELAND SECURITY OFFICE OF THE INSPECTOR GENERAL",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("HFHI-OFFICE OF INSPECTOR GENERAL",
                        "HFHI-FED. HOUSING FINANCE AGENCY OFFICE OF INSPECTOR GENERAL",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("HE13-OFFICE OF INSPECTOR GENERAL",
                        "HE13- HSS OFFICE OF INSPECTOR GENERAL",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("HB02-OFFICE OF INSPECTOR GENERAL",
                        "HB02-CFPFPWABSD OFFICE OF INSPECTOR GENERAL",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("AG23-OFFICE OF THE INSPECTOR GENERAL",
                        "AG23-DEPT. OF AGR. OFFICE OF THE INSPECTOR GENERAL",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("CM64-OFFICE OF THE INSPECTOR GENERAL",
                        "CM64-DEPT. OF COMMERCE OFFICE OF THE INSPECTOR GENERAL",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("DD26-OFFICE OF THE INSPECTOR GENERAL",
                        "DD26-DoD OFFICE OF THE INSPECTOR GENERAL",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("DJ10-OFFICE OF THE INSPECTOR GENERAL",
                        "DJ10-DoJ OFFICE OF THE INSPECTOR GENERAL",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("DLIG-DEPT. OF LABOR OFFICE OF THE INSPECTOR GENERAL",
                        "DLIG-OFFICE OF THE INSPECTOR GENERAL",
                        e98_17$agy_full)




e98_17$agy_full <- gsub("HUFF-OFFICE OF THE CHIEF FINANCIAL OFFICER",
                        "HUFF-HUD OFFICE OF THE CHIEF FINANCIAL OFFICER",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("GS11-OFFICE OF THE CHIEF FINANCIAL OFFICER",
                        "GS11-GSA OFFICE OF THE CHIEF FINANCIAL OFFICER",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("GS28-OFFICE OF THE CHIEF INFORMATION OFFICER",
                        "GS28-GSA OFFICE OF THE CHIEF INFORMATION OFFICER",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("GS14-OFFICE OF THE CHIEF PEOPLE OFFICER",
                        "GS14-GSA OFFICE OF THE CHIEF PEOPLE OFFICER",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("HUNN-OFFICE OF THE CHIEF PROCUREMENT OFFICER",
                        "HUNN-HUD OFFICE OF THE CHIEF PROCUREMENT OFFICER",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("HUQQ-OFFICE OF THE CHIEF INFORMATION OFFICER",
                        "HUQQ-HUD OFFICE OF THE CHIEF INFORMATION OFFICER",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("GS16-OFFICE OF THE CHIEF ACQUISITION OFFICER",
                        "GS16-GSA OFFICE OF THE CHIEF ACQUISITION OFFICER",
                        e98_17$agy_full)


e98_17$agy_full <- gsub("DLCF-OFFICE OF THE CHIEF FINANCIAL OFFICER",
                        "DLCF-DEPT. OF LABOR OFFICE OF THE CHIEF FINANCIAL OFFICER",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("EDEL-OFFICE OF THE CHIEF FINANCIAL OFFICER",
                        "EDEL-DEPT. OF EDUC. OFFICE OF THE CHIEF FINANCIAL OFFICER",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("EDEI-OFFICE OF THE CHIEF INFORMATION OFFICER",
                        "EDEI-DEPT. OF EDUC. OFFICE OF THE CHIEF INFORMATION OFFICER",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("GS14-OFFICE OF THE CHIEF HUMAN CAPITAL OFFICER",
                        "GS14-GSA OFFICE OF THE CHIEF HUMAN CAPITAL OFFICER",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("GS31-OFFICE OF EMERGENCY RESPONSE AND RECOVERY",
                        "GS31-GSA OFFICE OF EMERGENCY RESPONSE AND RECOVERY",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("HUBB-OFFICE OF THE CHIEF HUMAN CAPITAL OFFICER",
                        "HUBB-HUD OFFICE OF THE CHIEF HUMAN CAPITAL OFFICER",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("AG42-OFFICE OF BUDGET AND PROGRAM ANALYSIS",
                        "AG42-DEPT. OF AGRICULTURE OFFICE OF BUDGET AND PROGRAM ANALYSIS",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("AG92-OFFICE OF THE ADMINISTRATIVE LAW JUDGE",
                        "AG92-DEPT. OF AGRICULTURE OFFICE OF THE ADMINISTRATIVE LAW JUDGE",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("AG98-OFFICE OF OPERATIONS",
                        "AG98-EPT. OF AGRICULTURE OFFICE OF OPERATIONS",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("VAAA-OFFICE OF THE SECRETARY",
                        "VA-DEPT. OF VETERANS AFFAIRS OFFICE OF THE SECRETARY",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("HSAA-IMMEDIATE OFFICE OF THE SECRETARY",
                        "HSAA-DEPT. OF HOMELAND SECURITY IMMEDIATE OFFICE OF THE SECRETARY",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("TD11-OFFICE OF THE SECRETARY, RESEARCH AND TECHNOLOGY",
                        "TD11-DEPT. OF TRANS. OFFICE OF THE SECRETARY, RESEARCH AND TECHNOLOGY",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("CM51-OFFICE OF THE SECRETARY",
                        "CM51-DEPT. OF COMMERCE OFFICE OF THE SECRETARY",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("TR91-DEPARTMENTAL OFFICES",
                        "TR91-DEPT. OF TREASURY DEPARTMENTAL OFFICES",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("VAAE-GENERAL COUNSEL",
                        "VAAE-VA-DEPT. OF VETERANS AFFAIRS GENERAL COUNSEL",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("GS12-OFFICE OF GENERAL COUNSEL",
                        "GS12-GSA OFFICE OF GENERAL COUNSEL",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("AG14-OFFICE OF THE GENERAL COUNSEL",
                        "AG14-DEPT. OF AGRICULTURE OFFICE OF THE GENERAL COUNSEL",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("EDEG-OFFICE OF THE GENERAL COUNSEL",
                        "EDEG-DEPT. OF EDUC. OFFICE OF THE GENERAL COUNSEL",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("HUCC-OFFICE OF GENERAL COUNSEL",
                        "HUCC-HUD OFFICE OF GENERAL COUNSEL",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("VABC-DEPUTY ASSISTANT SECRETARY FOR HUMAN RESOURCES MANAGEMENT AND LABOR RELATIONS",
                        "VABC-DEPT. OF VETERANS AFFAIRS DEPUTY ASSISTANT SECRETARY FOR HUMAN RESOURCES MANAGEMENT AND LABOR RELATIONS",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("VAJA-OFFICE OF THE ASSISTANT SECRETARY FOR PUBLIC AND INTERGOVERNMENTAL AFFAIRS",
                        "VAJA-DEPT. OF VETERANS AFFAIRS OFFICE OF THE ASSISTANT SECRETARY FOR PUBLIC AND INTERGOVERNMENTAL AFFAIRS",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("VAJB-DEPUTY ASSISTANCE SECRETARY FOR INTERGOVERNMENTAL AFFAIRS",
                        "VAJB-DEPT. OF VETERANS AFFAIRS DEPUTY ASSISTANCE SECRETARY FOR INTERGOVERNMENTAL AFFAIRS",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("AGCR-OFFICE OF CIVIL RIGHTS",
                        "AGCR-DEPT. OF AGRICULTURE OFFICE OF CIVIL RIGHTS",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("EDEC-OFFICE FOR CIVIL RIGHTS",
                        "EDEC-DEPT. OF EDUC. OFFICE FOR CIVIL RIGHTS",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("GS04-OFFICE OF CIVIL RIGHTS",
                        "GS04-GSA OFFICE OF CIVIL RIGHTS",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("VAAA-OFFICE OF THE SECRETARY",
                        "VAAA-DEPARTMENT OF VETERANS AFFAIRS OFFICE OF THE SECRETARY",
                        e98_17$agy_full)



e98_17$agy_full <- gsub("GS14-OFFICE OF THE CHIEF HUMAN CAPITAL OFFICER",
                        "GS14-GSA OFFICE OF THE CHIEF HUMAN CAPITAL OFFICER",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("GHUBB-OFFICE OF THE CHIEF HUMAN CAPITAL OFFICER",
                        "HUBB-HUD OFFICE OF THE CHIEF HUMAN CAPITAL OFFICER",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("AG38-OFFICE OF THE CHIEF ECONOMIST",
                        "AG38-DEPT. OF AGRICULTURE OFFICE OF THE CHIEF ECONOMIST",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("AG90-OFFICE OF THE CHIEF FINANCIAL OFFICER",
                        "AG90-DEPT. OF AGRICULTURE OFFICE OF THE CHIEF FINANCIAL OFFICER",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("AGIT-OFFICE OF THE CHIEF INFORMATION OFFICER",
                        "AGIT-DEPT. OF AGRICULTURE OFFICE OF THE CHIEF INFORMATION OFFICER",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("AG13-OFFICE OF COMMUNICATIONS",
                        "AG13-DEPT. OF AGRICULTURE OFFICE OF COMMUNICATIONS",
                        e98_17$agy_full)
e98_17$agy_full <- gsub("AG14-OFFICE OF THE GENERAL COUNSEL",
                        "AG14-DEPT. OF AGRICULTURE OFFICE OF THE GENERAL COUNSEL",
                        e98_17$agy_full)

# Create agency name without leading code. First convert 
# to lower case, then Title Case, then remove leading code,
# then replace "U.s." with "U.S." (and similar)
e98_17$agy_name <- str_to_lower(e98_17$agy_full)
e98_17$agy_name <- str_to_title(e98_17$agy_name)
e98_17$agy_name <- substr(e98_17$agy_name, 6,
                          length(e98_17$agy_name))
e98_17$agy_name <- gsub("U.s.","U.S.",e98_17$agy_name)
e98_17$agy_name <- gsub("Gsa","GSA",e98_17$agy_name)
e98_17$agy_name <- gsub("Dod ","DoD ",e98_17$agy_name)
e98_17$agy_name <- gsub("Hss ","HSS ",e98_17$agy_name)
e98_17$agy_name <- gsub("Hud ","HUD ",e98_17$agy_name)
e98_17$agy_name <- gsub("Nasa ","NASA ",e98_17$agy_name)
e98_17$agy_name <- gsub(" Of "," of ",e98_17$agy_name)


# test df (state dept seems to shift counting methods...)
e98_17test <- e98_17[e98_17$agy_typ2=="Foreign Affairs", ]
e98_17test <- e98_17test %>%
  arrange(agy_name)
#a$Col3 <- rowSums(a[,2:3])
e98_17test$n <- rowSums(e98_17test[, 122:172])


###### CALC TOTALS BY AGENCY TYPE ######
# make year a character to protect it from sum function below
e98_17$yr <- as.character(e98_17$yr)
# calculate sums
e98_17atots <-e98_17 %>%
  group_by(yr, agy_typ2) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)
# bind agency-type totals to main df
e98_17 <- bind_rows(e98_17,e98_17atots)
# make year a numerical value again. 
e98_17$yr <- as.numeric(e98_17$yr)
# add in agency names for totals
# 1. first, make agy_typ and agy_typ2 a character
e98_17$agy_typ <- as.character(e98_17$agy_typ)
e98_17$agy_typ2 <- as.character(e98_17$agy_typ2)
# 2. add in type-specific agency codes
e98_17$AGYSUB[which(is.na(e98_17$AGYSUB))] <- 
  paste("TO",substr(e98_17$agy_typ2[is.na(e98_17$AGYSUB)],1,2),
        sep = "")
# 3. add in names for totals
e98_17$agy_full[which(is.na(e98_17$agy_full))] <-
  "TOTAL-ALL AGENCIES"
# 4. Add agency name
e98_17$agy_code[which(is.na(e98_17$agy_code))] <- "TO"
# 5. fill in names of missing agy_typ for totals
e98_17$agy_typ[e98_17$agy_code=="TO"] <- 
  e98_17$agy_typ2[e98_17$agy_code=="TO"]
# fill in missing full names for totals
# 1. fill in "All Agencies" for missing values
e98_17$agy_name[e98_17$agy_code=="TO"] <- "All Federal Agencies"
# 2. fill in more precise name for agency sub-totals
e98_17$agy_name[e98_17$agy_code=="TO" &
                  e98_17$agy_typ!="Totals"] <-
  paste(e98_17$agy_typ[e98_17$agy_code=="TO" &
                                 e98_17$agy_typ!="Totals"],
        " NET", sep = "")

# append number of employees in agency at last year of observation
# to agency name - total employees in all places.
e98_17 <- e98_17 %>%
  group_by(agy_name) %>%
  arrange(yr) %>%
  mutate(agy_name1 = paste(
    agy_name," (",last(prettyNum(n,big.mark=",",scientific=FALSE)),
    ")",sep = ""))

# same as above, but with only domenstic employees.
e98_17 <- e98_17 %>%
  group_by(agy_name) %>%
  arrange(yr) %>%
  mutate(agy_name2 = paste(
    agy_name," (",last(prettyNum(n_dom,big.mark=",",scientific=FALSE)),
    ")",sep = ""))


save.image(("R_Analyis/Full_Data_Trends_1998_2017/US_Fed_Employ_98_17.RData"))

###### CALC INDICIES TO FIRST YEAR ######

# total employees index
e98_17 <- e98_17 %>%
  group_by(AGYSUB) %>%
  arrange(yr) %>%
  mutate(n_index = n/first(n))

# total employees index - DOMESTIC EMPLOYEES ONLY
e98_17 <- e98_17 %>%
  group_by(AGYSUB) %>%
  arrange(yr) %>%
  mutate(n_dindex = n_dom/first(n_dom))

###### SIZE VARIABLE ######
e98_17$plot_size <- "A"
e98_17$plot_size[e98_17$agy_code=="TO"] <- "B"
e98_17$plot_size <- as.factor(e98_17$plot_size)

###### COLOR SCHEMES ######

# DIVERGING COLORS
colors <- c("#45001880",
            "#ff795480",
            "#ffcb3480",
            "#ddff8180",
            "#12250080",
            "#7dffcb80",
            "#0097b180",
            "#000c2180",
            "#00620680",
            "#fc040480",
#            "#4b3ae380",
            "#34343480",
            "#ac007480")

# HIGHLIGHT NATURE & RESOURCES
colors_NaR <- c("#afafaf33",#commerce
                "#afafaf33",#crime and law
                "#afafaf33",#defense
                "#afafaf33",#education
                "#afafaf33",#foreign affairs
                "#afafaf33",#health and social welfare
                "#afafaf33",#management and administration
                "#afafaf33",#money bannking and finance
                "#006206FF",#Nature
                "#afafaf33",#Research, Arts, Science
                "#343434FF",#Totals
                "#afafaf33")#work and labor

# HIGHLIGHT SCIENCE, HISTORY, ART
colors_SHA <- c("#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#fc0404A8",
                "#343434A8",
                "#afafaf33")

# HIGHLIGHT HEALTH AND SOCIAL WELFARE
colors_HSW <- c("#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#168dfeA8",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#343434A8",
                "#afafaf33")

# CRIME AND LAW
colors_CaL <- c("#afafaf33",
                "#fdc113A8",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#343434A8",
                "#afafaf33")

# MONEY FINANCE BANKING
colors_MBF <- c("#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#65df33A8",
                "#afafaf33",
                "#afafaf33",
                "#343434A8",
                "#afafaf33")

# DEFENSE
colors_DEF <- c("#afafaf33",
                "#afafaf33",
                "#fa8f2aA8",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#343434A8",
                "#afafaf33")

# WORK AND LABOR
colors_WaL <- c("#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#343434A8",
                "#6b0096A8")

# COMMERCE AND INFORMATION
colors_CaI <- c("#00c49dA8",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#343434A8",
                "#afafaf33")

# EDUCATION
colors_EDU <- c("#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#7a4100A8",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#343434A8",
                "#afafaf33")

# Foreign Affairs
colors_FOR <- c("#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#080038A8",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#343434A8",
                "#afafaf33")

# Management and Administration
colors_MaA <- c("#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#9b0163A8",
                "#afafaf33",
                "#afafaf33",
                "#afafaf33",
                "#343434A8",
                "#afafaf33")

# colors for all the professions
unique_occ <- unique(p98_17$OCCT[p98_17$OCCTYPT=="White Collar"])
col_num <- length(unique_occ) # get no. colors
colors_full_range <- rainbow(col_num, alpha = .5) # make color scheme
names(colors_full_range) <- levels(unique_occ)
colors_prof <- scale_fill_manual(name = "Agencies",
                                 #breaks = unique_agy_prof, #needs to match values used in alluvium variable
                                 values = colors_full_range)#,
                                 #str_wrap(unique_agy_prof, 30))


#colors <- rainbow(12, alpha = .5)

###### PLOT FUNCTIONS ######

# FUNCTION FOR PLOTTIG RAW DATA BY DIFF GROUPINGS
# (with ggrepel labels)
# ALL EMPLOYEES
plot_by_grp <- function(dta,x,y,
                            group,colour,
                            size,w1,w2,leg_nm,
                            breaks,labels,color_vals,
                            x_strt,x_end,y_strt,y_end,
                            x_unit,y_unit,
                            y_axis,
                            title,title_sz,
                            agy_type,
                            lab_sz,lbl_thresh,lbl_space){
  ggplot(dta,
         aes_string(x = x,
                    y = y,
                    group = group,
                    colour = colour,
                    size = size))+
    geom_line(data = subset(dta, agy_name!="All Federal Agencies")) + #background
    geom_line(data = subset(dta, agy_typ2==agy_type |
                              agy_name=="All Federal Agencies")) + #focus agency
    geom_segment(aes(x = 1998, y= 1, xend=2018, yend=1),
                 color = "black", size = .5) +
    labs(y = y_axis,
         title = title) +
    scale_color_manual(name= leg_nm,
                       breaks=breaks,
                       str_wrap(labels, 20),
                       values = color_vals,#) +#,
                       guide = 'none') + # no legend
    scale_size_manual(values=c(w1,w2),
                      guide = 'none')+
    scale_x_continuous(limits = c(x_strt-1,x_end+lbl_space),
                       breaks=seq(x_strt-1,x_end+1,x_unit)) +
    #scale_y_continuous(breaks=seq(y_strt,y_end,y_unit)) +
    scale_y_continuous(trans='log10',breaks=seq(y_strt+.05,y_end,.1)) +
    annotate(geom = "rect", xmin = x_end+.5, xmax = Inf,
             ymin = 0.01, ymax = Inf, fill = "white" ) + 
    # ^ this draws a white rectangle over plot to hide horizontal lines, etc.
    geom_text_repel(data = subset(dta, (yr == max(yr) &
                                    n>=lbl_thresh) & 
                                    (agy_typ2==agy_type |
                                       agy_name=="All Federal Agencies")),
                    aes(label=str_wrap(agy_name1, width = 45)),
                    size = lab_sz,
                    hjust = 0,
                    direction = "y",
                    nudge_x = 2,
                    segment.size = 0.2,
                    show.legend = FALSE
    ) +
    theme(panel.background = element_blank(),
          plot.title = element_text(hjust=0.5, size = title_sz),
          #axis.ticks = element_blank(),
          axis.title.x=element_blank(),
          axis.text.x = element_text(angle = 90, vjust=0.5),
          #axis.text.y = element_blank(), #y-axis lables blank
          panel.grid.major.y = element_line(colour = "#d3d3d3",
                                            size = 0.2),
          panel.grid.minor.y = element_line(colour = "#d3d3d3"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.title = element_blank())+#,#no legend title
    #legend.text=element_text(size=8),
    #legend.position = c(1.2, 0.75),
    #plot.margin = unit(c(.5,19,.5,.5), "lines")) + 
    coord_cartesian(xlim=c(x_strt+2, x_end+lbl_space),
                    ylim=c(y_strt, y_end))
}

# FUNCTION FOR PLOTTIG RAW DATA BY DIFF GROUPINGS
# (with ggrepel labels)
# DOMESTIC EMPLOYEES
plot_by_grp_dom <- function(dta,x,y,
                        group,colour,
                        size,w1,w2,leg_nm,
                        breaks,labels,color_vals,
                        x_strt,x_end,y_strt,y_end,
                        x_unit,y_unit,
                        y_axis,
                        title,title_sz,
                        agy_type,
                        lab_sz,lbl_thresh,lbl_space){
  ggplot(dta,
         aes_string(x = x,
                    y = y,
                    group = group,
                    colour = colour,
                    size = size))+
    geom_line(data = subset(dta, agy_name!="All Federal Agencies")) + #background
    geom_line(data = subset(dta, agy_typ2==agy_type |
                              agy_name=="All Federal Agencies")) + #focus agency
    geom_segment(aes(x = 1998, y= 1, xend=2018, yend=1),
                 color = "black", size = .5) +
    labs(y = y_axis,
         title = title) +
    scale_color_manual(name= leg_nm,
                       breaks=breaks,
                       str_wrap(labels, 20),
                       values = color_vals,#) +#,
                       guide = 'none') + # no legend
    scale_size_manual(values=c(w1,w2),
                      guide = 'none')+
    scale_x_continuous(limits = c(x_strt-1,x_end+lbl_space),
                       breaks=seq(x_strt-1,x_end+1,x_unit)) +
    #scale_y_continuous(breaks=seq(y_strt,y_end,y_unit)) +
    scale_y_continuous(trans='log10',breaks=seq(y_strt+.05,y_end,.1)) +
    annotate(geom = "rect", xmin = x_end+.5, xmax = Inf,
             ymin = 0.01, ymax = Inf, fill = "white" ) + 
    # ^ this draws a white rectangle over plot to hide horizontal lines, etc.
    geom_text_repel(data = subset(dta, (yr == max(yr) &
                                          n>=lbl_thresh) & 
                                    (agy_typ2==agy_type |
                                       agy_name=="All Federal Agencies")),
                    aes(label=str_wrap(agy_name2, width = 65)),
                    size = lab_sz,
                    hjust = 0,
                    direction = "y",
                    nudge_x = 2,
                    segment.size = 0.2,
                    show.legend = FALSE
    ) +
    theme(panel.background = element_blank(),
          plot.title = element_text(hjust=0.5, size = title_sz),
          #axis.ticks = element_blank(),
          axis.title.x=element_blank(),
          axis.text.x = element_text(angle = 90, vjust=0.5),
          #axis.text.y = element_blank(), #y-axis lables blank
          panel.grid.major.y = element_line(colour = "#d3d3d3",
                                            size = 0.2),
          panel.grid.minor.y = element_line(colour = "#d3d3d3"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.title = element_blank())+#,#no legend title
    #legend.text=element_text(size=8),
    #legend.position = c(1.2, 0.75),
    #plot.margin = unit(c(.5,19,.5,.5), "lines")) + 
    coord_cartesian(xlim=c(x_strt+2, x_end+lbl_space),
                    ylim=c(y_strt, y_end))
} 


###### SAVE AND GROUPING FUNCTIONS #######

# Set figure path for saving
fig_path = "Figures"

combo2 <- function(a,b,state,cl){
  f <- ggarrange(a,b, ncol = 1, nrow = 2,
                 #labels = c("A","B","C"),
                 align = "v", common.legend = cl,
                 legend = "none",
                 heights = c(1, 1))
  #  annotate_figure(f,
  #                  top = text_grob(state, size = 12))
}

combo4 <- function(a,b,c,d,state,cl){
  f <- ggarrange(a,b,c,d, ncol = 2, nrow = 2,
                 #labels = c("A","B","C"),
                 align = "v", common.legend = cl,
                 legend = "none",
                 heights = c(1, 1))
#  annotate_figure(f,
#                  top = text_grob(state, size = 12))
}

# Save plot (function)
save_plot <- function(root,addon,w,h,path){
  ggsave(paste(root,addon,".png",
               sep = ""), plot = last_plot(), path = path,
         width = w, height = h, units = "in", dpi = 380,
         limitsize = FALSE)
}
###### FILTER OBSERVATIONS BY SIZE/DURATION ######

# Below applies to overall trend plots, not to alluvial plots
# of composition of professions in agencies.

# drop agencies for all years when they have fewer than x
# employees in any year. That is, create df without these
# agencies.
# (use domestic employees)
x=25
e98_17plot <- e98_17 %>%
  group_by(AGYSUB) %>%
  filter(any(n_dom>x)) #drop entire group if domestic employment ever < x

# make variable coding for number of year observations
e98_17plot <- e98_17plot %>%
  group_by(AGYSUB) %>%
  mutate(yr_obs = n())

# reorder
e98_17plot <- e98_17plot %>%
  dplyr::select("AGYSUB","agy_name","yr","n","n_dom",
                "n_index","agy_typ2","yr_obs",everything())

# drop agencies with less than y years of observations
# (e.g. agencies without all years)
y = 20
e98_17plot <- e98_17plot[which(e98_17plot$yr_obs>=y), ]

#### ### ## # # ## ### ### ## # # ## ### ### ## # # ##
# NOTE: for some reason, my code produces two sets of
# "All Federal Agencies." The extra set has the
# variable "agy_full"==0. Drop those observations.
e98_17plot <- e98_17plot[e98_17plot$agy_full!="0", ]
#### ### ## # # ## ### ### ## # # ## ### ### ## # # ##

# drop agencieswith index greater than TOP
# and less than BOTTOM
TOP = 10
BOTTOM = 0.1
e98_17plot <- e98_17plot[which(e98_17plot$n_index<=TOP &
                                 e98_17plot$n_index>=BOTTOM), ]

# relable Office of the Secretary of the Interior" as
# "Management and Administration", not "Nature and Resources"
#e98_17plot$agy_typ2[e98_17plot$AGYSUB=="IN01"] <-
#  "Management and Administration"
###### PLOT ABSOLUTE ######

# Consider the following plot a piece of data-driven art.
# :)
plot_raw_by_grp(e98_17plot,"yr","n",
                "AGYSUB","agy_typ2",
                "Substantive Agency Focus",
                agy_codes,agy_codes,colors,
                1998,2017,10,2300000,
                "Number of Civilian Employees")

###### PLOT INDEX PRIMARY ######

e98_17_plot_nr <-
  e98_17plot[e98_17plot$agy_typ2=="Nature and Resources", ]
#reorder
e98_17_plot_nr <- e98_17_plot_nr %>%
  dplyr::select("AGYSUB","agy_name","yr","n","n_dom",
                "n_index","agy_typ2","yr_obs",everything())

#reorder
e98_17 <- e98_17 %>%
  dplyr::select("AGYSUB","agy_name","yr","n","n_dom",
                "n_index","agy_typ2",everything())


# !! Note that all of the following plots EXCEPT 
# crime and law consider domestic employees only
# (i.e. employess based in  the 50 U.S. states.)
# employees stationed abroad (civilian employees
# of the DoD, department of state, and so on),
# are not included---except in the crime and law
# category, where locations of employees are often
# not disclosed to protect identities, and so 
# aggregate counts--not sums of state-level counts,
# are used to produce plots.

# Nature and Resources
# The below plot is the plot for the figure published in
# ESTS, with total employees (not just domestic ones)
p1 <- plot_by_grp(e98_17plot,"yr","n_index",
                      "AGYSUB","agy_typ2",
                     "plot_size",.5,2,"Substantive Agency Focus",
                     agy_codes,agy_codes,colors_NaR,
                     1997,2018,.55,1.5,
                     2,.1,
                     "Employment Relative to 1998 (1 = no change)",
                     "Nature and Resources",12,
                     "Nature and Resources",
                     2.5,50,8) # label limit set at 50
p1
save_plot("Natureand_Resources","_ESTS_1998_2017",10,7,fig_path)

# the following plot corrects to DOMESTIC employees only. It also
# plots nature and resources on the same scale as all of the 
# following plots in other categories, to maximize comparability.
p1a <- plot_by_grp_dom(e98_17plot,"yr","n_dindex",
                  "AGYSUB","agy_typ2",
                  "plot_size",.5,2,"Substantive Agency Focus",
                  agy_codes,agy_codes,colors_NaR,
                  1997,2018,.45,2,
                  2,.1,
                  "Employment Relative to 1998 (1 = no change)",
                  "Nature and Resources",12,
                  "Nature and Resources",
                  2.5,50,8) # label limit set at 50
p1a
save_plot("Natureand_Resources","_1998_2017",10,7,fig_path)



# Research, Arts, and Sciences
p2 <- plot_by_grp_dom(e98_17plot,"yr","n_dindex",
                  "AGYSUB","agy_typ2",
                  "plot_size",.5,2,"Substantive Agency Focus",
                  agy_codes,agy_codes,colors_SHA,
                  1997,2018,.45,2,
                  2,.1,
                  "Employment Relative to 1998 (1 = no change)",
                  "Research, Arts, and Sciences",12,
                  "Research, Arts, and Sciences",
                  2.0,50,8) # label limit set at 50
p2
save_plot("Research_Arts_Sciences","_1998_2017",10,7,fig_path)


# Health and Social Welfare
p3 <- plot_by_grp_dom(e98_17plot,"yr","n_dindex",
                  "AGYSUB","agy_typ2",
                  "plot_size",.5,2,"Substantive Agency Focus",
                  agy_codes,agy_codes,colors_HSW,
                  1997,2018,.45,2,
                  2,.1,
                  "Employment Relative to 1998 (1 = no change)",
                  "Health and Social Welfare",12,
                  "Health and Social Welfare",
                  1.5,50,8)# label limit set at 50
p3
save_plot("Health_and_Social_Welfare","_1998_2017",10,7,fig_path)


# Crime and Law
# note: this plot uses aggregate counts for all "crime
# and law" employees, rather than the total number of 
# domestic employees summed across states.
p4 <- plot_by_grp(e98_17plot,"yr","n_index",
                  "AGYSUB","agy_typ2",
                  "plot_size",.5,2,"Substantive Agency Focus",
                  agy_codes,agy_codes,colors_CaL,
                  1997,2018,.45,2,
                  2,.1,
                  "Employment Relative to 1998 (1 = no change)",
                  "Crime and Law (including international)",12,
                  "Crime and Law",
                  2.5,25,8)# label limit set at 25
p4
save_plot("Crime_and_Law","_1998_2017",10,7,fig_path)



# Money, Finance, and Banking
p5 <- plot_by_grp_dom(e98_17plot,"yr","n_dindex",
                  "AGYSUB","agy_typ2",
                  "plot_size",.5,2,"Substantive Agency Focus",
                  agy_codes,agy_codes,colors_MBF,
                  1997,2018,.45,2,
                  2,.1,
                  "Employment Relative to 1998 (1 = no change)",
                  "Money, Finance, and Banking",12,
                  "Money, Finance, and Banking",
                  2.5,25,8)
p5
save_plot("Money_Finance_Banking","_1998_2017",10,7,fig_path)


# Defense
p6 <- plot_by_grp_dom(e98_17plot,"yr","n_dindex",
                  "AGYSUB","agy_typ2",
                  "plot_size",.5,2,"Substantive Agency Focus",
                  agy_codes,agy_codes,colors_DEF,
                  1997,2018,.45,2,
                  2,.1,
                  "Employment Relative to 1998 (1 = no change)",
                  "Defense",12,
                  "Defense",
                  1.5,2000,8) #label threshold set to 2000
p6
save_plot("Defense","_1998_2017",10,7,fig_path)

# Work and Labor
p7 <- plot_by_grp_dom(e98_17plot,"yr","n_dindex",
                  "AGYSUB","agy_typ2",
                  "plot_size",.5,2,"Substantive Agency Focus",
                  agy_codes,agy_codes,colors_WaL,
                  1997,2018,.45,2,
                  2,.1,
                  "Employment Relative to 1998 (1 = no change)",
                  "Work and Labor",12,
                  "Work and Labor",
                  2.5,25,8) #label threshold set to 25
p7
save_plot("Work_and_Labor","_1998_2017",10,7,fig_path)

# Commerce and Information
p8 <- plot_by_grp_dom(e98_17plot,"yr","n_dindex",
                  "AGYSUB","agy_typ2",
                  "plot_size",.5,2,"Substantive Agency Focus",
                  agy_codes,agy_codes,colors_CaI,
                  1997,2018,.45,2,
                  2,.1,
                  "Employment Relative to 1998 (1 = no change)",
                  "Commerce and Information",12,
                  "Commerce and Information",
                  2.0,100,8) #label threshold set to 100
p8
save_plot("Commerce_and_Information","_1998_2017",10,7,fig_path)

# Education
p9 <- plot_by_grp_dom(e98_17plot,"yr","n_dindex",
                  "AGYSUB","agy_typ2",
                  "plot_size",.5,2,"Substantive Agency Focus",
                  agy_codes,agy_codes,colors_EDU,
                  1997,2018,.45,2,
                  2,.1,
                  "Employment Relative to 1998 (1 = no change)",
                  "Education",12,
                  "Education",
                  2.5,5,8) #label threshold set to 5
p9
save_plot("Education","_1998_2017",10,7,fig_path)

# Foreign Affairs
p10 <- plot_by_grp_dom(e98_17plot,"yr","n_dindex",
                  "AGYSUB","agy_typ2",
                  "plot_size",.5,2,"Substantive Agency Focus",
                  agy_codes,agy_codes,colors_FOR,
                  1997,2018,.45,2,
                  2,.1,
                  "Employment Relative to 1998 (1 = no change)",
                  "Foreign Affairs",12,
                  "Foreign Affairs",
                  2.5,25,8) #label threshold set to 25
p10
save_plot("Foreign_Affairs","_1998_2017",10,7,fig_path)

# Management and Administration
p11 <- plot_by_grp_dom(e98_17plot,"yr","n_dindex",
                  "AGYSUB","agy_typ2",
                  "plot_size",.5,2,"Substantive Agency Focus",
                  agy_codes,agy_codes,colors_MaA,
                  1997,2018,.45,2,
                  2,.1,
                  "Employment Relative to 1998 (1 = no change)",
                  "Management and Administration",12,
                  "Management and Administration",
                  1.5,150,8) #label threshold set to 150
p11
save_plot("Management_and_Administration","_1998_2017",10,7,fig_path)

###### PLOT 2X2 CLUSTERS OF EMPLOYMENT TRENDS ######

combo2(p1,p2,"",FALSE)
save_plot("First_Two","_1998_2017",11,15,fig_path)
combo2(p3,p4,"",FALSE)
save_plot("Second_Two","_1998_2017",11,15,fig_path)
combo2(p5,p6,"",FALSE)
save_plot("Third_Two","_1998_2017",11,15,fig_path)
combo2(p7,p8,"",FALSE)
save_plot("Fourth_Two","_1998_2017",11,15,fig_path)
combo2(p9,p10,"",FALSE)
save_plot("Fifth_Two","_1998_2017",11,15,fig_path)
p11
save_plot("Sixth_Two","_1998_2017",11,7.5,fig_path)

combo4(p1,p2,p3,p4,"",FALSE)
save_plot("First_Four","_1998_2017",24,16,fig_path)

combo4(p5,p6,p7,p8,"",FALSE)
save_plot("Second_Four","_1998_2017",24,16,fig_path)

combo4(p9,p10,p11,"","",FALSE)
save_plot("Third_Four","_1998_2017",24,16,fig_path)




################################################################

# scrap paper

# make df with only foreign affairs
e98_17plot_FA <- e98_17plot[e98_17plot$agy_typ2=="Foreign Affairs", ]

# make df with only department of state (AGYSUB=="ST00")
e98_17plot_DoS <- e98_17plot[e98_17plot$AGYSUB=="ST00", ]

# make df with only crime and law
e98_17plot_CaL <- e98_17plot[e98_17plot$agy_typ2=="Crime and Law", ]

# make df with only education
e98_17plot_Edu <- e98_17plot[e98_17plot$agy_typ2=="Education", ]


