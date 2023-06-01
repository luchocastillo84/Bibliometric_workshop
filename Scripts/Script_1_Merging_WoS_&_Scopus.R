# Load the necessary packages
library(tidyverse)
library(here)
library(bibliometrix)
library(readr)
library(visdat)
library(stringr)
library(dplyr)
library(readxl)
library(naniar)
library(readr)


################################################################################
############################# Load Bibliometric Columns ########################
################################################################################

Biblio_col <- read_excel(here("Data", # Load the necessary column to conduct 
                              "Tags", # Bibliometric analysis
                              "colnames_M.xlsx")) #

col_names_M <- Biblio_col$`Var name` # extract columns name

################################################################################
################################### Web of Science #############################
################################################################################

# Creating a vector with thre files downloaded by the Web of Science platform
wos_file <- c("wos_1_1000_30052023.txt", "wos_1001_2000_30052023.txt",
              "wos_2001_2964_30052023.txt")

# Converting the data into a data frame readable in the bibliometrix package and
# Biblioshiy application

raw_W <- convert2df(here("Data", # Keeping the origianl data in a seperate DF
                         "Raw", 
                         wos_file), 
                    dbsource = "wos", 
                    format = "plaintext")

class(raw_W)

W <- convert2df(here("Data", # This is a copy of the raw data to modify
                     "Raw", 
                     wos_file), 
                dbsource = "wos", 
                format = "plaintext")

# this code bring the country of each author into the column AU_CO
W <- metaTagExtraction(W, 
                       Field = "AU_CO", 
                       sep = ";")

PF<- rep("wos", nrow(W)) # PF means platform to distinguish docs from scopus

W <- cbind(W, PF) # binding the column to W

 
# http://www.bibliometrix.org/vignettes/Data-Importing-and-Converting.html
# The reduction was from 74 columns to 29 

W <- W[ , c(col_names_M)]

# This will add International Standart Book Number ISBN to DOI column 
W$DI <- ifelse(is.na(W$DI), W$BN, W$DI) 


colnames(W) # check the column names

vis_miss(W) #  visualize missing values 

W <-  W %>% distinct(DI, .keep_all = T) # keeping only the unique docs by DI or DOI


################################################################################
################################### Scopus #####################################
################################################################################

raw_S <- convert2df(here("Data", # raw data frame
                         "Raw", 
                         "sco_1_1405_30052023.csv"),
                    dbsource = "scopus", 
                    format = "csv")

S <- convert2df(here("Data",  # processed data frame for bibliometrics 36 columns
                     "Raw", 
                     "sco_1_1405_30052023.csv"),
                dbsource = "scopus", 
                format = "csv")

# this code bring the country of each author into the column AU_CO 37 columns
S <- metaTagExtraction(S, 
                       Field = "AU_CO", 
                       sep = ";")

S <- metaTagExtraction(S, 
                       Field = "AU_UN", 
                       sep = ";")

vis_miss(S)

S <- S %>% replace_with_na_all(condition = ~. == "") # converting "" into NA within the df
S <- S %>% replace_with_na_all(condition = ~. == "NA") # converting "" into NA within the df

vis_miss(S)

S <- S %>% mutate("SC" = "", # add research areas (empty column) 
                  "WC" = "", # add WoS categories (empty column)
                  "Z9" = "") %>% # add Total Times Cited Count (empty column) total 42 columns
  rename("TI" = "TI", # renaming title according to WoS tags
         "ID" = "ID",# keywords
         "PU" = "PU", 
         "BN" = "URL")

PF<- rep("sco", nrow(S)) # PF means platform to distinguish docs from scopus and dimensions 39 columns
S <- cbind(S, PF) # binding the vector PF to M makes M to lose the attributes for biblioshiny

S <- S[, col_names_M] # match tag and number of columns from the Wos 29 columns

S <- S %>% replace_with_na_all(condition = ~. == "") # converting "" into NA within the S

S <-  S %>% distinct(DI, .keep_all = T) # keeping only the unique docs by DI or DOI
rownames(S) <- S$SR # using SR as row names
vis_miss(S)

################################################################################
################################### Merging the Data ###########################
################################################################################


SW <- anti_join(S, W, by= c("DI", "TI")) # filtering docs that only appear in SCOPUS and not WOS 

# M is the name of the df that (M) binds WOS and SCO 
M <-  rbind(W, SW) # binding the three data bases into B
M <- M %>% replace_with_na_all(condition = ~. == "") # converting "" into NA within the D
M <- M %>% replace_with_na_all(condition = ~. == "NA") # converting "" into NA within the D
vis_miss(M)



M <-  M %>% distinct(DI, .keep_all = T) # keeping only the unique docs by DOI
M <-  M %>% distinct(TI, .keep_all = T) # keeping only the unique docs by TI title
M <-  M %>% distinct(SR, .keep_all = T) # keeping only the unique docs by SR title

# If you want to filter years manualy
M <- M %>% arrange(PY) %>% filter(PY >= 2000 & PY< 2023) # excluding 2023


indices <- which(apply(M[, c("TI", "DE")], 1, # filters only TI, DE with the pattern
                       function(x) any(grepl("artificial intelligence", 
                                             x, ignore.case = TRUE)))) #  

M <- M[indices, ]

write_csv(M, file = here("Data", 
                         "Processed", 
                         "M_AI.csv")) # writing as CSV to make readable in biblioshiny


class(M)

class(M) <- c("bibliometrixDB", "data.frame")

class(M)


save(M, file = here("Data", 
                    "Processed", 
                    "M_AI.rda"))
