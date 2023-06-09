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

col_names_M <- Biblio_col$`Var name` # extract columns name as a vector



################################################################################
################################### Web of Science #############################
################################################################################

# You can work with bibtex and plaintex in the WoS
# Creating a vector with thre files downloaded by the Web of Science platform
wos_file <- c("wos_1_1000_07062023.bib", "wos_1001_1910_07062023.bib")

# Converting the data into a data frame readable in the bibliometrix package and
# Biblioshiy application

W <- convert2df(here("Data", # Load your Web of Science data and save it to W
                     "Raw", 
                     wos_file), 
                dbsource = "wos", 
                format = "bibtex") # options "plaintext", "bibtext", "csv"

class(W) # checking the type of data frame

PF<- rep("wos", nrow(W)) # PF means platform to distinguish docs from scopus and dimensions

W <- cbind(W, PF) # binding the vector PF to M makes M to lose the attributes for biblioshiny


class(W) # checking the type of data frame


################################################################################
################################### Scopus #####################################
################################################################################

# For this workflow you need to use bibtex in Scopus
sco_file <- c("sco_1_1287_07062023.bib", "sco_1_1329_07062023.bib",
              "sco_1_1723_07062023.bib")

S <- convert2df(here("Data", # Load your Scopus data and save it to S
                     "Raw", 
                     sco_file),
                dbsource = "scopus", 
                format = "bibtex") # options "plaintext", "bibtex", "csv"

class(S) # checking the type of data frame

PF<- rep("sco", nrow(S)) # PF means platform to distinguish docs from scopus and dimensions 39 columns
S <- cbind(S, PF) # binding the vector PF to M makes M to lose the attributes for biblioshiny

class(S) # checking the type of data frame


################################################################################
################################### Merging the Data ###########################
################################################################################


# M is the name of the df that merge WOS and SCO 
M <- mergeDbSources(W, S, remove.duplicated = TRUE)
# Delete repeated data
# 


# This code extracts the country from where the authors are
M <- metaTagExtraction(M, 
                        Field = "AU_CO", 
                        sep = ";")

# Summarize by platforms WoS and Scopus 
M %>% group_by(PF) %>% summarise(n())

# Identify which data base are the publications in M 

# M$PF <- ifelse(M$DI %in% S$DI, "sco", ifelse(M$DI %in% W$DI, "wos", NA))

# This will select only the necessary columns to conduct bibliometric analysis
M <- M[,col_names_M]


# This orders your df in chornological order
M = M[order(M$PY), ]

vis_miss(M) # Visualize M

M <- M %>% replace_with_na_all(condition = ~. == "") # converting "" into NA within the D
M <- M %>% replace_with_na_all(condition = ~. == "NA") # converting "" into NA within the D

# This will add the ISBN to the DOI column 
M$DI <- ifelse(is.na(M$DI), M$BN, M$DI)

vis_miss(M) # Visualize M


# Deleting duplicates 
M <-  M %>% distinct(DI, .keep_all = T) # keeping only the unique docs by DOI
M <-  M %>% distinct(TI, .keep_all = T) # keeping only the unique docs by TI title
M <-  M %>% distinct(SR, .keep_all = T) # keeping only the unique docs by SR title


# If you want to filter years manually
M <- M %>% arrange(PY) %>% filter(PY >= 1977 & PY< 2023) # excluding 2023



# If you want to refine your search and include only the publications 
# that contains the word artificial intelligence in the TI and DE
indices <- which(apply(M[, c("TI", "DE")], 1, # filters only TI, DE with the word
                       function(x) any(grepl("artificial intelligence", 
                                             x, ignore.case = TRUE)))) # 

indices <- which(grepl("artificial intelligence", M$TI, ignore.case = TRUE))



# This code select the documents that has Artificial intelligence word in the TI and DE
M <- M[indices, ]


# Perhaps you want to check also what are publications that do not contain AI in the TI
# indices <- which(!(grepl("artificial intelligence", 
#                          M$TI, ignore.case = TRUE) | grepl("artificial intelligence", 
#                                                            M$DE, ignore.case = TRUE)))

class(M)

class(M) <- c("bibliometrixDB", "data.frame")

class(M)

# If you want to save your data frame you can save it in CSV and then convert ti in EXCEL
write_csv(M, file = here("Data", 
                         "Processed", 
                         "M_AI.csv")) # writing as CSV to make readable in biblioshiny


# This will save you data in format rda which is readable in R and biblioshiny
save(M, file = here("Data", 
                    "Processed", 
                    "M_AI.rda"))

biblioshiny()

