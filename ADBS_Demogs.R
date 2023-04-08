library(googlesheets4)
library(dplyr)

# Download (from 10.11.1.30) the main assessment, sociodemographic, and subjectID sheets, and keep them all in one directory

# Allow linking with Google API using the csap.adbs@gmail.com

gs4_auth(email = "csap.adbs@gmail.com")


# Inputting the Unclean Scales & Cleaning them

#------- Main Assessment Sheet ---------#

Main_Assessment <- read.csv("mainassessment_table.csv", header = TRUE, na.strings = "NULL")

Main_Assessment <- Main_Assessment[,c(3,4,7,8)]

Main_Assessment_Excluded <- Main_Assessment[nchar(Main_Assessment$assessmentId) !=9 | 
                                              Main_Assessment$subjectID >170000 | 
                                              duplicated(Main_Assessment$assessmentId, fromLast = T), ]

Main_Assessment_Clean <- Main_Assessment[nchar(Main_Assessment$assessmentId)==9 & 
                                           Main_Assessment$subjectID <170000 & 
                                           !duplicated(Main_Assessment$assessmentId, fromLast = T), ]

Main_Assessment_Clean$Cohort <- ifelse(Main_Assessment_Clean$subjectID %in% c(110000:119999), "Addiction", 
                                       ifelse(Main_Assessment_Clean$subjectID %in% c(120000:129999), "Bipolar",
                                              ifelse(Main_Assessment_Clean$subjectID %in% c(130000:139999), "Dementia",
                                                     ifelse(Main_Assessment_Clean$subjectID %in% c(140000:149999), "OCD",
                                                            ifelse(Main_Assessment_Clean$subjectID %in% c(150000:159999), "Schiz",
                                                                   ifelse(Main_Assessment_Clean$subjectID %in% c(160000:169999), "GPC", NA))))))

colnames(Main_Assessment_Clean)[3:4] <- c("Brief_Date", "Deep_Date")

Main_Assessment_Clean$Brief_Date <- as.Date(Main_Assessment_Clean$Brief_Date)
Main_Assessment_Clean$Deep_Date <- as.Date(Main_Assessment_Clean$Deep_Date)


rm(Main_Assessment, Main_Assessment_Excluded)


# Adding subjectIds

subjecttable <- read.csv("subject_table.csv")

Main_Clean <- merge(Main_Assessment_Clean, 
                    subjecttable[, c("subjectID", "name", "phone", "city", "state")], by="subjectID", all.x=T)

# Sociodemographic cleaning

Sociodem <- read.csv("socio_demographic_details_table1.csv", header = T, na.strings = "NA")

Sociodem_Excluded <- Sociodem[nchar(Sociodem$assessmentId) !=9 | 
                                Sociodem$subjectID >170000 | 
                                duplicated(Sociodem$assessmentId, fromLast = T), ]

Sociodem_Clean <- Sociodem[nchar(Sociodem$assessmentId)==9 & 
                             Sociodem$subjectID <170000 & 
                             !duplicated(Sociodem$assessmentId, fromLast = T), ]

# Clean up the dates

Sociodem_Clean$FullSyndrome <-   gsub("- 1", "- 01", Sociodem_Clean$FullSyndrome)
Sociodem_Clean$FullSyndrome <-   gsub("- 2", "- 02", Sociodem_Clean$FullSyndrome)
Sociodem_Clean$FullSyndrome <-   gsub("- 3", "- 03", Sociodem_Clean$FullSyndrome)
Sociodem_Clean$FullSyndrome <-   gsub("- 4", "- 04", Sociodem_Clean$FullSyndrome)
Sociodem_Clean$FullSyndrome <-   gsub("- 5", "- 05", Sociodem_Clean$FullSyndrome)
Sociodem_Clean$FullSyndrome <-   gsub("- 6", "- 06", Sociodem_Clean$FullSyndrome)
Sociodem_Clean$FullSyndrome <-   gsub("- 7", "- 07", Sociodem_Clean$FullSyndrome)
Sociodem_Clean$FullSyndrome <-   gsub("- 8", "- 08", Sociodem_Clean$FullSyndrome)
Sociodem_Clean$FullSyndrome <-   gsub("- 9", "- 09", Sociodem_Clean$FullSyndrome)
Sociodem_Clean$FullSyndrome <-   gsub("- 010", "- 10", Sociodem_Clean$FullSyndrome)
Sociodem_Clean$FullSyndrome <-   gsub("- 011", "- 11", Sociodem_Clean$FullSyndrome)
Sociodem_Clean$FullSyndrome <-   gsub("- 012", "- 12", Sociodem_Clean$FullSyndrome)
Sociodem_Clean$FullSyndrome <-   gsub(" ", "", Sociodem_Clean$FullSyndrome)
Sociodem_Clean$FullSyndrome[Sociodem_Clean$FullSyndrome == ""] <- NA
Sociodem_Clean$FullSyndrome <- ifelse(is.na(Sociodem_Clean$FullSyndrome), NA, sapply(Sociodem_Clean$FullSyndrome , paste, "-01", sep=""))

Sociodem_Clean$InitialSyndrome <-   gsub("- 1", "- 01", Sociodem_Clean$InitialSyndrome)
Sociodem_Clean$InitialSyndrome <-   gsub("- 2", "- 02", Sociodem_Clean$InitialSyndrome)
Sociodem_Clean$InitialSyndrome <-   gsub("- 3", "- 03", Sociodem_Clean$InitialSyndrome)
Sociodem_Clean$InitialSyndrome <-   gsub("- 4", "- 04", Sociodem_Clean$InitialSyndrome)
Sociodem_Clean$InitialSyndrome <-   gsub("- 5", "- 05", Sociodem_Clean$InitialSyndrome)
Sociodem_Clean$InitialSyndrome <-   gsub("- 6", "- 06", Sociodem_Clean$InitialSyndrome)
Sociodem_Clean$InitialSyndrome <-   gsub("- 7", "- 07", Sociodem_Clean$InitialSyndrome)
Sociodem_Clean$InitialSyndrome <-   gsub("- 8", "- 08", Sociodem_Clean$InitialSyndrome)
Sociodem_Clean$InitialSyndrome <-   gsub("- 9", "- 09", Sociodem_Clean$InitialSyndrome)
Sociodem_Clean$InitialSyndrome <-   gsub("- 010", "- 10", Sociodem_Clean$InitialSyndrome)
Sociodem_Clean$InitialSyndrome <-   gsub("- 011", "- 11", Sociodem_Clean$InitialSyndrome)
Sociodem_Clean$InitialSyndrome <-   gsub("- 012", "- 12", Sociodem_Clean$InitialSyndrome)
Sociodem_Clean$InitialSyndrome <-   gsub(" ", "", Sociodem_Clean$InitialSyndrome)
Sociodem_Clean$InitialSyndrome[Sociodem_Clean$InitialSyndrome == ""] <- NA
Sociodem_Clean$InitialSyndrome <- ifelse(is.na(Sociodem_Clean$InitialSyndrome), NA, sapply(Sociodem_Clean$InitialSyndrome , paste, "-01", sep=""))

Sociodem_Clean$FirstTYear <-   gsub("- 1", "- 01", Sociodem_Clean$FirstTYear)
Sociodem_Clean$FirstTYear <-   gsub("- 2", "- 02", Sociodem_Clean$FirstTYear)
Sociodem_Clean$FirstTYear <-   gsub("- 3", "- 03", Sociodem_Clean$FirstTYear)
Sociodem_Clean$FirstTYear <-   gsub("- 4", "- 04", Sociodem_Clean$FirstTYear)
Sociodem_Clean$FirstTYear <-   gsub("- 5", "- 05", Sociodem_Clean$FirstTYear)
Sociodem_Clean$FirstTYear <-   gsub("- 6", "- 06", Sociodem_Clean$FirstTYear)
Sociodem_Clean$FirstTYear <-   gsub("- 7", "- 07", Sociodem_Clean$FirstTYear)
Sociodem_Clean$FirstTYear <-   gsub("- 8", "- 08", Sociodem_Clean$FirstTYear)
Sociodem_Clean$FirstTYear <-   gsub("- 9", "- 09", Sociodem_Clean$FirstTYear)
Sociodem_Clean$FirstTYear <-   gsub("- 010", "- 10", Sociodem_Clean$FirstTYear)
Sociodem_Clean$FirstTYear <-   gsub("- 011", "- 11", Sociodem_Clean$FirstTYear)
Sociodem_Clean$FirstTYear <-   gsub("- 012", "- 12", Sociodem_Clean$FirstTYear)
Sociodem_Clean$FirstTYear <-   gsub(" ", "", Sociodem_Clean$FirstTYear)
Sociodem_Clean$FirstTYear[Sociodem_Clean$FirstTYear == ""] <- NA
Sociodem_Clean$FirstTYear <- ifelse(is.na(Sociodem_Clean$FirstTYear), NA, sapply(Sociodem_Clean$FirstTYear , paste, "-01", sep=""))

Sociodem_Clean$Pt_DOB <- as.Date(Sociodem_Clean$Pt_DOB)
Sociodem_Clean$FullSyndrome <- as.Date(Sociodem_Clean$FullSyndrome)
Sociodem_Clean$InitialSyndrome <- as.Date(Sociodem_Clean$InitialSyndrome)
Sociodem_Clean$FirstTYear <- as.Date(Sociodem_Clean$FirstTYear)

# Calculate Age at Onsets
Sociodem_Clean$AAO_Init <- round(as.numeric(Sociodem_Clean$InitialSyndrome - Sociodem_Clean$Pt_DOB)/365, digits=0)
Sociodem_Clean$AAO_Full <- round(as.numeric(Sociodem_Clean$FullSyndrome - Sociodem_Clean$Pt_DOB)/365, digits=0)
Sociodem_Clean$AAO_FirstRx <- round(as.numeric(Sociodem_Clean$FirstTYear - Sociodem_Clean$Pt_DOB)/365, digits=0)

# Clean up diagnoses into 5 columns
Sociodem_Clean$Diag_Schiz <- ifelse(rowSums(sapply(Sociodem_Clean[,c("Psy1", "Psy2", "Psy3", "Psy4", "Psy5")], grepl, pattern = "Schiz|Psychos|Psychotic Disorder"))>0,1,0)
Sociodem_Clean$Diag_OCD <- ifelse(rowSums(sapply(Sociodem_Clean[,c("Psy1", "Psy2", "Psy3", "Psy4", "Psy5")], grepl, pattern = "Obsessive|OCD"))>0,1,0)
Sociodem_Clean$Diag_BPAD <- ifelse(rowSums(sapply(Sociodem_Clean[,c("Psy1", "Psy2", "Psy3", "Psy4", "Psy5")], grepl, pattern = "Bipolar|BPAD|Schizoaffective"))>0,1,0)
Sociodem_Clean$Diag_SUD <- ifelse(rowSums(sapply(Sociodem_Clean[,c("Psy1", "Psy2", "Psy3", "Psy4", "Psy5")], grepl, pattern = "Alcohol|ADS|Cannabis|Opioid"))>0,1,0)
Sociodem_Clean$Diag_AlzD <- ifelse(rowSums(sapply(Sociodem_Clean[,c("Psy1", "Psy2", "Psy3", "Psy4", "Psy5")], grepl, pattern = "Alz|Demen"))>0,1,0)

# Remove the useless characters introduced in the SQL backend
Sociodem_Clean[,"Psy1"] <- as.factor(gsub("Â", "",Sociodem_Clean[,"Psy1"]))
Sociodem_Clean[,"Psy2"] <- as.factor(gsub("Â", "",Sociodem_Clean[,"Psy2"]))
Sociodem_Clean[,"Psy3"] <- as.factor(gsub("Â", "",Sociodem_Clean[,"Psy3"]))
Sociodem_Clean[,"Psy4"] <- as.factor(gsub("Â", "",Sociodem_Clean[,"Psy4"]))
Sociodem_Clean[,"Psy5"] <- as.factor(gsub("Â", "",Sociodem_Clean[,"Psy5"]))

Sociodem_Clean$Category <- ifelse(Sociodem_Clean$subjectID > 160000, "PHC", ifelse(rowSums(select(Sociodem_Clean, starts_with("Diag_")))>0, "Affected", "Unaffected FDR"))

# Adding the FHD/FDR scores

FHD <- read.csv("FHD.csv")
FDR <- read.csv("FDR.csv")

Sociodem_Clean <- merge(Sociodem_Clean, FHD, by="subjectID", all.x=T)
Sociodem_Clean <- merge(Sociodem_Clean, FDR, by="subjectID", all.x=T)
names(Sociodem_Clean)

Sociodem_Reqd <- Sociodem_Clean [,c("assessmentId", "P_NO", "Pt_Gender", "Pt_Age", "NoEducation",
                                    "Psy1","Psy2","Psy3","Psy4","Psy5", "AAO_Init", "AAO_Full", "AAO_FirstRx",
                                    "Diag_Schiz","Diag_BPAD","Diag_OCD","Diag_SUD","Diag_AlzD", 
                                    "FHD_Schiz","FHD_BPAD","FHD_OCD","FHD_SUD","FHD_AlzD", 
                                    "FDR_Schiz","FDR_BPAD","FDR_OCD","FDR_SUD","FDR_AlzD", 
                                    "Category")]


rm(Sociodem, Sociodem_Excluded)


# --------------Getting sampleNos and FamilyIDs from the PBMC GoogleSheet-----------------#

PBMC <- read_sheet("https://docs.google.com/spreadsheets/d/1VdCihxlgBvHn6Pz8nVOiv6J3TmJqbLhAr4vnV06M_mQ/edit#gid=1758808820", 
                   sheet=1, range = "A:E", na = "NULL")

# Turning the Tibble into a data.frame

PBMC$Assessment_ID[PBMC$Assessment_ID == "NULL"] <- NA
PBMC$ADBS_ID[PBMC$ADBS_ID == "NULL"] <- NA
PBMC$D_Number[PBMC$D_Number == "NULL"] <- NA
PBMC$Family_Number[PBMC$Family_Number == "NULL"] <- NA

PBMC2 <- data.frame(cbind(unlist(PBMC$Sample_No),
                          unlist(PBMC$ADBS_ID), 
                          unlist(PBMC$Assessment_ID), 
                          unlist(PBMC$Family_Number),
                          unlist(PBMC$D_Number)))

colnames(PBMC2) <- c("sample_no", "subjectID", "assessmentId", "Family_Number","D_Number")

PBMC_Duplicates_AssessId <- PBMC2[duplicated(PBMC2$assessmentId) |  
                                    duplicated(PBMC2$assessmentId, fromLast = T), ]

PBMC_Duplicates_AssessId <- PBMC_Duplicates_AssessId[!(is.na(PBMC_Duplicates_AssessId$subjectID)), ]

PBMC_Duplicates_SampleNo <- PBMC2[duplicated(PBMC2$sample_no) |  
                                    duplicated(PBMC2$sample_no, fromLast = T), ]

PBMC_Duplicates_SampleNo <- PBMC_Duplicates_SampleNo[!(is.na(PBMC_Duplicates_SampleNo$subjectID)), ]

PBMC2 <- PBMC2[!is.na(PBMC2$sample_no),]

# Only the ADBS Samples:
PBMC3 <- PBMC2[!duplicated(PBMC2$assessmentId) & !(is.na(PBMC2$assessmentId)),]

# Merging them all! 

Audit_Full <- merge(Main_Clean, PBMC3[,-2], by="assessmentId", all.x=T)

Audit_Full <- Audit_Full  %>% dplyr::select("Cohort", "Family_Number", "subjectID", "D_Number", "assessmentId", 
                                            "sample_no", "name", "phone", "city", "state", "Brief_Date", "Deep_Date")

Audit_Full <- merge(Audit_Full, Sociodem_Reqd, by="assessmentId", all.x=T)




# --------------- Removing the COVID Assessments ------------------ #

COVID_Start <- as.Date("2020-04-01")
COVID_End <- as.Date("2021-07-01")

Phase1 <- Audit_Full[Audit_Full$assessmentId < 110000000, ]
Phase2 <- Audit_Full[Audit_Full$assessmentId > 110000000,]

Phase1$COVID_Assessment <- ifelse(Phase1$assessmentId > 102000000 &
                                    Phase1$Brief_Date > COVID_Start &
                                    Phase1$Brief_Date < COVID_End &
                                    is.na(Phase1$sample_no), "Yes", "")

Phase2$COVID_Assessment <- ifelse(Phase2$assessmentId > 112000000 &
                                    Phase2$Brief_Date > COVID_Start &
                                    Phase2$Brief_Date < COVID_End &
                                    is.na(Phase2$sample_no), "Yes", "")

Phase1 <- Phase1[Phase1$COVID_Assessment != "Yes",]
Phase2 <- Phase2[Phase2$COVID_Assessment != "Yes",]

Phase1 <- Phase1[!is.na(Phase1$assessmentId),]
Phase2 <- Phase2[!is.na(Phase2$assessmentId),]

write.csv(Phase1, "Phase1.csv")
write.csv(Phase2, "Phase2.csv")
