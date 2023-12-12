## https://labjs.readthedocs.io/en/latest/learn/deploy/3c-jatos.html
## https://labjs.readthedocs.io/en/latest/learn/deploy/3-third-party.html


# sets the directory of location of this script as the current directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

rm(list=ls(all=TRUE))
graphics.off()

############################################################################
# Pakete, Daten laden
############################################################################

### Pakete
# This code relies on the pacman, tidyverse and jsonlite packages
require(pacman)
p_load('tidyverse', 'jsonlite', 'pracma', 'xlsx', 'lme4',
       'performance', 'lattice', 'stargazer', 'psych')


### Daten
# Read the text file from JATOS ...
dir()
read_file('jatos_results_20210506103201.txt') %>%
  # ... split it into lines ...
  str_split('\n') %>% first() %>%
  # ... filter empty rows ...
  discard(function(x) x == '') %>%
  # ... parse JSON into a data.frame
  map_dfr(fromJSON, flatten=TRUE) -> dat

# select only PIDs who answered all questions:
dir()
dat_prolific <- read.csv(file = "prolific_export_6090066157f35e62068ba2ff.csv")
head(dat_prolific)

sum(dat_prolific$entered_code != "")
sum(dat_prolific$entered_code == "")
# sel_pids <- dat_prolific$participant_id[dat_prolific$entered_code != ""]


############################################################################
# Deskreptiv, ID Variable, Datenzusammenführung, Datenaufbereitung, Daten speichern
############################################################################
### Deskreptiv N
## N:
sum(!is.na(dat$prolific_pid))
sum(dat$sender_id == "0_0_0")

length(unique(dat$prolific_pid))

### ID Variable dat 1
dat$ID <- NA
tmp_IDcounter <- 0
for(i in 1:nrow(dat)){
  if(!is.na(dat$prolific_pid[i])){
    # tmp <- dat$prolific_pid[i]
    tmp_IDcounter = tmp_IDcounter + 1
  }
  dat$ID[i] <- tmp_IDcounter
}

## N descreptive
table(dat$ID)

### select all participants > sel_per
tmp_sel <- data.frame(ID = unique(dat$ID), PID = dat$prolific_pid[!is.na(dat$prolific_pid)],
                      solved_perc = as.numeric(table(dat$ID) / max(table(dat$ID)) * 100))

tmp_sel
nrow(tmp_sel); length(unique(tmp_sel$PID))

sel_per <- 70
# tmp_sel[tmp_sel$solved_perc > sel_per, ]
tmp_sel[tmp_sel$solved_perc <= sel_per, ]
# tmp_sel$PID[tmp_sel$solved_perc <= sel_per] %in% tmp_sel$PID[tmp_sel$solved_perc > sel_per]
sum(!tmp_sel$PID[tmp_sel$solved_perc <= sel_per] %in% tmp_sel$PID[tmp_sel$solved_perc > sel_per])
sel_pids <- tmp_sel$PID[tmp_sel$solved_perc > sel_per]
tmp_sel <- tmp_sel[tmp_sel$PID %in% sel_pids,]
nrow(tmp_sel); length(unique(tmp_sel$PID))

rm(sel_per); rm(sel_pids)




dat <- dat[dat$ID %in% tmp_sel$ID,]

dat$ID2 <- NA

for(i in 1:length(unique(tmp_sel$PID))){
  tmp <- tmp_sel$ID[tmp_sel$PID == unique(tmp_sel$PID)[i]]
  dat$ID2[dat$ID %in% tmp] <- i
}

table(dat$ID)
table(dat$ID2)
dat$ID <- dat$ID2
dat$ID2 <- NULL

length(unique(dat$ID))

rm(tmp_sel)

################
# Datenzusammenführung
################
###### simple case:
# glimpse(dat)
colnames(dat)



# dat$ID[dat$prolific_pid == "5e6473fbc15dbe1f71eea95b" & !is.na(dat$prolific_pid)]
# dat$loginID[dat$ID == 46]
# dat$dummy_honestycontract[dat$ID == 46]
# dat$ID[dat$prolific_pid == "5e6473fbc15dbe1f71eea95b" & !is.na(dat$prolific_pid)]
# dat$idcodefinal[dat$ID == 1 & !is.na(dat$idcodefinal)]
############################################################################
# Datenaufbereitung
############################################################################

### save all feedback as Excel file
write.xlsx2(x = data.frame(ID = dat[, "ID"][!is.na(dat[, "feedback_critic"]) & dat[, "feedback_critic"] != ""],
                           feedback = dat[, "feedback_critic"][!is.na(dat[, "feedback_critic"]) & dat[, "feedback_critic"] != ""]),
            file = "feedback_answered.xlsx")




write.xlsx2(x = colnames(dat),
            file = "colnames.xlsx")
##############
# Fragen ohne Loop (Fragebogen + demographische Fragen)
##############
ques_mixed <- c("prolific_pid", "loginID", "loginPassword", "idcodefinal", "dummy_informedconsent",
                "check_caminst", "dummy_honestycontract")


# socio-demographic questions
ques_demographic <- c("sociodemo_age", "sociodemo_gender", "sociodemo_education",
                      "sociodemo_impexp", "sociodemo_impexpwho",
                      "sociodemo_country", "sociodemo_faith")

# PANAS scale
ques_PANAS_p <- str_subset(string = colnames(dat), pattern = "^panas.+p$")
ques_PANAS_p <- sort(ques_PANAS_p); ques_PANAS_p

ques_PANAS_n <- str_subset(string = colnames(dat), pattern = "^panas.+n$")
ques_PANAS_n <- sort(ques_PANAS_n); ques_PANAS_n

# TAM scale
ques_TAM <- str_subset(string = colnames(dat), pattern = "^tam")
ques_TAM <- sort(ques_TAM); ques_TAM

# ethics scale
ques_ethics_relativist <- str_subset(string = colnames(dat), pattern = "^relativist")
ques_ethics_relativist <- sort(ques_ethics_relativist); ques_ethics_relativist

ques_ethics_contractualist <- str_subset(string = colnames(dat), pattern = "^contractualist")
ques_ethics_contractualist <- sort(ques_ethics_contractualist); ques_ethics_contractualist

ques_ethics_hedonism <- str_subset(string = colnames(dat), pattern = "^hedonism")
ques_ethics_hedonism <- sort(ques_ethics_hedonism); ques_ethics_hedonism

ques_ethics_utilitarian <- str_subset(string = colnames(dat), pattern = "^utilitarian")
ques_ethics_utilitarian <- sort(ques_ethics_utilitarian); ques_ethics_utilitarian

ques_ethics_deontology <- str_subset(string = colnames(dat), pattern = "^deontology")
ques_ethics_deontology <- sort(ques_ethics_deontology); ques_ethics_deontology

ques_ethics_virtue <- str_subset(string = colnames(dat), pattern = "^virtue")
ques_ethics_virtue <- sort(ques_ethics_virtue); ques_ethics_virtue

# conflict management scale
ques_conflictman <- str_subset(string = colnames(dat), pattern = "^conflictman")
ques_conflictman <- sort(ques_conflictman); ques_conflictman

# features of technology
ques_featurestechnologyfeelings <- str_subset(string = colnames(dat), pattern = "^featurestechnologyfeelings")
ques_featurestechnologyfeelings <- sort(ques_featurestechnologyfeelings); ques_featurestechnologyfeelings

# outcome questions
ques_outcome <- c("outcomerating_useful-01", "outcomerating_good-01",
                      str_subset(string = colnames(dat), pattern = "^outcomedummy"),
                  "ques_mostimpnode"); ques_outcome



# feedback questions Valence Software
ques_feedValenceGeneral <- c("feedCAM_repres", "feedCAM_technicalprobs", "feedCAM_technicalprobsText",
                      "feedCAM_stopdrawing", "feedCAM_stopdrawingText", "feedCAM_already", "feedCAM_alreadyText",
                      "feedback_critic"); ques_feedValenceGeneral

vec_vars <- c(ques_mixed,
              ques_demographic,
              ques_PANAS_p, ques_PANAS_n,
              ques_TAM,
              ques_ethics_relativist, ques_ethics_contractualist, ques_ethics_hedonism,
              ques_ethics_utilitarian, ques_ethics_deontology, ques_ethics_virtue,
              ques_conflictman,
              ques_featurestechnologyfeelings,
              ques_outcome,
              ques_feedValenceGeneral); length(vec_vars)


# vec_vars <- c("prolific_pid", "loginID", "loginPassword")


questionnaire <- data.frame(ID = unique(dat$ID)); nrow(questionnaire)

questionnairetype <- function(dataset, datasetques){
  for(c in 1:length(vec_vars)){
    if(any(colnames(dat) == vec_vars[c])){

    ## tmp IDs
    tmpid <- dataset$ID[!is.na(dataset[, vec_vars[c]])]
    ## tmp value variable
    tmpvalue <- dataset[, vec_vars[c]][!is.na(dataset[, vec_vars[c]])]
    datasetques[vec_vars[c]]  <- NA

    # prolific PID
    if(vec_vars[c] == "prolific_pid"){
    datasetques[vec_vars[c]] <- unique(tmpvalue)

    # if multiple answers are stored within JATOS
    }else if(sum(datasetques$ID %in% tmpid) !=  length(tmpvalue)){
      cat("multiple times answered: ", vec_vars[c], "by ID: \n")
      tmptab <- table(tmpid, tmpvalue)
      tmptab[table(tmpid, tmpvalue) == 0] <- NA
      tmptab[tmptab != 1 & !is.na(tmptab)] <- 1
      print(rownames(tmptab)[rowSums(tmptab, na.rm = TRUE) > 1])
      ## multiple idcodefinal in this dataset!
      if(vec_vars[c] != "idcodefinal"){
        for(j in 1:ncol(tmptab)){
          tmptab[,j] <-  as.numeric(colnames(tmptab))[j] * tmptab[,j]
        }
      tmpvalue <- round(x = rowMeans(tmptab, na.rm = TRUE), digits = 0)
      datasetques[datasetques$ID %in% tmpid, vec_vars[c]] <- as.numeric(tmpvalue)
      }else{
        for(k in 1:nrow(tmptab)){
          # print(names(tmptab[k,][tmptab[k,] == 1 & !is.na(tmptab[k,])]))
          datasetques[k, vec_vars[c]] <- paste0(names(tmptab[k,][tmptab[k,] == 1 & !is.na(tmptab[k,])]), collapse = ", ")
        }
      }
    }else{
      datasetques[datasetques$ID %in% tmpid, vec_vars[c]] <- tmpvalue
    }
    # print(c)
    }
  }
  return(datasetques)
}
## check
# c <- 12
# dataset = dat
# datasetques = questionnaire
# vec_vars[c]
# dataset[, vec_vars[c]][!is.na(dataset[, vec_vars[c]])]
# questionnaire[vec_vars[c]]

questionnaire <- questionnairetype(dataset = dat, datasetques = questionnaire)
questionnaire
sum(!is.na(questionnaire$loginID)); sum(!is.na(questionnaire$dummy_honestycontract)); nrow(questionnaire)


##### set variables to numeric
str(questionnaire)
questionnaire[, c("dummy_honestycontract", str_subset(string = colnames(questionnaire),
                             pattern = "^panas.|^tam|^relativist|^hedonism|^contractualist|^utilitarian|^deontology|^virtue|^conflictman|^featurestechnologyfeelings|^outcomerating|^outcomedummy"),
                  "feedCAM_repres", "feedCAM_technicalprobs", "feedCAM_stopdrawing", "feedCAM_already")] <-
  sapply(questionnaire[, c("dummy_honestycontract", str_subset(string = colnames(questionnaire),
                                      pattern = "^panas.|^tam|^relativist|^hedonism|^contractualist|^utilitarian|^deontology|^virtue|^conflictman|^featurestechnologyfeelings|^outcomerating|^outcomedummy"),
                           "feedCAM_repres", "feedCAM_technicalprobs", "feedCAM_stopdrawing", "feedCAM_already")],as.numeric)
str(questionnaire)

head(questionnaire)


### further variables
# paradefocus
questionnaire$paradefocus <- NA
for(i in 1:nrow(questionnaire)){
  if(sum(dat$paradefocus[dat$ID == i], na.rm = TRUE) > 0){
    questionnaire$paradefocus[i] <- sum(dat$paradefocus[dat$ID == i], na.rm = TRUE)
  }
}

# durationtotal
questionnaire$durationtotal <- NA
for(i in 1:nrow(questionnaire)){

  if(max(dat$time_end[dat$ID == i]) == dat$time_end[dat$ID == i][sum(dat$ID == i)]){
    questionnaire$durationtotal[i] <- round(x = max(dat$time_end[dat$ID == i]) / 1000 / 60, digits = 2)
  }

  # print(i)
  # print(unique(dat$prolific_pid[dat$ID == i])[!is.na(unique(dat$prolific_pid[dat$ID == i]))])
  # print(max(dat$time_end[dat$ID == i]) / 1000 / 60)
  # print(dat$time_end[dat$ID == i][sum(dat$ID == i)] / 1000 / 60)
  # print(dat_prolific$time_taken[dat_prolific$participant_id ==
  #                                 unique(dat$prolific_pid[dat$ID == i])[!is.na(unique(dat$prolific_pid[dat$ID == i]))]] / 60)
}

# sociodemo_studentdummy, sociodemo_employment
questionnaire$sociodemo_studentdummy <- NA
questionnaire$sociodemo_employment <- NA
for(i in 1:nrow(questionnaire)){
  questionnaire$sociodemo_studentdummy[i] <- dat_prolific$Student.Status[dat_prolific$participant_id %in% questionnaire$prolific_pid[i]]
  questionnaire$sociodemo_employment[i] <- dat_prolific$Employment.Status[dat_prolific$participant_id %in% questionnaire$prolific_pid[i]]
}


# questionnaire$prolific_pid <- NULL
# questionnaire$loginPassword <- NULL
write.xlsx(questionnaire, file = "questionnaire_tp1_2.xlsx", row.names = FALSE)

dim(questionnaire)
sum(is.na(questionnaire))
sort(colSums(is.na(questionnaire)))

write.xlsx(questionnaire[,c(1:6)], file = "report_data_tp1_2.xlsx", row.names = FALSE)



##########################################################################
##########################################################################
vec_vars <- c(c("06scenariotext", "10caminstruct1", "07quesTechnologicalImplant"),
              str_subset(string = unique(dat$sender), pattern = "^ethicscale_"),
              str_subset(string = unique(dat$sender), pattern = "^XXquesTAM_"),
              str_subset(string = unique(dat$sender), pattern = "^quesPANAS0"),
              str_subset(string = unique(dat$sender), pattern = "^XXquesConflictMan")); length(vec_vars)

questionnaire_para <- data.frame(ID = unique(dat$ID)); nrow(questionnaire_para)

# c <- 18
# dataset = dat
# datasetques = questionnaire_para
# rm(c); rm(dataset); rm(datasetques)

questionnairetype_para <- function(dataset, datasetques){
  for(c in 1:length(vec_vars)){
    # print(vec_vars[c])

    ##
    datasetques[paste0("duration_", vec_vars[c])]  <- NA

    ## tmp IDs
    tmpid <- dataset$ID[dataset$sender == vec_vars[c]]

    ### DURATION
    # tmpvalue_duration
    tmpvalue_duration <- round(x = dataset$duration[dataset$sender == vec_vars[c]] / 1000, digits = 2)


      # if multiple answers are stored within JATOS
      if(sum(datasetques$ID %in% tmpid) !=  length(tmpvalue_duration)){
        tmptab <- table(tmpid, tmpvalue_duration)
        tmptab[table(tmpid, tmpvalue_duration) == 0] <- NA

        if(sum(tmptab[table(tmpid, tmpvalue_duration) != 1] & !is.na(tmptab[table(tmpid, tmpvalue_duration) != 1])) != 0){
          stop("ERROR multiple duration times")
        }

        for(j in 1:ncol(tmptab)){
          tmptab[,j] <-  as.numeric(colnames(tmptab))[j] * tmptab[,j]
        }
        tmpvalue <- rowMeans(tmptab, na.rm = TRUE)
        datasetques[datasetques$ID %in% tmpid, paste0("duration_", vec_vars[c])] <- as.numeric(tmpvalue)
      }else{
        datasetques[datasetques$ID %in% tmpid, paste0("duration_", vec_vars[c])] <- as.numeric(tmpvalue_duration)
      }

    ### count clicks
    tmpvalue_paracountclicks <- dataset$paracountclicks[dataset$sender == vec_vars[c]]
    tmpvalue_paracountclicks

    if(all(!is.na(tmpvalue_paracountclicks))){
      datasetques[paste0("countclicks_", vec_vars[c])]  <- NA
    # if multiple answers are stored within JATOS
    if(sum(datasetques$ID %in% tmpid) !=  length(tmpvalue_paracountclicks)){
      tmptab <- table(tmpid, tmpvalue_paracountclicks)
      tmptab[table(tmpid, tmpvalue_paracountclicks) == 0] <- NA

      for(j in 1:ncol(tmptab)){
        tmptab[,j] <-  as.numeric(colnames(tmptab))[j] * tmptab[,j]
      }
      tmpvalue <- round(x = rowMeans(tmptab, na.rm = TRUE), digits = 0)
      datasetques[datasetques$ID %in% tmpid, paste0("countclicks_", vec_vars[c])] <- as.numeric(tmpvalue)
    }else{
      datasetques[datasetques$ID %in% tmpid, paste0("countclicks_", vec_vars[c])] <- as.numeric(tmpvalue_paracountclicks)
    # print("AAA 2")
      }
    }
  }
  return(datasetques)
}


questionnaire_para <- questionnairetype_para(dataset = dat, datasetques = questionnaire_para)
questionnaire_para

write.xlsx(questionnaire_para, file = "questionnaire_para_tp1_2.xlsx", row.names = FALSE)

a <- left_join(x = questionnaire, y = questionnaire_para)
dim(a); dim(questionnaire); dim(questionnaire_para)
colnames(a)


write.xlsx(a, file = "questionnaire_combined_tp1_2.xlsx", row.names = FALSE)
rm(a)






# questionnaire[, str_subset(string = colnames(questionnaire), pattern = "panas.+p$")]
#
# questionnaire[str_subset(string = colnames(questionnaire), pattern = "tam")] <-
#   sapply(questionnaire[str_subset(string = colnames(questionnaire), pattern = "tam")],as.numeric)
# questionnaire[str_subset(string = colnames(questionnaire), pattern = "panas")] <-
#   sapply(questionnaire[str_subset(string = colnames(questionnaire), pattern = "panas")],as.numeric)
#
#
# questionnaire$tam_bi_means <- rowMeans(x = questionnaire[, str_subset(string = colnames(questionnaire), pattern = "tam_bi")])
# questionnaire$panas_n_means <- rowMeans(x = questionnaire[, str_subset(string = colnames(questionnaire), pattern = "panas.+n")])
# questionnaire$panas_p_means <- rowMeans(x = questionnaire[, str_subset(string = colnames(questionnaire), pattern = "panas.+p")])
#
# boxplot(questionnaire$tam_bi_means ~ questionnaire$idcodegroup)
# boxplot(questionnaire$panas_n_means ~ questionnaire$idcodegroup)
# boxplot(questionnaire$panas_p_means ~ questionnaire$idcodegroup)
# cor(questionnaire$panas_n_means, questionnaire$tam_bi_means)
