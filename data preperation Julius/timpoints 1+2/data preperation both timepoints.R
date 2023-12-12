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
       'performance', 'lattice', 'stargazer', 'psych', 'plyr')


### Daten
# Read the text file from JATOS ...
dir()


dat_ques_t1 <- read.xlsx(file = "questionnaire_final_t1.xlsx", sheetIndex = 1)
dat_ques_t2 <- read.xlsx(file = "questionnaire_final_t2.xlsx", sheetIndex = 1)

dat_network_t1 <- read.xlsx(file = "networkindicatorsCAMs_t1.xlsx", sheetIndex = 1)
dat_network_t2 <- read.xlsx(file = "networkindicatorsCAMs_t2.xlsx", sheetIndex = 1)



##################
# long format data
##################
##### questionnaire
### time point
dat_ques_t1$timepoint <- 0
dat_ques_t2$timepoint <- 1

### idcodegroup for time point 1
dat_ques_t1$idcodegroup <- NA
for(i in 1:nrow(dat_ques_t1)){
  if(length(dat_ques_t2$idcodegroup[dat_ques_t2$prolific_pid %in% dat_ques_t1$prolific_pid[i]]) == 1){
    dat_ques_t1$idcodegroup[i] <- dat_ques_t2$idcodegroup[dat_ques_t2$prolific_pid %in% dat_ques_t1$prolific_pid[i]]
  }
}
table(dat_ques_t1$idcodegroup); table(dat_ques_t2$idcodegroup)
# dat_ques_t1[, c("prolific_pid", "idcodegroup")]


dat_ques_long <- rbind.fill(dat_ques_t1, dat_ques_t2)


##### network
dat_network_long <- rbind.fill(dat_network_t1, dat_network_t2)

dat_ques_long$loginID
dat_network_long$subject <- str_extract(string = dat_network_long$participantCAM, pattern = "[[:alpha:]]*[[:digit:]]*")
all(dat_ques_long$loginID %in% dat_network_long$subject)

dat_ques_long$loginID[!dat_ques_long$loginID %in% dat_network_long$subject]
dat_ques_long$subject[!dat_ques_long$subject %in% dat_ques_long$loginID]


##### merge questionnaire + network
dat_long_merged <- left_join(x = dat_ques_long, y = dat_network_long, by = c("loginID"="subject"))

# > save
write.table(x = dat_long_merged, file = "dat_long_merged_final.txt")
write.xlsx(dat_long_merged, file = "dat_long_merged_final.xlsx", row.names = FALSE)
haven::write_sav(data = dat_long_merged, path = "dat_long_merged_final.sav")

write.csv(x = dat_long_merged, file = "dat_long_merged_final.csv")

##################
# wide format data
##################

colnames(dat_ques_t1) <- paste0(colnames(dat_ques_t1), "_t0")
colnames(dat_ques_t2) <- paste0(colnames(dat_ques_t2), "_t1")

dat_ques_wide <- left_join(x = dat_ques_t1, y = dat_ques_t2, by = c("prolific_pid_t0"="prolific_pid_t1"))

dat_network_t1$prolific_pid <- NA
tmp <- str_extract(string = dat_network_t1$participantCAM, pattern = "[[:alpha:]]*[[:digit:]]*")
for(i in 1:nrow(dat_network_t1)){
  dat_network_t1$prolific_pid[i] <- dat_ques_t1$prolific_pid_t0[dat_ques_t1$loginID_t0 == tmp[i]]
}

dat_network_t2$prolific_pid <- NA
tmp <- str_extract(string = dat_network_t2$participantCAM, pattern = "[[:alpha:]]*[[:digit:]]*")
for(i in 1:nrow(dat_network_t2)){
  dat_network_t2$prolific_pid[i] <- dat_ques_t2$prolific_pid_t1[dat_ques_t2$loginID_t1 == tmp[i]]
}


colnames(dat_network_t1) <- paste0(colnames(dat_network_t1), "_t0")
colnames(dat_network_t2) <- paste0(colnames(dat_network_t2), "_t1")


dat_network_wide <- left_join(x = dat_network_t1, y = dat_network_t2, by = c("prolific_pid_t0"="prolific_pid_t1"))
dat_network_wide$mean_valence_macro_t1



##### merge questionnaire + network
dat_wide_merged <- left_join(x = dat_ques_wide, y = dat_network_wide, by = c("prolific_pid_t0"="prolific_pid_t0"))


# > save
write.table(x = dat_wide_merged, file = "dat_wide_merged_final.txt")
write.xlsx(dat_wide_merged, file = "dat_wide_merged_final.xlsx", row.names = FALSE)
haven::write_sav(data = dat_wide_merged, path = "dat_wide_merged_final.sav")

write.csv(x = dat_wide_merged, file = "dat_wide_merged_final.csv")

# plot(dat_network_wide$num_nodes_macro_t0, dat_network_wide$num_nodes_macro_t1)
# plot(dat_wide_merged$num_nodes_macro_t0, dat_wide_merged$num_nodes_macro_t1)


#############################
#############################
#############################
#############################
tmp <- dat_long_merged %>%
  dplyr::select(matches(match = "^tam_bi.*")) %>%
  rowwise() %>%
  dplyr::transmute(mean = mean(c_across()), sd = sd(c_across()))

dat_long_merged$mean_tam_bi <- tmp$mean
dat_long_merged$sd_tam_bi <- tmp$sd

tmp <- dat_long_merged %>%
  dplyr::select(matches(match = "^panas.*n$")) %>%
  rowwise() %>%
  dplyr::transmute(mean = mean(c_across()), sd = sd(c_across()))

dat_long_merged$mean_panas_neg <- tmp$mean
dat_long_merged$sd_panas_neg <- tmp$sd

tmp <- dat_long_merged %>%
  dplyr::select(matches(match = "^panas.*p$")) %>%
  rowwise() %>%
  dplyr::transmute(mean = mean(c_across()), sd = sd(c_across()))

dat_long_merged$mean_panas_pos <- tmp$mean
dat_long_merged$sd_panas_pos <- tmp$sd


# plot(dat_long_merged$mean_tam_bi, dat_long_merged$mean_panas_neg)
# cor(dat_long_merged$mean_tam_bi, dat_long_merged$mean_panas_neg)




tmp <- dat_long_merged %>%
  rowwise() %>%
  dplyr::transmute(relativist_mean = mean(c_across(cols = matches(match = "^relativist.*"))),
         contractualist_mean = mean(c_across(cols = matches(match = "^contractualist.*"))),
         hedonism_mean = mean(c_across(cols = matches(match = "^hedonism.*"))),
         utilitarian_mean = mean(c_across(cols = matches(match = "^utilitarian.*"))),
         deontology_mean = mean(c_across(cols = matches(match = "^deontology.*"))),
         virtue_mean = mean(c_across(cols = matches(match = "^virtue.*"))),
         ethic_mean = mean(c_across(cols = matches(match = "^relativist.*|^contractualist.*|^hedonism.*|^utilitarian.*|^deontology.*|^virtue.*"))))


# dat_long_merged$mean_relativist <- tmp$relativist_mean
# dat_long_merged$mean_contractualist <- tmp$contractualist_mean
# dat_long_merged$mean_relativist <- tmp$hedonism_mean
# dat_long_merged$mean_relativist <- tmp$utilitarian_mean
# dat_long_merged$mean_relativist <- tmp$deontology_mean
# dat_long_merged$mean_relativist <- tmp$virtue_mean
dat_long_merged$mean_ethic <- tmp$ethic_mean



# #########################
# library(MplusAutomation)
#
# tmp <- dat_ques_t1 %>%
#   select("ID_t0", matches(match = "relativist|contractualist|hedonism|utilitarian|deontology|virtue"))
# colnames(tmp)
# prepareMplusData(df = tmp, filename = "cfa_t1.dat")
# dir()
