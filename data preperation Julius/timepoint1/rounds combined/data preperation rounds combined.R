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


dat1 <- read.xlsx(file = "questionnaire_combined_tp1_1.xlsx", sheetIndex = 1)
dat2 <- read.xlsx(file = "questionnaire_combined_tp1_2.xlsx", sheetIndex = 1)

dat_select <- read.xlsx(file = "aa_report_data_finalselected_groups.xlsx", sheetIndex = 1)



colnames(dat1); dim(dat1)
colnames(dat2); dim(dat2)

?rbind.fill
dat_merged <- rbind.fill(dat1, dat2)

colnames(dat_merged); dim(dat_merged)

head(dat_merged)
dat_merged$sociodemo_impexpwho

### save all
write.table(x = dat_merged, file = "questionnaire_all_t1.txt")
write.xlsx(dat_merged, file = "questionnaire_all_t1.xlsx", row.names = FALSE)
haven::write_sav(data = dat_merged, path = "questionnaire_all_t1.sav")


dat_merged_sel <- dat_merged[dat_merged$prolific_pid %in% dat_select$prolific_pid, ]
sort(dat_merged_sel$prolific_pid) == sort(dat_select$prolific_pid)


### save subset
write.table(x = dat_merged_sel, file = "questionnaire_final_t1.txt")
write.xlsx(dat_merged_sel, file = "questionnaire_final_t1.xlsx", row.names = FALSE)
haven::write_sav(data = dat_merged_sel, path = "questionnaire_final_t1.sav")

