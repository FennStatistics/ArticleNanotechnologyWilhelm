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


dat <- read.xlsx(file = "questionnaire_combined_tp2.xlsx", sheetIndex = 1)

dat_select <- read.xlsx(file = "aa_report_data_finalselected_groups.xlsx", sheetIndex = 1)

colnames(dat); dim(dat)

# ?rbind.fill
# dat_merged <- rbind.fill(dat1, dat2)
# colnames(dat_merged); dim(dat_merged)
# head(dat_merged)


### save all
write.table(x = dat, file = "questionnaire_all_t2.txt")
write.xlsx(dat, file = "questionnaire_all_t2.xlsx", row.names = FALSE)
haven::write_sav(data = dat, path = "questionnaire_all_t2.sav")



#############
# different select mechanism -> check CAM data again
#############

sum(!is.na(dat_select$prolific_pid_selected))
dat_sel <- dat[dat$prolific_pid %in% dat_select$prolific_pid_selected, ]
# sort(dat_merged_sel$prolific_pid) == sort(dat_select$prolific_pid)


### save subset
write.table(x = dat_sel, file = "questionnaire_final_t2.txt")
write.xlsx(dat_sel, file = "questionnaire_final_t2.xlsx", row.names = FALSE)
haven::write_sav(data = dat_sel, path = "questionnaire_final_t2.sav")





#############
# check CAM data
#############
global_dir_data = "C:/DATEN/PHD/MAIN STUDY 1 - CAMs, quest/Analysis/MZP2/CAM data"


setwd(global_dir_data)

files_dirs <- list.files(path = global_dir_data, full.names = FALSE)
files_dirs_links <- str_subset(string = files_dirs, pattern = "_links")
files_dirs_links; length(files_dirs_links); length(unique(files_dirs_links))
files_dirs_blocks <- str_subset(string = files_dirs, pattern = "_blocks")
files_dirs_blocks; length(files_dirs_blocks); length(unique(files_dirs_blocks))

all(unlist(str_split(string = files_dirs_links, pattern = "_", simplify = TRUE))[,1] ==
      unlist(str_split(string = files_dirs_blocks, pattern = "_", simplify = TRUE))[,1])

setwd("media")
length(dir()); length(files_dirs_links)
a <- unlist(str_split(string = str_subset(string = dir(), pattern = ".png$"), pattern = "_", simplify = TRUE))[,1]
b <- unlist(str_split(string = files_dirs_links, pattern = "_", simplify = TRUE))[,1]

b[!b %in% a]
all(a == b)


#############
# compare CAM and dat_sel
#############
dat_sel$prolific_pid

b[!b %in% dat_sel$loginID]
dat_sel$loginID[!dat_sel$loginID %in% b]


all(sort(dat_sel$loginID) == sort(b))
