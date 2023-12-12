###preparation
#load packages
library(broom)
library(dplyr)
library(ggpubr)
library(psych)
library(readxl)
library(rstatix)
library(tidyverse)
library(tableHTML)

#set file path
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

rm(list=ls(all=TRUE))
graphics.off()

#load data
dat_long_merged_final <- 
  read_excel("dat_long_merged_final.xlsx")

#remove participants with missing data
dat_nodropout <- 
  dat_long_merged_final[!is.na(dat_long_merged_final$idcodegroup),]

#downgrade to 2x2
CAMediaid <- 
  dat_nodropout %>% 
  mutate_at("idcodegroup", str_replace, "positive", "experimental") %>%
  mutate_at("idcodegroup", str_replace, "negative", "experimental")
view(CAMediaid)












###pre-post differences cam
##eg
#filter and arrange
cam_pre_eg_raw <- CAMediaid %>%
  select(prolific_pid, timepoint, idcodegroup, mean_valence_macro) %>% 
  filter(timepoint == 0) %>%
  filter(str_detect(idcodegroup, "experimental"))
cam_pre_eg <- cam_pre_eg_raw[ order( cam_pre_eg_raw$prolific_pid), ]
#view(cam_pre_eg)

cam_post_eg_raw <- CAMediaid %>%
  select(prolific_pid, timepoint, idcodegroup, mean_valence_macro) %>% 
  filter(timepoint == 1) %>%
  filter(str_detect(idcodegroup, "experimental"))
cam_post_eg <- cam_post_eg_raw[ order( cam_post_eg_raw$prolific_pid), ]
#view(cam_post_eg)

#compute pre-post differences
cam_pre_eg$cam_diff_eg <- (cam_pre_eg$mean_valence_macro - cam_post_eg$mean_valence_macro)

#remove negative signs
cam_pre_eg$cam_diff_eg_abs <- abs(cam_pre_eg$cam_diff_eg)
#view(cam_diff_eg_abs)

#put together
cam_diff_eg <- cbind(pre=cam_pre_eg$mean_valence_macro, diff=cam_pre_eg$cam_diff_eg_abs, group=cam_pre_eg$idcodegroup)
#view(cam_diff_eg)






##cg
#filter and arrange
cam_pre_cg_raw <- CAMediaid %>%
  select(prolific_pid, timepoint, idcodegroup, mean_valence_macro) %>% 
  filter(timepoint == 0) %>%
  filter(str_detect(idcodegroup, "control"))
cam_pre_cg <- cam_pre_cg_raw[ order( cam_pre_cg_raw$prolific_pid), ]
#view(cam_pre_cg)

cam_post_cg_raw <- CAMediaid %>%
  select(prolific_pid, timepoint, idcodegroup, mean_valence_macro) %>% 
  filter(timepoint == 1) %>%
  filter(str_detect(idcodegroup, "control"))
cam_post_cg <- cam_post_cg_raw[ order( cam_post_cg_raw$prolific_pid), ]
#view(cam_post_cg)

#compute pre-post differences
cam_pre_cg$cam_diff_cg <- (cam_pre_cg$mean_valence_macro - cam_post_cg$mean_valence_macro)

#remove negative signs
cam_pre_cg$cam_diff_cg_abs <- abs(cam_pre_cg$cam_diff_cg)
#view(cam_diff_cg_abs)

#put together
cam_diff_cg <- cbind(pre=cam_pre_cg$mean_valence_macro, diff=cam_pre_cg$cam_diff_cg_abs, group=cam_pre_cg$idcodegroup)
#view(cam_diff_cg)






#put all together
cam_diff <- rbind(cam_diff_cg, cam_diff_eg)
view(cam_diff)

#adjust variable type
cam_diff <- as.data.frame(cam_diff) 
cam_diff$pre <- as.numeric(cam_diff$pre)
cam_diff$diff <- as.numeric(cam_diff$diff)






##check assumptions
#linearity assumption
ggscatter(
  cam_diff, x = "pre", y = "diff",
  color = "group", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = group)
  )

#homogeneity of regression slopes
cam_diff %>% anova_test(diff ~ group*pre)

#normality of residuals
cam_model <- lm(diff ~ pre + group, data = cam_diff)

cam_model_aug <- augment(cam_model) %>%
  select(-.hat, -.sigma, -.fitted)
head(cam_model_aug, 3)

shapiro_test(cam_model_aug$.resid)

#homogeneity of variances
cam_model_aug %>% levene_test(.resid ~ group)

#outliers
cam_model_aug %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()






##ancova cam
ancova_cam <- cam_diff %>% anova_test(diff ~ pre + group)
ancova_cam
view(ancova_cam)












###prepare tam3
#invert negative correlated items
CAMediaid$tam_canx.02 <- 8 - CAMediaid$tam_canx.02
CAMediaid$tam_canx.03 <- 8 - CAMediaid$tam_canx.03
CAMediaid$tam_canx.04 <- 8 - CAMediaid$tam_canx.04

CAMediaid$tam_pec.04 <- 8 - CAMediaid$tam_pec.04
CAMediaid$tam_res.04 <- 8 - CAMediaid$tam_res.04

CAMediaid$tam_vol.01 <- 8 - CAMediaid$tam_vol.01
CAMediaid$tam_vol.02 <- 8 - CAMediaid$tam_vol.02
CAMediaid$tam_vol.03 <- 8 - CAMediaid$tam_vol.03

#delete low correlated items
CAMediaid$tam_pec.04 <- NULL

CAMediaid$tam_vol.01 <- NULL
CAMediaid$tam_vol.02 <- NULL
CAMediaid$tam_vol.03 <- NULL

CAMediaid$tam_res.01 <- NULL
CAMediaid$tam_res.02 <- NULL
CAMediaid$tam_res.03 <- NULL
CAMediaid$tam_res.04 <- NULL

CAMediaid$tam_img.01 <- NULL
CAMediaid$tam_img.02 <- NULL
CAMediaid$tam_img.03 <- NULL

#correlation plot
psych::cor.plot(r = cor(CAMediaid[, str_detect(string = colnames(CAMediaid), pattern = "^tam_.+")]
                        , use = "pairwise.complete.obs"),
                upper = FALSE, xlas = 2)

#cronbach's alpha
alpha(subset(CAMediaid, select = c(str_detect(string = colnames(CAMediaid), pattern = "^tam_.+"))))

#compute tam mean
CAMediaid$tam_mean <- rowMeans(CAMediaid[, str_detect(string = colnames(CAMediaid), pattern = "^tam_.+")])
view(CAMediaid$tam_mean)












###pre-post differences tam
##eg
#filter and arrange
tam_pre_eg_raw <- CAMediaid %>%
  select(prolific_pid, timepoint, idcodegroup, tam_mean) %>% 
  filter(timepoint == 0) %>%
  filter(str_detect(idcodegroup, "experimental"))
tam_pre_eg <- tam_pre_eg_raw[ order( tam_pre_eg_raw$prolific_pid), ]
#view(tam_pre_eg)

tam_post_eg_raw <- CAMediaid %>%
  select(prolific_pid, timepoint, idcodegroup, tam_mean) %>% 
  filter(timepoint == 1) %>%
  filter(str_detect(idcodegroup, "experimental"))
tam_post_eg <- tam_post_eg_raw[ order( tam_post_eg_raw$prolific_pid), ]
#view(tam_post_eg)

#compute pre-post differences
tam_diff_eg <- (tam_pre_eg$tam_mean - tam_post_eg$tam_mean)
#view(tam_diff_eg)

#remove negative signs
tam_pre_eg$tam_diff_eg_abs <- abs(tam_diff_eg)
#view(tam_diff_eg_abs)

#put together
tam_diff_eg <- cbind(pre=tam_pre_eg$tam_mean, diff=tam_pre_eg$tam_diff_eg_abs, group=tam_pre_eg$idcodegroup)
#view(tam_diff_eg)






##cg
#filter and arrange
tam_pre_cg_raw <- CAMediaid %>%
  select(prolific_pid, timepoint, idcodegroup, tam_mean) %>% 
  filter(timepoint == 0) %>%
  filter(str_detect(idcodegroup, "control"))
tam_pre_cg <- tam_pre_cg_raw[ order( tam_pre_cg_raw$prolific_pid), ]
#view(tam_pre_cg)

tam_post_cg_raw <- CAMediaid %>%
  select(prolific_pid, timepoint, idcodegroup, tam_mean) %>% 
  filter(timepoint == 1) %>%
  filter(str_detect(idcodegroup, "control"))
tam_post_cg <- tam_post_cg_raw[ order( tam_post_cg_raw$prolific_pid), ]
#view(tam_post_cg)

#compute pre-post differences
tam_diff_cg <- (tam_pre_cg$tam_mean - tam_post_cg$tam_mean)
#view(tam_diff_cg)

#remove negative signs
tam_pre_cg$tam_diff_cg_abs <- abs(tam_diff_cg)
#view(tam_diff_cg_abs)

#put together
tam_diff_cg <- cbind(pre=tam_pre_cg$tam_mean, diff=tam_pre_cg$tam_diff_cg_abs, group=tam_pre_cg$idcodegroup)
#view(tam_diff_cg)






#put all together
tam_diff <- rbind(tam_diff_cg, tam_diff_eg)
view(tam_diff)

#adjust variable type
tam_diff <- as.data.frame(tam_diff) 
tam_diff$pre <- as.numeric(tam_diff$pre)
tam_diff$diff <- as.numeric(tam_diff$diff)






##check assumptions
#linearity assumption
ggscatter(
  tam_diff, x = "pre", y = "diff",
  color = "group", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = group)
  )

#homogeneity of regression slopes
tam_diff %>% anova_test(diff ~ group*pre)

#normality of residuals
tam_model <- lm(diff ~ pre + group, data = tam_diff)

tam_model_aug <- augment(tam_model) %>%
  select(-.hat, -.sigma, -.fitted)
head(tam_model_aug, 3)

shapiro_test(tam_model_aug$.resid)

#homogeneity of variances
tam_model_aug %>% levene_test(.resid ~ group)

#outliers
tam_model_aug %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()






##ancova tam
ancova_tam <- tam_diff %>% anova_test(diff ~ pre + group)
ancova_tam
view(ancova_tam)












###descriptive statistics
##sample
CAMediaid$sociodemo_age <- as.numeric(CAMediaid$sociodemo_age)
ds_age <- psych::describeBy(CAMediaid$sociodemo_age, group = CAMediaid$timepoint, mat=TRUE, digits = 2)
htmlTable::htmlTable(format(ds_age))

CAMediaid %>%
  group_by(sociodemo_gender) %>%
  summarise(frequency=n())

CAMediaid %>%
  group_by(sociodemo_country) %>%
  summarise(frequency=n())

CAMediaid %>%
  group_by(sociodemo_faith) %>%
  summarise(frequency=n())

CAMediaid %>%
  group_by(sociodemo_impexp) %>%
  summarise(frequency=n())

CAMediaid %>%
  group_by(dummy_honestycontract) %>%
  summarise(frequency=n())

CAMediaid %>%
  group_by(dummy_informedconsent) %>%
  summarise(frequency=n())






##cam
#compute descriptive statistics
ds_cam <- psych::describeBy(CAMediaid$mean_valence_macro, list(CAMediaid$idcodegroup, CAMediaid$timepoint), mat=TRUE, digits = 2)
ds_cam_diff <- psych::describeBy(cam_diff$diff, group = cam_diff$group, mat=TRUE, digits = 2)

#print results
htmlTable::htmlTable(format(ds_cam)) 
htmlTable::htmlTable(format(ds_cam_diff)) 
#view(ds_cam)
write_tableHTML(tableHTML(ds_cam), file = 'ds_cam.html')
write_tableHTML(tableHTML(ds_cam_diff), file = 'ds_cam_diff.html')





##tam
#compute descriptive statistics
ds_tam <- psych::describeBy(CAMediaid$tam_mean, list(CAMediaid$idcodegroup, CAMediaid$timepoint), mat=TRUE, digits = 2)
ds_tam_diff <- psych::describeBy(tam_diff$diff, group = tam_diff$group, mat=TRUE, digits = 2)

#print results
htmlTable::htmlTable(format(ds_tam)) 
htmlTable::htmlTable(format(ds_tam_diff)) 
#view(ds_tam)
write_tableHTML(tableHTML(ds_tam), file = 'ds_tam.html')
write_tableHTML(tableHTML(ds_tam_diff), file = 'ds_tam_diff.html')






##exploratory analyses
#number of nodes
ds_n <- psych::describeBy(CAMediaid$num_nodes_macro, list(CAMediaid$idcodegroup, CAMediaid$timepoint), mat=TRUE, digits = 2)
htmlTable::htmlTable(format(ds_n)) 
write_tableHTML(tableHTML(ds_n), file = 'ds_n.html')

ds_n_pos <- psych::describeBy(CAMediaid$num_nodes_pos_macro, list(CAMediaid$idcodegroup, CAMediaid$timepoint), mat=TRUE, digits = 2)
htmlTable::htmlTable(format(ds_n_pos)) 
write_tableHTML(tableHTML(ds_n_pos), file = 'ds_n_pos.html')

ds_n_neg <- psych::describeBy(CAMediaid$num_nodes_neg_macro, list(CAMediaid$idcodegroup, CAMediaid$timepoint), mat=TRUE, digits = 2)
htmlTable::htmlTable(format(ds_n_neg)) 
write_tableHTML(tableHTML(ds_n_neg), file = 'ds_n_neg.html')

ds_n_neut <- psych::describeBy(CAMediaid$num_nodes_neut_macro, list(CAMediaid$idcodegroup, CAMediaid$timepoint), mat=TRUE, digits = 2)
htmlTable::htmlTable(format(ds_n_neut)) 
write_tableHTML(tableHTML(ds_n_neut), file = 'ds_n_neut.html')

ds_n_amb <- psych::describeBy(CAMediaid$num_nodes_ambi_macro, list(CAMediaid$idcodegroup, CAMediaid$timepoint), mat=TRUE, digits = 2)
htmlTable::htmlTable(format(ds_n_amb)) 
write_tableHTML(tableHTML(ds_n_amb), file = 'ds_n_amb.html')






#politics
p_moral <- psych::describeBy(CAMediaid$outcomedummy_moral, list(CAMediaid$idcodegroup, CAMediaid$timepoint), mat=TRUE, digits = 2)
htmlTable::htmlTable(format(p_moral)) 
write_tableHTML(tableHTML(p_moral), file = 'p_moral.html')

with(CAMediaid, table(idcodegroup, timepoint, outcomedummy_moral)) 



p_insurance <- psych::describeBy(CAMediaid$outcomedummy_insurance, list(CAMediaid$idcodegroup, CAMediaid$timepoint), mat=TRUE, digits = 2)
htmlTable::htmlTable(format(p_insurance)) 
write_tableHTML(tableHTML(p_insurance), file = 'p_insurance.html')

with(CAMediaid, table(idcodegroup, timepoint, outcomedummy_insurance)) 



p_fund <- psych::describeBy(CAMediaid$outcomedummy_funds, list(CAMediaid$idcodegroup, CAMediaid$timepoint), mat=TRUE, digits = 2)
htmlTable::htmlTable(format(p_fund)) 
write_tableHTML(tableHTML(p_fund), file = 'p_fund.html')

with(CAMediaid, table(idcodegroup, timepoint, outcomedummy_funds)) 



p_prohibited <- psych::describeBy(CAMediaid$outcomedummy_prohibited, list(CAMediaid$idcodegroup, CAMediaid$timepoint), mat=TRUE, digits = 2)
htmlTable::htmlTable(format(p_prohibited)) 
write_tableHTML(tableHTML(p_prohibited), file = 'p_prohibited.html')

with(CAMediaid, table(idcodegroup, timepoint, outcomedummy_prohibited)) 












devtools::session_info()