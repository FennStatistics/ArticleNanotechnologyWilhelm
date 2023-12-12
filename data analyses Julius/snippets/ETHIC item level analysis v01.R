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
       'performance', 'lattice', 'stargazer', 'psych', 'ggplot2')

library(lavaan)
library(semPlot)


### Daten
# Read the text file from JATOS ...
dir()
dat_wide_t1 <- read.xlsx(file = "questionnaire_final_t1.xlsx", sheetIndex = 1)
colnames(dat_wide_t1) <- paste0(colnames(dat_wide_t1), "_t0")

dat_wide <- read.xlsx(file = "dat_wide_merged_final.xlsx", sheetIndex = 1)
dat_long <- read.xlsx(file = "dat_long_merged_final.xlsx", sheetIndex = 1)


######################
tmp <- names(table(dat_long$prolific_pid))[table(dat_long$prolific_pid) >= 2]
dat_long <- dat_long[dat_long$prolific_pid %in% tmp, ]


dat_long %>%
  group_by(timepoint) %>%
  summarise(mean(num_nodes_macro))


t.test(num_nodes_macro ~ timepoint, dat_long, paired=TRUE)
#######################



dat_long <- dat_long[dat_long$timepoint == 0, ]


sum(str_detect(string = colnames(dat_wide), pattern = "panas"))
sum(str_detect(string = colnames(dat_wide), pattern = "conflictman"))
sum(str_detect(string = colnames(dat_wide), pattern = "outcomerating|outcomedummy"))
sum(str_detect(string = colnames(dat_wide), pattern = "sociodemo"))
str_subset(string = colnames(dat_wide), pattern = "sociodemo")
##################
# reliablity analysis ()
##################
### Korrelation, Reliabilität (Cronbachs), EFA
corr_rel_EFA <- function(constructlist = NULL, constnum = NULL,
                         data = NULL,
                         nfacs = 1){
  ### correlation measures:
  # spearman
  cor_mat <- cor(data[,constructlist[[constnum]]],
                 use = "pairwise.complete.obs",
                 method = "spearman")

  ### reliability measures:
  # Cronbachs
  rel_cronbach <- psych::alpha(cor_mat)

  ### EFA (PAF):
  fit_efa <- fa(r = data[,constructlist[[constnum]]], nfactors = nfacs,
                rotate = "Promax", fm = "pa", max.iter = 500)


  ### return objects as list
  return_list <- list(round(x = cor_mat, digits = 2),
                      rel_cronbach,
                      fit_efa
  )
  names(return_list) <- c("Cor: Spearman",
                          "Reliability: Cronbach",
                          "fit EFA (PAF)")

  # > print
  cat("mean inter-item-correlation (Spearman):",
      round(x = mean(colMeans(x = cor_mat)), digits = 2), "\n\n")

  cat("Cronbachs Alpha:",
      round(x = rel_cronbach$total[[1]], digits = 2), "\n\n")

  cat("EFA (PAF) variance accounted first factor:",
      round(x = fit_efa$Vaccounted[2], digits = 2), "for", nfacs, "factors", "\n")
  tmpKMO <- psych::KMO(cor_mat)
  if(any(tmpKMO$MSAi < .6)){
    cat("KMO criteria is to low (< .6) for:", "\n",
        names(tmpKMO$MSAi[tmpKMO$MSAi < .6]), "\n",
        "mean KMO:", round(x = tmpKMO$MSA, digits = 2), "\n")
  }
  #

  return(return_list)
}


constructs_list <- list()

(vars_tmp <- colnames(dat_wide)[str_detect(string = colnames(dat_wide), "relativist.+_t0$")])
constructs_list[[1]] <- vars_tmp
(vars_tmp <- colnames(dat_wide)[str_detect(string = colnames(dat_wide), "relativist.+_t1$")])
constructs_list[[2]] <- vars_tmp

## relativist t1
result_tmp <- corr_rel_EFA(constructlist = constructs_list,
                           constnum = 1, data = dat_wide, nfacs = 1)
round(x = result_tmp$`Reliability: Cronbach`$alpha.drop, digits = 2)
print(result_tmp$`fit EFA (PAF)`$loadings, cutoff = .4)
## relativist t2
result_tmp <- corr_rel_EFA(constructlist = constructs_list,
                           constnum = 2, data = dat_wide, nfacs = 1)
round(x = result_tmp$`Reliability: Cronbach`$alpha.drop, digits = 2)
print(result_tmp$`fit EFA (PAF)`$loadings, cutoff = .4)




(vars_tmp <- colnames(dat_wide)[str_detect(string = colnames(dat_wide), "contractualist.+_t0$")])
constructs_list[[2]] <- vars_tmp

(vars_tmp <- colnames(dat_wide)[str_detect(string = colnames(dat_wide), "utilitarian.+_t0$")])
constructs_list[[3]] <- vars_tmp

(vars_tmp <- colnames(dat_wide)[str_detect(string = colnames(dat_wide), "deontology.+_t0$")])
constructs_list[[4]] <- vars_tmp

(vars_tmp <- colnames(dat_wide)[str_detect(string = colnames(dat_wide), "virtue.+_t0$")])
constructs_list[[5]] <- vars_tmp

(vars_tmp <- colnames(dat_wide)[str_detect(string = colnames(dat_wide), "hedonism.+_t0$")])
constructs_list[[6]] <- vars_tmp

# > all
constructs_list[[7]] <- unlist(constructs_list)



## all t1
result_tmp <- corr_rel_EFA(constructlist = constructs_list,
                           constnum = 7, data = dat_wide_t1, nfacs = 1) # change nfacs
round(x = result_tmp$`Reliability: Cronbach`$alpha.drop, digits = 2)
print(result_tmp$`fit EFA (PAF)`$loadings, cutoff = .4)




### PANAS
# P
(vars_tmp <- colnames(dat_wide)[str_detect(string = colnames(dat_wide), "panas.+p_t0$")])
constructs_list[[8]] <- vars_tmp
# N
(vars_tmp <- colnames(dat_wide)[str_detect(string = colnames(dat_wide), "panas.+n_t0$")])
constructs_list[[9]] <- vars_tmp

### PANAS
# bi
(vars_tmp <- colnames(dat_wide)[str_detect(string = colnames(dat_wide), "tam_bi.+_t0$")])
constructs_list[[10]] <- vars_tmp
# canx
(vars_tmp <- colnames(dat_wide)[str_detect(string = colnames(dat_wide), "tam_canx.+_t0$")])
constructs_list[[11]] <- vars_tmp

##################
# correlation plots
##################
dev.off()
psych::cor.plot(r = cor(dat_wide[, str_detect(string = colnames(dat_wide), pattern = "relativist.+_t0$|contractualist.+_t0$|hedonism.+_t0$|utilitarian.+_t0$|deontology.+_t0$|virtue.+_t0$")]
                        , use = "pairwise.complete.obs"),
                upper = FALSE, xlas = 2)

dev.off()
psych::cor.plot(r = cor(dat_wide[, str_detect(string = colnames(dat_wide), pattern = "relativist.+_t0$")]
                        , use = "pairwise.complete.obs"),
                upper = FALSE, xlas = 2)
dev.off()
psych::cor.plot(r = cor(dat_wide[, str_detect(string = colnames(dat_wide), pattern = "tam.+_t0$")]
                        , use = "pairwise.complete.obs"),
                upper = FALSE, xlas = 2)
##################
# mixed ANOVAs
##################


dat_long_ethicscale <- dat_long %>%
  select(matches(match = "hedonism|relativist|contractualist|utilitarian|deontology|virtue")) %>%
  mutate(mean_all = rowMeans(.), SD_all = apply(.,1,sd))

dat_long_ethicscale$zscore_all <- scale(x = dat_long_ethicscale$mean_all, center = TRUE, scale = TRUE)

dat_long_ethicscale$timepoint <- dat_long$timepoint
dat_long_ethicscale$prolific_pid <- dat_long$prolific_pid
dat_long_ethicscale$idcodegroup <- dat_long$idcodegroup

hist(dat_long_ethicscale$SD_all)
plot(dat_long_ethicscale$mean_all, dat_long_ethicscale$SD_all)


###
# > mixed anovas
###
fit1 <- afex::aov_car(mean_all ~ timepoint*idcodegroup + Error(prolific_pid / timepoint),
                      data = dat_long_ethicscale)

fit1a <- afex::aov_ez(id = "prolific_pid", dv = "mean_all",
                      data = dat_long_ethicscale, between=c("idcodegroup"), within=c("timepoint"))
# partical eta squared
anova(fit1, es = "pes")
# generalized eta squared
fit1a


#######################
###
# > check assumptions
###
### Shapiro-Wilk test of normality
# dat_long %>%
#   group_by(timepoint, idcodegroup) %>%
#   shapiro_test(mean_valence_macro)
#
# ggqqplot(indicators_long, "V_valence_corona.pandemie", ggtheme = theme_bw()) +
#   facet_grid(Time ~ Condition)


### Box's (1949) M-test for homogeneity of covariance matrices
# box_m(dat_long[, "mean_valence_macro", drop = FALSE], dat_long$idcodegroup)
# alternative:
# BoxResult <- heplots::boxM(indicators_wide[ ,c("V_valence_corona.pandemie_1", "V_valence_corona.pandemie_2")],indicators_wide$Condition) # time also
# BoxResult$cov
# BoxResult




### outliers
# visual check by boxplots
# bxp <- ggboxplot(
#   indicators_long, x = "Time", y = "V_valence_corona.pandemie",
#   color = "Condition", palette = "jco"
# )
# bxp

# values above Q3 + 1.5xIQR or below Q1 - 1.5xIQR are considered as outliers:
# indicators_long %>%
#   group_by(Time, Condition) %>%
#   identify_outliers(V_valence_corona.pandemie)

### Levene's test for homogeneity of variance across groups
# indicators_long %>%
#   group_by(Time) %>%
#   levene_test(V_valence_corona.pandemie ~ Condition)
# alternative:
# car::leveneTest(V_valence_corona.pandemie ~ Condition, data=indicators_long,
#                 center=mean)
#######################

###
# > use ggplot2 to draw barplot:
###

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      se = sd(x[[col]], na.rm=TRUE) / sqrt(length(x[[col]])))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}


dat_long_ethicscale_noNA <- dat_long_ethicscale[!is.na(dat_long_ethicscale$idcodegroup),]

dfvalcor <- data_summary(dat_long_ethicscale_noNA, varname="mean_all",
                         groupnames=c("timepoint","idcodegroup"))
dfvalcor

# adjust aesthetics of ggplot
ggplot_theme <- theme(axis.title.x = element_blank(),
                      axis.title.y = element_text(size=12),
                      axis.text.x = element_text(size=10,hjust=0.5,vjust=0.5,face="plain", colour = "black"),
                      axis.text.y = element_text(size=12,face="plain", colour = "black"),
                      panel.border = element_blank(),
                      axis.line = element_line(colour = "black"),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank())

# create ggplot2 object for X
p <- ggplot(dfvalcor, aes(x=timepoint, y=mean_all, fill=idcodegroup)) +
  geom_bar(stat="identity", color="black",
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean_all-se, ymax=mean_all+se), width=.2,
                position=position_dodge(.9)) + ggplot_theme + ylab(label = "average valence assigned to corona concept")
print(p)


boxplot(dat_long_ethicscale$mean_all ~ dat_long$idcodegroup)


plot(dat_long_ethicscale$mean_all, dat_long$mean_valence_macro)
cor(dat_long_ethicscale$mean_all, dat_long$mean_valence_macro)



##################
# detecting IER -> mahalanobis distance
##################
# SD
hist(dat_long_ethicscale$SD_all)
plot(dat_long_ethicscale$mean_all, dat_long_ethicscale$SD_all)

# -> mahalanobis distance
?mahalanobis
dat_long_ethicscale$mahal <- mahalanobis(dat_long_ethicscale[,1:38], colMeans(dat_long_ethicscale[,1:38]), cov(dat_long_ethicscale[,1:38]))

dat_long_ethicscale$p_mahal <- pchisq(dat_long_ethicscale$mahal, df=37, lower.tail=FALSE)

dat_long_ethicscale$p_mahal
dat_long_ethicscale$p_mahal <- round(dat_long_ethicscale$p_mahal, digits = 2)
dat_long_ethicscale[dat_long_ethicscale$p_mahal <.01, c("mean_all", "SD_all", "mahal", "p_mahal")]







##################
# item response theory
##################





###############################################
# dat_panas_n <- dat_long %>%
#   select(matches(match = "panas.+n$")) %>%
#   mutate(mean_all = rowMeans(.), SD_all = apply(.,1,sd))
# dat_panas_p <- dat_long %>%
#   select(matches(match = "panas.+p$")) %>%
#   mutate(mean_all = rowMeans(.), SD_all = apply(.,1,sd))
#
# dat_long$mean_panas_n <- dat_panas_n$mean_all
# dat_long$mean_panas_p <- dat_panas_p$mean_all
#
# plot(dat_long$mean_panas_n, dat_long$mean_panas_p)
#
# boxplot(dat_long$mean_panas_n ~ dat_long$idcodegroup)
# boxplot(dat_long$mean_panas_p ~ dat_long$idcodegroup)
#
# ###
# # > mixed anovas
# ###
# fit1 <- afex::aov_car(mean_panas_p ~ timepoint*idcodegroup + Error(prolific_pid / timepoint),
#                       data = dat_long)
#
# fit1a <- afex::aov_ez(id = "prolific_pid", dv = "mean_panas_p",
#                       data = dat_long, between=c("idcodegroup"), within=c("timepoint"))
# # partical eta squared
# anova(fit1, es = "pes")
# # generalized eta squared
# fit1a
###############################################




############################################################################
# Pakete, Daten laden
############################################################################
##################
# single CFA
##################
## all t1
result_tmp <- corr_rel_EFA(constructlist = constructs_list,
                           constnum = 7, data = dat_wide, nfacs = 1) # change nfacs
round(x = result_tmp$`Reliability: Cronbach`$alpha.drop, digits = 2)
print(result_tmp$`fit EFA (PAF)`$loadings, cutoff = .4)

constructs_list[[7]][result_tmp$`fit EFA (PAF)`$loadings <= .4]

##### function beginn: model_lavaan #####
model_lavaan <- function(vars = NULL){
  vec_mod <- c()
  for(i in 1:length(vars)){
    vec_mod[i] <- paste0(c(vars[i], "+"), collapse = "")
  }
  vec_mod[length(vec_mod)] <- gsub(pattern = "+", fixed = TRUE, replacement = "", x = vec_mod[length(vec_mod)])
  vec_mod <- paste(vec_mod,collapse="")
  mod <- paste0("factor1=~",vec_mod)

  return(mod)
}

mod_lavaan <- model_lavaan(vars = constructs_list[[7]][result_tmp$`fit EFA (PAF)`$loadings > .4])
fit <-cfa(mod_lavaan, data = dat_wide, missing = "ML", estimator = "ML")
summary(fit, standardized = TRUE)
pred_lavaan <- lavPredict(fit, method = "ML") # =Bartlett scores

semPlot::semPaths(object = fit, what = "std", edge.label.cex = 0.5)
fitmeasures_lavaan(lavaan_mod = fit)



dat_wide$fc_ethic <- pred_lavaan

##################
#
##################
### PANAS
# P
result_tmp <- corr_rel_EFA(constructlist = constructs_list,
                           constnum = 8, data = dat_wide, nfacs = 1)
print(result_tmp$`fit EFA (PAF)`$loadings, cutoff = .4)
constructs_list[[8]][result_tmp$`fit EFA (PAF)`$loadings <= .4]
mod_lavaan <- model_lavaan(vars = constructs_list[[8]][result_tmp$`fit EFA (PAF)`$loadings > .4])
mod_lavaan

mod_lavaan <- model_lavaan(vars = constructs_list[[8]][result_tmp$`fit EFA (PAF)`$loadings > .4])
fit <-cfa(mod_lavaan, data = dat_wide, missing = "ML", estimator = "ML")
summary(fit, standardized = TRUE)
pred_lavaan <- lavPredict(fit, method = "ML") # =Bartlett scores


dat_wide$fc_panasP <- pred_lavaan
# N
result_tmp <- corr_rel_EFA(constructlist = constructs_list,
                           constnum = 9, data = dat_wide, nfacs = 1)
print(result_tmp$`fit EFA (PAF)`$loadings, cutoff = .4)
constructs_list[[9]][result_tmp$`fit EFA (PAF)`$loadings <= .4]
mod_lavaan <- model_lavaan(vars = constructs_list[[9]][result_tmp$`fit EFA (PAF)`$loadings > .4])
mod_lavaan

mod_lavaan <- model_lavaan(vars = constructs_list[[9]][result_tmp$`fit EFA (PAF)`$loadings > .4])
fit <-cfa(mod_lavaan, data = dat_wide, missing = "ML", estimator = "ML")
summary(fit, standardized = TRUE)
pred_lavaan <- lavPredict(fit, method = "ML") # =Bartlett scores


dat_wide$fc_panasN <- pred_lavaan


(vars_tmp <- colnames(dat_wide)[str_detect(string = colnames(dat_wide), "panas.+n_t0$")])
constructs_list[[9]] <- vars_tmp


mod_lavaan <- model_lavaan(vars = constructs_list[[7]][result_tmp$`fit EFA (PAF)`$loadings > .4])
fit <-cfa(mod_lavaan, data = dat_wide, missing = "ML", estimator = "ML")
summary(fit, standardized = TRUE)
pred_lavaan <- lavPredict(fit, method = "ML") # =Bartlett scores


dat_wide$fc_ethic <- pred_lavaan
### TAM
# bi
result_tmp <- corr_rel_EFA(constructlist = constructs_list,
                           constnum = 10, data = dat_wide, nfacs = 1)
print(result_tmp$`fit EFA (PAF)`$loadings, cutoff = .4)
constructs_list[[10]][result_tmp$`fit EFA (PAF)`$loadings <= .4]
mod_lavaan <- model_lavaan(vars = constructs_list[[10]][result_tmp$`fit EFA (PAF)`$loadings > .4])
mod_lavaan

(vars_tmp <- colnames(dat_wide)[str_detect(string = colnames(dat_wide), "tam_bi.+_t0$")])
constructs_list[[10]] <- vars_tmp
# canx
# (vars_tmp <- colnames(dat_wide)[str_detect(string = colnames(dat_wide), "tam_canx.+_t0$")])
# constructs_list[[11]] <- vars_tmp

model <- '
  # measurement model
 ethicscale=~relativist01_t0+relativist02_t0+relativist03_t0+relativist04_t0+relativist05_t0+
 contractualist01_t0+contractualist02_t0+contractualist03_t0+contractualist04_t0+contractualist05_t0+contractualist06_t0+contractualist07_t0+
 utilitarian01_t0+utilitarian02_t0+utilitarian03_t0+utilitarian04_t0+utilitarian05_t0+utilitarian06_t0+utilitarian07_t0+utilitarian08_t0+utilitarian09_t0+
 deontology02_t0+deontology03_t0+deontology04_t0+deontology05_t0+deontology06_t0+deontology07_t0+deontology08_t0+
 virtue01_t0+virtue02_t0+virtue03_t0+virtue04_t0+virtue05_t0+virtue06_t0+virtue07_t0+virtue08_t0+virtue09_t0+
 hedonism01_t0+hedonism03_t0+hedonism04_t0+hedonism05_t0+hedonism06_t0+hedonism07_t0+hedonism08_t0+hedonism09_t0+hedonism10_t0

 panasP=~panas.01p_t0+panas.02p_t0+panas.03p_t0+panas.04p_t0+panas.05p_t0+panas.06p_t0+panas.07p_t0+panas.08p_t0+panas.09p_t0+panas.10p_t0
 panasN=~panas.01n_t0+panas.02n_t0+panas.03n_t0+panas.04n_t0+panas.05n_t0+panas.06n_t0+panas.07n_t0+panas.08n_t0+panas.09n_t0+panas.10n_t0

 tamBI=~tam_bi.01_t0+tam_bi.02_t0+tam_bi.03_t0+tam_bi.04_t0+tam_bi.05_t0
  # regressions
    panasP ~~ panasN
    ethicscale ~ panasP + panasN
    tamBI ~ ethicscale
'
fit <- sem(model, data = dat_wide)
summary(fit, standardized = TRUE)
semPlot::semPaths(object = fit, what = "std", edge.label.cex = 0.5)





model <- '
  # measurement model
 ethicscale=~relativist01_t0+relativist02_t0+relativist03_t0+relativist04_t0+relativist05_t0+
 contractualist01_t0+contractualist02_t0+contractualist03_t0+contractualist04_t0+contractualist05_t0+contractualist06_t0+contractualist07_t0+
 utilitarian01_t0+utilitarian02_t0+utilitarian03_t0+utilitarian04_t0+utilitarian05_t0+utilitarian06_t0+utilitarian07_t0+utilitarian08_t0+utilitarian09_t0+
 deontology02_t0+deontology03_t0+deontology04_t0+deontology05_t0+deontology06_t0+deontology07_t0+deontology08_t0+
 virtue01_t0+virtue02_t0+virtue03_t0+virtue04_t0+virtue05_t0+virtue06_t0+virtue07_t0+virtue08_t0+virtue09_t0+
 hedonism01_t0+hedonism03_t0+hedonism04_t0+hedonism05_t0+hedonism06_t0+hedonism07_t0+hedonism08_t0+hedonism09_t0+hedonism10_t0

 panasP=~panas.01p_t0+panas.02p_t0+panas.03p_t0+panas.04p_t0+panas.05p_t0+panas.06p_t0+panas.07p_t0+panas.08p_t0+panas.09p_t0+panas.10p_t0
 panasN=~panas.01n_t0+panas.02n_t0+panas.03n_t0+panas.04n_t0+panas.05n_t0+panas.06n_t0+panas.07n_t0+panas.08n_t0+panas.09n_t0+panas.10n_t0

 tamBI=~tam_bi.01_t0+tam_bi.02_t0+tam_bi.03_t0+tam_bi.04_t0+tam_bi.05_t0
  # regressions
    panasP ~~ panasN + ethicscale + tamBI
    panasN ~~ ethicscale + tamBI
    tamBI ~~ ethicscale
'
# tamBI ~ panasP + panasN + ethicscale
model <- '
  # measurement model
 ethicscale=~relativist01_t0+relativist02_t0+relativist03_t0+relativist04_t0+relativist05_t0+
 contractualist01_t0+contractualist02_t0+contractualist03_t0+contractualist04_t0+contractualist05_t0+contractualist06_t0+contractualist07_t0+
 utilitarian01_t0+utilitarian02_t0+utilitarian03_t0+utilitarian04_t0+utilitarian05_t0+utilitarian06_t0+utilitarian07_t0+utilitarian08_t0+utilitarian09_t0+
 deontology02_t0+deontology03_t0+deontology04_t0+deontology05_t0+deontology06_t0+deontology07_t0+deontology08_t0+
 virtue01_t0+virtue02_t0+virtue03_t0+virtue04_t0+virtue05_t0+virtue06_t0+virtue07_t0+virtue08_t0+virtue09_t0+
 hedonism01_t0+hedonism03_t0+hedonism04_t0+hedonism05_t0+hedonism06_t0+hedonism07_t0+hedonism08_t0+hedonism09_t0+hedonism10_t0

 tamBI=~tam_bi.01_t0+tam_bi.02_t0+tam_bi.03_t0+tam_bi.04_t0+tam_bi.05_t0
  # regressions
    tamBI ~ ethicscale
'
fit <- sem(model, data = dat_wide)
summary(fit, standardized = TRUE)
semPlot::semPaths(object = fit, what = "std", edge.label.cex = 0.5)

fitmeasures_lavaan(lavaan_mod = fit)



dat_wide$sociodemo_gender_t0

dat_wide[,c("sociodemo_faith_t0",
        "sociodemo_education_t0",
        "outcomerating_useful.01_t0",
        "outcomerating_good.01_t0")] <-
  lapply(dat_wide[,c("sociodemo_faith_t0",
                 "sociodemo_education_t0",
                 "outcomerating_useful.01_t0",
                 "outcomerating_good.01_t0")], ordered)


model <- '
  # measurement model
 ethicscale=~relativist01_t0+relativist02_t0+relativist03_t0+relativist04_t0+relativist05_t0+
 contractualist01_t0+contractualist02_t0+contractualist03_t0+contractualist04_t0+contractualist05_t0+contractualist06_t0+contractualist07_t0+
 utilitarian01_t0+utilitarian02_t0+utilitarian03_t0+utilitarian04_t0+utilitarian05_t0+utilitarian06_t0+utilitarian07_t0+utilitarian08_t0+utilitarian09_t0+
 deontology02_t0+deontology03_t0+deontology04_t0+deontology05_t0+deontology06_t0+deontology07_t0+deontology08_t0+
 virtue01_t0+virtue02_t0+virtue03_t0+virtue04_t0+virtue05_t0+virtue06_t0+virtue07_t0+virtue08_t0+virtue09_t0+
 hedonism01_t0+hedonism03_t0+hedonism04_t0+hedonism05_t0+hedonism06_t0+hedonism07_t0+hedonism08_t0+hedonism09_t0+hedonism10_t0

 panasP=~panas.01p_t0+panas.02p_t0+panas.03p_t0+panas.04p_t0+panas.05p_t0+panas.06p_t0+panas.07p_t0+panas.08p_t0+panas.09p_t0+panas.10p_t0
 panasN=~panas.01n_t0+panas.02n_t0+panas.03n_t0+panas.04n_t0+panas.05n_t0+panas.06n_t0+panas.07n_t0+panas.08n_t0+panas.09n_t0+panas.10n_t0

  # regressions
    panasP ~~ panasN + ethicscale
    panasN ~~ ethicscale
    ethicscale ~ sociodemo_faith_t0 + sociodemo_education_t0 + sociodemo_gender_t0
'
fit <- sem(model, data = dat_wide, ordered = c("sociodemo_faith_t0", "sociodemo_education_t0"))
summary(fit, standardized = TRUE)
semPlot::semPaths(object = fit, what = "std", edge.label.cex = 0.5)


#############################
#############################
dat_wide$sociodemo_age_t0 <- as.numeric(dat_wide$sociodemo_age_t0)
lm1 <- lm(formula = fc_ethic ~ fc_panasN + fc_panasP + mean_valence_macro_t0 + sociodemo_faith_t0 + sociodemo_education_t0 + sociodemo_gender_t0 + sociodemo_age_t0 + density_macro_t0, data = dat_wide)
summary(lm1)


lm2 <- lm(formula = fc_ethic ~ fc_panasN + fc_panasP + mean_valence_macro_t0, data = dat_wide)
summary(lm2)


dat_wide$mean_bi <- rowMeans(dat_wide[, str_subset(string = colnames(dat_wide), pattern = "tam_bi.*_t0")])


lm2 <- lm(formula = fc_panasP ~ fc_ethic + fc_panasN + mean_valence_macro_t0, data = dat_wide)
summary(lm2)

lm3 <- lm(formula = mean_bi ~ fc_ethic + fc_panasP + fc_panasN, data = dat_wide)
summary(lm3)

lm3 <- lm(formula = mean_bi ~ fc_ethic, data = dat_wide)
summary(lm3)

plot(dat_wide$mean_bi ~ dat_wide$fc_ethic)
abline(lm3)



ggplotRegression <- function (fit) {

  require(ggplot2)

  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) +
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",round(x = signif(summary(fit)$adj.r.squared, 5), digits = 3),
                       "Intercept =",round(x = signif(fit$coef[[1]],5 ), digits = 2),
                       " Slope =",round(x = signif(fit$coef[[2]], 5), digits = 2),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}
ggplotRegression(lm(mean_bi ~ fc_ethic, data = dat_wide))

# levels(dat_wide$sociodemo_faith_t0) <- c("")
lm1 <- lm(formula = fc_ethic ~ fc_panasN + fc_panasP + mean_valence_macro_t0 + sociodemo_faith_t0 +
            sociodemo_education_t0 + sociodemo_gender_t0 + sociodemo_age_t0 + density_macro_t0, data = dat_wide)
summary(lm1)
# lm1 <- lm(formula = mean_all ~ fc_panasN + fc_panasP + mean_valence_macro_t0 + sociodemo_faith_t0 + sociodemo_education_t0 + sociodemo_gender_t0 + sociodemo_age_t0, data = dat_wide)
# summary(lm1)
#
# lm1 <- lm(formula = fc_ethic ~ fc_panasN + fc_panasP + mean_valence_macro_t0 + sociodemo_faith_t0 + sociodemo_education_t0 + sociodemo_gender_t0 + sociodemo_age_t0, data = dat_wide)
# summary(lm1)

# library(tidyverse)
library(mvtnorm)
library(plotly)
library(MASS)
sigma <- matrix(c(4,1,2,1,5,4,2,4,6), ncol = 3)
sigma
cov2cor(sigma)
means <- c(0, 0, 0)
set.seed(42)
n <- 1000
x <- rmvnorm(n = n, mean = means, sigma = sigma)
d <- data.frame(x)
names(d)
p4 <- plot_ly(d, x = ~ X1, y = ~ X2, z = ~ X3,
              marker = list(color = ~ X2,
                            showscale = TRUE)) %>%
  add_markers()

p4



d$mahal <- mahalanobis(d, colMeans(d), cov(d))

d$p_mahal <- pchisq(d$mahal, df=2, lower.tail=FALSE)

dat_long_ethicscale$p_mahal
dat_long_ethicscale$p_mahal <- round(dat_long_ethicscale$p_mahal, digits = 2)
dat_long_ethicscale[, c("mean_all", "SD_all", "mahal", "p_mahal")]


#####################################
#####################################
mean(dat_wide$relativist01_t0)
mean(dat_wide$relativist03_t0)

vars <- str_subset(string = colnames(dat_wide), pattern = "^relativist.*_t0")
dat_wide[,vars]
rowMeans(x = dat_wide[,vars])


dat_wide %>%
  dplyr::select(matches(match = "^relativist.*_t0")) %>%
  rowwise() %>%
  mutate(relativst_mean = mean(c_across()), relativst_sd = sd(c_across()))

dat_wide %>%
  dplyr::select(matches(match = "^relativist.*_t0"), matches(match = "^contractualist.*_t0")) %>%
  rowwise() %>%
  mutate(relativst_mean = mean(c_across(cols = matches(match = "^relativist.*_t0"))),
         relativst_sd = sd(c_across(cols = matches(match = "^relativist.*_t0"))),
         contractualist_mean = mean(c_across(cols = matches(match = "^contractualist.*_t0"))),
         contractualist_sd = sd(c_across(cols = matches(match = "^contractualist.*_t0"))))



dat_wide2 <- dat_wide %>%
  rowwise() %>%
  mutate(relativist_mean = mean(c_across(cols = matches(match = "^relativist.*_t0"))),
         contractualist_mean = mean(c_across(cols = matches(match = "^contractualist.*_t0"))),
         hedonism_mean = mean(c_across(cols = matches(match = "^hedonism.*_t0"))),
         utilitarian_mean = mean(c_across(cols = matches(match = "^utilitarian.*_t0"))),
         deontology_mean = mean(c_across(cols = matches(match = "^deontology.*_t0"))),
         virtue_mean = mean(c_across(cols = matches(match = "^virtue.*_t0"))),
         ethic_mean = mean(c_across(cols = matches(match = "^relativist.*_t0|^contractualist.*_t0|^hedonism.*_t0|^utilitarian.*_t0|^deontology.*_t0|^virtue.*_t0"))))

aa <- dat_wide2

aa$relativist_mean <- dat_wide2$relativist_mean
aa$contractualist_mean <- dat_wide2$contractualist_mean
aa$hedonism_mean <- dat_wide2$hedonism_mean
aa$utilitarian_mean <- dat_wide2$utilitarian_mean
aa$deontology_mean <- dat_wide2$deontology_mean
aa$virtue_mean <- dat_wide2$virtue_mean
aa$ethic_mean <- dat_wide2$ethic_mean




### Grafik speichern
save_graphic <- function(filename){
  tmp <- paste(filename, ".png", sep = "")
  Cairo::Cairo(file=tmp,
               type="png",
               units="px",
               width=2500,
               height=1700,
               pointsize=44, #text is shrinking by saving graphic
               dpi= "auto",
               bg = "white")
}
save_graphic(filename = "saeulendia_iqkat")
hist(aa$ethic_mean, freq = FALSE)
# abline(v = mean(aa$ethic_mean, na.rm = TRUE))
# lines(density(aa$ethic_mean[!is.na(aa$ethic_mean)]), col="red") # empirical density
lines(seq(0, 7, by=.1), dnorm(seq(0, 7, by=.1),
                              mean(aa$ethic_mean, na.rm = TRUE), sd(aa$ethic_mean, na.rm = TRUE)), col="blue") # normal
dev.off()



haven::write_sav(data = aa, path = "questionnaire_final_t1_new.sav")
#
#
#
#
dat_wide2 %>%
  group_by(sociodemo_faith_t0) %>%
  summarise(n=n(), mean = mean(contractualist_mean), median = median(contractualist_mean))
