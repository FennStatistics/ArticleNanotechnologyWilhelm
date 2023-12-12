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
p_load('tidyverse', 'jsonlite', 'pracma', 'xlsx', 'lme4', 'nlme',
       'performance', 'lattice', 'stargazer', 'psych', 'ggplot2', 'lavaan', 'polycor', 'ggstatsplot')


### Daten
# Read the text file from JATOS ...
dir()
dat_wide <- read.xlsx(file = "dat_wide_merged_final.xlsx", sheetIndex = 1)
dat_long <- read.xlsx(file = "dat_long_merged_final.xlsx", sheetIndex = 1)


dat_long <- dat_long[!is.na(dat_long$idcodegroup),]
sort(table(dat_long$prolific_pid))

dat_long$pre <- ifelse(test = dat_long$timepoint == 0, yes = 1, no = 0)
dat_long$post <- ifelse(test = dat_long$timepoint == 1, yes = 1, no = 0)

dat_long <- fastDummies::dummy_cols(dat_long, select_columns = "idcodegroup",
                                    remove_first_dummy = FALSE)


head(dat_long)


############################################################################
# data cleaning
############################################################################
### PANAS pos
## Mahalanobis Distance
# dat_long$mahal_panas_pos <- mahalanobis(dat_long[, str_subset(string = colnames(dat_long), pattern = "panas.*p")],
#             colMeans(dat_long[, str_subset(string = colnames(dat_long), pattern = "panas.*p")]),
#             cov(dat_long[, str_subset(string = colnames(dat_long), pattern = "panas.*p")]))
# dat_long$p_mahal_panas_pos <- pchisq(dat_long$mahal_panas_pos, df=9, lower.tail=FALSE)
#
# dat_long[dat_long$p_mahal_panas_pos <.001, str_subset(string = colnames(dat_long), pattern = "panas.*p")]

## intra variability
# dat_long$SD_panas_pos <- apply(dat_long[, str_subset(string = colnames(dat_long), pattern = "panas.*p$")],
#       MARGIN = 1, sd)
# dat_long[dat_long$SD_panas_pos == 0, str_subset(string = colnames(dat_long), pattern = "panas.*p")]
# dat_long[dat_long$SD_panas_pos > 1.5, str_subset(string = colnames(dat_long), pattern = "panas.*p")]


### PANAS neg
## Mahalanobis Distance
# dat_long$mahal_panas_neg <- mahalanobis(dat_long[, str_subset(string = colnames(dat_long), pattern = "panas.*n")],
#                                         colMeans(dat_long[, str_subset(string = colnames(dat_long), pattern = "panas.*n")]),
#                                         cov(dat_long[, str_subset(string = colnames(dat_long), pattern = "panas.*n")]))
# dat_long$p_mahal_panas_neg <- pchisq(dat_long$mahal_panas_neg, df=9, lower.tail=FALSE)
#
# dat_long[dat_long$mahal_panas_neg <.001, str_subset(string = colnames(dat_long), pattern = "panas.*n")]


############################################################################
# mediation design: multivariate multilevel models
############################################################################
##################
# factor scores (Bartlett) for PANAS scale negative and positive
##################
### using polychoric correlation matrix
fit_efa <- fa(r = dat_long[,str_subset(string = colnames(dat_long), pattern = "panas.*n$")],
              nfactors = 1, rotate = "none", max.iter = 500, cor = "poly", scores = "Bartlett")
fa.diagram(fit_efa)
fit_efa
dat_long$fc_panas_neg <- fit_efa$scores




fit_efa <- fa(r = dat_long[,str_subset(string = colnames(dat_long), pattern = "panas.*p$")],
              nfactors = 1, rotate = "none", fm = "wls", max.iter = 500, cor = "poly", scores = "Bartlett")
fa.diagram(fit_efa)
fit_efa
dat_long$fc_panas_pos <- fit_efa$scores

plot(dat_long$fc_panas_neg, dat_long$mean_valence_macro)
cor(dat_long$fc_panas_neg, dat_long$mean_valence_macro)
plot(dat_long$fc_panas_pos, dat_long$mean_valence_macro)
cor(dat_long$fc_panas_pos, dat_long$mean_valence_macro)


hist(dat_long$mean_valence_macro)





##################
# multivariate multilevel models
##################
# mean difference between fixed occasions model for 3 groups:
mmm1 <- lme(mean_valence_macro ~ 1 + post*idcodegroup_positive +
              post*idcodegroup_negative,
            random = ~ 1 + post | prolific_pid,
            data = dat_long,
            control=list(sigma=1e-10, opt="optim"))

summary(mmm1)

stargazer(mmm1, type = "html", out = "mmm1.html")

dat_long %>%
  group_by(idcodegroup, timepoint) %>%
  summarize(mean(mean_valence_macro), mean(fc_panas_neg))


#######################################################
tmp <- dat_long %>%
  filter(timepoint == 1)
boxplot(tmp$mean_valence_macro ~ tmp$idcodegroup)

table(dat_long$idcodegroup)
tmp <- dat_long %>%
  filter(idcodegroup == "negative") %>%
  select(mean_valence_macro, timepoint)
boxplot(tmp$mean_valence_macro ~ tmp$timepoint)

tmp <- dat_long %>%
  filter(idcodegroup == "positive") %>%
  select(mean_valence_macro, timepoint)
boxplot(tmp$mean_valence_macro ~ tmp$timepoint)
#######################################################
set.seed(123)
library(dplyr, warn.conflicts = FALSE)


tmp <- dat_long %>%
  filter(idcodegroup == "negative") %>%
  select(mean_valence_macro, timepoint, prolific_pid)
# boxplot(tmp$mean_valence_macro ~ tmp$timepoint)
# create a plot
p <- ggwithinstats(
  data = tmp,
  x    = timepoint,
  y    = mean_valence_macro,
  type = "np"
)
# looking at the plot
p


tmp <- dat_long %>%
  filter(idcodegroup == "positive") %>%
  select(mean_valence_macro, timepoint, prolific_pid)
# boxplot(tmp$mean_valence_macro ~ tmp$timepoint)


# create a plot
p <- ggwithinstats(
  data = filter(dat_long, idcodegroup == "negative"),
  x    = timepoint,
  y    = mean_valence_macro,
  type = "np"
)
# looking at the plot
p



p <- ggwithinstats(
  data = filter(bugs_long, condition %in% c("HDHF", "HDLF")),
  x    = condition,
  y    = desire,
  type = "np"
)






# add Level-2 predictor
mmm2 <- lme(mean_valence_macro ~ 1 + post*idcodegroup_positive +
              post*idcodegroup_negative + post*fc_panas_neg,
            random = ~ 1 + post | prolific_pid,
            data = dat_long,
            control=list(sigma=1e-10, opt="optim"))

summary(mmm2)

stargazer(mmm2, type = "html", out = "mmm2.html")



set.seed(123)
library(dplyr, warn.conflicts = FALSE)

# create a plot
p <- ggwithinstats(
  data = filter(bugs_long, condition %in% c("HDHF", "HDLF")),
  x    = condition,
  y    = desire,
  type = "np"
)


# looking at the plot
p



############################################################################
# structural equation modelling
############################################################################
##################
# EFA ethic scale // TAM BI // PANAS neg, pos
##################
### using polychoric correlation matrix
fit_efa <- fa(r = dat_wide[,str_subset(string = colnames(dat_wide), pattern = "^relativist.*_t0|^contractualist.*_t0|^hedonism.*_t0|^utilitarian.*_t0|^deontology.*_t0|^virtue.*_t0")[c(1:13,15:31,33:38)]],
              nfactors = 1, rotate = "none", fm = "wls", max.iter = 500, cor = "poly")
fa.diagram(fit_efa)
fit_efa
dat_wide$fc_ethic <- fit_efa$scores
hist(dat_wide$fc_ethic)

### using polychoric correlation matrix
fit_efa <- fa(r = dat_wide[,str_subset(string = colnames(dat_wide), pattern = "tam_bi.*_t0")],
              nfactors = 1, rotate = "none", fm = "wls", max.iter = 500, cor = "poly")
fa.diagram(fit_efa)
fit_efa
dat_wide$fc_tambi <- fit_efa$scores
hist(dat_wide$fc_tambi)

### using polychoric correlation matrix
fit_efa <- fa(r = dat_wide[,str_subset(string = colnames(dat_wide), pattern = "panas.*n_t0$")],
              nfactors = 1, rotate = "none", fm = "wls", max.iter = 500, cor = "poly", scores = "Bartlett")
fa.diagram(fit_efa)
fit_efa
dat_wide$fc_panas_neg <- fit_efa$scores
hist(dat_wide$fc_panas_neg)

fit_efa <- fa(r = dat_wide[,str_subset(string = colnames(dat_wide), pattern = "panas.*p_t0$")],
              nfactors = 1, rotate = "none", fm = "wls", max.iter = 500, cor = "poly", scores = "Bartlett")
fa.diagram(fit_efa)
fit_efa
dat_wide$fc_panas_pos <- fit_efa$scores
hist(dat_wide$fc_panas_pos)


##################
# CAM network indicators
##################
psych::corPlot(r = dat_wide[,str_subset(string = colnames(dat_wide), pattern = "macro_t0$")[c(1,3:5,9,12,14,19,23:26)]])


fa.parallel(x = dat_wide[,str_subset(string = colnames(dat_wide), pattern = "macro_t0$")[c(1,3:5,9,12,14,19,23:26)]],n.obs=150, fm = "wls")

fit_efa <- fa(r = dat_wide[,str_subset(string = colnames(dat_wide), pattern = "macro_t0$")[c(1,3:5,9,12,14,19,23:26)]],
              nfactors = 4, rotate = "none", fm = "wls", max.iter = 500)
fa.diagram(fit_efa)
fit_efa$loadings
fit_efa

fa.parallel(x = dat_wide[,c("centr_degree_macro_t0",
                            "meanDistance_directed_macro_t0",
                            "diameter_unweighted_undirected_macro_t0",
                            "num_nodes_macro_t0",
                            "assortativityDegree_macro_t0")],n.obs=150, fm = "wls")
dat_wide$centr_degree_macro_t0 <- 1-dat_wide$centr_degree_macro_t0
psych::corPlot(r = dat_wide[,c("centr_degree_macro_t0",
                               "meanDistance_directed_macro_t0",
                               "diameter_unweighted_undirected_macro_t0",
                               "num_nodes_macro_t0",
                               "assortativityDegree_macro_t0")])


##################
# single CFA
##################
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

mod_lavaan <- model_lavaan(vars = str_subset(string = colnames(dat_wide), pattern = "panas.*n_t0$"))

dat_wide[,c("panas.01n_t0",
        "panas.02n_t0",
        "panas.03n_t0",
        "panas.04n_t0",
        "panas.05n_t0",
        "panas.06n_t0",
        "panas.07n_t0",
        "panas.08n_t0",
        "panas.09n_t0",
        "panas.10n_t0")] <-
  lapply(dat_wide[,c("panas.01n_t0",
                     "panas.02n_t0",
                     "panas.03n_t0",
                     "panas.04n_t0",
                     "panas.05n_t0",
                     "panas.06n_t0",
                     "panas.07n_t0",
                     "panas.08n_t0",
                     "panas.09n_t0",
                     "panas.10n_t0")], ordered)

fit <- cfa(mod_lavaan, data = dat_wide,
           ordered = c("panas.01n_t0",
                       "panas.02n_t0",
                       "panas.03n_t0",
                       "panas.04n_t0",
                       "panas.05n_t0",
                       "panas.06n_t0",
                       "panas.07n_t0",
                       "panas.08n_t0",
                       "panas.09n_t0",
                       "panas.10n_t0"))
summary(fit, standardized = TRUE)
modificationindices(fit, sort=TRUE)
fitmeasures(fit)
semPlot::semPaths(object = fit, what = "std", edge.label.cex = 0.5)

### correlated residuals
mod_lavaan <- "
factor1 =~ panas.01n_t0+panas.02n_t0+panas.03n_t0+panas.04n_t0+panas.05n_t0+
panas.06n_t0+panas.07n_t0+panas.08n_t0+panas.09n_t0+panas.10n_t0

### adjustements
panas.03n_t0 ~~ panas.07n_t0
panas.05n_t0 ~~ panas.06n_t0
panas.04n_t0 ~~ panas.10n_t0
"

fit <- cfa(mod_lavaan, data = dat_wide,
           ordered = c("panas.01n_t0",
                       "panas.02n_t0",
                       "panas.03n_t0",
                       "panas.04n_t0",
                       "panas.05n_t0",
                       "panas.06n_t0",
                       "panas.07n_t0",
                       "panas.08n_t0",
                       "panas.09n_t0",
                       "panas.10n_t0"))
summary(fit, standardized = TRUE)
modificationindices(fit, sort=TRUE)
fitmeasures(fit)
semPlot::semPaths(object = fit, what = "std", edge.label.cex = 0.5)




##################
# SEMs
##################

dat_wide[,c(  "tam_bi.01_t0",
              "tam_bi.02_t0",
              "tam_bi.03_t0",
              "tam_bi.04_t0",
              "tam_bi.05_t0")] <-
  lapply(dat_wide[,c(  "tam_bi.01_t0",
                       "tam_bi.02_t0",
                       "tam_bi.03_t0",
                       "tam_bi.04_t0",
                       "tam_bi.05_t0")], ordered)

dat_wide[,c("panas.01p_t0",
            "panas.02p_t0",
            "panas.03p_t0",
            "panas.04p_t0",
            "panas.05p_t0",
            "panas.06p_t0",
            "panas.07p_t0",
            "panas.08p_t0",
            "panas.09p_t0",
            "panas.10p_t0")] <-
  lapply(dat_wide[,c("panas.01p_t0",
                     "panas.02p_t0",
                     "panas.03p_t0",
                     "panas.04p_t0",
                     "panas.05p_t0",
                     "panas.06p_t0",
                     "panas.07p_t0",
                     "panas.08p_t0",
                     "panas.09p_t0",
                     "panas.10p_t0")], ordered)

dat_wide[,c("tam_pu.01_t0",
            "tam_pu.02_t0",
            "tam_pu.03_t0",
            "tam_pu.04_t0")] <-
  lapply(dat_wide[,c("tam_pu.01_t0",
                     "tam_pu.02_t0",
                     "tam_pu.03_t0",
                     "tam_pu.04_t0")], ordered)

dat_wide[,c(str_subset(string = colnames(dat_wide), pattern = "^relativist.*_t0|^contractualist.*_t0|^hedonism.*_t0|^utilitarian.*_t0|^deontology.*_t0|^virtue.*_t0")[c(1:13,15:31,33:38)])] <-
  lapply(dat_wide[,c(str_subset(string = colnames(dat_wide), pattern = "^relativist.*_t0|^contractualist.*_t0|^hedonism.*_t0|^utilitarian.*_t0|^deontology.*_t0|^virtue.*_t0")[c(1:13,15:31,33:38)])], ordered)



# str_subset(string = colnames(dat_wide), pattern = "^relativist.*_t0|^contractualist.*_t0|^hedonism.*_t0|^utilitarian.*_t0|^deontology.*_t0|^virtue.*_t0")[c(1:13,15:31,33:38)]
# mod_lavaan <- model_lavaan(vars = str_subset(string = colnames(dat_wide), pattern = "^relativist.*_t0|^contractualist.*_t0|^hedonism.*_t0|^utilitarian.*_t0|^deontology.*_t0|^virtue.*_t0")[c(1:13,15:31,33:38)]); mod_lavaan
# mod_lavaan <- model_lavaan(vars = str_subset(string = colnames(dat_wide), pattern = "tam_pu.*_t0$")); mod_lavaan

# CAMnet =~ centr_degree_macro_t0 + meanDistance_directed_macro_t0 + diameter_unweighted_undirected_macro_t0 + num_nodes_macro_t0 + assortativityDegree_macro_t0

# model <- '
#   # measurement model
#  negPan =~ panas.01n_t0+panas.02n_t0+panas.03n_t0+panas.04n_t0+panas.05n_t0+panas.06n_t0+panas.07n_t0+panas.08n_t0+panas.09n_t0+panas.10n_t0
#
#  posPan =~ panas.01p_t0+panas.02p_t0+panas.03p_t0+panas.04p_t0+panas.05p_t0+panas.06p_t0+panas.07p_t0+panas.08p_t0+panas.09p_t0+panas.10p_t0
#
#  puTAM =~ tam_pu.01_t0+tam_pu.02_t0+tam_pu.03_t0+tam_pu.04_t0
#
#  genEthic =~ relativist01_t0+relativist02_t0+relativist03_t0+relativist04_t0+relativist05_t0+contractualist01_t0+contractualist02_t0+contractualist03_t0+contractualist04_t0+contractualist05_t0+contractualist06_t0+contractualist07_t0+hedonism01_t0+hedonism03_t0+hedonism04_t0+hedonism05_t0+hedonism06_t0+hedonism07_t0+hedonism08_t0+hedonism09_t0+hedonism10_t0+utilitarian01_t0+utilitarian02_t0+utilitarian03_t0+utilitarian04_t0+utilitarian05_t0+utilitarian06_t0+utilitarian07_t0+utilitarian08_t0+utilitarian09_t0+deontology02_t0+deontology03_t0+deontology04_t0+deontology05_t0+deontology06_t0+deontology07_t0
#
#
#  biTAM =~ tam_bi.01_t0+tam_bi.02_t0+tam_bi.03_t0+tam_bi.04_t0+tam_bi.05_t0
#
#   # regressions
#     biTAM ~ negPan + posPan + puTAM + genEthic + mean_valence_macro_t0
# '


model <- '
  # measurement model
 negPan =~ panas.01n_t0+panas.02n_t0+panas.03n_t0+panas.04n_t0+panas.05n_t0+panas.06n_t0+panas.07n_t0+panas.08n_t0+panas.09n_t0+panas.10n_t0

 posPan =~ panas.01p_t0+panas.02p_t0+panas.03p_t0+panas.04p_t0+panas.05p_t0+panas.06p_t0+panas.07p_t0+panas.08p_t0+panas.09p_t0+panas.10p_t0

 puTAM =~ tam_pu.01_t0+tam_pu.02_t0+tam_pu.03_t0+tam_pu.04_t0

 biTAM =~ tam_bi.01_t0+tam_bi.02_t0+tam_bi.03_t0+tam_bi.04_t0+tam_bi.05_t0
  # regressions
    biTAM ~ negPan + posPan + puTAM + mean_valence_macro_t0 + fc_ethic

'


model <- '
  # measurement model
 negPan =~ panas.01n_t0+panas.02n_t0+panas.03n_t0+panas.04n_t0+panas.05n_t0+panas.06n_t0+panas.07n_t0+panas.08n_t0+panas.09n_t0+panas.10n_t0

 posPan =~ panas.01p_t0+panas.02p_t0+panas.03p_t0+panas.04p_t0+panas.05p_t0+panas.06p_t0+panas.07p_t0+panas.08p_t0+panas.09p_t0+panas.10p_t0

 puTAM =~ tam_pu.01_t0+tam_pu.02_t0+tam_pu.03_t0+tam_pu.04_t0

    # covariances
    mean_valence_macro_t0 ~~ negPan + posPan + puTAM
'

fit <- sem(model, data = dat_wide,
           ordered = c("panas.01n_t0",
                       "panas.02n_t0",
                       "panas.03n_t0",
                       "panas.04n_t0",
                       "panas.05n_t0",
                       "panas.06n_t0",
                       "panas.07n_t0",
                       "panas.08n_t0",
                       "panas.09n_t0",
                       "panas.10n_t0",
                       "panas.01p_t0",
                       "panas.02p_t0",
                       "panas.03p_t0",
                       "panas.04p_t0",
                       "panas.05p_t0",
                       "panas.06p_t0",
                       "panas.07p_t0",
                       "panas.08p_t0",
                       "panas.09p_t0",
                       "panas.10p_t0",
                       "tam_bi.01_t0",
                       "tam_bi.02_t0",
                       "tam_bi.03_t0",
                       "tam_bi.04_t0",
                       "tam_bi.05_t0",
                       "tam_pu.01_t0",
                       "tam_pu.02_t0",
                       "tam_pu.03_t0",
                       "tam_pu.04_t0"))


fit <- sem(model, data = dat_wide,
           ordered = c("panas.01n_t0",
                       "panas.02n_t0",
                       "panas.03n_t0",
                       "panas.04n_t0",
                       "panas.05n_t0",
                       "panas.06n_t0",
                       "panas.07n_t0",
                       "panas.08n_t0",
                       "panas.09n_t0",
                       "panas.10n_t0",
                       "panas.01p_t0",
                       "panas.02p_t0",
                       "panas.03p_t0",
                       "panas.04p_t0",
                       "panas.05p_t0",
                       "panas.06p_t0",
                       "panas.07p_t0",
                       "panas.08p_t0",
                       "panas.09p_t0",
                       "panas.10p_t0",
                       "tam_pu.01_t0",
                       "tam_pu.02_t0",
                       "tam_pu.03_t0",
                       "tam_pu.04_t0"))
summary(fit, standardized = TRUE)
semPlot::semPaths(object = fit, what = "std", edge.label.cex = 0.5)
fitmeasures(fit)



# https://rpubs.com/mkearney/103040
pars.factors <- standardizedSolution(fit)[ standardizedSolution(fit)[,'op']=='=~', c(1:5)]
pars.regressions <- standardizedSolution(fit)[ standardizedSolution(fit)[,'op']=='~', c(3,4,5,7)]

stargazer(pars.factors, summary=FALSE, type='html', rownames=FALSE,
          initial.zero=FALSE, digits=3, title='Factor loadings', out = "CFA_factorloadings.html")

stargazer(pars.regressions, summary=FALSE, type='html', rownames=FALSE,
          initial.zero=FALSE, digits=3, title='Predicting DV', out = "predict_DV.html")









###########################################
###########################################
lm1 <- lm(formula = fc_tambi ~ mean_valence_macro_t0+mean_valence_normed_macro_t0+density_macro_t0+transitivity_macro_t0+centr_degree_macro_t0+centr_clo_macro_t0+centr_betw_macro_t0+centr_eigen_macro_t0+meanDistance_directed_macro_t0+meanDistance_undirected_macro_t0+diameter_weighted_undirected_macro_t0+diameter_unweighted_undirected_macro_t0+diameter_unweighted_directed_macro_t0+num_nodes_macro_t0+num_nodes_pos_macro_t0+num_nodes_neg_macro_t0+num_nodes_neut_macro_t0+num_nodes_ambi_macro_t0+num_edges_macro_t0+num_edges_solid_macro_t0+num_edges_dashed_macro_t0+num_edges_invaliddashed_macro_t0+meanWeightEdges_macro_t0+reciprocity_macro_t0+assortativity_valence_macro_t0+assortativityDegree_macro_t0, data = dat_wide)
summary(lm1)
stargazer(lm1, type = "html", out = "explorative LM - DV TAM BI.html")

lm1 <- lm(formula = fc_tambi ~ fc_ethic+fc_panas_neg+fc_panas_pos+mean_valence_macro_t0+density_macro_t0+transitivity_macro_t0+centr_degree_macro_t0+centr_clo_macro_t0+centr_betw_macro_t0+centr_eigen_macro_t0+meanDistance_directed_macro_t0+meanDistance_undirected_macro_t0+diameter_weighted_undirected_macro_t0+diameter_unweighted_undirected_macro_t0+diameter_unweighted_directed_macro_t0+num_nodes_macro_t0+num_nodes_pos_macro_t0+num_nodes_neg_macro_t0+num_nodes_neut_macro_t0+num_edges_macro_t0+num_edges_solid_macro_t0+meanWeightEdges_macro_t0+reciprocity_macro_t0+assortativity_valence_macro_t0+assortativityDegree_macro_t0, data = dat_wide)
summary(lm1)
lm1 <- lm(formula = fc_tambi ~ fc_ethic+fc_panas_neg+fc_panas_pos+mean_valence_macro_t0+density_macro_t0+transitivity_macro_t0
          , data = dat_wide)
summary(lm1)

lm1 <- lm(formula = mean_valence_macro_t0 ~ fc_panas_neg+fc_panas_pos+density_macro_t0+transitivity_macro_t0, data = dat_wide)
summary(lm1)

hist(dat_wide$mean_valence_macro_t0)

# Logistics Regression
glm.fit <- glm(outcomedummy_prohibited_t0 ~ fc_ethic+fc_tambi+fc_panas_neg+fc_panas_pos+ mean_valence_macro_t0+mean_valence_normed_macro_t0+density_macro_t0+transitivity_macro_t0+centr_degree_macro_t0+centr_clo_macro_t0+centr_betw_macro_t0+centr_eigen_macro_t0+meanDistance_directed_macro_t0+meanDistance_undirected_macro_t0+diameter_weighted_undirected_macro_t0+diameter_unweighted_undirected_macro_t0+diameter_unweighted_directed_macro_t0+num_nodes_macro_t0+num_nodes_pos_macro_t0+num_nodes_neg_macro_t0+num_nodes_neut_macro_t0+num_nodes_ambi_macro_t0+num_edges_macro_t0+num_edges_solid_macro_t0+num_edges_dashed_macro_t0+num_edges_invaliddashed_macro_t0+meanWeightEdges_macro_t0+reciprocity_macro_t0+assortativity_valence_macro_t0+assortativityDegree_macro_t0,
               data = dat_wide, family = binomial)
stargazer(glm.fit, type = "html", out = "explorative Logistic - DV Dummy_Prohibit.html")



summary(glm.fit)

glm.fit <- glm(outcomedummy_funds_t0 ~ fc_ethic+fc_tambi+fc_panas_neg+fc_panas_pos+ mean_valence_macro_t0+mean_valence_normed_macro_t0+density_macro_t0+transitivity_macro_t0+centr_degree_macro_t0+centr_clo_macro_t0+centr_betw_macro_t0+centr_eigen_macro_t0+meanDistance_directed_macro_t0+meanDistance_undirected_macro_t0+diameter_weighted_undirected_macro_t0+diameter_unweighted_undirected_macro_t0+diameter_unweighted_directed_macro_t0+num_nodes_macro_t0+num_nodes_pos_macro_t0+num_nodes_neg_macro_t0+num_nodes_neut_macro_t0+num_nodes_ambi_macro_t0+num_edges_macro_t0+num_edges_solid_macro_t0+num_edges_dashed_macro_t0+num_edges_invaliddashed_macro_t0+meanWeightEdges_macro_t0+reciprocity_macro_t0+assortativity_valence_macro_t0+assortativityDegree_macro_t0,
               data = dat_wide, family = binomial)
summary(glm.fit)

glm.fit <- glm(outcomedummy_moral_t0 ~ fc_ethic+fc_tambi+fc_panas_neg+fc_panas_pos+ mean_valence_macro_t0+mean_valence_normed_macro_t0+density_macro_t0+transitivity_macro_t0+centr_degree_macro_t0+centr_clo_macro_t0+centr_betw_macro_t0+centr_eigen_macro_t0+meanDistance_directed_macro_t0+meanDistance_undirected_macro_t0+diameter_weighted_undirected_macro_t0+diameter_unweighted_undirected_macro_t0+diameter_unweighted_directed_macro_t0+num_nodes_macro_t0+num_nodes_pos_macro_t0+num_nodes_neg_macro_t0+num_nodes_neut_macro_t0+num_nodes_ambi_macro_t0+num_edges_macro_t0+num_edges_solid_macro_t0+num_edges_dashed_macro_t0+num_edges_invaliddashed_macro_t0+meanWeightEdges_macro_t0+reciprocity_macro_t0+assortativity_valence_macro_t0+assortativityDegree_macro_t0,
               data = dat_wide, family = binomial)
summary(glm.fit)

glm.fit <- glm(outcomedummy_insurance_t0 ~ fc_ethic+fc_tambi+fc_panas_neg+fc_panas_pos+ mean_valence_macro_t0+mean_valence_normed_macro_t0+density_macro_t0+transitivity_macro_t0+centr_degree_macro_t0+centr_clo_macro_t0+centr_betw_macro_t0+centr_eigen_macro_t0+meanDistance_directed_macro_t0+meanDistance_undirected_macro_t0+diameter_weighted_undirected_macro_t0+diameter_unweighted_undirected_macro_t0+diameter_unweighted_directed_macro_t0+num_nodes_macro_t0+num_nodes_pos_macro_t0+num_nodes_neg_macro_t0+num_nodes_neut_macro_t0+num_nodes_ambi_macro_t0+num_edges_macro_t0+num_edges_solid_macro_t0+num_edges_dashed_macro_t0+num_edges_invaliddashed_macro_t0+meanWeightEdges_macro_t0+reciprocity_macro_t0+assortativity_valence_macro_t0+assortativityDegree_macro_t0,
               data = dat_wide, family = binomial)
summary(glm.fit)
































pred_lavaan <- lavPredict(fit, method = "ML") # =Bartlett scores
dat_long$fc_panas_neg <- pred_lavaan


hist(dat_long$fc_panas_neg)
# dat_long[dat_long$fc_panas_neg < -6, c("panas.01n",
#                                        "panas.02n",
#                                        "panas.03n",
#                                        "panas.04n",
#                                        "panas.05n",
#                                        "panas.06n",
#                                        "panas.07n",
#                                        "panas.08n",
#                                        "panas.09n",
#                                        "panas.10n")]

mod_lavaan <- model_lavaan(vars = str_subset(string = colnames(dat_long), pattern = "panas.*p$"))

dat_long[,c("panas.01p",
            "panas.02p",
            "panas.03p",
            "panas.04p",
            "panas.05p",
            "panas.06p",
            "panas.07p",
            "panas.08p",
            "panas.09p",
            "panas.10p")] <-
  lapply(dat_long[,c("panas.01p",
                     "panas.02p",
                     "panas.03p",
                     "panas.04p",
                     "panas.05p",
                     "panas.06p",
                     "panas.07p",
                     "panas.08p",
                     "panas.09p",
                     "panas.10p")], ordered)

fit <- cfa(mod_lavaan, data = dat_long,
           ordered = c("panas.01p",
                       "panas.02p",
                       "panas.03p",
                       "panas.04p",
                       "panas.05p",
                       "panas.06p",
                       "panas.07p",
                       "panas.08p",
                       "panas.09p",
                       "panas.10p"))
fit <- cfa(mod_lavaan, data = dat_long)
summary(fit, standardized = TRUE)
fitmeasures(fit)
semPlot::semPaths(object = fit, what = "std", edge.label.cex = 0.5)

pred_lavaan <- lavPredict(fit, method = "ML") # =Bartlett scores
dat_long$fc_panas_pos <- pred_lavaan
hist(dat_long$fc_panas_pos)

# dat_long[dat_long$fc_panas_pos < -7, c("panas.01p",
#                                        "panas.02p",
#                                        "panas.03p",
#                                        "panas.04p",
#                                        "panas.05p",
#                                        "panas.06p",
#                                        "panas.07p",
#                                        "panas.08p",
#                                        "panas.09p",
#                                        "panas.10p")]
































############################################





fitmeasures_lavaan(lavaan_mod = fit)



dat_wide$fc_ethic <- pred_lavaan


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





# panasP=~panas.01p_t0+panas.02p_t0+panas.03p_t0+panas.04p_t0+panas.05p_t0+panas.06p_t0+panas.07p_t0+panas.08p_t0+panas.09p_t0+panas.10p_t0

model <- '
  # measurement model
 panasN=~panas.01n_t0+panas.02n_t0+panas.03n_t0+panas.04n_t0+panas.05n_t0+panas.06n_t0+panas.07n_t0+panas.08n_t0+panas.09n_t0+panas.10n_t0
'

fit <- cfa(model, data=dat_long)
fit <- sem(model, data = dat_wide)
summary(fit, standardized = TRUE)
semPlot::semPaths(object = fit, what = "std", edge.label.cex = 0.5)

############################################



dat_long$idcodegroup_negative

diff_group <- lme(det ~ 1 + post*treatment,
                  random = ~ 1 + post | ID,
                  data = rct.long,
                  control=list(sigma=1e-10, opt="optim"))

summary(diff_group)







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

(vars_tmp <- colnames(dat_wide)[str_detect(string = colnames(dat_wide), "panas.+p_t0$")])
constructs_list[[1]] <- vars_tmp

(vars_tmp <- colnames(dat_wide)[str_detect(string = colnames(dat_wide), "panas.+p_t1$")])
constructs_list[[2]] <- vars_tmp

(vars_tmp <- colnames(dat_wide)[str_detect(string = colnames(dat_wide), "tam.+_t0$")])
constructs_list[[3]] <- vars_tmp

(vars_tmp <- colnames(dat_wide)[str_detect(string = colnames(dat_wide), "tam.+_t1$")])
constructs_list[[4]] <- vars_tmp


## PANAS positive
result_tmp <- corr_rel_EFA(constructlist = constructs_list,
                           constnum = 1, data = dat_wide, nfacs = 1)
round(x = result_tmp$`Reliability: Cronbach`$alpha.drop, digits = 2)
print(result_tmp$`fit EFA (PAF)`$loadings, cutoff = .4)

result_tmp <- corr_rel_EFA(constructlist = constructs_list,
                           constnum = 2, data = dat_wide, nfacs = 1)

## TAM: all scales
result_tmp <- corr_rel_EFA(constructlist = constructs_list,
                           constnum = 3, data = dat_wide, nfacs = 8)
result_tmp <- corr_rel_EFA(constructlist = constructs_list,
                           constnum = 4, data = dat_wide, nfacs = 1)




##################
# correlation plots
##################

psych::cor.plot(r = cor(dat_wide[, str_detect(string = colnames(dat_wide), pattern = "virtue*.+_t0$")]
                        , use = "pairwise.complete.obs"),
                upper = FALSE, xlas = 2)


##################
# mixed ANOVAs
##################
###
# > mixed anovas
###
fit1 <- afex::aov_car(mean_valence_macro ~ timepoint*idcodegroup + Error(prolific_pid / timepoint),
                      data = dat_long)

fit1a <- afex::aov_ez(id = "prolific_pid", dv = "mean_valence_macro",
                      data = dat_long, between=c("idcodegroup"), within=c("timepoint"))
# partical eta squared
anova(fit1, es = "pes")
# generalized eta squared
fit1a



dat_long <- dat_long[!is.na(dat_long$idcodegroup),]




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

dfvalcor <- data_summary(dat_long, varname="mean_valence_macro",
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
p <- ggplot(dfvalcor, aes(x=timepoint, y=mean_valence_macro, fill=idcodegroup)) +
  geom_bar(stat="identity", color="black",
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean_valence_macro-se, ymax=mean_valence_macro+se), width=.2,
                position=position_dodge(.9)) + ggplot_theme + ylab(label = "average valence assigned to corona concept")
print(p)


###
# > mixed anovas
###
fit1 <- afex::aov_car(num_nodes_macro ~ timepoint*idcodegroup + Error(prolific_pid / timepoint),
                      data = dat_long)

fit1a <- afex::aov_ez(id = "prolific_pid", dv = "num_nodes_macro",
                      data = dat_long, between=c("idcodegroup"), within=c("timepoint"))
# partical eta squared
anova(fit1, es = "pes")
# generalized eta squared
fit1a



dfvalcor <- data_summary(dat_long, varname="num_nodes_macro",
                         groupnames=c("timepoint","idcodegroup"))
dfvalcor

# create ggplot2 object for X
p <- ggplot(dfvalcor, aes(x=timepoint, y=num_nodes_macro, fill=idcodegroup)) +
  geom_bar(stat="identity", color="black",
           position=position_dodge()) +
  geom_errorbar(aes(ymin=num_nodes_macro-se, ymax=num_nodes_macro+se), width=.2,
                position=position_dodge(.9)) + ggplot_theme + ylab(label = "average valence assigned to corona concept")
print(p)




###############################################
boxplot(dat_long$outcomerating_good.01 ~ dat_long$idcodegroup)
boxplot(dat_wide$outcomerating_good.01_t1 ~ dat_wide$idcodegroup_t1)
boxplot(dat_wide$outcomerating_useful.01_t1 ~ dat_wide$idcodegroup_t1)

dat_wide %>%
  group_by(idcodegroup_t1) %>%
  select(outcomerating_useful.01_t1) %>%
  dplyr::summarise(n())
