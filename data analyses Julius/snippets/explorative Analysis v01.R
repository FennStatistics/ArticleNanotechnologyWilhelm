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


### Daten
# Read the text file from JATOS ...
dir()


dat_wide <- read.xlsx(file = "dat_wide_merged_final.xlsx", sheetIndex = 1)
dat_long <- read.xlsx(file = "dat_long_merged_final.xlsx", sheetIndex = 1)

dat_wide$sociodemo_age_t0 <- as.numeric(dat_wide$sociodemo_age_t0)

### remove 15 with no data at t1 (time point 2)
dat_long <- dat_long[!is.na(dat_long$idcodegroup),]


##################
# Datenaufbereitung long data only
##################
### TAM canx
dat_long$tam_canx.02 <- 8 - dat_long$tam_canx.02
dat_long$tam_canx.03 <- 8 - dat_long$tam_canx.03
dat_long$tam_canx.04 <- 8 - dat_long$tam_canx.04

### remove TAM pec 04
dat_long$tam_pec.04 <- NULL

### TAM vol
dat_long$tam_vol.01 <- 8 - dat_long$tam_vol.01
dat_long$tam_vol.02 <- 8 - dat_long$tam_vol.02
dat_long$tam_vol.03 <- 8 - dat_long$tam_vol.03



############################################################################
# correlation plots
############################################################################


dat_long$tam_mean <- rowMeans(dat_long[, str_detect(string = colnames(dat_long), pattern = "^tam_.+")])


dev.off()
psych::cor.plot(r = cor(dat_long[, str_detect(string = colnames(dat_long), pattern = "^tam_.+")]
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







#######################
###
# > check assumptions
###
### Shapiro-Wilk test of normality
dat_long %>%
  group_by(timepoint, idcodegroup) %>%
  shapiro_test(mean_valence_macro)

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
  data_sum <- plyr::rename(data_sum, c("mean" = varname))
  return(data_sum)
}

dfvalcor <- data_summary(data = dat_long, varname="mean_valence_macro",
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
###############################################
###############################################
# dev.off()
# boxplot(dat_long$outcomerating_good.01 ~ dat_long$idcodegroup)
# boxplot(dat_wide$outcomerating_good.01_t1 ~ dat_wide$idcodegroup_t1)
# boxplot(dat_wide$outcomerating_useful.01_t1 ~ dat_wide$idcodegroup_t1)

# dat_wide %>%
#   group_by(idcodegroup_t1) %>%
#   select(outcomerating_useful.01_t1) %>%
#   dplyr::summarise(n())






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


# sum(str_detect(string = colnames(dat_wide), pattern = "panas"))
# sum(str_detect(string = colnames(dat_wide), pattern = "conflictman"))
# sum(str_detect(string = colnames(dat_wide), pattern = "outcomerating|outcomedummy"))
# sum(str_detect(string = colnames(dat_wide), pattern = "sociodemo"))
# str_subset(string = colnames(dat_wide), pattern = "sociodemo")
