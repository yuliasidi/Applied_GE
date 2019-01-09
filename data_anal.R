library(dplyr)
library(readxl)
library(reshape2)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(estimatr)

gender_cz <- readxl::read_excel("gender_cz.xls")
gender_nat <- readxl::read_excel("gender_nat.xls")

#remove weird last line in the data, that has no CZ
gender_cz <- gender_cz%>%
  dplyr::filter(is.na(cz)==F)


####################
#       Q1         #
####################

#Replicate figure 2
fig2 <- gender_cz%>%
  dplyr::filter(czname%in%c("New York","Charlotte"))%>%
  dplyr::select(czname,
                w2_pos_30_q1_f,w2_pos_30_q2_f,w2_pos_30_q3_f,w2_pos_30_q4_f,w2_pos_30_q5_f,
                w2_pos_30_q1_m,w2_pos_30_q2_m,w2_pos_30_q3_m,w2_pos_30_q4_m,w2_pos_30_q5_m)

fig2.1 <- fig2%>%
  tidyr::gather("var","value",2:11)%>%
  mutate(sex = case_when(grepl("f",var) ~ 'Females',
                         grepl("m",var) ~ 'Males'),
         quant = case_when(grepl("q1",var) ~ "Q1",
                           grepl("q2",var) ~ "Q2",
                           grepl("q3",var) ~ "Q3",
                           grepl("q4",var) ~ "Q4",
                           grepl("q5",var) ~ "Q5"))%>%
  dplyr::mutate(cz_sex = paste(sex,czname, sep = ", "))%>%
  dplyr::select(-var)


fig2.plot <- ggplot(fig2.1, aes(x=quant, y = round(100*value,0), group = cz_sex)) +
  geom_line(aes(color=cz_sex)) +
  geom_point(aes(color=cz_sex, shape = cz_sex)) +
  theme_bw() +
  xlab("Parent Household Income Quintile") +
  ylab("Percent Employed") +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
  guides(col = guide_legend(nrow=2))+
  scale_y_continuous(breaks = seq(60,90,10)) 

pdf("fig2.pdf")
fig2.plot
dev.off()

#Replicate figure 2 for Boston vs Bridgeport
fig2.new <- gender_cz%>%
  dplyr::filter(czname%in%c("Boston","Bridgeport"))%>%
  dplyr::select(czname,
                w2_pos_30_q1_f,w2_pos_30_q2_f,w2_pos_30_q3_f,w2_pos_30_q4_f,w2_pos_30_q5_f,
                w2_pos_30_q1_m,w2_pos_30_q2_m,w2_pos_30_q3_m,w2_pos_30_q4_m,w2_pos_30_q5_m)

fig2.new1 <- fig2.new%>%
  tidyr::gather("var","value",2:11)%>%
  mutate(sex = case_when(grepl("f",var) ~ 'Females',
                         grepl("m",var) ~ 'Males'),
         quant = case_when(grepl("q1",var) ~ "Q1",
                           grepl("q2",var) ~ "Q2",
                           grepl("q3",var) ~ "Q3",
                           grepl("q4",var) ~ "Q4",
                           grepl("q5",var) ~ "Q5"))%>%
  dplyr::mutate(cz_sex = paste(sex,czname, sep = ", "))%>%
  dplyr::select(-var)


fig2.new.plot <- ggplot(fig2.new1, aes(x=quant, y = round(100*value,0), group = cz_sex)) +
  geom_line(aes(color=cz_sex)) +
  geom_point(aes(color=cz_sex, shape = cz_sex)) +
  theme_bw() +
  xlab("Parent Household Income Quintile") +
  ylab("Percent Employed") +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
  guides(col = guide_legend(nrow=2)) +
  scale_y_continuous(breaks = seq(60,90,10)) 

pdf("fig2_new.pdf")
fig2.new.plot
dev.off()

#check F-M gaps in Boston and Bridgeport

fig.ys.gap <- 
  tidyr::spread(fig2.new1%>%select(-cz_sex),sex,value)%>%
  dplyr::bind_rows(tidyr::spread(fig2.1%>%select(-cz_sex),sex,value))%>%
  dplyr::mutate(gap = Males - Females)
  


fig.ys.gap.plot <- ggplot(fig.ys.gap, aes(x=quant, y = round(100*gap,1), group = czname)) +
  geom_line(aes(color=czname)) +
  geom_point(aes(color=czname, shape = czname)) +
  theme_bw() +
  xlab("Parent Household Income Quintile") +
  ylab("Percent Employed Gap (M-F)") +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())

pdf("fig_ys.pdf")
fig.ys.gap.plot
dev.off()

####################
#       Q2         #
####################

#Replicate figure 4b
fig4b <- gender_nat%>%
  dplyr::select(par_pctile,
                kid_indv_rank_30_m,kid_indv_rank_30_f)


fig4b.1 <- fig4b%>%
  tidyr::gather("var","value",2:3)%>%
  mutate(sex = case_when(grepl("f",var) ~ 'Females',
                         grepl("m",var) ~ 'Males'))%>%
  dplyr::select(-var)

fig4b.add <- fig4b%>%
  dplyr::filter(par_pctile%in%c(10,50,90))%>%
  dplyr::mutate(Difference = paste0(round(100*(kid_indv_rank_30_m - kid_indv_rank_30_f),1),'%'))%>%
  dplyr::mutate(`Male-Female` = case_when(par_pctile==10 ~ 'Parent p10',
                              par_pctile==50 ~ 'Parent p50',
                              par_pctile==90 ~ 'Parent p90'))%>%
  dplyr::select(`Male-Female`,Difference)

fig4b.plot <- 
  ggplot(fig4b.1, aes(x=par_pctile, y = round(100*value,2), group = sex)) +
  geom_point(aes(color=sex, shape=sex)) +
  theme_bw() +
  xlab("Parent Household Income Percentile") +
  ylab("Individual Income Percentile") +
  theme(legend.position="bottom") +
  scale_y_continuous(breaks = seq(30,70,10)) +
  theme(legend.title=element_blank()) +
  annotation_custom(tableGrob(fig4b.add, rows=NULL), xmin=75, xmax=100, ymin=35, ymax=45)

pdf("fig4b.pdf")
fig4b.plot
dev.off()

####################
#       Q3         #
####################

#national level data
college <- gender_nat%>%
  dplyr::select(par_pctile,coll1823_m,coll1823_f)%>%
  dplyr::mutate(gap = 100*(coll1823_m-coll1823_f),
                coll_f = 100*coll1823_f,
                coll_m = 100*coll1823_m,
                par_pctile2 = par_pctile^2)

#Regression model for girls
fit_coll_f <- lm(coll_f ~ par_pctile, college)
fit_coll_f.plot <- fit_coll_f%>%
  broom::augment()%>%
  ggplot(aes(x=.fitted, y=.resid)) +
  geom_point() +
  geom_hline(aes(yintercept=0),linetype=2)+
  theme_bw()+
  ylab("Residuals") +
  xlab("Fitted Values")

pdf("college_rates_model1_f.pdf")
fit_coll_f.plot 
dev.off()

#Regression model for boys
fit_coll_m <- lm(coll_m ~ par_pctile, college)
fit_coll_m.plot <- fit_coll_m%>%
  broom::augment()%>%
  ggplot(aes(x=.fitted, y=.resid)) +
  geom_point() +
  geom_hline(aes(yintercept=0),linetype=2)+
  theme_bw()+
  ylab("Residuals") +
  xlab("Fitted Values")

pdf("college_rates_model1_m.pdf")
fit_coll_m.plot 
dev.off()

#Plot gap
col_gap_PHIP <- college%>%
  ggplot(aes(x=par_pctile, y=gap)) +
  geom_point() +
  theme_bw() +
  xlab("Parent Household Income Percentile") +
  ylab("Percent Attended College (M-F)")

pdf("college_gap.pdf")
col_gap_PHIP
dev.off()


#Regression model for gap wihtout quadratic term
fit_coll_gap1 <- lm(gap ~ par_pctile, college)
fit_coll_gap1.plot <- 
fit_coll_gap1%>%
  broom::augment()%>%
  ggplot(aes(x=.fitted, y=.std.resid)) +
  geom_point() +
  geom_hline(aes(yintercept=0),linetype=2)+
  theme_bw()+
  ylab("Residuals") +
  xlab("Fitted Values")

pdf("college_gap_m1.pdf")
fit_coll_gap1.plot
dev.off()

#Regression model for gap wiht quadratic term
fit_coll_gap2 <- lm(gap ~ par_pctile + par_pctile2, college)
fit_coll_gap2.plot <- 
fit_coll_gap2%>%
  broom::augment()%>%
  ggplot(aes(x=.fitted, y=.std.resid)) +
  geom_point() +
  geom_hline(aes(yintercept=0),linetype=2)+
  theme_bw()+
  ylab("Residuals") +
  xlab("Fitted Values")

pdf("college_gap_m2.pdf")
fit_coll_gap2.plot
dev.off()

#F-test to select a model
anova(fit_coll_gap1,fit_coll_gap2)

#Fit regression line from M2
fit_coll_gap2_line.plot<-
fit_coll_gap2%>%
  broom::augment()%>%
  ggplot(
    aes(x = par_pctile)) +
  geom_line(aes(y = .fitted), size=2, color="blue") +
  geom_point(aes(y = gap)) +
  theme_bw() +
  xlab("Parent Household Income Percentile") +
  ylab("Percent Attended College (M-F)")


pdf("college_gap_m2_fitted.pdf")
fit_coll_gap2_line.plot
dev.off()

col_gap2_res <- 
  fit_coll_gap2%>%
  broom::tidy()%>%
  dplyr::rename(Variable=term, Estimate=estimate, Std = std.error, 
                Statistic = statistic, Pvalue=p.value)

xtable(col_gap2_res)

summary(fit_coll_gap2)

####################
#       Q4         #
####################

eincome <- gender_cz%>%
  dplyr::mutate(eincome.gap = e_rank_b_kir26_m_p25  - )






######################
# Not required by HB #
######################

#Replicate figure 1
fig1 <- gender_nat%>%
  dplyr::select(par_pctile,
                w2_pos_30_m,w2_pos_30_f)


fig1.1 <- fig1%>%
  tidyr::gather("var","value",2:3)%>%
  mutate(sex = case_when(grepl("f",var) ~ 'Females',
                         grepl("m",var) ~ 'Males'))%>%
  dplyr::select(-var)

fig1.add <- fig1%>%
  dplyr::filter(par_pctile%in%c(10,50,90))%>%
  dplyr::mutate(Difference = paste0(round(100*(w2_pos_30_m - w2_pos_30_f),1),'%'))%>%
  dplyr::mutate(`Male-Female` = case_when(par_pctile==10 ~ 'Parent p10',
                                          par_pctile==50 ~ 'Parent p50',
                                          par_pctile==90 ~ 'Parent p90'))%>%
  dplyr::select(`Male-Female`,Difference)

fig1.plot <- 
  ggplot(fig1.1, aes(x=par_pctile, y = round(100*value,2), group = sex)) +
  geom_point(aes(color=sex, shape=sex)) +
  theme_bw() +
  xlab("Parent Household Income Percentile") +
  ylab("Percent Employed") +
  theme(legend.position="bottom") +
  scale_y_continuous(breaks = seq(50,90,10)) +
  theme(legend.title=element_blank()) +
  annotation_custom(tableGrob(fig1.add, rows=NULL), xmin=75, xmax=100, ymin=60, ymax=70)

pdf("fig1.pdf")
fig1.plot
dev.off()


#Replicate figure 4c
fig4c <- gender_nat%>%
  dplyr::select(par_pctile,
                coll1823_m,coll1823_f)


fig4c.1 <- fig4c%>%
  tidyr::gather("var","value",2:3)%>%
  mutate(sex = case_when(grepl("f",var) ~ 'Females',
                         grepl("m",var) ~ 'Males'))%>%
  dplyr::select(-var)

fig4c.add <- fig4c%>%
  dplyr::filter(par_pctile%in%c(10,50,90))%>%
  dplyr::mutate(Difference = paste0(round(100*(coll1823_m - coll1823_f),1),'%'))%>%
  dplyr::mutate(`Male-Female` = case_when(par_pctile==10 ~ 'Parent p10',
                                          par_pctile==50 ~ 'Parent p50',
                                          par_pctile==90 ~ 'Parent p90'))%>%
  dplyr::select(`Male-Female`,Difference)

fig4c.plot <- 
  ggplot(fig4c.1, aes(x=par_pctile, y = round(100*value,2), group = sex)) +
  geom_point(aes(color=sex, shape=sex)) +
  theme_bw() +
  xlab("Parent Household Income Percentile") +
  ylab("Percent Attended College") +
  theme(legend.position="bottom") +
  scale_y_continuous(breaks = seq(20,100,10)) +
  theme(legend.title=element_blank()) +
  annotation_custom(tableGrob(fig4c.add, rows=NULL), xmin=75, xmax=100, ymin=30, ymax=50)

pdf("fig4c.pdf")
fig4c.plot
dev.off()





################

#Appendix Table 3-Panel A
tmp <- gender_cz%>%
  dplyr::select(cz, state_id, pop2000, 
                w2_pos_30_q1_m, w2_pos_30_q1_f, cs_race_bla_st,
                cs00_seg_inc_pov25_st, cs_fam_wkidsinglemom_st,
                e_rank_b_kir26_f_p25, e_rank_b_kir26_m_p25)%>%
  dplyr::mutate(gap.A= 100*(w2_pos_30_q1_m - w2_pos_30_q1_f),
                gap.B=(e_rank_b_kir26_m_p25-e_rank_b_kir26_f_p25))

summary(lm_robust(gap.A ~ cs_race_bla_st, 
                  data = tmp, 
                  clusters = state_id, 
                  weights = pop2000, 
                  se_type = "stata"))
summary(lm_robust(gap.A ~ cs00_seg_inc_pov25_st, 
                  data = tmp, 
                  clusters = state_id, 
                  weights = pop2000, 
                  se_type = "stata"))
summary(lm_robust(gap.A ~ cs_fam_wkidsinglemom_st, 
                  data = tmp, 
                  clusters = state_id, 
                  weights = pop2000, 
                  se_type = "stata"))

#Appendix Table 3-Panel B
summary(lm_robust(gap.B ~ cs_race_bla_st, 
                  data = tmp, 
                  clusters = state_id, 
                  weights = pop2000, 
                  se_type = "stata"))


test <- gender_nat%>%
  select(par_pctile,coll1823_f)%>%
  filter(par_pctile==20)

test1 <-gender_cz%>%
  select(czname, state_id, gradrate_r)

library(foreign)
mydata <- read.dta("../replicate/data/mydata.dta")
