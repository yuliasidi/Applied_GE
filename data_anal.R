library(dplyr)
library(readxl)
library(reshape2)
library(tidyr)
library(ggplot2)

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


fig2.plot <- ggplot(fig2.1, aes(x=quant, y = value, group = cz_sex)) +
  geom_line(aes(color=cz_sex)) +
  geom_point(aes(color=cz_sex)) +
  theme_bw() +
  xlab("Parent Household Income Quintile") +
  ylab("Percent Employed") +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())
  
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


fig2.new.plot <- ggplot(fig2.new1, aes(x=quant, y = value, group = cz_sex)) +
  geom_line(aes(color=cz_sex)) +
  geom_point(aes(color=cz_sex)) +
  theme_bw() +
  xlab("Parent Household Income Quintile") +
  ylab("Percent Employed") +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())

pdf("fig2_new.pdf")
fig2.new.plot
dev.off()

#check F-M gaps in Boston and Bridgeport

fig.ys.gap <- 
  tidyr::spread(fig2.new1%>%select(-cz_sex),sex,value)%>%
  dplyr::bind_rows(tidyr::spread(fig2.1%>%select(-cz_sex),sex,value))%>%
  dplyr::mutate(gap = Females - Males)
  


fig.ys.gap.plot <- ggplot(fig.ys.gap, aes(x=quant, y = gap, group = czname)) +
  geom_line(aes(color=czname)) +
  geom_point(aes(color=czname)) +
  theme_bw() +
  xlab("Parent Household Income Quintile") +
  ylab("Percent Employed Gap (F-M)") +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())

pdf("fig_ys.pdf")
fig.ys.gap.plot
dev.off()


