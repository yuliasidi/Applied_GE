#covariates
impMethod[ c("tuition") ] <- "2l.contextual.pmm"
predMnames3 <- names(inc_mice%>%
                       dplyr::select(-state, -tuition))
predM[ c("tuition"), predMnames3] <- 2

impMethod[ c("gradrate_r") ] <- "2l.contextual.pmm"
predMnames4 <- names(inc_mice%>%
                       dplyr::select(-state, -gradrate_r))
predM[ c("gradrate_r"), predMnames4] <- 2

impMethod[ c("num_inst_pc") ] <- "2l.contextual.pmm"
predMnames5 <- names(inc_mice%>%
                       dplyr::select(-state, -num_inst_pc))
predM[ c("num_inst_pc"), predMnames5] <- 2

impMethod[ c("dropout_r ") ] <- "2l.contextual.pmm"
predMnames6 <- names(inc_mice%>%
                       dplyr::select(-state, -dropout_r))
predM[ c("dropout_r"), predMnames6] <- 2

impMethod[ c("score_r") ] <- "2l.contextual.pmm"
predMnames7 <- names(inc_mice%>%
                       dplyr::select(-state, -score_r))
predM[ c("score_r"), predMnames7] <- 2

impMethod[ c("frac_worked1416") ] <- "2l.contextual.pmm"
predMnames8 <- names(inc_mice%>%
                        dplyr::select(-state, -frac_worked1416))
predM[ c("frac_worked1416"), predMnames8] <- 2

impMethod[ c("inc_share_1perc") ] <- "2l.contextual.pmm"
predMnames9 <- names(inc_mice%>%
                        dplyr::select(-state, -inc_share_1perc))
predM[ c("inc_share_1perc"), predMnames9] <- 2

impMethod[ c("ccd_pup_tch_ratio") ] <- "2l.contextual.pmm"
predMnames10 <- names(inc_mice%>%
                        dplyr::select(-state, -ccd_pup_tch_ratio))
predM[ c("ccd_pup_tch_ratio"), predMnames10] <- 2

impMethod[ c("crime_violent") ] <- "2l.contextual.pmm"
predMnames11 <- names(inc_mice%>%
                        dplyr::select(-state, -crime_violent))
predM[ c("crime_violent"), predMnames11] <- 2

impMethod[ c("d_tradeusch_pw_1990") ] <- "2l.contextual.pmm"
predMnames12 <- names(inc_mice%>%
                        dplyr::select(-state, -d_tradeusch_pw_1990))
predM[ c("d_tradeusch_pw_1990"), predMnames12] <- 2

impMethod[ c("scap_ski90pcm") ] <- "2l.contextual.pmm"
predMnames13 <- names(inc_mice%>%
                        dplyr::select(-state, -scap_ski90pcm))
predM[ c("scap_ski90pcm"), predMnames13] <- 2

impMethod[ c("mig_inflow") ] <- "2l.contextual.pmm"
predMnames14 <- names(inc_mice%>%
                        dplyr::select(-state, -mig_inflow))
predM[ c("mig_inflow"), predMnames14] <- 2

impMethod[ c("mig_outflow") ] <- "2l.contextual.pmm"
predMnames15 <- names(inc_mice%>%
                        dplyr::select(-state, -mig_outflow))
predM[ c("mig_outflow"), predMnames15] <- 2

impMethod[ c("taxrate") ] <- "2l.contextual.pmm"
predMnames16 <- names(inc_mice%>%
                        dplyr::select(-state, -taxrate))
predM[ c("taxrate"), predMnames16] <- 2
