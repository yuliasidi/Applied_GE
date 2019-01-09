boxcox(lm(value~Gender + Gender:par_pctile,data=college1),lambda=seq(-2,2,by=.1))


depvar.transformed <- yjPower(college1$value, lambda=0.5)

college2<-college1%>%
  mutate(value1=depvar.transformed)

m.col <- lm(value1~Gender + Gender:par_pctile, college2)
summary(m.col)


boxcox(lm(value~Gender + Gender:par_pctile,data=college1),lambda=seq(-2,2,by=.1))

x <- college1%>%
  dplyr::mutate(
    value_pct = 100*(value - lag(value,1))/lag(value,1),
    par_pctile_ind = factor(par_pctile<=25)
    )%>%
  dplyr::filter(between(par_pctile,2,99))
  
  
  x%>%
    ggplot2::ggplot(aes(x=par_pctile,y=value_pct,colour=Gender)) +
    ggplot2::geom_point(size=0.5,alpha=1)
  
fit_gaussian <- glm(value_pct ~ par_pctile + par_pctile:Gender + Gender, data=x)

fit_gamma <- glm(value_pct ~ par_pctile + par_pctile:Gender + Gender, data=x,
                     family = Gamma(link = "inverse"))
  
fit_gaussian_ind <- glm(value_pct ~ par_pctile*par_pctile_ind + par_pctile:Gender + Gender,
                 data=x)
  
fit_gamma_ind <- glm(value_pct ~ par_pctile*par_pctile_ind + par_pctile:Gender + Gender, data=x,
           family = Gamma(link = "inverse"))  

fit%>%broom::tidy()

fit_gamma%>%
  broom::augment()%>%
  ggplot(aes(x=.fitted, y=.resid)) +
  geom_point(aes(colour=Gender)) +
  geom_hline(aes(yintercept=0),linetype=2)+
  theme_bw() +
  theme(legend.position = 'bottom') +
  ylab("Std Residuals") +
  xlab("Fitted Values")

list(fit_gaussian,fit_gamma)%>%purrr::map_df(broom::glance)

fit_gaussian%>%
  broom::augment()%>%
  ggplot(
    aes(x = par_pctile,colour=Gender)) +
  geom_point(aes(y = .fitted)) +
  geom_point(aes(y = value_pct))

fit_gamma%>%
  broom::augment()%>%
  ggplot(
    aes(x = par_pctile,colour=Gender)) +
  geom_point(aes(y = 1/.fitted)) +
  geom_point(aes(y = value_pct))
