setwd('/home/heitor/Área de Trabalho/R Projects/Econometria Aplicada ao R/Séries de Tempo/Séries Temporais Estacionárias')
library(stats)
library(tseries)
library(forecast)
library(tidyverse)
library(plotly)
library(scales)
# tidyverts core
library(tsibble)
library(fable)
library(feasts)
# tidyverts improvements tools
library(tsibbletalk)
library(fable.prophet)
library(fabletools)

# Prelúdio ============================================

pmc <- read_csv('pmc.csv')

lubridate::guess_formats(pmc$Data, 'BY')

# pmc_ts   <- pmc |> as_tsibble(index = Data) #
# pmc$Data <- as.Date(pmc$Data, '%B%Y')        # don't work
# pmc$Data <- pmc$Data |> lubridate::my()     #

pmc$dt <- seq(from = lubridate::ym('2000 Jan'),
			  to   = lubridate::ym('2021 Aug'), # altern, can put length=
			  by   = "month")

pmc <- pmc |>
	dplyr::mutate(dt =
    tsibble::yearmonth(as.character(dt))) |>
	as_tsibble(index = dt)

#pmc$dt <- ts(start = c(2000,1), end = c(2021,8), frequency = 12)
#pmc$dt <- lubridate::make_date(year = 2000, month = 01)

pmc$Data <- NULL
pmc <- pmc |>
	as_tsibble()

interval(pmc)
glimpse(pmc)

ce_br <- pmc |> 
	select('dt', 'Ceará', 'Brasil') |> 
	rename(ce = Ceará,
		   br = Brasil)

# Visu Geral ==========================================

ce_br |> 
	ggplot() +
	geom_line(aes(x=dt, y=ce),
			  color='seagreen4') +
	geom_line(aes(x=dt, y=br),
			  color='deepskyblue3') +
	ylab('Volume de Vendas') +
	xlab('Tempo')

ce_br |> 
	ggplot() +
	geom_line(aes(x=dt, y=ce),
			  color='seagreen4') +
	geom_line(aes(x=dt, y=br),
			  color='deepskyblue3') +
	scale_x_date(limits =
	as.Date(c('2015-01-01', '2021-08-01'))) +
	scale_y_continuous(limits = c(50,130))

ggplotly(ce_br |> 
	filter(dt >=
		   	tsibble::yearmonth('2012 Jan')) |> 
	ggplot() +
	geom_line(aes(x=dt, y=ce),
			  color='seagreen4')+
	geom_line(aes(x=dt, y=br),
			  color='deepskyblue3')+
	scale_y_continuous(limits = c(50,130))+
	ylab('Volume de Vendas') +
	xlab('Tempo'),
	dynamicTicks = T)

#ce_br <- ce_br |> 
#	dplyr::filter(dt >=
#				tsibble::yearmonth('2012 Jan'))

# CE ~~~~~~~~~~~~~~~~~~~~~
ce_br |> gg_season(ce) |> plotly_build()
ce_br |> gg_subseries(ce) |> plotly_build()

# BR ~~~~~~~~~~~~~~~~~~~~~
ce_br |> gg_season(br) |> plotly_build()
ce_br |> gg_subseries(br) |> plotly_build()

## Decomposição clássica
ce_br |> 
	dplyr::select(ce, dt) |>
	model(classical_decomposition(ce,
								  type = "additive")) |>
	components() |>
	autoplot() |>
	labs(title = "Decomposição Adivita por Média Móvel do Índice Volume de Vendas Cearense") 

## Decomposição STL
ce_br |> model( STL(ce ~ trend(window = 21) +
			season(window = 13),
			robust = TRUE)) |>
	components() |>
	autoplot()

# Raíz Unitária com KPSS e PP =========================

## 2000~2004 ---------------------------
# CE ~~~~~~~~~~~~~~~~~~~~~
ce <- ce_br |> 
	dplyr::filter(dt <=
				  	tsibble::yearmonth('2004 Dec')) |>
	features(ce, c(unitroot_kpss, unitroot_pp)) 
# BR ~~~~~~~~~~~~~~~~~~~~~
br <- ce_br |> 
	dplyr::filter(dt <=
				  	tsibble::yearmonth('2004 Dec')) |>
	features(br,c(unitroot_kpss, unitroot_pp)) 

dplyr::bind_rows(list(Ce = ce, Br = br), .id = 'Região')

## 2005~2012 ---------------------------
# CE ~~~~~~~~~~~~~~~~~~~~~
ce <- ce_br |> 
	dplyr::filter(dt >=
				  	tsibble::yearmonth('2005 Jan') &
				  dt <=
				  	tsibble::yearmonth('2012 Dec')) |>
	features(ce, c(unitroot_kpss, unitroot_pp)) 
# BR ~~~~~~~~~~~~~~~~~~~~~
br <- ce_br |> 
	dplyr::filter(dt >=
				  	tsibble::yearmonth('2005 Jan') &
				  dt <=
				  	tsibble::yearmonth('2012 Dec')) |>
	features(br, c(unitroot_kpss, unitroot_pp)) 

dplyr::bind_rows(list(Ce = ce, Br = br), .id = 'Região')

## 2013~2021 ---------------------------
# CE ~~~~~~~~~~~~~~~~~~~~~
ce <- ce_br |> 
	dplyr::filter(dt >=
				  	tsibble::yearmonth('2013 Jan')) |>
	features(ce, c(unitroot_kpss, unitroot_pp))
# BR ~~~~~~~~~~~~~~~~~~~~~
br <- ce_br |> 
	dplyr::filter(dt >=
				  	tsibble::yearmonth('2013 Jan')) |>
	features(br, c(unitroot_kpss, unitroot_pp))

dplyr::bind_rows(list(Ce = ce, Br = br), .id = 'Região')

## Unit Roots -------------------------
# CE ~~~~~~~~~~~~~~~~~~~~~
ce_br |>
	features(ce,c(unitroot_ndiffs,
				  unitroot_nsdiffs))
# BR ~~~~~~~~~~~~~~~~~~~~~
ce_br |>
	features(br,c(unitroot_ndiffs,
				  unitroot_nsdiffs))

## 1 Diferença ------------------------

ce_br <- ce_br |>
	dplyr::mutate(ce_dif1 = difference(ce, lag = 1),
				  br_dif1 = difference(br, lag = 1)) |>
	dplyr::filter(!is.na(ce_dif1))

# CE ~~~~~~~~~~~~~~~~~~~~~
ce_br |>
	ggplot(aes(x=dt, y=ce_dif1))+
	geom_line()
ce_br |>
	dplyr::mutate(ce_dif1 = difference(ce, lag = 1))|>
	gg_season(ce_dif1) |> plotly_build()
ce_br |>
	dplyr::mutate(ce_dif1 = difference(ce, lag = 1))|>
	gg_subseries(ce_dif1) |> plotly_build()

ce_br |> features(ce_dif1,c(unitroot_kpss,
						   unitroot_pp))
ce_br |>
	feasts::ACF(ce_dif1, lag_max = 36) |>
	autoplot()
ce_br |>
	feasts::PACF(ce_dif1, lag_max = 36) |>
	autoplot()
# BR ~~~~~~~~~~~~~~~~~~~~~
ce_br |>
	ggplot(aes(x=dt, y=br_dif1))+
	geom_line()

ce_br |> features(br_dif1,c(unitroot_kpss,
						   unitroot_pp))
ce_br |>
	feasts::ACF(br_dif1, lag_max = 36) |>
	autoplot()
ce_br |>
	feasts::PACF(br_dif1, lag_max = 36) |>
	autoplot()

## 12 Diferença da 1 Diferença --------
ce_br <- ce_br |>
	dplyr::mutate(
		ce_dif112 = difference(ce_dif1, lag = 12),
		br_dif112 = difference(br_dif1, lag = 12)) |>
	dplyr::filter(!is.na(ce_dif112))
# CE ~~~~~~~~~~~~~~~~~~~~~
ce_br |>
	ggplot(aes(x=dt, y=ce_dif112))+
	geom_line()

ce_br |> features(ce_dif112,c(unitroot_kpss,
						   unitroot_pp))
ce_br |>
	feasts::ACF(ce_dif112, lag_max = 24) |>
	autoplot()
ce_br |>
	feasts::PACF(ce_dif112, lag_max = 24) |>
	autoplot()
# BR ~~~~~~~~~~~~~~~~~~~~~
ce_br |>
	ggplot(aes(x=dt, y=br_dif112))+
	geom_line()

ce_br |> features(br_dif112,c(unitroot_kpss,
						   unitroot_pp))
ce_br |>
	feasts::ACF(br_dif112, lag_max = 24) |>
	autoplot()
ce_br |>
	feasts::PACF(br_dif112, lag_max = 24) |>
	autoplot()

## 12 Diferença ----------------------- {
temp <- ce_br |>
	dplyr::mutate(
		ce_dif12 = difference(ce, lag = 12)) |>
	dplyr::filter(!is.na(ce_dif12))

temp |> ggplot(aes(x=dt, y=ce_dif12))+
	geom_line()

temp |> features(ce_dif12,c(unitroot_kpss,
						   unitroot_pp))
temp |>
	feasts::ACF(ce_dif12, lag_max = 36) |>
	autoplot()
temp |>
	feasts::PACF(ce_dif12, lag_max = 36) |>
	autoplot()
rm(temp)
# 12ª dif é horrível, não ajuda ------- } 

# Modelos e Sazonalidade ==============================

# CE ~~~~~~~~~~~~~~~~~~~~~
ce_models <- ce_br |>
	model(ce_1 = ARIMA(ce ~ 0 +
					   	pdq(3,1,3) +
					   	PDQ(1,1,0,
					   		period = "1 year")),
		  ce_11= ARIMA(ce ~ 1 +
					   	pdq(3,1,3) +
					   	PDQ(1,1,0,
					   		period = "1 year")),
		  ce_2 = ARIMA(ce ~ 0 +
					   	pdq(3,1,3) +
					   	PDQ(0,1,1,
					   		period = "1 year")),
		  ce_3 = ARIMA(ce ~ 0 +
		  			 	pdq(3,1,3) +
		  			 	PDQ(1,1,1,
		  			 		period = "1 year")),
		  ce_4 = ARIMA(ce ~ 0 +
		  			 	pdq(3,1,0) +
		  			 	PDQ(1,1,0,
		  			 		period = "1 year")),
		  ce_auto = ARIMA(ce,
		  				stepwise = F,
		  				approximation = F,
		  				order_constraint =p+q+P+Q<= 8 &
		  					(constant + d + D <= 3)))

ce_models |>
	pivot_longer(everything(),
				 names_to = "Model name",
				 values_to = "Orders")

glance(ce_models) |> arrange(AICc) |>
	select(.model:BIC)

ce_models <- ce_br |>
	model(ce_1 = ARIMA(ce ~ 0 +
		  			 	pdq(3,1,3) +
		  			 	PDQ(1,1,1,
		  			 		period = "1 year")),
		  ce_auto = ARIMA(ce ~ 0 +
		  					pdq(2,1,2) +
		  					PDQ(1,1,1,
		  						period = "1 year")))
# BR ~~~~~~~~~~~~~~~~~~~~~
br_models <- ce_br |>
	model(br_1 = ARIMA(br ~ 0 +
					   	pdq(1,1,1) +
					   	PDQ(1,1,1,
					   		period = "1 year")),
		  br_11= ARIMA(br ~ 1 +
					   	pdq(1,1,1) +
					   	PDQ(1,1,1,
					   		period = "1 year")),
		  br_2 = ARIMA(br ~ 0 +
					   	pdq(1,1,4) +
					   	PDQ(1,1,1,
					   		period = "1 year")),
		  br_3 = ARIMA(br ~ 0 +
		  			 	pdq(4,1,1) +
		  			 	PDQ(1,1,1,
		  			 		period = "1 year")),
		  br_4 = ARIMA(br ~ 0 +
		  			 	pdq(4,1,4) +
		  			 	PDQ(1,1,1,
		  			 		period = "1 year")),
		  br_auto = ARIMA(br,
		  				stepwise = F,
		  				approximation = F,
		  				order_constraint =p+q+P+Q<= 8 &
		  					(constant + d + D <= 3)))

br_models |>
	pivot_longer(everything(),
				 names_to = "Modelos",
			    values_to = "Ordens")

glance(br_models) |> arrange(AICc) |>
	select(.model:BIC)

br_models <- ce_br |>
	model(br_1 = ARIMA(br ~ 0 +
		  			 	pdq(4,1,4) +
		  			 	PDQ(1,1,1,
		  			 		period = "1 year")),
		  br_auto = ARIMA(br ~ 0 +
		  				pdq(3,1,4) +
		  				PDQ(0,1,1,
		  					period = "1 year")))

## Via ce_dif112 ------------------------------- {

ce_models_difs <- ce_br |>
	model(ce_1 = ARIMA(ce_dif112 ~ 0 +
					   	pdq(3,0,3) +
					   	PDQ(1,0,1,
					   		period = "1 year")),
		  ce_auto = ARIMA(ce_dif112,
		  				stepwise = F,
		  				approximation = F))
ce_models_difs |>
	pivot_longer(everything(),
				 names_to = "Model name",
				 values_to = "Orders")

glance(ce_models_difs) |>
	arrange(AICc) |>
	select(.model:BIC)
## dá a mesma coisa do primeiro ---------------- }

# Diagnóstico dos Resíduos ============================

## Fitted <> Real ---------------------
# CE ~~~~~~~~~~~~~~~~~~~~~
ggplotly(
	ce_models |>
		dplyr::select(ce_auto) |>
		fitted() |>
		ggplot() +
		geom_line(aes(x=dt, y=.fitted),
				  alpha = .66,
				  size= .8,
				  linetype="twodash") +
		geom_line(aes(x=dt, y=ce_br$ce),
				  color = 'seagreen4',
				  alpha = .75,
				  size= .8))

ce_models |>
	dplyr::select(ce_auto) |>
	fitted() |>
	ggplot() +
	geom_col(aes(x=dt, y=(.fitted - ce_br$ce)), 
			 color='seagreen4')

# BR ~~~~~~~~~~~~~~~~~~~~~
ggplotly(
br_models |>
	dplyr::select(br_auto) |>
	fitted() |>
	ggplot() +
	geom_line(aes(x=dt, y=.fitted),
			  alpha = .66,
			  size  = .8,
			  linetype="twodash") +
	geom_line(aes(x=dt, y=ce_br$br),
			  color = 'deepskyblue3',
			  alpha = .75,
			  size  = .8))

br_models |>
	dplyr::select(br_auto) |>
	fitted() |>
	ggplot() +
	geom_col(aes(x=dt, y=(.fitted - ce_br$br)),
			 color='deepskyblue3')

## Diagnósticos ---------------------
# CE ~~~~~~~~~~~~~~~~~~~~~
ce_models |> dplyr::select(ce_1) |>	gg_tsresiduals()
ce_models |> dplyr::select(ce_auto) |> gg_tsresiduals()

resid <- ce_models |>
	residuals() |>
	dplyr::filter(.model == "ce_1") |>
	as_tsibble()
jarque.bera.test(resid$.resid) # normalidade
ljung_box(resid$.resid)        # autocorrelação
stat_arch_lm(resid$.resid)     # heterocedasticidade

resid <- ce_models |>
	residuals() |>
	dplyr::filter(.model == "ce_auto") |>
	as_tsibble()
jarque.bera.test(resid$.resid) # normalidade
ljung_box(resid$.resid)        # autocorrelação
stat_arch_lm(resid$.resid)     # heterocedasticidade

ce_models |>
	dplyr::select(ce_auto) |>
	gg_arma()

# BR ~~~~~~~~~~~~~~~~~~~~~
br_models |> dplyr::select(br_1) |>	gg_tsresiduals()
br_models |> dplyr::select(br_auto) |> gg_tsresiduals()

resid <- br_models |>
	residuals() |>
	dplyr::filter(.model == "br_1") |>
	as_tsibble()
jarque.bera.test(resid$.resid) # normalidade
ljung_box(resid$.resid)        # autocorrelação
stat_arch_lm(resid$.resid)     # heterocedasticidade

resid <- br_models |>
	residuals() |>
	dplyr::filter(.model == "br_auto") |>
	as_tsibble()
jarque.bera.test(resid$.resid) # normalidade
ljung_box(resid$.resid)        # autocorrelação
stat_arch_lm(resid$.resid)     # heterocedasticidade

br_models |>
	dplyr::select(br_auto) |>
	gg_arma()

# Previsões ===========================================

## Previsões somente ------------------
# CE ~~~~~~~~~~~~~~~~~~~~~
prev_ce <- ce_models |> 
	dplyr::select(ce_auto) |>
	fabletools::forecast(h=16)

time_prev <- seq(as.Date("2001-02-01"),
				 as.Date("2022-12-31"),
				 by = "1 month") |> yearmonth()

prev_ce |> autoplot(ce_br)

ggplotly(
	ggplot() +
		geom_line(aes(
			x=time_prev,
			y= c(ce_br$ce, prev_ce$.mean)),
			color='seagreen4') +
		geom_line(aes(x=prev_ce$dt,
					  y=prev_ce$.mean)) +
		labs(title = 'Previsão Ceará: ARIMA(2,1,2)(1,1,1)[12]',
			 x= 'Tempo',
			 y='Índice Vomule de Vendas'))

# BR ~~~~~~~~~~~~~~~~~~~~~
prev_br <- br_models |> 
	dplyr::select(br_auto) |>
	forecast(h=16)

prev_br |> autoplot(ce_br)

ggplotly(
	ggplot() +
		geom_line(aes(
			x=time_prev,
			y= c(ce_br$br, prev_br$.mean)),
			color='deepskyblue3') +
		geom_line(aes(x=prev_br$dt,
					  y=prev_br$.mean)) +
		labs(title = 'Previsão Brasil: ARIMA(3,1,4)(0,1,1)[12]',
			 x= 'Tempo',
			 y='Índice Vomule de Vendas'))
