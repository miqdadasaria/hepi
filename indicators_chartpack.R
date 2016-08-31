# Data to go into indicators dashboards and underpinning slide packs
# 
# Author: Miqdad Asaria
# Date: March 2015
###############################################################################

memory.limit(22000)

library(MASS)
library(ggplot2)
library(grid)
library(scales)
library(gridExtra)
library(dplyr)
library(tidyr)
library(xtable)


extract_indicator_data_from_database = function(geo, cached){
	if(geo=="CCG"){
		cache_file_name = "data/indicators_db_data_ccg.RData"
	} else if(geo=="LAT") {
		cache_file_name = "data/indicators_db_data_lat.RData"
	} else if(geo=="LAD") {
		cache_file_name = "data/indicators_db_data_lad.RData"
	} else if(geo=="UTLA") {
		cache_file_name = "data/indicators_db_data_utla.RData"
	}
	
	if(cached){
		load(cache_file_name)
	} else {
		source("../db_connection.R")
		con = get_db_connection()
		if(geo=="CCG"){
			geo_sql = "ccg13nm"
		} else if(geo=="LAT") {
			geo_sql = "nhsat13nm AS"
		} else if(geo=="LAD") {
			geo_sql = "lad11nm AS"
		} else if(geo=="UTLA") {
			geo_sql = "utlanm AS"
		}
		sql = paste("SELECT year, ind.lsoa11cd, " ,geo_sql," GEOGRAPHY, indicator, indicator_value, normalised_rank, \'Q\'||quintile AS quintile, \'D\'||decile AS decile 
				FROM 
				HEPI_INDICATORS_LSOA_2011 ind
				INNER JOIN 
				IMD_2010_LSOA_2011 imd
				ON ind.lsoa11cd=imd.lsoa11cd
				INNER JOIN 
				NHS_GEOGRAPHY geo
				ON ind.lsoa11cd=geo.lsoa11cd
				WHERE YEAR>2000 AND YEAR<2012",sep="")
		lsoa_data = dbGetQuery(con,sql)
		
		sql2 = "SELECT 
				INDICATOR,
				YEAR,
				CASE 
				WHEN AGE_GROUP = '5-14' THEN '5-15'
				WHEN AGE_GROUP = '15-24' THEN '16-24'
				ELSE AGE_GROUP 
				END AS AGE_GROUP,
				IMD_GROUP,
				SEX,
				INDICATOR_VALUE
 				FROM HEPI_INDICATORS
				WHERE YEAR>2000 AND YEAR<2012
				AND age_group!='ALL'
				AND imd_group!='ALL' 
				AND sex!='A'
				AND age_group!='U'
				AND imd_group!='U' 
				AND imd_group!='Q' 
				AND sex!='U'"
		matrix_data = dbGetQuery(con,sql2)
		
		dbDisconnect(con)
		
		indicators_list = c("population",
				"need_adjusted_population",
				"amenable_mortality",
				"amenable_mortality_adjusted",
				"all_mortality",
				"all_mortality_adjusted",
				"gp_fte",
				"hosp_discharge_failure",
				"hosp_discharge_failure_adjusted",
				"hosp_discharge_failure_no_death",
				"hosp_discharge_failure_no_death_adjusted",
				"inpatient_wait",
				"inpatient_wait_adjusted",
				"cips_count_ind_inpatient_wait",
				"cips_count_ind",
				"unplanned_hospitalisations",
				"unplanned_hospitalisations_adjusted",
				"morbidity",
				"PHIS",
				"hospital_deaths")
		
		lsoa_data_wide = lsoa_data %>% filter(INDICATOR %in% indicators_list) %>% spread(INDICATOR, INDICATOR_VALUE)
		
		matrix_data_wide = matrix_data %>% filter(INDICATOR %in% indicators_list) %>% spread(INDICATOR, INDICATOR_VALUE)
		
		data = list()
		data[["lsoa_data_wide"]] = lsoa_data_wide
		data[["matrix_data_wide"]] = matrix_data_wide
		save(data,file=cache_file_name)
	}
	return(data)
}

aggregate_indicator_data = function(lsoa_data_wide, matrix_data_wide){
	indicators = list()
	
# deprecated in favour of patients per GP FTE
#	gp_fte = list()
#	gp_fte[["indicator_data"]] = lsoa_data_wide %>% 
#			select(YEAR,LSOA11CD,GEOGRAPHY,NORMALISED_RANK,QUINTILE,DECILE, numerator=gp_fte, exposure=population) %>% 
#			mutate(denominator=exposure/100000, lsoa_indicator=numerator/denominator) %>%
#			filter(YEAR>2003)
#	gp_fte[["indicator_label"]] = "Full time equivalent GPs (per 100,000)"
#	gp_fte[["title"]] = "Indicator 1. GP supply (unadjusted)"
#	gp_fte[["footnote"]] = "Indicator 1. GP supply: Full time equivalent GPs per 100,000, excluding registrars and retainers, adjusted for age, sex and health deprivation"
#	gp_fte[["slope_sign"]] = 1
#	indicators[["gp_fte"]] = gp_fte
#
	gp_fte_adj = list()
	gp_fte_adj[["indicator_data"]] = lsoa_data_wide %>% 
			select(YEAR,LSOA11CD,GEOGRAPHY,NORMALISED_RANK,QUINTILE,DECILE, numerator=gp_fte, exposure=need_adjusted_population) %>% 
			mutate(denominator=exposure/100000, lsoa_indicator=numerator/denominator) %>%
			filter(YEAR>2003)
	gp_fte_adj[["indicator_label"]] = "Full time equivalent GPs (per 100,000)"
	gp_fte_adj[["title"]] = "Indicator 1. GP supply"
	gp_fte_adj[["footnote"]] = "Indicator 1. GP supply: Full time equivalent GPs per 100,000, excluding registrars and retainers, adjusted for age, sex and health deprivation"
	gp_fte_adj[["slope_sign"]] = 1
	indicators[["gp_fte_adj"]] = gp_fte_adj

	pat_gp_fte = list()
	pat_gp_fte[["indicator_data"]] = lsoa_data_wide %>% 
			select(YEAR,LSOA11CD,GEOGRAPHY,NORMALISED_RANK,QUINTILE,DECILE, numerator=population, denominator=gp_fte) %>% 
			mutate(exposure=numerator, lsoa_indicator=numerator/denominator) %>%
			filter(YEAR>2003, denominator>0)
	pat_gp_fte[["indicator_label"]] = "Patients per full time equivalent GP"
	pat_gp_fte[["title"]] = "Indicator 1. Primary Care Supply (unadjusted)"
	pat_gp_fte[["footnote"]] = "Indicator 1. Primary Care Supply: Patients per full time equivalent GP, excluding registrars and retainers"
	pat_gp_fte[["slope_sign"]] = -1
	indicators[["pat_gp_fte"]] = pat_gp_fte
	
	pat_gp_fte_adj = list()
	pat_gp_fte_adj[["indicator_data"]] = lsoa_data_wide %>% 
			select(YEAR,LSOA11CD,GEOGRAPHY,NORMALISED_RANK,QUINTILE,DECILE, numerator=need_adjusted_population, denominator=gp_fte) %>% 
			mutate(exposure=numerator, lsoa_indicator=numerator/denominator) %>%
			filter(YEAR>2003, denominator>0)
	pat_gp_fte_adj[["indicator_label"]] = "Patients per full time equivalent GP"
	pat_gp_fte_adj[["title"]] = "Indicator 1. Primary Care Supply"
	pat_gp_fte_adj[["footnote"]] = "Indicator 1. Primary Care Supply: Patients per full time equivalent GP, excluding registrars and retainers, adjusted for age, sex and health deprivation"
	pat_gp_fte_adj[["slope_sign"]] = -1
	indicators[["pat_gp_fte_adj"]] = pat_gp_fte_adj
	
	PHIS = list()
	PHIS[["indicator_data"]] = lsoa_data_wide %>% 
			select(YEAR,LSOA11CD,GEOGRAPHY,NORMALISED_RANK,QUINTILE,DECILE, numerator=PHIS, denominator=population) %>% 
			mutate(numerator=numerator*denominator, exposure=denominator*100, lsoa_indicator=numerator/denominator) %>%
			filter(YEAR>2003)
	PHIS[["indicator_label"]] = "Public health impact score"
	PHIS[["title"]] = "Indicator 2. Primary Care Quality"
	PHIS[["footnote"]] = "Indicator 2. Primary Care Quality: clinical performance in the quality and outcomes framework (weighted by public health impact)"
	PHIS[["slope_sign"]] = 1
	indicators[["PHIS"]] = PHIS
	
 	waiting_time = list()
 	waiting_time[["indicator_data"]] = lsoa_data_wide %>% 
 			select(YEAR,LSOA11CD,GEOGRAPHY,NORMALISED_RANK,QUINTILE,DECILE, numerator=inpatient_wait, denominator=cips_count_ind_inpatient_wait) %>% 
 			mutate(lsoa_indicator=numerator/denominator, exposure=1) %>%
 			filter(YEAR<2012)
 	waiting_time[["matrix_data"]] = matrix_data_wide %>% 
 			select(YEAR,IMD_GROUP,AGE_GROUP,SEX, numerator=inpatient_wait, denominator=cips_count_ind_inpatient_wait) %>% 
 			mutate(matrix_indicator=numerator/denominator) %>%
 			filter(YEAR<2012) 
 	waiting_time[["indicator_label"]] = "Mean inpatient wait (days)"
 	waiting_time[["title"]] = "Indicator 3. Hospital Waiting Time (unadjusted)"
 	waiting_time[["footnote"]] = "Indicator 3. Hospital Waiting Time: days from outpatient decision-to-treat to inpatient admission-for-treatment"
 	waiting_time[["slope_sign"]] = -1
 	indicators[["waiting_time"]] = waiting_time
 
	waiting_time_adj = list()
	waiting_time_adj[["indicator_data"]] = lsoa_data_wide %>% 
			select(YEAR,LSOA11CD,GEOGRAPHY,NORMALISED_RANK,QUINTILE,DECILE, numerator=inpatient_wait_adjusted, denominator=cips_count_ind_inpatient_wait) %>% 
			mutate(lsoa_indicator=numerator/denominator, exposure=1) %>%
			filter(YEAR<2012)
	waiting_time_adj[["indicator_label"]] = "Mean inpatient wait (days)"
	waiting_time_adj[["title"]] = "Indicator 3. Hospital Waiting Time"
	waiting_time_adj[["footnote"]] = "Indicator 3. Hospital Waiting Time: days from outpatient decision-to-treat to inpatient admission-for-treatment adjusted for specialty"
	waiting_time_adj[["slope_sign"]] = -1
	indicators[["waiting_time_adj"]] = waiting_time_adj
	
	unplanned_hospitalisations = list()
	unplanned_hospitalisations[["indicator_data"]] = lsoa_data_wide %>% 
			select(YEAR,LSOA11CD,GEOGRAPHY,NORMALISED_RANK,QUINTILE,DECILE, numerator=unplanned_hospitalisations, exposure=population) %>% 
			mutate(denominator=exposure/1000, lsoa_indicator=numerator/denominator) %>%
			filter(YEAR<2012)
	unplanned_hospitalisations[["matrix_data"]] = matrix_data_wide %>% 
			select(YEAR,IMD_GROUP,AGE_GROUP,SEX, numerator=unplanned_hospitalisations, denominator=population) %>% 
			mutate(denominator=denominator/1000, matrix_indicator=numerator/denominator)  %>%
			filter(YEAR<2012)
	unplanned_hospitalisations[["indicator_label"]] = "Preventable hospital admissions (per 1,000)"
	unplanned_hospitalisations[["title"]] = "Indicator 4. Preventable Hospitalisation (unadjusted)"
	unplanned_hospitalisations[["footnote"]] = "Indicator 4. Preventable Hospitalisation: hospitalisations per 1,000 population for conditions amenable to healthcare"
	unplanned_hospitalisations[["slope_sign"]] = -1
	indicators[["unplanned_hospitalisations"]] = unplanned_hospitalisations
	
	unplanned_hospitalisations_adj = list()
	unplanned_hospitalisations_adj[["indicator_data"]] = lsoa_data_wide %>% 
			select(YEAR,LSOA11CD,GEOGRAPHY,NORMALISED_RANK,QUINTILE,DECILE, numerator=unplanned_hospitalisations_adjusted, exposure=population) %>% 
			mutate(denominator=exposure/1000, lsoa_indicator=numerator/denominator) %>%
			filter(YEAR<2012)
	unplanned_hospitalisations_adj[["indicator_label"]] = "Preventable hospital admissions (per 1,000)"
	unplanned_hospitalisations_adj[["title"]] = "Indicator 4. Preventable Hospitalisation"
	unplanned_hospitalisations_adj[["footnote"]] = "Indicator 4. Preventable Hospitalisation: hospitalisations per 1,000 population for conditions amenable to healthcare adjusted for age and sex"
	unplanned_hospitalisations_adj[["slope_sign"]] = -1
	indicators[["unplanned_hospitalisations_adj"]] = unplanned_hospitalisations_adj
	
# deprecated in favour of repeat hospitalisation - mortality link data looks truncated in final year
#	adverse_event = list()
#	adverse_event[["indicator_data"]] = lsoa_data_wide %>% 
#			select(YEAR,LSOA11CD,GEOGRAPHY,NORMALISED_RANK,QUINTILE,DECILE, numerator=hosp_discharge_failure, exposure=cips_count_ind) %>% 
#			mutate(denominator=exposure, lsoa_indicator=numerator/denominator) %>%
#			filter(YEAR<2012)
#	adverse_event[["matrix_data"]] = matrix_data_wide %>% 
#			select(YEAR,IMD_GROUP,AGE_GROUP,SEX, numerator=hosp_discharge_failure, denominator=cips_count_ind) %>% 
#			mutate(denominator=denominator, matrix_indicator=numerator/denominator) %>%
#			filter(YEAR<2012) 
#	adverse_event[["indicator_label"]] = "Proportion of post-hospital adverse events"
#	adverse_event[["title"]] = "Indicator 5. Post-hospital adverse event (unadjusted)"
#	adverse_event[["footnote"]] = "Indicator 5. Post-hospital adverse event: proportion of inpatients with subsequent emergency readmission or death the same year"
#	adverse_event[["slope_sign"]] = -1
#	indicators[["adverse_event"]] = adverse_event
#
#	adverse_event_adj = list()
#	adverse_event_adj[["indicator_data"]] = lsoa_data_wide %>% 
#			select(YEAR,LSOA11CD,GEOGRAPHY,NORMALISED_RANK,QUINTILE,DECILE, numerator=hosp_discharge_failure_adjusted, exposure=cips_count_ind) %>% 
#			mutate(denominator=exposure, lsoa_indicator=numerator/denominator) %>%
#			filter(YEAR<2012)
#	adverse_event_adj[["indicator_label"]] = "Proportion of post-hospital adverse events"
#	adverse_event_adj[["title"]] = "Indicator 5. Post-hospital adverse event"
#	adverse_event_adj[["footnote"]] = "Indicator 5. Post-hospital adverse event: proportion of inpatients with subsequent emergency readmission or death the same year adjusted for age and sex"
#	adverse_event_adj[["slope_sign"]] = -1
#	indicators[["adverse_event_adj"]] = adverse_event_adj

	repeat_hosp = list()
	repeat_hosp[["indicator_data"]] = lsoa_data_wide %>% 
			select(YEAR,LSOA11CD,GEOGRAPHY,NORMALISED_RANK,QUINTILE,DECILE, numerator=hosp_discharge_failure_no_death, exposure=cips_count_ind) %>% 
			mutate(denominator=exposure, lsoa_indicator=numerator/denominator) %>%
			filter(YEAR<2012)
	repeat_hosp[["matrix_data"]] = matrix_data_wide %>% 
			select(YEAR,IMD_GROUP,AGE_GROUP,SEX, numerator=hosp_discharge_failure_no_death, denominator=cips_count_ind) %>% 
			mutate(denominator=denominator, matrix_indicator=numerator/denominator) %>%
			filter(YEAR<2012) 
	repeat_hosp[["indicator_label"]] = "Proportion of repeat hospitalisations"
	repeat_hosp[["title"]] = "Indicator 5. Repeat Hospitalisation (unadjusted)"
	repeat_hosp[["footnote"]] = "Indicator 5. Repeat Hospitalisation: proportion of inpatients with subsequent emergency readmission the same year"
	repeat_hosp[["slope_sign"]] = -1
	indicators[["repeat_hosp"]] = repeat_hosp
	
	repeat_hosp_adj = list()
	repeat_hosp_adj[["indicator_data"]] = lsoa_data_wide %>% 
			select(YEAR,LSOA11CD,GEOGRAPHY,NORMALISED_RANK,QUINTILE,DECILE, numerator=hosp_discharge_failure_no_death_adjusted, exposure=cips_count_ind) %>% 
			mutate(denominator=exposure, lsoa_indicator=numerator/denominator) %>%
			filter(YEAR<2012)
	repeat_hosp_adj[["indicator_label"]] = "Proportion of repeat hospitalisations"
	repeat_hosp_adj[["title"]] = "Indicator 5. Repeat Hospitalisation"
	repeat_hosp_adj[["footnote"]] = "Indicator 5. Repeat Hospitalisation: proportion of inpatients with subsequent emergency readmission the same year adjusted for age and sex"
	repeat_hosp_adj[["slope_sign"]] = -1
	indicators[["repeat_hosp_adj"]] = repeat_hosp_adj
	
	hosp_death = list()
	hosp_death[["indicator_data"]] = lsoa_data_wide %>% 
			select(YEAR,LSOA11CD,GEOGRAPHY,NORMALISED_RANK,QUINTILE,DECILE, numerator=hospital_deaths, exposure=all_mortality) %>% 
			mutate(numerator=ifelse(is.na(numerator),0,numerator), denominator=exposure, lsoa_indicator=numerator/denominator) %>%
			filter(YEAR<2012)
	hosp_death[["matrix_data"]] = matrix_data_wide %>% 
			select(YEAR,IMD_GROUP,AGE_GROUP,SEX, numerator=hospital_deaths, denominator=all_mortality) %>% 
			mutate(denominator=denominator, matrix_indicator=numerator/denominator) %>%
			filter(YEAR<2012) 
	hosp_death[["indicator_label"]] = "Proportion of deaths in hospital" 
	hosp_death[["title"]] = "Indicator 6. Dying in Hospital"
	hosp_death[["footnote"]] = "Indicator 6. Dying in Hospital: proportion of deaths in hospital"
	hosp_death[["slope_sign"]] = -1
	indicators[["hosp_death"]] = hosp_death		
	
	amenable_mortality = list()
	amenable_mortality[["indicator_data"]] = lsoa_data_wide %>% 
			select(YEAR,LSOA11CD,GEOGRAPHY,NORMALISED_RANK,QUINTILE,DECILE, numerator=amenable_mortality, exposure=population) %>% 
			mutate(denominator=exposure/1000, lsoa_indicator=numerator/denominator) %>%
			filter(YEAR<2012) 
	amenable_mortality[["matrix_data"]] = matrix_data_wide %>% 
			select(YEAR,IMD_GROUP,AGE_GROUP,SEX, numerator=amenable_mortality, denominator=population) %>% 
			mutate(denominator=denominator/1000, matrix_indicator=numerator/denominator) %>%
			filter(YEAR<2012)
	amenable_mortality[["indicator_label"]] = "Amenable mortality (per 1,000)"
	amenable_mortality[["title"]] = "Indicator 7. Amenable Mortality (unadjusted)"
	amenable_mortality[["footnote"]] = "Indicator 7. Amenable Mortality: deaths per 1,000 population from causes amenable to health care"
	amenable_mortality[["slope_sign"]] = -1
	indicators[["amenable_mortality"]] = amenable_mortality

	amenable_mortality_adj = list()
	amenable_mortality_adj[["indicator_data"]] = lsoa_data_wide %>% 
			select(YEAR,LSOA11CD,GEOGRAPHY,NORMALISED_RANK,QUINTILE,DECILE, numerator=amenable_mortality_adjusted, exposure=population) %>% 
			mutate(denominator=exposure/1000, lsoa_indicator=numerator/denominator) %>%
			filter(YEAR<2012)
	amenable_mortality_adj[["indicator_label"]] = "Amenable mortality (per 1,000)"
	amenable_mortality_adj[["title"]] = "Indicator 7. Amenable Mortality"
	amenable_mortality_adj[["footnote"]] = "Indicator 7. Amenable Mortality: deaths per 1,000 population from causes amenable to health care adjusted for age and sex"
	amenable_mortality_adj[["slope_sign"]] = -1
	indicators[["amenable_mortality_adj"]] = amenable_mortality_adj
	
	all_mortality = list()
	all_mortality[["indicator_data"]] = lsoa_data_wide %>% 
			select(YEAR,LSOA11CD,GEOGRAPHY,NORMALISED_RANK,QUINTILE,DECILE, numerator=all_mortality, exposure=population) %>% 
			mutate(denominator=exposure/1000, lsoa_indicator=numerator/denominator) 
	all_mortality[["matrix_data"]] = matrix_data_wide %>% 
			select(YEAR,IMD_GROUP,AGE_GROUP,SEX, numerator=all_mortality, denominator=population) %>% 
			mutate(denominator=denominator/1000, matrix_indicator=numerator/denominator)	
	all_mortality[["indicator_label"]] = "All cause mortality (per 1,000)"
	all_mortality[["title"]] = "Indicator 8. Mortality (unadjusted)"
	all_mortality[["footnote"]] = "Indicator 8. Mortality: deaths per 1,000 population from all causes at all ages"
	all_mortality[["slope_sign"]] = -1
	indicators[["all_mortality"]] = all_mortality

	all_mortality_adj = list()
	all_mortality_adj[["indicator_data"]] = lsoa_data_wide %>% 
			select(YEAR,LSOA11CD,GEOGRAPHY,NORMALISED_RANK,QUINTILE,DECILE, numerator=all_mortality_adjusted, exposure=population) %>% 
			mutate(denominator=exposure/1000, lsoa_indicator=numerator/denominator) 
	all_mortality_adj[["indicator_label"]] = "All cause mortality (per 1,000)"
	all_mortality_adj[["title"]] = "Indicator 8. Mortality"
	all_mortality_adj[["footnote"]] = "Indicator 8. Mortality: death rate per 1,000 population adjusted for age and sex"
	all_mortality_adj[["slope_sign"]] = -1
	indicators[["all_mortality_adj"]] = all_mortality_adj

	#	
#	morbidity = list()
#	morbidity[["indicator_data"]] = lsoa_data_wide %>% 
#			select(YEAR,LSOA11CD,GEOGRAPHY,NORMALISED_RANK,QUINTILE,DECILE, numerator=morbidity, exposure=population) %>% 
#			mutate(denominator=exposure, lsoa_indicator=numerator/denominator)  %>%
#			filter(YEAR>2005)
#	morbidity[["indicator_label"]] = "Proportion of population with morbidity"
#	morbidity[["title"]] = "Indicator 9. Morbidity"
#	morbidity[["footnote"]] = "Indicator 9. Morbidity: proportion of people with one or more condition (based on QOF data)"
#	morbidity[["slope_sign"]] = -1
#	indicators[["morbidity"]] = morbidity

	return(indicators)
}


plot_matrix = function(indicator, scale="fixed"){
	point_size=0.5
	line_size=0.5
	year_labels = data.frame(YEAR=2000:2014,YEAR_LABEL=c("00/01","01/02","02/03","03/04","04/05","05/06","06/07","07/08","08/09","09/10","10/11","11/12","12/13","13/14","14/15"), stringsAsFactors=FALSE )
	imd_labels = c("Q1 (most deprived)","Q2","Q3","Q4","Q5 (least deprived)")
	
	indicator_matrix_data = inner_join(indicator[["matrix_data"]],year_labels,by="YEAR")
	indicator_matrix_data$AGE_GROUP = factor(indicator_matrix_data$AGE_GROUP, levels=c("0-4","5-15","16-24","25-39","40-59","60-74","75+"))
	indicator_matrix_data$IMD_GROUP = factor(indicator_matrix_data$IMD_GROUP, levels=paste("Q",1:5,sep=""), labels=imd_labels)
	indicator_matrix_data = indicator_matrix_data %>% filter(!is.na(matrix_indicator) & !is.nan(matrix_indicator))
	matrix = ggplot(indicator_matrix_data) + 
			aes(x=YEAR_LABEL, y=matrix_indicator, group=SEX, colour=SEX) + 
			geom_line(aes(linetype=SEX), size=line_size) + 
			geom_point(aes(colour=SEX), size=point_size) +
			xlab("Year") +
			ylab(indicator[["indicator_label"]]) + 
			ggtitle(paste(indicator[["title"]], sep="")) +
			scale_y_continuous(labels = comma) +
			scale_x_discrete(labels=c(year_labels[year_labels$YEAR==min(indicator_matrix_data$YEAR),2],rep("",max(indicator_matrix_data$YEAR)-min(indicator_matrix_data$YEAR)-1),year_labels[year_labels$YEAR==max(indicator_matrix_data$YEAR),2])) +
			scale_colour_manual(name="Sex", values=c("black","darkgrey"), labels=c("female", "male")) +
			scale_linetype_manual(name="Sex", values=c(1,2), labels=c("female", "male")) +
			theme_bw(base_size = 9) +
			theme(plot.title = element_text(lineheight=.8, face="bold", size=rel(2.3)), panel.grid.major = element_blank(), panel.grid.minor = element_blank() ) +
			facet_grid(AGE_GROUP ~ IMD_GROUP, scales=scale)
		
	matrix_plot = 
			arrangeGrob(matrix, 
					sub = textGrob("Breakdown by age, sex, deprivation and year", x = 0, hjust = -0.1, vjust=0.1,
							gp = gpar(fontface = "italic", fontsize = 10)),
					heights=c(0.90,0.10))
	
	return(matrix_plot) 
}


plot_quintiles = function(indicator_results, indicator, zero_line=FALSE){
	imd_labels = c("Q1 (most deprived)","Q2","Q3","Q4","Q5 (least deprived)")
	year_labels = data.frame(YEAR=2000:2014,YEAR_LABEL=c("00/01","01/02","02/03","03/04","04/05","05/06","06/07","07/08","08/09","09/10","10/11","11/12","12/13","13/14","14/15"))
	graph_data = indicator_results %>% 
			inner_join(year_labels,by="YEAR") %>% 
			gather(IMD_QUINTILE,value,Q1:Q5) %>%
			select(YEAR=YEAR_LABEL,IMD_QUINTILE,value)
	main_plot = ggplot(graph_data) + 
			aes(x=YEAR, y=value, group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
			geom_line(aes(linetype=IMD_QUINTILE, size=IMD_QUINTILE)) + 
			geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE)) +
			xlab("Year") +
			ylab(indicator[["indicator_label"]]) +
			ggtitle(indicator[["title"]]) + 
			scale_y_continuous(labels = comma) +
			scale_colour_manual(name="IMD Group", values=c("black","lightblue","lightblue","lightblue","darkgrey"), labels=imd_labels) +
			scale_shape_manual(name="IMD Group", values=c(19,21,24,0,15), labels=imd_labels) +
			scale_linetype_manual(name="IMD Group", values=c(1,2,2,2,1), labels=imd_labels) +
			scale_size_manual(name="IMD Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
			theme_bw() +
			theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.title = element_text(lineheight=.8, face="bold", size=rel(1.7)))
	
	index_data = indicator_results %>% 
			inner_join(year_labels,by="YEAR") %>% 
			select(YEAR,YEAR_LABEL,RII,RII_LCI,RII_UCI,SII,SII_UCI,SII_LCI)
	
	SII_plot = ggplot(index_data) + 
			aes(x=YEAR, y=SII) + 
			geom_line() + 
			geom_point() +
			geom_errorbar(aes(ymin=SII_LCI, ymax=SII_UCI), width=0.1 ) +
			xlab("Year") +
			ylab("") +
			ggtitle("Slope Index of Inequality") +
			scale_x_continuous(
					breaks=min(index_data$YEAR):max(index_data$YEAR),	
					labels=index_data$YEAR_LABEL)+
			scale_y_continuous(labels = comma) +
			theme_bw() +
			theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
	
	RII_plot = ggplot(index_data) + 
			aes(x=YEAR, y=RII) + 
			geom_line() + 
			geom_point() +
			geom_errorbar(aes(ymin=RII_LCI, ymax=RII_UCI), width=0.1 ) +
			xlab("Year") +
			ylab("") +
			ggtitle("Relative Index of Inequality") +
			scale_x_continuous(
					breaks=min(index_data$YEAR):max(index_data$YEAR),	
					labels=index_data$YEAR_LABEL) +
			scale_y_continuous(labels = percent) +
			theme_bw() +
			theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

	if(zero_line){
		SII_plot = SII_plot + geom_hline(yintercept=0, colour="darkgrey", linetype=2)
		RII_plot = RII_plot + geom_hline(yintercept=0, colour="darkgrey", linetype=2)
	}
		
	
	main_panel = arrangeGrob(
			main_plot,
			arrangeGrob(
					SII_plot,
					RII_plot, 
					ncol=2),
			heights=c(2/3,1/3),
			nrow=2)

	panel_plot = 
			arrangeGrob(main_panel, 
					sub = textGrob(indicator[["footnote"]], x = 0, hjust = -0.1, vjust=0.1,
							gp = gpar(fontface = "italic", fontsize = 10)),
					heights=c(0.90,0.10))
	return(panel_plot)
}

plot_gradient = function(indicator_results, indicator){
	min_year = min(indicator_results$YEAR)
	max_year = max(indicator_results$YEAR)
	graph_data = indicator_results %>% 
			filter(YEAR %in% c(min_year, max_year)) %>% 
			gather(IMD_QUINTILE,value,Q1:Q5) %>%
			select(YEAR,IMD_QUINTILE,value)
	graph_data$YEAR = factor(graph_data$YEAR, levels=c(min_year,max_year), labels=c(paste(min_year,"/",substr((min_year+1),3,4),sep=""),paste(max_year,"/",substr((max_year+1),3,4),sep="")))
	gradient = ggplot(graph_data) +
			aes(x=IMD_QUINTILE, y=value, fill=YEAR) + 
			geom_bar(stat="identity", position=position_dodge()) +
			xlab("Small area deprivation") +
			ylab(indicator[["indicator_label"]]) +
			ggtitle(indicator[["title"]]) + 
			scale_y_continuous(labels = comma) +
			scale_x_discrete(labels=c("Q1 (most deprived)","Q2","Q3","Q4","Q5 (least deprived)")) +
			scale_fill_manual(name="Year", values=c("grey","black")) +
			theme_bw() +
			theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.title = element_text(lineheight=.8, face="bold", size=rel(1.7)))
	gradient_plot = 
			arrangeGrob(gradient, 
					sub = textGrob(indicator[["footnote"]], x = 0, hjust = -0.1, vjust=0.1,
							gp = gpar(fontface = "italic", fontsize = 10)),
					heights=c(0.90,0.10))
		return(gradient_plot)
}


check_significance = function(benchmark,UCI,LCI){
	return((LCI <= benchmark & UCI <= benchmark) | (LCI >= benchmark & UCI >= benchmark))
}

calculate_trend = function(indicator_results, indicator="RII", national=TRUE){
	CI = qnorm(0.975)
	
	if(national){
		join_vars = "YEAR"
	} else {
		indicator_results = indicator_results %>% group_by(GEOGRAPHY)
		join_vars = c("YEAR","GEOGRAPHY")
	}
	
	index = indicator_results %>% select(one_of(c("YEAR",indicator)))
	index_2 = index
	index_2$YEAR = index$YEAR+2
	
	if(indicator %in% c("RII","SII")){
		index_se=indicator_results %>% select(one_of(c("YEAR",indicator,paste(indicator,"_UCI",sep="")))) 
		index_se[,"se"] = (index_se[,paste(indicator,"_UCI",sep="")]-index_se[,indicator])/CI
		index_se= index_se %>% select(one_of(c("YEAR","se")))
	} else {
		index_se = indicator_results %>% select(one_of(c("YEAR",paste(indicator,"_SE",sep=""))))
		if(national){
			colnames(index_se) = c("YEAR","se")
		} else {
			colnames(index_se) = c("GEOGRAPHY","YEAR","se")
		} 
	}
	
	index_se_2 = index_se
	index_se_2$YEAR = index_se$YEAR+2
	
	if(national){
		# sub national level use two year averages in RII calculations
		# add in intermediate years for national level
		index_1 = index
		index_1$YEAR = index$YEAR+1
		index_3 = index
		index_3$YEAR = index$YEAR+3
		index_se_1 = index_se
		index_se_1$YEAR = index_se$YEAR+1
		index_se_3 = index_se
		index_se_3$YEAR = index_se$YEAR+3
	}
	
	if(national){
		index_all = inner_join(inner_join(inner_join(index,index_1,by=join_vars),index_2,by=join_vars),index_3,by=join_vars)
		names(index_all)[grep(indicator,names(index_all))] = paste(indicator,0:3,sep="_")
		index_se_ = inner_join(inner_join(inner_join(index_se,index_se_1,by=join_vars),index_se_2,by=join_vars),index_se_3,by=join_vars)
		names(index_se_)[grep("se",names(index_se_))] = paste(indicator,0:3,sep="_")
		index_all[,paste(indicator,"_trend",sep="")] = 
				(index_all[,paste(indicator,"_0",sep="")]+index_all[,paste(indicator,"_1",sep="")])/2 - 
				(index_all[,paste(indicator,"_2",sep="")]+index_all[,paste(indicator,"_3",sep="")])/2
		index_all[,paste(indicator,"_trend_se",sep="")] = c(0.5 *sqrt(
						(index_se_[,paste(indicator,"_0",sep="")]^2 +
							index_se_[,paste(indicator,"_1",sep="")]^2 + 
							index_se_[,paste(indicator,"_2",sep="")]^2 + 
							index_se_[,paste(indicator,"_3",sep="")]^2)))
	} else {
		index_all = inner_join(index,index_2,by=join_vars)
		names(index_all)[grep(indicator,names(index_all))] = paste(indicator,c(0,2),sep="_")
		index_se_ = inner_join(index_se,index_se_2,by=join_vars)
		names(index_se_)[grep("se",names(index_se_))] = paste(indicator,c(0,2),sep="_")
		index_all[,paste(indicator,"_trend",sep="")] = 
				index_all[,paste(indicator,"_0",sep="")] - index_all[,paste(indicator,"_2",sep="")]
		index_all[,paste(indicator,"_trend_se",sep="")] = c(sqrt(
						(index_se_[,paste(indicator,"_0",sep="")]^2 +
									index_se_[,paste(indicator,"_2",sep="")]^2 )))
	}
	
	index_all[,paste(indicator,"_trend_UCI",sep="")] = index_all[,paste(indicator,"_trend",sep="")] + CI*index_all[,paste(indicator,"_trend_se",sep="")]
	index_all[,paste(indicator,"_trend_LCI",sep="")] = index_all[,paste(indicator,"_trend",sep="")] - CI*index_all[,paste(indicator,"_trend_se",sep="")]
	index_all[,paste(indicator,"_trend_significance",sep="")] = c((index_all[,paste(indicator,"_trend_UCI",sep="")]*index_all[,paste(indicator,"_trend_LCI",sep="")]) >= 0) 
	results = index_all %>% select(one_of(c("YEAR",paste(indicator,"_trend",sep=""),paste(indicator,"_trend_LCI",sep=""),paste(indicator,"_trend_UCI",sep=""),paste(indicator,"_trend_significance",sep=""))))
	return(results)[["indicator_data"]]
}

calculate_trend_indicator = function(indicator_results){
	decreasing = indicator_results$RII_trend_significance & 
			indicator_results$SII_trend_significance &
			(indicator_results$RII_trend * indicator_results$SII_trend) > 0 & 
			indicator_results$RII_trend < 0
	decreasing = !is.na(decreasing) & decreasing
	increasing = indicator_results$RII_trend_significance & 
			indicator_results$SII_trend_significance &
			(indicator_results$RII_trend * indicator_results$SII_trend) > 0 & 
			indicator_results$RII_trend > 0
	increasing = !is.na(increasing) & increasing
	
	indicator_results$overall_trend = "no clear trend"
	indicator_results[decreasing,"overall_trend"] = "clearly reducing"
	indicator_results[increasing,"overall_trend"] = "clearly increasing"
	return(indicator_results)
}


calculate_national_data = function(indicator, method){
	CI95 = qnorm(0.975)
	
	national_quintiles = indicator[["indicator_data"]] %>% 
			group_by(YEAR,QUINTILE) %>%
			summarise(national_indicator=sum(numerator,na.rm=TRUE)/sum(denominator,na.rm=TRUE)) %>% 
			spread(QUINTILE,national_indicator)
	
	# the exposure to predict for when using poisson or negative binomial methods
	predicted_exposure = 1000
	if(grepl("2",indicator["title"])){
		#PHIS
		predicted_exposure = 100
	} else if (grepl("3",indicator["title"])) {
		# mean waiting time
		predicted_exposure = 1
	}
	
	if(method=="OLS"|method=="OLS_stratified"){	
		national_ineq = indicator[["indicator_data"]] %>%
				select(YEAR, NORMALISED_RANK, lsoa_indicator, numerator, denominator) %>%
				group_by(YEAR) %>%
				mutate(OVERALL_MEAN=sum(numerator)/sum(denominator),
						OVERALL_MEAN_SE=sqrt((sd(numerator)/mean(numerator))^2 + (sd(denominator)/mean(denominator))^2 - 2*cov(numerator,denominator)/(mean(numerator)*mean(denominator)))/sqrt(n()),
						denominator = sum(denominator)
						) %>%
				group_by(YEAR, OVERALL_MEAN, OVERALL_MEAN_SE, denominator) %>%
				do(model = lm(lsoa_indicator~NORMALISED_RANK, data=.,na.action=na.omit)) %>%
				mutate(SII=coef(model)[2]*indicator[["slope_sign"]],
						SII_LCI=(coef(model)[2]-CI95*sqrt(vcov(model)[2,2]))*indicator[["slope_sign"]], 
						SII_UCI=(coef(model)[2]+CI95*sqrt(vcov(model)[2,2]))*indicator[["slope_sign"]],
						SII_significance=check_significance(0,SII_UCI,SII_LCI),
						RII=SII/OVERALL_MEAN, 
						RII_LCI=SII_LCI/OVERALL_MEAN, 
						RII_UCI=SII_UCI/OVERALL_MEAN,
						RII_significance=check_significance(0,RII_UCI,RII_LCI)) %>%
				select(YEAR,OVERALL_MEAN,OVERALL_MEAN_SE,SII,SII_LCI,SII_UCI,SII_significance,RII,RII_LCI,RII_UCI,RII_significance,denominator)
	}else if(method=="OLS_stratified_robust"){
			require(sandwich)
			national_ineq = indicator[["indicator_data"]] %>%
					select(YEAR, NORMALISED_RANK, lsoa_indicator, numerator, denominator) %>%
					group_by(YEAR) %>%
					mutate(OVERALL_MEAN=sum(numerator)/sum(denominator),
							OVERALL_MEAN_SE=sqrt((sd(numerator)/mean(numerator))^2 + (sd(denominator)/mean(denominator))^2 - 2*cov(numerator,denominator)/(mean(numerator)*mean(denominator)))/sqrt(n()),
							denominator = sum(denominator)
					) %>%
					group_by(YEAR, OVERALL_MEAN, OVERALL_MEAN_SE, denominator) %>%
					do(model = lm(lsoa_indicator~NORMALISED_RANK, data=.,na.action=na.omit)) %>%
					mutate(SII=coef(model)[2]*indicator[["slope_sign"]],
							SII_LCI=(coef(model)[2]-CI95*sqrt(vcovHC(model,"HC1")[2,2]))*indicator[["slope_sign"]], 
							SII_UCI=(coef(model)[2]+CI95*sqrt(vcovHC(model,"HC1")[2,2]))*indicator[["slope_sign"]],
							SII_significance=check_significance(0,SII_UCI,SII_LCI),
							RII=SII/OVERALL_MEAN, 
							RII_LCI=SII_LCI/OVERALL_MEAN, 
							RII_UCI=SII_UCI/OVERALL_MEAN,
							RII_significance=check_significance(0,RII_UCI,RII_LCI)) %>%
					select(YEAR,OVERALL_MEAN,OVERALL_MEAN_SE,SII,SII_LCI,SII_UCI,SII_significance,RII,RII_LCI,RII_UCI,RII_significance,denominator)
		}else if(method=="OLS_deciles"){	
		national_ineq = indicator[["indicator_data"]] %>%
				select(YEAR, NORMALISED_RANK, lsoa_indicator, numerator, denominator) %>%
				mutate(DECILE=as.numeric(as.character(cut(NORMALISED_RANK,seq(0,1,by=0.1),labels=seq(0.1,1,by=0.1))))) %>%
				group_by(YEAR) %>%
				mutate(OVERALL_MEAN=sum(numerator)/sum(denominator),
						OVERALL_MEAN_SE=sqrt((sd(numerator)/mean(numerator))^2 + (sd(denominator)/mean(denominator))^2 - 2*cov(numerator,denominator)/(mean(numerator)*mean(denominator)))/sqrt(n()),
						denominator = sum(denominator)
				) %>%
				group_by(YEAR, OVERALL_MEAN, OVERALL_MEAN_SE, denominator) %>%
				do(model = lm(lsoa_indicator~DECILE, data=.,na.action=na.omit)) %>%
				mutate(SII=coef(model)[2]*indicator[["slope_sign"]],
						SII_LCI=(coef(model)[2]-CI95*sqrt(vcov(model)[2,2]))*indicator[["slope_sign"]], 
						SII_UCI=(coef(model)[2]+CI95*sqrt(vcov(model)[2,2]))*indicator[["slope_sign"]],
						SII_significance=check_significance(0,SII_UCI,SII_LCI),
						RII=SII/OVERALL_MEAN, 
						RII_LCI=SII_LCI/OVERALL_MEAN, 
						RII_UCI=SII_UCI/OVERALL_MEAN,
						RII_significance=check_significance(0,RII_UCI,RII_LCI)) %>%
				select(YEAR,OVERALL_MEAN,OVERALL_MEAN_SE,SII,SII_LCI,SII_UCI,SII_significance,RII,RII_LCI,RII_UCI,RII_significance,denominator)
	}else if(method=="log_linear"){	
		national_ineq = indicator[["indicator_data"]] %>%
				select(YEAR, NORMALISED_RANK, lsoa_indicator, numerator, denominator) %>%
				filter(lsoa_indicator > 0) %>%
				group_by(YEAR) %>%				
				mutate(OVERALL_MEAN=sum(numerator)/sum(denominator),
						OVERALL_MEAN_SE=sqrt((sd(numerator)/mean(numerator))^2 + (sd(denominator)/mean(denominator))^2 - 2*cov(numerator,denominator)/(mean(numerator)*mean(denominator)))/sqrt(n()),
						numerator = sum(numerator)
				) %>%
				group_by(YEAR, OVERALL_MEAN, OVERALL_MEAN_SE, denominator) %>%
				do(model = lm(log(lsoa_indicator)~NORMALISED_RANK, data=.,na.action=na.omit)) %>%
				mutate(RII=coef(model)[2]*indicator[["slope_sign"]], 
						RII_LCI=(coef(model)[2]-CI95*sqrt(vcov(model)[2,2]))*indicator[["slope_sign"]], 
						RII_UCI=(coef(model)[2]+CI95*sqrt(vcov(model)[2,2]))*indicator[["slope_sign"]], 
						RII_significance=check_significance(0,RII_UCI,RII_LCI), 
						SII=exp(predict(model,data.frame(NORMALISED_RANK=1),type="response",se.fit=TRUE)$fit-predict(model,data.frame(NORMALISED_RANK=0),type="response",se.fit=TRUE)$fit)*indicator[["slope_sign"]], 
						SII_LCI=exp((predict(model,data.frame(NORMALISED_RANK=1),type="response",se.fit=TRUE)$fit-predict(model,data.frame(NORMALISED_RANK=0),type="response",se.fit=TRUE)$fit)-CI95*sqrt(sum(predict(model,data.frame(NORMALISED_RANK=c(0,1)),type="response",se.fit=TRUE)$se.fit^2)))*indicator[["slope_sign"]], 
						SII_UCI=exp((predict(model,data.frame(NORMALISED_RANK=1),type="response",se.fit=TRUE)$fit-predict(model,data.frame(NORMALISED_RANK=0),type="response",se.fit=TRUE)$fit)+CI95*sqrt(sum(predict(model,data.frame(NORMALISED_RANK=c(0,1)),type="response",se.fit=TRUE)$se.fit^2)))*indicator[["slope_sign"]],
						SII_significance=check_significance(0,SII_UCI,SII_LCI)) %>%
				select(YEAR,OVERALL_MEAN,OVERALL_MEAN_SE,SII,SII_LCI,SII_UCI,SII_significance,RII,RII_LCI,RII_UCI,RII_significance,denominator)
	}else if(method=="neg_bin"){
		national_ineq = indicator[["indicator_data"]] %>%
				select(YEAR, NORMALISED_RANK, numerator, denominator, exposure, lsoa_indicator) %>%
				group_by(YEAR) %>%
				mutate(OVERALL_MEAN=sum(numerator)/sum(denominator),
						OVERALL_MEAN_SE=sqrt((sd(numerator)/mean(numerator))^2 + (sd(denominator)/mean(denominator))^2 - 2*cov(numerator,denominator)/(mean(numerator)*mean(denominator)))/sqrt(n()),
						denominator = sum(denominator)
				) %>%
				group_by(YEAR, OVERALL_MEAN, OVERALL_MEAN_SE, denominator) %>%
				do(model = glm.nb(numerator~NORMALISED_RANK+offset(log(exposure)),data=.,na.action=na.omit)) %>%
				mutate(RII=(exp(coef(model)["NORMALISED_RANK"])-1)*indicator[["slope_sign"]],
						RII_LCI=(exp(confint(model)["NORMALISED_RANK","2.5 %"])-1)*indicator[["slope_sign"]], 
						RII_UCI=(exp(confint(model)["NORMALISED_RANK","97.5 %"])-1)*indicator[["slope_sign"]],
						RII_significance=check_significance(0,RII_UCI,RII_LCI),
						SII=RII*predict(model,data.frame(NORMALISED_RANK=0,exposure=predicted_exposure),type="response"), 
						SII_LCI=RII_LCI*predict(model,data.frame(NORMALISED_RANK=0,exposure=predicted_exposure),type="response"), 
						SII_UCI=RII_UCI*predict(model,data.frame(NORMALISED_RANK=0,exposure=predicted_exposure),type="response"),
						SII_significance=check_significance(0,SII_UCI,SII_LCI)) %>%
				select(YEAR,OVERALL_MEAN,OVERALL_MEAN_SE,SII,SII_LCI,SII_UCI,SII_significance,RII,RII_LCI,RII_UCI,RII_significance,denominator)
	}else if(method=="poisson"){
		national_ineq = indicator[["indicator_data"]] %>%
				select(YEAR, NORMALISED_RANK, numerator, denominator, exposure, lsoa_indicator) %>%
				group_by(YEAR) %>%
				mutate(OVERALL_MEAN=sum(numerator)/sum(denominator),
						OVERALL_MEAN_SE=sqrt((sd(numerator)/mean(numerator))^2 + (sd(denominator)/mean(denominator))^2 - 2*cov(numerator,denominator)/(mean(numerator)*mean(denominator)))/sqrt(n()),
						denominator = sum(denominator)
				) %>%
				group_by(YEAR, OVERALL_MEAN, OVERALL_MEAN_SE, denominator) %>%
				do(model = glm(numerator~NORMALISED_RANK+offset(log(exposure)),family="poisson",data=.,na.action=na.omit)) %>%
				mutate(RII=(exp(coef(model)["NORMALISED_RANK"])-1)*indicator[["slope_sign"]],
					RII_LCI=(exp(confint(model)["NORMALISED_RANK","2.5 %"])-1)*indicator[["slope_sign"]], 
					RII_UCI=(exp(confint(model)["NORMALISED_RANK","97.5 %"])-1)*indicator[["slope_sign"]],
					RII_significance=check_significance(0,RII_UCI,RII_LCI),
					SII=RII*predict(model,data.frame(NORMALISED_RANK=0,exposure=predicted_exposure),type="response"), 
					SII_LCI=RII_LCI*predict(model,data.frame(NORMALISED_RANK=0,exposure=predicted_exposure),type="response"), 
					SII_UCI=RII_UCI*predict(model,data.frame(NORMALISED_RANK=0,exposure=predicted_exposure),type="response"),
					SII_significance=check_significance(0,SII_UCI,SII_LCI)) %>%
			select(YEAR,OVERALL_MEAN,OVERALL_MEAN_SE,SII,SII_LCI,SII_UCI,SII_significance,RII,RII_LCI,RII_UCI,RII_significance,denominator)
}
	
	national_data = inner_join(national_quintiles,national_ineq,by=c("YEAR"))
	national_data = left_join(national_data,inner_join(calculate_trend(national_data,"RII",TRUE),calculate_trend(national_data,"SII",TRUE),by=c("YEAR")),by=c("YEAR"))
	national_data = calculate_trend_indicator(national_data)
	national_data = national_data %>%
					mutate(REAL_GAP=SII*0.5*denominator, REAL_GAP_LCI=SII_LCI*0.5*denominator, REAL_GAP_UCI=SII_UCI*0.5*denominator, REAL_GAP_significance=(REAL_GAP_UCI*REAL_GAP_LCI)>0)
	national_data = left_join(national_data, calculate_trend(national_data,"OVERALL_MEAN",TRUE),by=c("YEAR")) %>% 
					arrange(YEAR)
	return(national_data)
}

calculate_geo_data = function(indicator, national_data, method="OLS", years=2){
	CI95 = qnorm(0.975)
	
	indicator_data = indicator[["indicator_data"]]
	end_year = max(indicator_data$YEAR)
	start_year = min(indicator_data$YEAR)
	
	if(years>1){
		for(i in 2:years){
			year_data = indicator[["indicator_data"]]
			year_data$YEAR = year_data$YEAR + (i-1)
			start_year = min(year_data$YEAR)
			indicator_data = rbind(indicator_data,year_data)
		}
	}
	
	indicator_data = indicator_data %>% 
			filter(YEAR>=start_year & YEAR<=end_year) %>%
			group_by(YEAR,LSOA11CD,GEOGRAPHY,QUINTILE,DECILE,NORMALISED_RANK) %>%
			summarise(numerator=mean(numerator),denominator=mean(denominator),exposure=mean(exposure),lsoa_indicator=mean(lsoa_indicator)) %>%
			ungroup()
	
	# use latest SII/RII to test for significance - perhaps we should be calculating average
	national_II = national_data %>% 
			filter(YEAR>=start_year & YEAR<=end_year) %>%
			mutate(NATIONAL_RII=RII,NATIONAL_SII=SII) %>%
			select(YEAR,NATIONAL_RII,NATIONAL_SII)
	
	indicator_data = indicator_data %>%
			inner_join(national_II,by=c("YEAR"))
	
	geo_quintiles = indicator_data %>% 
			group_by(YEAR, GEOGRAPHY, QUINTILE) %>%
			summarise(geo_indicator=sum(numerator,na.rm=TRUE)/sum(denominator,na.rm=TRUE)) %>% 
			spread(QUINTILE,geo_indicator)
	
	# the exposure to predict for when using poisson or negative binomial methods
	predicted_exposure = 1000
	if(grepl("2",indicator["title"])){
		#PHIS
		predicted_exposure = 100
	} else if (grepl("3",indicator["title"])) {
		# mean waiting time
		predicted_exposure = 1
	}
	
	if(method=="OLS"){	
		# random effects
		require("lme4")	
		re_model = indicator_data %>%
				select(YEAR, GEOGRAPHY, NORMALISED_RANK, lsoa_indicator) %>%
				group_by(YEAR) %>%
				do(model = lmer(lsoa_indicator~NORMALISED_RANK + (1+NORMALISED_RANK|GEOGRAPHY), data=.,na.action=na.omit))
		geo_SII = data_frame()
		for(i in 1:length(re_model$YEAR)){
			YEAR = re_model$YEAR[i]
			model = re_model$model[[i]]
			random_effects = ranef(model,condVar=TRUE)
			GEOGRAPHY = row.names(random_effects$GEOGRAPHY)
			SII_i = (random_effects$GEOGRAPHY[,"NORMALISED_RANK"]+as.numeric(fixef(model)["NORMALISED_RANK"]))*indicator[["slope_sign"]]
			postVar = attr(random_effects$GEOGRAPHY, "postVar")
			se = sqrt(postVar[2, 2, ])
			SII_LCI_i = ifelse(is.na(SII_i),-Inf, SII_i-CI95*se*indicator[["slope_sign"]])
			SII_UCI_i = ifelse(is.na(SII_i),-Inf, SII_i+CI95*se*indicator[["slope_sign"]])
			SII_i = data_frame(YEAR,GEOGRAPHY,SII=SII_i,SII_LCI=SII_LCI_i,SII_UCI=SII_UCI_i)
			geo_SII = bind_rows(geo_SII,SII_i)
		}
		
		geo_ineq = indicator_data %>%
				select(YEAR, GEOGRAPHY, NORMALISED_RANK, lsoa_indicator, NATIONAL_RII, NATIONAL_SII, numerator, denominator) %>%
				group_by(YEAR, GEOGRAPHY) %>%
				summarise(OVERALL_MEAN=sum(numerator)/sum(denominator),
						OVERALL_MEAN_SE=sqrt((sd(numerator)/mean(numerator))^2 + (sd(denominator)/mean(denominator))^2 - 2*cov(numerator,denominator)/(mean(numerator)*mean(denominator)))/sqrt(n()),
						denominator=sum(denominator),
						NATIONAL_RII=mean(NATIONAL_RII), 
						NATIONAL_SII=mean(NATIONAL_SII)
				) %>% 
				ungroup() %>%
				left_join(geo_SII,by=c("YEAR","GEOGRAPHY"))  %>%
				mutate(SII_significance=check_significance(NATIONAL_SII,SII_UCI,SII_LCI), 
						RII=ifelse(is.na(SII),-Inf,SII/OVERALL_MEAN), 
						RII_LCI=ifelse(is.na(SII),-Inf,SII_LCI/OVERALL_MEAN), 
						RII_UCI=ifelse(is.na(SII),-Inf,SII_UCI/OVERALL_MEAN),
						RII_significance=ifelse(is.na(SII),FALSE,check_significance(NATIONAL_RII,RII_UCI,RII_LCI))) %>%
				select(YEAR,GEOGRAPHY,OVERALL_MEAN,OVERALL_MEAN_SE,SII,SII_LCI,SII_UCI,SII_significance,RII,RII_LCI,RII_UCI,RII_significance,denominator) %>%
				filter(SII_LCI>-Inf)
	} else if(method=="OLS_stratified"){	
		geo_ineq = indicator_data %>%
				select(YEAR, GEOGRAPHY, NORMALISED_RANK, lsoa_indicator, NATIONAL_RII, NATIONAL_SII, numerator, denominator) %>%
				group_by(YEAR, GEOGRAPHY) %>%
				mutate(OVERALL_MEAN=sum(numerator)/sum(denominator),
						OVERALL_MEAN_SE=sqrt((sd(numerator)/mean(numerator))^2 + (sd(denominator)/mean(denominator))^2 - 2*cov(numerator,denominator)/(mean(numerator)*mean(denominator)))/sqrt(n()),
						denominator = sum(denominator)
				) %>%
				group_by(YEAR, GEOGRAPHY, OVERALL_MEAN, OVERALL_MEAN_SE, NATIONAL_RII, NATIONAL_SII, denominator) %>%
				do(model = lm(lsoa_indicator~NORMALISED_RANK, data=.,na.action=na.omit)) %>%
				mutate(SII=coef(model)[2]*indicator[["slope_sign"]], 
						SII_LCI=ifelse(is.na(SII),-Inf,(coef(model)[2]-CI95*sqrt(vcov(model)[2,2]))*indicator[["slope_sign"]]), 
						SII_UCI=ifelse(is.na(SII),-Inf,(coef(model)[2]+CI95*sqrt(vcov(model)[2,2]))*indicator[["slope_sign"]]), 
						SII_significance=ifelse(is.na(SII),FALSE,check_significance(NATIONAL_SII,SII_UCI,SII_LCI)), 
						RII=ifelse(is.na(SII),-Inf,SII/OVERALL_MEAN), 
						RII_LCI=ifelse(is.na(SII),-Inf,SII_LCI/OVERALL_MEAN), 
						RII_UCI=ifelse(is.na(SII),-Inf,SII_UCI/OVERALL_MEAN),
						RII_significance=ifelse(is.na(SII),FALSE,check_significance(NATIONAL_RII,RII_UCI,RII_LCI))) %>%
				select(YEAR,GEOGRAPHY,OVERALL_MEAN,OVERALL_MEAN_SE,SII,SII_LCI,SII_UCI,SII_significance,RII,RII_LCI,RII_UCI,RII_significance,denominator) %>%
				filter(SII_LCI>-Inf)
	} else if(method=="OLS_stratified_robust"){	
		require(sandwich)
		geo_ineq = indicator_data %>%
				select(YEAR, GEOGRAPHY, NORMALISED_RANK, lsoa_indicator, NATIONAL_RII, NATIONAL_SII, numerator, denominator) %>%
				group_by(YEAR, GEOGRAPHY) %>%
				mutate(OVERALL_MEAN=sum(numerator)/sum(denominator),
						OVERALL_MEAN_SE=sqrt((sd(numerator)/mean(numerator))^2 + (sd(denominator)/mean(denominator))^2 - 2*cov(numerator,denominator)/(mean(numerator)*mean(denominator)))/sqrt(n()),
						denominator = sum(denominator)
				) %>%
				group_by(YEAR, GEOGRAPHY, OVERALL_MEAN, OVERALL_MEAN_SE, NATIONAL_RII, NATIONAL_SII, denominator) %>%
				do(model = lm(lsoa_indicator~NORMALISED_RANK, data=.,na.action=na.omit)) %>%
				mutate(SII=coef(model)[2]*indicator[["slope_sign"]], 
						SII_LCI=ifelse(is.na(SII),-Inf,(coef(model)[2]-CI95*sqrt(vcovHC(model,"HC1")[2,2]))*indicator[["slope_sign"]]), 
						SII_UCI=ifelse(is.na(SII),-Inf,(coef(model)[2]+CI95*sqrt(vcovHC(model,"HC1")[2,2]))*indicator[["slope_sign"]]), 
						SII_significance=ifelse(is.na(SII),FALSE,check_significance(NATIONAL_SII,SII_UCI,SII_LCI)), 
						RII=ifelse(is.na(SII),-Inf,SII/OVERALL_MEAN), 
						RII_LCI=ifelse(is.na(SII),-Inf,SII_LCI/OVERALL_MEAN), 
						RII_UCI=ifelse(is.na(SII),-Inf,SII_UCI/OVERALL_MEAN),
						RII_significance=ifelse(is.na(SII),FALSE,check_significance(NATIONAL_RII,RII_UCI,RII_LCI))) %>%
				select(YEAR,GEOGRAPHY,OVERALL_MEAN,OVERALL_MEAN_SE,SII,SII_LCI,SII_UCI,SII_significance,RII,RII_LCI,RII_UCI,RII_significance,denominator) %>%
				filter(SII_LCI>-Inf)
	}else if(method=="OLS_deciles"){	
		geo_ineq = indicator_data %>%
				select(YEAR, GEOGRAPHY, NORMALISED_RANK, lsoa_indicator, NATIONAL_RII, NATIONAL_SII, numerator, denominator) %>%
				mutate(DECILE=as.numeric(as.character(cut(NORMALISED_RANK,seq(0,1,by=0.1),labels=seq(0.1,1,by=0.1))))) %>%
				group_by(YEAR, GEOGRAPHY) %>%
				mutate(OVERALL_MEAN=sum(numerator)/sum(denominator),
						OVERALL_MEAN_SE=sqrt((sd(numerator)/mean(numerator))^2 + (sd(denominator)/mean(denominator))^2 - 2*cov(numerator,denominator)/(mean(numerator)*mean(denominator)))/sqrt(n()),
						denominator = sum(denominator)
				) %>%
				group_by(YEAR, GEOGRAPHY, OVERALL_MEAN, OVERALL_MEAN_SE, NATIONAL_RII, NATIONAL_SII, denominator) %>%
				do(model = lm(lsoa_indicator~DECILE, data=.,na.action=na.omit)) %>%
				mutate(SII=coef(model)[2]*indicator[["slope_sign"]], 
						SII_LCI=ifelse(is.na(SII),-Inf,(coef(model)[2]-CI95*sqrt(vcov(model)[2,2]))*indicator[["slope_sign"]]), 
						SII_UCI=ifelse(is.na(SII),-Inf,(coef(model)[2]+CI95*sqrt(vcov(model)[2,2]))*indicator[["slope_sign"]]), 
						SII_significance=ifelse(is.na(SII),FALSE,check_significance(NATIONAL_SII,SII_UCI,SII_LCI)), 
						RII=ifelse(is.na(SII),-Inf,SII/OVERALL_MEAN), 
						RII_LCI=ifelse(is.na(SII),-Inf,SII_LCI/OVERALL_MEAN), 
						RII_UCI=ifelse(is.na(SII),-Inf,SII_UCI/OVERALL_MEAN),
						RII_significance=ifelse(is.na(SII),FALSE,check_significance(NATIONAL_RII,RII_UCI,RII_LCI))) %>%
				select(YEAR,GEOGRAPHY,OVERALL_MEAN,OVERALL_MEAN_SE,SII,SII_LCI,SII_UCI,SII_significance,RII,RII_LCI,RII_UCI,RII_significance,denominator) %>%
				filter(SII_LCI>-Inf)
	}else if(method=="log_linear"){	
		geo_ineq = indicator_data %>%
				select(YEAR, GEOGRAPHY, NORMALISED_RANK, lsoa_indicator, NATIONAL_RII, NATIONAL_SII, numerator, denominator) %>%
				filter(lsoa_indicator > 0) %>%
				group_by(YEAR, GEOGRAPHY) %>%		
				mutate(OVERALL_MEAN=sum(numerator)/sum(denominator),
						OVERALL_MEAN_SE=sqrt((sd(numerator)/mean(numerator))^2 + (sd(denominator)/mean(denominator))^2 - 2*cov(numerator,denominator)/(mean(numerator)*mean(denominator)))/sqrt(n()),
						denominator = sum(denominator)
				) %>%
				group_by(YEAR, GEOGRAPHY, OVERALL_MEAN, OVERALL_MEAN_SE, NATIONAL_RII, NATIONAL_SII, denominator) %>%
				do(model = lm(log(lsoa_indicator)~NORMALISED_RANK, data=.,na.action=na.omit)) %>%
				mutate(RII=coef(model)[2]*indicator[["slope_sign"]], 
						RII_LCI=ifelse(is.na(RII),-Inf,(coef(model)[2]-CI95*sqrt(vcov(model)[2,2]))*indicator[["slope_sign"]]), 
						RII_UCI=ifelse(is.na(RII),-Inf,(coef(model)[2]+CI95*sqrt(vcov(model)[2,2]))*indicator[["slope_sign"]]), 
						RII_significance=ifelse(is.na(RII),FALSE,check_significance(NATIONAL_RII,RII_UCI,RII_LCI)), 
						SII=ifelse(is.na(RII),-Inf,exp(predict(model,data.frame(NORMALISED_RANK=1),type="response",se.fit=TRUE)$fit-predict(model,data.frame(NORMALISED_RANK=0),type="response",se.fit=TRUE)$fit)*indicator[["slope_sign"]]), 
						SII_LCI=ifelse(is.na(RII),-Inf,exp((predict(model,data.frame(NORMALISED_RANK=1),type="response",se.fit=TRUE)$fit-predict(model,data.frame(NORMALISED_RANK=0),type="response",se.fit=TRUE)$fit)-CI95*sqrt(sum(predict(model,data.frame(NORMALISED_RANK=c(0,1)),type="response",se.fit=TRUE)$se.fit^2)))*indicator[["slope_sign"]]), 
						SII_UCI=ifelse(is.na(RII),-Inf,exp((predict(model,data.frame(NORMALISED_RANK=1),type="response",se.fit=TRUE)$fit-predict(model,data.frame(NORMALISED_RANK=0),type="response",se.fit=TRUE)$fit)+CI95*sqrt(sum(predict(model,data.frame(NORMALISED_RANK=c(0,1)),type="response",se.fit=TRUE)$se.fit^2)))*indicator[["slope_sign"]]),
						SII_significance=ifelse(is.na(RII),FALSE,check_significance(NATIONAL_SII,SII_UCI,SII_LCI))) %>%
				select(YEAR,GEOGRAPHY,OVERALL_MEAN,OVERALL_MEAN_SE,SII,SII_LCI,SII_UCI,SII_significance,RII,RII_LCI,RII_UCI,RII_significance,denominator) %>%
				filter(RII_LCI>-Inf)
	}else if(method=="neg_bin"){
		geo_ineq = indicator_data %>%
				select(YEAR, GEOGRAPHY, NORMALISED_RANK, numerator, exposure, lsoa_indicator, NATIONAL_RII, NATIONAL_SII, numerator, denominator) %>%
				group_by(YEAR, GEOGRAPHY) %>%
				mutate(OVERALL_MEAN=sum(numerator)/sum(denominator),
						OVERALL_MEAN_SE=sqrt((sd(numerator)/mean(numerator))^2 + (sd(denominator)/mean(denominator))^2 - 2*cov(numerator,denominator)/(mean(numerator)*mean(denominator)))/sqrt(n()),
						denominator = sum(denominator)
				) %>%
				group_by(YEAR, GEOGRAPHY, OVERALL_MEAN, OVERALL_MEAN_SE, NATIONAL_RII, NATIONAL_SII, denominator) %>%
				do(model = glm.nb(numerator~NORMALISED_RANK+offset(log(exposure)),data=.,na.action=na.omit)) %>%
				mutate(RII=(exp(coef(model)["NORMALISED_RANK"])-1)*indicator[["slope_sign"]],
						RII_LCI=ifelse(is.na(RII),-Inf,(exp(confint(model)["NORMALISED_RANK","2.5 %"])-1))*indicator[["slope_sign"]], 
						RII_UCI=ifelse(is.na(RII),-Inf,(exp(confint(model)["NORMALISED_RANK","97.5 %"])-1))*indicator[["slope_sign"]],
						RII_significance=ifelse(is.na(RII),-Inf,check_significance(NATIONAL_RII,RII_UCI,RII_LCI)),
						SII=ifelse(is.na(RII),-Inf,RII*predict(model,data.frame(NORMALISED_RANK=0,exposure=predicted_exposure),type="response")), 
						SII_LCI=ifelse(is.na(RII),-Inf,RII_LCI*predict(model,data.frame(NORMALISED_RANK=0,exposure=predicted_exposure),type="response")), 
						SII_UCI=ifelse(is.na(RII),-Inf,RII_UCI*predict(model,data.frame(NORMALISED_RANK=0,exposure=predicted_exposure),type="response")),
						SII_significance=ifelse(is.na(RII),-Inf,check_significance(NATIONAL_SII,SII_UCI,SII_LCI))) %>%
				select(YEAR,GEOGRAPHY,OVERALL_MEAN,OVERALL_MEAN_SE,SII,SII_LCI,SII_UCI,SII_significance,RII,RII_LCI,RII_UCI,RII_significance,denominator) %>%
				filter(RII_LCI>-Inf)
	}else if(method=="poisson"){
		geo_ineq = indicator_data %>%
				select(YEAR, GEOGRAPHY, NORMALISED_RANK, numerator, exposure, lsoa_indicator, NATIONAL_RII, NATIONAL_SII, numerator, denominator) %>%
				group_by(YEAR, GEOGRAPHY) %>%
				mutate(OVERALL_MEAN=sum(numerator)/sum(denominator),
						OVERALL_MEAN_SE=sqrt((sd(numerator)/mean(numerator))^2 + (sd(denominator)/mean(denominator))^2 - 2*cov(numerator,denominator)/(mean(numerator)*mean(denominator)))/sqrt(n()),
						denominator = sum(denominator)
				) %>%
				group_by(YEAR, GEOGRAPHY, OVERALL_MEAN, OVERALL_MEAN_SE, NATIONAL_RII, NATIONAL_SII, denominator) %>%
				do(model = glm(numerator~NORMALISED_RANK+offset(log(exposure)),family="poisson",data=.,na.action=na.omit)) %>%
				mutate(RII=(exp(coef(model)["NORMALISED_RANK"])-1)*indicator[["slope_sign"]],
						RII_LCI=ifelse(is.na(RII),-Inf,(exp(confint(model)["NORMALISED_RANK","2.5 %"])-1))*indicator[["slope_sign"]], 
						RII_UCI=ifelse(is.na(RII),-Inf,(exp(confint(model)["NORMALISED_RANK","97.5 %"])-1))*indicator[["slope_sign"]],
						RII_significance=ifelse(is.na(RII),-Inf,check_significance(NATIONAL_RII,RII_UCI,RII_LCI)),
						SII=ifelse(is.na(RII),-Inf,RII*predict(model,data.frame(NORMALISED_RANK=0,exposure=predicted_exposure),type="response")), 
						SII_LCI=ifelse(is.na(RII),-Inf,RII_LCI*predict(model,data.frame(NORMALISED_RANK=0,exposure=predicted_exposure),type="response")), 
						SII_UCI=ifelse(is.na(RII),-Inf,RII_UCI*predict(model,data.frame(NORMALISED_RANK=0,exposure=predicted_exposure),type="response")),
						SII_significance=ifelse(is.na(RII),-Inf,check_significance(NATIONAL_SII,SII_UCI,SII_LCI))) %>%
				select(YEAR,GEOGRAPHY,OVERALL_MEAN,OVERALL_MEAN_SE,SII,SII_LCI,SII_UCI,SII_significance,RII,RII_LCI,RII_UCI,RII_significance,denominator) %>%
				filter(RII_LCI>-Inf)
	}
	geo_data = inner_join(geo_quintiles,geo_ineq,by=c("YEAR", "GEOGRAPHY")) 	
	#if(end_year >= start_year+4){	
	#geo_data = left_join(geo_data,inner_join(calculate_trend(geo_data,"RII",FALSE),calculate_trend(geo_data,"SII",FALSE),by=c("GEOGRAPHY", "YEAR")),by=c("GEOGRAPHY", "YEAR"))
	#geo_data = calculate_trend_indicator(geo_data) 
	#}
	geo_data = geo_data %>% 
			select(YEAR,GEOGRAPHY,geo_q1 = Q1) %>% 
			inner_join(select(national_data,YEAR,national_q1=Q1), by="YEAR") %>%
			mutate(RELATIVE_GAP_NDF=(national_q1-geo_q1)/national_q1, ABSOLUTE_GAP_NDF=(national_q1-geo_q1)) %>%
			select(YEAR,GEOGRAPHY,ABSOLUTE_GAP_NDF,RELATIVE_GAP_NDF) %>%
			inner_join(geo_data,by=c("YEAR","GEOGRAPHY")) 
	geo_data = geo_data %>%
			mutate(REAL_GAP=SII*0.5*denominator, REAL_GAP_LCI=SII_LCI*0.5*denominator, REAL_GAP_UCI=SII_UCI*0.5*denominator, REAL_GAP_significance=(REAL_GAP_UCI*REAL_GAP_LCI)>0) %>%
			arrange(YEAR,desc(RII))
	if(end_year >= start_year+4){
		geo_data = left_join(geo_data, calculate_trend(geo_data,"OVERALL_MEAN",FALSE),by=c("GEOGRAPHY","YEAR")) %>%  
				arrange(YEAR,desc(RII))	
	}
	return(geo_data)
}

plot_geo_scatter2 = function(geo_name, year, indicator, reverse, geo, years=1){
	if(year>max(indicator[["indicator_data"]]$YEAR)){
		year = max(indicator[["indicator_data"]]$YEAR)
	}
	
	indicator_data = indicator[["indicator_data"]] %>% 
			filter(YEAR<=year & YEAR>=(year-(years-1))) %>%
			group_by(GEOGRAPHY,NORMALISED_RANK) %>%
			summarise(lsoa_indicator=mean(numerator)/mean(denominator)) %>%
			ungroup()
	national_model = lm(lsoa_indicator ~ NORMALISED_RANK, data=indicator_data)
	geo_data = indicator_data %>% 
			filter(GEOGRAPHY==geo_name) %>% 
			select(NORMALISED_RANK, lsoa_indicator)
	geo_average = mean(geo_data$lsoa_indicator)
	national_average = mean(indicator_data$lsoa_indicator)
	model = lm(lsoa_indicator ~ NORMALISED_RANK, data=geo_data)
	max_point = predict(model,data_frame(NORMALISED_RANK=1))
	min_point = predict(model,data_frame(NORMALISED_RANK=0))
	geo_data = geo_data %>% filter(!(NORMALISED_RANK %in% c(0,1)))
	geo_data = rbind(geo_data,c(1,max_point))
	geo_data = rbind(geo_data,c(0,min_point))
	
	trend = predict(model,data_frame(NORMALISED_RANK=geo_data$NORMALISED_RANK))
	national_trend = predict(national_model,data_frame(NORMALISED_RANK=geo_data$NORMALISED_RANK))
	## trend_se = predict(model,data_frame(NORMALISED_RANK=geo_data$NORMALISED_RANK),se.fit=TRUE)$se.fit
	## CI_95 = qnorm(0.975)
	## trend_UCI = trend + CI_95*trend_se
	## trend_LCI = trend - CI_95*trend_se
	level = rep(max_point, length(trend))
	area = data.frame(NORMALISED_RANK=geo_data$NORMALISED_RANK,level,trend)
	geo_data=inner_join(geo_data, area, by="NORMALISED_RANK")
	lines = data.frame(NORMALISED_RANK=geo_data$NORMALISED_RANK,geo_average,geo_trend=trend,national_average,national_trend) %>% gather(line,value,-NORMALISED_RANK)
	geo_data=inner_join(lines, geo_data, by="NORMALISED_RANK")
	
	scatter = ggplot(data=geo_data, aes(x=(1-NORMALISED_RANK),y=lsoa_indicator, group=line)) +
			geom_jitter(size=1, width=0.05, height=0.05, colour="lightblue", aes(x=(1-NORMALISED_RANK),y=lsoa_indicator)) +
			xlab("Small area deprivation rank") + 
			ylab(indicator[["indicator_label"]]) +
			ggtitle(paste(indicator[["title"]]," ",year,"/",substr(as.character(year+1),3,4),sep="")) + 
			scale_x_continuous(breaks=seq(0,1,0.2), labels=c("least deprived","","","","","most deprived")) +
			geom_line(aes(y=value, linetype=line, colour=line, size=line)) +
			geom_ribbon(aes(ymax=trend, ymin=level), alpha=0.1) +
			scale_colour_manual(name="", values = c("geo_average" = "black", "geo_trend" = "black", "national_average" = "red", "national_trend" = "red"), labels=c(paste(geo," Average",sep=""),paste(geo," Slope",sep=""),"National Average","National Slope")) +
			scale_linetype_manual(name="", values = c("geo_average" = 2, "geo_trend" = 1, "national_average" = 4, "national_trend" = 1), labels=c(paste(geo," Average",sep=""),paste(geo," Slope",sep=""),"National Average","National Slope")) +
			scale_size_manual(name="", values = c("geo_average" = 1.5, "geo_trend" = 0.5, "national_average" = 1.5, "national_trend" = 0.5), labels=c(paste(geo," Average",sep=""),paste(geo," Slope",sep=""),"National Average","National Slope")) +
			theme_bw() +
			theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.title = element_text(lineheight=.8, face="bold", size=rel(1.7)))
	if(reverse){
		scatter = scatter + scale_y_reverse(labels=comma)
	} else {
		scatter = scatter + scale_y_continuous(labels=comma)
	}
	scatter_plot = 
			arrangeGrob(scatter, 
					sub = textGrob(paste0(indicator[["footnote"]]), x = 0, hjust = -0.1, vjust=0.1,
							gp = gpar(fontface = "italic", fontsize = 10)),
					heights=c(0.90,0.10))
	return(scatter_plot)
}

plot_national_scatter2 = function(year, indicator, reverse){
	indicator_data = indicator[["indicator_data"]] %>% 
			filter(YEAR==year) %>% 
			select(NORMALISED_RANK, lsoa_indicator)
	average = mean(indicator_data$lsoa_indicator)
	model = lm(lsoa_indicator ~ NORMALISED_RANK, data=indicator_data)
	max_point = predict(model,data_frame(NORMALISED_RANK=1))
	min_point = predict(model,data_frame(NORMALISED_RANK=0))
	
	trend = predict(model,data_frame(NORMALISED_RANK=seq(0,1,0.1)))
	trend_se = predict(model,data_frame(NORMALISED_RANK=seq(0,1,0.1)),se.fit=TRUE)$se.fit
	CI_95 = qnorm(0.975)
	trend_UCI = trend + CI_95*trend_se
	trend_LCI = trend - CI_95*trend_se
	level = rep(max_point, length(trend))
	area = data.frame(DEP=as.factor(seq(0,1,0.1)),level,trend,trend_LCI,trend_UCI,average)
	averages = indicator_data %>%
			mutate(DEPRIVATION=round(NORMALISED_RANK,1)) %>%
			group_by(DEPRIVATION) %>%
			summarise(lsoa_indicator=mean(lsoa_indicator)) %>%
			mutate(DEP=as.factor(DEPRIVATION))
	graph_data = inner_join(area, averages, by="DEP")
	scatter = ggplot(data=graph_data, aes(x=(1-DEPRIVATION),y=lsoa_indicator)) +
			geom_point(size=2, colour="black") +
			xlab("Small area deprivation rank") + 
			ylab(indicator[["indicator_label"]]) +
			ggtitle(paste(indicator[["title"]]," ",year,"/",substr(as.character(year+1),3,4),sep="")) + 
			scale_x_continuous(breaks=seq(0,1,0.2), labels=c("least deprived","","","","","most deprived")) +
			geom_line(aes(y=trend), colour="black") +	
			geom_line(aes(y=average),linetype=2, size=1.5, colour="red") +
			geom_ribbon(aes(ymax=trend, ymin=level), alpha=0.1) +
			theme_bw() +
			theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.title = element_text(lineheight=.8, face="bold", size=rel(1.7)))
	if(reverse){
		scatter = scatter + scale_y_reverse(labels=comma)
	} else {
		scatter = scatter + scale_y_continuous(labels=comma)
	}
	scatter_plot = 
			arrangeGrob(scatter, 
					sub = textGrob(indicator[["footnote"]], x = 0, hjust = -0.1, vjust=0.1,
							gp = gpar(fontface = "italic", fontsize = 10)),
					heights=c(0.90,0.10))
	return(scatter_plot)
}

plot_national_correlation = function(year, geo_data, indicator){
	source("../db_connection.R")
	con = get_db_connection()
	sql = paste("SELECT CCG13NM AS GEOGRAPHY, normalised_rank 
					FROM 
					CCG_DETAILS
					WHERE YEAR=",year,sep="")
	ccg_data = dbGetQuery(con,sql)
	graph_data = geo_data %>% 
			filter(YEAR==2011) %>%
			inner_join(ccg_data,by="GEOGRAPHY") %>%
			ungroup() %>%
			select(NORMALISED_RANK,MEAN=OVERALL_MEAN,AGI=SII) %>%
			gather(INDICATOR,VALUE,-NORMALISED_RANK)
	
	correlation = ggplot(data=graph_data, aes(x=(1-NORMALISED_RANK),y=VALUE)) +
			geom_point(size=2) +
			ggtitle(paste(indicator[["title"]]," ",year,"/",substr(as.character(year+1),3,4),sep="")) +
			xlab(paste("Deprivation",sep="")) + 
			ylab("") +
			scale_x_continuous(breaks=seq(0,1,0.2), labels=c("least deprived","","","","","most deprived")) +
			facet_wrap(~INDICATOR, scales = "free") +
			theme_bw() +
			theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.title = element_text(lineheight=.8, face="bold", size=rel(1.7)),panel.margin = unit(2, "lines"))
	
	correlation_plot = 
			arrangeGrob(correlation, 
					sub = textGrob(indicator[["footnote"]], x = 0, hjust = -0.1, vjust=0.1,
							gp = gpar(fontface = "italic", fontsize = 10)),
					heights=c(0.90,0.10))
	return(correlation_plot)
}


plot_geo_caterpillar = function(geo_name, year, indicator_results, indicator, national_mean_sii, geo){
	if(year>max(indicator_results$YEAR)){
		year = max(indicator_results$YEAR)
	}
	
	other_label = paste("Other ",geo,"s",sep="")
	indicator_results = indicator_results %>% filter(YEAR==year) %>% select(GEOGRAPHY,SII,SII_LCI,SII_UCI)
	selected_geo_results = indicator_results %>% filter(GEOGRAPHY==geo_name) %>% mutate(group=geo_name)
	indicator_results = indicator_results %>% filter(GEOGRAPHY!=geo_name) %>% mutate(group=other_label)
	caterpillar_data = bind_rows(indicator_results,selected_geo_results) %>% arrange(desc(SII))
	caterpillar_data[,"SII_RANK"] = (1:nrow(caterpillar_data))/nrow(caterpillar_data)
	caterpillar_data$group = factor(caterpillar_data$group, levels=c(other_label,geo_name))
	caterpillar = ggplot(caterpillar_data, aes(x=SII_RANK,y=SII, group=group, colour=group, alpha=group)) +
			geom_point(size=1) +
			geom_errorbar(aes(ymin=SII_LCI, ymax=SII_UCI)) +
			scale_color_manual(name=paste(geo,"s (",year,"/",substr(as.character(year+1),3,4),")",sep=""),values=c("grey","black"),labels=c(other_label,geo_name)) +
			scale_alpha_manual(name=paste(geo,"s (",year,"/",substr(as.character(year+1),3,4),")",sep=""),values=c(0.6,1),labels=c(other_label,geo_name)) +
			xlab(paste(geo," equity rank",sep="")) + 
			ylab(paste("AGI",indicator[["indicator_label"]],sep=" ")) +
			ggtitle(paste(indicator[["title"]]," ",year,"/",substr(as.character(year+1),3,4),sep="")) +
			geom_hline(yintercept=national_mean_sii, colour="red", linetype=2) +
			scale_y_continuous(labels = comma) +
			scale_x_continuous(breaks=seq(0,1,0.2), labels=c("least equitable","","","","","most equitable")) +
			theme_bw() +
			theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.title = element_text(lineheight=.8, face="bold", size=rel(1.7)))
	caterpillar_plot = 
			arrangeGrob(caterpillar, 
			sub = textGrob(indicator[["footnote"]], x = 0, hjust = -0.1, vjust=0.1,
					gp = gpar(fontface = "italic", fontsize = 10)),
			heights=c(0.90,0.10))
	
	return(caterpillar_plot)
}

plot_national_caterpillar = function(year, indicator_results, indicator, national_mean_sii, geo){
	if(year>max(indicator_results$YEAR)){
		year = max(indicator_results$YEAR)
	}
	
	indicator_results = indicator_results %>% filter(YEAR==year) %>% select(GEOGRAPHY,SII,SII_LCI,SII_UCI) %>% ungroup()
	caterpillar_data = indicator_results %>% arrange(desc(SII))
	caterpillar_data[,"SII_RANK"] = (1:nrow(caterpillar_data))/nrow(caterpillar_data)
	caterpillar = ggplot(caterpillar_data, aes(x=SII_RANK,y=SII)) +
			geom_point(size=1) +
			geom_errorbar(aes(ymin=SII_LCI, ymax=SII_UCI)) +
			xlab(paste(geo," equity rank",sep="")) + 
			ylab(paste("AGI",indicator[["indicator_label"]],sep=" ")) +
			ggtitle(paste(indicator[["title"]]," ",year,"/",substr(as.character(year+1),3,4),sep="")) +
			geom_hline(yintercept=national_mean_sii, colour="red", linetype=2) +
			scale_y_continuous(labels = comma) +
			scale_x_continuous(breaks=seq(0,1,0.2), labels=c("least equitable","","","","","most equitable")) +
			theme_bw() +
			theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.title = element_text(lineheight=.8, face="bold", size=rel(1.7)))
	caterpillar_plot = 
			arrangeGrob(caterpillar, 
					sub = textGrob(indicator[["footnote"]], x = 0, hjust = -0.1, vjust=0.1,
							gp = gpar(fontface = "italic", fontsize = 10)),
					heights=c(0.90,0.10))
	return(caterpillar_plot)
}

create_population_ouput = function(data, output_directory, geo, geo_names, years){
	lsoa_data_wide = data[["lsoa_data_wide"]]
	matrix_data_wide=data[["matrix_data_wide"]]
	geo_population = lsoa_data_wide %>% 
			select(YEAR,LSOA11CD,GEOGRAPHY,QUINTILE,DECILE,lsoa_indicator=population) %>%
			group_by(YEAR,GEOGRAPHY, QUINTILE) %>%
			summarise(population=sum(lsoa_indicator)) %>%
			spread(QUINTILE,population)
	
	national_population = lsoa_data_wide %>% 
			select(YEAR,LSOA11CD,GEOGRAPHY,QUINTILE,DECILE,lsoa_indicator=population) %>%
			group_by(YEAR,QUINTILE) %>%
			summarise(population=sum(lsoa_indicator)) %>%
			spread(QUINTILE,population)

	geo_cips = lsoa_data_wide %>% 
			select(YEAR,LSOA11CD,GEOGRAPHY,QUINTILE,DECILE,lsoa_indicator=cips_count_ind) %>%
			group_by(YEAR,GEOGRAPHY, QUINTILE) %>%
			summarise(cips=sum(lsoa_indicator)) %>%
			spread(QUINTILE,cips)
	
	national_cips = lsoa_data_wide %>% 
			select(YEAR,LSOA11CD,GEOGRAPHY,QUINTILE,DECILE,lsoa_indicator=cips_count_ind) %>%
			group_by(YEAR,QUINTILE) %>%
			summarise(cips=sum(lsoa_indicator)) %>%
			spread(QUINTILE,cips)

	geo_population_adj = lsoa_data_wide %>% 
			select(YEAR,LSOA11CD,GEOGRAPHY,QUINTILE,DECILE,lsoa_indicator=need_adjusted_population) %>%
			group_by(YEAR,GEOGRAPHY, QUINTILE) %>%
			summarise(need_adjusted_population=sum(lsoa_indicator)) %>%
			spread(QUINTILE,need_adjusted_population)
	
	national_population_adj = lsoa_data_wide %>% 
			select(YEAR,LSOA11CD,GEOGRAPHY,QUINTILE,DECILE,lsoa_indicator=need_adjusted_population) %>%
			group_by(YEAR,QUINTILE) %>%
			summarise(need_adjusted_population=sum(lsoa_indicator)) %>%
			spread(QUINTILE,need_adjusted_population)
	
	national_output_directory = paste(output_directory,"England/",sep="")
	dir.create(national_output_directory, showWarnings = FALSE)
	national_output_directory_pop = paste(national_output_directory,"population/",sep="")
	dir.create(national_output_directory_pop, showWarnings = FALSE)
	geo_cat_dir = paste(output_directory,geo,"/",sep="") 
	dir.create(geo_cat_dir, showWarnings = FALSE)
	geo_cat_dir_pop = paste(geo_cat_dir,"population/",sep="") 
	dir.create(geo_cat_dir_pop, showWarnings = FALSE)
	
	write.csv(national_population,file=paste(national_output_directory_pop,"dashboard_data_national_population.csv",sep=""),na="",row.names=FALSE)
	write.csv(geo_population,file=paste(geo_cat_dir_pop,"dashboard_data_",geo,"_population.csv",sep=""),na="",row.names=FALSE)

	write.csv(national_cips,file=paste(national_output_directory_pop,"dashboard_data_national_cips.csv",sep=""),na="",row.names=FALSE)
	write.csv(geo_cips,file=paste(geo_cat_dir_pop,"dashboard_data_",geo,"_cips.csv",sep=""),na="",row.names=FALSE)
	
	write.csv(national_population_adj,file=paste(national_output_directory_pop,"dashboard_data_national_population_adj.csv",sep=""),na="",row.names=FALSE)
	write.csv(geo_population_adj,file=paste(geo_cat_dir_pop,"dashboard_data_",geo,"_population_adj.csv",sep=""),na="",row.names=FALSE)
	
	population_matrix = matrix_data_wide %>% 
			select(YEAR,IMD_GROUP,AGE_GROUP,SEX, matrix_indicator=population)	
	
	population = list()
	population[["matrix_data"]] = population_matrix
	population[["indicator_label"]] = "Population"
	population[["title"]] = "Population"
	population[["footnote"]] = "ONS mid-year population estimates by age and sex at small area level"
	
	national_matrix_plot_free = plot_matrix(population,scale="free_y")
	national_matrix_plot_fixed = plot_matrix(population,scale="fixed")		
	ggsave(filename=paste(national_output_directory_pop,"population_matrix_fixed.png",sep=""),plot=national_matrix_plot_fixed,width=27,height=20,units="cm",dpi=300)
	ggsave(filename=paste(national_output_directory_pop,"population_matrix_free.png",sep=""),plot=national_matrix_plot_free,width=27,height=20,units="cm",dpi=300)	

	for(geo_name in geo_names){
		geo_directory = paste(geo_cat_dir_pop,gsub(" ","_",geo_name),"/",sep="")
		dir.create(geo_directory, showWarnings = FALSE)
		for(year in years){
			create_geo_population_table(lsoa_data_wide, geo_name, year, geo_directory)
		}
	}
}

create_geo_population_table = function(lsoa_data, geo_name, year, output_directory){
	geo_pop = lsoa_data %>% 
			filter(GEOGRAPHY == geo_name, YEAR==year) %>%
			group_by(QUINTILE) %>%
			summarise(population=sum(population)) %>%
			inner_join(lsoa_data %>% 
							filter(GEOGRAPHY == geo_name, YEAR==year) %>%
							group_by(QUINTILE) %>%
							summarise(lsoas=n()), by="QUINTILE") %>%
			ungroup() %>%
			mutate(total_population=sum(population), 
					population_percentage=round(population*100/total_population,2),
					total_lsoas=sum(lsoas),
					lsoas_percentage=round(lsoas*100/total_lsoas,2)) %>%
			select(QUINTILE,population,population_percentage,lsoas,lsoas_percentage)
	
	geo_population = xtable(rbind(geo_pop,c("Total",round(colSums(geo_pop[-1])))), caption = paste("Summary of ",geo_name," Population: ", year,sep="")) 
	sink(file=paste(output_directory,"population_table_",year,".tex",sep=""))
	print(geo_population,size="\\footnotesize",format.args=list(big.mark=","),table.placement="H",include.rownames=FALSE)
	sink()
}

create_indicators_output = function(indicators, indicator_names, output_directory, geo, geo_names, years, methods, trim, smooth_years){
	final_indicator_list = c("pat_gp_fte_adj","PHIS","waiting_time_adj","unplanned_hospitalisations_adj","repeat_hosp_adj","hosp_death","amenable_mortality_adj","all_mortality_adj")
	ranks_by_method = list()
	national_output_directory = paste(output_directory,"England/",sep="")
	dir.create(national_output_directory, showWarnings = FALSE)
	national_output_file = "data_national.csv"
	national_output_file_dash = "dashboard_data_national.csv"
	geo_cat_dir = paste(output_directory,geo,"/",sep="") 
	dir.create(geo_cat_dir, showWarnings = FALSE)
	geo_output_file = paste("data_",geo,".csv",sep="")
	#geo_output_file_1year = paste("data_",geo,"_1year.csv",sep="")
	geo_output_file_dash = paste("dashboard_data_",geo,".csv",sep="")
	#geo_output_file_1year_dash = paste("dashboard_data_",geo,"_1year.csv",sep="")
	
	for(method in methods){
		national_method_directory = paste(national_output_directory,method,"/",sep="")
		dir.create(national_method_directory, showWarnings = FALSE)
		geo_method_dir = paste(geo_cat_dir,"/",method,"/",sep="") 
		dir.create(geo_method_dir, showWarnings = FALSE)
		for(geo_name in geo_names){
			geo_directory = paste(geo_method_dir,gsub(" ","_",geo_name),"/",sep="")
			dir.create(geo_directory, showWarnings = FALSE)
		}
		ranks_by_method[[method]] = data.frame(INDICATOR=character(),RII=double(),GEOGRAPHY=character(),RANK=integer(),INV_RANK=integer(),stringsAsFactors = FALSE)		
	}
	
	for(indicator_name in indicator_names){
		indicator = indicators[[indicator_name]]
		if(indicator$slope_sign==1){
			reverse=TRUE
		}else{
			reverse=FALSE
		}
		# remove extreme outliers i.e. bad data
		indicator[["indicator_data"]] = indicator[["indicator_data"]] %>% filter(!is.infinite(lsoa_indicator))
		if(trim==TRUE){
			untrimmed = data_frame()
			num_sds = 6
			if(indicator_name=="PHIS"){
				max=100
				num_sds = 3
			} else if(indicator_name=="pat_gp_fte_adj"){
				max=10000
			} else{ 
				max=-1
			}
			for(year in unique(indicator[["indicator_data"]]$YEAR)){
				year_indicator = indicator[["indicator_data"]][indicator[["indicator_data"]]$YEAR==year,]
				year_indicator = year_indicator %>% filter(!is.infinite(lsoa_indicator))
				if(max>0){
					year_indicator = year_indicator %>% filter(lsoa_indicator<=max)
				}
				year_indicator = year_indicator %>% filter(lsoa_indicator>=0)
				indicator_mean = mean(year_indicator$lsoa_indicator, na.rm=TRUE)
				indicator_sd = sd(year_indicator$lsoa_indicator, na.rm=TRUE)
				lower_limit = indicator_mean - num_sds*indicator_sd
				upper_limit = indicator_mean + num_sds*indicator_sd
				untrimmed = bind_rows(untrimmed,year_indicator %>% filter(lsoa_indicator>=lower_limit & lsoa_indicator<=upper_limit))
			}
			indicator[["indicator_data"]] = untrimmed
		}
	
		# national matrix file for those indicators where possible to break down
		if("matrix_data" %in% names(indicator)){
			national_matrix_plot_free = plot_matrix(indicator,scale="free_y")
			national_matrix_plot_fixed = plot_matrix(indicator,scale="fixed")		
			ggsave(filename=paste(national_output_directory,indicator_name,"_matrix_fixed.png",sep=""),plot=national_matrix_plot_fixed,width=27,height=20,units="cm",dpi=300)
			ggsave(filename=paste(national_output_directory,indicator_name,"_matrix_free.png",sep=""),plot=national_matrix_plot_free,width=27,height=20,units="cm",dpi=300)
		}		
		
		for(method in methods){
			# national data file
			national_data = calculate_national_data(indicator, method)
			national_method_directory = paste(national_output_directory,method,"/",sep="")
	
			national_file_exists = file.exists(paste(national_method_directory,national_output_file,sep=""))
			write.table(cbind(INDICATOR=indicator_name,national_data),file=paste(national_method_directory,national_output_file,sep=""),sep=",",na="",append=national_file_exists,col.names=!national_file_exists,row.names=FALSE)

			national_file_exists = file.exists(paste(national_method_directory,national_output_file_dash,sep=""))
			national_data_dash = cbind(INDICATOR=indicator_name,national_data) %>%
									filter(YEAR==max(years))
			write.table(national_data_dash,file=paste(national_method_directory,national_output_file_dash,sep=""),sep=",",na="",append=national_file_exists,col.names=!national_file_exists,row.names=FALSE)
			
			# national gradient plot
			national_gradient_plot = plot_gradient(national_data, indicator)
			ggsave(filename=paste(national_method_directory,indicator_name,"_gradient.png",sep=""),plot=national_gradient_plot,width=27,height=19,units="cm",dpi=300)

			# national panel plot
			national_panel_plot = plot_quintiles(national_data, indicator)
			ggsave(filename=paste(national_method_directory,indicator_name,"_panel.png",sep=""),plot=national_panel_plot,width=27,height=19,units="cm",dpi=300)
		
			# calculate nhs geography level output for this method
			geo_method_dir = paste(geo_cat_dir,"/",method,"/",sep="") 

			# nhs geography data file including every unit at this level averaged over smooth_years years
			geo_data = calculate_geo_data(indicator, national_data, method, years=smooth_years)
			geo_file_exists = file.exists(paste(geo_method_dir,geo_output_file,sep=""))
			write.table(cbind(INDICATOR=indicator_name,geo_data),file=paste(geo_method_dir,geo_output_file,sep=""),sep=",",na="",append=geo_file_exists,col.names=!geo_file_exists,row.names=FALSE)

			geo_file_exists = file.exists(paste(geo_method_dir,geo_output_file_dash,sep=""))
			geo_data_dash = cbind(INDICATOR=indicator_name,geo_data) %>%
					filter(YEAR==max(years)) %>%
					mutate(GEOGRAPHY=sub("NHS ", "",GEOGRAPHY)) %>%
					arrange(INDICATOR,GEOGRAPHY)
			write.table(cbind(GEO_NUM=1:nrow(geo_data_dash),geo_data_dash),file=paste(geo_method_dir,geo_output_file_dash,sep=""),sep=",",na="",append=geo_file_exists,col.names=!geo_file_exists,row.names=FALSE)
			
#			geo_data_1year = calculate_geo_data(indicator, national_data, method, years=1)
#			geo_file_exists = file.exists(paste(geo_method_dir,geo_output_file_1year,sep=""))
#			write.table(cbind(INDICATOR=indicator_name,geo_data_1year),file=paste(geo_method_dir,geo_output_file_1year,sep=""),sep=",",na="",append=geo_file_exists,col.names=!geo_file_exists,row.names=FALSE)
#
#			geo_file_exists = file.exists(paste(geo_method_dir,geo_output_file_1year_dash,sep=""))
#			geo_data_1year_dash = cbind(INDICATOR=indicator_name,geo_data_1year) %>%
#								filter(YEAR==max(years)) %>%
#								mutate(GEOGRAPHY=sub("NHS ", "",GEOGRAPHY)) %>%
#								arrange(INDICATOR,GEOGRAPHY)
#			write.table(cbind(GEO_NUM=1:nrow(geo_data_1year_dash),geo_data_1year_dash),file=paste(geo_method_dir,geo_output_file_1year_dash,sep=""),sep=",",na="",append=geo_file_exists,col.names=!geo_file_exists,row.names=FALSE)
			
			for(year in years){
				caterpillar = plot_national_caterpillar(year, geo_data, indicator, subset(national_data,YEAR==year)$SII, geo)
				ggsave(filename=paste(national_method_directory,indicator_name,"_",year,"_caterpillar.png",sep=""),plot=caterpillar,width=27,height=19,units="cm",dpi=300)
				scatter = plot_national_scatter2(year, indicator, reverse)
				ggsave(filename=paste(national_method_directory,indicator_name,"_",year,"_scatter.png",sep=""),plot=scatter,width=27,height=19,units="cm",dpi=300)
				if(geo=="CCG"){
					correlation_plot = plot_national_correlation(year, geo_data, indicator)	
					ggsave(filename=paste(national_method_directory,indicator_name,"_",year,"_correlation.png",sep=""),plot=correlation_plot,width=27,height=19,units="cm",dpi=300)				
				}
			}
			
			rank_year = max(geo_data$YEAR)
			rank = geo_data %>%
					filter(YEAR==rank_year) %>%
					ungroup() %>%
					select(GEOGRAPHY,SII,SII_significance) %>%
					mutate(INDICATOR=indicator[["title"]],SII=round(SII,2),GEOGRAPHY=substr(GEOGRAPHY,4,44),RANK=row_number(SII),INV_RANK=row_number(-1*SII))
		
			if(indicator_name %in% final_indicator_list){
				ranks_by_method[[method]] = ranks_by_method[[method]] %>% bind_rows(rank)
			}
			
			top_ten = rank %>%
						arrange(RANK) %>%
						select(RANK,GEOGRAPHY,SII,SII_significance) %>%
						head(10)
			colnames(top_ten) = c("Rank",geo,"SII","Significant")
			top_ten_table = xtable(top_ten, caption = paste("Best performing ",geo,"s in terms of SII: ", rank_year,sep="")) 
			sink(file=paste(geo_method_dir,indicator_name,"_top_ten_table.tex",sep=""))
			print(top_ten_table,size="\\scriptsize",format.args=list(big.mark=","),table.placement="H",include.rownames=FALSE)
			sink()

			bottom_ten = rank %>%
					arrange(INV_RANK) %>%
					select(INV_RANK,GEOGRAPHY,SII,SII_significance) %>%
					head(10)
			colnames(bottom_ten) = c("Rank",geo,"SII","Significant")
			bottom_ten_table = xtable(bottom_ten, caption = paste("Worst performing ",geo,"s in terms of SII: ", rank_year,sep="")) 
			sink(file=paste(geo_method_dir,indicator_name,"_bottom_ten_table.tex",sep=""))
			print(bottom_ten_table,size="\\scriptsize",format.args=list(big.mark=","),table.placement="H",include.rownames=FALSE)
			sink()
		
			for(geo_name in geo_names){
				geo_directory = paste(geo_method_dir,gsub(" ","_",geo_name),"/",sep="")
				# nhs geography level gradient plot
				# geo_gradient = plot_gradient(geo_data %>% filter(GEOGRAPHY==geo_name), indicator)
				# ggsave(filename=paste(geo_directory,indicator_name,"_gradient.png",sep=""),plot=geo_gradient,width=27,height=19,units="cm",dpi=300)
				# nhs geography level panel plot
				geo_panel = plot_quintiles(geo_data %>% filter(GEOGRAPHY==geo_name), indicator)
				ggsave(filename=paste(geo_directory,indicator_name,"_panel.png",sep=""),plot=geo_panel,width=27,height=19,units="cm",dpi=300)
				# nhs geography level population summary table
				for(year in years){
					scatter = plot_geo_scatter2(geo_name, year, indicator, reverse, geo, years=smooth_years)
					ggsave(filename=paste(geo_directory,indicator_name,"_",year,"_scatter.png",sep=""),plot=scatter,width=27,height=19,units="cm",dpi=300)
					#caterpillar = plot_geo_caterpillar(geo_name, year, geo_data, indicator,subset(national_data,YEAR==year)$SII, geo)	
					#ggsave(filename=paste(geo_directory,indicator_name,"_",year,"_caterpillar.png",sep=""),plot=caterpillar,width=27,height=19,units="cm",dpi=300)
				}
			}
		}
	}
	
	for(method in methods){
		national_method_directory = paste(national_output_directory,method,"/",sep="")
		ranks = ranks_by_method[[method]]
		overall_rank = ranks %>%
						select(GEOGRAPHY, RANK) %>%
						group_by(GEOGRAPHY) %>%
						summarise(OVERALL_RANK=sum(RANK)) %>%
						mutate(TOP_RANK=row_number(OVERALL_RANK),BOTTOM_RANK=row_number(-1*OVERALL_RANK))
		write.csv(overall_rank,file=paste(national_method_directory,"overall_table.csv",sep=""))
		
		top_ten = overall_rank %>%
				arrange(TOP_RANK) %>%
				select(TOP_RANK,GEOGRAPHY) %>%
				head(10)
		colnames(top_ten) = c("Rank",geo)
		top_ten_table = xtable(top_ten, caption = paste("Best performing ",geo,"s in terms of RII: ", rank_year,sep="")) 
		sink(file=paste(national_method_directory,"overall_top_ten_table.tex",sep=""))
		print(top_ten_table,size="\\scriptsize",format.args=list(big.mark=","),table.placement="H",include.rownames=FALSE)
		sink()
		
		bottom_ten = overall_rank %>%
				arrange(BOTTOM_RANK) %>%
				select(BOTTOM_RANK,GEOGRAPHY) %>%
				head(10)
		colnames(bottom_ten) = c("Rank",geo)
		bottom_ten_table = xtable(bottom_ten, caption = paste("Worst performing ",geo,"s in terms of RII: ", rank_year,sep="")) 
		sink(file=paste(national_method_directory,"overall_bottom_ten_table.tex",sep=""))
		print(bottom_ten_table,size="\\scriptsize",format.args=list(big.mark=","),table.placement="H",include.rownames=FALSE)
		sink()		

		for(geo_name in geo_names){
			geo_directory = paste(geo_method_dir,gsub(" ","_",geo_name),"/",sep="")
			geo_name_ranks = ranks %>%
					filter(GEOGRAPHY==substr(geo_name,4,nchar(geo_name))) %>%
					select(INDICATOR,SII,RANK,SII_significance)					
#			geo_name_ranks = geo_name_ranks %>% bind_rows(
#					overall_rank %>% 
#							filter(GEOGRAPHY==substr(geo_name,4,nchar(geo_name))) %>%
#							mutate(INDICATOR="Overall Rank",RANK=TOP_RANK,RII=NA) %>%
#							select(INDICATOR,RII,RANK))
			colnames(geo_name_ranks) = c("Indicator","SII","Rank","Significant")
			equity_summary_table = xtable(geo_name_ranks, caption = paste("Equity summary for ",geo_name," in terms of SII: 2011",sep="")) 
			sink(file=paste(geo_directory,"equity_summary_table_2011.tex",sep=""))
			print(equity_summary_table,size="\\scriptsize",format.args=list(big.mark=","),table.placement="H",include.rownames=FALSE)
			sink()		
		}
				
	}
}

### run the code ###

produce_chart_pack_figures = function(trim, smooth_years, methods, geo, geo_names, years, cached){
	## set key parameters
	output_directory = "output/"
	#years = 2011
	#geo_names = c("NHS Hull")
	## geo_names = c("NHS Vale of York",
	##			"NHS City and Hackney",
	##         "NHS North Manchester",
	##         "NHS Central Manchester",
	##         "NHS Newham",
	##         "NHS Norwich",
	##         "NHS Wandsworth",
	##         "NHS Richmond",
	##         "NHS Hambleton, Richmondshire and Whitby",
	##         "NHS Fareham and Gosport",
	##         "NHS Chiltern")
	#geo = "CCG"
	#geo_names = c("North Yorkshire and Humber")
	#geo = "LAT"
	#geo_names = c("York")
	#geo = "LAD"
	#geo_names = c("York")
	#geo = "UTLA"
	
	# extract the data from the database
	data = extract_indicator_data_from_database(geo, cached)
	
	# aggregate the data by indicator
	indicators = aggregate_indicator_data(data[["lsoa_data_wide"]], data[["matrix_data_wide"]])
	indicator_names = names(indicators)
	# create population files for reference
	create_population_ouput(data, output_directory, geo, geo_names, years)
	# create data tables and figures for indicators at national and nhs geography levels
	create_indicators_output(indicators, indicator_names, output_directory, geo, geo_names, years, methods, trim, smooth_years)
}

plot_trimming_distribution = function(title, label, lsoa_data, lower, upper){
	graph_data=lsoa_data
	graph_data$lower_limit = lower
	graph_data$upper_limit = upper
	plot = ggplot(graph_data, aes(x=lsoa_indicator)) + 
			geom_density(fill="darkgrey") + 
			ggtitle(title) + 
			xlab(label) +
			geom_vline(aes(xintercept=min(lower_limit)), colour="red", linetype="dashed")+ 
			geom_vline(aes(xintercept=max(upper_limit)), colour="red", linetype="dashed") +
			theme_bw() +
			theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.title = element_text(lineheight=.8, face="bold", size=rel(1)))
	return(plot)
}

calculate_trim_table = function(indicator, max){
	num_sds = 6
	# extreme outliers
	trimmed = data_frame()
	for(year in unique(indicator[["indicator_data"]]$YEAR)){
		year_indicator = indicator[["indicator_data"]][indicator[["indicator_data"]]$YEAR==year,]
		year_indicator = year_indicator %>% filter(!is.infinite(lsoa_indicator))
		if(max>0){
			year_indicator = year_indicator %>% filter(lsoa_indicator<=max)
		}
		year_indicator = year_indicator %>% filter(lsoa_indicator>=0)
		indicator_mean = mean(year_indicator$lsoa_indicator, na.rm=TRUE)
		indicator_sd = sd(year_indicator$lsoa_indicator, na.rm=TRUE)
		lower_limit = indicator_mean - num_sds*indicator_sd
		upper_limit = indicator_mean + num_sds*indicator_sd
		trimmed = bind_rows(trimmed,year_indicator %>% filter(lsoa_indicator<lower_limit | lsoa_indicator>upper_limit))
		if(year == max(indicator[["indicator_data"]]$YEAR)){
			plot = plot_trimming_distribution(indicator$title, indicator$indicator_label, year_indicator, lower_limit, upper_limit)
		}
	}
	trimmed$title = indicator$title
	trimmed$label = indicator$indicator_label
	return(list(trimmed,plot))
}

create_trim_tables = function(indicator_names){
	data = extract_indicator_data_from_database("CCG", cached=FALSE)
	indicators = aggregate_indicator_data(data[["lsoa_data_wide"]], data[["matrix_data_wide"]])
	trimmed_national = data_frame()
	trimmed_national_dep = data_frame()
	trimmed_ccg = data_frame()
	trimmed_ccg_dep = data_frame()
	national_count = data_frame()
	national_count_dep = data_frame()
	ccg_count = data_frame()
	ccg_count_dep = data_frame()
	trim_table_plots = list()
	for(name in indicator_names){
		indicator = indicators[[name]]
		national_count = bind_rows(national_count,indicator[["indicator_data"]] %>% group_by(YEAR) %>% summarise(TOTAL_COUNT = n()) %>% mutate(INDICATOR_NAME = name))
		national_count_dep = bind_rows(national_count_dep,indicator[["indicator_data"]] %>% mutate(DEPRIVATION_VINGTILE = ceiling((trunc(NORMALISED_RANK*100)+1)/5)) %>% group_by(YEAR, DEPRIVATION_VINGTILE) %>% summarise(TOTAL_COUNT = n()) %>% mutate(INDICATOR_NAME = name))
		ccg_count = bind_rows(ccg_count,indicator[["indicator_data"]] %>% mutate(CCG=GEOGRAPHY) %>% group_by(YEAR, CCG) %>% summarise(TOTAL_COUNT = n()) %>% mutate(INDICATOR_NAME = name))
		ccg_count_dep = bind_rows(ccg_count_dep,indicator[["indicator_data"]] %>% mutate(CCG=GEOGRAPHY, DEPRIVATION_VINGTILE = ceiling((trunc(NORMALISED_RANK*100)+1)/5)) %>% group_by(YEAR, CCG, DEPRIVATION_VINGTILE) %>% summarise(TOTAL_COUNT = n()) %>% mutate(INDICATOR_NAME = name))
		if(name=="PHIS"){
			max=100
		} else if(name=="pat_gp_fte_adj"){
			max=10000
		} else{ 
			max=-1
		}
		trimmed = calculate_trim_table(indicator,max)
		trim_table = trimmed[[1]]
		trim_table_plots[[name]] = trimmed[[2]]
		trimmed_national = bind_rows(trimmed_national,trim_table %>% group_by(YEAR, title, label) %>% summarise(TRIM_COUNT = n()) %>% mutate(INDICATOR_NAME = name))
		trimmed_national_dep = bind_rows(trimmed_national_dep,trim_table %>% mutate(DEPRIVATION_VINGTILE = ceiling((trunc(NORMALISED_RANK*100)+1)/5)) %>% group_by(YEAR, DEPRIVATION_VINGTILE, title, label) %>% summarise(TRIM_COUNT = n()) %>% mutate(INDICATOR_NAME = name))
		trimmed_ccg = bind_rows(trimmed_ccg, trim_table %>% mutate(CCG=GEOGRAPHY) %>% group_by(YEAR, CCG, title, label) %>% summarise(TRIM_COUNT = n()) %>% mutate(INDICATOR_NAME = name))
		trimmed_ccg_dep = bind_rows(trimmed_ccg_dep, trim_table %>% mutate(CCG=GEOGRAPHY, DEPRIVATION_VINGTILE = ceiling((trunc(NORMALISED_RANK*100)+1)/5)) %>% group_by(YEAR, CCG, DEPRIVATION_VINGTILE, title, label) %>% summarise(TRIM_COUNT = n()) %>% mutate(INDICATOR_NAME = name))
	}
	
	trim_panel = arrangeGrob(
					trim_table_plots[[1]],
					trim_table_plots[[2]],
					trim_table_plots[[3]],
					trim_table_plots[[4]],
					trim_table_plots[[5]],
					trim_table_plots[[6]],
					trim_table_plots[[7]],
					trim_table_plots[[8]],
					ncol=3)
	
	ggsave(filename="trim_panel.png",plot=trim_panel,width=40,height=30,units="cm",dpi=300)
	
	national_trim_table = left_join(trimmed_national,national_count,by=c("YEAR", "INDICATOR_NAME")) %>%
			mutate(TRIM_PERCENT = (TRIM_COUNT/TOTAL_COUNT)*100) %>%
			select(title, label,YEAR,TOTAL_COUNT,TRIM_COUNT,TRIM_PERCENT)
	national_dep_trim_table = left_join(trimmed_national_dep,national_count_dep,by=c("YEAR", "DEPRIVATION_VINGTILE", "INDICATOR_NAME")) %>%
			mutate(TRIM_PERCENT = (TRIM_COUNT/TOTAL_COUNT)*100) %>%
			select(title, label,YEAR,DEPRIVATION_VINGTILE,TOTAL_COUNT,TRIM_COUNT,TRIM_PERCENT)
	ccg_trim_table = left_join(trimmed_ccg,ccg_count,by=c("YEAR", "CCG", "INDICATOR_NAME")) %>%
			mutate(TRIM_PERCENT = (TRIM_COUNT/TOTAL_COUNT)*100) %>%
			select(title, label,YEAR,CCG,TOTAL_COUNT,TRIM_COUNT,TRIM_PERCENT)
	ccg_dep_trim_table = left_join(trimmed_ccg_dep,ccg_count_dep,by=c("YEAR", "CCG", "DEPRIVATION_VINGTILE", "INDICATOR_NAME")) %>%
			mutate(TRIM_PERCENT = (TRIM_COUNT/TOTAL_COUNT)*100) %>%
			select(title, label,YEAR,CCG,DEPRIVATION_VINGTILE,TOTAL_COUNT,TRIM_COUNT,TRIM_PERCENT)
	
	trim_deprivation_plot = ggplot(subset(national_dep_trim_table,YEAR==2011), aes(y=TRIM_PERCENT, x=DEPRIVATION_VINGTILE)) +
			geom_bar(stat="identity", position = position_dodge()) +
			facet_wrap(~title,nrow=3,scales="free_y") +
			ylab("Amount of Data Trimmed in Deprivation Vingtile (%)") +
			xlab("Deprivation Vingtile (1 = most deprived)") +
			theme_bw() +
			theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.title = element_text(lineheight=.8, face="bold", size=rel(1)))
	ggsave(filename="trim_deprivation_2011.png",plot=trim_deprivation_plot,width=40,height=30,units="cm",dpi=300)
	
	year_labels = c("01/02","02/03","03/04","04/05","05/06","06/07","07/08","08/09","09/10","10/11","11/12")
	trim_time_plot = ggplot((national_trim_table), aes(y=TRIM_PERCENT, x=YEAR)) +
			geom_line() +
			facet_wrap(~title,nrow=3,scales="free_y") +
			ylab("Amount of Data Trimmed (%)") +
			xlab("Year") +
			scale_x_continuous(breaks=2001:2011, labels=year_labels) +
			theme_bw() +
			theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.title = element_text(lineheight=.8, face="bold", size=rel(1)))
	ggsave(filename="trim_time_series.png",plot=trim_time_plot,width=40,height=30,units="cm",dpi=300)
	
	
	write.csv(national_trim_table,"national_trim_table.csv")
	write.csv(national_dep_trim_table,"national_dep_trim_table.csv")
	write.csv(ccg_trim_table,"ccg_trim_table.csv")
	write.csv(ccg_dep_trim_table,"ccg_dep_trim_table.csv")
}

trimmed_LSOAs = function(indicator, max, num_sds){
	# extreme outliers
	trimmed = data_frame()
	for(year in unique(indicator[["indicator_data"]]$YEAR)){
		year_indicator = indicator[["indicator_data"]][indicator[["indicator_data"]]$YEAR==year,]
		current_trimmed = year_indicator %>% filter(!is.infinite(lsoa_indicator))
		if(max>0){
			current_trimmed = current_trimmed %>% filter(lsoa_indicator<=max)
		}
		current_trimmed = current_trimmed %>% filter(lsoa_indicator>=0)
		indicator_mean = mean(current_trimmed$lsoa_indicator, na.rm=TRUE)
		indicator_sd = sd(current_trimmed$lsoa_indicator, na.rm=TRUE)
		lower_limit = indicator_mean - num_sds*indicator_sd
		upper_limit = indicator_mean + num_sds*indicator_sd
		current_trimmed = current_trimmed %>% filter(lsoa_indicator>=lower_limit & lsoa_indicator<=upper_limit)
		current_trimmed = setdiff(year_indicator,current_trimmed)
		current_trimmed = current_trimmed %>%
				mutate(INDICATOR=indicator[["title"]], LAD=GEOGRAPHY, LOWER_LIMIT=lower_limit, UPPER_LIMIT=upper_limit)
		trimmed = bind_rows(trimmed, current_trimmed)
	}
	return(trimmed)
}

create_LAD_trim_tables = function(indicator_names){
	data = extract_indicator_data_from_database("LAD", cached=TRUE)
	indicators = aggregate_indicator_data(data[["lsoa_data_wide"]], data[["matrix_data_wide"]])
	trimmed_lad = data_frame()
	for(name in indicator_names){
		indicator = indicators[[name]]
		num_sds = 6
		if(name=="PHIS"){
			max=100
			num_sds = 3
		} else if(name=="pat_gp_fte_adj"){
			max=10000
		} else{ 
			max=-1
		}
		trimmed = trimmed_LSOAs(indicator,max,num_sds) 
		trimmed_lad = bind_rows(trimmed_lad, trimmed)
	}
	
	trimmed_lad = trimmed_lad %>% arrange(YEAR,INDICATOR,LAD,LSOA11CD)
	write.csv(trimmed_lad,"lad_trim_table.csv")
}
#methods = c("OLS_stratified_robust","OLS_stratified","OLS","neg_bin","poisson","log_linear","OLS_deciles")
lad_list = c("Adur", "Allerdale", "Amber Valley", "Arun", "Ashfield", "Ashford", "Aylesbury Vale", 
		"Babergh", "Barking and Dagenham", "Barnet", "Barnsley", "Barrow-in-Furness", "Basildon", 
		"Basingstoke and Deane", "Bassetlaw", "Bath and North East Somerset", "Bedford", "Bexley", 
		"Birmingham", "Blaby", "Blackburn with Darwen", "Blackpool", "Bolsover", "Bolton", "Boston", 
		"Bournemouth", "Bracknell Forest", "Bradford", "Braintree", "Breckland", "Brent", "Brentwood", 
		"Brighton and Hove", "Bristol, City of", "Broadland", "Bromley", "Bromsgrove", "Broxbourne", 
		"Broxtowe", "Burnley", "Bury", "Calderdale", "Cambridge", "Camden", "Cannock Chase", "Canterbury", 
		"Carlisle", "Castle Point", "Central Bedfordshire", "Charnwood", "Chelmsford", "Cheltenham", 
		"Cherwell", "Cheshire East", "Cheshire West and Chester", "Chesterfield", "Chichester", "Chiltern", 
		"Chorley", "Christchurch", "City of London", "Colchester", "Copeland", "Corby", "Cornwall", "Cotswold",
		"County Durham", "Coventry", "Craven", "Crawley", "Croydon", "Dacorum", "Darlington", "Dartford", 
		"Daventry", "Derby", "Derbyshire Dales", "Doncaster", "Dover", "Dudley", "Ealing", "East Cambridgeshire",
		"East Devon", "East Dorset", "East Hampshire", "East Hertfordshire", "East Lindsey", 
		"East Northamptonshire", "East Riding of Yorkshire", "East Staffordshire", "Eastbourne", 
		"Eastleigh", "Eden", "Elmbridge", "Enfield", "Epping Forest", "Epsom and Ewell", "Erewash", "Exeter", 
		"Fareham", "Fenland", "Forest Heath", "Forest of Dean", "Fylde", "Gateshead", "Gedling", "Gloucester",
		"Gosport", "Gravesham", "Great Yarmouth", "Greenwich", "Guildford", "Hackney", "Halton", "Hambleton",
		"Hammersmith and Fulham", "Harborough", "Haringey", "Harlow", "Harrogate", "Harrow", "Hart", 
		"Hartlepool", "Hastings", "Havant", "Havering", "Herefordshire, County of", "Hertsmere", "High Peak", 
		"Hillingdon", "Hinckley and Bosworth", "Horsham", "Hounslow", "Huntingdonshire", "Hyndburn", "Ipswich",
		"Isle of Wight", "Islington", "Kensington and Chelsea", "Kettering", 
		"King's Lynn and West Norfolk", "Kingston upon Hull, City of", "Kingston upon Thames", "Kirklees", 
		"Knowsley", "Lambeth", "Lancaster", "Leeds", "Leicester", "Lewes", "Lewisham", "Lichfield", "Lincoln", 
		"Liverpool", "Luton", "Maidstone", "Maldon", "Malvern Hills", "Manchester", "Mansfield", "Medway", 
		"Melton", "Mendip", "Merton", "Mid Devon", "Mid Suffolk", "Mid Sussex", "Middlesbrough", "Milton Keynes", 
		"Mole Valley", "New Forest", "Newark and Sherwood", "Newcastle upon Tyne", "Newcastle-under-Lyme", 
		"Newham", "North Devon", "North Dorset", "North East Derbyshire", "North East Lincolnshire", 
		"North Hertfordshire", "North Kesteven", "North Lincolnshire", "North Norfolk", "North Somerset", 
		"North Tyneside", "North Warwickshire", "North West Leicestershire", "Northampton", "Northumberland", 
		"Norwich", "Nottingham", "Nuneaton and Bedworth", "Oadby and Wigston", "Oldham", "Oxford", "Pendle", 
		"Peterborough", "Plymouth", "Poole", "Portsmouth", "Preston", "Purbeck", "Reading", "Redbridge", 
		"Redcar and Cleveland", "Redditch", "Reigate and Banstead", "Ribble Valley", "Richmond upon Thames", 
		"Richmondshire", "Rochdale", "Rochford", "Rossendale", "Rother", "Rotherham", "Rugby", "Runnymede", 
		"Rushcliffe", "Rushmoor", "Rutland", "Ryedale", "Salford", "Sandwell", "Scarborough", "Sedgemoor", 
		"Sefton", "Selby", "Sevenoaks", "Sheffield", "Shepway", "Shropshire", "Slough", "Solihull", 
		"South Bucks", "South Cambridgeshire", "South Derbyshire", "South Gloucestershire", "South Hams", 
		"South Holland", "South Kesteven", "South Lakeland", "South Norfolk", "South Northamptonshire", 
		"South Oxfordshire", "South Ribble", "South Somerset", "South Staffordshire", "South Tyneside", 
		"Southampton", "Southend-on-Sea", "Southwark", "Spelthorne", "St Albans", "St Edmundsbury", 
		"St. Helens", "Stafford", "Staffordshire Moorlands", "Stevenage", "Stockport", "Stockton-on-Tees", 
		"Stoke-on-Trent", "Stratford-on-Avon", "Stroud", "Suffolk Coastal", "Sunderland", "Surrey Heath", 
		"Sutton", "Swale", "Swindon", "Tameside", "Tamworth", "Tandridge", "Taunton Deane", "Teignbridge", 
		"Telford and Wrekin", "Tendring", "Test Valley", "Tewkesbury", "Thanet", "Thurrock", 
		"Tonbridge and Malling", "Torbay", "Torridge", "Tower Hamlets", "Trafford", "Tunbridge Wells", 
		"Uttlesford", "Vale of White Horse", "Wakefield", "Walsall", "Waltham Forest", "Wandsworth", 
		"Warrington", "Warwick", "Watford", "Waveney", "Waverley", "Wealden", "Wellingborough", 
		"Welwyn Hatfield", "West Berkshire", "West Devon", "West Dorset", "West Lancashire", "West Lindsey", 
		"West Oxfordshire", "West Somerset", "Westminster", "Weymouth and Portland", "Wigan", "Wiltshire", 
		"Winchester", "Windsor and Maidenhead", "Wirral", "Woking", "Wokingham", "Wolverhampton", "Worcester", 
		"Worthing", "Wychavon", "Wycombe", "Wyre", "Wyre Forest", "York")
produce_chart_pack_figures(trim=TRUE, smooth_years=1, methods=c("OLS_stratified"), geo="CCG", geo_names=c("NHS Central Manchester"), years=c(2011), cached=TRUE)
#produce_chart_pack_figures(trim=TRUE, smooth_years=1, methods=c("OLS_stratified"), geo="LAD", geo_names=lad_list, years=c(2011), cached=TRUE)
#produce_chart_pack_figures(trim=TRUE, smooth_years=1, methods=c("OLS_stratified"), geo="UTLA", geo_names=c("York"), years=c(2011), cached=TRUE)
#final_indicator_list = c("pat_gp_fte_adj","PHIS","waiting_time_adj","unplanned_hospitalisations_adj","repeat_hosp_adj","hosp_death","amenable_mortality_adj","all_mortality_adj")
#create_trim_tables(final_indicator_list)
#create_LAD_trim_tables(final_indicator_list)
