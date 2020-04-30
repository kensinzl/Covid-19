# Load required package
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)

library(magrittr) # http://blog.fens.me/r-magrittr/
library(dplyr)
library(reshape2)

library(leaflet) # interactive maps, https://rstudio.github.io/leaflet/
library(ggplot2)
library(plotly)

library(geojsonio)
library(data.table)

library(stringr)
library(nlme)


# setting the color
covid_col = "#cc4c02"  # base map brown
covid_cul = "#403d3d"       #accumulate case gray dark_world
covid_new = "#ff0400"       #accumulate new case red_world

covid_other_col = "#662506"
sars_col = "#045a8d"
h1n1_col = "#4d004b"
ebola_col = "#016c59"



# setting the log file
#addHandler(handler = writeToFile, logger="System", file="system.log")


world_country_date_by_define_case = function(input, file_name) {
	# init missing data check
	if (sum(is.na(input))/(sum(!is.na(input))+sum(is.na(input))) >= 0.1) { 
		stop(paste0("Error: Too much missing data for the covid-19 ", switch(file_name, 'Case' = 'Case', 'Recovery' = 'Recovery', 'Death' = 'Death'), " file. ")) 
	}
	# rought inset missing value
	input[is.na(input)] = 0

	# Consider the Lat and Long is based on the province unit and not complete. 
	# In this instance, remove the Lat and Long
	input = input %>% select(-c('Lat', 'Long'))

	# -------------------------------------------------
	# | Province/State | Country/Region | Date | Value | 
	# -------------------------------------------------
	input_by_date = reshape2::melt(input, id = c('Province/State', 'Country/Region'))

	# This Case param is accumulate case
	# -------------------------------------------------
	# | Province | Country | Date | (Accumulate) Case | 
	# -------------------------------------------------
	names(input_by_date) = c('Province', 'Country', 'Date', 'Case')

	# Notice: Taiwan should be one part of China. One China Policy!!! Zero toleration.
	input_by_date$Province[input_by_date$Country=="Taiwan*"] = 'Taiwan'
	input_by_date$Country[input_by_date$Country=="Taiwan*"] = 'Taiwan'
	# country name change
	input_by_date$Country[input_by_date$Country=="Korea, South"] = "Republic of Korea"
	input_by_date$Country[input_by_date$Country=="Congo (Brazzaville)" | input_by_date$Country=="Republic of the Congo"] = "Republic of the Congo"
	input_by_date$Country[input_by_date$Country=="Congo (Kinshasa)"] = "Democratic Republic of the Congo"
	input_by_date$Country[input_by_date$Country=="Gambia"] = "The Gambia"
	input_by_date$Country[input_by_date$Country=="Bahamas"] = "The Bahamas"
	input_by_date$Country[input_by_date$Country=="Czechia"] = "Czech Republic"
	input_by_date$Country[input_by_date$Country=="Holy See"] = "Holy See (Vatican City)"
	input_by_date$Country[input_by_date$Country=="Iran"] = "Iran (Islamic Republic of)"
	input_by_date$Country[input_by_date$Country=="Laos"] = "Lao People's Democratic Republic"
	input_by_date$Country[input_by_date$Country=="Moldova"] = "Moldova, Republic of"
	input_by_date$Country[input_by_date$Country=="Laos"] = "Lao People's Democratic Republic"
	input_by_date$Country[input_by_date$Country=="Tanzania"] = "Tanzania, United Republic of"
	input_by_date$Country[input_by_date$Country=="United Kingdom"] = "UK"
	input_by_date$Country[input_by_date$Country=="US"] = "USA"
	input_by_date$Country[input_by_date$Country=="China"] = "Mainland China"

	# ------------------------------------
	# | Country | Date | Accumulate Case | 
	# ------------------------------------
	world_cases_country_date = input_by_date %>% group_by(Country, Date) %>% summarise('Accumulate_Case' = sum(Case))

	# -----------------------------------------------
	# | Country | Date | Accumulate Case | New Case | 
	# -----------------------------------------------
	unique_country = unique(world_cases_country_date$Country)
	country_case_with_new_by_date_list = lapply(unique_country, function(country_name) {
		country_case_by_date = world_cases_country_date[world_cases_country_date$Country == country_name, ]
		country_case_by_date$New_Case = country_case_by_date$Accumulate_Case - c(country_case_by_date$Accumulate_Case[1], country_case_by_date$Accumulate_Case[1:length(country_case_by_date$Accumulate_Case)-1])
		country_case_by_date
	})
	world_cases_with_new_country = data.frame(do.call('rbind', country_case_with_new_by_date_list))
	# check the global accumulate number
	param1 = sum(input[,ncol(input)])
	latest_day = names(input)[ncol(input)]
	param2 = sum((world_cases_country_date %>% filter(Date == latest_day))$Accumulate_Case)
	if(param1 == param2) {
		print(str_c('Until ', latest_day, ', the global ', switch(file_name, 'Case' = 'Case', 'Recovery' = 'Recovery', 'Death' = 'Death'), ' is ', param2, ' .'))
	} else {
		stop(str_c('Global ', switch(file_name, 'Case' = 'Case', 'Recovery' = 'Recovery', 'Death' = 'Death'), ' ', param1, ' should be equal to ', param2, ' .')) 
	}

	# -----------------------------------------------
	# | Country | Date | Accumulate Case | New Case | 
	# -----------------------------------------------
	world_cases_with_new_country
}


##############################################################
#                      Import Data                           #
##############################################################
# Note: data.table:fread is more effeicient for csv: https://www.r-bloggers.com/how-data-tables-fread-can-save-you-a-lot-of-time-and-memory-and-take-input-from-shell-commands/
# I take Shanghai of China as an example, and this data seems robust.
# The Lat and Long is basing the provice of this country.
# ----------------------------------------------------------
# | Province/State | Country/Region | Lat | Long | Date... |
# ----------------------------------------------------------
world_cases = as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))
print(str_c("The country name length(include ship): ", length(unique(world_cases$`Country/Region`))))

# ----------------------------------------------------------
# | Province/State | Country/Region | Lat | Long | Date... |
# ----------------------------------------------------------
world_death = as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"))
# ----------------------------------------------------------
# | Province/State | Country/Region | Lat | Long | Date... |
# ----------------------------------------------------------
world_recovery = as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"))

# they should have country consistence
if((length(unique(world_cases$`Country/Region`)) != length(unique(world_death$`Country/Region`))) || (length(unique(world_cases$`Country/Region`)) != length(unique(world_recovery$`Country/Region`)))) {
	stop(paste0("Error: Data import has inconsistence !!!!!")) 
}
if(sum(unique(world_cases$`Country/Region`) %in% unique(world_death$`Country/Region`)) != sum(unique(world_recovery$`Country/Region`) %in% unique(world_death$`Country/Region`))) {
	stop(paste0("Error: Data import county has inconsistence !!!!!")) 
}

# -----------------------------------------------
# | Country | Date | Accumulate Case | New Case | 
# -----------------------------------------------
world_cases_date_with_new = world_country_date_by_define_case(world_cases, 'Case')

# ------------------------------------------------
# | Country | Date | Accumulate_Death | New Death | 
# ------------------------------------------------
world_death_date_with_new = world_country_date_by_define_case(world_death, 'Death')
names(world_death_date_with_new) = c('Country', 'Date', 'Accumulate_Death', 'New_Death')

# ------------------------------------------------------
# | Country | Date | Accumulate_Recovery | New Recovery | 
# ------------------------------------------------------
world_recovery_with_new = world_country_date_by_define_case(world_recovery, 'Recovery')
names(world_recovery_with_new) = c('Country', 'Date', 'Accumulate_Recovery', 'New_Recovery')


# before merging, check again
if(nrow(world_cases_date_with_new) != nrow(world_death_date_with_new) || nrow(world_death_date_with_new) != nrow(world_recovery_with_new)) {
	stop(paste0("Error: Data maniputaion has inconsistence !!!!!")) 
}


# merging
temp = inner_join(world_cases_date_with_new, world_death_date_with_new, by = c("Country", "Date"))
# -------------------------------------------------------------------------------------------------------------------
# | Country | Date | Accumulate Case | New Case | Accumulate_Death | New_Death | Accumulate_Recovery | New_Recovery |
# -------------------------------------------------------------------------------------------------------------------
cv_cases = inner_join(temp, world_recovery_with_new, by = c("Country", "Date"))
# Accumulate Case = Accumulate_Death + Accumulate_Recovery + Active_Case
cv_cases$Active_Case = cv_cases$Accumulate_Case - cv_cases$Accumulate_Death - cv_cases$Accumulate_Recovery


##############################################################
#         Import country alpha3 and population               #
##############################################################
# I need the countryname, alpha3, population, latitude and logitude
countries = read.csv("input_data/countries_codes_and_coordinates.csv")

# Just easy to join
names(countries)[2] = 'Country'
print(str_c("countries' country name length(include HK, Macao and Taiwan): ", length(unique(countries$Country))))

# ensure get the not null data
countries = countries %>% filter(!is.na(population)) %>% filter(!is.na(alpha3)) %>% filter(!is.na(latitude)) %>% filter(!is.na(longitude))
print(str_c("After removing missing data, countries' country name length : ", length(unique(countries$Country))))

print("The following countries from cv_cases not be includeed in countries to get population, latitude and longitude. They can be ignored !!")
# Cabo Verde, Diamond Princess, MS Zaandam and West Bank and Gaza
print(cv_cases %>% filter(!(Country %in% countries$Country)) %>% select(Country) %>% unique)


##############################################################
#                      Add population, lat and lang          #
##############################################################

# Just notice
# class(countries$Country) => factor, then if you use inner_join's by with character. you would get joining character vector and factor, coercing into character vector
countries$Country = as.character(countries$Country)
# length(unique(cv_cases$Country)) => 184
# length(unique((inner_join(cv_cases, countries, by = "Country"))$Country)) => 169
# some country is gone, will do some stupid manual fix
# target_countries = unique(cv_cases$Country)
# do.call('rbind', lapply(target_countries, function(country_name) {
#   if(nrow(countries %>% filter(Country == country_name)) == 0) {
#     country_name
#   } else {
#     NULL
#   }
# }))



# Cabo Verde, Diamond Princess, MS Zaandam and West Bank and Gaza which is valid in cv_cases but not in countries, would be removed in inner_join
cv_cases = inner_join(cv_cases, countries, by = "Country")
# put Taiwan, HK, Mac into (Mainland) China
mainLand = cv_cases %>% filter(Country == "Mainland China")
taiwan = countries %>% filter(Country == "Taiwan")
HK = countries %>% filter(Country == "Hong Kong")
Macao = countries %>% filter(Country == "Macao")
mainLand$population = mainLand$population + taiwan$population + HK$population + Macao$population

cv_cases[cv_cases$Country == "Mainland China", 'population'] = mainLand$population
cv_cases[cv_cases$Country == "Mainland China", 'Country'] = "China"

# remove taiwan record
cv_cases = cv_cases %>% filter(Country != 'Taiwan')

# 100K per index
cv_cases$per100k = as.numeric(format(round(cv_cases$Accumulate_Case/(cv_cases$population/100000), 1), nsmall = 1))
cv_cases$newper100k = as.numeric(format(round(cv_cases$New_Case/(cv_cases$population/100000),1), nsmall=1))
cv_cases$activeper100k = as.numeric(format(round(cv_cases$Active_Case/(cv_cases$population/100000),1), nsmall=1))

# sum cv case counts by date
# cv_aggregated => draw the bar and line from histroy to defined plot day tracing
cv_aggregated = cv_cases %>% group_by(Date) %>% summarise(Accumulate_Case = sum(Accumulate_Case), New_Case = sum(New_Case), New_Recovery = sum(New_Recovery), New_Death = sum(New_Death))
cv_aggregated$Date = as.Date(cv_aggregated$Date,"%m/%d/%y") # fix the Date factor
cv_aggregated$Region = "Global"

# current_date = as.Date(max(cv_cases$date), "%Y-%m-%d")
cv_cases$Date = as.Date(cv_cases$Date,"%m/%d/%y")
today_cv = cv_cases %>% filter(Date == max(cv_cases$Date))
cv_max_date = today_cv$Date[1]
cv_min_date = min(cv_cases$Date)

# Temp way to fix one stange
#cv_aggregated[1, c(3:5)] = 0


# load the geo info
worldcountry = geojson_read("input_data/countries.geojson", what = "sp")
worldcountry_df = data.frame(Country = worldcountry$ADMIN, alpha2 = worldcountry$ISO_A2, alpha3 = worldcountry$ISO_A3)

# the name is different but actually exist
# cv_cases %>% filter(!(Country %in% worldcountry_df$Country)) %>% select(Country, alpha3) %>% unique %>% nrow
# using the alpha code to fetch
print('The following countries exist in cv_cases but without geo information')
print(cv_cases %>% filter(!(alpha3 %in% worldcountry_df$alpha3)) %>% select(Country, alpha3) %>% unique)
# remove Kosovo record
cv_cases = cv_cases %>% filter(Country != 'Kosovo')

# just select the column what we need
cv_cases = cv_cases %>% select("Country", "Date", "Accumulate_Case", "New_Case", "Accumulate_Death", "New_Death", 
	"Accumulate_Recovery", "New_Recovery", "Active_Case", "alpha3", "latitude", "longitude", "population", "per100k", "newper100k", "activeper100k")

print("-------- Until now all cv_cases contry with valid geo info in worldcountry, but please using alpha3 -------- ")




# # data clean
# if (sum(is.na(world_cases))/(sum(!is.na(world_cases))+sum(is.na(world_cases))) >= 0.1) { 
# 	stop(paste0("Error: Too much missing data for the covid-19 CSV file. ")) 
# }
# world_cases[is.na(world_cases)] = 0

# # Consider the Lat and Long is based on the province unit and not complete. 
# # In this instance, remove the Lat and Long
# world_cases = world_cases %>% select(-c('Lat', 'Long'))

# world_cases_by_date = reshape2::melt(world_cases, id =c('Province/State', 'Country/Region'))
# # This Case param is accumulate case
# # -------------------------------------------------
# # | Province | Country | Date | (Accumulate) Case | 
# # -------------------------------------------------
# names(world_cases_by_date) = c('Province', 'Country', 'Date', 'Case')

# # Notice: Taiwan should be one part of China. One China Policy!!! Zero toleration.
# world_cases_by_date$Province[world_cases_by_date$Country=="Taiwan*"] = 'Taiwan'
# world_cases_by_date$Country[world_cases_by_date$Country=="Taiwan*"] = 'China'
# # country name change
# world_cases_by_date$Country[world_cases_by_date$Country=="Korea, South"] = "Republic of Korea"
# world_cases_by_date$Country[world_cases_by_date$Country=="Congo (Brazzaville)" | world_cases_by_date$Country=="Republic of the Congo"] = "Congo"
# world_cases_by_date$Country[world_cases_by_date$Country=="Congo (Kinshasa)"] = "Democratic Republic of the Congo"
# world_cases_by_date$Country[world_cases_by_date$Country=="Gambia, The"] = "The Gambia"
# world_cases_by_date$Country[world_cases_by_date$Country=="Bahamas, The"] = "The Bahamas"

# # ------------------------------------
# # | Country | Date | Accumulate Case | 
# # ------------------------------------
# world_cases_country = world_cases_by_date %>% group_by(Country, Date) %>% summarise('Accumulate_Case' = sum(Case))

# # -----------------------------------------------
# # | Country | Date | New Case | Accumulate Case | 
# # -----------------------------------------------
# unique_country = unique(world_cases_country$Country)
# country_case_by_date_list = lapply(unique_country, function(country_name) {
# 	country_case_by_date = world_cases_country[world_cases_country$Country == country_name, ]
# 	country_case_by_date$New_Cases = country_case_by_date$Accumulate_Case - c(country_case_by_date$Accumulate_Case[1], country_case_by_date$Accumulate_Case[1:length(country_case_by_date$Accumulate_Case)-1])
# 	country_case_by_date
# })
# world_cases_country = data.frame(do.call('rbind', country_case_by_date_list))
# # check the global accumulate number
# param1 = sum(world_cases[,ncol(world_cases)])
# latest_day = names(world_cases)[ncol(world_cases)]
# param2 = sum(world_cases_country %>% filter(Date == latest_day) %>% select(Accumulate_Case))
# if(param1 == param2) {
# 	print(str_c('Until ', latest_day, ', the global cases is ', param2, ' .'))
# } else {
# 	stop(str_c('Global cases1 ', param1, ' should be equal to ', param2, ' .')) 
# }


# # ----------------------------------------------------------
# # | Province/State | Country/Region | Lat | Long | Date... |
# # ----------------------------------------------------------
# world_death = as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"))


# world_recovery = as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"))











# # Accumulate Case = Active Case + Accumulate Recovery + Accumulate Death
# # ----------------------------------------------------------------------------------------------------------------------------------------------
# # | Country | Date | Lat | Long | New Case | Active Case | New Recovery | New Death | Accumulate Case | Accumulate Recovery | Accumulate Death |
# # ----------------------------------------------------------------------------------------------------------------------------------------------






# # -----------------------------------------------
# # | Country | Date | New Case | Accumulate Case | 
# # -----------------------------------------------
# world_cases_country$New_Cases = world_cases_country$Accumulate_Case - c(world_cases_country$Accumulate_Case[1], world_cases_country$Accumulate_Case[1:(length(world_cases_country$Accumulate_Case)-1)])








# Note: data.table:fread is more effeicient for csv: 
# https://www.r-bloggers.com/how-data-tables-fread-can-save-you-a-lot-of-time-and-memory-and-take-input-from-shell-commands/
# dim() => see the row and columns
# mode() => basic structure like numeric, complex, character and logical, class() is not basic structure like data.frame, matrix
#cv_cases = as.data.frame(data.table::fread("input_data/coronavirus.csv"))
# countries = read.csv("input_data/countries_codes_and_coordinates.csv")
# worldcountry = geojson_read("input_data/countries.geo.json", what = "sp")


##############################################################
#                      Import Data Check                     #
##############################################################
# covid-19 country should be all in the world country name
# if (all(unique(cv_cases$Country) %in% countries$country) == FALSE) {
#   print("Error: inconsistent country names")
# }

# remove the NULL population country
# countries %>% filter(is.na(population)) => no important tiny island
# countries = countries[!is.na(countries$population), ]

# 



##############################################################
#               Set mapping colour for each outbreak         #
##############################################################
# covid_col = "#cc4c02"
# covid_cul = "#111111"       #brown
# covid_new = "#f63621"       #red
# covid_other_col = "#662506"
# sars_col = "#045a8d"
# h1n1_col = "#4d004b"
# ebola_col = "#016c59"


# ##############################################################
# #                      Data Manipulation                     #
# ##############################################################
# cv_cases = merge(cv_cases, countries, by = "country")
# cv_cases$per100k = as.numeric(format(round(cv_cases$cases/(cv_cases$population/100000), 1), nsmall = 1))
# cv_cases$newper100k = as.numeric(format(round(cv_cases$new_cases/(cv_cases$population/100000),1), nsmall=1))
# cv_cases$activeper100k = as.numeric(format(round(cv_cases$active_cases/(cv_cases$population/100000),1), nsmall=1))


# # sum cv case counts by date
# # cv_cases %>% group_by(date) %>% summarise(sum(cases))
# # cv_aggregated = aggregate(cv_cases$cases, by=list(Category=cv_cases$date), FUN=sum)

# cv_aggregated = cv_cases %>% group_by(date) %>% summarise(cases = sum(cases), new_cases = sum(new_cases), new_recovered = sum(new_recovered), new_deaths = sum(new_deaths))
# names(cv_aggregated) = c("date", "cases", "new_cases", "new_recovered", "new_deaths")
# cv_aggregated$region = "Global"
# cv_aggregated$date = as.Date(cv_aggregated$date,"%Y-%m-%d")
# # Temp way to fix one stange
# cv_aggregated[1, c(3:5)] = 0





# # current_date = as.Date(max(cv_cases$date), "%Y-%m-%d")
# today_cv = cv_cases %>% filter(date == max(cv_cases$date))
# cv_max_date = today_cv$date[1]
# cv_min_date = min(cv_cases$date)

