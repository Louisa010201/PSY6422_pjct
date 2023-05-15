# Global Unmet Need for Contraception in Women of Reproductive Age Over Time

# install.packages("here")
# install.packages("tidyverse")
# install.packages("gifski")

library(here)
library(tidyverse)
library(gganimate)
library(gifski)

# --------IMPORT DATA-----------
# first go to the file "data" and then "Additional Codebook.txt" for information on how to download and save the data

# load data
df <- read_csv(here("data", "data.csv")) 
head(df) 

# created new column names
col_names <- c("measure", "location_id", "location", "sex_id", "sex", "age_group_id", "age_group","metric", "year", "mean", "upper", "lower")             
colnames(df) <- col_names 

# ---------CLEANING DATA-------------
cf <- df %>%
  filter(measure != 'Any contraceptive prevalence', measure != 'Modern contraceptive prevalence', measure != 'Demand satisfied with modern methods', measure != 'Female sterilization prevalence', measure != 'Male sterilization prevalence', measure != 'Injections prevalence', measure != 'IUD prevalence', measure != 'Implants prevalence', measure != 'Pill prevalence', measure != 'Condom prevalence', measure != 'Diaphragm prevalence', measure != 'Emergency contraception prevalence', measure != 'Other modern methods prevalence', measure != 'Lactional amenorrhea method prevalence', measure != 'Rhythm prevalence', measure != 'Withdrawal prevalence', measure != 'Other traditional methods prevalence') %>%   #removed unneeded measures 
  filter(age_group != '15-19 years', age_group != '20-24 years', age_group != '25-29 years', age_group != '30-34 years', age_group != '35-39 years', age_group != '40-44 years', age_group != '45-49 years', age_group != 'Age-standardized (15-49 years)') %>% #removed unneeded age ranges
  mutate(age_group=str_sub(age_group, start = 10, end = 20)) %>% #tidied row
  subset(select = -c(location_id, sex_id, sex, metric, age_group_id, upper, lower)) #removed unneeded columns 
head(cf)
# save clean data
write.csv(cf, here("data", "processed", "clean_data.csv")) 

# --------DATA PREPARATION----------
# creating data frames for regions, assigning to new column
central_europe_eastern_europe_central_asia <- cf %>%
  filter(location == "Armenia"| location == "Azerbaijan"| location == "Georgia"| location == "Kazakhstan"| location == "Kyrgyzstan"| location == "Mongolia"| location == "Tajiikistan"| location == "Turkmenistan"| location =="Uzbekistan"| location == "Albania"| location == "Bosnia and Herzegovina"| location == "Bulgaria"| location == "Croatia"| location == "Hungary"| location == "Montenegro"| location == "North Macedonia"| location == "Poland"| location == "Romania"| location == "Serbia"| location == "Slovakia"| location == "Slovenia"| location == "Belarus"| location == "Estnoia"| location == "Latvia"| location == "Lithuania"| location == "Moldova"| location == "Russia"| location == "Ukraine")
central_europe_eastern_europe_central_asia$region <- "central europe, eastern europe and central asia" 

high_income <- cf %>%
  filter(location == "Australia"| location == "New Zealand"| location == "Brunei"| location == "Japan"| location == "Singapore"| location == "South Korea"| location == "Canada"| location == "Greenland"| location == "United States of America"| location == "Argentina"| location == "Chile"| location == "Uruguay"| location == "Andorra"| location == "Austria"| location == "Belgium"| location == "Cyprus"| location == "Denmark"| location == "Finland"| location == "France"| location == "Germany"| location == "Greece"| location == "Iceland"| location == "Ireland"| location == "Israel"| location == "Italy"| location == "Luxembourg"| location == "Malta"| location == "Monaco"| location == "Netherlands"| location == "Norway"| location == "Portugal"| location == "San Marino"| location == "Spain"| location == "Sweden"| location == "Switzerland"| location == "United Kingdom")
high_income$region <- "high income"

latin_america_caribbean <- cf %>%
  filter(location == "Bolivia"| location == "Ecuador"| location == "Peru"| location == "Caribbean"| location == "Antigua and Barbuda"| location == "Bahamas"| location == "Barbados"| location == "Belize"| location == "Bermuda"| location == "Cuba"| location == "Dominica"| location == "Dominican Republic"| location == "Grenada"| location == "Guyana"| location == "Haiti"| location == "Jamaica"| location == "Puerto Rico"| location == "Saint Kitts and Nevis"| location == "Saint Lucia"| location == "Saint Vicent and the Grenadines"| location == "Suriname"| location == "Trinidad and Tobago"| location == "Virgin Islands"| location == "Colombia"| location == "Costa Rica"| location == "El Salvador"| location == "Guatemala"| location == "Mexico"| location == "Nicaragua"| location == "Panama"| location == "Venezeula"| location == "Brazil"| location == "Paraguay")
latin_america_caribbean$region <- "latin america and caribbean"

north_africa_middle_east <- cf %>%
  filter(location == "Afghanistan"| location == "Algeria"| location == "Bahrain"| location == "Egypt"| location == "Iran"| location == "Jordan"| location == "Kuwait"| location == "Lebanon"| location == "Libya"| location == "Morocco"| location == "Oman"| location == "Palestine"| location == "Qatar"| location == "Saudi Arabia"| location == "Sudan"| location == "Syria"| location == "Tunisia"| location == "Turkey"| location == "United Arab Emirates"| location == "Yemen")
north_africa_middle_east$region <- "north africa and middle east"

south_asia <- cf %>%
  filter(location == "Bangladesh"| location == "Bhutan"| location == "India"| location == "Nepal"| location == "Pakistan")
south_asia$region <- "south asia"

southeast_asia_east_asia_oceania <- cf %>% 
  filter(location == "China"| location == "North Korea"| location == "Taiwan"| location == "American Samoa"| location == "Cook Islands"| location == "Micronesia"| location == "Fiji"| location == "Guam"| location == "Kiribati"| location == "Marshall Islands"| location == "Nauru"| location == "Niue"| location == "Northen Mariana Islands"| location == "Palau"| location == "Papua New Guinea"| location == "Samoa"| location == "Solomon Islands"| location == "Tokelau"| location == "Tonga"| location == "Tuvalu"| location == "Vanuatu"| location == "Cambodia"| location == "Indonesia"| location == "Laos"| location == "Malaysia"| location == "Maldives"| location == "Mauritius"| location == "Myanmar"| location == "Philippines"| location == "Seychelles"| location == "Sri Lanka"| location == "Thailand"| location == "Timor-Leste"| location == "Vietnam")
southeast_asia_east_asia_oceania$region <- "southest asia, east asia and oceania"

subsaharan_africa <- cf %>%
  filter(location == "Congo"| location == "Central African Republic"| location == "Congo"| location == "Democratic Republic of the Congo"| location == "Equatorial Guinea"| location == "Gabon"| location == "Burundi"| location == "Comoros"| location == "Djibouti"| location == "Eritrea"| location == "Ethiopia"| location == "Kenya"| location == "Madagascar"| location == "Malawi"| location == "Mozambique"| location == "Rwanda"| location == "Somalia"| location == "South Sudan"| location == "Tanzania"| location == "Uganda"| location == "Zambia"| location == "Botswana"| location == "Eswatini"| location == "Lesotho"| location == "Namibia"| location == "South Africa"| location == "Zimbabwe"| location == "Benin"| location == "Burkina Faso"| location == "Cameroon"| location == "Cape Verde"| location == "Chad"| location == "Côte d'Ivoire"| location == "Gambia"| location == "Ghana"| location == "Guinea"| location == "Guinea-Bissau"| location == "Liberia"| location == "Mali"| location == "Mauritania"| location == "Niger"| location == "Nigeria"| location == "Sao Tome and Principe"| location == "Senegal"| location == "Sierra Leone"| location == "Toga")
subsaharan_africa$region <- "subsaharan africa"

# joining data frames 
cf1 <- rbind(central_europe_eastern_europe_central_asia, high_income, latin_america_caribbean, north_africa_middle_east, south_asia, southeast_asia_east_asia_oceania, subsaharan_africa) 

# --------PLOTING DATA--------
# assigning mean to data frame 
average <- cf1 %>%
  group_by(year, region) %>%
  summarise(mean_yr_region = mean(mean))

# plotting and animating data
p <- ggplot(average, aes(x=year, y=mean_yr_region, col=region)) + geom_line() +
  labs(title = "Global Unmet Need for Contraception Over Time", x = "Year", y = "Mean Unmet Need for Any Contraceptive Method", color = "World Super Regions" ) +
  ylim(0, 0.25) +
  theme(plot.title = element_text(size = 14, face = "bold"), axis.title.x = element_text(size = 13), axis.title.y = element_text(size = 13), legend.title = element_text(size = 13), legend.text = element_text(size = 11)) +
  scale_colour_brewer(palette = "Set1")
p
anim <- p + geom_point(size=2.5) + transition_reveal(year) 
animate(anim, renderer=gifski_renderer()) 

# save output as gif
anim_save(here("figs", "contraception_year_region.gif"), anim, renderer=gifski_renderer()) 

