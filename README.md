# Analysis of crimes, calls for service and police stops conducted by San Diego Police Department
By: [Lauryn Schroeder](https://www.sandiegouniontribune.com/sdut-lauryn-schroeder-staff.html), [Greg Moran](https://www.sandiegouniontribune.com/sdut-greg-moran-staff.html) and [Lyndsay Winkley](https://www.sandiegouniontribune.com/sdut-lyndsay-winkley-staff.html)

This repository contains data and code for the analysis [reported and published](XXXXXX) by *The San Diego Union-Tribune* in 2022.

### About

Studies over the past decade have shown San Diego police stop, search and arrest people of color more often than Whites. Police officials have repeatedly said factors other than racial bias are driving the numbers. Specifically, that officers respond to crimes, emergency calls and complaints, and these are what largely drive the numbers.

The Union-Tribune analyzed stops conducted by the San Diego Police Department (SDPD) with calls for service and reported crimes in each beat from January 2019 through June 2021 to determine whether stops of drivers and pedestrians are proportional to crime activity and calls for service in each neighborhood.

Ratios of stops per reported crime, people stopped per reported crime and stops per every Part 1 -- or serious -- crime were calculated to identify areas with disproportionate policing, and population data was used to identify majority-White and majority-non-White beats.

The analysis is the latest examination of crime and policing in San Diego undertaken by the Union-Tribune. 

Since 2019, the [Crime Counts](https://www.sandiegouniontribune.com/topic/crime-counts) series has examined where crime occurs in city neighborhoods, and where crime has increased or decreased over the years. In 2021 The [Color of Authority](https://www.sandiegouniontribune.com/news/watchdog/story/2021-03-28/the-color-of-authority-san-diego-police-sheriffs-deputies-disproportionately-target-minorities-data-show) series took a close look at RIPA data, examining who police stop, why drivers and pedestrians are stopped and how stops are conducted.

Stop data come from the Racial and Identity Profiling Act of 2015 (RIPA), which requires nearly all California law enforcement agencies to submit demographic data on all detentions and searches. The Union-Tribune downloaded this stop data from the city of San Diego's [data portal](https://data.sandiego.gov/datasets/police-ripa-stops/).

Crime data was requested and obtained under the California Public Records Act.

Data containing all calls for service during the same time frame was downloaded via the city's [data portal](https://data.sandiego.gov/datasets/police-calls-for-service/).

Shapefiles for police beats were also obtained through the city's [data portal](https://data.sandiego.gov/datasets/police-beats/).

### Methodology / Notes

Since more than one individual can be involved in a stop (officers are required to record the race/ethnicity of drivers and passengers) the Union-Tribune opted to analyze the race/ethnicity of each person involved, which is the same technique used by RIPA officials.

In some circumstances, officers list more than one perceived race for an individual involved in traffic stops. 

Individuals who were perceived as Hispanic and any other race were included in Hispanic totals. Individuals perceived as more than one race were categorized as those with two or more races. The remaining race categories were left the same.

Beat population data was calculated using the U.S. Census Bureau's American Community Survey for 2019.

The Union-Tribune aimed to identify the number of Part 1 and Part 2 crimes that occurred in each beat, as defined by the FBI Uniform Crime Reporting program. Since city crime data did not carry this distinction, the Union-Tribune categorized crimes as part 1 or part 2 manually, using descriptions and code sections outlined in the [California Penal Code](https://leginfo.legislature.ca.gov/faces/codes_displaySection.xhtml?sectionNum=667.5.&lawCode=PEN).

Stop, crime and call data files all contained rows with beat names or numbers not found in the city of San Diego's police beat shapefile. These were removed from the analyses.

The Union-Tribune broke down searches into two categories: discretionary and non-discretionary.

Non-discretionary searches are typically required under department policy or state law, and include those made during arrests or when officers execute a warrant. The Union-Tribune considered the following search reasons non-discretionary: vehicle inventory, incident to arrest and search warrant.

The remaining search reasons were considered discretionary to some degree, meaning it was partially or entirely up to the officer to decide whether to search an individual, such as when they smell drugs or think they see a weapon.

If both a discretionary and non-discretionary reason were listed, the search was classified as non-discretionary, since a search would be required by law or policy, regardless of other circumstances.

### The SDUT repository contains the following:

- `beat_demographics_2019_acs.csv` - Population data for each beat, based on the U.S. Census Bureau's American Community Survey for 2019.
- `crime_desc_part1_cats.csv` - Descriptions of crimes found in SDPD crime data, containing Part 1 or Part 2 distinctions.
- `master-ripa-2019.csv` - Stops conducted by SDPD in 2019, collected under RIPA.
- `master-ripa-2020.csv` - Stops conducted by SDPD in 2020, collected under RIPA.
- `master-ripa-2021.csv` - Stops conducted by SDPD in 2021, collected under RIPA.
- `pd_beats_datasd` - Folder containing shapefiles for SDPD beats.
- `pd_calls_for_service_2019_datasd.csv` - Emergency calls for service in San Diego in 2019.
- `pd_calls_for_service_2020_datasd.csv` - Emergency calls for service in San Diego in 2020.
- `pd_calls_for_service_2021_datasd.csv` - Emergency calls for service in San Diego in 2021.
- `pd_dispo_codes_datasd.csv` - Disposition codes and descriptions for emergency calls for service.
- `PRA 21-4922_2019-2021_Crimes__UCR Revised.xlsx` - Data of all crimes reported to SDPD, requested and obtained under the California Public Records Act.
- `crime-calls-stops-sdpd-analysis.R` - Import and analysis R script documenting findings published by the Union-Tribune. Main code used to create and export stops-crime-calls-by-beat.csv, showing crimes, stops, calls for service and other variables by SDPD beat.
- `stops-crime-calls-by-beat.csv` - Filed created with R analysis showing crimes, stops, calls for service and other variables by SDPD beat.

### Sourcing
Please link and source [*The San Diego Union-Tribune*](https://www.sandiegouniontribune.com/) when referencing any analysis or findings in published work.

### Questions / Feedback

Email Lauryn Schroeder at [lauryn.schroeder@sduniontribune.com](mailto:lauryn.schroeder@sduniontribune.com).
