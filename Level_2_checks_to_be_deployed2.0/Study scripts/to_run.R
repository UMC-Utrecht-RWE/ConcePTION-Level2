rm(list=ls())
if(!require(rmarkdown)){install.packages("rmarkdown")}
library(rmarkdown)
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)

source(paste0(projectFolder,"/","99_path.R"))
source(paste0(projectFolder,"/Functions/IMPORT_PATTERN.R"))


## Run the individual checks (unless certain essential files are missing)
files <- dir(dir.data, pattern = '.csv')
check1 <- any(grepl('^VISIT_OCCURRENCE', files))
check2 <- any(grepl('^PERSONS', files))
check3 <- any(grepl('^PERSON_RELATIONSHIPS', files))
check4 <- any(grepl('^OBSERVATION_PERIODS', files))
if(!check1) warning('VISIT_OCCURRENCE is missing, checks 5 and 6 are skipped.')
if(!check2) warning('PERSONS is missing, checks 1, 2, 4 and 8 are skipped.')
if(!check3) warning('PERSON_RELATIONSHIPS is missing, check 8 is skipped.')
if(!check4) warning('OBSERVATION_PERIODS is missing, check 3 is skipped.')
if(check2) system.time(render("Report_2_1.Rmd", output_dir = output_dir))
if(check2) system.time(render("Report_2_2.Rmd", output_dir = output_dir))
if(check4) system.time(render("Report_2_3.Rmd", output_dir = output_dir))
if(check2) system.time(render("Report_2_4.Rmd", output_dir = output_dir))
if(check1) system.time(render("Report_2_5.Rmd", output_dir = output_dir))
if(check1) system.time(render("Report_2_6.Rmd", output_dir = output_dir))
if(check1) system.time(render("Report_2_7.Rmd", output_dir = output_dir))
if(check2 & check3) system.time(render("Report_2_8.Rmd", output_dir = output_dir))
print('End of Level 2 checks')
