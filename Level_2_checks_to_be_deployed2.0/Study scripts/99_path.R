
#Level 2 checks

#Directory

Studyname<-"ConcePTION"
setwd('..') #in Level_2_checks_to_be_deployed
dir_base<-getwd()
#set output folder to g_output
output_dir<-paste0(dir_base, "/g_output/")
setwd('..') #in Data characterisation
setwd('..') #in ConcePTION
dir_base<-getwd()
dir.data<-paste0(dir_base,"/CDMInstances/", Studyname, "/")

setwd(projectFolder)


## Import, CDM source, extract creation_date and then export CDM source
CDM_SOURCE <- read.csv(paste0(dir.data, '/CDM_SOURCE.csv'))
creation_date <- CDM_SOURCE$date_creation
creation_date <- as.Date(as.character(creation_date), format = '%Y%m%d')
write.table(CDM_SOURCE, file = paste0(output_dir, 'CDM_SOURCE.csv'), sep = ',', row.names = F)
