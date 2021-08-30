## Goal: create EML metadata for project using the Environmental Data Initiative's EML Assembly Line (https://github.com/EDIorg/EMLassemblyline)

# Put data and code in a folder together to be grabbed by make_eml
# Generate metadata files by editing current ones or generating them (see Github page for tutorial)
# Edit and run this script


#### set up ####

# clear environment
rm(list=ls())

# load libraries
library(EMLassemblyline)
library(tidyverse)
library(knitr)


#### import templates ####

# list of data files
dlist <- list.files(path = "./data",
                    pattern = ".csv")

# create high level text files
template_core_metadata(path = "./metadata",
                       license = "CCBY")

# create an attribute file for each data table
template_table_attributes(path = "./metadata",
                          data.path = "./data",
                          data.table = dlist)

# create a categorical value file for each data table
template_categorical_variables(path = "./metadata",
                               data.path = "./data")

# look at units
view_unit_dictionary()


#### data file descriptors ####

dlist

# description list
ddlist <- NA

ddlist[1] <- "heights of plants in Appendix S3 experiment"
ddlist[2] <- "foliar lesions on plants in Appendix S3 experiment"
ddlist[3] <- "leaf wetness of plants in Appendix S3 experiment"
ddlist[4] <- "biomass of M. vimineum and E. virginicus"
ddlist[5] <- "establishment and foliar lesions of M. vimineum and E. virginicus"

# name list
dnlist <- c("App. S3 height data", 
            "App. S3 lesion data",
            "App. S3 leaf wetness data",
            "biomass data",
            "establishment and lesion data")

# print table
# dtable <- data.frame(data = dlist, description = ddlist)
# kable(dtable)


#### code descriptors ####

# list of code files
clist <- c(list.files(path = "./code",
                      pattern = ".R"),
           list.files(path = "./edi",
                      pattern = ".pdf"))

# remove the eml code
clist <- clist[-3]

# code descripions
cdlist <- c(
  "code for analyses and figures in Appendix S3",
  "code to analyze biomass data",
  "code to process data for establishment and disease incidence analyses",
  "code to analyze establishment and disease incidence",
  "code for figures and values in main text",
  "code to analyze model residuals",
  "description of experiment in Appendix S3"
)

# name list
cnlist <- c("App. S3 code", 
            "Biomass analysis",
            "Data processing",
            "Establishment and disease incidence analysis",
            "Figures and values",
            "Residuals analysis",
            "App. S3 experiment description")

# print table
# ctable <- data.frame(code = clist, desription = cdlist)
# kable(ctable)


#### make eml ####

# copied data and code from the respective folders and put into edi folder

make_eml(path = "./metadata",
         data.path = "./edi",
         dataset.title = "Invasive grass litter suppresses a native grass species and promotes disease",
         data.table = dlist,
         data.table.name = dnlist,
         data.table.description = ddlist,
         data.table.quote.character = rep("\"", length(dlist)),
         other.entity = clist,
         other.entity.name = cnlist,
         other.entity.description = cdlist,
         temporal.coverage = c("2018-05-09", "2018-11-04"),
         geographic.description = "Gainesville, FL, USA",
         geographic.coordinates = c(29.64, -82.36, 29.64, -82.36),
         maintenance.description = "completed", 
         user.id = "aekendig",
         user.domain = "EDI",
         package.id = "edi.189.3")


#### check warnings ####

eml <- EML::read_eml("./metadata/edi.189.3.xml")
EML::eml_validate(eml)
