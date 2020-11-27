# Author: Kate Schneider (kate.schneider@tufts.edu)
# Last modified: 26 November 2020
# This R script is converted from Supp4_NutrientRequirementsDataManagement.do by Zhining Sun
# Purpose: Software tools to compile DRI nutrient requirements recalculated for
#   WHO growth references and with optional adjustment if including children 6-23 
#   months.	The final data shape is strucuture appropriately as the nutrient requirement
#   inputs to the protocol for calculating the Cost of Nutrient Adequacy (CoNA),
#   described in the companion software tools article Bai et al (2020).
# Supporting Explanatory Documentation: 
#   1.  Schneider, K. & Herforth, A. 2020. "Software tools for practical application 
#         of human nutrient requirements in food-based social science research". 
#         Gates Open Research.
#   2.  Input datasets "Notes" files

# Input data: Multiple datasets described in:
#   1. Supplement 1: 1_NutrientRequirements_Notes
#   2. Supplement 2: 2_WHOGrowthCharts_Notes
#   3. Supplement 3: 3_NutrientRequirements6-23months_Notes
#
# Output datasets:
#   1. EAR+UL+AMDR+CDRR_allgroups
#   2. EAR+UL+AMDR+CDRR+BF_allgroups
#   3. EAR+UL+AMDR+CDRR+BF+WHOGrowth_allgroups

# Part 5 gives example code to extract the nutrient requirements for a single
#   age-sex group at a specific level of physical activity. This is produces
#   the data in a format suited to merging with the food prices and composition 
#   data in preparation for linear programming in the CoNA protocol. 

# Licence:
#   Copyright 2020 Kate Schneider
    
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
    
#   http://www.apache.org/licenses/LICENSE-2.0
    
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#

################################################################################
# Part 0 File Management
# Set working directory
wd<-"yourfilepath" 
setwd(wd)
getwd()
rm(list=ls())
################################################################################
# Part 1 Import and Reshape
#####
# Micronutrients EAR
  nr_Micronutrients_EAR<-read.csv("1_NutrientRequirements_MicronutrientsEAR.csv")

  # Rename variables (nutr_no follows the "Units_Notes" sheet)
  colnames(nr_Micronutrients_EAR)
  names(nr_Micronutrients_EAR)[names(nr_Micronutrients_EAR)=="vitA"]<-"ear6"
  names(nr_Micronutrients_EAR)[names(nr_Micronutrients_EAR)=="vitC"]<-"ear8"
  names(nr_Micronutrients_EAR)[names(nr_Micronutrients_EAR)=="vitE"]<-"ear9"
  names(nr_Micronutrients_EAR)[names(nr_Micronutrients_EAR)=="thiamin"]<-"ear10"
  names(nr_Micronutrients_EAR)[names(nr_Micronutrients_EAR)=="riboflavin"]<-"ear11"
  names(nr_Micronutrients_EAR)[names(nr_Micronutrients_EAR)=="niacin"]<-"ear12"
  names(nr_Micronutrients_EAR)[names(nr_Micronutrients_EAR)=="vitB6"]<-"ear13"
  names(nr_Micronutrients_EAR)[names(nr_Micronutrients_EAR)=="folate"]<-"ear14"
  names(nr_Micronutrients_EAR)[names(nr_Micronutrients_EAR)=="vitB12"]<-"ear15"
  names(nr_Micronutrients_EAR)[names(nr_Micronutrients_EAR)=="calcium"]<-"ear16"
  names(nr_Micronutrients_EAR)[names(nr_Micronutrients_EAR)=="copper"]<-"ear17"
  names(nr_Micronutrients_EAR)[names(nr_Micronutrients_EAR)=="iron"]<-"ear18"
  names(nr_Micronutrients_EAR)[names(nr_Micronutrients_EAR)=="magnesium"]<-"ear19"
  names(nr_Micronutrients_EAR)[names(nr_Micronutrients_EAR)=="phosphorus"]<-"ear20"
  names(nr_Micronutrients_EAR)[names(nr_Micronutrients_EAR)=="selenium"]<-"ear21"
  names(nr_Micronutrients_EAR)[names(nr_Micronutrients_EAR)=="zinc"]<-"ear22"
  names(nr_Micronutrients_EAR)[names(nr_Micronutrients_EAR)=="sodium"]<-"ear23"
  nr_Micronutrients_EAR$ear1<-NA
  nr_Micronutrients_EAR$ear2<-NA
  nr_Micronutrients_EAR$ear3<-NA
  nr_Micronutrients_EAR$ear4<-NA
  nr_Micronutrients_EAR$ear5<-NA
  nr_Micronutrients_EAR$ear7<-NA

  # Make sure all variables are numeric
  str(nr_Micronutrients_EAR)
  install.packages("dplyr")
  library ("dplyr")
  nr_Micronutrients_EAR <- suppressWarnings(mutate_each(nr_Micronutrients_EAR, funs(as.numeric), ear6:ear23))
  str(nr_Micronutrients_EAR)
  nr_Micronutrients_EAR <- suppressWarnings(mutate_each(nr_Micronutrients_EAR, funs(as.numeric), ear1:ear7))
  install.packages("tidyr")
  library(tidyr)
  nr_Micronutrients_EAR_1<- pivot_longer(nr_Micronutrients_EAR, 
                                         cols =starts_with("ear"),
                                         names_to = c("e","nutr_no"), 
                                         names_pattern="([A-Za-z]+)(\\d+)", 
                                         values_to = "ear")
  nr_Micronutrients_EAR_1<-subset(nr_Micronutrients_EAR_1,select=-e)
  replace(nr_Micronutrients_EAR_1$ear,nr_Micronutrients_EAR_1$nutr_no==3,NA)
  Micronutrients_EAR<-nr_Micronutrients_EAR_1
  rm(nr_Micronutrients_EAR)
  rm(nr_Micronutrients_EAR_1)


#####
# Micronutrients UL 
  nr_Micronutrients_ul<-read.csv("1_NutrientRequirements_MicronutrientsUL.csv")
  colnames(nr_Micronutrients_ul)
  names(nr_Micronutrients_ul)[names(nr_Micronutrients_ul)=="vitA"]<-"ul6"
  names(nr_Micronutrients_ul)[names(nr_Micronutrients_ul)=="vitC"]<-"ul8"
  names(nr_Micronutrients_ul)[names(nr_Micronutrients_ul)=="vitE"]<-"ul9"
  names(nr_Micronutrients_ul)[names(nr_Micronutrients_ul)=="thiamin"]<-"ul10"
  names(nr_Micronutrients_ul)[names(nr_Micronutrients_ul)=="riboflavin"]<-"ul11"
  names(nr_Micronutrients_ul)[names(nr_Micronutrients_ul)=="niacin"]<-"ul12"
  names(nr_Micronutrients_ul)[names(nr_Micronutrients_ul)=="vitB6"]<-"ul13"
  names(nr_Micronutrients_ul)[names(nr_Micronutrients_ul)=="folate"]<-"ul14"
  names(nr_Micronutrients_ul)[names(nr_Micronutrients_ul)=="vitB12"]<-"ul15"
  names(nr_Micronutrients_ul)[names(nr_Micronutrients_ul)=="calcium"]<-"ul16"
  names(nr_Micronutrients_ul)[names(nr_Micronutrients_ul)=="copper"]<-"ul17"
  names(nr_Micronutrients_ul)[names(nr_Micronutrients_ul)=="iron"]<-"ul18"
  names(nr_Micronutrients_ul)[names(nr_Micronutrients_ul)=="magnesium"]<-"ul19"
  names(nr_Micronutrients_ul)[names(nr_Micronutrients_ul)=="phosphorus"]<-"ul20"
  names(nr_Micronutrients_ul)[names(nr_Micronutrients_ul)=="selenium"]<-"ul21"
  names(nr_Micronutrients_ul)[names(nr_Micronutrients_ul)=="zinc"]<-"ul22"
  names(nr_Micronutrients_ul)[names(nr_Micronutrients_ul)=="sodium"]<-"ul23"
  names(nr_Micronutrients_ul)[names(nr_Micronutrients_ul)=="retinol"]<-"ul7"
  nr_Micronutrients_ul$ul1<-NA
  nr_Micronutrients_ul$ul2<-NA
  nr_Micronutrients_ul$ul3<-NA
  nr_Micronutrients_ul$ul4<-NA
  nr_Micronutrients_ul$ul5<-NA

  # Make sure all variables are numeric
  str(nr_Micronutrients_ul)
  # install.packages("dplyr")
  library ("dplyr")
  nr_Micronutrients_ul <- suppressWarnings(mutate_each(nr_Micronutrients_ul, funs(as.numeric), ul6:ul23))
  str(nr_Micronutrients_ul)
  nr_Micronutrients_ul <- suppressWarnings(mutate_each(nr_Micronutrients_ul, funs(as.numeric), ul1:ul5))
  #install.packages("tidyr")
  library(tidyr)
  nr_Micronutrients_ul_1<- pivot_longer(nr_Micronutrients_ul, cols =starts_with("ul"), 
    names_to = c("e", "nutr_no"), names_pattern="([A-Za-z]+)(\\d+)", values_to = "ul")
  keep<-c("age_sex_grp","nutr_no","ul")
  nr_Micronutrients_ul_2<-nr_Micronutrients_ul_1[keep]
  Micronutrients_ul<-nr_Micronutrients_ul_2
    rm(nr_Micronutrients_ul_2)
    rm(nr_Micronutrients_ul_1)
    rm(nr_Micronutrients_ul)
    rm(keep)

#####
# Macronutrients EAR
  Macronutrients_EAR<-read.csv("1_NutrientRequirements_MacronutrientsEAR.csv")
  Macronutrients_EAR_1<-subset(Macronutrients_EAR,select=-c(lifestage,sex,age_lower_yrs,age_upper_yrs))
  colnames(Macronutrients_EAR_1)
  names(Macronutrients_EAR_1)[names(Macronutrients_EAR_1)=="refweight_kg"]<-"refweight_protein_kg"
  Macronutrients_EAR_1$nutr_no<-4
  Macronutrients_EAR<-Macronutrients_EAR_1
    rm(Macronutrients_EAR_1)

#####
#Macronutrients AMDR
  Macronutrients_AMDR<-read.csv("1_NutrientRequirements_MacronutrientsAMDR.csv")
  Macronutrients_AMDR_1<-subset(Macronutrients_AMDR,select=-c(lifestage,sex,age_lower_yrs,age_upper_yrs),age_sex_grp=".")
  colnames(Macronutrients_AMDR_1)
  names(Macronutrients_AMDR_1)[names(Macronutrients_AMDR_1)=="lipids_lower"]<-"amdr_lower5"
  names(Macronutrients_AMDR_1)[names(Macronutrients_AMDR_1)=="lipids_upper"]<-"amdr_upper5"
  names(Macronutrients_AMDR_1)[names(Macronutrients_AMDR_1)=="carbohydrate_lower"]<-"amdr_lower3"
  names(Macronutrients_AMDR_1)[names(Macronutrients_AMDR_1)=="carbohydrate_upper"]<-"amdr_upper3"
  names(Macronutrients_AMDR_1)[names(Macronutrients_AMDR_1)=="protein_lower"]<-"amdr_lower4"
  names(Macronutrients_AMDR_1)[names(Macronutrients_AMDR_1)=="protein_upper"]<-"amdr_upper4"
  Macronutrients_AMDR_2<-reshape(as.data.frame(Macronutrients_AMDR_1),direction='long',varying=list(amdr_lower=c(2,4,6),amdr_upper=c(3,5,7)),
        timevar='nutr_no',
        times=c(5,3,4),
        v.names=c("amdr_lower","amdr_upper"),
        idvar="age_sex_grp")
  keep<-c("age_sex_grp","nutr_no","amdr_lower","amdr_upper")
  Macronutrients_AMDR_1<-Macronutrients_AMDR_2[keep]
  Macronutrients_AMDR<-Macronutrients_AMDR_1
    rm(Macronutrients_AMDR_1)
    rm(Macronutrients_AMDR_2)
    rm(keep)

#####
# Energy
  Energy<-read.csv("1_NutrientRequirements_Energy.csv")
  Energy$nutr_no<-2
  Energy$pal<-Energy$PA_level
  Energy$pal<-Energy$PA_level
  # install.packages("dplyr")
  library ("dplyr")
  Energy$pal<-recode(Energy$pal,"sedentary"=1,"low active"=2,"active"=3, "very active"=4)
    table(Energy$pal)
    Energy$pal <- ordered(Energy$pal,
    levels = c(1,2,3,4),
    labels = c("sedentary","low active","active","very active"))
    table(Energy$pal)
  keep<-c("age_sex_grp","energy","nutr_no","pal")
  Energy_1<-Energy[keep]
  col_order<-c("age_sex_grp","nutr_no","pal","energy")
  Energy<-Energy_1[,col_order]
  library(dplyr)
  Energy_1<-Energy_1%>%arrange(age_sex_grp,pal)
    Energy<-Energy_1
    rm(Energy_1)
    rm(col_order)
    rm(keep)
  
  # Units Notes - imported long by nutrient, merge with UL keeping relevant variables only
  library ("readxl")
  Units_notes<-read.csv("1_NutrientRequirements_UnitsNotes.csv")
  keep<-c("nutr_no","ul_note","unit","amdr_unit")
    Units_notes<-Units_notes[keep]
  Micronutrients_UL_1<-merge(Micronutrients_ul,Units_notes,by=c("nutr_no"),all=T)
    col_order<-c("age_sex_grp","nutr_no","unit","ul","ul_note","amdr_unit")
  Micronutrients_UL<-Micronutrients_UL_1[,col_order]

#NOTE: the sodium UL is the not a Tolerable Upper Intake Level (UL), it is the 
#   Chronic Disease Risk Reduction (CDRR) level as established in the 2019 
#   update to the DRIs for sodium and potassium (IOM 2019). 
#   However it functions in practice as an upper bound constraint for the 
#   definition of healthy and adequate diets and therefore is included with 
#   the ULs for analytical convenience.

  Micronutrients_UL<-Micronutrients_UL
    rm(Units_nots)
    rm(Micronutrients_UL_1)
    rm(keep)
    rm(col_order)

################################################################################
#Part 2 MERGE ALL
  # Merge datasets except energy
  # Micronutrients EARs and ULs
  Micronutrients_UL_1<-merge(Micronutrients_UL,Micronutrients_EAR,by=c("age_sex_grp","nutr_no"),all=T)
  distinct_age_sex_grp<-unique(Micronutrients_UL_1$age_sex_grp)
    length(distinct_age_sex_grp)
  distinct_nutr_no<-unique(Micronutrients_UL_1$nutr_no)
    length(distinct_nutr_no)
  
  # Macronutrients EAR
  Micronutrients_UL_3<-merge(Micronutrients_UL_1,Macronutrients_EAR,by=c("age_sex_grp","nutr_no"),all=T)
    Micronutrients_UL_2<- suppressWarnings(mutate_each(Micronutrients_UL_3, funs(as.numeric), protein))
    Micronutrients_UL_4<- suppressWarnings(mutate_each(Micronutrients_UL_2, funs(as.numeric),carbohydrate ))
    suppressWarnings(replace(Micronutrients_UL_4$ear,Micronutrients_UL_4$nutr_no==4,Micronutrients_UL_4$protein))
    suppressWarnings(replace(Micronutrients_UL_4$ear,Micronutrients_UL_4$nutr_no==3,Micronutrients_UL_4$carbohydrate))
  
  # Macronutrients AMDR
  # Note the macronutrients unit is % total calories
  Micronutrients_UL_5<-merge(Micronutrients_UL_4,Macronutrients_AMDR,by=c("age_sex_grp","nutr_no"),all=T)
  # Check if match
   View(Micronutrients_UL_5)
  # Confirm variables are numeric
  str(nutr_no)
  #install.packages("dplyr")
  #library ("dplyr")
  Micronutrients_UL_5 <- suppressWarnings(mutate_each(Micronutrients_UL_5, funs(as.numeric), nutr_no))
  library(dplyr)
  Micronutrients_UL_6<-Micronutrients_UL_5
  Micronutrients_UL_6<-Micronutrients_UL_6%>%arrange(age_sex_grp,nutr_no)
  distinct_age_sex_grp<-unique(Micronutrients_UL_6$age_sex_grp)
    length(distinct_age_sex_grp)
  distinct_nutr_no<-unique(Micronutrients_UL_6$nutr_no)
    length(distinct_nutr_no)
  rm(distinct_age_sex_grp)
  rm(distinct_nutr_no)
    
  # Energy
  Micronutrients_UL_7<-merge(Micronutrients_UL_6,Energy,by=c("age_sex_grp","nutr_no"),all=T)
  colnames(Micronutrients_UL_7)
  Micronutrients_UL_8<-Micronutrients_UL_7[,c(1,2,18,11,4,12,16,17,19,3,5,6,7,8,9,10,13,14,15)]
    attach(Micronutrients_UL_8)
  Micronutrients_UL_9<-Micronutrients_UL_8[order(age_sex_grp,nutr_no,pal),]
      rm(Micronutrients_UL_1)
      rm(Micronutrients_UL_3)
      rm(Micronutrients_UL_2)
      rm(Micronutrients_UL_4)
      rm(Micronutrients_UL_5)
      rm(Micronutrients_UL_6)
      rm(Micronutrients_UL_7)
      rm(Micronutrients_UL_8)
        
    # Expand macronutrients to calculate per physical activity level (PAL)
    Micronutrients_UL_expand<-Micronutrients_UL_9[rep(row.names(Micronutrients_UL_9),
      ifelse(Micronutrients_UL_9$nutr_no==3|Micronutrients_UL_9$nutr_no==4|Micronutrients_UL_9$nutr_no==5,4,1)),]
      attach(Micronutrients_UL_expand)
    Micronutrients_expand<-Micronutrients_UL_expand[order(age_sex_grp,nutr_no,pal),]
    install.packages("data.table")
    library(data.table)
    Micronutrients_expand_1<-transform(Micronutrients_expand,v1=ave(nutr_no,rleid(age_sex_grp,nutr_no),FUN=seq_along))
    library ("dplyr")
    Micronutrients_expand_2 <- suppressWarnings(mutate_each(Micronutrients_expand_1, funs(as.numeric), pal))
    
    Micronutrients_expand_1$pal<-suppressWarnings(replace(Micronutrients_expand_1$pal,
                    Micronutrients_expand_1$v1==1& (Micronutrients_expand_1$nutr_no==3| Micronutrients_expand_1$nutr_no==4| Micronutrients_expand_1$nutr_no==5),
                    "sedentary"))
    Micronutrients_expand_1$pal<-suppressWarnings(replace(Micronutrients_expand_1$pal,
                    Micronutrients_expand_1$v1==2& (Micronutrients_expand_1$nutr_no==3| Micronutrients_expand_1$nutr_no==4| Micronutrients_expand_1$nutr_no==5),
                    "low active"))
    Micronutrients_expand_1$pal<-suppressWarnings(replace(Micronutrients_expand_1$pal,
                    Micronutrients_expand_1$v1==3& (Micronutrients_expand_1$nutr_no==3| Micronutrients_expand_1$nutr_no==4| Micronutrients_expand_1$nutr_no==5),
                    "active"))
    Micronutrients_expand_1$pal<-suppressWarnings(replace(Micronutrients_expand_1$pal,
                    Micronutrients_expand_1$v1==4& (Micronutrients_expand_1$nutr_no==3| Micronutrients_expand_1$nutr_no==4| Micronutrients_expand_1$nutr_no==5),
                    "very active"))
    library(dplyr)
      Micronutrients_expand_2 <- suppressWarnings(mutate_each(Micronutrients_expand_1, funs(as.numeric), energy))
      options(warn=-1)
      library(Hmisc)
      Micronutrients_expand_2$energy.lag<-lag(Micronutrients_expand_2$energy,4)
      Micronutrients_expand_2$energy_new<-coalesce(Micronutrients_expand_2$energy.lag,Micronutrients_expand_2$energy)
      Micronutrients_expand_2$energy.lag_2<-lag(Micronutrients_expand_2$energy,8)
      Micronutrients_expand_2$energy.lag<-coalesce(Micronutrients_expand_2$energy_new,Micronutrients_expand_2$energy.lag_2)
      Micronutrients_expand_2$energy.lag_3<-lag(Micronutrients_expand_2$energy,12)
      Micronutrients_expand_2$energy<-coalesce(Micronutrients_expand_2$energy.lag,Micronutrients_expand_2$energy.lag_3)
      Micronutrients_expand_3<-subset(Micronutrients_expand_2,select=-c(21:24))
        rm(Micronutrients_expand_2)
        rm(Micronutrients_expand_1)
        rm(Micronutrients_expand)
      Micronutrients_expand_4<-subset(Micronutrients_expand_3,select=-c(v1,protein_perkg,refweight_protein_kg))
    
      #Replace missing EAR with value of energy
      Micronutrients_expand_4$ear<-with(Micronutrients_expand_4,ifelse(is.na(Micronutrients_expand_4$ear)& Micronutrients_expand_4$nutr_no==2,energy,ear))

  # Replace AMDRs as grams of each macronutrient, per PAL
  # Note: AMDRs are currently expressed as a percent
  library ("dplyr")
  Micronutrients_expand_4<- mutate_each(Micronutrients_expand_4, funs(as.numeric), c(7:8))

  #Convert to percent and multiply by total energy
  Micronutrients_expand_4$amdr_kcal_lower<-with(Micronutrients_expand_4,ifelse(nutr_no==3|nutr_no==4|nutr_no==5,energy*amdr_lower/100,""))
  Micronutrients_expand_4$amdr_kcal_upper<-with(Micronutrients_expand_4,ifelse(nutr_no==3|nutr_no==4|nutr_no==5,energy*amdr_upper/100,""))

  #Divide the total calories per macronutrient by the kcal per gram
    #Protein and Carbohydrates have 4 calories per gram
    #Lipids have 9 calories per gram
    library ("dplyr")
    Micronutrients_expand_4<- mutate_each(Micronutrients_expand_4, funs(as.numeric), c(18:19))
    Micronutrients_expand_4$amdr_lower<-with(Micronutrients_expand_4,ifelse(nutr_no==3|nutr_no==4,amdr_kcal_lower/4,amdr_lower))
    Micronutrients_expand_4$amdr_lower<-with(Micronutrients_expand_4,ifelse(nutr_no==5,amdr_kcal_lower/9,amdr_lower))
    Micronutrients_expand_4$amdr_upper<-with(Micronutrients_expand_4,ifelse(nutr_no==3|nutr_no==4,amdr_kcal_upper/4,amdr_upper))
    Micronutrients_expand_4$amdr_upper<-with(Micronutrients_expand_4,ifelse(nutr_no==5,amdr_kcal_lower/9,amdr_upper))
    Micronutrients_expand_4$amdr_lower<-with(Micronutrients_expand_4,ifelse(!(nutr_no==3|nutr_no==4|nutr_no==5),"",amdr_lower))
    Micronutrients_expand_4$amdr_upper<-with(Micronutrients_expand_4,ifelse(!(nutr_no==3|nutr_no==4|nutr_no==5),"",amdr_upper))

    #Result: total grams 
    Micronutrients_expand_4$amdr_unit<-with(Micronutrients_expand_4,ifelse(!(nutr_no==3|nutr_no==4|nutr_no==5),"g",amdr_unit))

  #Make sure all variables are numeric
  str(Micronutrients_expand_4)
  Micronutrients_expand_4$lifestage2<-Micronutrients_expand_4$lifestage
  library ("dplyr")
  Micronutrients_expand_4$lifestage2<-recode(Micronutrients_expand_4$lifestage2,"Adolescent"=1,"Adult"=2,"Child"=3, "Infant"=4,
    "Lactation"=5,"Older Adult"=6,"Pregnancy"=7)
  Micronutrients_expand_4$lifestage <- ordered(Micronutrients_expand_4$lifestage2,
    levels = c(1,2,3,4,5,6,7),
    labels = c("Adolescent","Adult","Child","Infant","Lactation","Older Adult","Pregnancy"))
    table(Micronutrients_expand_4$lifestage)
  Micronutrients_expand_4<- mutate_each(Micronutrients_expand_4, funs(as.numeric), c(lifestage))
  Micronutrients_expand_4$sex2<-Micronutrients_expand_4$sex
  library ("dplyr")
  Micronutrients_expand_4$sex2<-recode(Micronutrients_expand_4$sex2,"Female"=1,"Male"=2,"all"=3)
  Micronutrients_expand_4$sex <- ordered(Micronutrients_expand_4$sex2,
    levels = c(1,2,3),
    labels = c("Female","Male","all"))
    table(Micronutrients_expand_4$sex)
  Micronutrients_expand_4<- mutate_each(Micronutrients_expand_4, funs(as.numeric), c(sex))
    table(Micronutrients_expand_4$sex)
  Micronutrients_expand_4$sex<-recode(Micronutrients_expand_4$sex,"1"=1,"2"=0,"3"=3)
  Micronutrients_expand_4$sex<-with(Micronutrients_expand_4,ifelse(sex==3,"",sex))
  Micronutrients_expand_4$sex <- ordered(Micronutrients_expand_4$sex,
    levels = c(0,1),
    labels = c("Male","Female"))
  str(Micronutrients_expand_4$sex)
  Micronutrients_expand_4<- mutate_each(Micronutrients_expand_4, funs(as.numeric), c(sex))
  Micronutrients_expand_4<- mutate_each(Micronutrients_expand_4, funs(as.numeric), c(7:8))

  #Order and Sort
  Micronutrients_expand_5<-subset(Micronutrients_expand_4,select=-c(protein,carbohydrate,energy,amdr_kcal_lower,amdr_kcal_upper,sex2,lifestage2))
    colnames(Micronutrients_expand_5)
  Micronutrients_expand_6<-Micronutrients_expand_5[,c(1,2,7,3,4,5,6,8,9,10,11,12,13,14)]
    attach(Micronutrients_expand_6)
  Micronutrients_expand_7<-Micronutrients_expand_6[order(age_sex_grp,nutr_no,pal),]
  
    #Label nutrients
    Micronutrients_expand_7$nutr<-NA
    Micronutrients_expand_7$nutr<-with(Micronutrients_expand_7,ifelse(nutr_no==1,"Price",nutr))
    Micronutrients_expand_7$nutr<-with(Micronutrients_expand_7,ifelse(nutr_no==2,"Energy",nutr))
    Micronutrients_expand_7$nutr<-with(Micronutrients_expand_7,ifelse(nutr_no==3,"Carbohydrate",nutr))
    Micronutrients_expand_7$nutr<-with(Micronutrients_expand_7,ifelse(nutr_no==4,"Protein",nutr))
    Micronutrients_expand_7$nutr<-with(Micronutrients_expand_7,ifelse(nutr_no==5,"Lipids",nutr))
    Micronutrients_expand_7$nutr<-with(Micronutrients_expand_7,ifelse(nutr_no==6,"Vit_A",nutr))
    Micronutrients_expand_7$nutr<-with(Micronutrients_expand_7,ifelse(nutr_no==7,"Retinol",nutr))
    Micronutrients_expand_7$nutr<-with(Micronutrients_expand_7,ifelse(nutr_no==8,"Vit_C",nutr))
    Micronutrients_expand_7$nutr<-with(Micronutrients_expand_7,ifelse(nutr_no==9,"Vit_E",nutr))
    Micronutrients_expand_7$nutr<-with(Micronutrients_expand_7,ifelse(nutr_no==10,"Thiamin",nutr))
    Micronutrients_expand_7$nutr<-with(Micronutrients_expand_7,ifelse(nutr_no==11,"Riboflavin",nutr))
    Micronutrients_expand_7$nutr<-with(Micronutrients_expand_7,ifelse(nutr_no==12,"Niacin",nutr))
    Micronutrients_expand_7$nutr<-with(Micronutrients_expand_7,ifelse(nutr_no==13,"Vit_B6",nutr))
    Micronutrients_expand_7$nutr<-with(Micronutrients_expand_7,ifelse(nutr_no==14,"Folate",nutr))
    Micronutrients_expand_7$nutr<-with(Micronutrients_expand_7,ifelse(nutr_no==15,"Vit_B12",nutr))
    Micronutrients_expand_7$nutr<-with(Micronutrients_expand_7,ifelse(nutr_no==16,"Calcium",nutr))
    Micronutrients_expand_7$nutr<-with(Micronutrients_expand_7,ifelse(nutr_no==17,"Copper",nutr))
    Micronutrients_expand_7$nutr<-with(Micronutrients_expand_7,ifelse(nutr_no==18,"Iron",nutr))
    Micronutrients_expand_7$nutr<-with(Micronutrients_expand_7,ifelse(nutr_no==19,"Magnesium",nutr))
    Micronutrients_expand_7$nutr<-with(Micronutrients_expand_7,ifelse(nutr_no==20,"Phosphorus",nutr))
    Micronutrients_expand_7$nutr<-with(Micronutrients_expand_7,ifelse(nutr_no==21,"Selenium",nutr))
    Micronutrients_expand_7$nutr<-with(Micronutrients_expand_7,ifelse(nutr_no==22,"Zinc",nutr))
    Micronutrients_expand_7$nutr<-with(Micronutrients_expand_7,ifelse(nutr_no==23,"Sodium",nutr))
    colnames(Micronutrients_expand_7)
    Micronutrients_expand_8<-Micronutrients_expand_7[,c(1,2,15,7,3,4,5,6,8,9,10,11,12,13,14)]
    Micronutrients_expand_8$ear<-with(Micronutrients_expand_8,ifelse(nutr=="Price","0",ear))
    colnames(Micronutrients_expand_8)
    Micronutrients_expand_9<-Micronutrients_expand_8[,c(1,2,3,6,7,8,4,5,9,10,11,12,13,14,15)]

    EAR_UL_AMDR_CDRR_allgroups<-Micronutrients_expand_9
    O1<-mutate_each(EAR_UL_AMDR_CDRR_allgroups, funs(as.numeric), c(8))
    EAR_UL_AMDR_CDRR_allgroups<-O1
      rm(O1)

    View(EAR_UL_AMDR_CDRR_allgroups)
      rm(Micronutrients_expand_3)
      rm(Micronutrients_expand_4)
      rm(Micronutrients_expand_5)
      rm(Micronutrients_expand_6)
      rm(Micronutrients_expand_7)
      rm(Micronutrients_expand_8)
      rm(Micronutrients_expand_9)
      rm(Micronutrients_UL_9)
      rm(Micronutrients_UL_expand)
    
save(object=EAR_UL_AMDR_CDRR_allgroups, file = "EAR_UL_AMDR_CDRR_allgroups.RData")

################################################################################
# PART 3 # USE WHO GROWTH CHARTS BASED REFERENCE WEIGHTS 
    whoenergy<-read.csv("2_WHOGrowthCharts_EnergyEER.csv")
    keep<-c("age_sex_grp","PA_level","who_energy","iom_energy")
      whoenergy_2<-whoenergy[keep]
    whoenergy_2$pal<-whoenergy_2$PA_level
  library ("dplyr")
    whoenergy_2$pal<-recode(whoenergy_2$pal,"active"="sedentary","low active"="low active","sedentary"="active", "very active"="very active")
    whoenergy_3<-subset(whoenergy_2,select=-c(PA_level))
    whoenergy_3$pal<-recode(whoenergy_3$pal,"active"=1,"low active"=2,"sedentary"=3, "very active"=4)
    str(whoenergy_3$who_energy)
    str(whoenergy_3$pal)
  library(dplyr)
    whoenergy_3<- mutate_each(whoenergy_3, funs(as.numeric), c(who_energy))
    whoenergy_3%>% group_by(pal) %>% 
      summarise(whoenergy = sum(who_energy, na.rm = TRUE))
    whoenergy_3$nutr_no<-2
    whoenergy<-whoenergy_3
    rm(whoenergy_1)
    rm(whoenergy_2)
    rm(whoenergy_3)

    whoprotein<-read.csv("2_WHOGrowthCharts_ProteinEAR.csv")
    keep<-c("age_sex_grp","who_protein")
    whoprotein_1<-whoprotein[keep]
    whoprotein_1$nutr_no<-4
    whoprotein<-whoprotein_1
    rm(whoprotein_1)

  merge_1<-merge(EAR_UL_AMDR_CDRR_allgroups,whoenergy,by=c("age_sex_grp","nutr_no","pal"),all=T)
  merge_2<-merge(merge_1,whoprotein,by=c("age_sex_grp","nutr_no"),all=T)
  merge_3<- mutate_each(merge_2, funs(as.numeric), c(5,15,17))
  merge_4<-mutate_each(merge_3, funs(as.numeric), c(3))
    str(merge_4$pal)
  merge_4$ear<-with(merge_4,ifelse(nutr_no==2,who_energy,ear))
  merge_4$ear<-with(merge_4,ifelse(nutr_no==4,who_protein,ear))
  merge_5<-subset(merge_4,select=-c(16:18))
  EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups<-merge_5
    rm(merge_1)
    rm(merge_2)
    rm(merge_3)
    rm(merge_4)
    rm(merge_5)
  
save(object=EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups, file = "EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups.RData")
################################################################################
# PART 4 # ADJUSTMENTS FOR THE CONTRIBUTION OF BREASTMILK 
#   If calculating nutrient requirements for infants 6-23 months still breastfeeding,
#		  adjustment must be made to reduce nutrient requirements to only the percentage 
#		  required from food.

    bf<-read.csv("3_NutrientRequirements6-23months_6-23moFoodNeeds.csv")
    keep<-c("age_sex_grp","nutr_no","pctfromfood","bf_match_id")
      bf_1<-bf[keep]
      bf<-bf_1
      rm(bf_1)

  # Using DRI reference weights
    EAR_UL_AMDR_CDRR_allgroups_1<-EAR_UL_AMDR_CDRR_allgroups[rep(row.names(EAR_UL_AMDR_CDRR_allgroups),
    ifelse(EAR_UL_AMDR_CDRR_allgroups$age_sex_grp==2,2,1)),]
    EAR_UL_AMDR_CDRR_allgroups_1$bf_match_id<-NA
    EAR_UL_AMDR_CDRR_allgroups_1$bf_match_id<-with(EAR_UL_AMDR_CDRR_allgroups_1,ifelse(duplicated(EAR_UL_AMDR_CDRR_allgroups_1)=="TRUE","1","0"))
    EAR_UL_AMDR_CDRR_allgroups_1$bf_match_id<-with(EAR_UL_AMDR_CDRR_allgroups_1,
      ifelse(!(age_sex_grp==2|age_sex_grp==3),"",bf_match_id))
    library ("dplyr")
    EAR_UL_AMDR_CDRR_allgroups_1$bf_match_id<-recode(EAR_UL_AMDR_CDRR_allgroups_1$bf_match_id,"0"=1,"1"=2)
    EAR_UL_AMDR_CDRR_allgroups_2<-EAR_UL_AMDR_CDRR_allgroups_1[rep(row.names(EAR_UL_AMDR_CDRR_allgroups_1),
      ifelse(EAR_UL_AMDR_CDRR_allgroups_1$age_sex_grp==3,2,1)),]
    EAR_UL_AMDR_CDRR_allgroups_2$group3split<-NA
    EAR_UL_AMDR_CDRR_allgroups_2$group3split<-with(EAR_UL_AMDR_CDRR_allgroups_2,ifelse(duplicated(EAR_UL_AMDR_CDRR_allgroups_2)=="TRUE","1","0"))
    EAR_UL_AMDR_CDRR_allgroups_2$bf_match_id<-with(EAR_UL_AMDR_CDRR_allgroups_2,
      ifelse(age_sex_grp==3&group3split==0,"3",bf_match_id))
    EAR_UL_AMDR_CDRR_allgroups_2$bf_match_id<-with(EAR_UL_AMDR_CDRR_allgroups_2,
      ifelse(age_sex_grp==3&group3split==1,"",bf_match_id))
    EAR_UL_AMDR_CDRR_allgroups_3<-subset(EAR_UL_AMDR_CDRR_allgroups_2,select=-c(group3split))
      table(EAR_UL_AMDR_CDRR_allgroups_3$age_sex_grp,EAR_UL_AMDR_CDRR_allgroups_3$bf_match_id)
    EAR_UL_AMDR_CDRR_allgroups_4<-merge(EAR_UL_AMDR_CDRR_allgroups_3,bf,by=c("age_sex_grp","bf_match_id","nutr_no"),all=T)

  # Reduce needs of children 6-23 months to only what is required from food
  # Note WHO recommends continued breastfeeding through 2 years
    EAR_UL_AMDR_CDRR_allgroups_4$pctfromfood<-with(EAR_UL_AMDR_CDRR_allgroups_4,
      ifelse(!(age_sex_grp==2|age_sex_grp==3),"",pctfromfood))
    str(EAR_UL_AMDR_CDRR_allgroups_4$ear)
    library ("dplyr")
    EAR_UL_AMDR_CDRR_allgroups_5<- (mutate_each(EAR_UL_AMDR_CDRR_allgroups_4, funs(as.numeric), ear))
    str(EAR_UL_AMDR_CDRR_allgroups_5$ear)
    str(EAR_UL_AMDR_CDRR_allgroups_5$pctfromfood)
    EAR_UL_AMDR_CDRR_allgroups_6<- (mutate_each(EAR_UL_AMDR_CDRR_allgroups_5, funs(as.numeric), pctfromfood))
    str(EAR_UL_AMDR_CDRR_allgroups_6$pctfromfood)
    EAR_UL_AMDR_CDRR_allgroups_6$ear<-with(EAR_UL_AMDR_CDRR_allgroups_6,
      ifelse((!is.na(pctfromfood))&(bf_match_id==2|bf_match_id==3|bf_match_id==1),ear*pctfromfood/100,ear))
    EAR_UL_AMDR_CDRR_BF_allgroups<-EAR_UL_AMDR_CDRR_allgroups_6
      rm(EAR_UL_AMDR_CDRR_allgroups_1)
      rm(EAR_UL_AMDR_CDRR_allgroups_2)
      rm(EAR_UL_AMDR_CDRR_allgroups_3)
      rm(EAR_UL_AMDR_CDRR_allgroups_4)
      rm(EAR_UL_AMDR_CDRR_allgroups_5)
      rm(EAR_UL_AMDR_CDRR_allgroups_6)
save(object=EAR_UL_AMDR_CDRR_BF_allgroups, file = "EAR_UL_AMDR_CDRR_BF_allgroups.RData")

# Using WHO Growth Charts for Reference Weights
    EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_1<-EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups[rep(row.names(EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups),
      ifelse(EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups$age_sex_grp==2,2,1)),]
    EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_1$bf_match_id<-NA
    EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_1$bf_match_id<-with(EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_1,ifelse(EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_1=="TRUE","1","0"))
    EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_1$bf_match_id<-with(EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_1,
      ifelse(!(age_sex_grp==2|age_sex_grp==3),"",bf_match_id))
    library ("dplyr")
    EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_1$bf_match_id<-recode(EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_1$bf_match_id,"0"=1,"1"=2)
    EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_2<-EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_1[rep(row.names(EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_1),
      ifelse(EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_1$age_sex_grp==3,2,1)),]
    EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_2$group3split<-NA
    EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_2$group3split<-with(EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_2,ifelse(duplicated(EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_2)=="TRUE","1","0"))
    EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_2$group3split<-with(EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_2,
      ifelse(age_sex_grp==3&group3split==0,"3",bf_match_id))
    EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_2$bf_match_id<-with(EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_2,
      ifelse(age_sex_grp==3&group3split==1,"",bf_match_id))
    EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_3<-subset(EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_2,select=-c(group3split))
      table(EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_3$age_sex_grp,EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_3$bf_match_id)
    EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_4<-merge(EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_3,bf,by=c("age_sex_grp","bf_match_id","nutr_no"),all=T)

    # Reduce needs of children 6-23 months to only what is required from food
      # Note WHO recommends continued breastfeeding through 2 years
    EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_4$pctfromfood<-with(EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_4,
      ifelse(!(age_sex_grp==2|age_sex_grp==3),"",pctfromfood))
    str(EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_4$ear)
    str(EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_4$pctfromfood)
    library ("dplyr")
    EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_5<- (mutate_each(EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_4, funs(as.numeric), pctfromfood))
    str(EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_5$pctfromfood)
    EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_5$ear<-with(EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_5,
      ifelse((!is.na(pctfromfood))&(bf_match_id==2|bf_match_id==3|bf_match_id==1),ear*pctfromfood/100,ear))
    EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_6<-subset(EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_5,select=-c(pctfromfood))
    EAR_UL_AMDR_CDRR_BF_WHOgrowthref_allgroups<-EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_6
      rm(EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_6)
      rm(EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_5)
      rm(EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_4)
      rm(EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_3)
      rm(EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_2)
      rm(EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_1)
save(object=EAR_UL_AMDR_CDRR_BF_WHOgrowthref_allgroups, file = "EAR_UL_AMDR_CDRR_BF_WHOgrowthref_allgroups.RData")

################################################################################
# PART 5 # EXTRACT INDIVIDUAL DATA FILES 
#   To import for linear programming:
#   Extract separate data files for each age-sex group at desired PAL

  # Example using Group 16 Adult Female (age range: 19-30), DRI Reference Weights and PAL=3
  load(EAR_UL_AMDR_CDRR_allgroups.RData)
    EAR_UL_AMDR_CDRR_allgroups_1<-filter(EAR_UL_AMDR_CDRR_allgroups,age_sex_grp==16)
      table(EAR_UL_AMDR_CDRR_allgroups_1$nutr)
    EAR_UL_AMDR_CDRR_allgroups_2<-filter(EAR_UL_AMDR_CDRR_allgroups_1,!(pal==1|pal==2|pal==4))
    keep<-c("nutr_no","nutr","ear","ul","amdr_lower","amdr_upper","unit","ul_note")
    EAR_UL_AMDR_CDRR_allgroups_3<-EAR_UL_AMDR_CDRR_allgroups_2[keep]
    NR16_DRIWeight_PAL3<-EAR_UL_AMDR_CDRR_allgroups_3
  save(object=NR16_DRIWeight_PAL3, file = "NR16_DRIWeight_PAL3.RData")
    rm(EAR_UL_AMDR_CDRR_allgroups_1)
    rm(EAR_UL_AMDR_CDRR_allgroups_2)
    rm(EAR_UL_AMDR_CDRR_allgroups_3)

  # Example using Group 10 Adult Male (age range: 19-30), WHO Reference Weights and PAL=4 (developing country farmer)
  load(EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups.RData)
    EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_1<-filter(EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups,age_sex_grp==10)
      table(EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_1$nutr)
    EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_2<-filter(EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_1,!(pal==1|pal==2|pal==4))
    keep<-c("nutr_no","nutr","ear","ul","amdr_lower","amdr_upper","unit","ul_note")
    EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_3<-EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_2[keep]
    NR10_WHOWeight_PAL4<-EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_3
  save(object=NR10_WHOWeight_PAL4, file = "NR10_WHOWeight_PAL4.RData")
    rm(EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_1)
    rm(EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_2)
    rm(EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_3)

  # Example using Group 3 (Infants 12-23 months still breastfeeding), WHO Reference Weights and breastfeeding
  load(EAR_UL_AMDR_CDRR_BF_WHOgrowthref_allgroups.RData)
    EAR_UL_AMDR_CDRR_BF_WHOgrowthref_allgroups_1<-filter(EAR_UL_AMDR_CDRR_BF_WHOgrowthref_allgroups,age_sex_grp==3)
      table(AR_UL_AMDR_CDRR_BF_WHOgrowthref_allgroups_1$nutr)
    EAR_UL_AMDR_CDRR_BF_WHOgrowthref_allgroups_2<-filter(AR_UL_AMDR_CDRR_BF_WHOgrowthref_allgroups_1,!(pal==1|pal==2|pal==4))
    keep<-c("nutr_no","nutr","ear","ul","amdr_lower","amdr_upper","unit","ul_note")
    EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_3<-AR_UL_AMDR_CDRR_BF_WHOgrowthref_allgroups_2[keep]
    NR3_WHOWeight_ContBF<-EAR_UL_AMDR_CDRR_WHOgrowthref_allgroups_3
  save(object=NR3_WHOWeight_ContBF4, file = "NR3_WHOWeight_ContBF4.RData")
    rm(EAR_UL_AMDR_CDRR_BF_WHOgrowthref_allgroups_1)
    rm(EAR_UL_AMDR_CDRR_BF_WHOgrowthref_allgroups_2)
    rm(EAR_UL_AMDR_CDRR_BF_WHOgrowthref_allgroups_3)
  






                     
                






























