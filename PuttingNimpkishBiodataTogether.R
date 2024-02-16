## R packages
library(tidyverse)
library(dplyr)
library(tidyr)
library(readxl)
library(gdata)
library(stringr)

NimpBio04_10 <- read_excel("O:/5. NI STAD/3. Adult Escapement/3. ECVI/Nimpkish River/2011/Pieter_Nimpkish Biodata.xls",1)

NimpBio04_10 <- NimpBio04_10 %>%
  rename(
    Fish_Number = `Original Fish #`,
    Sample_Date = `Sample Start Date (dd/mmm/yy)`,
    Date_Caught = `Date Caught (dd/mm/yyyy)`,
    River_Name = `Project Name`,
    Length_POF_mm = `POF Length (post orbital fork)`,
    Length_POH_mm = `POH Length (post orbital hyperal)`,
    Length_FL_mm = `Fork Length (FL) (nose to fork)`,
    Scale_Book_Number = `Scale Book Number`,
    Scale_Number = `Scale Number`,
    Otolith_Box_Number = `Otolith Box Number`,
    Otolith_Number = `Otolith Number`,
    DNA = `DNA #`,
    Ad_Clipped = `Mark Type (fin clips)`,
    Tag_PITorCWT = `CWT Detection (yes/no/not checked)`,
    Comments = `Comments...82`,
    Comments2 = `Comments...87`
  ) %>%
  mutate(
    Samplers = NA,
    Environmental_Data = NA
  ) %>%
  select(
    Fish_Number,
    Sample_Date,
    Date_Caught,
    River_Name,
    Species,
    Length_POF_mm,
    Length_POH_mm,
    Length_FL_mm,
    Weight,
    Sex,
    Scale_Book_Number,
    Scale_Number,
    Otolith_Box_Number,
    Otolith_Number,
    DNA,
    Ad_Clipped,
    Tag_PITorCWT,
    Samplers,
    Environmental_Data,
    Comments,
    Comments2
  )

view(NimpBio04_10)


## Nimpkish Biodata 2012 ----
NimpBio12 <- read_excel("O:/5. NI STAD/3. Adult Escapement/3. ECVI/Nimpkish River/2012/2012 Nimpkish Chum Biological Data.xls",1)

NimpBio12 <- NimpBio12 %>%
  rename(Sample_Date = Date,
         Fish_Number = Fish,
         River_Name = River,
         Length_POF_mm = `POF Length`,
         Length_POH_mm = `POH Length`,
         Sex = `Sex (1=m; 2=fm)`,
         Scale_Book_Number = `Scale Book`,
         Scale_Number = `Scale #`,
         Samplers = `Sampler (s)`)

NimpBio12 <- NimpBio12 %>%
  mutate(Sex = ifelse(Sex == 1, "Male", ifelse(Sex == 2, "Female", Sex)))

## Convert 'Sex' column in NimpBio12 to character
NimpBio12$Sex <- as.character(NimpBio12$Sex)

## Combine both 2004-2012 (no 2011 biodata) ----
NimpBio <- bind_rows(NimpBio04_10, NimpBio12) %>%
  select(Fish_Number, Sample_Date, Date_Caught, River_Name, Species, Length_POF_mm, Length_POH_mm, Length_FL_mm, Weight, Sex,
         Scale_Book_Number, Scale_Number, Otolith_Box_Number, Otolith_Number,
         DNA, Ad_Clipped, Tag_PITorCWT, Samplers, Environmental_Data, Comments, Comments2)


NimpBio <- NimpBio %>% mutate(River_Name = str_extract(River_Name, "^\\S+\\s+\\S+"))


## 2020 Biodata ----
NimpBio20 <- read_excel("O:/5. NI STAD/3. Adult Escapement/3. ECVI/Nimpkish River/2020/Gwa'ni_Brook_Samples_2020.xlsx")

## Rename existing columns
NimpBio20 <- rename(NimpBio20, River_Name = Stream, Scale_Book_Number = Scalebook, Length_POF_mm = Length_POF, Length_POH_mm = Length_POH,
                    DNA = DNA_Cell, Otolith_Box_Number = Otolith_Box, Otolith_Vial_Number = Otolith_Vial, Sample_Date = Date)

## Identify missing columns in NimpBio20
missing_columns <- setdiff(names(NimpBio), names(NimpBio20))

## Add missing columns to NimpBio20 with NA values
for (col in missing_columns) {
  NimpBio20[[col]] <- NA
}

## Remove "Area" column from NimpBio20
NimpBio20 <- NimpBio20[, !names(NimpBio20) %in% "Area"]


# Add "Otolith_Vial" and "Capture_Site columns to NimpBio filled with NA
NimpBio$Otolith_Vial_Number <- NA
NimpBio$Capture_Site <- NA


# Add rows from NimpBio20 below the last row of NimpBio
NimpBio <- rbind(NimpBio, NimpBio20)


## 2022 Biodata ----
NimpBio22 <- read_excel("O:/5. NI STAD/3. Adult Escapement/3. ECVI/Nimpkish River/2022/2022 Nimpkish Brood Biological Data.xlsx")

NimpBio22 <- rename(NimpBio22, Fish_Number = Fish, Sample_Date = 'Sample Date', River_Name = River, Capture_Site = "Catch Location", Sex = "Sex      (M or F)",
                    Length_POF_mm = "POF Length (mm)", Length_POH_mm = "POH Length (mm)", Scale_Book_Number = "Scale Book", Scale_Number = "Scale #", Ad_Clipped = "Ad Clipped? (Y or N)",
                    DNA = "DNA cell #", Otolith_Box_Number = "Oto box #", Otolith_Vial_Number = "Oto Vial #", Samplers = "Sampler (s)")

## Add missing columns to NimpBio22 with NA values
missing_columns <- setdiff(names(NimpBio), names(NimpBio22))
for (col in missing_columns) {
  NimpBio22[[col]] <- NA
}

## Remove extra columns
NimpBio22 <- NimpBio22[, !names(NimpBio22) %in% "Area"]
NimpBio22 <- NimpBio22[, !names(NimpBio22) %in% "Column1"]
NimpBio22 <- NimpBio22[, !names(NimpBio22) %in% "Column2"]

## Add rows from NimpBio22 below the last row of NimpBio
NimpBio <- rbind(NimpBio, NimpBio22)


## 2023 Biodata ----
NimpBio23 <- read_excel("O:/5. NI STAD/3. Adult Escapement/3. ECVI/Nimpkish River/2023/2023 Nimpkish Brood Biological Data.xlsx")
NimpBio23 <- rename(NimpBio23, Fish_Number = Fish, Sample_Date = 'Sample Date', River_Name = River, Capture_Site = "Catch Location", Sex = "Sex      (M or F)",
                    Length_POF_mm = "POF Length (mm)", Length_POH_mm = "POH Length (mm)", Scale_Book_Number = "Scale Book", Scale_Number = "Scale #", Ad_Clipped = "Ad Clipped? (Y or N)",
                    DNA = "DNA cell #", Otolith_Box_Number = "Oto box #", Otolith_Vial_Number = "Oto Vial #", Samplers = "Sampler (s)")

## Add missing columns to NimpBio23 with NA values
missing_columns <- setdiff(names(NimpBio), names(NimpBio23))
for (col in missing_columns) {
  NimpBio23[[col]] <- NA
}

## Remove extra columns
NimpBio23 <- NimpBio23[, !names(NimpBio23) %in% "Area"]

## Add rows from NimpBio22 below the last row of NimpBio
NimpBio <- rbind(NimpBio, NimpBio23)

## View unique values in some columns----
cat("Unique values in 'Species':\n", unique(NimpBio$Species), "\n\n")
cat("Unique values in 'Sex':\n", unique(NimpBio$Sex), "\n\n")
cat("Unique values in 'DNA':\n", unique(NimpBio$DNA), "\n\n")
cat("Unique values in 'Ad_Clipped':\n", unique(NimpBio$Ad_Clipped), "\n\n")
cat("Unique values in 'Tag_PITorCWT':\n", unique(NimpBio$Tag_PITorCWT), "\n\n")
 
## Species = Chinook and Chum only
NimpBio$Species <- ifelse(tolower(NimpBio$Species) == "chinook", "Chinook", NimpBio$Species)
NimpBio$Species <- ifelse(tolower(NimpBio$Species) == "chum", "Chum", NimpBio$Species)

## Sex = Male, Female, NA, unknown
NimpBio$Sex <- ifelse(tolower(NimpBio$Sex) %in% c("female", "f", "F"), "Female", NimpBio$Sex)
NimpBio$Sex <- ifelse(tolower(NimpBio$Sex) %in% c("male", "m", "M"), "Male", NimpBio$Sex)

## Ad_Clipped = Adipose Clipped, Adipose Unclipped, NA
NimpBio$Ad_Clipped <- ifelse(tolower(NimpBio$Ad_Clipped) == "adipose unclipped", "Adipose Unclipped",
                             ifelse(tolower(NimpBio$Ad_Clipped) == "adipose clipped", "Adipose Clipped",
                                    ifelse(tolower(NimpBio$Ad_Clipped) == "n", "Adipose Unclipped", NimpBio$Ad_Clipped)))

## Because DNA column has Yes, No, NA, and DNA numbers, making a column for DNA(yes or no) and a column for DNA number
### Create a DNA # column
NimpBio <- NimpBio %>%
  mutate(DNA_Number = DNA, .after = "DNA")

### In DNA column, have only yes and no
NimpBio <- NimpBio %>%
  mutate(DNA = ifelse(grepl("^\\d+$", DNA), "Yes", DNA)) %>%
  mutate(DNA = case_when(
    is.na(DNA) ~ NA_character_,
    grepl("^\\d+$", DNA) ~ "Yes",
    tolower(DNA) == "yes" ~ "Yes",
    tolower(DNA) == "no" ~ "No",
    TRUE ~ DNA
  ))

### In DNA # column, remove Yes and No - change to NA as we don't have the number
NimpBio <- NimpBio %>%
  mutate(DNA_Number = case_when(
    tolower(DNA_Number) %in% c("yes", "no") | DNA_Number == "" ~ NA_character_,
    TRUE ~ DNA_Number
  ))
cat("Unique values in 'DNA_Number':\n", unique(NimpBio$DNA_Number), "\n\n")


## Go from having POF, POH, and FL columns to having one "Length_Type" and one with the measurement
NimpBio$Length_Type <- ifelse(!is.na(NimpBio$Length_POF_mm), "POF", 
                         ifelse(!is.na(NimpBio$Length_POH_mm), "POH", 
                                ifelse(!is.na(NimpBio$Length_FL_mm), "FL", NA)))

NimpBio <- NimpBio %>%
  mutate(Length_mm = as.character(case_when(
    Length_Type == "POF" ~ as.character(Length_POF_mm),
    Length_Type == "POH" ~ as.character(Length_POH_mm),
    Length_Type == "FL" ~ as.character(Length_FL_mm),
    TRUE ~ NA_character_
  )))

NimpBio <- NimpBio %>%
  select(-Length_POF_mm, -Length_POH_mm, -Length_FL_mm) #remove three extra length columns

## Clean up river names
NimpBio <- NimpBio %>%
  mutate(River_Name = case_when(
    River_Name %in% c("Nimpkish", "Nimpkish River Brood") ~ "Nimpkish River",
    TRUE ~ River_Name
  ))


## Reorfer columns
NimpBio <- NimpBio %>%
  select(Fish_Number, River_Name, Capture_Site, Sample_Date, Date_Caught,
         Species, Length_Type, Length_mm, Weight, Sex, Scale_Number, Scale_Book_Number,
         Otolith_Number, Otolith_Vial_Number, Otolith_Box_Number, DNA, DNA_Number, Ad_Clipped, Tag_PITorCWT, Samplers,
         Environmental_Data, Comments, Comments2)

## Export new df as an excel file
library(writexl)
write_xlsx(NimpBio, "U:/NimpkishBiodata_2004-2023.xlsx")

