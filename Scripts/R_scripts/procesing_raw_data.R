library(haven)
library(labelled)
library(foreign)
library(tidyverse)
library(ggplot2)



# Import list of variables


varlist_info <- read.csv("data_tidy/varlist_info.csv")


# Variables used in GEM data
vars_gem <- as.vector(varlist_info[8:22, 2])




# Import Raw GEM datasets ----


# Import the files in a list object
gem_data <- lapply(
  Sys.glob("data_raw/gem_datasets/GEM * APS_Ind.zip"),
  read_sav
)


# Getting the metadata from each variable in each dataset
vars_metadata <- lapply(gem_data, function(x) {
  look_for(x, details = T)
})


# Find the common variables between the 16 GEM

# first make sure all variables of interest have the same spelling
gem_data <- lapply(gem_data, function(x) {
  if ("TEAyy" %in% colnames(x)) {
    rename(x,
      teayy = TEAyy, teayyopp = TEAyyOPP, teayynec = TEAyyNEC,
      gemeduc = GEMEDUC, gemwork = GEMWORK
    )
  } else {
    x
  }
})

# creating a vector with the common variables in the 16 datasets
vars_common <- Reduce(intersect, lapply(gem_data, names))

# Remove the common variables that are not of interest for the research
remove_vars <- c(
  "weight", "age9c", "bjobst", "ownmge", "busang", "suacts",
  "suown", "suowners", "suwage", "suwageyr", "suyr5job",
  "sureason", "omown", "omowners", "omwageyr", "omnowjob",
  "omyr5job", "omreason", "barel"
)

# creating the vector with the GEM variables needed for the analysis
gem_vars <- vars_common[!vars_common %in% remove_vars]

# creating the codebook for the GEM variables
gem_codebook <- vars_metadata[[1]][vars_metadata[[1]]$variable %in%
  gem_vars, ]




## Creating a dataframe with only the variables and countries of interest ----


# Subset the data by the variables of interest
gem_final <- lapply(gem_data, function(x) {
  select(x, gem_vars)
})

# Checking the correct class to each variable
gem_final <- lapply(gem_final, function(y) {
  lapply(y, function(x) {
    if (class(x) == "numeric") {
      x
    } else {
      droplevels(to_factor(x, sort_levels = "l"))
    }
  })
})



# generate the vector of countries of interest (Balkans)
balkans_list <- c(
  "Albania", "Romania", "Croatia", "Slovenia",
  "Bosnia and Herzegovina", "Macedonia", "Bulgaria",
  "Montenegro", "Serbia", "Kosovo"
)

# Filter the dataset by the Balkan countries
gem_balkans <- lapply(gem_final, function(x) {
  filter(as.data.frame(x), country %in% balkans_list)
})

# Check the correct class of variable and dropping unused levels of categoricals
gem_balkans <- lapply(gem_balkans, function(y) {
  lapply(y, function(x) {
    if (class(x) == "numeric") {
      x
    } else {
      droplevels(to_factor(x, sort_levels = "l"))
    }
  })
})

# Merge the datasets from different years in one dataframe
c.factor <- function(..., recursive = TRUE) {
  unlist(list(...),
    recursive = recursive
  )
}

df_gem_balkans <- as.data.frame(do.call(mapply, c(
  FUN = c.factor, gem_balkans,
  SIMPLIFY = FALSE
)))

df_gem_21vars <- as.data.frame(do.call(mapply, c(
  FUN = c.factor, gem_final,
  SIMPLIFY = FALSE
)))


# Changing the class of age to numeric
df_gem_balkans$age <- as.numeric(df_gem_balkans$age)
df_gem_21vars$age <- as.numeric(df_gem_21vars$age)


# Recoding factor levels to avoid different spellings
# sapply(as.list(df_gem_balkans), levels)

# gemeduc variable
df_gem_balkans$gemeduc <- recode_factor(df_gem_balkans$gemeduc,
  "SOME SECONDARY" = "Some secondary",
  "SECONDARY DEGREE" = "Secondary degree",
  "POST SECONDARY" = "Post secondary",
  "GRAD EXP" = "Graduate experience",
  "NONE" = "None"
)

df_gem_21vars$gemeduc <- recode_factor(df_gem_21vars$gemeduc,
  "SOME SECONDARY" = "Some secondary",
  "SECONDARY DEGREE" = "Secondary degree",
  "POST SECONDARY" = "Post secondary",
  "GRAD EXP" = "Graduate experience",
  "NONE" = "None"
)


# gemwork variable
df_gem_balkans$gemwork <- recode_factor(df_gem_balkans$gemwork,
  "Full: full or part time (includes self-employed)" = "Full: full or part time (includes self-employment)",
  "Part time only" = "Part time only",
  "Student" = "Student",
  "Retired, disabled" = "Retired, disabled",
  "Homemaker" = "Homemaker",
  "Not working" = "Not working, other",
  "Other" = "Not working, other"
)

df_gem_21varss$gemwork <- recode_factor(df_gem_21vars$gemwork,
  "Full: full or part time (includes self-employed)" = "Full: full or part time (includes self-employment)",
  "Part time only" = "Part time only",
  "Student" = "Student",
  "Retired, disabled" = "Retired, disabled",
  "Homemaker" = "Homemaker",
  "Not working" = "Not working, other",
  "Other" = "Not working, other"
)

# teayy variable
df_gem_balkans$teayy <- recode_factor(df_gem_balkans$teayy,
  "NO" = "No",
  "YES" = "Yes"
)

df_gem_21vars$teayy <- recode_factor(df_gem_21vars$teayy,
  "NO" = "No",
  "YES" = "Yes"
)

# teayyopp variable
df_gem_balkans$teayyopp <- recode_factor(df_gem_balkans$teayyopp,
  "NO" = "No",
  "YES" = "Yes"
)

df_gem_21vars$teayyopp <- recode_factor(df_gem_21vars$teayyopp,
  "NO" = "No",
  "YES" = "Yes"
)

# teayynec variable
df_gem_balkans$teayynec <- recode_factor(df_gem_balkans$teayynec,
  "NO" = "No",
  "YES" = "Yes"
)

df_gem_21vars$teayynec <- recode_factor(df_gem_21vars$teayynec,
  "NO" = "No",
  "YES" = "Yes"
)






# Summaries -----
df_gem_balkans %>%
  group_by(yrsurv) %>%
  summarise(
    panels = n_distinct(country),
    n_obs = n(),
    TEA = mean(as.numeric(teayy), na.rm = T)
  )

df_gem_balkans %>%
  group_by(country) %>%
  summarise(
    panels = n_distinct(yrsurv),
    n_obs = n(),
    TEA = mean(as.numeric(teayy), na.rm = T),
    TEA_NEC = mean(as.numeric(teayynec), na.rm = T),
    TEA_OPP = mean(as.numeric(teayyopp), na.rm = T)
  )

df_gem_balkans %>%
  group_by(yrsurv) %>%
  summarise(countries = unique(country))


df_gem_balkans %>%
  group_by(country) %>%
  summarise(
    panels = n_distinct(yrsurv),
    n_obs = n(),
    TEA = sum(as.numeric(teayy) - 1) / n(),
    TEA_NEC = sum(as.numeric(teayynec) - 1) / n(),
    TEA_OPP = sum(as.numeric(teayyopp) - 1) / n()
  )



# Graphs
# Availability of information 
ggplot(data = df_gem_balkans, aes(x = yrsurv, y = country)) +
  geom_line()





# Write the data -------

# [gem_final (list)] contains all the countries and the 21 variables
# of interest for the years 2001 to 2016. 
# Write it in .RData because is a list of lists (conserves metadata)
#save(gem_data, file = "data_tidy/gem_complete_list_01_16.RData", compress = T)
save(gem_final, file = "data_tidy/gem_21vars_01_16.RData", compress = T)


# [gem_balkans (list)] contains only the balkans and the 21 variables
# of interest for the years 2001 to 2016
write_csv(df_gem_balkans, "data_tidy/gem_balkans_01_16.csv", col_names = T)
#write_csv(df_gem_21vars, "data_tidy/gem_21vars_01_16.csv", col_names = T)


# [gem_codebook (dataframe)] contains the 21 variables of interest and
# their descriptions
write_csv(gem_codebook, "data_tidy/gem_balkans_codebook.csv", col_names = T)


# [vars_metadata (list)] contains descriptions about all the variables
# used in the GEM datasets from 2001 to 2016
lapply(vars_metadata, function(x) {
  write_csv(
    data.frame(x),
    "data_tidy/gem_vars_metadata.csv", append = T
  )
})





