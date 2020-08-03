Data transformation process from raw to tidy
================
Jonathan
8/1/2020

## Data processing log

### Tidy datasets created as output of this process and stored in `data_tidy` folder:

  - **gem\_21vars\_01\_16.RData**: which includes the 21 variables of
    interest for all the countries in years 2001 to 2016. Saved as
    .RData to optimize space and because we don’t need to share this
    file or work on it.
  - **gem\_balkans\_01\_16.csv**: which includes the 21 variables only
    for the Balkans for the years 2001 to 2016. Saved as .csv to share
    it.
  - **gem\_balkans\_codebook.csv**: includes the variable names, labels,
    descriptions, categories and other details necessary to understand
    the data in *“gem\_balkans\_01\_16.csv”*.
  - **gem\_vars\_metadata.csv**: includes all the variables used in GEM
    reports and datasets. Their names, labels, descriptions, categories
    and other details necessary to understand the entire GEM data in
    case we could need other variable apart from the 21 used.

### Raw GEM datasets

The GEM APS Individual Level datasets from each year were downloaded
from the [GEM Consortium
website](https://www.gemconsortium.org/data/sets?id=aps).

**Warning:** The GEM dataset of 2013 (.zip) had problems to import with
the function `read_sav` (using macOS Mojave 10.14.6, R version 3.6.2),
so it was not readable. To solve this problem, it was necesssary to
decompress the dataset and read the .sav file directly in R with
`read_sav` function, then save a new .sav version of the dataset from R
using `write_sav`. Finally, this new file was compressed in .zip.

Then, the data was imported as a list of lists (ie. each GEM dataset is
a list and all the datasets are contained in one list) using `read_sav`:

``` r
library(haven)


# Import Raw GEM datasets


# Import the files in a list object
gem_data <- lapply(Sys.glob("data_raw/gem_datasets/GEM * APS_Ind.zip"), 
                       read_sav)
```

After importing all GEM datasets, I extracted the metadata from all the
variables used in those datasets for future reference:

``` r
library(labelled)


# Getting the metadata from each variable in each dataset
    vars_metadata <- lapply(gem_data, function(x){
      look_for(x, details = T)
    })
```

Then, I filtered the variables of interest for the research and created
the **codebook** to use in the study. Those are 21 variables of
potential interest that are present in the 16 datasets.

``` r
# Find the common variables between the 16 GEM 
    
    # first make sure all variables of interest have the same spelling
    gem_data = lapply(gem_data, function(x){
      if ("TEAyy" %in% colnames(x)){
      rename(x, teayy = TEAyy, teayyopp = TEAyyOPP, teayynec = TEAyyNEC,
             gemeduc = GEMEDUC, gemwork = GEMWORK)
      } else {x}
    })
    
    # creating a vector with the common variables in the 16 datasets
    vars_common <- Reduce(intersect, lapply(gem_data, names))
    
    # Remove the common variables that are not of interest for the research
    remove_vars <- c("weight", "age9c", "bjobst", "ownmge", "busang", "suacts", 
                     "suown", "suowners", "suwage", "suwageyr", "suyr5job",
                     "sureason", "omown", "omowners", "omwageyr", "omnowjob", 
                     "omyr5job", "omreason", "barel")
    
    # creating the vector with the GEM variables needed for the analysis
    gem_vars <- vars_common[!vars_common %in% remove_vars]
    


# Codebook 
    
    # creating the codebook for the GEM variables
    gem_codebook <- vars_metadata[[1]][vars_metadata[[1]]$variable %in% 
                                         gem_vars,]
```

After that, I filtered the data leaving only the variables of interest
in each dataset (creating a new list called `gem_final`), also checking
that each of the 21 variables have their corresponding class (numeric
for continuous and factor for categorical):

``` r
   # Subset the data by the variables of interest
    gem_final <- lapply(gem_data, function(x){
      select(x, gem_vars)
    })


  # Checking the correct class to each variable
    gem_final <- lapply(gem_final, function(y) {
      lapply(y, function(x) {
          if (class(x) == "numeric"){ 
            x
          } else {
            droplevels(to_factor(x, sort_levels = "l"))
          }
        }
      )})
```

And created the vector of the Balkan countries for the analysis:

``` r
# generate the vector of countries of interest (Balkans)
balkans_list <- c(
  "Albania", "Romania", "Croatia", "Slovenia",
  "Bosnia and Herzegovina", "Macedonia", "Bulgaria",
  "Montenegro", "Serbia", "Kosovo"
)
```

Next, I created a new dataset (list) containing only the Balkans and the
21 variables of interest (`gem_balkans`):

``` r
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
```

At this point, I am managing all the datasets in lists, so I merged the
lists to create a dataframe with the final data (Balkans and 21
variables of interest) to use in the analysis:

``` r
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

# Changing the class of age to numeric
df_gem_balkans$age <- as.numeric(df_gem_balkans$age)
```

As GEM datasets have slightly differences in the spelling of the
categorical varibles options, I recoded the levels of them for
consistency:

``` r
# Recoding factor levels to avoid different spellings
sapply(as.list(df_gem_balkans), levels)

df_gem_balkans$gemeduc <- recode_factor(df_gem_balkans$gemeduc,
  "SOME SECONDARY" = "Some secondary",
  "SECONDARY DEGREE" = "Secondary degree",
  "POST SECONDARY" = "Post secondary",
  "GRAD EXP" = "Graduate experience",
  "NONE" = "None"
)

df_gem_balkans$gemwork <- recode_factor(df_gem_balkans$gemwork,
  "Full: full or part time (includes self-employed)" = "Full: full or part time (includes self-employment)",
  "Part time only" = "Part time only",
  "Student" = "Student",
  "Retired, disabled" = "Retired, disabled",
  "Homemaker" = "Homemaker",
  "Not working" = "Not working, other",
  "Other" = "Not working, other"
)

df_gem_balkans$teayy <- recode_factor(df_gem_balkans$teayy,
  "NO" = "No",
  "YES" = "Yes"
)

df_gem_balkans$teayyopp <- recode_factor(df_gem_balkans$teayyopp,
  "NO" = "No",
  "YES" = "Yes"
)

df_gem_balkans$teayynec <- recode_factor(df_gem_balkans$teayynec,
  "NO" = "No",
  "YES" = "Yes"
)
```

Then exported the datasets created:

``` r
# Write the data -------

# [gem_final (list)] contains all the countries and the 21 variables
# of interest for the years 2001 to 2016. 
# Write it in .RData because is a list of lists (conserves metadata)
save(gem_final, file = "data_tidy/gem_21vars_01_16.RData", compress = T)


# [gem_balkans (list)] contains only the balkans and the 21 variables
# of interest for the years 2001 to 2016
write_csv(df_gem_balkans, "data_tidy/gem_balkans_01_16.csv", col_names = T)


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
```
