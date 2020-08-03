Data Analysis
================
Jonathan
8/3/2020

# Dataset descriptions

1.  ***balkans\_gem\_02\_16.csv***: which includes the 21 variables only
    for the Balkans for the years 2002 to 2016. Saved as .csv to share
    it.
2.  ***balkans\_gem\_rem\_wb\_02\_16.csv***: which includes 39 variables
    (21 from GEM + 1 from Remittances + 17 from World Bank of Balkans
    for 2002 to 2016). Saved as .csv.
3.  ***balkans\_gem\_rem\_mig\_wb\_05\_15.csv***: which includes the 40
    variables (21 from GEM + 1 from Remittances + 1 from Migrarion + 17
    from World Bank of Balkans for 2005, 2010 and 2015).
4.  ***balkans\_codebook.csv***: includes the variable names, labels,
    descriptions, categories and other details necessary to understand
    the data in *“balkans\_gem\_02\_16.csv”*.
5.  ***gem\_vars\_metadata.csv***: includes all the variables used in
    GEM reports and datasets. Their names, labels, descriptions,
    categories and other details necessary to understand the entire GEM
    data in case we could need other variable apart from the used.

-----

# Analysis of the data

First, lets import the datasets created in the [data
processing](https://github.com/jonaperezl/Diaspora-EE-NIS/blob/master/data_tidy/README.md).
In this case, the datasets (2) and (3):

``` r
library(readr)
library(here)
```

    ## here() starts at /Users/jonathan/OneDrive - Universidad de los Andes/Projects/IEMJ-SI/Diaspora-EE-NIS

``` r
# Import datasets 

# Dataset with only remittances 
balkans_gem_rem_wb_02_16 <- read_csv(here("data_tidy",
                                              "balkans_gem_rem_wb_02_16.csv"), 
                           col_types = "dfdfdffffffffffffffffddddffddddddddddddd")


# Dataset wit remittances and migration
balkans_gem_rem_mig_wb_05_15 <- read_csv(here("data_tidy",
                                              "balkans_gem_rem_mig_wb_05_15.csv"), 
                           col_types = "dfdfdffffffffffffffffdddddffdddddddddddddd")


summary(balkans_gem_rem_mig_wb_05_15)
```

    ##      yrsurv                       country         setid          
    ##  Min.   :2005   Croatia               :6000   Min.   :4.010e+10  
    ##  1st Qu.:2010   Slovenia              :8037   1st Qu.:3.821e+11  
    ##  Median :2010   Bosnia and Herzegovina:2000   Median :3.852e+11  
    ##  Mean   :2011   Macedonia             :4003   Mean   :3.323e+11  
    ##  3rd Qu.:2015   Montenegro            :2000   3rd Qu.:3.862e+11  
    ##  Max.   :2015   Romania               :4237   Max.   :3.892e+11  
    ##                 Bulgaria              :2002                      
    ##     gender           age        bstart       futsup      discent     
    ##  Female:15009   Min.   : 2.0   No  :25705   No  :22149   No  :25320  
    ##  Male  :13269   1st Qu.:18.0   Yes : 2478   Yes : 4209   Yes :  694  
    ##  NA's  :    1   Median :30.0   NA's:   96   NA's: 1921   NA's: 2265  
    ##                 Mean   :29.8                                         
    ##                 3rd Qu.:41.0                                         
    ##                 Max.   :83.0                                         
    ##                 NA's   :82                                           
    ##  knowent       opport      suskill      fearfail     equalinc     nbgoodc     
    ##  Yes :10050   No  :16016   No  :12202   No  :14527   Yes :18434   Yes :14728  
    ##  No  :15663   Yes : 6069   Yes :12703   Yes :10158   No  : 6379   No  : 8581  
    ##  NA's: 2566   NA's: 6194   NA's: 3374   NA's: 3594   NA's: 3466   NA's: 4970  
    ##                                                                               
    ##                                                                               
    ##                                                                               
    ##                                                                               
    ##  nbstatus     nbmedia     
    ##  No  : 8830   No  :10971  
    ##  Yes :14818   Yes :12869  
    ##  NA's: 4631   NA's: 4439  
    ##                           
    ##                           
    ##                           
    ##                           
    ##                                                gemwork     
    ##  Not working, other                                : 4049  
    ##  Homemaker                                         : 1442  
    ##  Student                                           : 2064  
    ##  Retired, disabled                                 : 4920  
    ##  Full: full or part time (includes self-employment):14858  
    ##  Part time only                                    :  622  
    ##  NA's                                              :  324  
    ##                 gemeduc      teayy       teayyopp    teayynec   
    ##  Some secondary     : 6399   No :26515   No :27192   No :27649  
    ##  Graduate experience:  984   Yes: 1764   Yes: 1087   Yes:  630  
    ##  Secondary degree   :11957                                      
    ##  Post secondary     : 7299                                      
    ##  None               : 1261                                      
    ##  NA's               :  379                                      
    ##                                                                 
    ##   remittances       migration          gdp_ppp          gdp_percap_ppp 
    ##  Min.   : 164.5   Min.   : 119620   Min.   :8.452e+09   Min.   : 9355  
    ##  1st Qu.: 306.7   1st Qu.: 138082   1st Qu.:3.466e+10   1st Qu.:13831  
    ##  Median : 641.4   Median : 816168   Median :6.528e+10   Median :18342  
    ##  Mean   :1269.9   Mean   : 995851   Mean   :1.069e+11   Mean   :19442  
    ##  3rd Qu.:1822.3   3rd Qu.:1155732   3rd Qu.:9.674e+10   3rd Qu.:23853  
    ##  Max.   :5391.4   Max.   :3370044   Max.   :4.286e+11   Max.   :31637  
    ##                                                                        
    ##  ease_business_score transp_corrup_rating business_reg_rating
    ##  Min.   :71.35       3   : 2000           4   : 2000         
    ##  1st Qu.:72.46       NA's:26279           NA's:26279         
    ##  Median :72.72                                               
    ##  Mean   :73.83                                               
    ##  3rd Qu.:74.71                                               
    ##  Max.   :77.92                                               
    ##  NA's   :18265                                               
    ##  new_business_density new_business_registered rail_lines_km  
    ##  Min.   :2.481        Min.   : 2370           Min.   :  249  
    ##  1st Qu.:3.829        1st Qu.: 5550           1st Qu.: 1027  
    ##  Median :3.995        Median : 6729           Median : 1228  
    ##  Mean   :4.688        Mean   :20445           Mean   : 3004  
    ##  3rd Qu.:4.815        3rd Qu.:46518           3rd Qu.: 2726  
    ##  Max.   :9.863        Max.   :64417           Max.   :10777  
    ##  NA's   :7016         NA's   :7016                           
    ##    population       exports_per_gdp  education_per_gdp fdi_inflows_per_gdp
    ##  Min.   :  619428   Min.   :0.2970   Min.   :0.031     Min.   :0.001055   
    ##  1st Qu.: 2048583   1st Qu.:0.3673   1st Qu.:0.035     1st Qu.:0.019334   
    ##  Median : 3705472   Median :0.4102   Median :0.049     Median :0.026813   
    ##  Mean   : 5594854   Mean   :0.4812   Mean   :0.046     Mean   :0.036578   
    ##  3rd Qu.: 4310145   3rd Qu.:0.5981   3rd Qu.:0.056     3rd Qu.:0.039984   
    ##  Max.   :20246871   Max.   :0.7715   Max.   :0.056     Max.   :0.183189   
    ##                                      NA's   :14005                        
    ##  cost_business_start_per_gni urban_population_per_total remittances_per_gdp
    ##  Min.   :0.0000              Min.   :0.4556             Min.   :0.001866   
    ##  1st Qu.:0.0000              1st Qu.:0.5266             1st Qu.:0.005472   
    ##  Median :0.1330              Median :0.5389             Median :0.011353   
    ##  Mean   :0.1536              Mean   :0.5581             Mean   :0.017717   
    ##  3rd Qu.:0.2270              3rd Qu.:0.5709             3rd Qu.:0.026058   
    ##  Max.   :0.4500              Max.   :0.7399             Max.   :0.052572   
    ##                                                                            
    ##  migration_per_pop   gdp_growth       gdp_percap_growth   exports_growth   
    ##  Min.   :0.05980   Min.   :-0.03901   Min.   :-0.033287   Min.   :0.04563  
    ##  1st Qu.:0.06692   1st Qu.: 0.01344   1st Qu.: 0.009028   1st Qu.:0.06448  
    ##  Median :0.16967   Median : 0.02734   Median : 0.032738   Median :0.10161  
    ##  Mean   :0.17930   Mean   : 0.02083   Mean   : 0.022388   Mean   :0.10031  
    ##  3rd Qu.:0.24052   3rd Qu.: 0.03856   3rd Qu.: 0.037783   3rd Qu.:0.11364  
    ##  Max.   :0.42350   Max.   : 0.04314   Max.   : 0.046552   Max.   :0.23680  
    ## 

## Descriptive statistics

``` r
quantile(balkans_gem_rem_wb_02_16$remittances_per_gdp, na.rm = T)
```

    ##          0%         25%         50%         75%        100% 
    ## 0.001865727 0.005652359 0.012147369 0.027979796 0.080174991

``` r
mean(balkans_gem_rem_mig_wb_05_15$remittances_per_gdp)
```

    ## [1] 0.01771728

``` r
table(balkans_gem_rem_mig_wb_05_15$country,
      balkans_gem_rem_mig_wb_05_15$yrsurv)
```

    ##                         
    ##                          2005 2010 2015
    ##   Croatia                2000 2000 2000
    ##   Slovenia               3016 3012 2009
    ##   Bosnia and Herzegovina    0 2000    0
    ##   Macedonia                 0 2002 2001
    ##   Montenegro                0 2000    0
    ##   Romania                   0 2235 2002
    ##   Bulgaria                  0    0 2002
