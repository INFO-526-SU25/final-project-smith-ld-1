

get_race_mappings <- function(){ 
  white <- c(
    "WHITE" = "White",
    "HISPANIC WHITE" = "White",
    "EUROPEAN AMERICAN" = "White",
    "CAUCASIAN OR WHITE" = "White",
    "WHITE, NON-HISPANIC" = "White",
    "CAUCASIAN" = "White",
    "NON-HISPANIC WHITE" = "White",
    "NH WHITE" = "White",
    "NON-HISPANIC WHITE AND ADDITIONAL RACE AND ETHNICITIES" = "White",
    "WHITE/CAUCASIAN"="White",
    "NON HISPANIC WHITES"="White",
    "CAUCASIANS"="White",
    "WHITE - NOT HISPANIC OR LATINO",
    "WHITE (NON-HISPANIC)"="White",
    "WHITE NON-HISPANIC"="White",
    "WHITE DONOR / WHITE RECIPIENT"="White",
    "U.S.-BORN, NON-HISPANIC WHITE;"="White",
    "NON-HISPANIC, WHITE"="White",
    "WHITE (OR CAUCASIAN)"="White",
    "WHITES"="White",
    "WHITE-NON-HISPANIC"="White",
    "NON-LATINA WHITE"="White",
    "WHITE OR CAUCASIAN"="White",
    "WHITE - NOT HISPANIC OR LATINO"="White",
    "NON HISPANIC WHITE"="White"
    
  )
  
  black <- c(
    "BLACK" = "Black or African American",
    "NON-HISPANIC BLACK" = "Black or African American",
    "AFRICAN AMERICAN" = "Black or African American",
    "BLACK/AFRICAN AMERICAN" = "Black or African American",
    "BLACK, NON-HISPANIC" = "Black or African American",
    "BLACK NON-HISPANIC" = "Black or African American",
    "BLACK OR AFRICAN AMERICAN" = "Black or African American",
    "AFRICAN AMERICANS" = "Black or African American",
    "AFRICAN-AMERICAN" = "Black or African American",
    "AFRICAN AMERICAN/BLACK" = "Black or African American",
    "U.S.-BORN, NON-HISPANIC BLACK" = "Black or African American",
    "NON-HISPANIC, BLACK" = "Black or African American",
    "BLACKS" = "Black or African American",
    "BLACK (NON-HISPANIC)" = "Black or African American",
    "NONBLACK" = "Black or African American",
    "AFRICAN AMERICAN OR BLACK" = "Black or African American",
    "BLACK DONOR / BLACK RECIPIENT" = "Black or African American",
    "BLACK DONOR / NONBLACK RECIPIENT" = "Black or African American",
    "AFRICAN AMERICAN, NON-HISPANIC" = "Black or African American",
    "AFRICAN-AMERICAN, NON-HISPANIC" = "Black or African American",
    "NON-HISPANIC BLACK/AFRICAN AMERICAN" = "Black or African American"
  )
  
  hispanic <- c(
    "HISPANIC" = "Hispanic or Latino",
    "HISPANIC WHITES" = "Hispanic or Latino",
    "HISPANIC/LATINA" = "Hispanic or Latino",
    "LATINA/HISPANIC" = "Hispanic or Latino",
    "US-BORN HISPANIC" = "Hispanic or Latino",
    "OTHER HISPANIC" = "Hispanic or Latino",
    "LATINA" = "Hispanic or Latino",
    "HISPANIC OR LATINO"="Hispanic or Latino",
    "LATINO"="Hispanic or Latino",
    "LATINO AMERICAN"="Hispanic or Latino",
    "HISPANIC OR LATINA"="Hispanic or Latino",
    "HISPANIC OR LATINO"="Hispanic or Latino",
    "HISPANIC (OF ANY RACE)"="Hispanic or Latino",
    "HISPANIC, SPANISH-SPEAKING"="Hispanic or Latino",
    "LATINA WHITE"="Hispanic or Latino",
    "FOREIGN-BORN HISPANIC"="Hispanic or Latino",
    "HISPANIC, NOT OTHERWISE SPECIFIED"="Hispanic or Latino",
    "HISPANIC DONOR / HISPANIC RECIPIENT"="Hispanic or Latino",
    "HISPANIC/LATINX"="Hispanic or Latino",
    "HISPANIC/LATINO/A"="Hispanic or Latino",
    "HISPANIC DONOR / NON-HISPANIC RECIPIENT"="Hispanic or Latino",
    "HISPANIC, ENGLISH-SPEAKING"="Hispanic or Latino",
    "HISPANIC AMERICAN"="Hispanic or Latino",
    "HISPANIC, LATINA"="Hispanic or Latino",
    "HISPANIC, ENGLISH-SPEAKING"="Hispanic or Latino",
    "HISPANIC OR LATINX"="Hispanic or Latino",
    "LATINA (SPANISH SPEAKING)"="Hispanic or Latino",
    "LATINA (ENGLISH-SPEAKING)"="Hispanic or Latino",
    "HISPANIC/LATINO"="Hispanic or Latino"
  )
  
  asian <- c(
    "ASIAN" = "Asian",
    "ASIAN AMERICAN" = "Asian",
    "NON-HISPANIC ASIAN" = "Asian",
    "ASIAN/PACIFIC ISLANDER" = "Asian",
    "ASIAN OR PACIFIC ISLANDER" = "Asian",
    "ASIAN/PACIFIC" = "Asian",
    "NON-HISPANIC ASIAN OR PACIFIC ISLANDER" = "Asian",
    "CHINESE"="Asian",
    "NON-HISPANIC, ASIAN" = "Asian",
    "ASIAN DONOR / NON-ASIAN RECIPIENT" = "Asian",
    "ASIAN DONOR / ASIAN RECIPIENT" = "Asian",
    "SOUTH ASIAN" = "Asian",
    "ASIAN AND OTHER" = "Asian",
    "ASIAN OR ASIAN AMERICAN" = "Asian",
    "ASIAN/NHOPI" = "Asian",
    "ASIAN, NON-HISPANIC" = "Asian",
    "ASIAN AMERICAN AND PACIFIC ISLANDER" = "Asian",
    "ASIAN/ASIAN AMERICAN" = "Asian"
    
  )
  
  aian <- c(
    "AMERICAN INDIAN/ALASKA NATIVE" = "American Indian or Alaska Native",
    "NON-HISPANIC AMERICAN INDIAN/ALASKA NATIVE" = "American Indian or Alaska Native",
    "NON-HISPANIC NATIVE AMERICAN" = "American Indian or Alaska Native",
    "AMERICAN INDIAN/ALASKA NATIVE" = "American Indian or Alaska Native",
    "AMERICAN INDIAN/ALASKAN" = "American Indian or Alaska Native",
    "AMERICAN INDIAN, ALASKA NATIVE, AND NATIVE HAWAIIAN" = "American Indian or Alaska Native",
    "AMERICAN INDIAN OR ALASKA NATIVE" = "American Indian or Alaska Native",
    "AMERICAN INDIAN OR ALASKA NATIVE, NON-HISPANIC" = "American Indian or Alaska Native",
    "NATIVE AMERICAN" = "American Indian or Alaska Native",
    "NON-HISPANIC AMERICAN INDIAN" = "American Indian or Alaska Native",
    "INDIGENOUS AMERICAN" = "American Indian or Alaska Native",
    "AMERICAN INDIAN/ALASKAN NATIVE"="American Indian or Alaska Native",
    "AMERICAN INDIAN/ALASKA NATIVE"="American Indian or Alaska Native",
    "INDIAN AMERICAN"="American Indian or Alaska Native",
    "NATIVE AMERICAN OR ALASKAN NATIVE"="American Indian or Alaska Native"
  )
  
  other_unk <- c(
    "AMERICAN INDIAN"= "Other",
    "ALASKAN NATIVE"= "Other",
    "NON-HISPANIC AMERICAN INDIAN OR ALASKA NATIVE" = "Other",
    "AMERICAN INDIAN OR ALASKAN" = "Other",
    "INDIGENOUS" = "Other",
    "OTHER" = "Other",
    "OTHER RACE" = "Other",
    "OTHERS"="Other",
    "OTHER/UNKNOWN"="Other",
    "WHITE DONOR / NONWHITE RECIPIENT" = "Other",
    "NON WHITE" = "Other",
    "NON WHITE RACE" = "Other",
    "NON-WHITE" = "Other",
    "NON-BLACK" = "Other",
    "HBNO (HISPANIC, BLACK, NATIVE AMERICAN/ALASKAN AND OTHER)" = "Other",
    "NON-HISPANIC OTHER" = "Other",
    "NON-HISPANIC OTHER RACE" = "Other",
    "NON-HISPANIC, ALL OTHERS" = "Other",
    "OTHER OR MULTIPLE RACES, NON-HISPANIC" = "Other",
    "HISPANIC ETHNICITY" = "Other",
    "MULTIRACIAL" = "Other",
    "ADDITIONAL RACES AND ETHNICITIES OR MIXED" = "Other",
    "MULTIPLE OR OTHER" = "Other",
    "TWO OR MORE RACES" = "Other",
    "NON-HISPANIC OTHER-MULTI-RACE" = "Other",
    "OTHER OR MISSING" = "Other",
    "OTHER, MIXED, UNKNOWN" = "Other",
    "SOMALIA-BORN" = "Other",
    "AFRICAN-BORN BLACK" = "Other",
    "OTHER MINORITY" = "Other",
    "OTHER/MULTIRACIAL" = "Other",
    "MEXICAN/AMERICAN" = "Other",
    "OTHER RACES OR ETHNICITY" = "Other",
    "MULTIRACIAL OR OTHER" = "Other",
    "OTHER RACE-ETHNICITY" = "Other",
    
    "UNKNOWN"="Unknown",
    "NOT IDENTIFIED"="Unknown",
    "OTHER OR UNKNOWN"="Unknown",
    "OTHER OR ASIAN"="Unknown",
    "MULTIRACIAL"="Unknown",
    "MISSING"="Unknown",
    "OTHER/UNKNOWN"="Unknown",
    "MORE THAN ONE RACE"="Unknown",
    "MULTIPLE RACES"="Unknown",
    "NOT IDENTIFIED"="Unknown",
    "UNKNOWN OR MISSING"="Unknown",
    "NONE OF THE ABOVE"="Unknown",
    "UNABLE TO DETERMINE"="Unknown",
    "DECLINED OR UNKNOWN"="Unknown",
    "NATIVE AMERICAN OR UNKNOWN"="Unknown",
    "UNKNOWN/OTHER"="Unknown",
    "OTHER/NOS/UNKNOWN"="Unknown",
    "OTHER/MISSING"="Unknown",
    "ALL OTHER/UNKNOWN OR MISSING"="Unknown",
    "RECIPIENTS MISSING RACE/ETHNICITY"="Unknown",
    "UNAVAILABLE"="Unknown",
    "OTHERS OR UNKNOWN"="Unknown",
    "NONE OF THE ABOVE, MULTIPLE, OR UNKNOWN"="Unknown",
    "DECLINED OR UNAVAILABLE"="Unknown",
    "NONE OF THE ABOVE, NON-HISPANIC"="Unknown",
    "UNKNOWN/NOT DOCUMENTED"="Unknown",
    "ITEM MISSED OR SKIPPED"="Unknown",
    "OTHER, MIXED, OR UNKNOWN"="Unknown"
  )
  
  nhpi <- c(
    "HAWAIIAN"="Native Hawaiian Or Pacific Islander",
    "PACIFIC ISLANDER/HAWAIIAN"="Native Hawaiian Or Pacific Islander",
    "NATIVE HAWAIIAN"="Native Hawaiian Or Pacific Islander",
    "PACIFIC ISLANDER"="Native Hawaiian Or Pacific Islander",
    "NON-HISPANIC ASIAN/PACIFIC ISLANDER"="Native Hawaiian Or Pacific Islander",
    "NONHISPANIC ASIAN OR PACIFIC ISLANDER"="Native Hawaiian Or Pacific Islander",
    "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER"="Native Hawaiian Or Pacific Islander",
    "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER, NON-HISPANIC"="Native Hawaiian Or Pacific Islander",
    "HAWAIIAN/PACIFIC ISLANDER"="Native Hawaiian Or Pacific Islander",
    "NON-HISPANIC ASIAN AND PACIFIC ISLANDER"="Native Hawaiian Or Pacific Islander",
    "NATIVE HAWAIIAN OR PACIFIC ISLANDER"="Native Hawaiian Or Pacific Islander",
    "NATIVE HAWAIIAN/PACIFIC ISLANDER"="Native Hawaiian Or Pacific Islander",
    
    "URM (BLACK, HISPANIC, NATIVE AMERICAN/ALASKAN, ASIAN/PACIFIC ISLANDER, AND OTHER)"="Uncategorized",
    "WHITE/OTHER"="Uncategorized",
    "SOUTH AND CENTRAL AMERICAN"="Uncategorized",
    "PUERTO RICAN"="Uncategorized",
    "MEXICAN"="Uncategorized",
    "CUBAN"="Uncategorized",
    "NONWHITE"="Uncategorized",
    "NH OTHER"="Uncategorized",
    "NON-HISPANIC, NONE OF THE ABOVE"="Uncategorized",
    "NON-HISPANIC MIXED"="Uncategorized",
    "AMERICAN INDIAN, ALASKA NATIVE, OR OTHER"="Uncategorized",
    "ASIAN, NATIVE HAWAIIAN, OR PACIFIC ISLANDER"="Uncategorized",
    "NATIVE"="Uncategorized",
    "NH BLACK"="Uncategorized",
    "OTHER (ASIAN, PACIFIC ISLANDER, NATIVE AMERICAN, MIXED OR OTHER)"="Uncategorized",
    "NON AFRICAN AMERICAN"="Uncategorized",
    "NON-AFRICAN AMERICAN"="Uncategorized",
    "EUROPEAN AMERICANS"="Uncategorized",
    "MINORITY"="Uncategorized"
  )
  
  
  race_mapping <- c(
    white,
    black,
    hispanic,
    asian,
    aian,
    other_unk,
    nhpi
  )  
  
  return (race_mapping)
  
}


load_and_cleanse_race <- function(df) {
  
  # get mappings of race to cleans
  race_mapping <- get_race_mappings()
  
  # vector of all race columns
  race_columns <- c("race1", "race2", "race3", "race4", "race5", "race6", "race7", "race8")
  
  # pivot data frame longer of given race columns
  
  unique_race_values <- df |>
    pivot_longer(cols = all_of(race_columns), names_to="race_column", values_to = "race_cleansed") |>
    mutate(race_cleansed=toupper(race_cleansed))
  
  
  # assign a new column of the cleansed values
  unique_race_values <- unique_race_values |>
    mutate(clean_race = race_mapping[as.character(race_cleansed)])
  
  return (unique_race_values)
}



assign_race_quantities <- function(df) {
  # knowing that a row will have what reference we need, it will not have 
  # the reference of what the individual value is, so we can find it and save
  # it here 
  rows_with_values <- df |> 
    rowwise() |>
    mutate(race_value = cur_data()[[paste0(race_column, "_ss")]]) 
  
  return (rows_with_values)
}


aggregate_values_by_year_and_race <- function(df) {
  data <- df |> select(clean_race, race_value, journal, year) |> 
    filter(!is.na(clean_race) | !is.na(race_value))
  
  
  aggregated_values <- data |> group_by(year, clean_race) |> summarise(
    total_counts = sum(race_value)
  )
  
  # we find that some values = -99, initial journal incorrect inputs
  
  aggregated_values <- aggregated_values |> ungroup() |> filter(total_counts != -99)
  
  return (aggregated_values)
}


get_relative_proportions <- function(df) {
  proportions <- df |> 
    group_by(year, clean_race) |> 
    summarise(n = sum(total_counts)) |>
    mutate(freq = (n / sum(n)) * 100)
  
  return (proportions)
}

