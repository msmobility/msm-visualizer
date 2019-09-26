####### The functions used in the reactives #######

# function for subsetting data related to the selected silo spatial attribute
siloSpatialSelector <- function(myData, zoneLevel, myAttribute, myYear){
  reactive({
    zonesData <- zones %>% 
      select(shp_id, shp_muni, shp_area)
    df <- myData() %>% 
      select(Zone, Year, myAttribute) %>% 
      filter(Year == myYear) %>% 
      inner_join(zonesData, by = c("Zone" = "shp_id"))
    if(zoneLevel == FALSE){
      df <- df %>% 
        group_by(shp_muni) %>% 
        st_sf()
      if(myAttribute %in% c(siloAccessibilities, "avePrice")){
        df <- df %>% 
          summarize_at(myAttribute, funs(mean), na.rm = TRUE) %>% 
          ungroup()
      } else {
        df <- df %>% 
          summarize_at(myAttribute, funs(1000000 * sum(.) / sum(shp_area))) %>% 
          ungroup()
      }
      df <- df %>% select(shp_muni, myAttribute, geometry)
    } else {
      df <- df %>% select(Zone, myAttribute, geometry) %>% 
        st_sf()
    }
    df[[2]] = round(df[[2]], digits = 2)
    df
  })
}


### functions for subsetting data related to the selected silo aspatial attribute ###

siloAspatialSelector0 <- function(myData, myFilter){
  reactive({
    df <- filter(myData(), Attribute %in% myFilter) %>%
      separate(Results, into = c("Key", "Value"), sep = ",") %>%
      select(Year, Key, Value) %>%
      mutate(Value = as.numeric(Value))
    df
  })
}

siloAspatialSelector1 <- function(myData, myFilter, myColumns){
  reactive({
    df <- filter(myData(), Attribute %in% myFilter) %>%
      separate(Results, into = myColumns, sep = ",") %>%
      select(Year, Key, Value) %>%
      mutate(Value = as.numeric(Value))
    df
  })
}

siloAspatialSelector2 <- function(myData, myFilter, myColumns){
  reactive({
    df <- filter(myData(), Attribute %in% myFilter) %>% 
      separate(Results, into = myColumns, sep = ",") %>% 
      gather(key = "Key", value = "Value", -Year, -Attribute, -Key0) %>% 
      mutate(Key = str_replace_all(Key, "i", " ")) %>% 
      select(Year, Key, Value) %>% 
      mutate(Value = as.numeric(Value))
    df
  })
}

siloAspatialSelector3 <- function(myData, myFilter, myColumns, iReplace = FALSE){
  reactive({
    df <- filter(myData(), Attribute %in% myFilter) %>% 
      separate(Results, into = myColumns, sep = ",") %>% 
      gather(key = "Key", value = "Value", -Year, -Attribute, -Key2, -Key0) %>% 
      select(Year, Key, Key2, Value) %>% 
      mutate(Value = as.numeric(Value))
    if(iReplace == TRUE){
      df <- df %>% 
        mutate(Key = str_replace_all(Key, "i", " "))
    }
    df
  })
}

siloAspatialSelector4 <- function(myData, myFilter, myColumns){
  reactive({
    df <- filter(myData(), Attribute %in% myFilter) %>% 
      separate(Results, into = myColumns, sep = ",") %>% 
      gather(key = "Key", value = "Value", -Year, -Attribute, -Key0, -Key00) %>% 
      select(Year, Key, Value) %>% 
      mutate(Value = as.numeric(Value))
    df
  })
}

siloAspatialSelector5 <- function(myData, myFilter, mySeparator, myReplace){
  reactive({
    df <- filter(myData(), Attribute %in% myFilter) %>% 
      separate(Results, into = c("Key", "Value"), sep = ",") %>%
      select(Year, Key, Value) %>%
      separate(Key, into = c("Key", "Key2"), sep = mySeparator) %>% 
      mutate(Key = str_replace(Key, myReplace, "")) %>% 
      mutate(Value = as.numeric(Value))
    df
  })
}

siloAspatialSelector6 <- function(myData, myFilter){
  reactive({
    df <- filter(myData(), Attribute %in% myFilter) %>% 
      separate(Results, into = c("Key2", "Male", "Female"), sep = ",") %>% 
      gather(key = "Key", value = "Value", -Year, -Attribute, -Key2) %>%
      select(Year, Key, Key2, Value) %>% 
      mutate(Value = as.numeric(Value))
    df
  })
}

# specific to the Overview attribute
siloAspatialSelector7 <- function(myData){
  reactive({
    # to extract the total number of persons, males and females for each year
    df_pp <- filter(myData(), Attribute == "Age") %>% 
      separate(Results, into = c("Item", "Males", "Females"), sep = ",") %>%
      mutate_at(vars(Males, Females), funs(as.numeric)) %>% 
      group_by(Year) %>% 
      summarize(Males = sum(Males), Females = sum(Females)) %>% 
      ungroup() %>% 
      mutate(Persons = Males + Females)
    
    # to extract the total number of households for each year
    df_hh <- filter(myData(), Attribute == "hhByType") %>% 
      separate(Results, into = c("Key", "Value"), sep = ",") %>% 
      mutate(Value = as.numeric(Value)) %>% 
      group_by(Year) %>% 
      summarize(Households = sum(Value)) %>% 
      ungroup() 
    
    # to extract the total number of dwellings for each year
    df_dd <- filter(myData(), Attribute == "CountOfDD") %>% 
      separate(Results, into = c("Item", "Key", "Value"), sep = ",") %>% 
      mutate(Value = as.numeric(Value)) %>% 
      group_by(Year) %>% 
      summarize(Dwellings = sum(Value)) %>% 
      ungroup()
    
    # to merge the extracted information into one table
    df <- left_join(df_pp, df_hh, by = "Year") %>% 
      left_join(df_dd, by = "Year") %>% 
      gather (key = "Key", value = "Value", Males:Dwellings) %>% 
      mutate(Value = as.numeric(Value))
    
    df$Key <- factor(df$Key, levels = c("Persons", "Males", "Females", "Households", "Dwellings"))
    df
  })
}

# specific to the average rent by income attribute
siloAspatialSelector8 <- function(myData, myFilter, myColumns){
  reactive({
    df <- filter(myData(), Attribute %in% myFilter) %>%
      separate(Results, into = myColumns, sep = ",") %>%
      select(Year, Key, Value) %>%
      # mapvalues to convert incomes into characters to ensure appropriate ordering of incomes
      mutate(Value = as.numeric(Value), Key = mapvalues(Key, from = siloIncomesNum,
                                                        to = siloIncomesChar, warn_missing = FALSE))
    
    df
  })
}

# specific to the household by rent and income attribute
siloAspatialSelector9 <- function(myData, myFilter, myColumns, iReplace = FALSE){
  reactive({
    df <- filter(myData(), Attribute %in% myFilter) %>% 
      separate(Results, into = myColumns, sep = ",") %>% 
      gather(key = "Key", value = "Value", -Year, -Attribute, -Key2, -Key0) %>% 
      select(Year, Key, Key2, Value) %>% 
      # mapvalues to convert incomes into characters to ensure appropriate ordering of incomes
      mutate(Value = as.numeric(Value), Key2 = mapvalues(Key2, from = siloIncomesNum,
                                                         to = siloIncomesChar, warn_missing = FALSE))
    if(iReplace == TRUE){
      df <- df %>% 
        mutate(Key = str_replace_all(Key, "i", " "))
    }
    df
  })
}


# function for joining two silo aspatial data frames for comparison
siloJoinAspatial <- function(myData1, myData2){
  reactive({
    if(ncol(myData1()) == 3){
      df <- left_join(myData1(), myData2(), by = c("Year", "Key"), suffix = c("_base", "_comparison")) %>% 
        select(Year, Key, Value_base, Value_comparison)
    } else if (ncol(myData1()) == 4){
      df <- left_join(myData1(), myData2(), by = c("Year", "Key", "Key2"), suffix = c("_base", "_comparison")) %>% 
        select(Year, Key, Key2, Value_base, Value_comparison)
    }
    df %>% 
      mutate(Value = 100 * (Value_comparison - Value_base) / Value_base)
  })
}


# function for joining two silo spatial data frames for comparison
siloJoinSpatial <- function(myData1, myData2){
  reactive({
    myDataA = myData1() %>% select(1:2)
    myDataB = myData2() %>% select(1:2)
    myDataB = st_drop_geometry(myDataB)
    df <- left_join(myDataA, myDataB, by = "shp_muni", suffix = c("_base", "_comparison"))
    df = df %>% mutate(Change = 100 *  (.[[4]] - .[[2]]) / .[[2]])
    df
  })
}
