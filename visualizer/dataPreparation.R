### data cleaner functions ###

# function for cleaning silo aspatial data
siloAspatialCleaner <- function(data){
  reactive({
    if(length(data()) > 1){
      df <- data()[["aspatial"]]
    } else if(length(data) == 1){
      df <- data()[["data"]]
    }
    
    # make 2nd column (X2) numeric to change all non-numeric characters to 'NA' and assign it to Year
    # recode strings with unrecognizable characters such as as +, <, >
    df <- mutate(df, Year = as.numeric(X2),
                 Results = str_replace_all(X1, c("3\\+cars" = "3cars", "\\<18" = "0-17", "\\>=65" = "65-Inf"))) %>%
      select(-X2) %>%
      
      # fill all empty rows with the non-NA value preceding it in the same column to assign Year to all rows.
      na.locf() %>%
      
      # remove caption rows of results that have both caption rows and column header rows)
      filter(!X1 %in% c("Year", "Count", "Housing")) %>%
      
      # separate first column (X1) into 2 columns (Item and value)and use the Item column as an attribute identifier
      separate(X1, into = c("Item", "Value"), sep = ",")
    
    # create a new table which contains rows whose identifier is contained in the set of aspatial attributes to be considered
    df2 <- filter(df, Item %in% siloAspatialAll) %>%
      select(Year, Item, Value) %>%
      # assign the identifer to a new column (Attribute)
      mutate(Attribute = Item)
    
    # join the newly created table to the original table
    df <- left_join(df, df2, by = c("Year", "Item", "Value")) %>%
      
      # fill all empty rows with the non-NA value preceding it in the same column (column of interest is the Attribute column)
      # this will add a common identifier to all rows belonging to the same atttribute. Thus, all rows that have Age records
      # will have 'Age' as an identifier in the Attribute column
      na.locf() %>%
      
      # remove all header rows. Thus all remaining rows will contain values with corresponding identifier in the Attribute column
      filter(!Item %in% siloAspatialWithHeaders) %>%
      select(Year, Attribute, Results) %>%
      
      # remove rows with BirthdayEvent. Relevant? Values too high to be displayed together with other person events
      filter(Attribute != "BirthDayEvent")
    df
 })
}


# function for cleaning silo spatial data
siloSpatialCleaner <- function(data){
  reactive({
    if(length(data()) > 1){
      df <- data()[["spatial"]]
    } else if(length(data) == 1){
      df <- data()[["data"]]
    }
    
    # create column headers
    dfName <- unlist(strsplit(as.character(df[2, 1]), split = ","))
    
    # fill all empty rows with the non-NA value preceding it in the same column (assign Year attribute to all rows)
    
    
    
    
    df <- na.locf(df) %>% 
      mutate(Year = as.numeric(X2)) %>% 
      filter(!str_detect(X1, "Year")) %>% 
      # separate first column (X1) which is comma-separated into multiple columns
      separate(X1, into = dfName, sep = ",")
      #mutate(Zone := Year2000) %>% 
      #select(- Year2000) %>% 
      #mutate_all(funs(as.numeric))
    
    #dpplyr does not accept dynamic variable names (according to my limited knowledge), so I use normal R code
    #to remove the column named "Year_BASE_YEAR"
    aux_var_name = paste("Year", base_year, sep ="")
    df[["Zone"]] = df[[aux_var_name]] 
    df = df[,!(names(df) %in% c(aux_var_name))]
    df = df %>% mutate_all(funs(as.numeric))
      
    df
  })
}


# function for cleaning mito aspatial data
mitoAspatialCleaner <- function(data){
  reactive({
    if(length(data()) > 1){
      df <- data()[["aspatial"]]
    } else if(length(data) == 1){
      df <- data()[["data"]]
    }
    
    # separate Attribute column into two columns using the underscore as a separator
    df <- separate(df, Attribute, into = c("Feature", "Alternatives"), sep = "_") %>% 
      mutate_at(vars(HBW:NHBO), funs(as.numeric)) %>% 
      mutate_at(vars(HBW:NHBO), funs(replace_na), replace = 0) %>% 
      group_by(Feature) %>%
      mutate_at(vars(HBW:NHBO), funs(s = 100 * . / sum(.))) %>% 
      ungroup() %>% 
      mutate_at(vars(HBW_s:NHBO_s), funs(replace_na), replace = 0) %>% 
      mutate_at(vars(HBW_s:NHBO_s), funs(round), digits = 2)
    
    df
  })
}


# function for cleaning mito spatial data
mitoSpatialCleaner <- function(data, zoneLevel){
  reactive({
    if(length(data()) > 1){
      df <- data()[["spatial"]]
    } else if(length(data) == 1){
      df <- data()[["data"]]
    }
    zonesData <- zones %>% 
      select(shp_id, shp_muni, shp_area)
    df <- df %>%
      inner_join(zonesData, by = c("Zone" = "shp_id")) 
    
    # for visualizing at the gemeinde level
    if (zoneLevel == FALSE){
      df <- df %>%
        group_by(shp_muni) %>%
        st_sf()
      
      # create table for attributes to be summarized by sum
      df1 <- df %>%
        summarize_at(c(paste0(mitoPurposes, "P"), paste0(mitoPurposes, "A"), "shp_area"),
                     funs(sum), na.rm = TRUE) %>%
        ungroup()
      
      # create table for attributes to be summarized by mean
      df2 <- df %>%
        summarize_at(c(paste0(mitoPurposes, "AvDist"), paste0(mitoPurposes, "AvTime"),
                       paste0(mitoPurposes, "TTB")), funs(mean), na.rm = TRUE) %>%
        ungroup() %>%
        st_set_geometry(NULL)
      
      # merge the two tables
      df <- inner_join(df1, df2, by = "shp_muni")
    }
    df <- df %>% 
      mutate_at(paste0(mitoPurposes, "P"), funs(1000000 * . / shp_area)) %>% 
      mutate_at(paste0(mitoPurposes, "A"), funs(1000000 * . / shp_area)) %>% 
      mutate_at(paste0(mitoPurposes, "P"), funs(round)) %>% 
      mutate_at(paste0(mitoPurposes, "A"), funs(round)) %>% 
      st_sf()
    df
  })
}