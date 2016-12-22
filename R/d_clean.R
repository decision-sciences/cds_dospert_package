#' d_clean function
#'
#' This function takes dataframe and reshapes the data in panel format using unique identifying variable.
#' @param df Dataframe that is not formatted, downloaded from Qualtrics.
#' @param var Name of the column serves as unique identifier in dataset.
#' @param file_type csv or xml
#' @keywords dospert
#' @export
#' @examples
#' csvdata <- read_csv("data/raw_data/DOSPERT_test.csv")
#' csvClean <- d_clean(csvdata, "uid", file_type = "csv")
#' xmldata <- xmlToDataFrame("data/raw_data/DOSPERT_test.xml", stringsAsFactors = F)
#' xmlClean <- d_clean(xmldata, "uid", file_type = "xml")


d_clean <- function(df, var, file_type = "csv"){

  # for .csv, use the identified variable as unique_ID and remove the first row (variable name,)
  uniqR_csv <- function(df, var) {
    df <- df %>% rename_(unique_ID = as.symbol(var))
    df <- df[-1, ]
    return(df)
  }
  # for .xml, use the identified variable as unique_ID
  uniqR_xml <- function(df, var){
    df <- df %>% rename_(unique_ID = as.symbol(var))
    return(df)
  }

  full.panel <- function(df){

    fac_friendly <- function(x) {
      x <- as.numeric(as.character(x))
    }
    selectcol <- function(df){

      pat = "[a-z]{3}[A-Z]{2}_[0-9]{1}|unique_ID"
      df <- df[ , grepl( pat, colnames(df))]

      df[ , !(colnames(df) == "unique_ID")] <- df %>%
        select(-unique_ID) %>% mutate_each(funs(fac_friendly))

      ## making sure correct type of conversion
      return(df)
    }

    # panelform function is used in [full.panel] function
    # changes input df into longform and identifies the question
    # domain, number and type (e.g. fin 6 RT, 6th question in
    # finance domain, of Risk Taking)
    panelform <- function(df) {
      wide <- reshape2::melt(df, id.vars = "unique_ID")
      wide$domain <- substr(wide$variable, 1, 3)
      wide$Qnumber <- substr(wide$variable, 7, 7)
      wide$type <- substr(wide$variable, 4, 5)

      wide_RB <- wide %>% filter(type == "RB") %>% select(-variable, 
                                                          -type) %>% dplyr::rename(RB = value)
      wide_RP <- wide %>% filter(type == "RP") %>% select(-variable, 
                                                          -type) %>% dplyr::rename(RP = value)
      wide_RT <- wide %>% filter(type == "RT") %>% select(-variable, 
                                                          -type) %>% dplyr::rename(RT = value)

      l = list(wide_RB, wide_RT, wide_RP)
      df <- join_all(l, type = "full") %>% select(unique_ID, domain, Qnumber, RB, RP, RT)
      rm(wide)

      df <- df %>% arrange(unique_ID, domain, Qnumber)

      return(df)
    }

    df <- panelform(selectcol(df))
    # df <- mutate(df, id=as.numeric(factor(unique_ID)))
    return(df)
  }


  if (file_type == "csv") {
    df <- full.panel(uniqR_csv(df, var))
    return(df)

  } else if (file_type == "xml") {
    df <- full.panel(uniqR_xml(df, var))
    return(df)

  }
}

