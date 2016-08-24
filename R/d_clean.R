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

  # make sure factors are correctly converted into numeric format
  # used in [selectcol] function


  # for .csv, use the identified variable as unique_id and remove the first row

  uniqR_csv <- function(df, var) {
    df <- df %>% rename_(unique_id = as.symbol(var))
    df <- df[-1, ]
  }

  # for .xml, use the identified variable as unique_id

  uniqR_xml <- function(df, var){
    df <- df %>% rename_(unique_id = as.symbol(var))
  }

  # take dataframe and subset only the columns of responses
  # to risk- questions and make sure that they are in numeric
  # format. Then, link unique_id and responses back as identifier.

  # df should be contain only rows of responses
  # column names of risk-responses should start with first three
  # characters of the domain followed by RT, RB, RP
  # df should have a column named 'unique_id', uniquely identifying the
  # survey taker.


  # full.panel separates benefit/taking/perception questions and
  # merge them by unique_id, domain and question number
  # and add a numeric id by unique_id

  full.panel <- function(df){

    ffac_friendly <- function(x) {
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
    panelform <- function(df, var) {
      df <- df %>% select( unique_id, contains(paste0(var, "_")))
      df <- melt(df, id="unique_id", value.name = var)
      df <- mutate(df, domain = substr(variable, 1, 3), Qnumber=substr(variable, 7, 7))
      df <- select(df, -variable)
      return(df)
    }

    df <- selectcol(df)
    benefit <- panelform(df, "RB")
    taking <- panelform(df, "RT")
    perception <- panelform(df, "RP")

    df <- Reduce(function(x,y) merge(x, y, by=c("unique_id", "domain", "Qnumber"),
                                     all=TRUE), list(taking, perception, benefit))
    df <- mutate(df, id=as.numeric(factor(unique_id)))
    return(df)
  }


  df <- dplyr:: tbl_df(df)

  if (file_type == "csv") {
    df <- uniqR_csv(df, var)
    dat <- full.panel(df)
    dat$sum <- rowSums(cbind(dat$RT, dat$RP, dat$RB))

    dat_group <- dat %>% group_by(id) %>% dplyr::mutate(group_sum = sum(sum)) %>%
      filter(!is.na(group_sum)) %>% select(-sum, -group_sum)
    dat <- ungroup(dat_group)
    return(dat)
  } else if (file_type == "xml") {
    df <- uniqR_xml(df, var)
    dat <- full.panel(df)
    dat$sum <- rowSums(cbind(dat$RT, dat$RP, dat$RB))
    dat_group <- dat %>% group_by(id) %>% dplyr::mutate(group_sum = sum(sum)) %>%
      filter(!is.na(group_sum)) %>% select(-sum, -group_sum)
    dat <- ungroup(dat_group)
    return(dat)
  }
}

