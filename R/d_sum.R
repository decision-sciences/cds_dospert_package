#' d_sum function
#'
#' This function takes dataframe, variable that uniquely identifies the respondent, domain, risk type (RT, RB, RP) and file type (either csv or xml) to return the sum of the responses for the domain-risk type pair for each respondent.
#' @param df Dataframe that is not formatted, downloaded from Qualtrics.
#' @param var Name of the column serves as unique identifier in dataset.
#' @param domain Domain of interest
#' @param risk_type RT(risk taking), RB(risk benefit), RP(risk perception)
#' @param file_type csv or xml
#' @keywords dospert
#' @export
#' @examples
#' csvdata <- read_csv("data/raw_data/DOSPERT_test.csv")
#' finsum_csv <- d_sum(csvdata, "uid", "fin", "RT", file_type = "csv")
#' xmldata <- xmlToDataFrame("data/raw_data/DOSPERT_test.xml", stringsAsFactors = F)
#' finsum_xml <- d_sum(xmldata, "uid", "fin", "RT", file_type = "xml")

d_sum <- function(df, var, domain, risk_type, file_type = "csv"){

  uniqR_csv <- function(df, var) {
    df <- df %>% rename_(unique_id = as.symbol(var))
    df <- df[-1, ]
  }

  # for .xml, use the identified variable as unique_id
  uniqR_xml <- function(df, var){
    df <- df %>% rename_(unique_id = as.symbol(var))
  }

  fac_friendly <- function(x) {
    x <- as.numeric(as.character(x))
  }

  if(file_type == "csv"){
    df <- uniqR_csv(df, var)
  } else if (file_type == "xml") {
    df <- uniqR_xml(df, var)
  }

  selectcol <- function(df, domain, risk_type){

    x<- df %>% select(
      starts_with(eval(paste0(domain, risk_type))))

    x <- x %>% mutate_each(funs(fac_friendly))
    ## making sure correct type of conversion
    dat <- data.frame(select(df, unique_id), x)
    return(dat)
  }

  df <- selectcol(df, domain, risk_type)
  df$sum <- rowSums(df[ , -1])
  df <- df %>% select(unique_id, sum)
  colnames(df)[2] <- paste0(domain, risk_type, "_sum")

  return(df)
}
