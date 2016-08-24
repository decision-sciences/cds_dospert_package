#' d_score function
#'
#' This function takes dataframe and appends the results of analyses at the left end of the original dataframe.
#' @param df Dataframe that is not formatted, downloaded from Qualtrics.
#' @param var Name of the column serves as unique identifier in dataset.
#' @param file_type csv or xml
#' @keywords dospert
#' @export
#' @examples
#' csvdata <- read_csv("data/raw_data/DOSPERT_test.csv")
#' csvScore <- d_score(csvdata, "uid", file_type = "csv")
#' xmldata <- xmlToDataFrame("data/raw_data/DOSPERT_test.xml", stringsAsFactors = F)
#' xmlScore <- d_score(xmldata, "uid", file_type = "xml")

# new_dscore --------------------------------------------------------------

d_score <- function(df, var, file_type = "csv"){


  uniqR_csv <- function(df, var) {
    df <- df %>% rename_(unique_ID = as.symbol(var))
    df <- df[-1, ]
  }

  # for .xml, use the identified variable as unique_id
  uniqR_xml <- function(df, var){
    df <- df %>% rename_(unique_ID = as.symbol(var))
  }

  wideformat <- function(df, var, file_type){
    if(file_type == "csv"){
      df <- uniqR_csv(df, var)
    } else if (file_type == "xml") {
      df <- uniqR_xml(df, var)
    }

    return(df)
  }

  d_clean <- function(df, var, file_type){

    # for .csv, use the identified variable as unique_ID and remove the first row (variable name,)
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

        wide_RB <- wide %>% filter(type == "RB") %>% select(-variable, -type) %>%
          rename(RB = value)
        wide_RP <- wide %>% filter(type == "RP") %>% select(-variable, -type) %>%
          rename(RP = value)
        wide_RT <- wide %>% filter(type == "RT") %>% select(-variable, -type) %>%
          rename(RT = value)

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


  format.result <- function(df){
    result <- attr(df, "split_labels")
    for ( i in 1: dim(result)[1] ){
      result$int[i] <- df[[i]]$coefficients[1]
      result$RB[i] <- df[[i]]$coefficients[2]
      result$RP[i] <- df[[i]]$coefficients[3]
    }

    result <- merge(result, idlist, by="unique_ID") %>%
      select(unique_ID, domain, int, RB, RP)

    return(result)
  }

  if(file_type != "csv" & file_type != "xml"){
    print("file_type should be either .csv or .xml")
  } else{
    clean_df <- d_clean(df, var, file_type)
    reg <- dlply(clean_df, c("unique_ID", "domain"), function(data) lm(RT ~ RB + RP, data = data))
    domainlist <- distinct(select(clean_df, domain))
    idlist <- unique(select(clean_df, unique_ID))
    reg_result <- format.result(reg)
    split <- split(reg_result, reg_result$domain)
    list_df <- list()

    # see if fin.int -> fin_int

    for (i in 1:nrow(domainlist)){
      temp <- as.data.frame(split[i])[, -2]

      names(temp) <- ifelse(stringr::str_detect(names(temp), "unique_ID"),
                            "unique_ID", names(temp))
      assign(paste0("coef_", domainlist[i, ]), temp)

      colnames(temp)[2] <- paste0(domainlist[i, ], "_int")
      colnames(temp)[3] <- paste0(domainlist[i, ], "_RB")
      colnames(temp)[4] <- paste0(domainlist[i, ], "_RP")

      list_df[[i]] <- temp
    }

    full_coef <- Reduce(function(x,y) merge(x, y, all = TRUE, by = "unique_ID"), list_df)
    wide_temp <- wideformat(df, var, file_type)
    df <- merge(wide_temp, full_coef, by = "unique_ID")
    return(df)
  }
}
