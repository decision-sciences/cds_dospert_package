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

  # component functions
    # uniqR_csv
    # uniqR_xml

  # for .csv, use the identified variable as unique_id and remove the first row
  uniqR_csv <- function(df, var) {
    df <- df %>% rename_(unique_id = as.symbol(var))
    df <- df[-1, ]
  }

  # for .xml, use the identified variable as unique_id
  uniqR_xml <- function(df, var){
    df <- df %>% rename_(unique_id = as.symbol(var))
  }

  wideformat <- function(df, var, file_type){
    if(file_type == "csv"){
      df <- uniqR_csv(df, var)
    } else if (file_type == "xml") {
      df <- uniqR_xml(df, var)
    }

    return(df)
  }

  d_clean <- function(df, var, file_type) {



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

      # make sure factors are correctly converted into numeric format
      # used in [selectcol] function

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
    } else if (file_type == "xml"){
      df <- uniqR_xml(df, var)
      dat <- full.panel(df)
      dat$sum <- rowSums(cbind(dat$RT, dat$RP, dat$RB))
      dat_group <- dat %>% group_by(id) %>% dplyr::mutate(group_sum = sum(sum)) %>%
      filter(!is.na(group_sum)) %>% select(-sum, -group_sum)
      dat <- ungroup(dat_group)
      return(dat)
    }
  }

  format.result <- function(df){
    result <- attr(df, "split_labels")
    for ( i in 1: dim(result)[1] ){
      result$int[i] <- df[[i]]$coefficients[1]
      result$RB[i] <- df[[i]]$coefficients[2]
      result$RP[i] <- df[[i]]$coefficients[3]
    }

    result <- merge(result, idlist, by="id") %>%
      select(unique_id, domain, int, RB, RP)

    return(result)
  }

  if(file_type != "csv" & file_type != "xml"){
    print("file_type should be either .csv or .xml")
  } else{
    wide_temp <- wideformat(df, var, file_type)
    clean_df <- d_clean(df, var, file_type)
    reg <- dlply(clean_df, c("id", "domain"), function(data) lm(RT ~ RB + RP, data = data))
    domainlist <- distinct(select(clean_df, domain))
    idlist <- unique(select(clean_df, unique_id, id))
    reg_result <- format.result(reg)
    split <- split(reg_result, reg_result$domain)
    list_df <- list()

    # see if fin.int -> fin_int

    for (i in 1:nrow(domainlist)){
      temp <- as.data.frame(split[i])[, -2]

      names(temp) <- ifelse(stringr::str_detect(names(temp), "unique_id"),
                            "unique_id", names(temp))
      assign(paste0("coef_", domainlist[i, ]), temp)

      colnames(temp)[2] <- paste0(domainlist[i, ], "_int")
      colnames(temp)[3] <- paste0(domainlist[i, ], "_RB")
      colnames(temp)[4] <- paste0(domainlist[i, ], "_RP")

      list_df[[i]] <- temp
    }

    full_coef <- Reduce(function(x,y) merge(x, y, all = TRUE, by = "unique_id"), list_df)
    df <- merge(wide_temp, full_coef, by = "unique_id")
    return(df)
  }
}
