#'
#' This file contains functions to identify cohorts in a study.  Its
#' contents, as well as the contents of any R files whose name begins
#' with "cohort_", will be automatically sourced when the request is
#' executed.
#'
#' For simpler requests, it will often be possible to do all cohort
#' manipulation in functions within this file.  For more complex
#' requests, it may be more readable to separate different cohorts or
#' operations on cohorts into logical groups in different files.
#'


## Get results_tbl

get_results <- function(tbl_name) {
  
  rslt <- results_tbl(tbl_name) %>% collect()
  
}


#' output a list of tables to the database 
#' 
#' @param output_list list of tables to output
#' @param append logical to determine if you want to append if the table exists
#' 
#' @return tables output to the database; if 
#' table already exists, it will be appended
#' 

output_list_to_db <- function(output_list,
                              append=FALSE) {
  
  
  if(append) {
    
    for(i in 1:length(output_list)) {
      
      output_tbl_append(data=output_list[[i]],
                        name=names(output_list[i]))
      
    }
    
  } else {
    
    for(i in 1:length(output_list)) {
      
      output_tbl(data=output_list[[i]],
                 name=names(output_list[i]))
      
    }
    
  }
  
}

#' output table to database if it does not exist, or
#' append it to an existing table with the same name if it does
#' 
#' @param data the data to output
#' @param name the name of the table to output 
#' 
#' Parameters are the same as `output_tbl`
#' 
#' @return The table as it exists on the databse, with the new data
#' appended, if the table already existts.
#' 

output_tbl_append <- function(data, name = NA, local = FALSE,
                              file = ifelse(config('execution_mode') !=
                                              'development', TRUE, FALSE),
                              db = ifelse(config('execution_mode') !=
                                            'distribution', TRUE, FALSE),
                              results_tag = TRUE, ...) {
  
  if (is.na(name)) name <- quo_name(enquo(data))
  
  if(db_exists_table(config('db_src'),intermed_name(name,temporary=FALSE))) {
    
    tmp <- results_tbl(name) %>% collect_new 
    new_tbl <- 
      dplyr::union(tmp,
                   data %>% collect())
    output_tbl(data=new_tbl,
               name=name,
               local=local,
               file=file,
               db=db,
               results_tag = TRUE, ...)
  } else {
    output_tbl(data=data,
               name=name,
               local=local,
               file=file,
               db=db,
               results_tag = TRUE, ...)
  }
  
  
}
