end_of_century_average <- function(df=NA, groups=NA){
  if(is.na(groups)==F){
    df <- df %>% 
      filter(grepl(fg, pattern=paste(groups, collapse="|")))
  }
  new_df <- df %>% 
    group_by(year, row_num, col_num, type, RCP, design, mpa, MPA) %>% 
    summarize(value = sum(value)) %>% 
    ungroup() %>% 
    filter(year > 90) %>% 
    group_by(row_num, col_num, type, RCP, design, mpa, MPA) %>% 
    summarize(value = mean(value)) %>% 
    ungroup() 
  return(new_df)
}
