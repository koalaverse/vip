#' @title converts factor to numeric preserving numeric levels and order in
#'   character levels.
#' @description before converting we check whether the levels contain a number,
#'   if they do the number will be preserved.
#' @param vec vector
#' @return vector
#' @examples
#' fac_num = factor( c(1,3,8) )
#' fac_chr = factor( c('foo','bar') )
#' fac_chr_ordered = factor( c('a','b','c'), ordered = TRUE )
#'
#' manip_factor_2_numeric( fac_num )
#' manip_factor_2_numeric( fac_chr )
#' manip_factor_2_numeric( fac_chr_ordered )
#' # does not work for decimal numbers
#' manip_factor_2_numeric(factor(c("A12", "B55", "10e4")))
#' manip_factor_2_numeric(factor(c("1.56", "4.56", "8.4")))
#' @seealso \code{\link[stringr]{str_detect}}
#' @rdname manip_factor_2_numeric
#' @export
#' @importFrom stringr str_detect str_replace
#' @import dplyr
#' @importFrom grDevices boxplot.stats col2rgb rgb
#' @importFrom stats var
#' @importFrom utils head
#' @importFrom stats median
manip_factor_2_numeric = function(vec){

  bool = as.character(vec) %>%
    stringr::str_detect('^\\d+$' ) %>%
    all()

  if( bool ){

    vec = vec %>%
      as.character() %>%
      as.numeric()

  } else{
    vec = as.numeric(vec)
  }

  return(vec)
}


#' @title bin numerical columns
#' @description centers, scales and Yeo Johnson transforms numeric variables in
#'   a dataframe before binning into n bins of equal range. Outliers based on
#'   boxplot stats are capped (set to min or max of boxplot stats).
#' @param x dataframe with numeric variables, or numeric vector
#' @param bins number of bins for numerical variables, passed to cut as breaks
#'   parameter, Default: 5
#' @param bin_labels labels for the bins from low to high, Default: c("LL",
#'   "ML", "M", "MH", "HH"). Can also be one of c('mean', 'median', 'min_max',
#'   'cuts'), the corresponding summary function will supply the labels.
#' @param scale logical, Default: T
#' @param center logical, Default: T
#' @param transform logical, apply Yeo Johnson Transformation, Default: T
#' @param round_numeric, logical, rounds numeric results if bin_labels is
#'   supplied with a supported summary function name.
#' @param digits, integer, number of digits to round to
#' @param NA_label character vector, define label for missing data, Default:
#'   'NA'
#' @examples
#' summary( mtcars2 )
#' summary( manip_bin_numerics(mtcars2) )
#' summary( manip_bin_numerics(mtcars2, bin_labels = 'mean'))
#' summary( manip_bin_numerics(mtcars2, bin_labels = 'cuts'
#'   , scale = FALSE, center = FALSE, transform = FALSE))
#' @return dataframe
#' @rdname manip_bin_numerics
#' @import recipes
#' @importFrom purrr is_bare_numeric walk
#' @importFrom tibble is_tibble
#' @export
manip_bin_numerics = function(x
                              , bins = 5
                              , bin_labels = c('LL', 'ML', 'M', 'MH', 'HH')
                              , center = T
                              , scale = T
                              , transform = T
                              , round_numeric = T
                              , digits = 2
                              , NA_label = 'NA'){

  # check if input is vector or dataframe, conv vec to df
  if(purrr::is_bare_numeric(x)){
    df = tibble( x = x )
    input_vector = T
  } else if( is.data.frame(x) | is_tibble(x) ){
    df = x
    input_vector = F
  } else{
    return(x)
  }

  numerics = df %>%
    select_if( is.numeric ) %>%
    select_if( function(x) var(x, na.rm = T) > 0 ) %>%  ##boxplotstats produces NA if var == 0
    names()
  
  characters = df %>%
    select_if( is.character ) %>%
    names()
  
  columns = names(df)

  if( is_empty(numerics) ){
    return( df )
  }
  
  if( length(bin_labels) != bins[1] & ! bin_labels[1] %in% c('median', 'cuts', 'mean', 'min_max') ){
    stop( "bin_labels length must be equal to bins or one of  c('median', 'cuts', 'mean', 'min_max')")
  }
  
  # we need to assign an ID to restore the correct order at the end
  df = mutate(df, easyalluvialid = row_number() ) 
  
  suppressWarnings({
    
    # recipes 0.1.6 uses old unnest()/nest() API which throws warnings
    requireNamespace('recipes')
    
    rec = recipe(df) %>%
      update_role( easyalluvialid, new_role = 'id variable') 

    if( center ) rec = rec %>%
      step_center( one_of(numerics) )
  
    if( scale ) rec = rec  %>%
      step_scale( one_of(numerics) )
  
    if( transform ) rec = rec %>%
      step_YeoJohnson( one_of(numerics) )
  
      rec = rec %>%
        prep()
  
    rename_levels = function(x){
      levels(x) = bin_labels
      return(x)
    }
  
    data_new <- bake(rec, df ) %>%
      mutate_at( vars(numerics), function(x) ifelse( x > max(boxplot.stats(x)$stats)
                                                     , max(boxplot.stats(x)$stats)
                                                     , x)
                 ) %>%
      mutate_at( vars(numerics), function(x) ifelse( x < min(boxplot.stats(x)$stats)
                                                     , min(boxplot.stats(x)$stats)
                                                     , x)
                 ) %>%
      mutate_at( vars(numerics), function(x) cut(x, breaks = bins) ) %>%
      #bake() is converting character variables to factor which we need to revert
      mutate_at( vars(characters), as.character )
  })
  
  purrr::walk(numerics, check_empty_lvl, data_new)
  
  summary_as_label = function(df, df_old, fun){
    # joins df with original dataframe. Groups by segments and calculates
    # summary stat using the given function. Summary stat replaces segment
    # labels
    
    df = df %>%
      mutate_if(is.factor, fct_explicit_na, na_level = NA_label ) %>%
      left_join( select(df_old, one_of( c(numerics, 'easyalluvialid') ) ), by = 'easyalluvialid')
    
    for(num in numerics){
      df = df %>%
        group_by( !! as.name( paste0(num, '.x') ) ) %>%
        mutate( !! as.name( paste0(num, '.y') ) := fun( !! as.name( paste0(num, '.y') ) ) )
    }
    
    df = df  %>%
      ungroup() %>%
      select( - ends_with('.x') )
    
    if(round_numeric){
      
      df = df %>%
        mutate_if( is.numeric, round, digits = digits )
    }
    
    df = df %>%
      mutate_if(is.numeric, as.factor ) %>%
      mutate_if(is.factor, fct_explicit_na, na_level = NA_label ) %>%
      rename_at( vars( ends_with('.y') ) , .funs = function(x) str_replace(x, '\\.y$', '') )
    
    return(df)
    
  }
  
  if( length(bin_labels) == bins[1] ){
    
    data_new = data_new %>%
      mutate_at( vars(numerics),  rename_levels) 
    
  }else if( bin_labels == 'median'){
    data_new = data_new  %>%
      summary_as_label(df_old = df, fun = median) 
  }else if( bin_labels == 'mean'){
    data_new = summary_as_label(data_new, df_old = df, fun = mean)
  }else if( bin_labels == 'min_max'){
    df_min = summary_as_label(data_new, df_old = df, fun = min)
    df_max = summary_as_label(data_new, df_old = df, fun = max)
    
    join_by = names(df_min)[! names(df_min) %in% numerics ]
    join_by = c(join_by, 'easyalluvialid')
    join_by = unique(join_by)
    
    data_new = df_min %>%
      left_join(df_max, by = join_by )
    
    for(num in numerics){
      
      sym_min = as.name( paste0(num, '.x') )
      sym_max = as.name( paste0(num, '.y') )
      
      data_new = data_new %>%
        arrange( !! sym_min ) %>%
        mutate( !! as.name( num ) := map2_chr( !! sym_max, !! sym_min, function(x,y) paste(x,'-\n',y) ) ) %>%
        mutate( !! as.name( num ) := as_factor(!! as.name( num ) ) )
      
      NA_min_max = paste(NA_label,'-\n',NA_label)
      
      if( NA_min_max %in% levels(data_new[[num]]) ){
        data_new = data_new %>%
          mutate( !! as.name( num ) := fct_recode(!! as.name( num )
                                                  , !! as.name(NA_label) := NA_min_max  ))
      }
    }
    
    data_new = data_new %>%
      select( -ends_with('.x'), -ends_with('.y') ) %>%
      arrange( easyalluvialid )
    
  }
  
  #remove easyalluvialid
  data_new = select(data_new, columns) %>%
    mutate_if( is.factor, fct_explicit_na, na_level = NA_label )
  
  if( input_vector ){
    return( data_new$x )
  }else{
    return(data_new)
  }

}

#' @title get most frequent level of factor
#' @param x factor
#' @return factor
#' @examples 
#' x <- LETTERS[c(7,6,7,7,3,4,20)]
#' x <- as.factor(x)
#' get_most_frequent_lvl(x)
#' @rdname get_most_frequent_lvl
#' @noRd
get_most_frequent_lvl <- function(x) {
  stopifnot(is.factor(x))
  
  x <- unname(x)
  
  lvl <- table(x) %>%
    sort(decreasing = TRUE) %>%
    .[1] %>%
    names()
  
  return(x[which(x == lvl)][1])
}

#' @title check for empty lvl
#' @param col character, column name
#' @return df dataframe
#' @examples 
#' df <- data.frame(x = cut(c(1,1,2,2,9,9,10), 5))
#' check_empty_lvl("x", df)
#' @rdname check_empty_lvl
#' @noRd
check_empty_lvl <- function(col, df){
  df_check <- df %>%
    group_by(!! as.name(col), .drop = FALSE) %>%
    count() %>%
    filter(n == 0)
  
  if(nrow(df_check) > 0){
    warning(paste("bins ", paste(unique(df_check[[col]]), collapse = ","), "of", col, "are empty" ))
  }
}

