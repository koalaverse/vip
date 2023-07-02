
# satisfy CMDcheck
# https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when


if(getRversion() >= "2.15.1"){
  utils::globalVariables( c('x', '.', ':=', 'alluvial_id', 'fill_flow', 'fill_value', 'value'
                            , 'fill', 'easyalluvialid', 'total', 'perc', 'label', 'prefix', 'lvl'
                            , 'cum_imp', 'variable', 'label', 'len', 'ori', 'bin'
                            , 'colors', 'rwn', 'value_col', 'var_num', 'y', 'predict', 'plotted'
                            , 'const_values', 'Importance', 'Variable', 'Sign') )
}



#'@title alluvial plot of data in wide format
#'@description plots a dataframe as an alluvial plot. All numerical variables
#'  are scaled, centered and YeoJohnson transformed before binning. Plots all
#'  variables in the sequence as they appear in the dataframe until maximum
#'  number of values is reached.
#'@param data a dataframe
#'@param id unquoted column name of id column or character vector with id column
#'  name
#'@param max_variables maximum number of variables, Default: 20
#'@param bins number of bins for numerical variables, Default: 5
#'@param bin_labels labels for the bins from low to high, Default: c("LL", "ML",
#'  "M", "MH", "HH")
#'@param NA_label character vector, define label for missing data, Default: 'NA'
#'@param order_levels character vector denoting levels to be reordered from low
#'  to high
#'@param fill_by one_of(c('first_variable', 'last_variable', 'all_flows',
#'  'values')), Default: 'first_variable'
#'@param col_vector_flow HEX colors for flows, Default: palette_filter( greys =
#'  F)
#'@param col_vector_value Hex colors for y levels/values, Default:
#'  RColorBrewer::brewer.pal(9, "Greys")[c(3, 6, 4, 7, 5)]
#'@param colorful_fill_variable_stratum logical, use flow colors to colorize
#'  fill variable stratum, Default: TRUE
#'@param verbose logical, print plot summary, Default: F
#'@param stratum_labels logical, Default: TRUE
#'@param stratum_label_size numeric, Default: 4.5
#'@param stratum_width double, Default: 1/4
#'@param auto_rotate_xlabs logical, Default: TRUE
#'@param ... additional arguments passed to
#'  \code{\link[easyalluvial]{manip_bin_numerics}}
#'@return ggplot2 object
#'@details Under the hood this function converts the wide format into long
#'  format. ggalluvial also offers a way to make alluvial plots directly from
#'  wide format tables but it does not allow individual colouring of the stratum
#'  segments. The tradeoff is that we can only order levels as a whole and not
#'  individually by variable, Thus if some variables have levels with the same
#'  name the order will be the same. If we want to change level order
#'  independently we have to assign unique level names first.
#' @examples
#'\dontrun{
#' alluvial_wide( data = mtcars2, id = ids
#'                 , max_variables = 3
#'                 , fill_by = 'first_variable' )#'
#' # more coloring variants----------------------
#' alluvial_wide( data = mtcars2, id = ids
#'                 , max_variables = 5
#'                 , fill_by = 'last_variable' )
#'
#' alluvial_wide( data = mtcars2, id = ids
#'                 , max_variables = 5
#'                 , fill_by = 'all_flows' )
#'
#' alluvial_wide( data = mtcars2, id = ids
#'                 , max_variables = 5
#'                 , fill_by = 'first_variable' )
#'
#' # manually order variable values and colour by stratum value
#'
#' alluvial_wide( data = mtcars2, id = ids
#'                  , max_variables = 5
#'                  , fill_by = 'values'
#'                  , order_levels = c('4', '8', '6') )
#'}
#'@seealso \code{\link[easyalluvial]{alluvial_wide}} ,
#'  \code{\link[ggalluvial]{geom_flow}} , \code{\link[ggalluvial]{geom_stratum}}
#'  , \code{\link[easyalluvial]{manip_bin_numerics}}
#'@rdname alluvial_wide
#'@export
#'@importFrom RColorBrewer brewer.pal
#'@importFrom forcats fct_relevel
#'@importFrom ggalluvial stat_stratum geom_flow geom_stratum StatStratum
#'@importFrom magrittr %>%
alluvial_wide = function( data
                            , id = NULL
                            , max_variables = 20
                            , bins = 5
                            , bin_labels = c('LL', 'ML', 'M', 'MH', 'HH')
                            , NA_label = 'NA'
                            , order_levels = NULL
                            , fill_by = 'first_variable'
                            , col_vector_flow = palette_qualitative() %>% palette_filter( greys = F)
                            , col_vector_value =  RColorBrewer::brewer.pal(9, 'Greys')[c(4,7,5,8,6)]
                            , colorful_fill_variable_stratum = T
                            , verbose = F
                            , stratum_labels = T
                            , stratum_label_size = 4.5
                            , stratum_width = 1/4
                            , auto_rotate_xlabs = T
                            , ...
                            ){
  # quos
  
  id = enquo( id )
  
  if( rlang::quo_is_null(id) ){
    id_str = NULL
  }else{
    id_str = quo_name(id)
  }
  
  # save params to attach to plot later
  params = list(
    id = id_str
    , max_variables = max_variables
    , bins = bins
    , bin_labels = bin_labels
    , NA_label = NA_label
    , order_levels = order_levels
    , fill_by = fill_by
    , col_vector_flow = col_vector_flow
    , col_vector_value =  col_vector_value
    , verbose = verbose
    , stratum_labels = stratum_labels
    , stratum_width = stratum_width
    , auto_rotate_xlabs = auto_rotate_xlabs
  )
  
  
  # get variables
  
  variables = names(data)
  
  if( ! is_empty(id_str %in% variables) ) variables = variables[ ! variables %in% id_str ]
  
  # adjust variable length
  
  if( max_variables > length(variables) ) max_variables = length(variables)
  
  variables = unique(variables)
  
  variables = variables[1:max_variables]
  
  
  # prepare ID column
  
  if( rlang::quo_is_null(id) ){
    
    data = data %>%
      mutate( ID = row_number() )
    
    id_str = 'ID'
  }
  
  data = data %>%
    mutate( !! as.name(id_str) := as.character( !! as.name(id_str) ) )

    # organise fill argument
  
  fill_by =  switch (fill_by
                     , first_variable  = variables[1]
                     , last_variable   = variables[ length(variables) ]
                     , all_flows       = 'alluvial_id'
                     , values          = 'value'
  )
  
  # reduce data to selected variables
  
  data = data %>%
    select( one_of(variables, id_str) )
  
  # ungroup
  
  data = ungroup(data)

  # transform numerical variables for binning

  data = data %>%
    manip_bin_numerics( bins, bin_labels, NA_label = NA_label, ... )
  
  # strings as factor
  char_cols = names( select_if(data, is.character) )
  char_cols = char_cols[ char_cols %in% variables]
  
  if(! is_empty(char_cols)){
    data = data %>%
      mutate_at( .vars = vars( one_of(char_cols) ), as.factor )
  }
    
  # to ensure dbplyr 0.8.0. compatibility we 
  # transform factors to character before grouping
  # and back after grouping
  
  factor_cols = names( select_if(data, is.factor) )
  factor_cols = factor_cols[ factor_cols %in% variables]
  
  data_trans = data %>%
    select( one_of(variables) ) %>%
    mutate_at( .vars = vars( one_of(factor_cols) ), as.character ) %>%
    group_by_all() %>%
    count() %>%
    ungroup() %>%
    mutate( alluvial_id = row_number() )  %>%
    mutate_at( .vars = vars( one_of(factor_cols) ), as.factor )

  suppressMessages({
    data_alluvial = data %>%
      mutate_if( is.factor, as.character ) %>%
      left_join( mutate_if(data_trans, is.factor, as.character) ) %>%
      select( one_of( id_str, 'alluvial_id') )
  })

  # preserve order of categorical variables

  ordered_levels = data %>%
    select( one_of(variables) ) %>%
    select_if( is.factor ) %>%
    head(1) %>%
    mutate_all( function(x) list(levels(x)) ) %>%
    gather( key = 'key', value = 'levels') %>%
    unnest( c(levels) ) %>%
    .$levels %>%
    unique()

  if( ! is.null(order_levels) ){
    ordered_levels = ordered_levels[ ! ordered_levels %in% order_levels ]
    ordered_levels = c( order_levels, ordered_levels)

  }

  # Prepare dataframe

  # this code contains duplicated code sections but the different fill options require
  # different transformations. I have marked the sections where there are
  # differences with ## ***


  suppressWarnings(

    if( fill_by != 'value' ){

      data_new <- data_trans %>%
        mutate(  fill = !! as.name(fill_by) )  %>% ## ***
        gather( key = 'x', value = 'value', - alluvial_id, - n , - fill) %>% ## ***
        mutate( value = ifelse( is.na(value), NA_label, value )
                , value = as.factor(value)
                , value = forcats::fct_relevel( value, ordered_levels )
                , value = forcats::fct_rev(value)
                , x = as.factor(x)
                , x = forcats::fct_relevel(x, variables)
                , fill = as.factor(fill) ## ***
                , alluvial_id = as.factor(alluvial_id)
        )
    }else{

      data_new <- data_trans %>%
        gather( key = 'x', value = 'value', - alluvial_id, - n ) %>% ## ***
        mutate( value = ifelse( is.na(value), NA_label, value )
                , value = as.factor(value)
                , value = forcats::fct_relevel( value, ordered_levels )
                , value = forcats::fct_rev(value)
                , x = as.factor(x)
                , x = forcats::fct_relevel(x, variables)
                , fill = as.factor(value) ## ***
                , alluvial_id = as.factor(alluvial_id)
        )
    }

  )

  n_flows    = max( manip_factor_2_numeric( data_new$alluvial_id) )
  reduced_to = round( n_flows/nrow(data) * 100, 1 )
  max_weight = max( data_new$n )
  max_weight_perc = round( max_weight/nrow(data) * 100, 1 )

  line1 = paste('Number of flows:', n_flows)
  line2 = paste('Original Dataframe reduced to', reduced_to, '%' )
  line3 = paste('Maximum weight of a single flow', max_weight_perc, '%')

  if( verbose ){
    print( line1 )
    print( line2 )
    print( line3 )
  }

  caption = paste( line1, line2, line3, sep = '\n' )

  if(n_flows >= 1500){

    print( line1 )
    print( line2 )
    print( line3 )

    warning( paste( n_flows, ' flows are a lot and the plot will take a long time to render') )
  }

  #adjust col_vector length fill flows

  n_colors_needed = length( unique(data_new$fill) )

  col_vector_flow = palette_increase_length( col_vector_flow, n_colors_needed  )

  order_fill_cols = ordered_levels[ ordered_levels %in% unique(data_new$fill) ]
  
  df_fill_flow = tibble( fill = unique(data_new$fill) ) %>%
    mutate( fill = fct_relevel(fill, order_fill_cols) ) %>%
    arrange( fill ) %>%
    mutate( fill_flow = col_vector_flow ) 

  suppressMessages({
    data_new = data_new %>%
      mutate( fill = fct_relevel(fill, order_fill_cols) ) %>%
      left_join( df_fill_flow )
  })

  # adjust col_vector length fill value

  n_colors_needed = length( unique(data_new$value) )

  col_vector_value = palette_increase_length( col_vector_value, n_colors_needed  )
  
  order_value_cols = ordered_levels[ ordered_levels %in% unique(data_new$value) ]

  df_fill_value = tibble( value = unique(data_new$value) ) %>%
    mutate( value = fct_relevel(value, order_value_cols) ) %>%
    arrange( value ) %>%
    mutate( fill_value = col_vector_value ) 
                         
  suppressMessages({
    data_new = data_new %>%
      mutate( value = fct_relevel(value, order_value_cols) ) %>%
      left_join( df_fill_value )
  })

  data_new = data_new %>%
    mutate( value = fct_rev(value)
            , fill = fct_rev(fill) )
  
  if(colorful_fill_variable_stratum){
    data_new = data_new %>%
      mutate( fill_value = ifelse( x == fill_by, fill_flow, fill_value) )
  }
  
  p <- ggplot(data_new,
              aes(x = x
                  , stratum = value
                  , alluvium = alluvial_id
                  , y = n
                  , label = value)) +
    ggalluvial::geom_flow(stat = "alluvium"
                          , lode.guidance = "leftright"
                          , aes( fill = fill_flow
                                 , color = fill_flow )
                          , width = stratum_width
    ) +
    ggalluvial::geom_stratum(  aes(fill = fill_value
                                   , color = fill_value)
                               , width = stratum_width
    ) +
    theme(legend.position = "none" ) +
    scale_fill_identity() +
    scale_color_identity() +
    labs( x = '', y = 'count', caption = caption)

  if(stratum_labels){
    p = p + geom_label( stat = ggalluvial::StatStratum
                        , size = stratum_label_size )
  }

  # angle x labels------------------------------------

  max_length_x_level = levels( data_new$x ) %>%
    map_int( nchar ) %>%
    max()

  if( max_length_x_level > 5 & auto_rotate_xlabs ){
    p = p +
      theme( axis.text.x = element_text( angle = 90 ) )
  }

  # attach alluvial_id to id keys

  suppressMessages({

    data_key = data_new %>%
      mutate( alluvial_id = manip_factor_2_numeric(alluvial_id) ) %>%
      left_join( data_alluvial ) %>%
      select( - fill_flow, -fill_value, -fill ) %>%
      spread( key = x, value = value ) %>%
      select( one_of(id_str, variables, 'alluvial_id', 'n' ) ) %>%
      mutate_if( is.factor, fct_drop)
    
  })

  p$data_key = data_key
  p$alluvial_type = 'wide'
  p$alluvial_params = params
  
  return(p)
}



