
#'@title alluvial plot of data in long format
#'@description Plots two variables of a dataframe on an alluvial plot. A third
#'  variable can be added either to the left or the right of the alluvial plot
#'  to provide coloring of the flows. All numerical variables are scaled,
#'  centered and YeoJohnson transformed before binning.
#'@param data a dataframe
#'@param key unquoted column name or string of x axis variable
#'@param value unquoted column name or string of y axis variable
#'@param id unquoted column name or string of id column
#'@param fill unquoted column name or string of fill variable which will be used to
#'  color flows, Default: NULL
#'@param fill_right logical, TRUE fill variable is added to the right FALSE to
#'  the left, Default: T
#'@param bins number of bins for automatic binning of numerical variables,
#'  Default: 5
#'@param bin_labels labels for bins, Default: c("LL", "ML", "M", "MH", "HH")
#'@param order_levels_value character vector denoting order of y levels from low
#'  to high, does not have to be complete can also just be used to bring levels
#'  to the front, Default: NULL
#'@param order_levels_key character vector denoting order of x levels from low
#'  to high, does not have to be complete can also just be used to bring levels
#'  to the front, Default: NULL
#'@param order_levels_fill character vector denoting order of color fill
#'  variable levels from low to high, does not have to be complete can also just
#'  be used to bring levels to the front, Default: NULL
#'@param complete logical, insert implicitly missing observations, Default: TRUE
#'@param NA_label character vector define label for missing data
#'@param fill_by one_of(c('first_variable', 'last_variable', 'all_flows',
#'  'values')), Default: 'first_variable'
#'@param col_vector_flow HEX color values for flows, Default: palette_filter( greys = F)
#'@param col_vector_value HEX color values  for y levels/values,
#'  Default:RColorBrewer::brewer.pal(9, 'Greys')[c(3,6,4,7,5)]
#'@param verbose logical, print plot summary, Default: F
#'@param stratum_labels logical, Default: TRUE
#'@param stratum_label_size numeric, Default: 4.5
#'@param stratum_width double, Default: 1/4
#'@param auto_rotate_xlabs logical, Default: TRUE
#'@param ... additional parameter passed to \code{\link[easyalluvial]{manip_bin_numerics}}
#'@return ggplot2 object
#'@seealso \code{\link[easyalluvial]{alluvial_wide}}
#'  ,\code{\link[ggalluvial]{geom_flow}}, \code{\link[ggalluvial]{geom_stratum}}
#'  ,\code{\link[easyalluvial]{manip_bin_numerics}}
#' @examples
#'
#' \dontrun{
#'  data = quarterly_flights
#'
#'  alluvial_long( data, key = qu, value = mean_arr_delay, id = tailnum, fill_by = 'last_variable' )
#'
#'  # more flow coloring variants ------------------------------------
#'
#'  alluvial_long( data, key = qu, value = mean_arr_delay, id = tailnum, fill_by = 'first_variable' )
#'  alluvial_long( data, key = qu, value = mean_arr_delay, id = tailnum, fill_by = 'all_flows' )
#'  alluvial_long( data, key = qu, value = mean_arr_delay, id = tailnum, fill_by = 'value' )
#'
#'  # color by additional variable carrier ---------------------------
#'
#'  alluvial_long( data, key = qu, value = mean_arr_delay, fill = carrier, id = tailnum )
#'
#'  # use same color coding for flows and y levels -------------------
#'
#'  palette = c('green3', 'tomato')
#'
#'  alluvial_long( data, qu, mean_arr_delay, tailnum, fill_by = 'value'
#'                 , col_vector_flow = palette
#'                 , col_vector_value = palette )
#'
#'
#'  # reorder levels ------------------------------------------------
#'
#'  alluvial_long( data, qu, mean_arr_delay, tailnum, fill_by = 'first_variable'
#'                , order_levels_value = c('on_time', 'late') )
#'
#'  alluvial_long( data, qu, mean_arr_delay, tailnum, fill_by = 'first_variable'
#'                , order_levels_key = c('Q4', 'Q3', 'Q2', 'Q1') )
#'
#' require(dplyr)
#' require(magrittr)
#'
#'  order_by_carrier_size = data %>%
#'    group_by(carrier) %>%
#'    count() %>%
#'    arrange( desc(n) ) %>%
#'    .[['carrier']]
#'
#'  alluvial_long( data, qu, mean_arr_delay, tailnum, carrier
#'                 , order_levels_fill = order_by_carrier_size )
#'
#' }
#'@rdname alluvial_long
#'@export
#'@importFrom RColorBrewer brewer.pal
#'@importFrom forcats fct_relevel fct_rev
#'@importFrom rlang UQ quo_is_null
#'@importFrom tidyr unnest gather complete spread
#'@import ggalluvial
#'@import dplyr
#'@import purrr
#'@import ggplot2
alluvial_long = function( data
                          , key
                          , value
                          , id
                          , fill = NULL
                          , fill_right = T
                          , bins = 5
                          , bin_labels = c('LL', 'ML', 'M', 'MH', 'HH')
                          , NA_label = 'NA'
                          , order_levels_value = NULL
                          , order_levels_key = NULL
                          , order_levels_fill = NULL
                          , complete = TRUE
                          , fill_by = 'first_variable'
                          , col_vector_flow = palette_qualitative() %>% palette_filter( greys = F)
                          , col_vector_value =  RColorBrewer::brewer.pal(9, 'Greys')[c(3,6,4,7,5)]
                          , verbose = F
                          , stratum_labels = T
                          , stratum_label_size = 4.5
                          , stratum_width = 1/4
                          , auto_rotate_xlabs = T
                          , ...
){

  # quosures

  key = enquo( key )
  value = enquo( value )
  id = enquo( id )
  fill = enquo( fill )

  key_str = quo_name(key)
  value_str = quo_name(value)
  id_str = quo_name(id)

  # in order to make the function parameters compatible with strings we convert the strings
  # extracted from the quosures to symbols and override the quosures

  key = as.name( key_str )
  value = as.name( value_str )
  id = as.name( id_str )

  # fill
  
  if( rlang::quo_is_null(fill) ){
    fill_str = NULL
  }else{
    fill_str = quo_name(fill)
    fill = as.name( fill_str )
  }
  

  # store params to attach to plot
  
  params = list(
  key = key_str
  , value = value_str
  , id = id_str
  , fill = fill_str
  , fill_right = fill_right
  , bins = bins
  , bin_labels = bin_labels
  , NA_label = NA_label
  , order_levels_value = order_levels_value
  , order_levels_key = order_levels_key
  , order_levels_fill = order_levels_fill
  , complete = complete
  , fill_by = fill_by
  , col_vector_flow = col_vector_flow
  , col_vector_value =  col_vector_value
  , verbose = verbose
  , stratum_labels = stratum_labels
  , stratum_width = stratum_width
  , auto_rotate_xlabs = auto_rotate_xlabs
  )
  
  # ungroup
  
  data = ungroup(data)

  # transform numerical variables for binning

  data = data %>%
    ungroup() %>%
    select( one_of( c(key_str, value_str, id_str, fill_str) ) ) %>%
    mutate( !! key_str := as.factor( !! key )
            , !! id_str := as.factor( !! id )
            )
  

  data_trans = data %>%
    manip_bin_numerics( bins, bin_labels, NA_label = NA_label, ... ) %>%
    mutate( !! value_str := as.factor( !! value ) )

  if( ! is_null(fill_str) ){
    if( fill_str %in% levels(data_trans[[key_str]]) ){
      stop( paste( 'Name of fill variable/column:', fill_str, ', cannot be one of'
                   , levels(data_trans[[key_str]]) ) )
    }
  }
  
  #complete data

  if( is.null(fill_str) ){

    data_trans = data_trans %>%
      complete( !! key , !! id )

  }else{

    id_2_fill_keys = data_trans %>%
      group_by( !! id, !! fill) %>%
      summarise()

    data_trans = data_trans %>%
      complete( !! key , !! id ) %>% ## leaves NA values for fill
      select( - !! fill ) %>%     ## deselect and rejoin fill
      left_join( id_2_fill_keys, by = id_str )

  }

  # preserve order of categorical variables

  ordered_levels_x = c( order_levels_key, levels( select(data_trans, !! key)[[1]] ) ) %>% unique()
  ordered_levels_y = c( order_levels_value, levels( select(data_trans, !! value)[[1]] ) ) %>% unique()

  if( ! is.null(fill_str) ){
    ordered_levels_fill = c( order_levels_fill, levels( select(data_trans, !! fill)[[1]] ) ) %>% unique()
    ordered_levels_y = c( ordered_levels_y, ordered_levels_fill) %>% unique()

    if(fill_right){
      ordered_levels_x = c( ordered_levels_x, fill_str )
    }else{
      ordered_levels_x = c( fill_str, ordered_levels_x )
    }

  }else{
    ordered_levels_fill = NULL
  }

  # convert NA values in value to NA_label

  data_trans = data_trans %>%
    mutate(   !! value_str := as.character( !! value )
              ,  !! value_str := ifelse( is.na( !!  value ), NA_label, !! value )
              ,  !! value_str := as.factor( !! value) ) ##factor label will be restored further down


  suppressWarnings({

    # add alluvial ids
    data_spread = data_trans %>%
      spread( key = !! key, value = !! value )
    
    # to ensure dbplyr 0.8.0. compatibility we 
    # transform factors to character before grouping
    # and back after grouping. The default behaviour has
    # changed so we comment out the to character transformation
    
    factor_cols = names( select_if(data_spread, is.factor) )
    
    data_alluvial_id = data_spread %>%
      select( - !! id ) %>%
      # mutate_at( .vars = vars( one_of(factor_cols) ), as.character ) %>%
      group_by_all() %>%
      count() %>%
      ungroup() %>%
      mutate( alluvial_id = row_number() ) %>%
      mutate_at( .vars = vars( one_of(factor_cols) ), as.factor )
      

    data_new = data_alluvial_id %>%
      gather( key = 'x', value = 'value'
              , - one_of(c('alluvial_id','n', fill_str))  ) %>%
      mutate( x = as.factor(x)
              , x = forcats::fct_relevel(x, ordered_levels_x))
  })

  # attach alluvial_id to id keys
  # will be attached to plot later
  
  join_by = names(data_spread)[names(data_spread) %in% names(data_alluvial_id)]
  
  data_key = data_spread %>%
    left_join( data_alluvial_id, by =  join_by) %>%
    mutate_if( is.factor, fct_drop)

  # compose fill columns

  last_x = levels(data_new$x) %>%
    .[ length(.) ]

  first_x = levels(data_new$x)[1]

  if( ! is.null(fill_str) ){

    data_fill = data_new %>%
      filter( x == last_x ) %>%
      mutate( value = !! fill
              , x = fill_str )

    suppressWarnings({

        data_new = data_new %>%
        bind_rows( data_fill )
    })

    data_new$fill = select( data_new, !! fill)[[1]]

  }else if( fill_by %in% c( 'first_variable', 'last_variable') ) {

    data_fill = data_new

    if( fill_by == 'first_variable') data_fill = data_fill %>%
        filter( x == first_x )


    if(fill_by == 'last_variable') data_fill = data_fill %>%
        filter( x == last_x )

    data_fill = data_fill %>%
      select( alluvial_id, value ) %>%
      rename( fill = value )

    join_by = names(data_new)[names(data_new) %in% names(data_fill)]
    
    data_new = data_new %>%
      left_join( data_fill, by = join_by )

          
  } else if( fill_by == 'all_flows'){

    data_new$fill = data_new$alluvial_id %>%
      as.factor(.)

  }else if( fill_by == 'value'){

    data_new$fill = data_new$value

  }else{
    warning( 'no valid fill option selected')

    data_new$fill = 'a'

  }

  # reformat factors

  data_new = data_new %>%
    mutate_if( is.character, as.factor ) %>%
    mutate( x =  forcats::fct_relevel( x, ordered_levels_x )
            , value =  forcats::fct_relevel( value, ordered_levels_y )
            , fill =  forcats::fct_relevel( fill , ordered_levels_fill )
            ) %>%
    mutate( value =  forcats::fct_rev(value) )

  n_flows    = max( manip_factor_2_numeric( data_new$alluvial_id) )
  reduced_to = round( n_flows/ nrow(data_key) * 100, 1 )
  max_weight = max( data_new$n )
  max_weight_perc = round( max_weight/nrow(data_key) * 100, 1 )

  line1 = paste('Number of flows:', n_flows)
  line2 = paste('Original Dataframe reduced to', reduced_to, '%' )
  line3 = paste('Maximum weight of a singfle flow', max_weight_perc, '%')

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

  df_fill_flow = tibble( fill = unique(data_new$fill)
                         , fill_flow = col_vector_flow )

  data_new = data_new %>%
      left_join( df_fill_flow, by = 'fill' )

  # adjust col_vector length fill value

  n_colors_needed = length( unique(data_new$value) )

  col_vector_value = palette_increase_length( col_vector_value, n_colors_needed  )

  d_fill_value = tibble( value = unique(data_new$value)
                         , fill_value = col_vector_value )

  data_new = data_new %>%
    left_join( d_fill_value, by = 'value' )


  if( ! is.null(fill_str) ){

    data_new = data_new %>%
      mutate( fill_value = ifelse( as.character(value) == as.character(!!fill)
                                   & x == fill_str
                                   , fill_flow, fill_value ) )
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
      theme( axis.text.x = element_text( angle = 90, vjust = 0.5 , hjust = 0 ) )
  }

  p$data_key = data_key
  p$alluvial_type = 'long'
  p$alluvial_params = params

  return(p)
}



