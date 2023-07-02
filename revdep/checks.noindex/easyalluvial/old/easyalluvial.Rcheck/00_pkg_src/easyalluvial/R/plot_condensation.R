


# satisfy CMDcheck
# https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when


if(getRversion() >= "2.15.1"){
  utils::globalVariables( c('comb', 'comb_str', 'condensation', 'condensed', 'hline', 'key', 'n_flows', 'var1', 'var2') )
}


#' @title Plot dataframe condensation potential
#' @description plotting the condensation potential is meant as a decision aid
#'   for which variables to include in an alluvial plot. All variables are
#'   transformed to categoric variables and then two variables are selected by
#'   which the dataframe will be grouped and summarized by. The pair that
#'   results in the greatest condensation of the original dataframe is selected.
#'   Then the next variable which offers the greatest condensation potential is
#'   chosen until all variables have been added. The condensation in percent is
#'   then plotted for each step along with the number of groups (flows) in the
#'   dataframe. By experience it is not advisable to have more than 1500 flows
#'   because then the alluvial plot will take a long time to render. If there is
#'   a particular variable of interest in the dataframe this variable can be
#'   chosen as a starting variable.
#' @param df dataframe
#' @param first unquoted expression or string denoting the first variable to be
#'   picked for condensation, Default: NULL
#' @return ggplot2 plot
#' @examples
#'
#'  plot_condensation(mtcars2)
#'
#'  plot_condensation(mtcars2, first = 'disp')
#'
#' @seealso \code{\link[rlang]{quosure}} \code{\link[purrr]{reexports}}
#'   \code{\link[RColorBrewer]{RColorBrewer}}
#' @rdname plot_condensation
#' @export
#' @importFrom rlang quo_is_null
#' @importFrom purrr is_null
#' @importFrom RColorBrewer brewer.pal
plot_condensation = function(df, first = NULL){

  # quos

  first = enquo( first )

  if( rlang::quo_is_null(first) ){
    first = NULL
  }else{
    first = quo_name(first)
  }

  if(! (is_tibble(df) | is.data.frame(df)) ){
    stop('df is not a dataframe')
  }

  # Prep basis

  df = manip_bin_numerics(df) %>%
    mutate_if(is.factor, as.character)

  if( purrr::is_null(first) ){

    df_comb = tibble( var1 = list( names(df) )
                      , var2 = list( names(df) ) ) %>%
      unnest( c(var1) ) %>%
      unnest( c(var2) ) %>%
      filter( var1 != var2 ) %>%
      mutate( comb = map2(var1, var2, function(x,y) sort( c(x, y) )  )
              , var1 = map_chr( comb, function(x) x[1] )
              , var2 = map_chr( comb, function(x) x[2])
              ) %>%
      group_by(var1, var2) %>%
      summarise() %>%
      ungroup() %>%
      mutate( comb = map2(var1, var2, function(x,y) c(x, y)) )

  } else{

    if( ! first %in% names(df) ){
      stop( paste( first, ' is not a column in df') )
    }


    df_comb = tibble( var1 = first, var2 = list( names(df) )) %>%
      unnest( c(var2) ) %>%
      filter(var1 != var2 ) %>%
      mutate( comb = map2(var1, var2, function(x,y) c(x, y)) )

  }

  condense = function( group_vars ){

    group_vars_sym = map( group_vars, as.name )

    df_gr = group_by(df, !!! group_vars_sym) %>%
      summarize()

    condensation = nrow(df_gr) / nrow(df)
    n_flows = nrow(df_gr)

    return( list(condensation = condensation
                 , n_flows = n_flows)  )

  }


  df_cond = df_comb %>%
    mutate( condensed = map(comb, condense)
            , condensation = map_dbl(condensed, 'condensation')
            , n_flows = map_dbl(condensed, 'n_flows') ) %>%
    arrange( condensation )

  df_results = df_cond %>%
    head(1) %>%
    select( comb, condensation, n_flows)


  # apply loop

  for ( i in seq(1, ncol(df) - 2 )  ){

    group = df_results$comb[[1]]

    remaining = names(df)[ ! names(df) %in% group ]

    df_comb = tibble( comb = list(group), vars = list(remaining) ) %>%
      unnest( c(vars) ) %>%
      mutate( comb = map2(comb, vars, function(x,y) c(x,y) )
              , condensed = map(comb, condense)
              , condensation = map_dbl(condensed, 'condensation')
              , n_flows = map_dbl(condensed, 'n_flows') ) %>%
      arrange( condensation )

    df_results = df_results %>%
      bind_rows( df_comb[1,c('comb', 'condensation', 'n_flows')] ) %>%
      arrange( desc(condensation) )

  }

  df_results = df_results %>%
    mutate(comb_str = map_chr(comb, paste, collapse = '\n') )


  p = df_results %>%
    mutate( max_flows = max(n_flows) ) %>%
    gather(key = 'key', value = 'value', condensation, n_flows ) %>%
    mutate( hline = case_when( key == 'n_flows' & max_flows >= 1500 ~ 1500
                                 , key == 'condensation' ~ 0.5
                                 )
            , key = case_when( key == 'n_flows' ~ 'number of flows'
                             , T ~ 'percent condensation')
            ) %>%
    ggplot( aes(x = comb_str, y = value, fill = key) ) +
    geom_col( width = 0.5, show.legend = F ) +
    geom_hline( aes(yintercept = hline)
                , linetype = 2
                , na.rm = T ) +
    facet_wrap(~key, ncol = 1, scales = 'free_y') +
    scale_fill_manual( values = RColorBrewer::brewer.pal(8, 'Blues')[c(4,7)] ) +
    theme_minimal() +
    labs( x = '', y = '')


  return(p)

}


