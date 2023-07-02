
#' @title plot feature importance 
#' @description plot important features of model response alluvial as bars
#' @param p alluvial plot
#' @param data_input dataframe used to generate alluvial plot
#' @param truncate_at integer, limit number of features to that value, Default: 50
#' @param color character vector, Default: 'darkgrey'
#' @return ggplot object
#' @examples 
#' \dontrun{
#'df = mtcars2[, ! names(mtcars2) %in% 'ids' ]
#'
#'train = caret::train( disp ~ .
#'                      , df
#'                      , method = 'rf'
#'                      , trControl = caret::trainControl( method = 'none' )
#'                      , importance = TRUE )
#'
#'pred_train = caret::predict.train(train, df)
#'
#'p = alluvial_model_response_caret(train, degree = 3, pred_train = pred_train)
#'
#'plot_imp(p, mtcars2)
#'
#' }
#' @rdname plot_imp
#' @export 
plot_imp = function(p, data_input, truncate_at = 50, color = 'darkgrey'){
  
  if( ! 'alluvial_type' %in% names(p)){
    stop('plot must be alluvial plot of type model_response')
  }
  
  if(p$alluvial_type != 'model_response'){
    stop('plot must be alluvial plot of type model_response')
  }
  
  if( nrow(p$alluvial_params$imp %>% tidy_imp(df = data_input) ) > truncate_at ){
    warning( paste('More than', truncate_at, 'features detected, they will be truncated. 
                   Adjust threshold using the truncate_at parameter') )
  }
  
  imp = p$alluvial_params$imp %>%
    head(truncate_at)
  
  all_vars = p$data$x %>%
    levels() %>%
    .[ ! . == 'pred']
  
  imp_df = tidy_imp(imp, data_input) %>%
    mutate( perc = imp/ sum(imp)
            , plotted = ifelse( vars %in% all_vars, 'y', 'n') )
  
  perc_total_plotted = imp_df %>%
    filter(plotted == 'y') %>%
    .$perc %>%
    sum()
  
  constant_values = p$alluvial_params$dspace %>%
    select( - one_of(all_vars) ) %>%
    mutate_all(as.character) %>%
    head(1) %>%
    gather(key = 'vars', value = 'const_values')
  
  imp_df = imp_df %>%
    bind_rows( tibble( vars = 'total\nalluvial'
                       , perc = perc_total_plotted
                       , plotted = 'y') )
  
  if( ! is_empty(constant_values) ){
    imp_df = imp_df %>%
      left_join(constant_values, by = 'vars')
  }
  
  imp_df = imp_df %>%
    mutate( vars = as_factor(vars)
            , vars = fct_relevel(vars, 'total\nalluvial')
            , vars = fct_rev(vars)
            #, const_values = ifelse(is.na(const_values), '', const_values) 
            )
  
  
  p_imp = ggplot(imp_df, aes_string('vars', 'perc', fill = 'plotted')) +
    geom_col( color = color
              , show.legend = F
              , size = 1) 
  
  if(! is_empty(constant_values) ){
    p_imp = p_imp +
      scale_fill_manual( values = c('white', color) )
  }else{
    p_imp = p_imp +
      scale_fill_manual( values = c(color, 'white') ) 
  }
  
  p_imp = p_imp +
    coord_flip() +
    theme_minimal() +
    labs( x = '', y = 'Percent Importance') +
    scale_y_continuous( position = 'right', limits = c(0,1) ) +
    geom_text( aes( label = round(perc,3) )
               , hjust = 0) 
  
  if(p$alluvial_params$method == 'median' & ! is_empty(constant_values) ){
    p_imp = p_imp +
      geom_label( aes( y = 1, label = const_values)
                , data = filter(imp_df, ! is.na(const_values) )
                , show.legend = F
                , hjust = 1
                , label.r = unit(0.07, "lines"))
  }
    
  return(p_imp)
}

#' @title add bar plot of important features to model response alluvial plot
#' @description adds bar plot of important features to model response alluvial plot
#' @param grid gtable or ggplot
#' @param p alluvial plot, optional if alluvial plot has already been passed as grid.  Default: NULL
#' @param data_input dataframe used to generate alluvial plot
#' @param plot logical if plot should be drawn or not
#' @param ... additional parameters passed to \code{\link[easyalluvial]{plot_imp}}
#' @return gtable
#' @examples 
#' \dontrun{
#'df = mtcars2[, ! names(mtcars2) %in% 'ids' ]
#'
#'train = caret::train( disp ~ .
#'                      , df
#'                      , method = 'rf'
#'                      , trControl = caret::trainControl( method = 'none' )
#'                      , importance = TRUE )
#'
#'pred_train = caret::predict.train(train, df)
#'
#'p = alluvial_model_response_caret(train, degree = 4, pred_train = pred_train)
#'
#'p_grid = add_marginal_histograms(p, data_input = df)
#'
#'p_grid = add_imp_plot(p_grid, p, data_input = df)
#' }
#' @seealso 
#'  \code{\link[gridExtra]{arrangeGrob}}
#'  \code{\link[easyalluvial]{plot_imp}}
#' @rdname add_imp_plot
#' @export 
#' @importFrom gridExtra grid.arrange
add_imp_plot = function(grid, p = NULL, data_input, plot = T, ... ){
  
  if(plot){
    .f = gridExtra::grid.arrange
  }else{
    .f = gridExtra::arrangeGrob
  }
  
  
  if( is_null(p) ){
    if( class(grid)[1] %in% c('gg','ggplot') ){
      grid = grid +
        labs( y = '', caption = '',  subtitle = '')
      
      p = grid
    }else{
      stop('an alluvial plot must be supplied via the grid or p parameter')
    }
  }
  
  p_imp = plot_imp(p, data_input, ... )
  
  .f( grid, p_imp, layout_matrix = t( as.matrix( c(1,1,1,1,1,2) ) ) )
  
}
