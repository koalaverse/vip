
apply_cuts = function(x, cuts){
  
  if( min(x) < cuts[1] ){
    cuts[1] <-  min(x) - 1
  }
  
  if( max(x) > cuts[length(cuts)] ){
    cuts[length(cuts)] <-  max(x) + 1
  }
  
  return( cut(x, cuts) )
  
}

#' @title plot histogram of alluvial plot variable
#' @description helper function used by add_marginal_histograms
#' @param var character vector, variable name
#' @param p alluvial plot
#' @param data_input dataframe used to create alluvial plot
#' @param ... additional arguments for specific alluvial plot types: pred_train
#'   can be used to pass training predictions for model response alluvials
#' @return ggplot object
#' @rdname plot_hist
#' @export
plot_hist = function( var, p, data_input, ... ){
  
  data_input = data_input %>%
    mutate_if(is.character, as.factor)
  
  p_ori = p
  
  if( p$alluvial_type == 'wide'){
    
    p = plot_hist_wide( var, p_ori, data_input)
    
  }else if( p$alluvial_type == 'model_response'){
    
    p = plot_hist_model_response( var, p_ori, data_input, ...)
    
  } else if(p$alluvial_type == 'long'){
    
    p = plot_hist_long( var, p_ori, data_input)
    
  }else{
    stop('Plot not supported')
  }
  
  p = p +
    scale_fill_identity() +
    scale_color_identity() 
  
  if( p_ori$alluvial_type == 'model_response' & var == 'pred'){
    p = p +
      theme( line = element_blank()
             , axis.title = element_blank()
             , panel.background = element_blank() ) 
  }else {
    p = p +
      theme( line = element_blank()
             , axis.text.y = element_blank()
             , axis.title = element_blank()
             , panel.background = element_blank() ) 
  }
  
  return(p)
}

plot_hist_long = function(var, p, data_input){
  
  is_num = is.numeric(data_input[[p$alluvial_params$value]])
  key_str = as.character(p$alluvial_params$key)
  id_str = as.character(p$alluvial_params$id)
  value_str = as.character(p$alluvial_params$value)
  
  if( is_null(p$alluvial_params$fill) ){
    var_is_fill = F
    var_has_fill = F
    fill_str = NULL
  }else{
    var_has_fill = T
    fill_str = p$alluvial_params$fill
    if( fill_str == var ){
      var_is_fill = T
    }else{
      var_is_fill = F
    }
  }
  
  var_order = levels(p$data$value)
  
  data_input[[id_str]] <- as.character( data_input[[id_str]] )
  data_input[[key_str]] <- as.character( data_input[[key_str]] )
  
  df_col = p$data
  
  if(var_is_fill){
    df_col = df_col %>%
      filter( x == fill_str)
  }else{
    df_col = df_col %>%
      filter( x %in% unique(data_input[[key_str]]) )
  }
  
  df_col = df_col %>%
    select(value, fill_value) %>%
    distinct() %>%
    arrange(value) %>%
    mutate(rwn = row_number() ) %>%
    mutate_if(is.factor, as.character)
  
  if( ! var_is_fill ){
  df_filt = data_input %>%
    filter( !! as.name(key_str) == var )
  }else{
    df_filt = data_input
  }

  if(is_num){
    
    cols_to_gather = unique(data_input[[key_str]])
    
    df_match = p$data_key %>%
      select( -n, -alluvial_id) %>%
      gather( key = !! as.name(key_str), value = 'bin'
              , !!! map(cols_to_gather, as.name) ) %>%
      mutate( !! as.name(id_str) := as.character(!! as.name(id_str) )
              , !! as.name(key_str) := as.character(!! as.name(key_str) )) %>%
      left_join( select(data_input, one_of( c(key_str, value_str, id_str) ) )
                 , by = c(id_str, key_str))  %>%
      rename( value = !! as.name(value_str) ) %>%
      mutate( bin = as.factor(bin)
              , bin = fct_relevel(bin, var_order) )

    m = randomForest::randomForest(bin ~ value, df_match)
    
    dens_var = density(df_filt[[value_str]])
    
    df_plot = tibble(x = dens_var$x
                     , y = dens_var$y ) %>%
      mutate( bin = predict(m, tibble( value = x) )
              , rwn = as.integer(bin) ) %>%
      left_join(df_col, by=c('rwn' = 'rwn') )
    
    p = ggplot(df_plot ) +
      geom_ribbon( aes(x, fill = fill_value, color = fill_value, ymin = 0, ymax = y) ) +
      geom_rug( data = df_filt, mapping = aes_string(value_str), sides = 'b')
  
  }else if(var_is_fill){
    df_plot = df_filt %>%
      select( one_of( id_str, var) ) %>%
      distinct() %>%
      rename(  value_col =  !! as.name(var) ) %>%
      mutate( value_col = as.character(value_col) ) %>%
      left_join( df_col, by = c( 'value_col' = 'value') ) 
    
    p = ggplot(df_plot, aes(value_col, fill = fill_value) ) +
      geom_bar( width = 1/2 ) 
    
  }else{
    df_plot = df_filt %>%
      rename(  value_col =  !! as.name(value_str) ) %>%
      mutate( value_col = as.character(value_col) ) %>%
      left_join( df_col, by = c( 'value_col' = 'value') ) 
    
    p = ggplot(df_plot, aes(value_col, fill = fill_value) ) +
      geom_bar( width = 1/2 ) 
  }
  
  
}

plot_hist_model_response = function(var, p, data_input, pred_train = NULL, scale = 400, resp_var = NULL){

  var_str = var
  var_quo = as.name(var_str)
  
  is_pred = var == 'pred'
  
  if(is_pred){
    
    # set ori_name -----------------------------------------------
    ori_name = NULL
    if( ! is_null(resp_var) ){
      if( resp_var %in% names(data_input) ){
        ori_name = resp_var
      }
    }
    
    if( is_null(ori_name)){
      ori_name = names(data_input)[ ! names(data_input) %in% names(p$alluvial_params$dspace) ] %>% unlist()
    }
    
    
    if( length(ori_name) > 1){
      stop( paste('\n"data_input" should only contain explanatory and response variables, so response variable can be inferred.
                  \nPotential response variables:', paste( ori_name, collapse = ', ')
                  , '\nPlease pass correct response variable as string using the "resp_var" parameter.' ) )
    }
    
    is_num = is.numeric( data_input[[ori_name]] )
  }else{
    is_num = is.numeric( data_input[[var_str]] )
  }
  
  df_col = p$data %>%
    filter( x == var_str ) %>%
    arrange( desc(value) ) %>%
    select(fill_value, value) %>%
    distinct() %>%
    select( fill_value ) %>%
    mutate( fill_value = as_factor(fill_value)
            , rwn = row_number() )
  
  if( is_num & ! is_pred){
    
    df_median = p$alluvial_params$dspace %>%
      select( !! var_quo ) %>%
      distinct() %>%
      arrange(!! var_quo) %>%
      bind_cols(df_col)
    
    p = ggplot(data_input, aes_string( x = var_str ) ) +
      geom_density( aes_string( y = '..density..'), size = 1 ) +
      geom_rug()
    
    for( i in seq(1, nrow(df_median) ) ){
      p = p +
        geom_vline(xintercept = df_median[[var_str]][i]
                   , color = df_median[['fill_value']][i]
                   , size = 2)
    }
  }
  
  if( ! is_num & ! is_pred ){
    
    df_plot = data_input %>%
      select( !! var_quo ) %>%
      mutate( colors = !! var_quo
              , colors = as.integer(colors) ) %>%
      left_join( df_col, by = c( 'colors' = 'rwn') )
    
    p = ggplot(df_plot, aes_string(var_str, fill = 'fill_value') ) +
      geom_bar( width = 1/2 ) 
    
  }
  
  if( ! is_num & is_pred){
    var_str = ori_name
    
    df_input = data_input %>%
      select( !! as.name(var_str) ) %>%
      mutate( variable = var_str ) %>%
      rename( value = !! as.name(var_str) ) 
    
    df_resp = p$data %>%
      filter( x == 'pred') %>%
      select( x, value ) %>%
      rename( variable = x) %>%
      mutate( variable = as.character(variable)
              , value = fct_drop(value)
              # p$data does not preserve factor order
              , value = fct_relevel(value, levels(df_input$value) ) )
    
    stopifnot(all(levels(df_input$value) == levels(df_resp$value)))

    if(is.ordered(df_input$value)){
      df_resp$value = as.ordered(df_resp$value)
    }
    
    df_plot = df_input %>%
      bind_rows(df_resp)

    if( ! is_null(pred_train) ){
      df_plot = df_plot %>%
        bind_rows( tibble(variable = 'pred_train'
                        , value = pred_train) ) %>%
        mutate( variable = as.factor(variable)
                , variable = fct_relevel(variable, c(var_str, 'pred_train', 'pred') ) )
      
      strip_pos = 'top'
    }else{
      df_plot = df_plot %>%
        mutate( variable = as.factor(variable)
                , variable = fct_relevel(variable, c(var_str), 'pred' ) )
      strip_pos = 'top'
    }
    
    df_plot = df_plot %>%
      mutate( colors = value
              , colors = as.integer(colors) ) %>%
      left_join( df_col, by = c( 'colors' = 'rwn') ) %>%
      group_by( variable, value, fill_value ) %>%
      count() %>%
      group_by( variable ) %>%
      mutate( perc = n / sum(n) )
    
    p = ggplot(df_plot ) +
      geom_col( aes( x = value, y = perc, fill = fill_value), width = 1/2 ) +
      facet_wrap(~variable, nrow = 1, strip.position = strip_pos) +
      theme( axis.text.y = element_blank() )
  }
  
  if( is_num & is_pred ){
    
    var_str = ori_name
    
    df_ori = data_input %>%
      select( !! as.name(var_str) ) %>%
      mutate( variable = var_str ) %>%
      rename( value = !! as.name(var_str) )
    
    
    df_pred = tibble( variable = 'pred'
                      , value = p$alluvial_params$pred )
    

    dens_pred = density( df_pred$value )
    dens_ori = density( df_ori$value)
    

    # pred_train -------------------------------------
    
    pred_train_from_plot = ! is_null(p$alluvial_params$pred_train)
    pred_train_as_arg = ! is_null(pred_train)
    
    if(pred_train_from_plot){
      pred_train = p$alluvial_params$pred_train
    }
    
    if(pred_train_from_plot | pred_train_as_arg){
      df_pred_train = tibble( variable = 'pred_train'
                              , x = density(pred_train)$x
                              , y = density(pred_train)$y
                              , colors = 'lightgrey')
    }
    
    # assemple df_plot ----------------------------
    df_plot = tibble( variable = 'pred'
                      , x = dens_pred$x
                      , y = dens_pred$y )
    
    if( pred_train_from_plot){
      
      df_plot = df_plot %>%
        bind_rows( select(df_pred_train, - colors) )
      
    }
    
    # apply cuts and select colors -----------------
    df_plot = df_plot%>%
      mutate( rwn = apply_cuts(x, p$alluvial_params$new_cuts) 
              , rwn = as.integer(rwn) )
    
    min_rwn_pred = p$alluvial_params$pred %>%
      apply_cuts( p$alluvial_params$new_cuts) %>%
      as.integer() %>%
      min()
    
    max_color = max(df_plot$rwn)
    
    df_col = df_col %>%
      mutate( rwn = rwn + min_rwn_pred -1 ) 
    
    unused_colours = p$alluvial_params$col_vector_flow[ ! p$alluvial_params$col_vector_flow %in% df_col$fill_value]
    unused_rwn = seq(1,max_color)
    unused_rwn = unused_rwn[! unused_rwn %in% df_col$rwn]
    
    n_missing_colors = length(unused_rwn) - length(unused_colours)
    n_missing_colors = ifelse(n_missing_colors < 0, 0, n_missing_colors)
    
    unused_colours = c( unused_colours, rep('grey', n_missing_colors) )
    
    df_col = df_col %>%
      mutate( fill_value = as.character(fill_value) ) %>%
      bind_rows( tibble(rwn = unused_rwn, fill_value = unused_colours[0:length( unused_rwn) ]) )
      
    df_plot = df_plot %>%
      left_join( df_col, by = c( 'rwn' = 'rwn') ) %>%
      rename( colors = fill_value ) %>%
      mutate( colors = as.character(colors) ) %>%
      bind_rows( tibble(variable = var_str
                        , x = dens_ori$x
                        , y = dens_ori$y
                        , colors = 'lightgrey') )
    
      
    if( pred_train_as_arg & (! pred_train_from_plot) ){
      df_plot = df_plot %>%
        bind_rows( df_pred_train )
    }
    
    # adjust order of variables on y-axis ----------------------------

    if( is_null(pred_train) ){
      df_plot = mutate(df_plot, variable = fct_relevel(variable, 'pred') )
    }else{
      df_plot = mutate(df_plot, variable = fct_relevel(variable, 'pred', 'pred_train') )
    }

  # plot ---------------------------------------------------------------
    
    p = ggplot(df_plot) +
      ggridges::geom_ridgeline_gradient(aes(x = x
                                            , y = variable
                                            , height = y
                                            , fill = colors
                                            , group = variable )
                              , scale = scale) +
      geom_rug( aes(x = value), data = df_pred ) +
      geom_rug( aes(x = value), data = df_ori, sides = 't' )
    
  }
  
  return(p)
}

plot_hist_wide = function( var, p, data_input){
  # use ID to join data_key with data_input to
  # have sratum name combined with original numeric value
  # get stratum color from p$data
  # use density() to get x,y density coordinate
  # train model on stratum_name (key) and original numeric value
  # apply model to x density coordinates to get segment borders
  
  var_str = var
  var_quo = as.name(var_str)
  
  is_num = is.numeric( data_input[[var_str]] )
  
  data_input = data_input %>%
    rename( var_num = !! var_quo )

  p$data_key = p$data_key %>%
    rename( var_key = !! var_quo )
  
  if( is_null(p$alluvial_params$id) ){
    p$alluvial_params$id = 'ID'
    
    data_input = data_input %>%
      mutate( ID = as.character(row_number() ) )
  }
  
  # in order not to produce a warning we change joining
  # variable to character 
  df_left = data_input %>%
    mutate( !! as.name(p$alluvial_params$id) := as.character(!! as.name(p$alluvial_params$id)) )
  
  df_right = p$data_key %>%
    mutate( !! as.name(p$alluvial_params$id) := as.character(!! as.name(p$alluvial_params$id)) )
  
  df = left_join(df_left, df_right, by = p$alluvial_params$id) %>%
    select( starts_with('var') ) 
  
  df_col = p$data %>%
    filter( x == var_str) %>%
    select( value, fill_value) %>%
    distinct() %>%
  mutate_if( is.factor, as.character )
  
  df_rug = data_input %>% 
    select(var_num) %>% 
    rename( x = var_num) %>%
    mutate( y = 0)
  
  
  if( is_num ){
  
    d = density(df$var_num)
    m = randomForest::randomForest(var_key ~ var_num, df)
    pred = predict(m, newdata = tibble(var_num = d$x), type = 'class' )
    
    df_plot = tibble( x = d$x
                      , y = d$y
                      , fill = as.character(pred) ) %>%
      left_join( df_col, by = c('fill' = 'value') )
    

    p = ggplot(df_plot ) +
      geom_ribbon( aes(x, fill = fill_value, color = fill_value, ymin = 0, ymax = y) ) +
      geom_rug( data = df_rug, mapping = aes(x), sides = 'b')
    

  } else{
    df = df %>%
      mutate_if( is.factor, as.character ) %>%
      left_join( df_col, by = c('var_key' = 'value') ) 
    
    p = ggplot(df, aes(var_num, fill = fill_value) ) +
      geom_bar( width = 1/2 ) 
  }
    
  return(p)
}

#'@title add marginal histograms to alluvial plot
#'@description will add density histograms and frequency plots of original data
#'  to alluvial plot
#'@param p alluvial plot
#'@param data_input dataframe, input data that was used to create dataframe
#'@param top logical, position of histograms, if FALSE adds them at the bottom,
#'  Default: TRUE
#'@param keep_labels logical, keep title and caption, Default: FALSE
#'@param plot logical if plot should be drawn or not
#'@param ... additional arguments for model response alluvial plot concerning
#'  the response variable \describe{ \item{pred_train}{display training
#'  prediction, not necessary if pred_train has already been passed to
#'  alluvial_model_response()} \item{scale}{int, y-axis distance between the
#'  ridge plots, Default: 400 } \item{resp_var}{character vector, specify
#'  response variable in data_input, if not set response variable will try to be
#'  inferred, Default: NULL } }
#'@return gtable
#' @examples
#'\dontrun{
#' p = alluvial_wide(mtcars2, max_variables = 3)
#' p_grid = add_marginal_histograms(p, mtcars2)
#'}
#'@seealso \code{\link[gridExtra]{arrangeGrob}}
#'@rdname add_marginal_histograms
#'@export
#'@importFrom gridExtra grid.arrange arrangeGrob
#'@importFrom ggridges geom_ridgeline_gradient
#'@importFrom stats density
add_marginal_histograms = function(p, data_input, top = TRUE, keep_labels = FALSE, plot = TRUE, ...){
  
  if(plot){
    .f = gridExtra::grid.arrange
  }else{
    .f = gridExtra::arrangeGrob
  }
  
  vars = levels( p$data$x )

  hists = map( vars, plot_hist, p = p, data_input = data_input, ...)
  
  p_margin = do.call( gridExtra::arrangeGrob, c( hists, nrow = 1) )
  
  if(keep_labels){
    params = list( top = paste( p$labels$title, '-', p$labels$subtitle ), bottom = p$labels$caption  )
  }else{
    params = list( top = NULL, bottom = NULL)
  }
  
  p = p +
    labs( y = '', caption = '', title = '', subtitle = '') +
    coord_cartesian( xlim = c(1.25, length(vars) - 0.25 ) ) +
    theme( axis.text.y = element_blank(), axis.ticks.y = element_blank()
           )
    
  if(! top){
    layout = as.matrix( data.frame( x = c(1,1,1,1,1,1,1,2) ) )
    
    p_full = do.call( .f, c( list(p, p_margin), ncol = 1, params
                      , layout_matrix = list(layout) ) )
    
  }else{

    layout = as.matrix( data.frame( x = c(1,2,2,2,2,2,2,2) ) )
    
    p_full = do.call( .f, c( list(p_margin, p) , ncol = 1, params
                                                  , layout_matrix = list(layout) ) )
    
  }
  
}

#' @title plot marginal histograms of alluvial plot
#' @description will create gtable with density histograms and frequency plots
#'   of all variables of a given alluvial plot.
#' @param p alluvial plot
#' @param data_input dataframe, input data that was used to create dataframe
#' @param top logical, position of histograms, if FALSE adds them at the bottom,
#'   Default: TRUE
#' @param keep_labels logical, keep title and caption, Default: FALSE
#' @param ... additional arguments for specific alluvial plot types: pred_train
#'   can be used to pass training predictions for model response alluvials
#' @return gtable
#' @examples
#'\dontrun{
#' p = alluvial_wide(mtcars2, max_variables = 3)
#' plot_all_hists(p, mtcars2)
#' }
#' @seealso \code{\link[gridExtra]{arrangeGrob}}
#' @seealso \code{\link[easyalluvial]{add_marginal_histograms}}
#' @rdname plot_all_hists
#' @export
#' @importFrom gridExtra grid.arrange
#' @importFrom ggridges geom_ridgeline_gradient
#' @importFrom stats density
plot_all_hists = function(p, data_input, top = TRUE, keep_labels = FALSE, ...){
  
  vars = levels( p$data$x )
  
  hists = map( vars, plot_hist, p = p, data_input = data_input, ...)
  
  p_margin = do.call( gridExtra::grid.arrange, c( hists, nrow = 1) )
  
}


