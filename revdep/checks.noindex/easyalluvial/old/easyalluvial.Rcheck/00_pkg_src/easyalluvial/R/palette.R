

# satisfy CMDcheck
# https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when


if(getRversion() >= "2.15.1"){
  utils::globalVariables( c('hex', 'red', 'green', 'blue', 'index', 'similar_index', 'RGB') )
}



#' @title compose palette from qualitative RColorBrewer palettes
#' @description uses c('#FF0065','#009850', '#A56F2B', '#005EAA', '#710500', '#7B5380', '#9DD1D1')
#' and then adds all unique values found in all qualitative RColorBrewer palettes

#' @return vector with hex values
#' @examples
#' palette_qualitative()
#' @seealso
#'  \code{\link[RColorBrewer]{RColorBrewer}}
#' @rdname palette_qualitative
#' @export
#' @importFrom RColorBrewer brewer.pal.info brewer.pal
palette_qualitative = function(){

  qual_col_pals = RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
  palette = unlist(mapply(RColorBrewer::brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  return( unique(c( c('#FF0065','#009850', '#A56F2B', '#005EAA', '#710500', '#7B5380', '#9DD1D1'),palette) ) )
}

#' @title color filters for any vector of hex color values
#' @description filters are based on rgb values
#' @param palette any vector with hex color values, Default: palette_qualitative()
#' @param similar, logical, allow similar colours, similar colours are detected using a
#'   threshold (thresh_similar), two colours are similar when each value for RGB
#'   is within threshold range of the corresponding RGB value of the second
#'   colour, Default: F
#' @param greys, logical, allow grey colours, blue == green == blue , Default: T
#' @param reds, logical, allow red colours, blue < 50 & green < 50  & red > 200 , Default:
#'   T
#' @param greens, logical, allow green colours, green > red & green > blue, Default: T
#' @param blues, logical, allow blue colours, blue > green & green > red, Default: T
#' @param dark, logical, allow colours of dark intensity, sum( red, green, blue) < 420 ,
#'   Default: T
#' @param medium, logical, allow colours of medium intensity, between( sum( red, green,
#'   blue), 420, 600) , Default: T
#' @param bright, logical, allow colours of bright intensity, sum( red, green, blue) > 600, Default: T
#' @param thresh_similar, int, threshold for defining similar colours, see similar, Default: 25
#' @return vector with hex colors
#' @examples
#'
#' require(magrittr)
#'
#' palette_qualitative() %>%
#'   palette_filter(thresh_similar = 0) %>%
#'   palette_plot_intensity()
#'
#'\dontrun{
#'# more examples---------------------------
#'
#' palette_qualitative() %>%
#'   palette_filter(thresh_similar = 25) %>%
#'   palette_plot_intensity()
#'
#' palette_qualitative() %>%
#'   palette_filter(thresh_similar = 0, blues = FALSE) %>%
#'   palette_plot_intensity()
#'}
#' @rdname palette_filter
#' @export
palette_filter = function( palette = palette_qualitative()
                         , similar = F
                         , greys = T
                         , reds = T
                         , greens = T
                         , blues = T
                         , dark = T
                         , medium = T
                         , bright = T
                         , thresh_similar = 25 ){

  # create tibble with RGB
  palette = tibble( hex = palette ) %>%
    mutate( rgb = map(hex, col2rgb)
            , rgb = map(rgb, t)
            , rgb = map(rgb, as_tibble) ) %>%
    unnest( c(rgb) )

  # for greys R == G == B
  if( greys == F ){
    palette = palette %>%
      filter( red != green, red != blue)
  }

  if( reds == F ){
    palette = palette %>%
      filter( ! ( blue < 50 & green < 50  & red > 200 ) )
  }

  if( greens == F ){
    palette = palette %>%
      filter( ! ( green > red & green > blue ) )
  }

  if( blues == F ){
    palette = palette %>%
      filter( ! ( blue > green & green > red) )
  }

  if( bright == F){
    palette = palette %>%
      filter( (red + green + blue ) < 600 )
  }

  if( medium == F){
    palette = palette %>%
      filter( ! between( (red + green + blue ), 420, 600 ) )
  }

  if( dark == F){
    palette = palette %>%
      filter( (red + green + blue ) > 420 )
  }

  if( similar == F & thresh_similar > 0){

    get_similar = function(i, r, g, b, df, thresh_similar ){
  
      df %>%
        filter( i != index ) %>%
        filter( red >= (r - thresh_similar), red <= (r + thresh_similar) ) %>%
        filter( green >= (g - thresh_similar), green <= (g + thresh_similar) ) %>%
        filter( blue >= (b - thresh_similar), blue <= (b + thresh_similar) ) %>%
        .$index
    }
  
    palette = mutate(palette, index = row_number() )
  
    filter_indeces = palette %>%
      mutate( similar_index = pmap( list(index, red, green, blue), get_similar, palette, thresh_similar ) ) %>%
      unnest( c(similar_index) ) %>%
      mutate( larger_index = ifelse( similar_index > index, similar_index, index) ) %>%
      .[['larger_index']] %>%
      unique()
  
    palette = palette %>%
      filter( ! index %in% filter_indeces )

  }

  return( palette$hex  )

}


#' @title increases length of palette by repeating colours
#' @description works for any vector
#' @param palette any vector, Default: palette_qualitative()
#' @param n, int, length, Default: 100
#' @return vector with increased length
#' @examples
#'
#' require(magrittr)
#'
#' length(palette_qualitative())
#'
#' palette_qualitative() %>%
#'   palette_increase_length(100) %>%
#'   length()
#' @rdname palette_increase_length
#' @export
palette_increase_length = function( palette = palette_qualitative(), n = 100 ){

  multiple = n / length(palette)
  multiple = ceiling(multiple)

  palette = rep( palette, multiple )

  return( palette[1:n] )

}

#' @title plot rgb values of palette
#' @description grouped bar chart
#' @param palette any vector containing color hex values
#' @return ggplot2 plot
#' @examples
#' \dontrun{
#' if(interactive()){
#' palette_qualitative() %>%
#'   palette_filter( thresh = 50) %>%
#'   palette_plot_rgp()
#'  }
#' }
#' @seealso
#'  \code{\link[easyalluvial]{palette_plot_intensity}}
#' @rdname palette_plot_rgp
#' @export
#' @import forcats
palette_plot_rgp = function(palette){

  palette = unique(palette)

  hex = tibble( hex = palette ) %>%
    mutate( RGB = map(hex, col2rgb)
            , RGB = map(RGB, t)
            , RGB = map(RGB, as_tibble)
    ) %>%
    # arrange(hex) %>%
    mutate( hex = forcats::as_factor(hex) )

  palette = hex %>%
    unnest( c(RGB) ) %>%
    gather( key = 'RGB', value = 'value', - hex ) %>%
    group_by(hex) %>%
    mutate( sum = sum(value) ) %>%
    ungroup() %>%
    mutate( hex = fct_reorder(hex, sum) ) %>%
    arrange(hex)

  p = ggplot( palette, aes(hex, value, group = RGB, fill = hex) ) +
    geom_col( position = 'dodge', color = 'white') +
    coord_flip() +
    scale_fill_manual( values = levels( palette$hex) )

  return(p)
}

#' @title plot colour intensity of palette
#' @description sum of red green and blue values
#' @param palette any vector containing color hex values
#' @return ggplot2 plot
#' @examples
#' \dontrun{
#' if(interactive()){
#' palette_qualitative() %>%
#'   palette_filter( thresh = 25) %>%
#'   palette_plot_intensity()
#'  }
#' }
#' @seealso
#'  \code{\link[easyalluvial]{palette_plot_rgp}}
#' @rdname palette_plot_intensity
#' @export

palette_plot_intensity = function(palette){

  palette = unique(palette)

  hex = tibble( hex = palette ) %>%
    mutate( RGB = map(hex, col2rgb)
            , RGB = map(RGB, t)
            , RGB = map(RGB, as_tibble)
    ) %>%
    mutate( hex = forcats::as_factor(hex) )

  palette = hex %>%
    unnest( c(RGB) ) %>%
    gather( key = 'RGB', value = 'value', - hex ) %>%
    group_by( hex ) %>%
    summarise( sum = sum(value) ) %>%
    mutate( hex = forcats::fct_reorder(hex, sum))

  ggplot( palette, aes(hex, sum, fill = hex) ) +
    geom_col(  color = 'white') +
    geom_hline(yintercept = 420) +
    geom_hline(yintercept = 600) +
    coord_flip() +
    scale_fill_manual( values = levels( palette$hex) ) +
    labs( y = 'RGB sum', x = '', caption = 'lines denote thresholds for dark, medium, bright colors')
}

