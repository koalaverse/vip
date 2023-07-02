
require(hexSticker)
require(tidyverse)
require(easyalluvial)

monthly_flights = nycflights13::flights %>%
  group_by(month, tailnum, origin, dest, carrier) %>%
  summarise() %>%
  group_by( tailnum, origin, dest, carrier) %>%
  count() %>%
  filter( n == 12 ) %>%
  select( - n ) %>%
  left_join( nycflights13::flights ) %>%
  .[complete.cases(.), ] %>%
  ungroup() %>%
  mutate( flight_id = pmap_chr(list(tailnum, origin, dest, carrier), paste )
          , qu = cut(month, 4)) %>%
  group_by(flight_id, carrier, origin, dest, qu ) %>%
  summarise( mean_arr_delay = mean(arr_delay) ) %>%
  ungroup() %>%
  mutate( mean_arr_delay = ifelse( mean_arr_delay < 10, 'on_time', 'late' ) )



levels(monthly_flights$qu) = c('Q1', 'Q2', 'Q3', 'Q4')

data_long = monthly_flights

col =  viridisLite::inferno(12)

p = alluvial_long(monthly_flights, key = qu, value =  mean_arr_delay, fill = carrier, id = flight_id
     , col_vector_flow = col
     , col_vector_value = c('#666633', '#999966')
     , stratum_labels = F
     , stratum_width = 1/4 ) +
  labs( caption = '') +
  theme_minimal() +
  theme( text = element_blank(), panel.grid = element_blank() )


p <- p + theme_void() + theme_transparent()

sticker(p, package="easyalluvial", p_size=12, s_x=1, s_y=.9,
        p_x = 1
        , p_y = 1.625
        , p_color = '#0f0f0a'
        , s_width = 1.3
        , s_height = 1.3
        , h_fill = 'ivory'
        , h_color = '#666633'
        , filename = './inst/logo/logo.png'
        )


