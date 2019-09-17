library(plumber)
library(tidyverse)
library(scico)
#* @apiTitle Amusement park injuries Tidy Tuesday

#* Plot a tileplot of 20190910 #tidytuesday challenge
#* @param p_color_pallete palette from scico package.
#* @param p_pallete_direction palette direction.
#* @png (width = 5 , height = 7,units="in" , res=300)
#* @get /plot_tile
function( p_color_pallete="roma",p_pallete_direction="-1") {
    
    # cast to integer
    palette_direction_param <- as.integer(p_pallete_direction)
    
    safer_parks <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-10/saferparks.csv")
    
    safer_parks_no_na <- safer_parks %>% 
        select(category,tradename_or_generic,device_type,
               device_category,industry_sector,bus_type,
               source,fix_port,acc_state,acc_date,acc_id)
    
    pf_data <-  safer_parks_no_na %>% 
        mutate(date_acc=lubridate::mdy(acc_date)) %>% 
        mutate(year=as.factor(lubridate::year(date_acc)))
    
    
    pf_data_min <- min(pf_data$date_acc)
    pf_data_max <- max(pf_data$date_acc)
    
    pf <- pf_data %>% 
        count(acc_state,year,sort = TRUE) %>% 
        mutate(acc_state=fct_reorder(acc_state,n)) %>% 
        filter(n>0) %>% 
        ggplot(aes(x=year,y=acc_state,fill=n))+
        # geom_col()+
        geom_tile()+
        scale_fill_scico(palette = p_color_pallete,direction = palette_direction_param)+
        scale_x_discrete()+
        theme_light()+
        labs(x="",y="",fill="#accidents",
             title="Amusement Park Accidents In USA",
             subtitle = paste0("from ",pf_data_min, " to ",pf_data_max),
             caption="#TidyTuesday")
    # ggridges::geom_density_ridges()
    # coord_flip()
    # theme(legend.position = "none")
    # ggsave(filename = "accidents_by_year.png",plot = pf,width = 5,height = 7)
    
    print(pf) # < VERY IMPORTANT TO AVOID BLANK SCREEN :P
}
#* get scico color palette names
#* @get /palette_names
function(){
    scico::scico_palette_names()
}


