library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

pdf(NULL)	#	Stop the autogeneration of "Rplots.pdf"

centre <- 150 # NZ mostly centred, prevents Africa and South America wrapping.
projection <- "eqearth" # Equal Earth Projection
crs_string <- paste( "+proj=", projection, " +lon_0=", centre, " +axis=wsu", sep="") # WSU puts South up
target_crs <- st_crs(  crs_string  )

#	Generate the world
worldrn <- ne_countries( scale = "medium", returnclass = "sf" ) %>%
   st_make_valid()
 
#	Add the local names  
##	Remove NA so that it doesn't get confused with "NA" (Namibia)
worldrn$iso_a2 <- ifelse(is.na(worldrn$iso_a2), "??", worldrn$iso_a2)
CC_Names <- read.csv("CC-to-local_name.csv") #	CSV containing "CN, 中国" etc
worldrn <- merge(worldrn, CC_Names, by.x = "iso_a2", by.y = "CC", all.x = TRUE)	#	Merge the local_name column
##	Add back in the genuine NA
worldrn$iso_a2 <- ifelse(worldrn$iso_a2 == "??", NA, worldrn$iso_a2)
#	Replace any blanks in local_name with the long name from the original data
worldrn$local_name <- ifelse(is.na(worldrn$local_name), worldrn$name_long, worldrn$local_name)

#	Move the offset
offset <- 180 - centre

#  Recalculate the polygons
polygon <- st_polygon( x = list(rbind(
                                 c(-0.0001 - offset, 90),
                                 c(0 - offset, 90),
                                 c(0 - offset, -90),
                                 c(-0.0001 - offset, -90),
                                 c(-0.0001 - offset, 90)
))) %>%
   st_sfc() %>%
   st_set_crs(4326)

#	Modify world dataset to remove overlapping portions with world's polygons
world2 <- worldrn %>% st_difference(polygon)

#	Transform
world3 <- world2 %>% st_transform(crs = target_crs)

#	Create a plot with the borders, country names, and curve lines
ggplot(
	data = world3, 
	aes(group = admin)
) +
   geom_sf( 
   	fill = "NA", 
   	lwd = 1 ) + #	Thin borders
   geom_sf_text( 
   	aes( 
   		label = local_name,	#	Use the country's local name. or use name_long for names in English
   		fontface = "bold", 
   		family = "sans-serif"
   	),
   	size = 15
   ) + 
   theme(
      plot.background = element_blank(),	#	Remove plot bg 
      plot.margin = margin(0,0,0,0),	#	Remove any margin
      panel.background = element_blank(),	#	No fill
		panel.grid.major = element_line( color = gray(.5), linetype = "dashed", size = 1 ),	#	Dotted curve lines 
      axis.title.x = element_blank(),  #	remove all axis titles, labels, ticks
		axis.text.x  = element_blank(),
		axis.ticks.x = element_blank(),
		axis.title.y = element_blank(),
		axis.text.y  = element_blank(),
		axis.ticks.y = element_blank()
   )

#	Save as reference PNG and editable SVG
#	992 arrived at by trial and error
#ggsave("Equal-Earth-150E-South-Up-Borders-Labels.png", width = (16*992), height = (9*992), units = "px", limitsize = FALSE, bg='NA')
#	3.125 to correct the size, 1.333 for the conversion from 90dpi to 96dpi
ggsave("Equal-Earth-150E-South-Up-Borders-Labels.svg", width = (16*992*3.125*(4/3)), height = (9*992*3.125*(4/3)), units = "px", limitsize = FALSE, bg='NA')
