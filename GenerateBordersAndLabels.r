library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(xml2)

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

filename = "Equal-Earth-150E-South-Up-Borders-Labels"
#ggsave( paste(filename, ".png", sep=""), width = (16*992), height = (9*992), units = "px", limitsize = FALSE, bg='NA')
#	3.125 to correct the size, 1.333 for the conversion from 90dpi to 96dpi
ggsave( paste(filename, ".svg", sep=""), width = (16*992*3.125*(4/3)), height = (9*992*3.125*(4/3)), units = "px", limitsize = FALSE, bg='NA')

#	Add the background map
##	Define file paths
input_file  <- paste(filename, ".svg", sep="")
output_file <- paste(filename, ".svg", sep="")

##	Read the SVG file
svg <- read_xml(input_file)

##	Create the map background element
new_element <- '<image x="495" y="832" xlink:href="Equal-Earth-Physical-Relief-No-Halos-150E-South-Up.png"/>'

##	Parse the new element
new_element <- read_xml(new_element, as_html = FALSE)

##	Add it after the background rectangle
target_element <- xml_find_first(svg, "//d1:rect") #	"d1" because it has a namespace. Otherwise the library barfs
xml_add_sibling(target_element, new_element, .after = TRUE)

#	Add the colophon
colophon_element <- '<text style="fill: rgb(0, 0, 0); font-family: \'DejaVu Sans\'; font-size: 42.7px; font-weight: 700; white-space: pre;">
	<tspan x="14120" y="7760">South Up, Aotearoa Centred, Equal-Earth Projection Map</tspan>
	<tspan>By Terence Eden: </tspan>
	<tspan style="font-family: \'DejaVu Sans Mono\';">https://shkspr.mobi/blog/map</tspan>
	<tspan>Public Domain data from:</tspan>
	<tspan style="font-family: \'DejaVu Sans Mono\';">https://equal-earth.com</tspan>
	<tspan style="font-family: \'DejaVu Sans Mono\';">https://naturalearthdata.com</tspan>
	<tspan>CC BY-SA data from:</tspan>
	<tspan style="font-family: \'DejaVu Sans Mono\';">https://openstreetmap.org</tspan>
</text>'

##	Parse the new element
colophon_element <- read_xml( gsub("[\r\n]", "", colophon_element), as_html = FALSE)

##	Add it after the background image
target_element <- xml_find_first(svg, "//d1:g") #	"d1" because it has a namespace. Otherwise the library barfs
xml_add_sibling(target_element, colophon_element, .after = TRUE)

##	Save it
write_xml(svg, output_file)
