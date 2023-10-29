library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(dplyr)
library(xml2)

pdf(NULL)	#	Stop the autogeneration of "Rplots.pdf"

#	Function to convert uppercase letters to their Unicode Regional Indicator Symbols
#	So GB becomes 🇬 🇧, which is then returned as 🇬🇧
string_to_regional_indicators <- function(input_string) {
	if (nchar(input_string) == 2 && substr(input_string, 1, 1) %in% LETTERS && substr(input_string, 2, 2) %in% LETTERS) {
		indicator1 <- intToUtf8(utf8ToInt("🇦") + (utf8ToInt(substr(input_string, 1, 1)) - utf8ToInt("A")))
		indicator2 <- intToUtf8(utf8ToInt("🇦") + (utf8ToInt(substr(input_string, 2, 2)) - utf8ToInt("A")))
		return( paste(indicator1, indicator2, sep="") )
  } else {
    return("") # Or 🚩 
  }
}


centre <- 150 # NZ mostly centred, prevents Africa and South America wrapping.
projection <- "eqearth" # Equal Earth Projection
crs_string <- paste( "+proj=", projection, " +lon_0=", centre, " +axis=wsu", sep="") # WSU puts South up
target_crs <- st_crs(  crs_string  )

#	Generate the world
worldrn <- ne_countries( scale = "medium", returnclass = "sf" ) %>%
   st_make_valid()
   
#	Add the flags
worldrn <- worldrn %>%
  mutate(flag = sapply(iso_a2, string_to_regional_indicators))

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
   		label = flag,	#	Use the country's local name. or use name_long for names in English
   		family = "monospace"
   	),
   	size = 40
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
filename = "Equal-Earth-150E-South-Up-Borders-Flags"
#ggsave("Equal-Earth-150E-South-Up-Borders-Labels.png", width = (16*992), height = (9*992), units = "px", limitsize = FALSE, bg='NA')
#	3.125 to correct the size, 1.333 for the conversion from 90dpi to 96dpi
ggsave( 
	paste(filename, ".svg", sep=""), 
	width  = (16*992*3.125*(4/3)), 
	height = ( 9*992*3.125*(4/3)), 
	units = "px", 
	bg = 'NA',
	limitsize     = FALSE,	#	Prevent large filesize warning 
	fix_text_size = FALSE	#	Make text editable
)

#	Add the background map
##	Define file paths
input_file  <- paste(filename, ".svg", sep="")
output_file <- paste(filename, ".svg", sep="")

##	Read the SVG file
svg <- read_xml(input_file)

##	Create the map background element
bg_element <- '<image x="495" y="832" xlink:href="Equal-Earth-Physical-Relief-No-Halos-150E-South-Up.png"/>'

##	Parse the new element
bg_element <- read_xml(bg_element, as_html = FALSE)

##	Add it after the background rectangle
target_element <- xml_find_first(svg, "//d1:rect") #	"d1" because it has a namespace. Otherwise the library barfs
xml_add_sibling(target_element, bg_element, .after = TRUE)

#	Add the colophon
colophon_element <- '<text style="fill: rgb(0, 0, 0); font-family: \'DejaVu Sans\'; font-size: 42.7px; font-weight: 700; white-space: pre;">
    <tspan x="14120" y="7750" dy="0em"   >South Up, Aotearoa Centred, Equal-Earth Projection Map</tspan>
    <tspan x="14120" y="7750" dy="1.25em">By Terence Eden:</tspan>
    <tspan x="14120" y="7750" dy="2.5em"  style="font-family: \'DejaVu Sans Mono\';">https://shkspr.mobi/blog/map</tspan>
    <tspan x="14120" y="7750" dy="3.75em">Public Domain data from:</tspan>
    <tspan x="14120" y="7750" dy="5em"    style="font-family: \'DejaVu Sans Mono\';">https://equal-earth.com</tspan>
    <tspan x="14120" y="7750" dy="6.25em" style="font-family: \'DejaVu Sans Mono\';">https://naturalearthdata.com</tspan>
    <tspan x="14120" y="7750" dy="7.5em" >CC BY flags from:</tspan>
    <tspan x="14120" y="7750" dy="8.75em" style="font-family: \'DejaVu Sans Mono\';">https://github.com/twitter/twemoji</tspan>
  </text>'

##	Parse the new element
colophon_element <- read_xml( gsub("[\r\n]", "", colophon_element), as_html = FALSE)

##	Add it after the background image
target_element <- xml_find_first(svg, "//d1:g") #	"d1" because it has a namespace. Otherwise the library barfs
xml_add_sibling(target_element, colophon_element, .after = TRUE)

##	Save it
write_xml(svg, output_file)

