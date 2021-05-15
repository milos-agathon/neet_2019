# SIMPLE CHOROPLETH MAP OF EUROPE

#load libraries
library(ggplot2, quietly=T) 
library(plyr, quietly=T) 
library(rgdal, quietly=T)
library(rgeos, quietly=T)
library(grid, quietly=T) 
library(dplyr, quietly=T)
library(classInt, quietly=T)
library(raster, quietly=T)

set.seed(20210515)

##############
# SHAPEFILES #
##############

#download Eurostat NUTS 2016 shapefiles
url <- "https://gisco-services.ec.europa.eu/distribution/v2/nuts/download/ref-nuts-2016-01m.shp.zip" # location on the Eurostat website
download.file(url, basename(url), mode="wb") #download Eurostat country shapefiles
unzip("ref-nuts-2016-01m.shp.zip") # unzip the boundary data
unzip("NUTS_RG_01M_2016_4326_LEVL_2.shp.zip") # unzip the NUTS-2 folder

#download the country shp
url <- "https://gisco-services.ec.europa.eu/distribution/v2/countries/download/ref-countries-2013-01m.shp.zip" # location on the Eurostat website
download.file(url, basename(url), mode="wb") #download Eurostat country shapefiles
unzip("ref-countries-2013-01m.shp.zip") # unzip the boundary data
unzip("CNTR_RG_01M_2013_4326.shp.zip")

#load NUTS shapefile
nuts2 <- readOGR(getwd(),
		 "NUTS_RG_01M_2016_4326_LEVL_2",
		 verbose = TRUE, 
		 stringsAsFactors = FALSE)

#load country shapefile
cntry <- readOGR(getwd(),
		 "CNTR_RG_01M_2013_4326", 
		 verbose = TRUE, 
		 stringsAsFactors = FALSE)

#only European countries on the map
out <- c("MA", "TN", "DZ", "EG", "LY",
	"JO", "IL", "PS", "SY", "SA",
	"LB", "IQ", "IR", "GL")
cn <- subset(cntry, !FID%in%out)
c <- fortify(cn)

###########
# DATASET #
###########    
# get NUTS2-level data on young people aged 15-34 NEET
edat_lfse_38 <- eurostat::get_eurostat("edat_lfse_38",
                                    time_format = "num")
# let's subset the dataset
neet <- edat_lfse_38  %>%
  			filter(time==2019, # only the year of 2018 
                citizen=="TOTAL", # both citizens and residents
				age=="Y15-34", # ages 15-34
				sex=="T") %>% # all genders
    		dplyr::select (geo, time, values)
names(neet)[1] <- "NUTS_ID"

# merge shp and data.frame
f1 <- merge(neet, nuts2, by="NUTS_ID")
e <- fortify(nuts2, region = "NUTS_ID") %>% 
  mutate(NUTS_ID = as.character(id))
d <- e %>% left_join(f1, by = "NUTS_ID")

# let's find a natural interval with quantile breaks
ni = classIntervals(d$values, 
	            n = 7, 
	            style = 'quantile')$brks
# this function uses above intervals to create categories
labels <- c()
for(i in 1:length(ni)){
    labels <- c(labels, paste0(round(ni[i], 0), 
                             "–", 
                             round(ni[i + 1], 0)))
}
labels <- labels[1:length(labels)-1]

# finally, carve out the categorical variable 
# based on the breaks and labels above
d$cat <- cut(d$values, 
              breaks = ni, 
              labels = labels, 
              include.lowest = T)
levels(d$cat) # let's check how many levels it has (7)

# label NAs, too
lvl <- levels(d$cat)
lvl[length(lvl) + 1] <- "No data"
d$cat <- factor(d$cat, levels = lvl)
d$cat[is.na(d$cat)] <- "No data"
levels(d$cat)

#########
#  PLOT #
#########

p <- 
ggplot() +
geom_polygon(data = c, aes(x = long, 
                                y = lat, 
                                group = group),
                  fill = "grey80") +
  geom_polygon(data = subset(d, !is.na(values)), aes(x = long, 
                                y = lat, 
                                group = group,
                  fill = cat)) +
     geom_path(data = subset(d, !is.na(values)), aes(x = long, 
                                   y = lat, 
                                   group = group), 
              color = NA, size = 0) +
  geom_path(data = c, aes(x = long, 
                                   y = lat, 
                                   group = group), 
              color = "white", size = 0.2) +
coord_map(xlim=c(-10.6600,44.07), ylim=c(32.5000,71.0500), projection="lambert", parameters=c(10.44,52.775)) +
expand_limits(x=c$long,y=c$lat)+
labs(x = "",
     title="Young people aged 15-34 not in\neducation, employment or training in 2019",
     subtitle = "NUTS-2 level",
     caption="©2021 Milos Popovic https://milospopovic.net\nSource: Eurostat\nhttps://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=edat_lfse_38&lang=en")+
scale_fill_manual(name= "% of active population",
  values=c('#fff1c5', '#c3d7ba', '#91bbaf', '#689ca3', '#457e97', '#275f8b', '#00407e', "grey80"),
  labels=c("5–7",  "7–8",  "8–9",  "9–12",    
  	"12–14",   "14–18",   ">18", "No data"),
  drop=F)+
guides(fill=guide_legend(
            direction = "horizontal",
            keyheight = unit(1.15, units = "mm"),
            keywidth = unit(15, units = "mm"),
            title.position = 'top',
            title.hjust = 0.5,
            label.hjust = .5,
            nrow = 1,
            byrow = T,
            reverse = F,
            label.position = "bottom"
          )
    ) +
theme_minimal() +
theme(panel.background = element_blank(), 
legend.background = element_blank(),
legend.position = c(.45, .04),
panel.border = element_blank(),
panel.grid.minor = element_blank(),
panel.grid.major = element_line(color = "white", size = 0.2),
plot.title = element_text(size=20, color="#004b8b", hjust=0.5, vjust=-10),
plot.subtitle = element_text(size=14, color="#4cc2b6", hjust=0.5, vjust=-15, face="bold"),
plot.caption = element_text(size=9, color="grey60", hjust=0.5, vjust=9),
axis.title.x = element_text(size=7, color="grey60", hjust=0.5, vjust=5),
legend.text = element_text(size=10, color="grey20"),
legend.title = element_text(size=11, color="grey20"),
strip.text = element_text(size=12),
plot.margin = unit(c(t=-2, r=-2, b=-2, l=-2),"lines"), #added these narrower margins to enlarge map
axis.title.y = element_blank(),
axis.ticks = element_blank(),
axis.text.x = element_blank(),
axis.text.y = element_blank())

ggsave(filename="neet_2019.png", width=7, height=8.5, dpi = 600, device='png', p)