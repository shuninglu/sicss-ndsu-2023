### Data Visualization
### check out https://r-graph-gallery.com/index.html or https://r-graph-gallery.com/ggplot2-package.html for tons of good ideas


library(tidyverse)
library(RColorBrewer)
library(maps)
library(ggnetwork)
library(gcookbook)
library(networkD3)
library(igraph)
library(mapproj)


ca <- read_csv("https://raw.githubusercontent.com/ScienceParkStudyGroup/r-lesson-based-on-ohi-data-training/gh-pages/data/ca.csv") 
View(ca) # always, always check your data


p <- ggplot(data=ca,
            mapping = aes(x=year,
                          y=visitors))
p
### Why does it show nothing?

p + geom_point()
p + geom_smooth()
p + geom_point() + 
  geom_smooth()

### Why is this not a very good chart?

p <- ggplot(data=ca,
            mapping = aes(x=year,
                          y=visitors/1000, # reduce visitors var by 1000 
                          color=park_name))

p + geom_smooth() #+ facet_wrap(~park_name) show.legend = FALSE
p + geom_boxplot() 


p <- ggplot(data=ca,
            mapping = aes(x=year,
                          y=visitors,
                          color=park_name,
                          size=visitors/1000))

p + geom_point(alpha=0.4) + 
  xlab('Visitors') +
  ylab('Year') +
  labs(color="Parks", size="Visitors (x1000)")
  #scale_color_manual(values = c('red', 'blue', 'green', 'purple', 'yellow', 'black', 'magenta', 'gray', 'green'))
  #scale_color_brewer(type="div", palette = 1, aesthetics = "color")

  
# On to maps
map()
map('usa')
map('county')
map('state', regions="north dakota")
map('france')


# These functions draw a polygon representation of a map. They're nice, but we can't do much with them.
# Let's look inside:
map_data('county')
map_data('state', regions='north dakota')
midwest_counties <- subset(map_data('county'), region=="ohio"|
                          region=="indiana"|
                          region=="wisconsin"|
                          region=="illinois"|
                          region=="michigan")
                          #region=="north dakota")

# Now, let's fill them:
p <- ggplot(data=map_data('county'),
            mapping = aes(x=long, y=lat, group=group)) #Sets up the canvas

p + geom_polygon(fill='white', color='black') + # Recreates in ggplot so now we can use the full grammar
  coord_map(projection = 'sinusoidal') +
  xlab('The Longer Side')


# Let's make a choropleth map of poverty levels in the midwest
# This requires JOINING two datasets: our map polygons and data

View(midwest_counties)
View(midwest)
### Note that `midwest` is just a dataframe. You can find or create your own. And then:

### Joining two DFs requires a "key" (a column) that's the same in both datasets. But we don't have that...
### So let's make one:

midwest$subregion <- tolower(midwest$county)
midwest_merged <- left_join(midwest_counties, midwest) #You can specify merge key or let R figure it out

View(midwest_merged)

### Now we can go back to ggplot:

p <- ggplot(data=midwest_merged, aes(x=long, y=lat, group=group, fill=percbelowpoverty))

p + 
  geom_polygon(color="gray50")
  #scale_fill_distiller(palette = "RdYlBu")


### Networks
madmen_relations <- madmen
madmen

p <- simpleNetwork(madmen_relations)
p
# saveWidget() lets you save this as a html file
# Or use igraph:
network <- graph_from_data_frame(madmen_relations, directed=F)
network #tweak directed variable
plot(network)



### Sankey Network
links <- madmen
links$value <- 1
#links <- links[links$Name1 < links$Name2, ]  # Removes reciprocal links
links # why value = 1?

nodes <- data.frame(name=c(madmen$Name1, madmen$Name2)) %>% 
                      unique()
nodes

links$IDName1 <- match(links$Name1, nodes$name)-1 # Needs to be zero-indexed. No idea why
links$IDName2 <- match(links$Name2, nodes$name)-1
links

p <- sankeyNetwork(Links = links, 
                   Nodes = nodes, 
                   Source="IDName1", 
                   Target="IDName2", 
                   Value="value", 
                   NodeID="name",
                   sinksRight=TRUE) 
p
