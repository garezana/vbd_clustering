# Libraries
library(tidyverse)
library(viridis)
library(patchwork)
library(hrbrthemes)
library(circlize)
library(networkD3)
library(gtools)

multi_group_data = read.csv("/Users/virnaliz/Desktop/Garezana/gideon_data/data_clusters.csv")#colClasses=c("NULL", NA, NA))
multi_group_data <- multi_group_data[,-1]

#functions
make_two_feat_df = function( df, feature_1, feature_2){
  if (nrow(df) > 2){
  two_feature_df = df %>% select(feature_1,feature_2)
  return(two_feature_df)
  } else {
    return(df)
    }
}

transform_long_percentages = function(multi_group_data){
  #two_feature_df = make_two_feat_df(df, feature_1, feature_2)
  output = as.data.frame.matrix(table(multi_group_data))
  output = output %>% mutate(total = rowSums(.))
  proportions = apply(output, 2,function(x,y) (x/y),  output$total)
  new=subset(proportions, select = -c(total))
  return(as.data.frame(new))
  
}


# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/13_AdjacencyDirectedWeighted.csv", header=TRUE)




# I need a long format
sankey_plotting=function(percent_data){
data_long <- percent_data %>%
  rownames_to_column %>%
  gather(key = 'key', value = 'value', -rowname) %>%
  filter(value > 0)
colnames(data_long) <- c("source", "target", "value")
data_long$target <- paste(data_long$target, " ", sep="")

data_long=data_long %>% arrange(as.numeric(source))

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(name=c(as.character(data_long$source), as.character(data_long$target)) %>% unique()) 


nodes=mixedsort(nodes)
# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
data_long$IDsource=match(data_long$source, nodes$name)-1 
data_long$IDtarget=match(data_long$target, nodes$name)-1

# prepare colour scale
ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

# Make the Network
sankeyNetwork(Links = data_long, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name", 
              sinksRight=FALSE, colourScale=ColourScal, fontSize=13, nodePadding=5,iterations = 0)


}


percent_data = transform_long_percentages(multi_group_data)

sankey_plotting (percent_data)
