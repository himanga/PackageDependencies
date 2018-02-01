library(tidyverse)
library(magrittr)
library(igraph)
library(ggraph)

#All installed packages
pkgs <- installed.packages() %>% as_tibble()

#Tibble with dependencies of installed packages in a list, one row per package
#takes a few seconds to run
pkgs_depends <- pkgs %>% 
  rowwise %>%
  mutate(dependencies = map(Package, function(x) tools::package_dependencies(x)[[1]] ) ) %>% 
  select(Package, dependencies)

#Packages to color in the graph
highlighted <- c("methods","stats", "Rcpp", "tidyverse", "utils", "graphics", "grDevices");
highlightedcolors <- c("methods" = "blue","stats" = "orange", "Rcpp" = "darkred", 
                       "tidyverse" = "green", "utils" = "purple", "graphics" = "black", 
                       "grDevices"="red");

#Tibble with pairs of packages and dependencies, one row per dependency
pkgs_depends_expanded <- pkgs_depends %>% 
  ungroup %>% 
  mutate(hasdep = map_lgl(pkgs_depends$dependencies, function(x) {if(length(x) > 0) {T} else F } ) ) %>% 
  filter(hasdep == T) %>% 
  unnest(dependencies) %>% 
  mutate(hasdep = NULL,
         dependcolor = map_chr(dependencies, function(d) 
           ifelse(d %in% highlighted,
                  d,
                  "other"
           )
         )
  )

#Vector of all packages referenced, should only need this if missing a dependency
allpkgs <- c(pull(pkgs_depends_expanded,"Package"), pull(pkgs_depends_expanded,"dependencies"))

#Make a data frame with info for all verticies (packages), used for package label
vertices <- data_frame( id=allpkgs, label=allpkgs ) %>% 
  group_by(id) %>% 
  summarize(n=n()) %>% 
  mutate(n=NULL, label=id)

#Combine tibbles into a graph
pkgs_depends_graph <- graph_from_data_frame(pkgs_depends_expanded, vertices = vertices)

#Visualize
ggraph(pkgs_depends_graph, layout="linear", circular=TRUE) + 
  geom_edge_arc(aes(color=dependcolor)) + 
  geom_node_label(aes(label=label)) + 
  theme_graph() +
  scale_edge_color_manual(labels=c("other", highlighted ), values=c("other" = "gray", highlightedcolors))
  
