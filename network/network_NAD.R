library(tidyverse)
library(igraph)
library(ggraph)
library(rgexf)


NAD_master <- read.csv("M:/NAD Development/_sas_datadump/nad_for_slicers.csv", stringsAsFactors = F) %>% 
  filter(!he_institution %in% c("Brunel University London", "", "Non-Scottish University") & schlname_post_merge != "") %>% 
  mutate(schlname_post_merge = str_replace_all(schlname_post_merge, "[//(//)]", "")) %>% 
  subset(nchar(as.character(subj)) >2)

network_1 <- NAD_master %>% 
  select(subj, jacs1_exp)
#############################
#Creating node and edge lists

test <- edge.list(network_1)

Edge_list <- as.data.frame(test$edges)

Node_list <- as.data.frame(test$nodes)

Node_list <- Node_list %>% 
  mutate(category = ifelse(label %in% c("Academic studies in education",
                           "Accounting",
                           "Aerospace engineering",
                           "Agriculture",
                           "Anatomy, physiology & pathology",
                           "Animal science",
                           "Anthropology",
                           "Archaeology",
                           "Architecture",
                           "Artificial intelligence",
                           "Astronomy",
                           "Aural & oral sciences",
                           "Biology",
                           "Biotechnology",
                           "Botany",
                           "Broadly-based programmes within biological sciences",
                           "Broadly-based programmes within law",
                           "Building",
                           "Business studies",
                           "Celtic studies",
                           "Chemical, process & energy engineering",
                           "Chemistry",
                           "Cinematics & photography",
                           "Civil engineering",
                           "Classical studies",
                           "Clinical dentistry",
                           "Clinical medicine",
                           "Clinical veterinary medicine & dentistry",
                           "Combined",
                           "Comparative literary studies",
                           "Complementary medicines, therapies & well-being",
                           "Computer science",
                           "Crafts",
                           "Design studies",
                           "Drama",
                           "Economics",
                           "Electronic & electrical engineering",
                           "English studies",
                           "Finance",
                           "Fine art",
                           "Food & beverage studies",
                           "Forensic & archaeological sciences",
                           "Forestry & arboriculture",
                           "French studies",
                           "Games",
                           "General engineering",
                           "Genetics",
                           "Geology",
                           "German studies",
                           "History by area",
                           "History by period",
                           "History by topic",
                           "Hospitality, leisure, sport, tourism & transport",
                           "Human & social geography",
                           "Human resource management",
                           "Imaginative writing",
                           "Information systems",
                           "Italian studies",
                           "Journalism",
                           "Landscape & garden design",
                           "Law by area",
                           "Law by topic",
                           "Linguistics",
                           "Management studies",
                           "Marketing",
                           "Mathematics",
                           "Mechanical engineering",
                           "Media studies",
                           "Medical technology",
                           "Microbiology",
                           "Molecular biology, biophysics & biochemistry",
                           "Music",
                           "Naval architecture",
                           "Nursing",
                           "Nutrition",
                           "Office skills",
                           "Operational research",
                           "Ophthalmics",
                           "Others in Biological Sciences",
                           "Others in business & administrative studies",
                           "Others in creative arts & design",
                           "Others in education",
                           "Others in engineering",
                           "Others in European languages, literature & related subjects",
                           "Others in historical & philosophical studies",
                           "Others in law",
                           "Others in linguistics, classics & related subjects",
                           "Others in medicine & dentistry",
                           "Others in physical sciences",
                           "Others in social studies",
                           "Others in subjects allied to medicine",
                           "Others in technology",
                           "Pharmacology, toxicology & pharmacy",
                           "Philosophy",
                           "Physical geographical sciences",
                           "Physics",
                           "Planning (urban, rural & regional)",
                           "Politics",
                           "Pre-clinical dentistry",
                           "Pre-clinical medicine",
                           "Pre-clinical veterinary medicine",
                           "Production & manufacturing engineering",
                           "Psychology",
                           "Publicity studies",
                           "Russian & East European studies",
                           "Science of aquatic & terrestrial environments",
                           "Social policy",
                           "Social work",
                           "Sociology",
                           "Software engineering",
                           "Spanish studies",
                           "Sport & exercise science",
                           "Statistics",
                           "Theology & religious studies",
                           "Training teachers",
                           "Zoology"), "University Course", "College Course"))

Node_list <- Node_list %>% 
  mutate(shape = ifelse(category %in% "University Course", "square", "circle"))


network_graph <- graph.data.frame(d=Edge_list, vertices = Node_list, directed = F)
network_graph <- simplify(network_graph)

#calculating degree
deg <- degree(network_graph, mode = "in")

deg <- deg/3

V(network_graph)$size <- deg/3



################################


 cl <- fastgreedy.community(network_graph)
 weights <- ifelse(crossing(cl, network_graph), 1, 100)
 layout <- layout_with_kk(network_graph, weights = weights)
 
 layout <- layout_with_mds(network_graph)


 ggraph(network_graph,
        layout = "manual",
        node.positions = data.frame(x = layout[, 1], y = layout[, 2])) +
   geom_edge_link(colour = '#cccccc',
                  width = 0.01) +
   geom_node_point(aes(size = deg, shape = category, col = category)) +
   theme_graph()
#########################################################

# layout <- create_layout(network_graph, layout='igraph', algorithm = 'linear')
# 
# ggraph(network_graph, layout = "igraph", algorithm = "fr") +
#   geom_edge_link(arrow = arrow(length = unit(4, 'mm')),
#                  end_cap = circle(1, 'mm'),
#                  colour = 'grey',
#                  width = 0.1) +
#   geom_node_point(aes(shape = category, col=category)) +
#   theme_graph()
colrs <- c("red", "green")
V(network_graph)$color <- colrs[V(network_graph)$category]

plot(network_graph, layout = layout_with_dh, vertex.label = NA)


