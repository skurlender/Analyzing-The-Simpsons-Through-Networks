library(dplyr)
library(igraph)
library(readr)

base_simpsons = read_csv("network analysis the simpsons data set - Sheet1.csv")

df = data.frame(base_simpsons)

library(tidyr)
library(reshape2)

m_df = df %>% melt(id.vars = c("Season", "Episode", "IMDB.Rating",
                               "Episode.Title")) %>% 
  filter(value > 0) %>% 
  rename(person = variable) %>% 
  mutate(episode_id = paste0(Season, "_", Episode),
         person = as.character(person))

nodes1 = data.frame(name = m_df$episode_id, type=T, stringsAsFactors = F)
nodes2 = data.frame(name = as.character(m_df$person),
                    type=F, stringsAsFactors = F)
nodes = dplyr::bind_rows(nodes1, nodes2) %>% distinct()

## make a bipartite network of character-to-episode
g = graph_from_data_frame(m_df %>% select(episode_id, person, IMDB.Rating),
                          directed=F,
                          nodes)

plot(g, layout=layout_as_bipartite(g, vgap = 0.5), edge.width=3*E(g)$weight, vertex.size=10)

bp = bipartite.projection(g, which="both")
g1 = bp[[1]] ## the network of character-to-character
g2 = bp[[2]] ## the network of episode-to-episode

plot(g1)
plot(g2)

list.edge.attributes(g1)
table(E(g1)$weight)
r = E(g1)$weight < 15
g3 = g1 - E(g1)[r]
plot(g3)


clt = cluster_walktrap(g1)
clt2 = cluster_walktrap(g2)
member = membership(clt)
member2 = membership(clt2)
table(member)
table(member2)
member_df = data.frame(node = names(member),
                       cluster = as.vector(member),
                       stringsAsFactors = F) %>% 
  mutate(sam_cluster = gsub("^.*[.](?=[0-9])", "", node, perl=T))

member_df3 = data.frame(node = names(member2),
                       cluster = as.vector(member2),
                       stringsAsFactors = F) %>% 
  mutate(sam_cluster = gsub("^.*[.](?=[0-9])", "", node, perl=T))


member_df %>% filter(cluster == 7) %>% count(sam_cluster)
m_df %>% select(Episode, Episode.Title, Season, episode_id, IMDB.Rating) %>% 
  inner_join(member_df, by=c("episode_id"="node")) %>% 
  distinct(episode_id, Episode.Title, IMDB.Rating, cluster) -> m_df2

m_df %>% select(Episode, Episode.Title, Season, episode_id, IMDB.Rating) %>% 
  inner_join(member_df3, by=c("episode_id"="node")) %>% 
  distinct(episode_id, Episode.Title, IMDB.Rating, cluster) -> m_df3

m_df2 %>% group_by(cluster) %>% summarise(x = mean(IMDB.Rating))

plot(g1, vertex.color = member,
     mark.groups=communities(clt), vertex.label.cex=0.7)

plot(g2, vertex.color = member2,
     mark.groups=communities(clt2), vertex.label.cex=0.7)

#################################################################

m_df3 = df %>% melt(id.vars = c("Season", "Episode", "IMDB.Rating",
                               "Episode.Title")) %>% 
  filter(value > 0, IMDB.Rating > 8.5) %>%
  rename(person = variable) %>% 
  mutate(episode_id = paste0(Season, "_", Episode),
         person = as.character(person))

nodes3 = data.frame(name = m_df3$episode_id, type=T, stringsAsFactors = F)
nodes4 = data.frame(name = as.character(m_df3$person),
                    type=F, stringsAsFactors = F)
nodes1 = dplyr::bind_rows(nodes3, nodes4) %>% distinct()

gnew = graph_from_data_frame(m_df3 %>% select(episode_id, person, IMDB.Rating),
                          directed=F,
                          nodes1)

plot(gnew)

#keep copy pasting
