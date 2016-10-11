library(igraph)
library(shiny)
library(shinydashboard)
source('attorney.plot.R')
source('SubPlots.R')
library(visNetwork)

ttl = 'Active_Clients_Steps'

#This file is of clients to connected attorneys
df_atty = read.csv("NA-Anon-AllPersonnel.csv")
df_atty = df_atty[df_atty$Kirkland.Title=='Partner' & df_atty$Status=='Active',]
df_atty$colors = 0
#str(df_atty)

df_clients = read.csv("NA-Anon-ClientRelationships.csv")

#str(df_clients)
#Filter on Active Clients
df_clients_active = subset(df_clients, df_clients$Client.Status == 'Active' )
df_clients_active$colors = 1
df_clients_active$colors[which(df_clients_active$Contact.Types=='Alumni')] = 3
#str(df_clients_active)
#table(df_clients$Contact.Types)


#START EDITING HERE
## Start with Actors
actors = rbind(unique(df_clients_active[,c(1,2,5,16)]),df_atty[,c(1,3,6,13)])
actors = actors[!duplicated(actors[,1]),]
colnames(actors)[1:2]= c('id','fullname')
actors$fullname = as.character(actors$fullname)
#str(actors)
#table(actors$colors)

#contacts = data.frame(ids=df$Contact.ID, colors=1, fullname=df$Full.Name..Company.or.Person.)
#contacts = contacts[!duplicated(contacts),]

#reshuffle order - contacts, alumni, attorneys
actors_new = actors[actors$colors==1,]
actors_new = rbind(actors_new,actors[actors$colors==3,])
actors_new = rbind(actors_new,actors[actors$colors==0,])
actors = actors_new


#attorneys = data.frame(ids=df$Related.Contacts.Contact.ID , colors=0, fullname = df$Related.Contacts.Name)
#attorneys = attorneys[!duplicated(attorneys),]

# Create Relationships
relations_atty_client = data.frame(from=df_clients_active$Contact.ID, to=df_clients_active$Related.Contacts.Contact.ID)
#remove Inactive Attorneys
relations_atty_client = relations_atty_client[is.element(relations_atty_client$to, df_atty$Contact.ID),]
relations = relations_atty_client

#actors = rbind(contacts,attorneys)
#actors = actors[!duplicated(actors[,1]),]
#actors = actors[complete.cases(actors),]
#str(actors)
#sum(is.na(actors))

#relations = data.frame(from=df$Related.Contacts.Contact.ID,to=df$Contact.ID)
#relations = data.frame(from=0,to=0)
#sum(is.na(relations))
#relations = relations[complete.cases(relations),]
#str(relations)



#Create Graph
#g = graph_from_data_frame(relations, directed=FALSE, vertices=actors)
set.seed(1)
#relations=relations[sample.int(length(relations$from),length(relations$from)/10),]
actors = actors[actors$id %in% unique(rbind(data.frame(id=relations$to),data.frame(id=relations$from), deparse.level = 0))[,'id'],]
#print(dim(relations))
#print(dim(actors))
#g = graph_from_data_frame(relations, directed=FALSE, vertices=actors)
#set.seed(1)
#g_layout = layout.drl(g, use.seed=TRUE)
#g_layout_new = normalize_layout(g_layout)




#Setup Colors
#clr=get.vertex.attribute(g,'colors')
#clr = actors$colors
#clr[clr==3] = '#ffc0cb'
actors$group = 'Contacts'
actors$group[actors$colors==0] = 'Attorneys'
actors$group[actors$colors==2] = 'Top Clients'
actors$group[actors$colors==3] = 'Alumni'
#clr[clr==2] = '#008000'
#clr[clr==1] = '#ff0000'
#clr[clr==0] = '#0000ff'
#actors$color = clr
#actors$color.background = clr
#actors$color.highlight.background = '#ffff00'
actors$size = 1
actors$title = ifelse(actors$colors == 0, paste0("<p><b>", actors$fullname,"</b><br>",actors$Job.Title,"</p>"),"")
actors$color.highlight = '#ffff00'
actors$color.highlight.background = '#ffff00'
relations$color = "#a8a8a8" 
relations$size = 1
#relations$color.highlight.background = '#ffff00'
#relations$color.highlight = '#ffff00'
#print(table(actors$colors))

uiNames = data.frame(fullname=actors$fullname[actors$colors==0], id = actors$id[actors$colors==0])
uiNames = uiNames[order(uiNames$fullname),]
uiNames = with(uiNames, split(id,fullname))

nodes = actors[,c('id','fullname','Job.Title','group')]
g = graph_from_data_frame(relations, directed=FALSE, vertices=nodes)
print('Made it here')
write.csv(uiNames, 'out.csv')
#g_layout = layout.drl(g, use.seed=TRUE)

