install.packages("vosonSML")
library(vosonSML)
#Google Api Key
Myapikey ="AIzaSyAvD4At_L1REv4rPOEcKBDe_NTjCMhgwyY"
Key = Authenticatewith("youtube",apikey =Myapikey)

Key_youtube ="AIzaSyCO6xwc-qg0BbYsgl2ieH3w-g0WQgTpcg0"
youtubeAuth <- Authenticate("youtube", apiKey = Key_youtube)


# create a list of youtube video ids to collect on
videoIDs <- GetYoutubeVideoIDs(c("https://youtu.be/32o0DnuRjfg", 
                                 "https://youtu.be/NLrb41ls4qo"))

# collect approximately 200 threads/comments for each youtube video
youtubeData <- youtubeAuth %>% 
  Collect(videoIDs = videoIDs, writeToFile = TRUE, verbose = FALSE, maxComments = 200)

str(youtubeData)
View(youtubeData)

write.csv(youtubeData,file = 'youtube_data.csv',row.names = FALSE)

#Reading the data 

data =read.csv(file = 'youtube_data.csv',header = T)
data = data[data$ReplyToAnotherUser!= FALSE,]
y =data.frame(data$User,data$ReplyToAnotherUser)

#Create User Network
library(igraph)
net = graph.data.frame(y,directed = TRUE)

#If an user is replying to his or her comment we need to eliminate it
net =simplify(net)


#Checking for vertices and edges
V(net)
E(net)

V(net)$label =V(net)$name
V(net)$degree =degree(net)

#Visualization
hist(V(net)$degree,col = "grey",main = "plot showing frequency",ylab ="frequency",xlab = "degree")

#Network digram
plot(net,vertex.size =0.8*V(net)$degree,
edge.arrow.size = 0.8,
vertex.label.cex =0.01*V(net)$degree)

#Sentiment analysis
install.packages("syuzhet")
library(syuzhet)
data =read.csv(file = 'youtube_data.csv',header = T)

#for sentiment analysis we need only comment column 
comments = iconv(data$Comment,"UTF-8")
s =get_nrc_sentiment(comments)
head(s)
s$neutral =ifelse(s$positive +s$negative ==0,1,0)
#bar plot
barplot(100*colSums(s)/sum(s),las =2,col=rainbow(10),
                                               ylab="percentage",main ="Sentiment Analysis of youtube video comments")