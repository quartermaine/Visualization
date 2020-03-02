Sys.setlocale(locale = "english")
library(plotly)
library(ggplot2)
library(seriation)
library(dplyr)
library(scales)


#1.1
data <- read.csv(file="prices-and-earnings.csv")
pe <- data[c(1,2,5,6,7,9,10,16,17,18,19)+1]
rownames(pe) <- data[,1]


#1.2
mat <- as.matrix(pe)
plot_ly(x=colnames(mat), y=rownames(mat), 
        z=mat, type="heatmap", colors =colorRamp(c("yellow", "red")))

# It is impossible to see the clusters and outliers based on such heatmap, since all the observations
# have the similar values for all parameters.

#1.3a
rowdist<-dist(mat,method = "minkowski", p=2)
coldist<-dist(t(mat),method = "minkowski", p=2)
order1<-seriate(rowdist, "HC")
order2<-seriate(coldist, "HC")
ord1<-get_order(order1); O1 <- ord1
ord2<-get_order(order2)
reordmatr<-mat[rev(ord1),ord2]
plot_ly(x=colnames(reordmatr), y=rownames(reordmatr), 
        z=reordmatr, type="heatmap", colors =colorRamp(c("yellow", "red")))

# Based on the leaf-node order of Hierarchical clustering (HC), the heatmap indicates that 
# people in such 72 cities have similar conditions for most parameters such as *Vacation Day*,
# *Bread.kg.in.min.*, *Rice.kg.in.min.*, etc. 
#However, *Goods & Services*, *Work Hours* and *Womens Clothing* are relatively different depending on our plot. For instance, only people in Mumbai and
# Delhi spend less than $1500 on *Goods & Services* which give them the lowest *Goods & Services Index* (34.1 and 33.1).
#additionally, there is a slight correlation among *Goods & Services*, *Womens Clothing*, *Food Costs* and *Cloth index*.

c1<-criterion(rowdist, order1)



#1.3b
coldist<-as.dist(1-cor(mat))
rowdist<-as.dist(1-cor(t(mat)))
order1<-seriate(rowdist, "HC")
order2<-seriate(coldist, "HC")
ord1<-get_order(order1)
ord2<-get_order(order2)
reordmatr<-mat[rev(ord1),ord2]
plot_ly(x=colnames(reordmatr), y=rownames(reordmatr), 
        z=reordmatr, type="heatmap", colors =colorRamp(c("yellow", "red")))
#如果Hamiltonian Path Length顺序代表相关性，那么是否表示无论工时多少，人们都会在其他上面花钱
criterion(rowdist, order1)
#第一个好看，因为可以直接看到不同城市哪些时相同的，哪些时不同的

#1.4a
rowdist<-dist(mat,method = "minkowski", p=2)
coldist<-dist(t(mat),method = "minkowski", p=2)
order1<-seriate(rowdist, "TSP")
order2<-seriate(coldist, "TSP")
ord1<-get_order(order1)
ord2<-get_order(order2)
reordmatr<-mat[ord1,ord2]
plot_ly(x=colnames(reordmatr), y=rownames(reordmatr), 
        z=reordmatr, type="heatmap", colors =colorRamp(c("yellow", "red")))

c2<-criterion(rowdist, order1)
#比较c1和c2发现TSP在哈密顿路径长度和梯度度量都更小

# #1.4b
# rowdist<-as.dist(1-cor(mat))
# coldist<-as.dist(1-cor(t(mat)))
# order1<-seriate(rowdist, "TSP")
# order2<-seriate(coldist, "TSP")
# ord1<-get_order(order1)
# ord2<-get_order(order2)
# reordmatr<-mat[rev(ord2),ord1]
# plot_ly(x=colnames(reordmatr), y=rownames(reordmatr), 
#         z=reordmatr, type="heatmap", colors =colorRamp(c("yellow", "red")))
# 
# criterion(rowdist, order1)




1.5
ord=c(8,9,4,6,2,5,1,3,7,10,11)
#ord=ord2
dims0=list()
for( i in 1:ncol(pe)){
  dims0[[i]]=list( label=colnames(pe)[ord[i]],
                   values=as.formula(paste("~",colnames(pe)[ord[i]])))
}
p <- pe %>%
  plot_ly(type = 'parcoords',
          line = list(color = ~as.numeric(Good.and.Services.Index)),

          dimensions = dims0
  )
p



#1.6

Ps=list()
nPlot=72

pe[O1,] %>%
  add_rownames( var = "group" ) %>%
  mutate_each(funs(rescale), -group) -> pe_radar

for (i in 1:nPlot){
  Ps[[i]] <- htmltools::tags$div(
    plot_ly(type = 'scatterpolar',  
            r=as.numeric(pe_radar[i,-1]),
            theta= colnames(pe_radar)[-1], 
            fill="toself")%>%
      layout(title=pe_radar$group[i]), style="width: 20%;")  # 4 plots per row
}
h <-htmltools::tags$div(style = "display: flex; flex-wrap: wrap", Ps)
htmltools::browsable(h)

#1.7

