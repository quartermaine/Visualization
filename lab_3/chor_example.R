rds<-readRDS("gadm36_GBR_2_sf.rds")
df<-read.csv("GBcities.csv")

rownames(df)=df$name
rds$Price=df[rds$NAME_2, "Price"]
#Data for some regions absent, setting to 0
rds$Price[is.na(rds$Price)]=0

#plotly
jj<-plot_ly()%>%add_sf(data=rds, split=~NAME_2, color=~Price, showlegend=F, alpha=1)

jj