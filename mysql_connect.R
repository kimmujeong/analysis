install.packages("RMySQL")
library(RMySQL)
con <- dbConnect(MySQL(), dbname = "world", user = "root", password = "0000")
sql<-'select * from perf'
perf<-dbGetQuery(con,sql)
dbDisconnect(con)

