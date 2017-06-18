options(bitmapType='cairo')
#options(bitmapType='cairo-png')
#options(shiny.usecairo = FALSE)

library(shiny)
library(ggplot2)
library(Cairo)
library(wordcloud2)

dfpath=getwd()
df_ind <- read.csv(file.path(dfpath,"zhongchou_index.out"),header = T, sep = '\t',fileEncoding="utf8")
df_sku <- read.csv(file.path(dfpath,"zhongchou_skus.out"),header = T, sep = '\t',fileEncoding="utf8")
df_wrd <- read.csv(file.path(dfpath,"zhongchou_words.out"),header = T, sep = '\t',fileEncoding="utf8")
df_skuind <- read.csv(file.path(dfpath,"zhongchou_skus_index.out"),header = T,sep = '\t',fileEncoding="utf8")
#df_ind$项目URL=df_ind$项目id
df_ind[,6]=round(df_ind[,6],2)
df_ind[,7]=round(df_ind[,7],2)
df_ind[,10]=round(df_ind[,10],2)
df_skuind[,4]=round(df_skuind[,4],2)
df_skuind[,6]=round(df_skuind[,6],2)
df_skuind[,7]=round(df_skuind[,7],2)
df_skuind[,8]=round(df_skuind[,8],2)
df_skuind[,11]=round(df_skuind[,11],2)
df_ind$关键词=paste(substr(df_ind$关键词,1,50),'...')

#if (nrow(hw)>nsample)
#  hw=hw[sample(1:nrow(hw), nsample, replace=F),] 

# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  # Generate a plot of the data. Also uses the inputs to build
  # the plot label. Note that the dependencies on both the inputs
  # and the data reactive expression are both tracked, and
  # all expressions are called in the sequence implied by the
  # dependency graph
  output$tableplot0<- renderTable({
    df=df_sku[df_sku$项目id==as.numeric(input$zid),]
    df=as.data.frame(table(df$项目匹配词))
    df$n=as.numeric(lapply(strsplit(paste(df$Var1),','),length))
    df=df[df$Freq>0,]
    df=df[order(df$n,decreasing = F),]
    ndf=nrow(df) 
    if (ndf>20)
       df=df[(ndf-20):ndf,]
    df[order(df$n,decreasing = T),]
  })

  output$plot0 <- renderPlot({
    df=df_sku[df_sku$项目id==as.numeric(input$zid),]
    df=as.data.frame(table(df$项目匹配词))
    df$n=as.numeric(lapply(strsplit(paste(df$Var1),','),length))
    df=df[df$Freq>0,]
    df=df[order(df$n,decreasing = F),]
    ndf=nrow(df) 
    if (ndf>20)
       df=df[(ndf-20):ndf,]
    barplot(df$n,names.arg=df$Var1,horiz=T,
            axisnames=0)
  })

  output$plot10 <- renderPlot({
    df=df_sku[df_sku$项目id==as.numeric(input$zid),]
    bins=c(0,seq(100,1500,100),max(df$SKU价格)+1)
    h=hist(df$SKU价格,breaks=bins)
    barplot(h$count,names.arg=h$mids+50,horiz=F,
       main='SKU Price Segment sta')
  })
  
  output$plot11 <- renderWordcloud2({
    df=df_wrd[df_wrd$项目id==as.numeric(input$zid),]
    df=df[df$指数类型=='zhi',]
    #wordcloud2(c('abc','777','zgfad','我的'),c(3,8,3,11))
    wordcloud2(data.frame(word=df$关键词,freq=df$权重))
    #hist(rnorm(20))
  })
  
  output$plot12 <- renderWordcloud2({
    df=df_wrd[df_wrd$项目id==as.numeric(input$zid),]
    df=df[df$指数类型=='xin',]
    #wordcloud2(data.frame(word=c('abc','777','cc','ni的'),freq=c(3,8,3,11)))
    wordcloud2(data.frame(word=df$关键词,freq=df$权重))
    #hist(rnorm(20))
  })
  
  # Generate a summary of the data
  output$summary <- renderPrint({
    df=df_sku[df_sku$项目id==as.numeric(input$zid),]
    summary(df)
    #summary(data())
  })
  
  # Generate an HTML table view of the data

  output$table21 <- renderTable({
    df_ind$项目URL=df_ind$项目id
    df_ind[df_ind$项目id==as.numeric(input$zid),]
  })

  output$table22 <- renderTable({
    df_ind$项目URL=df_ind$项目id
    df_ind[df_ind$项目id==as.numeric(input$zid),]
  })

  output$table23 <- renderTable({
    df_ind$项目URL=df_ind$项目id
    df_ind[df_ind$项目id==as.numeric(input$zid),]
  })

  output$table3 <- renderTable({
    df=df_sku[df_sku$项目id==as.numeric(input$zid),]
    df=df[order(df$相似度,decreasing =T),]
    df$SKUurl=df$SKUid
    df[1:100,]
  })

  output$table33 <- renderDataTable({
    df=df_skuind[df_skuind$项目id==as.numeric(input$zid),]
    df=df[order(df$相似度,decreasing =T),]
    df$URL和名称=paste(df$SKUurl,df$SKU名称)
    df=df[1:min(100,nrow(df)),c(-1,-2,-3,-12)]
    df[,c(9,1:8)]
    }, 
    options = list(bSortClasses = TRUE,columns.width='50%')
    #options = list(bSortClasses = TRUE,autoWidth=TRUE)
  )

  output$table441 <- renderDataTable({
    #df_ind$综合指数=paste(ceiling((df_ind[,6]+df_ind[,7]+df_ind[,8]+df_ind[,9])/4.0*10/2.0),'颗星')
    df_ind$综合指数=paste(ceiling((df_ind[,6]+df_ind[,7]+df_ind[,8]+df_ind[,9])/4.0*10),'颗星')
    df_ind$URL和名称=paste(substr(df_ind$项目URL,8,100),df_ind$项目名称)
    df_ind[,c(12,6,7,8,9,10,11)]
    }, 
    options = list(bSortClasses = TRUE, bFilter = FALSE)
  )
  output$table442 <- renderDataTable({
    #data.frame(URL核心词=paste(df_ind$项目URL,df_ind$核心词),关键词=df_ind$关键词)
    df_ind[,c(4,5)]
    }, 
    options = list(bSortClasses = TRUE, bFilter = FALSE,bPaginate = FALSE)
  )

  output$table4 <- renderTable({
    df_ind
  })

  # index keywords textoutput
  output$text11 <- renderPrint({
   df=df_wrd[df_wrd$项目id==as.numeric(input$zid),]
   df=df[df$指数类型=='zhi',]
   df=df[order(df$权重,decreasing = T),]
   ndf=nrow(df)
   if (ndf>100)
      df=df[1:100,]
   #as.vector(df$关键词)
   paste(df$关键词,collapse=',')
  })

  output$text12 <- renderPrint({
   df=df_wrd[df_wrd$项目id==as.numeric(input$zid),]
   df=df[df$指数类型=='ozhi',]
   df=df[order(df$权重,decreasing = T),]
   ndf=nrow(df)
   if (ndf>100)
      df=df[1:100,]
   #as.vector(df$关键词)
   paste(df$关键词,collapse=',')
  })

  output$text21 <- renderPrint({
   df=df_wrd[df_wrd$项目id==as.numeric(input$zid),]
   df=df[df$指数类型=='shi',]
   df=df[order(df$权重,decreasing = T),]
   ndf=nrow(df)
   if (ndf>100)
      df=df[1:100,]
   #as.vector(df$关键词)
   paste(df$关键词,collapse=',')
  })

  output$text22 <- renderPrint({
   df=df_wrd[df_wrd$项目id==as.numeric(input$zid),]
   df=df[df$指数类型=='oshi',]
   df=df[order(df$权重,decreasing = T),]
   ndf=nrow(df)
   if (ndf>100)
      df=df[1:100,]
   #as.vector(df$关键词)
   paste(df$关键词,collapse=',')
  })

  output$image1 <- renderImage({
      return(list(
        src = "./pic/logo_index.PNG",
        contentType = "image/png",
        alt = "test png Face"
      ))
  }, deleteFile = FALSE) 

  # output upload txt
  output$upcontent <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$zfile
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath,sep='|',header=F)
    #, header=input$header, sep=input$sep,quote=input$quote)
  })
  
})
