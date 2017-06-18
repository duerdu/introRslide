library(shiny)
library(markdown)
library(wordcloud2)

dfpath=getwd()
df_ind <- read.csv(file.path(dfpath,"zhongchou_index.out"),header = T, sep = '\t',fileEncoding="utf8")

shinyUI(fluidPage(
    
  # Application title
  title = "Hello zhongchou index!",
  fluidRow(
    column(width = 3,
      imageOutput("image1", height = 30)
    ),
    column(width = 3,
      br(),
	  #看指数 选项目 ~
      p(strong(" "))
    ),
    column(width = 2, offset=1,
      "上传项目关键词:" 
    ),
    column(width = 3,
      fileInput('zfile', ' ', accept=c('text/csv', 
                'text/comma-separated-values,text/plain', '.csv'))
    )
  ),

 tabsetPanel(type = "tabs",
  tabPanel("指数概览",
    p(strong("众筹项目指数列表:")),
    fluidRow(
      column(width = 7,
        dataTableOutput("table441")
      ),
      column(width = 5,
        dataTableOutput("table442")
      )
    ),
    #
    fluidRow(
      column(width = 2,
       wellPanel( radioButtons("zid", "选中查看相似SKU:",
                  c("Fitband睡眠手环" = "578",
                   "ERI运动手环" = "206",
                   "逗bi开心麻花" = "740",
                   "Sense-U爱+" = "878",
                   "开心熊宝智能音箱" = "784",
                   "OXA智能耳机" = "782")))
                   #paste(df_ind[1,3])=df_ind[1,1] ))
      ),
      column(width = 10,
        wellPanel(
        p("创新指数：取值0～1，越大创新度越高；京东在售相似商品越多(依据匹配的关键词数判断)，创新度越低。"),
        p("热销指数：取值0～1，越大商城的类似产品越热销；依据商城在售相似商品的销量来评估。"),
        p("智能指数：取值0～1，越大表示越“智能”；根据商城智能设备相关商品的描述建个词库，项目出现这些词越多越智能。"),
        p("实用指数：取值0～1，越大表示越“实用”；根据商城生活日用相关商品的描述建个词库，项目出现这些词越多越实用。"),
        #p("智能指数：取值0～1，越大表示越“智能”；根据商城智能设备相关SKU建智能关键词库，根据项目词的匹配度判断。"),
        #p("实用指数：取值0～1，越大表示越“实用”；根据商城日用外设相关SKU建实用关键词库，根据项目词的匹配度判断。"),
        p("综合指数：前述4个指数的算术平均（*10后取整），综合指数越高，越建议考虑项目入围,也可单项指数评估。"),
        p("价格参考：依据商城在售相似商品的价格，给出在售商品的价格参考，价格汇总根据相似度加权。")
        )
      )
    ),
    #
    p("SKU明细最相似的前100个:"),
    wellPanel(dataTableOutput("table33"))
  ),
  ####
        tabPanel("指数解析", 
                 #tableOutput("table22"),
                 p(strong("同时匹配关键词最多的组合20个:")),
                 splitLayout(
                       tableOutput("tableplot0"),
                       plotOutput("plot0",height="700px")
                 ),

                 p(strong("SKU价格区段统计:")),
                 plotOutput("plot10"),

                 p(strong("智能指数关键词:  左侧为匹配项目的关键词, 右侧为匹配SKU的其他关键词")),
                 splitLayout(
                       verbatimTextOutput("text11"),
                       verbatimTextOutput("text12")
                 ),
                 p(strong("实用指数关键词:  左侧为匹配项目的关键词, 右侧为匹配SKU的其他关键词")),
                 splitLayout(
                       verbatimTextOutput("text21"),
                       verbatimTextOutput("text22")
                 ),

                 br(),
                 p(strong("SKU匹配的统计概览:")),
                 verbatimTextOutput("summary"),

                 splitLayout(
                       wordcloud2Output("plot11"),
                       wordcloud2Output("plot12")
                 )
        ), 

                 
        tabPanel("上传内容", tableOutput("upcontent")),
        tabPanel("使用说明",
                  br(),
                  p("1.上传文件:内容为众筹项目关键词,tab分割.(not support yet)"),
                  br(),
                  p("2.在左侧列表选择相应项目,enjoy"),
                  br(),
                  p("3.指数计算公式（示例）："),
                  p("创新指数：0.5*max(si)+0.3*avg(si)+0.2*min(ceil(log10(N(si))),5)/5，si为项目与sku的相似度，si=相同关键词数/众筹项目关键词数"),
                  p("热销指数：0.5*max(ri)+0.5*avg(si)，si同上，ri为sku单品的热销度，ri=sqrt(si)*min(ceil(log10(N(qtty))),5)/5"),
                  p("智能指数：sum(关键词出现词数*关键词权重)，关键词从商城智能设备相关品类的sku提取，权重为前述si"),
                  p("实用指数：sum(关键词出现词数*关键词权重), 关键词从商城日常百货相关品类的sku提取，权重为前述si"),
                  p("综合指数：前4个指数的算术平均，取整"),
                  p("价格参考：sum(pi*si/(sum(si)))，pi为相似的sku单价，si同上，为项目与sku的相似度")

        )

 ),
    fluid=TRUE

  )
)
