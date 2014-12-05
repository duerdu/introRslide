
# Sys.getlocale("LC_ALL")
# Sys.setlocale("LC_ALL",'CHS')

#install.packages('shiny')
#install.packages('ggplot2')
#install.packages('Cairo')
#install.packages('wordcloud')

library(shiny)

runExample("01_hello")
runExample("02_text")
runExample("03_reactivity")


#install.packages("Rwordseg", repos = "http://R-Forge.R-project.org", type = "source")
library('Rwordseg')


runApp('../../demoRapply/zhong01/')
#invalid multibyte string 4



