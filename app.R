library(shinydashboard)
library(shiny)
library(DT)
library(dplyr)
library(officer)
#library(data.table)
library(tm)
library(SnowballC)
library(wordcloud)
library(ggplot2)

review_data<-read.csv("1429_1.csv")
print(names(review_data))
#content <- docx_summary(doc)
library(docxtractr)
#real_world <- read_docx(flist[1])
#tbls <- docx_extract_all(real_world)
#tbls1<-content[content$content_type == "paragraph"&content$text != ""&content$text %like% ':',c(4)]

library(stringr)
#hData = str_split(tbls1,":")
#hData
# cheader<-NULL
# for (i in 1:length(hData)) {
#   print(hData[[i]][1])
#   cheader<-rbind(cheader, hData[[i]][1])
#   
#   
# }
dbHeader<-dashboardHeader(titleWidth = "1px")
headerhtml<-withTags({
   h2(style="text-align: center;","Customer Review Analysis")
})
dbHeader$children[[3]]$children[[3]]<- headerhtml
ui <- dashboardPage(
  #dashboardHeader(title = "Consumer Review Analytics"),
   dbHeader,
 dashboardSidebar(disable = T),#textInput("text", "Text")),
  dashboardBody(
   
      tabBox(
        title = "  ",width="100%",side="left",
        tabPanel("Data Analyzer   ", width="100%",
        fluidRow(
   # valueBox(100, "Basic example"),
           box(width=6, radioButtons("cat","Select Review by",c("brand","categories","manufacturer","reviews.rating","reviews.title"),selected ="categories",inline = T)),    
   box(width=6, selectInput("files","select an option",unique(review_data$categories))),
    #htmlOutput("textdata"),
    #uiOutput("ui"),
   box(width=12,div(style = 'overflow-x: scroll;overflow-y: scroll',height="200px",
    dataTableOutput("dtbl",height = "200px")),solidHeader = T, collapsible = T, title = "Filtered Review data", status = "primary")
)),
tabPanel("Word Analyzer    ", width="100%",
         fluidRow(
            box(width=6,plotOutput("plot1",width="100%",height = "500px"),solidHeader = T, collapsible = T, title = "WordCloud", status = "primary"),box(width=6,plotOutput("plot2",width="100%",height = "500px"),solidHeader = T, collapsible = T, title = "Top Word Frequency", status = "primary"),
                #plotlyOutput("plot2",width="100%",height = "500px"),
                box(width=12,box(width=12,status="primary", uiOutput("xyx"),div(style='overflow-y: scroll')),solidHeader = T, collapsible = T, title = "Filter by Word", status = "primary",
                       box(width=12,div(style = 'overflow-x: scroll;overflow-y: scroll',height="300px", DT::dataTableOutput("ptable"),solidHeader = T, collapsible = T, title = "Filtered Word data", status = "primary")))
                       
         
         
            
         ))



)))
server <- function(input, output,session) {
  pdf_content<-NULL
  data_word<-NULL
  rdata<-NULL
  val<-reactiveValues(nwords=NULL)
observeEvent(input$cat,{
   updateSelectInput(session,"files","Select an option:",unique(review_data[,c(input$cat)]))
})

observeEvent(input$files,{
   output$dtbl<-renderDataTable({
      # print(names(review_data))
      rdata<<-review_data[review_data[,c(input$cat)]==input$files,c(-1,-2,-3,-6,-8,-9,-10,-13,-16,-14,-20,-21)]
      print(names(rdata))
      # txt<-pdf_text(input$files)
      # pdf_content<<- data.table(data.frame(txt))
      # pdf_content
      datatable(rdata[,c(-7)],options=list(pageLength=5))
   })
})
  output$plot1<-renderPlot({
     
     #review_data1<-review_data[review_data$categories==input$cat,]
     #rdata<-review_data[review_data$categories==input$files,c(17)]
     rdata<-review_data[review_data[,c(input$cat)]==input$files,c(17)]
    # txt<-pdf_text(input$files)
     #pdf_content<<- data.frame(txt)
     reviewcorpus<-VCorpus(VectorSource(rdata))
     toSpace<- content_transformer(function(x,pattern){return(gsub(pattern," ", x))})
     reviewcorpus<-tm_map(reviewcorpus,toSpace,",")
     reviewcorpus<-tm_map(reviewcorpus,toSpace,";")
     reviewcorpus<-tm_map(reviewcorpus,toSpace,":")
     reviewcorpus<-tm_map(reviewcorpus,toSpace,"\\.")
     reviewcorpus<-tm_map(reviewcorpus,toSpace,"\"")
     reviewcorpus<-tm_map(reviewcorpus,toSpace,"\\)")
     
     reviewcorpus<-tm_map(reviewcorpus,content_transformer(tolower))
     reviewcorpus<-tm_map(reviewcorpus,stripWhitespace)
     reviewcorpus<-tm_map(reviewcorpus,removeWords,stopwords("english"))
     tdm<-TermDocumentMatrix(reviewcorpus)
     m=as.matrix(tdm)
     v=sort(rowSums(m!=0),decreasing=T)
     v<-subset(v,v>0)
     data_word<<-data.frame(wordt=names(v),freq=v)
     val$nwords<-data_word
     print(val$words)
     output$plot2<-renderPlot({
        xx<-input$files
        yy<-data_word
        # print(yy)
        if(!is.null(data_word)){
           if(nrow(data_word)>0){
              ggplot(data=head(data_word), aes(x=wordt, y=freq)) +
                 geom_bar(stat="identity", fill="steelblue")+
                 geom_text(aes(label=freq), vjust=1.6, color="white", size=3.5)+
                 theme_minimal()
           }}
     })
     print("inwc")
     print(head(val$nwords))
     #updateSelectInput(session,"pdfwords","select a word to display the rows:",as.list(rownames(data_word)))
     #updateSelectInput(session,"selword",as.list(rownames(data_word)))
     # wordcloud::wordcloud(words = d$wordt,freq = d$freq,min.freq = 10,max.words = 50,random.order = F,rot.per = 0.4
     #                      #,colors = brewer.pal(8,"Dark2")
     # ) 
     # 
     set.seed(9876)
     wordcloud(reviewcorpus, max.words =100,min.freq=3,scale=c(4,.5), 
               random.order = FALSE,rot.per=.5,vfont=c("sans serif","plain"),colors=palette())
     
     
  })
 
  output$ptable<-renderDataTable({
     xx<-input$files
   x<-input$selwords
   rdata<<-review_data[review_data[,c(input$cat)]==input$files,c(-1,-2,-3,-6,-8,-9,-10,-13,-16,-14,-20,-21)]
    datatable(rdata[,c(1,3,6,7)],options = list(searchHighlight =T,search=list(search=x)))
     
  })
  
  output$xyx<-renderUI({
     xx<-input$files
     print("render UI")
     print("Print in pdf dropdown")
     #selectInput("selwords","select a word to display the rows:",unique(review_data$name))
     selectInput("selwords","select a word to display the rows:",as.list(rownames(data_word)))
     
  })
  observe({
     xx<-input$files
     rdata<<-rdata
    # data_word<<-val$nwords
     
  })
  

  


 
 
}
shinyApp(ui, server)
