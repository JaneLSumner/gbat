library(shiny)
library(rjson)
library(plyr)
library(stringr)
library(stringi)
library(NLP)
library(openNLP)
library(RWeka)
library(genderizeR)
library(gender)
library(wru)
library(tm)

shinyUI(fluidPage(
  tags$head(includeScript("googleanalytics.js")),
  
  titlePanel("Gender Balance Assessment Tool (GBAT)"),
  h4("Women are cited less often than men, and are also underrepresented in syllabi.
     Yet even well-meaning scholars may find that they have difficulty assessing how
     gender-balanced their bibliographies and syllabi really are. Counting is tedious and prone to human
     error, and scholars may not know the gender identities of all the authors they cite. This tool
     aims to help with that, by automating the process of evaluating the (probabilistic) gender 
     of each name
     and then providing an estimate of what percentage of the authors on a syllabus are women."),
  sidebarLayout(
    sidebarPanel(
      h3("Instructions:"),
      p("Copy or upload a .txt or .bib file of your syllabus or bibliography."),
      p("(If uploading produces an error, try copying to the text box.)"),
      p("Note: Gender prediction is based on given names. Syllabi without given names (or
        with only initials) will either produce no estimate or a wildly inaccurate one."),

      fileInput('file1', 'Choose .txt or .bib File',
                accept=c('text/csv', "application/x-bibtex",
                         'text/comma-separated-values,text/plain', 
                         '.csv',".bib")),
      textInput('text1',"Or paste text here",""),
      checkboxInput('racetoo',"Check race too?"),
      p("Note: Race predictions less accurate than gender predictions. Proceed with caution!"),
      submitButton(text="Go.")),
 
    mainPanel(

      h4("Your assigned readings are approximately",align="center"),
      h2(textOutput("contents"),align="center"),
      h4("percent woman-authored.",align="center"),
      conditionalPanel(condition="input.racetoo == true",
      br(),
      h4("Race breakdown (probabilistic)",align="center"),
      h4(textOutput("race"),align="center"),
      h4(textOutput("race2"),align="center"),
      br())
    ,
      p("For more information, please see:"),
      p("Sumner, Jane Lawrence. 'The Gender Balance Assessment Tool (GBAT): a web-based tool for estimating gender balance in syllabi and bibliographies.' PS: Political Science & Politics 51, no. 2 (2018): 396-400."),
       p("This tool use the gender prediction algorithm from:"),
      p("Kamil Wais (2015). genderizeR: Gender Prediction Based on First Names. R package version
        1.2.0. https://CRAN.R-project.org/package=genderizeR."), 

      p("and the race prediction algorithm from:"),
      p("Kabir Khanna and Kosuke Imai (2016). wru: Who Are you? Bayesian Prediction of
        Racial Category Using Surname and Geolocation. R package version 0.1.9, 
        https://cran.r-project.org/web/packages/wru/wru.pdf."),
      br(),
      p("Created and inconsistently maintained by:"),
      tags$a(href="http://www.janelawrencesumner.com", "Jane Lawrence Sumner"),
      p("Assistant Professor,
        Dept. of Political Science, University of Minnesota."),
      p("Contact email: jlsumner@umn.edu."),
      p("Last updated: December 10, 2019")
      
      )
      ))
      )
