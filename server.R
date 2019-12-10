library(shiny)
library(rsconnect)
library(rjson)
library(plyr)
library(stringr)
library(stringi)
library(NLP)
library(openNLP)
library(genderizeR)
library(wru)
library(gender)
library(tm)
library(maps)
library(pdftools)
library(RWeka)

# 
shinyServer(function(input, output) {
  
  parseSyllabus <- reactive({

    
    if(input$text1==""){
      oldw <- getOption("warn")
      options(warn = -1)
      
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
    syl <- paste0(scan(inFile$datapath, what = character(),quote="\""), collapse = " ")
    options(warn = oldw)
    } else{
      syl <- input$text1
    }
 
        bib <- grepl("timestamp =",syl)
    
    if(bib==TRUE){
      authors <- str_extract_all(syl,"(author = \\{)[A-z ',-]{1,100}(\\})")
      authors <- unlist(lapply(authors, function(x){
        gsub("author = \\{|\\}","",x)
      }))
      to_split <- authors[grepl("\\band\\b",authors)]
      authors <- authors[grep("\\band\\b",authors,invert=T)]
      splat <- str_trim(unlist(str_split(to_split,"\\band\\b")),"both")
      authors <- c(authors,splat)
      reoriented_names <- unlist(lapply(
        authors, function(x){
          if(str_detect(x,",")){
            z <- unlist(str_split(x,","))
            unlist(str_trim(paste(z[2],z[1],sep=" "),"both"))
          } else{
            str_trim(x,"both")
          }
        })
      )
      authors <- reoriented_names
      
    } else{
    
    syl_str <- as.String(syl)
    syl_str <- gsub("[0-9():/?!]"," ",syl_str)
    syl_str <- gsub("\n", " ",syl_str)
    syl_str <- iconv(syl_str, "latin1", "ASCII", sub="")
    stops <- stopwords("en")[grep("\\band\\b|\\ba\\b|\\bi\\b",stopwords("en"),invert=T)]
    stops <- c(stops,"i'm","i've","i'd","i'll")
    syl_str <- gsub(paste("\\b",paste(stops,collapse="\\b|\\b"),"\\b",sep=""),
                    " ",syl_str,ignore.case=T)
    titlewords <- c("Americ",
                    "Analy",
                      "Attitud",
                    "Basic",
                    "Book",
                    "Bayes",
                    "Behavi",
                    "Bureau",
                    "California",
                    "Michigan",
                    "Cambridge",
                    "Central",
                    "College",
                    "Communis",
                    "Congress",
                    "Control",
                    "Depart",
                    "Develop",
                    "E.U.",
                    "Eastern",
                    "Econo",
                    "Estimat",
                    "EU",
                    "Europe",
                    "Explor",
                    "Forthcoming",
                    "Harvard",
                    "Hypoth",
                    "Improv",
                    "Institut",
                    "Interact",
                    "International",
                    "Journal",
                    "Knowledg",
                    "Legislat",
                    "Little, Brown",
                    "Maximi",
                    "Maximu",
                    "Method",
                    "Multipl",
                    "New York",
                    "of Georgia",
                    "of Virginia",
                    "Paper",
                    "Polic",
                    "Politi",
                    "Press",
                    "Publi",
                    "Quarter",
                    "Reconsider",
                    "Relev",
                    "Research",
                    "Respon",
                    "Review",
                    "School",
                    "Science",
                    "Social",
                    "Societ",
                    "State",
                    "Statistic",
                    "Studies",
                    "Study",
                    "Transit",
                    "Typescript",
                    "U.K.",
                    "U.S.",
                    "Understand",
                    "Univers",
                    "Vote",
                    "Voting",
                    "Washington, DC",
                    "Yale",
                    "Oxford",
                    "Democ",
                    "Communis",
                    "Sociali",
                    "Capital",
                    "Who",
                    "What",
                    "When",
                    "Where",
                    "Why",
                    "Nation",
                    "Asia",
                    "Africa",
                    "Model",
                    "Problem",
                    "Judici",
                    "First",
                    "Second",
                    "Theor",
                    "Critic",
                    "Elect",
                    "Govern",
                    "Collect",
                    "Justice",
                    "Framework",
                    "Minorit",
                    "Majorit",
                    "Introduc",
                    "Essay",
                    "Global",
                    "Local","Foreign","Lobb","Counter",
                    "Public",
                    "Logic",
                    "Geograph",
                    "Behavio","Civil")
    syl_str <- gsub(paste(titlewords,collapse="|")," ",syl_str,ignore.case=T)
    syl_str <- gsub("\\band\\b",";",syl_str)
    

    
    patternand <- "[A-Z]{1}[A-z]{1,30}, [A-Z]{1}[A-z]{1,30}"
    oneinit <- "[A-Z]{1}[A-z]{1,30}, [A-Z]{1}[.]{1}"
    twoinit <- "[A-Z]{1}[A-z]{1,30}, [A-Z]{1}[.]{1}[ ]{0,1}[A-Z]{1}[.]{1}"
    
    givensurname2 <- "[A-Z]{1}[.]{1}[ ]{0,1}[A-Z]{1}[.]{1} [A-Z]{1}[A-z]{1,30}"
    givensurname1 <- "[A-Z]{1}[.]{1} [A-Z]{1}[A-z]{1,30}"
    givensurname3 <- "[A-Z]{1}[A-z]{2,15} [A-Z]{1}[.]{1} [A-Z]{1}[A-z]{1,30}"
    givensurname4 <- "[A-Z]{1}[A-z]{2,15} [A-Z]{1}[.]{1}[A-Z]{1}[.]{1} [A-Z]{1}[A-z]{1,30}"
    givensurname <- paste(c(givensurname1,givensurname2,givensurname3,
                            givensurname4),collapse="|")
    
    hyphens <- "[A-Z]{1}[A-z]{1,30}[-][A-Z]{1}[A-z]{1,30}, [A-Z]{1}[A-z]{1,30}"
    hyphensoneinit <- "[A-Z]{1}[A-z]{1,30}[-][A-Z]{1}[A-z]{1,30}, [A-Z]{1}[.]{1}"
    hyphenstwoinit <- "[A-Z]{1}[A-z]{1,30}[-][A-Z]{1}[A-z]{1,30}, [A-Z]{1}[.]{1}[ ]{0,1}[A-Z]{1}[.]{1}"
    
    search_terms <- paste(paste(patternand,hyphens,twoinit,oneinit,
                                hyphensoneinit,hyphenstwoinit,sep="|"),
                          paste(c(givensurname),collapse="|"),sep="|")
     bw <- unlist(str_extract_all(syl_str,search_terms))

    bw <- bw[!is.na(str_match(bw," "))]
    syl_str <- str_replace_all(syl_str,search_terms,"") 

        reoriented_names <- unlist(lapply(
      bw, function(x){
        if(str_detect(x,",")){
          z <- unlist(str_split(x,","))
          unlist(str_trim(paste(z[2],z[1],sep=" "),"both"))
        } else{
          str_trim(x,"both")
        }
      })
    )


authors <- reoriented_names
        

authors <- as.character(as.factor(authors)[stri_count_words(authors)>1])
to_split <- authors[grepl(paste(" ","\\band\\b|&"," ",sep=""),authors)]
authors <- authors[grepl(paste(" ","\\band\\b|&"," ",sep=""),authors)==0]
splat <- str_trim(unlist(str_split(to_split,"\\band\\b|&")),"both")
authors <- c(authors,splat,reoriented_names)  ## initnames
}


surnames <- word(authors,(str_count(authors," ")+1))
first_names <- word(authors,1)


denominator <- length(first_names)
all_together <- data.frame(first_names=first_names,
                           surnames=surnames)

all_together <- all_together[!is.na(all_together$first_names),]
all_together$first_names <- gsub("[@#$-&0-9]","",all_together$first_names)
all_together <- all_together[which(all_together$first_names!=""),]

inits <- "[A-Z][.]"
probably_geog <- "[A-Z]{2}"
all_together$first_names <- gsub(paste(inits,probably_geog,sep="|"),"",all_together$first_names)
all_together <- all_together[nchar(all_together$first_names)>1,]
all_together <- all_together[nchar(as.character(all_together$surnames))>1,]

first_names <- str_trim(as.character(all_together$first_names),"both")
surnames <- str_trim(as.character(all_together$surnames),"both")


namelookup <- function(name){
  dat <- findGivenNames(name,apikey="48ae3f781c053ac36f50cc06c5554f75")
  gender <- dat$gender
  if(length(dat$gender>0)){
    probability <- as.numeric(dat$probability)
    pr_female <- ifelse(gender=="female",probability,1-probability)
  } else{
    pr_female <- NA
  }
  return(pr_female)
}

racelookup <- function(name){
  dat <- predict_race(data.frame(surname=name),surname.only=T)
  if(dat$pred.whi==.621 & dat$pred.bla==.132 & dat$pred.his==0.174 &
       dat$pred.asi==0.054 & dat$pred.oth==0.019){
    dat$pred.whi <- NA
    dat$pred.bla <- NA
    dat$pred.his <- NA
    dat$pred.asi <- NA
    dat$pred.oth <- NA
  }
  return(dat[,2:ncol(dat)])
}

if(length(first_names)>0){
  
  probs <- NULL
  raceprobs <- NULL
  withProgress(message="Assessing ... ",detail="This may take a minute.",value = 0, {
  n <- length(first_names)
  
  for(i in 1:n){
        incProgress(1/n, detail = paste("Author",i))
    if(first_names[i]!=""){
      prob <- invisible(namelookup(first_names[i]))
      prob <- mean(prob,na.rm=T)
      if(!is.na(prob)){
        raceprob <- racelookup(gsub(" ","",surnames[i]))
      } else{
        raceprob <- rep(NA,5)
        names(raceprob) <- c("pred.whi","pred.bla","pred.his","pred.asi","pred.oth")
      }
    } else{
      prob <- NA
      raceprob <- rep(NA,5)
      names(raceprob) <- c("pred.whi","pred.bla","pred.his","pred.asi","pred.oth")
    }
    probs <- c(probs,prob)
    raceprobs <- rbind(raceprobs,raceprob)
  } })
  total <- round(sum(probs,na.rm=T)/sum(!is.na(probs))*100,2)
  racetotal <- round(apply(raceprobs,2,sum,na.rm=T)/nrow(na.omit(raceprobs))*100,2)
  pred_gender <- round(sum(!is.na(probs))/denominator*100,2)
  pred_race <- round(sum(!is.na(raceprobs[,1]))/denominator*100,2)

  
} else{
  total <- NA
  racetotal <- rep(NA,5)
  n <- NA
  pred_gender <- 0
  pred_race <- 0
}
# })
return(list(total=total,racetotal=racetotal,citations=n,pred_gender=pred_gender,
            pred_race=pred_race))      
  })

  

  
  # 
  #   
  output$race <- renderText({
    paste(parseSyllabus()$racetotal[4],"% Asian, ",
          parseSyllabus()$racetotal[2],"% Black, ",
          parseSyllabus()$racetotal[3],"% Hispanic, ",sep="")
  })
  output$race2 <- renderText({
    paste(parseSyllabus()$racetotal[5],"% Other, ",
          parseSyllabus()$racetotal[1],"% White",
          sep="")
  })

    output$contents <- renderText({
    parseSyllabus()$total
  
      
  })
  
})