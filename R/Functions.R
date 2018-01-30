#' Create Question-Level Proportion Judgment Plots
#'
#' This function lets you create plots of the bias, ignorance, and cross-unit variability on a per-question basis,
#' From results of a JAGS model.  If you want to print those on a common structure, feed the output into printQuestionParameterPDF
#' @param data A dataframe containing the fields: question, bias, biasQCTau, biasLow, biasHigh, and ditto for lambda and tau
#' @keywords proportionJudgments
#' @export
#' @seealso printQuestionParameterPDF
#' @examples
#' createQuestionLevelProportionJudgmentPlots()
createQuestionLevelProportionJudgmentPlots <- function(data, year="", countryVariabilityBars=FALSE){
  crossNationalBias <- ggplot(data
                                  , aes(x=bias, y=question, xmin=bias-1/sqrt(biasQCTau), xmax=bias+1/sqrt(biasQCTau))) +
    geom_errorbarh(height=0.5, aes(xmin=biasLow, xmax=biasHigh))+geom_point(size=3)+theme_bw()+ theme(text=element_text(size=20)) +
    scale_y_discrete(name=paste("Question ", year)) + scale_x_continuous(name="Bias")+
    coord_cartesian(xlim=c(-4,5))+geom_vline(xintercept = 0, color=rgb(0,.2,.5))
  if(countryVariabilityBars){
    crossNationalBias <- crossNationalBias + geom_errorbarh(height=0.2, color="gray")
  }


  crossNationalLambda <- ggplot(data
                                    , aes(x=lambda, y=question, xmin=lambda-1/sqrt(lambdaQCTau), xmax=lambda+1/sqrt(lambdaQCTau))) +
    geom_errorbarh(height=0.5, aes(xmin=lambdaLow, xmax=lambdaHigh))+geom_point(size=3)+theme_bw()+ theme(text=element_text(size=20)) +
    scale_y_discrete(paste("Question ", year)) + scale_x_continuous(name="Certainty")+
    coord_cartesian(xlim=c(0,1))+geom_vline(xintercept = 1, color=rgb(0,.2,.5))+theme(axis.title.y=element_blank(),
                                                                                      axis.text.y=element_blank(),
                                                                                      axis.ticks.y=element_blank())

  if(countryVariabilityBars){
    crossNationalLambda <- crossNationalLambda + geom_errorbarh(height=0.2, color="gray")
  }
  
  

  crossNationalTau <- ggplot(data
                                 , aes(x=tauQuestion, y=question, xmin=tauQuestion-1/sqrt(tauQCTau), xmax=tauQuestion+1/sqrt(tauQCTau))) +
    geom_errorbarh(height=0.5, aes(xmin=(tauLow), xmax=(tauHigh)))+geom_point(size=3)+theme_bw()+ theme(text=element_text(size=20)) +
    scale_y_discrete(paste("Question ", year)) + scale_x_continuous(name="Precision")+
    coord_cartesian(xlim=c(0,2.5))+geom_vline(xintercept = 0, color=rgb(0,.2,.5))+theme(axis.title.y=element_blank(),
                                                                                        axis.text.y=element_blank(),
                                                                                        axis.ticks.y=element_blank())

  if(countryVariabilityBars){
    crossNationalTau <- crossNationalTau + geom_errorbarh(height=0.2, color="gray")
  }
  crossNationalBiasTau <- ggplot(data
                                     , aes(x=1/sqrt(biasQCTau), y=question, xmin=1/sqrt(biasQCTau), xmax=1/sqrt(biasQCTau))) +
    geom_errorbarh(height=0.5, aes(xmin=(biasTauLowSD), xmax=(biasTauHighSD)))+geom_point(size=3)+theme_bw()+ theme(text=element_text(size=20)) +
    scale_y_discrete(paste("Question ", year)) + scale_x_continuous(name="Cross-National Variability")+
    coord_cartesian(xlim=c(0,2.5))+geom_vline(xintercept = 0, color=rgb(0,.2,.5))+theme(axis.title.y=element_blank(),
                                                                                        axis.text.y=element_blank(),
                                                                                        axis.ticks.y=element_blank())

  if(countryVariabilityBars){
    crossNationalBiasTau <- crossNationalBiasTau + geom_errorbarh(height=0.2, color="gray")
  }
  
  crossNationalResidual <- ggplot(data
                                      , aes(x=meanResid, y=question)) +
    geom_errorbarh(height=0.5, aes(xmin=residLow, xmax=residHigh))+geom_point(size=3)+theme_bw()+ theme(text=element_text(size=20)) +
    scale_y_discrete(paste("Question ", year)) + scale_x_continuous(name="Residual")+
    coord_cartesian(xlim=c(-2,2))+geom_vline(xintercept = 0, color=rgb(0,.2,.5))


  list(  bias=crossNationalBias
       , lambda=crossNationalLambda
       , tau=crossNationalTau
       , biasTau= crossNationalBiasTau
       , residual= crossNationalResidual)

}



#' Calculate the mean of a set of proportions, where the mean is done in log odds space.
#'
#' Utility function to calculate the proportion whose log-odds is the mean of all the items in the input vector.
#' @param proportions A vector of proportions
#' @export
#' @examples
#' logOddsMean()
logOddsMean <- function(proportions) {
  proportions[proportions==1] <- 0.995
  proportions[proportions==0] <- 1-0.995
  d <- exp(mean(log(proportions/(1-proportions)), na.rm=T))
  d/(d+1)
}

#' Calculate a t-test of a set of proportions, where the test is done in log odds space.
#'
#' Utility function to calculate the value of a within-t-test over the log-odds of the values.
#' @param proportions A vector of proportions
#' @export
#' @examples
#' logOddsMean()
logOddsT <- function(x,y) {
  x[x==1] <- 0.995
  x[x==0] <- 1-0.995
d <- t.test(log(x/(1-x))-log(y/(1-y)))$p.value
  d
}


#' Create World Maps of Bias Data
#'
#' This function lets you create plots of the bias across countries on a per-question basis
#' @param data A dataframe containing the fields: country, biasQC
#' @keywords proportionJudgments
#' @export
#' @examples
#' createChoropleth()
createChoropleth <- function(data){
  library(ggplot2)
  library(maps)
  library(maptools)
  library(rgeos)
  library(Cairo)
  #library(ggmap)
  library(scales)
  library(RColorBrewer)
  data$n <- 1:nrow(data)
  data$country = gsub("Great Britain", "United Kingdom"
                      , gsub("the US", "United States"
                             , gsub("South Korea", "Korea, Republic of")
                             , data$country))
  ISO3 <- wrld_simpl$ISO3
  names(ISO3) <- wrld_simpl$NAME
  data$id <- ISO3[as.character(data$country)]
  wrld_simpl.f <- fortify(wrld_simpl)
  mergedData <- merge(wrld_simpl.f, data, by="id", all.x=FALSE)
  mergedData <- mergedData[order(mergedData$order), ] 
  
  ggplot() +
    geom_polygon(data = mergedData, 
                 aes(x = long, y = lat, group = group, fill = biasQC), 
                 color = "black", size = 0.25)+
    coord_map(ylim=c(-50, 80))+theme_bw()+scale_fill_gradient2()+theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())
  
  
}


#' Create PDF's of parameters
#'
#' This function creates a pdf of your data. You must give it the output of a 
#' call to createQuestionLevelProportionJudgmentPlots. and a set of n items;
#' @param graphs the output of a call to createQuestionLevelProportionJudgmentPlots
#' @param file the name of the pdf file to use
#' @param items 3 items, of those returned by createQuestionLevelProportionJudgmentPlots, to plot. This is a vector
#'  of indices, with codes as follows: 1=crossNationalBias, 2=crossNationalLambda, 3=crossNationalTau, 4=biasTau, 5=residual
#' @keywords proportionJudgments
#' @export
#' @examples
#' printQuestionParameterPDF()
printQuestionParameterPDF <- function(graphs, file="parameters.pdf",  items=c(1:3)){
 if(length(items)>3){
   stop("You must pass no more than three items to print")
 } 
  n = length(items)
  pdf(file, width=12, height=4.5)
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1,n, widths=c(1, rep(0.75, n-1) ))))
  for(i in 1:n){
    print(graphs[[items[i]]], vp = vplayout(1, i))
  }
  dev.off()
}




#' Recode standard political demographic labels and return a subject file
#'
#' This function takes a tibbl containing 
#' political demographic data, and calculated pk, gender, ideology, etc.
#' and returns a subject-file with the standard calculated subcomponents.
#' @param data a tibble containing columns pk1-pk5, ideology, gender, and education
#' @keywords proportionJudgments, TIM
#' @export
#' @examples
#' createSubjectFileFromQualtricsDemographicData(data)
createSubjectFileFromQualtricsDemographicData <- function(data, dataProcessed="none"){
  
  veepList <- c("vicepresident", "vp", "vice-president", "vicepresidency", 
                "vice",  "viceprisedent", "47thvicepresident", "veep", "vicepresident?", "vice toddler", 
                "vicepresiden", "viceprisident","vicepresident." , "47th and current Vice President of the United States", "Exiting vice president","48thvicepresident", "48th and current Vice President of the United States", "48thandcurrentvicepresident", "VP", "vice presiddent"
                , "VICE PRESIDENT OF THE US" , "Vice President of the U.S."
  )
  
  vetoList <- c("2/3", "66", "67", "66%", "two thirds", 
                "A two-thirds majority vote in both the House of Representatives and in the Senate", 
                "Two-thirds", "67%",  "2/3s", "two-thirds vote", "Two thirds", 
                "2/3RDS" , ".66", "2 thirds", "66.67", "two-thirds",  "2/3rd", 
                "two third", "2/3rds", "Two Thirds",  "two thirds", "Two-Thirds","2 /3", 
                "2/3rds majority", "66.6", "A two-thirds majority", "3-Feb", "two-thirds majority", "Two-thirds", "2 out of 3 ", "two thirds", "66"
  ) #Hand-coded by inspection of all correct responses
  
  
  nIdeology <- as.numeric(sapply(
    gsub("Independent that leans", "Leaner", data$ideology), function(x){grep(x, 
                                                                                      c("Strong Democrat",  
                                                                                        "Not very strong Democrat",
                                                                                        "Leaner toward Democrat",
                                                                                        "Independent",                            
                                                                                        "Leaner toward Republican",
                                                                                        "Not very strong Republican",
                                                                                        "Strong Republican"             
                                                                                      )
                                                                                      
                                                                                      , fixed=TRUE)[1]}))
  
  
  data$gender[is.na(data$gender)] <- "No report"
  gender <- as.numeric(sapply(
    data$gender, function(x){grep(x,                                                      c("Female", "Male", "Other", "No Report")   , fixed=TRUE)}))-1
  
  
  
  
  
  data$education <- factor(data$education, levels=c( "less than high school credential"  , "high-school credential", "some post-high-school no bachelor", "bachelor"   , "graduate degree" ), ordered=T) 
  dataSubjects <- ddply(data, .(id), function(d){
    #bnt1Acc <- d$Q2=="30 out of 50 throws" #bnt1
    #bnt2Acc <- d$Q3=="25%" #bnt2
    #bnt3Acc <- d$Q4=="20 out of 70 throws" #bnt3
    #bnt4Acc <- d$Q5=="50%" #bnt4
    nIdeology <- as.numeric(sapply(
      gsub("Independent that leans", "Leaner", d$ideology), function(x){grep(x, 
                                                                             c("Strong Democrat",  
                                                                               "Not very strong Democrat",
                                                                               "Leaner toward Democrat",
                                                                               "Independent",                            
                                                                               "Leaner toward Republican",
                                                                               "Not very strong Republican",
                                                                               "Strong Republican"             
                                                                             )
                                                                             
                                                                             , fixed=TRUE)}))
    gender <- as.numeric(sapply(
      d$gender, function(x){grep(x,                                                      c("Female", "Male", "Other", "No Report"   )                                                     , fixed=TRUE)}))
    
    
    
    education <- factor(as.character(d$education), levels=c( "less than high school credential"  , "high-school credential", "some post-high-school no bachelor", "bachelor"   , "graduate degree" ), ordered=T) 
          
          
    data.frame(
      id = mean(d$id)
      , pk =  (gsub("oftheusa|oftheus|oftheunitedstates|ofusa|ofus|oftheu.s.|ofamerica|us", "",
                    tolower(gsub(" ", "", d$pk1))) %in% veepList)
      +  (d$pk2 %in% c("The Supreme Court"))
      +  (d$pk3 %in% vetoList)
      +  (d$pk4 %in% c("Republican Party"))
      +  (d$pk5 %in% c("Republican Party"))
      # , bnt = (bnt1Acc+bnt2Acc+bnt3Acc+bnt4Acc)/4
      , nIdeology = nIdeology
      , gender = gender
      , education = education
      , correlation = ifelse(max(dataProcessed=="none"), 0, unique(with(filter(dataProcessed, id==mean(d$id)), cor.test(question, response)$est)))
    )
  }
  )
  
   as.tibble(dataSubjects)
}

