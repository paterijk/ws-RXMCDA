##########################################
# Usage:
# R --slave --vanilla --file=generate.R --args "[inDirectory]" "[outDirectory]" "[criteriaFile]" "[alternativesFile]" "[performanceTableFile]" "[messagesFile]" "[meansFile]" "[standardDeviationsFile]" "[variancesFile]" "[minsFile]" [maxesFile]" "[correlationsFile]" "[mediansFile]"
# Example: 
# R --slave --vanilla --file=generate.R --args "${PWD}/in" "${PWD}/out" "criteria.xml" "alternatives.xml" "performanceTable.xml" "messages.xml" "means.xml" "standardDeviations.xml" "variances.xml" "mins.xml" "maxes.xml" "correlation.xml" "medians.xml"
##########################################

rm(list=ls())

##########################################
# Start by loading the RXMCDA library which allows to read and write XMCDA files
##########################################

library(RXMCDA)

errFile<-NULL
errData<-NULL
errCalc<-NULL
infoCalc <- NULL

execFlag<-FALSE

##########################################
# Get the in and out directories from the arguments
# as well as the in and out files
##########################################

inDirectory <- commandArgs(trailingOnly=TRUE)[1]
outDirectory <- commandArgs(trailingOnly=TRUE)[2]
criteriaFile <- commandArgs(trailingOnly=TRUE)[3]
alternativesFile <- commandArgs(trailingOnly=TRUE)[4]
performanceTableFile <- commandArgs(trailingOnly=TRUE)[5]
messagesFile <- commandArgs(trailingOnly=TRUE)[6]
meansFile <- commandArgs(trailingOnly=TRUE)[7]
standardDeviationsFile <- commandArgs(trailingOnly=TRUE)[8]
variancesFile <- commandArgs(trailingOnly=TRUE)[9]
minsFile <- commandArgs(trailingOnly=TRUE)[10]
maxesFile <- commandArgs(trailingOnly=TRUE)[11]
correlationsFile  <- commandArgs(trailingOnly=TRUE)[12]
mediansFile <- commandArgs(trailingOnly=TRUE)[13]

##########################################
# Set the working directory as the "in" directory
##########################################

setwd(inDirectory)

##########################################
# Try to load the mandatory input files.
# If they contain XML, the XML tree is stored in a variable.
# Then the input files are checked against the XMCDA schema.
# If something goes wrong, everything stops at this stage.
#
# Load the mandatory file containing the criteria ids (criteria.xml), the 
# criteria means (means.xml), the criteria standard deviations (standardDeviations.xml),
# the bayesian network (graph.xml)
# and the number of alternatives to generate (parameters.xml)
##########################################

tmpErr<-try(
{
  treeCriteria<-xmlTreeParse(criteriaFile,useInternalNodes=TRUE)
}
)
if (inherits(tmpErr, 'try-error')){
  errFile<-"Cannot read the criteria file."
}
if (is.null(errFile)){
  tmpErr<-try(
{
  treeAlternatives<-xmlTreeParse(alternativesFile,useInternalNodes=TRUE)
}
  )
  if (inherits(tmpErr, 'try-error')){
    errFile<-"Cannot read the alternatives file."
  }
}	
if (is.null(errFile)){
  tmpErr<-try(
{
  treePerformanceTable<-xmlTreeParse(performanceTableFile,useInternalNodes=TRUE)
}
  )
  if (inherits(tmpErr, 'try-error')){
    errFile<-"Cannot read the performance table file."
  }
}

if (is.null(errFile)){
  
  # mandatory files were correctly loaded
  # now we check if files are valid according to xsd
  
  if (checkXSD(treeCriteria)==0)
  {
    errFile<-"Criteria file is not XMCDA valid."	
  }
  if (is.null(errFile)){
    if (checkXSD(treeAlternatives)==0)
    {
      errFile<-"Alternatives file is not XMCDA valid."	
    }
  }
  if (is.null(errFile)){
    if (checkXSD(treePerformanceTable)==0)
    {
      errFile<-"Performance table file is not XMCDA valid."	
    }
  }	
}

##########################################
# Extract data from the XMCDA trees.
# If everything went right up to here, we extract the data from the XMCDA trees via the functions of the RXMCDA library.
#
# In this example, we extract the alternatives' IDs and the number of alternatives to be considered.
# If the optional "seed" file is loaded, we also read the value of the seed.
##########################################

if (is.null(errFile)){
  
  # files were correctly loaded and are valid according to xsd
  
  errData<-NULL
  
  flag<-TRUE
  
  
  criteriaIDs<-getCriteriaIDs(treeCriteria)
  
  
  if (flag){
    if (criteriaIDs$status == "OK")
    {
      critIDs<-criteriaIDs[[1]]
    }
    else 
    {
      errData<-criteriaIDs$status
      flag<-FALSE
    } 	
  }
  
  if (flag){
    alternativesIDs <- getAlternativesIDs(treeAlternatives)
    if (alternativesIDs$status == "OK") 
    {
      altIDs<-alternativesIDs[[1]]
    }
    else
    {
      errData <- alternativesIDs$status
      flag <- FALSE
    }
  }
  
  if (flag){
    performanceTables <- getPerformanceTables(treePerformanceTable, altIDs, critIDs)
    if (performanceTables$status == "OK") 
    {
      performanceTable<-performanceTables[[1]]
    }
    else
    {
      errData <- performanceTables$status
      flag <- FALSE
    }
  }
  
  ##########################################
  # Run the algorithm.
  # If everything went right up to here, run the calculations.
  ##########################################
  
  if (is.null(errData))
  {
    # all data elements have been correctly extracted from the xml trees
    # we can proceed to the calculations now
    
    tmpErr<-try(
{
    critMins <- c()
    for (i in 1:length(critIDs)){
      critMins <- rbind(critMins, c(i,min(performanceTable[,i])))
    }
    critMaxes <- c()
    for (i in 1:length(critIDs)){
      critMaxes <- rbind(critMaxes, c(i,max(performanceTable[,i])))
    }
    critMedians <- c()
    for (i in 1:length(critIDs)){
      critMedians <- rbind(critMedians, c(i,median(performanceTable[,i])))
    }
    critMeans <- c()
    for (i in 1:length(critIDs)){
      critMeans <- rbind(critMeans, c(i,mean(performanceTable[,i])))
    }
    critVariances <- c()
    for (i in 1:length(critIDs)){
      critVariances <- rbind(critVariances, c(i,var(performanceTable[,i])))
    }
    critVariances <- c()
    for (i in 1:length(critIDs)){
      critVariances <- rbind(critVariances, c(i,var(performanceTable[,i])))
    }
    critStandardDeviations <- c()
    for (i in 1:length(critIDs)){
      critStandardDeviations <- rbind(critStandardDeviations, c(i,sd(performanceTable[,i])))
    }
    
    critCorrelations <- cor(performanceTable,performanceTable)
    rownames(critCorrelations) <- colnames(performanceTable)
    colnames(critCorrelations) <- colnames(performanceTable)
    
}
    )
    
    if (inherits(tmpErr, 'try-error')){
      errCalc<-"Cannot generate data."
    } else {
      if(!is.null(infoCalc)) execFlag<-FALSE
      else execFlag<-TRUE
    }
  }
}
######################################################################################
##########################################
# Write the output files.
# Four possibilities appear:
#  - If everything went right with the calculation, then we write the output data into the right files: 
#    performanceTable.xml containing the generated data, and messages.xml containing an execution status.
#  - If there was an error during the calculation, we write an error message in messages.xml.
#  - If there was an error during the extraction of the data from the XMCDA trees, we write an error in messages.xml.
#  - If there was an error during the loading of the files, we write an error in messages.xml.
#  - If it was impossible to generate the data, and an info on the correlations can be given, write the info in messages.xml
##########################################

if (execFlag){
  
  # execution was successfull and we can write the output files
  
  setwd(outDirectory)
  
  # the mins file
  
  outTree = newXMLDoc()
  
  newXMLNode("xmcda:XMCDA", 
             attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2012/XMCDA-2.2.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.2.0.xsd"),
             suppressNamespaceWarning=TRUE, 
             namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2012/XMCDA-2.2.0"), 
             parent=outTree)
  
  status<-putCriteriaValues(outTree, critMins, critIDs)
  
  status<-saveXML(outTree, file=minsFile)
  
  # the maxes file
  
  outTree = newXMLDoc()
  
  newXMLNode("xmcda:XMCDA", 
             attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2012/XMCDA-2.2.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.2.0.xsd"),
             suppressNamespaceWarning=TRUE, 
             namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2012/XMCDA-2.2.0"), 
             parent=outTree)
  
  status<-putCriteriaValues(outTree, critMaxes, critIDs)
  
  status<-saveXML(outTree, file=maxesFile)
  
  # the medians file
  
  outTree = newXMLDoc()
  
  newXMLNode("xmcda:XMCDA", 
             attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2012/XMCDA-2.2.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.2.0.xsd"),
             suppressNamespaceWarning=TRUE, 
             namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2012/XMCDA-2.2.0"), 
             parent=outTree)
  
  status<-putCriteriaValues(outTree, critMedians, critIDs)
  
  status<-saveXML(outTree, file=mediansFile)
  
  # the means file
  
  outTree = newXMLDoc()
  
  newXMLNode("xmcda:XMCDA", 
             attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2012/XMCDA-2.2.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.2.0.xsd"),
             suppressNamespaceWarning=TRUE, 
             namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2012/XMCDA-2.2.0"), 
             parent=outTree)
  
  status<-putCriteriaValues(outTree, critMeans, critIDs)
  
  status<-saveXML(outTree, file=meansFile)
  
  # the variances file
  
  outTree = newXMLDoc()
  
  newXMLNode("xmcda:XMCDA", 
             attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2012/XMCDA-2.2.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.2.0.xsd"),
             suppressNamespaceWarning=TRUE, 
             namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2012/XMCDA-2.2.0"), 
             parent=outTree)
  
  status<-putCriteriaValues(outTree, critVariances, critIDs)
  
  status<-saveXML(outTree, file=variancesFile)
  
  # the standard deviations file
  
  outTree = newXMLDoc()
  
  newXMLNode("xmcda:XMCDA", 
             attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2012/XMCDA-2.2.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.2.0.xsd"),
             suppressNamespaceWarning=TRUE, 
             namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2012/XMCDA-2.2.0"), 
             parent=outTree)
  
  status<-putCriteriaValues(outTree, critStandardDeviations, critIDs)
  
  status<-saveXML(outTree, file=standardDeviationsFile)
  
  # the correlations file
  
  outTree = newXMLDoc()
  
  newXMLNode("xmcda:XMCDA", 
             attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2012/XMCDA-2.2.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.2.0.xsd"),
             suppressNamespaceWarning=TRUE, 
             namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2012/XMCDA-2.2.0"), 
             parent=outTree)
  
  status<-putCriteriaMatrix(outTree, critCorrelations)
  
  status<-saveXML(outTree, file=correlationsFile)
  
  # now the messages file
  
  outTree2 = newXMLDoc()
  
  newXMLNode("xmcda:XMCDA", 
             attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2012/XMCDA-2.2.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.2.0.xsd"),
             suppressNamespaceWarning=TRUE, 
             namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2012/XMCDA-2.2.0"), 
             parent=outTree2)
  
  status<-putLogMessage(outTree2, "OK", name = "executionStatus")
  
  status<-saveXML(outTree2, file=messagesFile)
}

if (!is.null(errCalc)){
  
  # something went wrong at the calculation step
  
  setwd(outDirectory)
  
  outTree2 = newXMLDoc()
  
  newXMLNode("xmcda:XMCDA", 
             attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2012/XMCDA-2.2.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.2.0.xsd"),
             suppressNamespaceWarning=TRUE, 
             namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2012/XMCDA-2.2.0"), 
             parent=outTree2)
  
  status<-putErrorMessage(outTree2, errCalc, name="Error")
  
  status<-saveXML(outTree2, file=messagesFile)
  
}

if (!is.null(infoCalc)){
  
  # could not generate the data, but got some info on the correlation graph
  
  setwd(outDirectory)
  
  outTree2 = newXMLDoc()
  
  newXMLNode("xmcda:XMCDA", 
             attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2012/XMCDA-2.2.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.2.0.xsd"),
             suppressNamespaceWarning=TRUE, 
             namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2012/XMCDA-2.2.0"), 
             parent=outTree2)
  
  status<-putErrorMessage(outTree2, infoCalc, name="Error")
  
  status<-saveXML(outTree2, file=messagesFile)
  
}


if (!is.null(errData)){
  
  # something went wrong at the data extraction steps
  
  setwd(outDirectory)
  
  outTree = newXMLDoc()
  
  newXMLNode("xmcda:XMCDA", 
             attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2012/XMCDA-2.2.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.2.0.xsd"),
             suppressNamespaceWarning=TRUE, 
             namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2012/XMCDA-2.2.0"), 
             parent=outTree)
  
  status<-putErrorMessage(outTree, errData, name = "Error")
  
  status<-saveXML(outTree, file=messagesFile)
}

if (!is.null(errFile)){
  
  # something went wrong while loading the files
  
  setwd(outDirectory)
  
  outTree = newXMLDoc()
  
  newXMLNode("xmcda:XMCDA", 
             attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2012/XMCDA-2.2.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.2.0.xsd"),
             suppressNamespaceWarning=TRUE, 
             namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2012/XMCDA-2.2.0"), 
             parent=outTree)
  
  status<-putErrorMessage(outTree, errFile, name = "Error")
  
  status<-saveXML(outTree, file=messagesFile)
}

