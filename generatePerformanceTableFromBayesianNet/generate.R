##########################################
# Usage:
# R --slave --vanilla --file=generate.R --args "[inDirectory]" "[outDirectory]" "[criteriaFile]" "[graphFile]" "[meansFile]" "[standardDeviationsFile]" "[parametersFile]" "[messagesFile]" "[performanceTableFile]" "[alternativesFile]"
# Example: 
# R --slave --vanilla --file=generate.R --args "${PWD}/in" "${PWD}/out" "criteria.xml" "graph.xml" "means.xml" "standardDeviations.xml" "parameters.xml" "messages.xml" "performanceTable.xml" "alternatives.xml"
##########################################

rm(list=ls())

##########################################
# Start by loading the RXMCDA library which allows to read and write XMCDA files
##########################################

library(RXMCDA)
library(bnlearn)
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
graphFile <- commandArgs(trailingOnly=TRUE)[4]
meansFile <- commandArgs(trailingOnly=TRUE)[5]
standardDeviationsFile <- commandArgs(trailingOnly=TRUE)[6]
parametersFile <- commandArgs(trailingOnly=TRUE)[7]
messagesFile <- commandArgs(trailingOnly=TRUE)[8]
performanceTableFile <- commandArgs(trailingOnly=TRUE)[9]
alternativesFile <- commandArgs(trailingOnly=TRUE)[10]

# get the directory of this script to be able to import the fonctions.R file
args <- commandArgs(trailingOnly = F)  
scriptPath <- normalizePath(dirname(sub("^--file=", "", args[grep("^--file=", args)])))

source(paste(scriptPath,"/","fonctions.R",sep=""))

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
  treeMeans<-xmlTreeParse(meansFile,useInternalNodes=TRUE)
}
  )
  if (inherits(tmpErr, 'try-error')){
    errFile<-"Cannot read the means file."
  }
}	
if (is.null(errFile)){
  tmpErr<-try(
{
  treeStandardDeviations<-xmlTreeParse(standardDeviationsFile,useInternalNodes=TRUE)
}
  )
  if (inherits(tmpErr, 'try-error')){
    errFile<-"Cannot read the standard deviations file."
  }
}
if (is.null(errFile)){
  tmpErr<-try(
{
  treeGraph<-xmlTreeParse(graphFile,useInternalNodes=TRUE)
}
  )
  if (inherits(tmpErr, 'try-error')){
    errFile<-"Cannot read the graph file."
  }
}
if (is.null(errFile)){
  tmpErr<-try(
{
  treeParameters<-xmlTreeParse(parametersFile,useInternalNodes=TRUE)
}
  )
  if (inherits(tmpErr, 'try-error')){
    errFile<-"Cannot read the parameters file."
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
    if (checkXSD(treeMeans)==0)
    {
      errFile<-"Means file is not XMCDA valid."	
    }
  }
  if (is.null(errFile)){
    if (checkXSD(treeStandardDeviations)==0)
    {
      errFile<-"Standard deviations file is not XMCDA valid."	
    }
  }	
  if (is.null(errFile)){
    if (checkXSD(treeGraph)==0)
    {
      errFile<-"Graph file is not XMCDA valid."	
    }
  }	
  if (is.null(errFile)){
    if (checkXSD(treeParameters)==0)
    {
      errFile<-"Parameters file is not XMCDA valid."	
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
      criteriaMeans <- getCriteriaValues(treeMeans,critIDs)
    }
    else 
    {
      errData<-criteriaIDs$status
      flag<-FALSE
    } 	
  }
  
  if (flag){
    if (criteriaMeans$status == "OK") 
    {
      critMeans<-criteriaMeans[[1]]
      criteriaStandardDeviations<-getCriteriaValues(treeStandardDeviations,critIDs)
    }
    else
    {
      errData <- criteriaMeans$status
      flag <- FALSE
    }
  }
  
  if (flag){
    if (criteriaStandardDeviations$status == "OK") 
    {
      critSD<-criteriaStandardDeviations[[1]]
      graphObj<-getCriteriaComparisonsLabels(treeGraph,critIDs)
    }
    else
    {
      errData <- criteriaStandardDeviations$status
      flag <- FALSE
    }
  }
  
  # check if all the data concerns the same number of criteria
  if (flag){
    if((dim(critSD)[1] != dim(critMeans)[1])|(dim(critSD)[1] != length(critIDs))|(dim(critMeans)[1] != length(critIDs)))
    {
      errData <- "Standard deviations file, means and / or criteria ids files are not compatible."
      flag <- FALSE
    }
  }
  
  
  if (flag){
    if (graphObj$status == "OK") 
    {
      graph<-graphObj[[1]]
      parameters<-getParameters(treeParameters)
    }
    else
    {
      errData <- graphObj$status
      flag <- FALSE
    }
  }
  
  if (flag){
    if (parameters$status == "OK") 
    {
      if (("numAlt"%in%names(parameters))&("prefix"%in%names(parameters))){
        numAlt<-parameters$numAlt
        prefix<-parameters$prefix  
      }
      else
      {
        errData<-"Missing parameter in parameters file"
        flag <- FALSE
      }
      # the seed is optional
      # if it is present, then we read it, else seed remains NULL
      seed<-NULL
      if ("seed"%in%names(parameters))
        seed <- parameters$seed
    }
    else
    {
      errData <- parameters$status
      flag <- FALSE
    }
  }
  
  ##########################################
  # Run the algorithm.
  # If everything went right up to here, run the calculations.
  #
  # In this example, we generate a vector of random ranks for the alternatives.
  ##########################################
  
  if (is.null(errData))
  {
    # all data elements have been correctly extracted from the xml trees
    # we can proceed to the calculations now
    
    tmpErr<-try(
{
  if (!is.null(seed)){
    set.seed(seed)
  }
  
  numCrit <- length(critIDs)
  
  ############# debut programme bayesien ###############################	
  
  mat.adj<-t(t(convert.graphe.adj(critIDs,numcrit,graph)))
  
  modelstring<-convert.str.adj(mat.adj)
  
  network<-model2network(modelstring)
  
  order_topo<-node.ordering(network)
  
  mean<-create.mean.sd(critIDs,numCrit,critMeans,critSD)$Mean
  
  sigma<-create.mean.sd(critIDs,numCrit,critMeans,critSD)$Sigma
  
  mean.topo<-create.mean.sd.topo(modelstring,mean,sigma)$Mean.topo
  
  sigma.topo<-create.mean.sd.topo(modelstring,mean,sigma)$Sigma.topo
  
  mean.nodes<-create.mean.mat.topo(mean.topo,sigma.topo,mat.adj,order_topo)[[1]]
  
  mat.adjency.topo<-create.mean.mat.topo(mean.topo,sigma.topo,mat.adj,order_topo)[[2]]
  
  list.nodes.bloc<-create.bloc.nodes(mat.adjency.topo,order_topo)
  
  dist<-insert.cor.param(critIDs,graph,list.nodes.bloc,mean.nodes,mat.adjency.topo,network)
  
  if(dist$State.parametrisation)
  {
    res<-custom.fit(network,dist[[1]])
    gener<-rbn(res,numAlt)
    colnames(gener)<-critIDs
    performanceTable<-gener
    
    altIDs <- c()
    for (i in 1:numAlt)
      
      altIDs <- c(altIDs,paste(prefix,i, sep=""))
    rownames(performanceTable) <- altIDs
    
    # print(dist$Message.error)
  }
  else
  {
    
    infoCalc <- paste(dist$Message.error, "Wrong correlation in:", paste("(",paste(dist$Correlation.error, sep="",collapse=","),").",sep=""), sep=" ")
    
    execFlag <- FALSE
    
    if(is.nan(dist$Borne.correlation[1])||is.nan(dist$Borne.correlation[2]))
    {
      infoCalc <- paste(infoCalc, "Impossible to calculate bounds for the correlation.", sep=" ")
      
    }
    
    else
    {
      infoCalc <- paste(infoCalc, "Correlation should be in:", paste("[",paste(dist$Borne.correlation,sep="",collapse=","),"]."), sep=" ")
    }
  }
  
  ############# fin programme bayesien #######################################  
  
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
  
  # first the result file
  
  outTree = newXMLDoc()
  
  newXMLNode("xmcda:XMCDA", 
             attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2012/XMCDA-2.2.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.2.0.xsd"),
             suppressNamespaceWarning=TRUE, 
             namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2012/XMCDA-2.2.0"), 
             parent=outTree)
  
  status<-putPerformanceTable(outTree, performanceTable)
  
  status<-saveXML(outTree, file=performanceTableFile)
  
  # then the alternatives ids file
  
  outTree = newXMLDoc()
  
  newXMLNode("xmcda:XMCDA", 
             attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2012/XMCDA-2.2.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.2.0.xsd"),
             suppressNamespaceWarning=TRUE, 
             namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2012/XMCDA-2.2.0"), 
             parent=outTree)
  
  status<-putAlternativesIDs(outTree, altIDs)
  
  status<-saveXML(outTree, file=alternativesFile)
  
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

