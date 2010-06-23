# usage:
# R --slave --vanilla --args "[inDirectory]" "[outDirectory]" < plotCriteriaValuesPreorder.R
# example: 
#  R --slave --vanilla --args "${PWD}/plotCriteriaValuesPreorderInFiles" "${PWD}/plotCriteriaValuesPreorderOutFiles" < plotCriteriaValuesPreorder.R

rm(list=ls())

library(RXMCDA)

library(Rgraphviz)

errFile<-NULL
errData<-NULL
errCalc<-NULL

execFlag<-FALSE

# get the in and out directories from the arguments

inDirectory <- commandArgs()[5]
outDirectory <- commandArgs()[6]

# set the working directory as the in directory

setwd(inDirectory)

# try to load the mandatory input files

tmpErr<-try(
		{
			treeCriteria<-xmlTreeParse("criteria.xml",useInternalNodes=TRUE)
		}
)
if (inherits(tmpErr, 'try-error')){
	errFile<-"Cannot read the criteria file."
}
if (is.null(errFile)){
	tmpErr<-try(
			{
				treeCriteriaRanks<-xmlTreeParse("criteriaRanks.xml",useInternalNodes=TRUE)
			}
	)
	if (inherits(tmpErr, 'try-error')){
		errFile<-"Cannot read the criteria values file."
	}
}

if (is.null(errFile)){
	
	# files were correctly loaded
	# now we check if files are valid according to xsd
	
	if (checkXSD(treeCriteria)==0)
	{
		errFile<-"Criteria file is not XMCDA valid."	
	}
	if (is.null(errFile)){
		if (checkXSD(treeCriteriaRanks)==0)
		{
			errFile<-"Criteria values file is not XMCDA valid."	
		}
	}	
}

if (is.null(errFile)){
	
	# files were correctly loaded and are valid according to xsd
	
	errData<-NULL
	flag<-TRUE
	
	criteriaIDs<-getCriteriaIDs(treeCriteria)
	
	if(flag){
		if ((criteriaIDs$status == "OK")) 
		{
			critIDs<-criteriaIDs[[1]]
			critRanks<-getCriteriaValues(treeCriteriaRanks, critIDs)
		}
		else
		{
			errData <- criteriaIDs$status
			flag<-FALSE
		}
	}
	
	if (flag){
		if (critRanks$status == "OK") 
		{
			criteriaRanks<-critRanks[[1]]
			if (is.null(criteriaRanks))
			{
				errData<- "Could not read values on the criteria."
				flag<-FALSE
			}
		}
		else
		{
			errData <- critRanks$status
			flag <- FALSE	
		}
	}
	
	if (is.null(errData)){
		
		# all data elements have been correctly extracted from the xml trees
		# we can proceed to the calculations now
		
		setwd(outDirectory)
		
		graph<-NULL
		
		# we first order the ranks from best to worst
		
		orderedRanks<-criteriaRanks[order(criteriaRanks[,2],decreasing=TRUE),]
		
		# we now construct the nodes of the graph (criteria with equal rank are put together into one node)
		
		prevRank<-orderedRanks[1,2]
		curLab<-orderedRanks[1,1]
		labels<-c(critIDs[curLab])
		
		for (i in 2:length(orderedRanks[,2]))
		{
			
			if (orderedRanks[i,2] != orderedRanks[i-1,2]){
				
				labels<-c(labels, critIDs[orderedRanks[i,1]])
			}
			else
			{
				labels[length(labels)] <- paste(labels[length(labels)],critIDs[orderedRanks[i,1]],sep=",")
			}
		}
		
		# the length of the longest node label
		llnl<-max(nchar(labels))
		
		
		# we now construct the edges of the graph
		
		edg<-vector("list",length=length(labels))
		names(edg)<-labels
		if (length(labels)==1){
			edg[[1]]<-list(edges=character(0))
		}
		else{
			for (i in 1:(length(labels)-1))
			{
				edg[[i]]<-list(edges=labels[i+1])
			}
			edg[[length(labels)]]<-list(edges=character(0))
		}
		
		# finally we construct the graph
		
		tmpErr<-try(
				{
					graph <- new("graphNEL", nodes=labels, edgeL=edg, edgemode="directed")
					
					pdf("out.pdf", width=llnl*0.1+2, height=numNodes(graph)*0.7+2)
					
#					bitmap("out.png",type="pngmono", units="px", width=llnl*15+40, height=numNodes(graph)*100+150)
					
					plot(graph,
							attrs = list(node = list(shape = "box", fixedsize = FALSE, fontsize=24)))
					
					# to shut down the graphics device and write the bitmap specified earlier on the disk
					dev.off()
					
					system("convert out.pdf out.png")
					
					system("base64 -w0 out.png > out.base64")
					tmp<-readLines(file("out.base64","rt"))
					system("rm out.base64 out.png out.pdf")
					
					closeAllConnections()
				}
		)
		
		if (inherits(tmpErr, 'try-error')){
			errCalc<-"Cannot plot criteria values."
			setwd(outDirectory)
			file.remove("out.png")
			file.remove("out.pdf")
			file.remove("out.base64")
		} else {
			execFlag<-TRUE
		}
	}
}

if (execFlag){
	
	# execution was successfull and we can write the output files
	# Note however that: 
	# the optimization method might find no solution
	# in that case, errData is not null, and it is not possible to write the solutions
	
	setwd(outDirectory)
	
	# first the result file
	
	outTree = newXMLDoc()
	
	newXMLNode("xmcda:XMCDA", 
			attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2009/XMCDA-2.0.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.0.0.xsd"),
			suppressNamespaceWarning=TRUE, 
			namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2009/XMCDA-2.0.0"), 
			parent=outTree)
	
	status<-putCriteriaPlot(outTree, tmp, labels, mcdaConcept=names(critRanks)[1])
	
	status<-saveXML(outTree, file="criteriaValuesPlot.xml")
	
	# now the messages file
	
	outTree2 = newXMLDoc()
	
	newXMLNode("xmcda:XMCDA", 
			attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2009/XMCDA-2.0.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.0.0.xsd"),
			suppressNamespaceWarning=TRUE, 
			namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2009/XMCDA-2.0.0"), 
			parent=outTree2)
	
	status<-putLogMessage(outTree2, "OK", name = "executionStatus")
	
	status<-saveXML(outTree2, file="messages.xml")
}

if (!is.null(errCalc)){
	
	# something went wrong at the calculation step
	
	setwd(outDirectory)
	
	outTree2 = newXMLDoc()
	
	newXMLNode("xmcda:XMCDA", 
			attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2009/XMCDA-2.0.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.0.0.xsd"),
			suppressNamespaceWarning=TRUE, 
			namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2009/XMCDA-2.0.0"), 
			parent=outTree2)
	
	status<-putErrorMessage(outTree2, errCalc, name="Error")
	
	status<-saveXML(outTree2, file="messages.xml")
	
}

if (!is.null(errData)){
	
	# something went wrong at the data extraction steps
	
	setwd(outDirectory)
	
	outTree = newXMLDoc()
	
	newXMLNode("xmcda:XMCDA", 
			attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2009/XMCDA-2.0.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.0.0.xsd"),
			suppressNamespaceWarning=TRUE, 
			namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2009/XMCDA-2.0.0"), 
			parent=outTree)
	
	status<-putErrorMessage(outTree, errData, name = "Error")
	
	status<-saveXML(outTree, file="messages.xml")
}


if (!is.null(errFile)){
	
	# something went wrong while loading the files
	
	setwd(outDirectory)
	
	outTree = newXMLDoc()
	
	newXMLNode("xmcda:XMCDA", 
			attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2009/XMCDA-2.0.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.0.0.xsd"),
			suppressNamespaceWarning=TRUE, 
			namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2009/XMCDA-2.0.0"), 
			parent=outTree)
	
	status<-putErrorMessage(outTree, errFile, name = "Error")
	
	status<-saveXML(outTree, file="messages.xml")
}
