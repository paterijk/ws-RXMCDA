# usage:
# R --slave --vanilla --args "[inDirectory]" "[outDirectory]" < plotCriteriaComparisons.R
# example: 
# R --slave --vanilla --args "${PWD}/plotCriteriaComparisonsInFiles" "${PWD}/plotCriteriaComparisonsOutFiles" < plotCriteriaComparisons.R

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
				treeCriteriaComparisons<-xmlTreeParse("criteriaComparisons.xml",useInternalNodes=TRUE)
			}
	)
	if (inherits(tmpErr, 'try-error')){
		errFile<-"Cannot read the criteria comparisons file."
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
		if (checkXSD(treeCriteriaComparisons)==0)
		{
			errFile<-"Criteria comparisons file is not XMCDA valid."	
		}
	}	
}

if (is.null(errFile)){
	
	# let us now load the optional xml files
	# if an error occurs, we suppose that the file was not present (optional files!!)
	
	treeCutLevel<-NULL
	
	tmpErr<-try(
			{
				treeCutLevel<-xmlTreeParse("cutLevel.xml",useInternalNodes=TRUE)
			}
	)
}

if (is.null(errFile)){
	
	# we must now check if the optional files are XMCDA valid
	# for the optional files we first check whether a tree has been loaded
	
	if ((!is.null(treeCutLevel))){
		if (checkXSD(treeCutLevel)==0)
		{
			errFile<-"Cut level parameters file is not XMCDA valid."	
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
			criteriaComparisonsLabels<-getCriteriaComparisonsLabels(treeCriteriaComparisons,critIDs)
		}
		else
		{
			errData <- criteriaIDs$status
			flag<-FALSE
		}
	}
	
	if (flag){
		if (criteriaComparisonsLabels$status == "OK") 
		{
			criteriaComparisons<-criteriaComparisonsLabels[[1]]
			if (is.null(criteriaComparisons))
			{
				errData<- "Could not read any comparisons on the criteria."
				flag<-FALSE
			}
		}
		else
		{
			errData <- criteriaComparisonsLabels$status
			flag <- FALSE	
		}
	}
	
	# for the optional file on the cut level, let us first check if a tree has been loaded
	cut <- NULL
	bcut <- NULL
	cutLevels<-NULL
	
	if (flag){
		if ((!is.null(treeCutLevel)))
		{
			# first let us try to find a standard ">=" cutlevel
			cutLevels<-getParameters(treeCutLevel, "cutLevel")
			
			if (cutLevels$status == "OK") 
			{
				cut<-cutLevels[[1]]
			}
			else
			{
				cutLevels<-getParameters(treeCutLevel, "bipolarCutLevel")
				
				if (cutLevels$status == "OK") 
				{
					bcut<-cutLevels[[1]]
				}
				else
				{
					errData <- cutLevels$status
					flag <- FALSE
				}
			}
		}
	}
	
	if (is.null(errData)){
		
		# all data elements have been correctly extracted from the xml trees
		# we can proceed to the calculations now
		
		graph<-NULL
		
		# we now construct the nodes of the graph
		
		labels<-c(critIDs)
		
		# the length of the longest node label
		
		llnl<-max(nchar(labels))
		
		# we now construct the edges of the graph
		
		edg<-vector("list",length=length(labels))
		
		names(edg)<-labels
		
		for (i in 1:(length(labels)))
		{
			edg[[i]]<-list(edges=character(0), weights=c())
		}
		
		# then the arcs are valued and a cut level might be used
		
		if (!is.null(cut)){
			
			# then cut contains the cut level
			
			if (length(labels)!=1){
				for (i in 1:(dim(criteriaComparisons)[1]))
				{
					if (!is.na(criteriaComparisons[i,3])){
						if (as.numeric(criteriaComparisons[i,3])>= cut)
							edg[[which(labels[]==criteriaComparisons[i,1])]]$edges <- c(edg[[which(labels[]==criteriaComparisons[i,1])]]$edges, criteriaComparisons[i,2])
					}
					else
					{
						# if no value is loaded, add the edge anyway
						
						edg[[which(labels[]==criteriaComparisons[i,1])]]$edges <- c(edg[[which(labels[]==criteriaComparisons[i,1])]]$edges, criteriaComparisons[i,2])
					}
				}
			}
		}
		else 
		{
			if (!is.null(bcut)){
				
				# then bcut contains the beta cut level
				
				if (length(labels)!=1){
					for (i in 1:(dim(criteriaComparisons)[1]))
					{
						if (!is.na(criteriaComparisons[i,3])){
							if ((as.numeric(criteriaComparisons[i,3])>= bcut) | (as.numeric(criteriaComparisons[i,3])<= -bcut))
								edg[[which(labels[]==criteriaComparisons[i,1])]]$edges <- c(edg[[which(labels[]==criteriaComparisons[i,1])]]$edges, criteriaComparisons[i,2])
						}
						else
						{
							# if no value is loaded, add the edge anyway
							
							edg[[which(labels[]==criteriaComparisons[i,1])]]$edges <- c(edg[[which(labels[]==criteriaComparisons[i,1])]]$edges, criteriaComparisons[i,2])
						}
					}
				}
			}
			else
			{
				# we take all the arcs, whatever the cut level is
				
				if (length(labels)!=1){
					for (i in 1:(dim(criteriaComparisons)[1]))
					{
						edg[[which(labels[]==criteriaComparisons[i,1])]]$edges <- c(edg[[which(labels[]==criteriaComparisons[i,1])]]$edges, criteriaComparisons[i,2])
						# edg[[which(labels[]==criteriaComparisons[i,1])]]$weights <- c(edg[[which(labels[]==criteriaComparisons[i,1])]]$weights, as.numeric(criteriaComparisons[i,3]))
					}
				}
			}
		}
		
		# finally we construct the graph
		
		tmpErr<-try(
				{
					setwd(outDirectory)
					
					graph <- new("graphNEL", nodes=labels, edgeL=edg, edgemode="directed")
					
					pdf("out.pdf", 
#							width=llnl*0.1+1, height=numNodes(graph)*0.7+1
					)
					
					plot(graph, 
							recipEdges = "distinct",
							attrs = list(node = list(shape = "box", fixedsize = FALSE, fontsize=24)))
					
					# to shut down the graphics device and write the pdf specified earlier on the disk
					dev.off()
					
					system("convert out.pdf out.png")
					
					system("base64 -w0 out.png > out.base64")
					tmp<-readLines(file("out.base64","rt"))
					system("rm out.base64 out.png out.pdf")
					
					closeAllConnections()
				}
		)
		
		if (inherits(tmpErr, 'try-error')){
			errCalc<-"Cannot plot criteria comparisons."
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
	
	status<-putCriteriaPlot(outTree, tmp, labels, mcdaConcept=names(criteriaComparisonsLabels)[1])
	
	status<-saveXML(outTree, file="criteriaComparisonsPlot.xml")
	
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
