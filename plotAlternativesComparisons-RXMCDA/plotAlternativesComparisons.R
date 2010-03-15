# usage:
# R --slave --vanilla --args "[inDirectory]" "[outDirectory]" < plotAlternativesComparisons.R
# example: 
# R --slave --vanilla --args "${PWD}/plotAlternativesComparisonsInFiles" "${PWD}/plotAlternativesComparisonsOutFiles" < plotAlternativesComparisons.R

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
			treeAlternatives<-xmlTreeParse("alternatives.xml",useInternalNodes=TRUE)
		}
)
if (inherits(tmpErr, 'try-error')){
	errFile<-"Cannot read the alternatives file."
}
if (is.null(errFile)){
	tmpErr<-try(
			{
				treeAlternativesComparisons<-xmlTreeParse("alternativesComparisons.xml",useInternalNodes=TRUE)
			}
	)
	if (inherits(tmpErr, 'try-error')){
		errFile<-"Cannot read the alternatives comparisons file."
	}
}

if (is.null(errFile)){
	
	# files were correctly loaded
	# now we check if files are valid according to xsd
	
	if (checkXSD(treeAlternatives)==0)
	{
		errFile<-"Alternatives file is not XMCDA valid."	
	}
	if (is.null(errFile)){
		if (checkXSD(treeAlternativesComparisons)==0)
		{
			errFile<-"Alternatives comparisons file is not XMCDA valid."	
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
	
	numberOfAlternatives<-getNumberOfAlternatives(treeAlternatives)
	
	if (numberOfAlternatives$status == "OK") 
	{
		numAlt<-numberOfAlternatives[[1]]
		
		if (numAlt == 1){
			errData <- "At least 2 alternatives required."
			flag<-FALSE
		}
		else 
			alternativesIDs<-getAlternativesIDs(treeAlternatives)
	}
	else {
		errData <- numberOfAlternatives$status
		flag<-FALSE
	}
	
	if(flag){
		if ((alternativesIDs$status == "OK")) 
		{
			altIDs<-alternativesIDs[[1]]
			alternativesComparisonsLabels<-getAlternativesComparisonsLabels(treeAlternativesComparisons,altIDs)
		}
		else
		{
			errData <- alternativesIDs$status
			flag<-FALSE
		}
	}
	
	if (flag){
		if (alternativesComparisonsLabels$status == "OK") 
		{
			alternativesComparisons<-alternativesComparisonsLabels[[1]]
			if (is.null(alternativesComparisons))
			{
				errData<- "Could not read any comparisons on the alternatives."
				flag<-FALSE
			}
		}
		else
		{
			errData <- alternativesComparisonsLabels$status
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
		
		labels<-c(altIDs)
		
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
				for (i in 1:(dim(alternativesComparisons)[1]))
				{
					if (!is.na(alternativesComparisons[i,3])){
						if (as.numeric(alternativesComparisons[i,3])>= cut)
							edg[[which(labels[]==alternativesComparisons[i,1])]]$edges <- c(edg[[which(labels[]==alternativesComparisons[i,1])]]$edges, alternativesComparisons[i,2])
					}
					else
					{
						# if no value is loaded, add the edge anyway
		
						edg[[which(labels[]==alternativesComparisons[i,1])]]$edges <- c(edg[[which(labels[]==alternativesComparisons[i,1])]]$edges, alternativesComparisons[i,2])
					}
				}
			}
		}
		else 
		{
			if (!is.null(bcut)){
				
				# then bcut contains the beta cut level
			
				if (length(labels)!=1){
					for (i in 1:(dim(alternativesComparisons)[1]))
					{
						if (!is.na(alternativesComparisons[i,3])){
							if ((as.numeric(alternativesComparisons[i,3])>= bcut) | (as.numeric(alternativesComparisons[i,3])<= -bcut))
								edg[[which(labels[]==alternativesComparisons[i,1])]]$edges <- c(edg[[which(labels[]==alternativesComparisons[i,1])]]$edges, alternativesComparisons[i,2])
						}
						else
						{
							# if no value is loaded, add the edge anyway
					
							edg[[which(labels[]==alternativesComparisons[i,1])]]$edges <- c(edg[[which(labels[]==alternativesComparisons[i,1])]]$edges, alternativesComparisons[i,2])
						}
					}
				}
			}
			else
			{
				# we take all the arcs, whatever the cut level is
		
				if (length(labels)!=1){
					for (i in 1:(dim(alternativesComparisons)[1]))
					{
						edg[[which(labels[]==alternativesComparisons[i,1])]]$edges <- c(edg[[which(labels[]==alternativesComparisons[i,1])]]$edges, alternativesComparisons[i,2])
						# edg[[which(labels[]==alternativesComparisons[i,1])]]$weights <- c(edg[[which(labels[]==alternativesComparisons[i,1])]]$weights, as.numeric(alternativesComparisons[i,3]))
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
			errCalc<-"Cannot plot alternatives comparisons."
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
	
	status<-putAlternativesPlot(outTree, tmp, labels, mcdaConcept=names(alternativesComparisonsLabels)[1])
	
	status<-saveXML(outTree, file="alternativesComparisonsPlot.xml")
	
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
