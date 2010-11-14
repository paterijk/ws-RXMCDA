# usage:
# R --slave --vanilla --file=alternativesRankingViaQualificationDistillation.R --args "[inDirectory]" "[outDirectory]" "[alternativesFile]" "[outrankingRelationFile]" "[upwardsDistillationFile]" "[downwardsDistillationFile]" "[intersectionDistillationFile]" "[messagesFile]"
# example: 
# R --slave --vanilla --file=alternativesRankingViaQualificationDistillation.R --args "${PWD}/alternativesRankingViaQualificationDistillationInFiles" "${PWD}/alternativesRankingViaQualificationDistillationOutFiles" "alternatives.xml" "alternativesComparisons.xml" "upwardsDistillation.xml" "downwardsDistillation.xml" "intersectionDistillation.xml" "messages.xml"

rm(list=ls())

library(RXMCDA)

errFile<-NULL
errData<-NULL
errCalc<-NULL

execFlag<-FALSE

# get the in and out directories from the arguments

inDirectory <- commandArgs(trailingOnly=TRUE)[1]
outDirectory <- commandArgs(trailingOnly=TRUE)[2]
alternativesFile <- commandArgs(trailingOnly=TRUE)[3]
outrankingRelationFile <- commandArgs(trailingOnly=TRUE)[4]
upwardsDistillationFile <- commandArgs(trailingOnly=TRUE)[5]
downwardsDistillationFile <- commandArgs(trailingOnly=TRUE)[6]
intersectionDistillationFile <- commandArgs(trailingOnly=TRUE)[7]
messagesFile <- commandArgs(trailingOnly=TRUE)[8]


strength<-function(alternativesIDs, comparisons){
	x <-c()
	for (i in 1:length(alternativesIDs)){
		x<-rbind(x, c(i,0))
	}
	
	for (i in 1:(length(alternativesIDs))){
		for (j in 1:(dim(comparisons)[1])){
			if (!is.na(comparisons[j,3])){
				# if the value is not loaded, then we have an NA in the third column. 
				if( (comparisons[j,1]==alternativesIDs[i]) & (comparisons[j,3]==1)){
					x[i,2] <- x[i,2]+1
				}
			}
		}
	}
	return(x)
}

weakness<-function(alternativesIDs, comparisons){
	# we need a valued outranking relation with 0s and 1s (which means a cut relation)! 
	x <-c()
	for (i in 1:length(alternativesIDs)){
		x<-rbind(x, c(i,0))
	}
	for (i in 1:(length(alternativesIDs))){
		for (j in 1:(dim(comparisons)[1])){
			if (!is.na(comparisons[j,3])){
				# if the value is not loaded, then we have an NA in the third column. 
				if( (comparisons[j,2]==alternativesIDs[i]) & (comparisons[j,3]==1)){
					x[i,2] <- x[i,2]+1
				}
			}
		}
	}
	return(x)
}

qualification<-function(alternativesIDs, comparisons){
	x <-c()
	for (i in 1:length(alternativesIDs)){
		x<-rbind(x, c(i,0))
	}
			
	x[,2] <- strength(alternativesIDs, comparisons)[,2] - weakness(alternativesIDs, comparisons)[,2]
	return(x)
}


# set the working directory as the in directory

setwd(inDirectory)

# try to load the mandatory input files

tmpErr<-try(
		{
			treeAlternatives<-xmlTreeParse(alternativesFile,useInternalNodes=TRUE)
		}
)
if (inherits(tmpErr, 'try-error')){
	errFile<-"Cannot read the alternatives file."
}
if (is.null(errFile)){
	tmpErr<-try(
			{
				treeAlternativesComparisons<-xmlTreeParse(outrankingRelationFile,useInternalNodes=TRUE)
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
	
	if (is.null(errData)){
		
		# all data elements have been correctly extracted from the xml trees
		# we can proceed to the calculations now
		tmpErr<-try(
				{	
					# to be sure that we don't lose anything we make copies of the data

					origAltIDsInRelation <- intersect(altIDs,c(alternativesComparisons[,1], alternativesComparisons[,2]))
					origAlternativesComparisons <- alternativesComparisons

					# let us first make the downwards distilation

					altIDsInRelation <- origAltIDsInRelation
					qual<-qualification(origAltIDsInRelation, alternativesComparisons)
					
					downDistil<-c()
					for (i in 1:length(origAltIDsInRelation)){
						downDistil<-rbind(downDistil, c(i,0))
					}
					rownames(downDistil)<-origAltIDsInRelation
					rank<-1
					while (length(alternativesComparisons) > 0){

						# remove the alternatives of the relation which have maximal qualification and add them to the downDistil vector
						removedAlts<-altIDsInRelation[which(qual[,2] == max(qual[,2]))]
						
						for (k in 1:length(removedAlts)){
							downDistil[removedAlts[k],2]<-rank
						}
						rank<-rank+length(removedAlts)
						
						# first we remove the alternatives from the "origin" column
						for (k in 1:length(removedAlts))
							alternativesComparisons<-alternativesComparisons[-which(alternativesComparisons[,1] == removedAlts[k]),]
						if (length(alternativesComparisons) == 0){
							break
						} else{
							# then from the "destination" column, if something is left
							for (k in 1:length(removedAlts))
								alternativesComparisons<-alternativesComparisons[-which(alternativesComparisons[,2] == removedAlts[k]),]
						}
						# if something is left in the relation we redetermine which alternatives
						# and on those alternatives we calculate the qualification
						if (length(alternativesComparisons) == 0){
							break
						} else{
							if (!is.null(dim(alternativesComparisons))){
								altIDsInRelation <- intersect(altIDs,c(alternativesComparisons[,1], alternativesComparisons[,2]))
								qual <- qualification(altIDsInRelation, alternativesComparisons)
							}
							else{
								# this means that the alternativesComparisons is reduced to only
								# one line, containing the same alternative (x,x,1)
								# in that case, x is just to be added to the downDistil list
								downDistil[alternativesComparisons[1],2]<-rank
								break
							}
						}						
					}

					# let us now make the upward distilation
					altIDsInRelation <- origAltIDsInRelation
					alternativesComparisons <- origAlternativesComparisons
					qual<-qualification(origAltIDsInRelation, alternativesComparisons)
					
					upDistil<-c()
					for (i in 1:length(origAltIDsInRelation)){
						upDistil<-rbind(upDistil, c(i,0))
					}
					rownames(upDistil)<-origAltIDsInRelation
					rank<-length(origAltIDsInRelation)
					while (length(alternativesComparisons) > 0){

						# remove the alternatives of the relation which have minimal qualification and add them to the upDistil vector
						removedAlts<-altIDsInRelation[which(qual[,2] == min(qual[,2]))]
						
						for (k in 1:length(removedAlts)){
							upDistil[removedAlts[k],2]<-rank
						}
						rank<-rank-length(removedAlts)
						
						# first we remove the alternatives from the "origin" column
						for (k in 1:length(removedAlts))
							alternativesComparisons<-alternativesComparisons[-which(alternativesComparisons[,1] == removedAlts[k]),]
						if (length(alternativesComparisons) == 0){
							break
						} else{
							# then from the "destination" column, if something is left
							for (k in 1:length(removedAlts))
								alternativesComparisons<-alternativesComparisons[-which(alternativesComparisons[,2] == removedAlts[k]),]
						}
						# if something is left in the relation we redetermine which alternatives
						# and on those alternatives we calculate the qualification
						if (length(alternativesComparisons) == 0){
							break
						} else{
							if (!is.null(dim(alternativesComparisons))){
								altIDsInRelation <- intersect(altIDs,c(alternativesComparisons[,1], alternativesComparisons[,2]))
								qual <- qualification(altIDsInRelation, alternativesComparisons)
							}
							else{
								# this means that the alternativesComparisons is reduced to only
								# one line, containing the same alternative (x,x,1)
								# in that case, x is just to be added to the upDistil list
								upDistil[alternativesComparisons[1],2]<-rank
								break
							}
						}						
					}
					if (min(upDistil[,2]) != 1){
						upDistil[,2] <- rank(upDistil[,2], ties.method = "min")
					}
					

					# let us now compute the intersection of the upward and the downward distilations
					intersectionDistilation <- matrix(nrow=0,ncol=2)
					downDistil <- downDistil[order(downDistil[,2]),]
					upDistil <- upDistil[order(upDistil[,2]),]
					for (i in 1:(dim(upDistil)[1]-1)){
						for (j in (i+1):dim(upDistil)[1]){
							if(downDistil[rownames(upDistil)[i],2] <= downDistil[rownames(upDistil)[j],2]){
								intersectionDistilation <- rbind(intersectionDistilation, c(rownames(upDistil)[i],rownames(upDistil)[j]))
							}
							
							
						}
					}
				}
		)
		
		if (inherits(tmpErr, 'try-error')){
			errCalc<-"Cannot compute alternatives ranking."
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
	
	# first the result files
	
	outTree = newXMLDoc()
	
	newXMLNode("xmcda:XMCDA", 
			attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2009/XMCDA-2.0.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.0.0.xsd"),
			suppressNamespaceWarning=TRUE, 
			namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2009/XMCDA-2.0.0"), 
			parent=outTree)
	
	status<-putAlternativesValues(outTree, upDistil, origAltIDsInRelation, "Upwards distillation")
	
	status<-saveXML(outTree, file=upwardsDistillationFile)

	outTree = newXMLDoc()
	
	newXMLNode("xmcda:XMCDA", 
			attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2009/XMCDA-2.0.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.0.0.xsd"),
			suppressNamespaceWarning=TRUE, 
			namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2009/XMCDA-2.0.0"), 
			parent=outTree)
	
	status<-putAlternativesValues(outTree, downDistil, origAltIDsInRelation, "Downwards distillation")
	
	status<-saveXML(outTree, file=downwardsDistillationFile)

	outTree = newXMLDoc()
	
	newXMLNode("xmcda:XMCDA", 
			attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2009/XMCDA-2.0.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.0.0.xsd"),
			suppressNamespaceWarning=TRUE, 
			namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2009/XMCDA-2.0.0"), 
			parent=outTree)
	
	status<-putAlternativesComparisonsLabels(outTree, intersectionDistilation, "Intersection of upwards and downwards distillation")
	
	status<-saveXML(outTree, file=intersectionDistillationFile)
	
	# now the messages file
	
	outTree2 = newXMLDoc()
	
	newXMLNode("xmcda:XMCDA", 
			attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2009/XMCDA-2.0.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.0.0.xsd"),
			suppressNamespaceWarning=TRUE, 
			namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2009/XMCDA-2.0.0"), 
			parent=outTree2)
	
	status<-putLogMessage(outTree2, "OK", name = "executionStatus")
	
	status<-saveXML(outTree2, file=messagesFile)
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
	
	status<-saveXML(outTree2, file=messagesFile)
	
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
	
	status<-saveXML(outTree, file=messagesFile)
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
	
	status<-saveXML(outTree, file=messagesFile)
}
