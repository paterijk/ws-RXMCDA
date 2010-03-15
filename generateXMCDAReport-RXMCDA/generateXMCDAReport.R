# usage:
# R --slave --vanilla --args "[inDirectory]" "[outDirectory]" < generateXMCDAReport.R
# example: 
#  R --slave --vanilla --args "${PWD}/generateXMCDAReportInFiles" "${PWD}/generateXMCDAReportOutFiles" < generateXMCDAReport.R

rm(list=ls())

library(RXMCDA)

#source("../libxmcda.R")


err2<-NULL
err3<-NULL
errRead<-NULL
errXSD<-NULL
errNodes<-NULL


# if run from command line the dataDirectory argument tells us
# where to find the data which has to be loaded and written

inDirectory <- commandArgs()[5]
outDirectory <- commandArgs()[6]

setwd(inDirectory)

files<-list.files(inDirectory)

# print(files)

outTree = newXMLDoc()

root<-newXMLNode("xmcda:XMCDA", 
		attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2009/XMCDA-2.0.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.0.0.xsd"),
		suppressNamespaceWarning=TRUE, 
		namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2009/XMCDA-2.0.0"), 
		parent=outTree)

for (i in 1:length(files)){
	
	err1<-NULL
	tmpErr<-NULL
	tmpErr<-try(
			{
				tree<-xmlTreeParse(files[i],useInternalNodes=TRUE)
			}
	)
	
	if (inherits(tmpErr, 'try-error')){
		err1<-"Cannot read an xml file."
		errRead<-err1
	} else {
		# check if files are valid according to xsd
		if (checkXSD(tree)==0){
			err1<-"At least one input file is not valid according to the XMCDA XML schema."
			errXSD<-err1
		}
	}
		
	nodes<-NULL
	
	if (is.null(err1)){
		
		tmpErr<-try(
				{
					nodes<-getNodeSet(tree,"/*/*")
				}
		)
		
		if (inherits(tmpErr, 'try-error')){
			err2<-"At least one XMCDA file could not be read correctly."
			errNodes<-err2
		}
		
		if (length(nodes)!=0){
			for (j in 1:length(nodes))
			{
				addChildren(root,nodes[[j]])
			}	
		}
	}
}

# write error message or log message and output tree

setwd(outDirectory)
	
outTree2 = newXMLDoc()
	
tmp<-newXMLNode("xmcda:XMCDA",      		
			attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2009/XMCDA-2.0.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.0.0.xsd"),
			suppressNamespaceWarning=TRUE, 
			namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2009/XMCDA-2.0.0"),			
			parent=outTree2)

	
	
if ((is.null(errRead))&(is.null(errXSD))&(is.null(errNodes))){
	toto<-putLogMessage(outTree2, "OK", name = "executionStatus")
} else {
	if (!is.null(errRead))
		toto<-putErrorMessage(outTree2, errRead, name = "Error")
	if (!is.null(errXSD))
		toto<-putErrorMessage(outTree2, errXSD, name = "Error")
	if (!is.null(errNodes))
		toto<-putErrorMessage(outTree2, errNodes, name = "Error")
}
		
toto<-saveXML(outTree2, file="messages.xml")

toto<-saveXML(outTree, file="report.xml")
