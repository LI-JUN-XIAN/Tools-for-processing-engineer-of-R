
#group_split_byseq(AA,"L")

group_split_byseq <- function(DF,Group){

	DF$SEQ_O <- 1:length(DF[,Group])
	InfoMissing <- sum(is.na(DF[,Group]))

	if(InfoMissing>0){
		DF_GroupNA <- DF[is.na(DF[,Group]),]
		DF <- DF[!is.na(DF[,Group]),]
		DF <- DF[order(DF$SEQ_O),]
		DF$SEQ <- 1:length(DF[,Group])
		}else{
		DF$SEQ <- DF$SEQ_O
		}

	DF[,Group] <- as.character(DF[,Group])
	G <- DF[,Group]
	CP <- c(0,which(G!=c(G[-1],tail(G,1))),length(G))
	DF$G2 <- as.numeric(cut(1:length(G),CP))
	DF$Group2 <- as.character(paste0(G,DF$G2))

	GT <- unique(DF[c(Group,'G2')])
	GT <- do.call(rbind,lapply(split(GT,GT[,Group]),function(x){ 
		x <- x[order(x$G2),]
		x$G3 <- c(1:length(x[,Group]))
		return(x)
		}))
	GT$Group22 <- as.character(paste0(GT$L,GT$G2))
	GroupSplited <- paste0(Group,'_Splited')
	GT[,GroupSplited] <- paste0(GT$L,GT$G3)
	GT <- GT[,-which(names(GT) %in% c(Group,'G2','G3'))]

	DF <- merge(x=DF,y=GT,by.x="Group2",by.y="Group22",all.x=T,suffixes=c("_x","_y"))
	DF <- DF[,-which(names(DF) %in% c('Group2','G2','SEQ'))]

	if(InfoMissing>0){
		DF_GroupNA[,GroupSplited] <- NA
		DF <- rbind(DF,DF_GroupNA)
		DF <- DF[order(DF$SEQ_O),]
		}

	DF <- DF[order(DF$SEQ_O),]
	DF <- subset(DF,select=-SEQ_O)

	return(DF)

}