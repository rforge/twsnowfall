twMergeSequences <- function(
    ### merge several sequences to a single sequence
    nItem       ##<< integer vector (nSeq) number of items in each sequence
    ,mergeMethod= c( ##<< method of merging
        ##describe<<        
        random="random"     ##<< sequences are merged randomly 
        ,stack="stack"      ##<< stack sequences 
        ,slice="slice"      ##<< slices of the sequences occur distributed across resulting sequence
        ##end<<
    ) 
    ,nInSlice=4     ##<< length of a slice for mergeMethod slice
){
    ##seealso<< \link{twMisc}
    
    nSeq <- length(nItem)   # number of sequences
    nTot <- sum(nItem)
    
    mergeMethod <- match.arg(mergeMethod)
    seq <- switch( mergeMethod,
        "random" = {
            stack <- do.call( c, lapply(1:nSeq, function(i){ rep(i,nItem[i]) }))
            stack[sample.int(nTot)] 
        }
        ,"stack" = do.call( c, lapply(1:nSeq, function(i){ rep(i,nItem[i]) })) 
        ,"slice" = {
            nSliceSeq0 <- nItem %/% nInSlice    # number of slices of length nInSeq
            nSliceSeqM <- max(nSliceSeq0)
            pS <- nSliceSeqM/(nSliceSeq0)
            iSliceSeq <- lapply( pS, function(pSi){ tmp<-round(pSi*(1:nSliceSeqM)); tmp[tmp <= nSliceSeqM]})
            nSliceSeq <- sapply(iSliceSeq, length)
            modSeq <- nItem - nSliceSeq*nInSlice    # if nSamplePop is not a multiple of nInSeq
            res <- vector( "integer", nTot )
            #---- fill in the modulo entries at the beginning
            i0 <- 0
            for( iSeq in 1:nSeq){
                if( 0 != modSeq[iSeq]){
                    res[i0 + 1:modSeq[iSeq] ] <- iSeq
                    i0 <- i0 + modSeq[iSeq]
                } # if modPop != 0
            } # iPop
            #---- in each slice check if current seq contributes to it
            iInSeq <- 1:nInSlice
            for( iSlice in 1:nSliceSeqM){
                for( iSeq in 1:nSeq){
                    if( !is.na(match(iSlice, iSliceSeq[[iSeq]])) ){ ## iiSeq is the index in iSeqPop
                        res[i0 + iInSeq ] <- iSeq
                        i0 <- i0 + nInSlice
                    } # if in this slice
                } # iSeq
            } # iSlice
            res
        } # method slice  
        , stop(paste("mergeSequences: unknown mergeMethod",mergeMethod))
    )
    ##value<< integer vector of length (sum(n)), which each position specifying the index of the originating sequence 
    seq
}
attr(twMergeSequences,"ex") <- function(){
    nItem = c(3,5,8)
    twMergeSequences(nItem, "stack")
    twMergeSequences(nItem)
    twMergeSequences(nItem, "slice")
}
