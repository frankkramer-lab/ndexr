## Create an RCX object
\dontrun{
rcx = rcx_new(c('@id'=1, n='Some Name', r='HGNC:Symbol'))

## Convert to ngraph (deprecated)
ngraph = ngraph_fromRCX(rcx)
## or (deprecated)
ngraph = rcx_toNGraph(rcx)

## Convert to RCXgraph
rcxgraph = rcxgraph_fromRCX(rcx)
## or
rcxgraph = rcx_toRCXgraph(rcx)


## Convert NGraph back to RCX (deprecated)
rcx = rcx_fromNGraph(ngraph)
## or (deprecated)
rcx = ngraph_toRCX(ngraph)

## Convert RCXgraph back to RCX
rcx = rcx_fromRCXgraph(rcxgraph)
## or 
rcx = rcxgraph_toRCX(rcxgraph)
}
