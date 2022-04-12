\dontrun{
## Create an RCX object
rcx = rcx_new(c('@id'=1, n='Some Name', r='HGNC:Symbol'))

## Convert to RCXgraph
rcxgraph = rcxgraph_fromRCX(rcx)
## or
rcxgraph = rcx_toRCXgraph(rcx)

## Convert RCXgraph back to RCX
rcx = rcx_fromRCXgraph(rcxgraph)
## or 
rcx = rcxgraph_toRCX(rcxgraph)
}