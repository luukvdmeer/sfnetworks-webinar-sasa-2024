library(sfnetworks)
library(sf)
library(spdep)

create_spatial_random = function(n, dist, bounds, directed  = TRUE) {
  # Sample n points in bounds.
  nodes = st_sf(geometry = st_sample(bounds, n))
  # Compute distances between the points.
  dists = st_distance(nodes)
  diag(dists) = Inf # No loop edges.
  if (!directed) upper.tri(dists) = Inf # No duplicated edges.
  # Find node pairs within the distance threshold from each other.
  pairs = which(dists < dist, arr.ind = TRUE)
  edges = setNames(as.data.frame(pairs), c("from", "to"))
  sfnetwork(nodes, edges)
}

create_spatial_delauney = function(nodes) {
  geo = st_geometry(nodes)
  nb = tri2nb(geo)
  wt = sfdep::st_nb_dists(geo, nb)
  sfdep::st_as_graph(geo, nb, wt)
}

create_spatial_gabriel = function(nodes) {
  geo = st_geometry(nodes)
  nb = graph2nb(gabrielneigh(geo), sym = TRUE)
  wt = sfdep::st_nb_dists(geo, nb)
  sfdep::st_as_graph(geo, nb, wt)
}

