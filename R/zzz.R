.onAttach = function(...){
  packageStartupMessage("This is `geosimilarity` 3.6.
                        \nTo cite `geosimilarity` in publications, please use:
                        \nSong, Y. (2022). Geographically Optimal Similarity. Mathematical Geosciences. doi: 10.1007/s11004-022-10036-8.
                        ")
}

.onLoad = function(...) {
  loadNamespace("tibble")
}
