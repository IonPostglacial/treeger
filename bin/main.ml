module OrthoPathFinding = Treeger.Path.Finding (Treeger.Graph.OrthogonalGraph)
module HexPathFinding = Treeger.Path.Finding (Treeger.Graph.HexagonalGraph)

let () =
  let debug_cood (p : Treeger.Graph.coordinates) = (Printf.printf "(%d, %d) " p.x p.y); print_newline () in
  let debug_coord_array = Array.iter debug_cood in

  let start : Treeger.Graph.coordinates = {x = 2; y = 2} in
  let goal : Treeger.Graph.coordinates = {x = 5; y = 8} in

  let og : Treeger.Graph.orthogonal_grid = {width = 10; height = 10} in
  let hg : Treeger.Graph.hexagonal_grid = {width = 10; height = 10} in

  print_newline ();
  debug_coord_array (OrthoPathFinding.find og start goal);
  print_newline ();
  debug_coord_array (HexPathFinding.find hg start goal);
