module type Grid = sig
  type grid_t
  type node_t
  type index_t

  val max_neighbors : int
  val node_index : grid_t -> node_t -> index_t
  val are_neighbor : grid_t -> node_t -> node_t -> bool
  val write_neighbors : grid_t -> node_t -> node_t array -> int
  val is_valid : grid_t -> node_t -> bool
  val distance : grid_t -> node_t -> node_t -> int
end

type coordinates =
  { x : int
  ; y : int
  }

type orthogonal_grid =
  { width: int
  ; height: int
  }

module OrthogonalGraph : Grid with type node_t = coordinates and type grid_t = orthogonal_grid = struct
  type node_t = coordinates
  type grid_t = orthogonal_grid
  type index_t = int

  let max_neighbors = 8

  let is_valid grid node =
    0 <= node.x && node.x < grid.width &&
    0 <= node.y && node.y < grid.height

  let node_index grid node =
    node.x + node.y * grid.width

  let are_neighbor _g n1 n2 =
    let dx = (n1.x - n2.x) in
    let dy = (n1.y - n2.y) in
		abs dx <= 1 && abs dy <= 1

  let distance _g n1 n2 =
    let dx = n1.x - n2.x in
		let dy = n1.y - n2.y in
		max (abs dx) (abs dy)

  let write_neighbors grid node neighbors_buffer =
    let count = ref 0 in
    let add_neighbor candidate =
      neighbors_buffer.(!count) <- candidate;
      incr count
    in
    let can_cross_left = is_valid grid {x = node.x - 1; y = node.y} in
    let can_cross_top = is_valid grid {x = node.x; y = node.y - 1} in
    let can_cross_right = is_valid grid {x = node.x + 1; y = node.y} in
    let can_cross_bottom = is_valid grid {x = node.x; y = node.y + 1} in
    
    if can_cross_left then
      add_neighbor {x = node.x - 1; y = node.y};
    if can_cross_top then
      add_neighbor {x = node.x; y = node.y - 1};
    if can_cross_right then
      add_neighbor {x = node.x + 1; y = node.y};
    if can_cross_bottom then
      add_neighbor {x = node.x; y = node.y + 1};

    if can_cross_top && can_cross_left && is_valid grid {x = node.x - 1; y = node.y - 1} then
      add_neighbor {x = node.x - 1; y = node.y - 1};
    if can_cross_top && can_cross_right && is_valid grid {x = node.x + 1; y = node.y - 1} then
      add_neighbor {x = node.x + 1; y = node.y - 1};
    if can_cross_bottom && can_cross_right && is_valid grid {x = node.x + 1; y = node.y + 1} then
      add_neighbor {x = node.x + 1; y = node.y + 1};
    if can_cross_bottom && can_cross_left && is_valid grid {x = node.x - 1; y = node.y + 1} then
      add_neighbor {x = node.x - 1; y = node.y + 1};
    !count
end

type hexagonal_grid =
  { width: int
  ; height: int
  }

module HexagonalGraph : Grid with type node_t = coordinates and type grid_t = hexagonal_grid = struct
  type node_t = coordinates
  type grid_t = hexagonal_grid
  type index_t = int

  let max_neighbors = 6

  let is_valid grid node =
    let x = node.x in
    let y = node.y in
    x + (y / 2) >= 0 && x + (y / 2) < grid.width && y >= 0 && y < grid.height

  let node_index grid node =
    node.x + node.y * grid.width

  let distance _g n1 n2 =
		(abs (n1.x - n2.x) + abs (n1.x + n1.y - n2.x - n2.y) + abs (n1.y - n2.y)) / 2

  let are_neighbor _g n1 n2 =
    distance _g n1 n2 = 1

  let write_neighbors grid node neighbors_buffer =
    let count = ref 0 in
    let add_neighbor candidate =
      neighbors_buffer.(!count) <- candidate;
      incr count
    in
    if is_valid grid {x = node.x - 1; y = node.y} then
      add_neighbor {x = node.x - 1; y = node.y};
    if is_valid grid {x = node.x - 1; y = node.y + 1} then
      add_neighbor {x = node.x - 1; y = node.y + 1};
    if is_valid grid {x = node.x; y = node.y - 1} then
      add_neighbor {x = node.x; y = node.y - 1};
    if is_valid grid {x = node.x; y = node.y + 1} then
      add_neighbor {x = node.x; y = node.y + 1};
    if is_valid grid {x = node.x + 1; y = node.y - 1} then
      add_neighbor {x = node.x + 1; y = node.y - 1};
    if is_valid grid {x = node.x + 1; y = node.y} then
      add_neighbor {x = node.x + 1; y = node.y};
    !count
end