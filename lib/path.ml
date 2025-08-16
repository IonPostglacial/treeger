module Finding (Grid : Graph.Grid) = struct

  type score = {
    current_node: Grid.node_t;
    previous_node: Grid.node_t option;
    cost_so_far: int;
    estimated_cost: int;
  }

  let cmp_scores a b =
    compare (a.estimated_cost) (b.estimated_cost)

  let reconstruct_path grid scores start goal =
    let final_cost = (Hashtbl.find scores (Grid.node_index grid goal)).cost_so_far in
    let path_buffer = Array.make final_cost goal in
    let rec aux index node =
      path_buffer.(index) <- node;
      match Hashtbl.find_opt scores (Grid.node_index grid node) with
      | Some { previous_node = Some prev; _ } when prev <> start -> 
          aux (index - 1) prev
      | _ -> index
    in
    let start_index = aux (final_cost - 1) goal in
    let path_length = final_cost - start_index in
    if path_length = (Array.length path_buffer) then
      path_buffer
    else
      Array.sub path_buffer start_index path_length

  let find grid start goal =
    let scores = Hashtbl.create 10 in
    let frontier = Pairing_heap.create ~min_size:10 ~cmp:cmp_scores () in
    let firstScore = { current_node = start; previous_node = None; cost_so_far = 0; estimated_cost = Grid.distance grid start goal } in
    let neighbors_buffer = Array.make Grid.max_neighbors start in
    Hashtbl.add scores (Grid.node_index grid start) firstScore;
    Pairing_heap.add frontier firstScore;
    let rec loop () =
      if Pairing_heap.is_empty frontier then
        [||]
      else
        let current_score = Pairing_heap.pop_exn frontier in
        let current_node = current_score.current_node in

        if current_node = goal then
          reconstruct_path grid scores start goal
        else
          let neighbor_count = Grid.write_neighbors grid current_node neighbors_buffer in
          for i = 0 to neighbor_count - 1 do
            let neighbor = neighbors_buffer.(i) in
            let cost_to_neighbor = current_score.cost_so_far + Grid.distance grid current_node neighbor in
            let heuristic = Grid.distance grid neighbor goal in
            let neighbor_index = Grid.node_index grid neighbor in
            match Hashtbl.find_opt scores neighbor_index with
            | Some previous_evaluation when cost_to_neighbor >= previous_evaluation.cost_so_far -> ()
            | _ ->
                let neighbor_score = { current_node = neighbor
                                     ; previous_node = Some current_node
                                     ; cost_so_far = cost_to_neighbor
                                     ; estimated_cost = cost_to_neighbor + heuristic 
                                     } in
                Hashtbl.replace scores neighbor_index neighbor_score;
                Pairing_heap.add frontier neighbor_score
          done;
          loop ()
    in
    loop ()
end