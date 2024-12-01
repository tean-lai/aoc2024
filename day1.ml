let left, right = 
  let left, right = ref [], ref [] in 
  try
    while true do
      let h1, h2 = Scanf.scanf " %d %d " (fun x y -> x, y) in 
      left := h1 :: !left;
      right := h2 :: !right;
    done; !left, !right
  with End_of_file -> !left, !right

let left = List.sort compare left
let right = List.sort compare right 

let total_distance = 
  List.map2 (fun x y -> abs (x - y)) left right |> List.fold_left (+) 0

let () = Printf.printf "total distance = %d\n" total_distance
let sum_list = List.fold_left (+) 0

let count_appearance lst n = List.fold_left (fun acc x -> if x = n then acc + 1 else acc) 0 lst

let similarity = left |> List.map (fun x -> x * (count_appearance right x)) |> sum_list
let () = Printf.printf "similarity = %d\n" similarity