open Lib

module F2 = IntegerModulo.Make (struct let m = 2 end)
module PolF2 = Polynomial.Make (F2)

let all n =
  let rec all' i acc =
    if i = n then
      acc
    else (
      let acc' = List.concat_map (fun x -> [0 :: x; 1 :: x]) acc in
      all' (i + 1) acc'
    )
  in
  List.map PolF2.create (all' 0 [[1]])

let products xs ys =
  List.concat_map (fun x -> List.map (fun y -> PolF2.(x * y)) ys) xs

let reducible =
  List.concat [
    products (all 1) (all 1);
    products (all 1) (all 2);
    products (all 1) (all 3);
    products (all 2) (all 2);
  ]

let all =
  List.concat [
    all 1;
    all 2;
    all 3;
    all 4;
  ]

let irreducible =
  List.filter (fun x -> not (List.mem x reducible)) all

let () =
  List.iter (fun x ->
      Printf.printf "%s\n" (PolF2.to_string x)
    ) irreducible
