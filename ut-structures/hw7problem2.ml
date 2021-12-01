open Lib

module F2 = IntegerModulo.Make (struct let m = 2 end)
module PolF2 = Polynomial.Make (F2)
module R = Quotient.Make (struct
    module R = PolF2
    let m = [1; 0; 0; 1]
  end)

let create x = R.(x + zero)

let all =
  let rec all' i acc =
    if i = 3 then
      acc
    else (
      let acc' = List.concat_map (fun x -> [0 :: x; 1 :: x]) acc in
      all' (i + 1) acc'
    )
  in
  List.map create (all' 0 [[]])

let () =
  Printf.printf "%-8s " "";
  List.iter (fun x ->
      Printf.printf "%-8s " (R.to_string x)
    ) all;
  Printf.printf "\n";

  List.iter (fun y ->
      Printf.printf "%-8s " (R.to_string y);
      List.iter (fun x ->
          Printf.printf "%-8s " (R.(to_string (x * y)))
        ) all;
      Printf.printf "\n";
    ) all
