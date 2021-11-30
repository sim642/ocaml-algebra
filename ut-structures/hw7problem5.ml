open Lib

module F2 = IntegerModulo.Make (struct let m = 2 end)
module PolF2 = Polynomial.Make (F2)
module F16 = Quotient.Make (struct
    module R = PolF2
    let m = [1; 1; 1; 1; 1]
  end)

let pow = Common.repeat F16.( * ) F16.one

let ord x =
  let rec ord' i acc =
    if F16.(acc = one) then
      i
    else
      ord' (i + 1) F16.(acc * x)
  in
  ord' 1 x

let create x = F16.(x + zero)

let all =
  let rec all' i acc =
    if i = 4 then
      acc
    else (
      let acc' = List.concat_map (fun x -> [0 :: x; 1 :: x]) acc in
      all' (i + 1) acc'
    )
  in
  List.map create (all' 0 [[]])

let () =
  List.iter (fun x ->
      if x <> [] then
        Printf.printf "%s: %d\n" (F16.to_string x) (ord x)
    ) all
