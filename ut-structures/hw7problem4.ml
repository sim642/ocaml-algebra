open Lib

module F5 = IntegerModulo.Make (struct let m = 5 end)
module PolF5 = Polynomial.Make (F5)
module F25 = Quotient.Make (struct
    module R = PolF5
    let m = [1; 1; 1]
  end)

let pow = Common.repeat F25.( * ) F25.one

let () =
  let create x = F25.(x + zero) in
  print_endline F25.(to_string (create [3; 0; 2; 0; 1]));
  print_endline F25.(to_string (pow 2021 [-1; 1]));
  print_endline F25.(to_string (create [2; 0; 1]));
  print_endline F25.(to_string ([3; 0; 2; 0; 1] + pow 2021 [-1; 1] + [2; 0; 1]));
  print_endline F25.(to_string (pow 19 [0; 1] + [0; -1]));
  print_endline F25.(to_string (create [2; 1; 0; 3]));
  print_endline F25.(to_string ((pow 19 [0; 1] + [0; -1]) + [2; 1; 0; 3]));
  print_endline F25.(to_string (([3; 0; 2; 0; 1] + pow 2021 [-1; 1] + [2; 0; 1]) * ((pow 19 [0; 1] + [0; -1]) + [2; 1; 0; 3])));
