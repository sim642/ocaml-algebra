open Lib

module F5 = IntegerModulo.Make (struct let m = 5 end)
module PolF5 = Polynomial.Make (F5)
module F25 = Quotient.Make (struct
    module R = PolF5
    let m = [1; 1; 1]
  end)

let pow = Common.repeat F25.( * ) F25.one

let () =
  for i = 0 to 24 do
    Printf.printf "%d %s\n" i (F25.to_string (pow i [2; 1]))
  done
