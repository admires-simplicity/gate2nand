type var = string

type gate =
  | VAR of var
  | NAND of gate * gate
  | NOT of gate
  | AND of gate * gate
  | OR of gate * gate
  (* | NOR of gate * gate
  | XOR of gate * gate
  | XNOR of gate * gate *)


let rec gate2nand (g: gate): gate =
  match g with
  | VAR v -> VAR v
  | NAND (x, y) -> NAND (gate2nand x, gate2nand y)
  | NOT x -> gate2nand (NAND (x, x))
  | AND (x, y) -> gate2nand (NOT (NAND (x, y)))
  | OR (x, y) -> gate2nand (NOT (AND ((NOT x), (NOT y))))