let foo = [%expr (print_endline "A") ]

let _ = [%e foo]

