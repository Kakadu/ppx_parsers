type stream
type err = unit
type 'rez r = Parsed of 'rez * unit * stream | Failed of unit
type 'rez parser = stream -> 'rez r

let look : string -> stream -> string r = fun patt s -> assert false

let (<|>) : 'a parser -> 'a parser -> 'a parser = fun p1 p2 -> assert false

