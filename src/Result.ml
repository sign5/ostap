open Errors

type ('b, 'stream) result =
| Parsed of ('b * 'stream) * Errors.error option
| Failed of                  Errors.error option

let emptyResult = Failed None

let cmp : Errors.error option -> Errors.error option -> Errors.error option =
  fun opt1 opt2 ->
    match opt1, opt2 with
    | None, Some x -> Some x
    | Some x, None -> Some x
    | None, None   -> None
    | Some err1, Some err2 -> if Errors.pos err1 > Errors.pos err2 then opt1 else opt2

let (<@>) : ('b, 'stream) result -> ('b, 'stream) result -> ('b, 'stream) result =
  fun res1 res2 ->
    match res1, res2 with
    | Parsed (res, opt1), Failed opt2        -> Parsed (res, cmp opt1 opt2)
    | Failed opt1,        Parsed (res, opt2) -> Parsed (res, cmp opt1 opt2)
    | Parsed (res, opt1), Parsed (_, opt2)   -> Parsed (res, cmp opt1 opt2)
    | Failed opt1,        Failed opt2        -> Failed (cmp opt1 opt2)
