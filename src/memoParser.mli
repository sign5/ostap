class t :
  string ->
  object ('a)
    val table : ((int * int) * int) list
    method col : int
    method coord : Msg.Coord.t
    method get :
      string ->
      Str.regexp -> ('a, Matcher.Token.t, Reason.t) Combinators.result
    method getEOF : ('a, Matcher.Token.t, Reason.t) Combinators.result
    method line : int
    method loc : Msg.Locator.t
    method look :
      string -> ('a, Matcher.Token.t, Reason.t) Combinators.result
    method memoize :
      ('a, 'p, 'e) Combinators.parse -> ('a, 'p, 'e) Combinators.result
    method pos : int
    method prefix : int -> string
    method regexp :
      string -> string -> ('a, Matcher.Token.t, Reason.t) Combinators.result
    method skip :
      int ->
      Msg.Coord.t -> [ `Failed of Msg.t | `Skipped of int * Msg.Coord.t ]
  end
