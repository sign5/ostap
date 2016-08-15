(** {2 Main parsing types } *)

(** Type pattern for the result of parsing. Here ['a] --- type of {i parsed value}, ['b] --- type of
    {i failure reason} (description of parse problem). Result is 
    {ul {- either a parsed value coupled with optional reason designated to denote deferred errors}
        {- or a failure with optional reason.}
    }
    
    Deferred reasons are those which can be potentially signalled in the future. For example, 
    parsing the string "A, B" with the rule ("A" "B")? has to return parsed value with deferred failure
    reason "B expected".
 *)
type ('a, 'b) tag = Parsed of 'a * 'b option | Failed of 'b option

(** The type 
    {C [type ('stream, 'parsed, 'error) result = ('parsed * 'stream, 'error) tag]}
    denotes the result of parsing a stream with a parser. This result is either parsed value of type 
    ['parsed] and the residual stream of type ['stream], or failure with reason of type ['error].
 *)
type ('stream, 'parsed, 'error) result = ('parsed * 'stream, 'error) tag

(** The type 
    {C [type ('stream, 'parsed, 'error) parse  = 'stream -> ('stream, 'parsed, 'error) result]}
    corresponds to a parser. Parser takes a stream of type ['stream] and returns result.
 *)
type ('stream, 'parsed, 'error) parse  = 'stream -> ('stream, 'parsed, 'error) result
