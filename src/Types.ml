type ('a, 'b) tag = Parsed of 'a * 'b option | Failed of 'b option

type ('a, 'b, 'c) result = ('b * 'a, 'c) tag
and  ('a, 'b, 'c) parse  = 'a -> ('a, 'b, 'c) result
