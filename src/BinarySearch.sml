structure BinarySearch :
sig
  type 'a seq = 'a ArraySequence.t
  val search : ('a * 'a -> order) -> 'a seq -> 'a -> int
end =
struct

  structure Seq = ArraySequence
  type 'a seq = 'a Seq.t

  fun search cmp s x =
    let
      fun loop lo hi =
        case hi - lo of
          0 => lo
        | n =>
          let
            val mid = lo + n div 2
            val pivot = Seq.nth s mid
          in
            case cmp (x, pivot) of
              LESS    => loop lo mid
            | EQUAL   => mid
            | GREATER => loop (mid+1) hi
          end
    in
      loop 0 (Seq.length s)
    end

end
