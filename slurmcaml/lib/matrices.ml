module type IntegerMatrix = sig
  val values : int array array
  val rows : int
  val columns : int
end

module type FloatMatrix = sig
  val values : float array array
  val rows : int
  val columns : int
end
