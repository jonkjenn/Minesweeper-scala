//val v = Vector(Vector(true,false),Vector(false,true))
//val w = Vector(Vector(true,true),Vector(false,false))
sealed trait CellState
case object Initial extends CellState
case object Flagged extends CellState

val v = Vector(false)
val w = Vector(true,true,false)


var a = Vector.fill[Either[CellState,Boolean]](5,5)(Left(Initial))
//var a = Vector.fill[Int](5)(1)
//var a = Vector.fill(5){1}

var b = Vector(5,4,2,6,7)

b.reduce((a,b) => a + b)

math.pow(1000000000,-9)

//val t = Vector(Vector(1,2,3),Vector(3,2,1))
//val r = Vector(Vector(7,7,7),Vector(8,8,8))

//t.zip(r).flatten




def test():Vector[Boolean] = {
  return Vector(true,false,true)
}

