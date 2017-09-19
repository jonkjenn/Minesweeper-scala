import org.scalatest.{BeforeAndAfterEach, FunSuite}
import com.jonkjenn.minesweeper.Helpers._
import com.jonkjenn.minesweeper._

class HelpersTest extends FunSuite with BeforeAndAfterEach {

  val map: Vector[Vector[Int]] = Vector(Vector(1,2),Vector(5,2),Vector(7,1))
  //val nbs: Vector[Vector[Int]] =

  /*
  - - - x
  - - - -
  - - - -
  x - - x

  0 0 1 0
  0 0 1 1
  1 1 1 1
  0 1 1 0
   */
  val nbs1 = Vector(Vector(0,1,0,0),Vector(1,1,0,0),Vector(1,1,1,1),Vector(0,1,1,0))
  val uncoveredMap1 = Vector(Vector(false,false,false,false),Vector(false,false,false,false),Vector(false,false,false,false),Vector(false,false,false,false))
  val mineMap1 = Vector(Vector(true,false,false,false), Vector(false,false,false,false), Vector(false,false,false,false),Vector(true,false,false,true))

  override def beforeEach() {

  }

  test("testValues") {
    val res = values[Int](Vector(Point(0,1),Point(2,0)),map)
    assert(res.size == 2)
    assert(res(0) == 2)
    assert(res(1) == 7)

    val resEmpty = values(Vector(),map)
    assert(resEmpty.size == 0)
  }

  test("testUncover") {
    val p = Point(0,3)
    val r = uncover(p,4,4,uncoveredMap1,mineMap1,nbs1)
    assert(r(0)(3) == true)
    assert(r(1)(3) == true)
    assert(r(0)(2) == true)
    assert(r(1)(2) == true)
    assert(r(0)(0) == false)
    assert(r(0)(1) == false)
    assert(r(1)(0) == false)
    assert(r(1)(1) == false)
    assert(r(2)(0) == false)
    assert(r(2)(1) == false)
    assert(r(2)(2) == false)
    assert(r(2)(3) == false)
    assert(r(3)(0) == false)
    assert(r(3)(1) == false)
    assert(r(3)(2) == false)
    assert(r(3)(3) == false)

    val p2 = Point(0,1)
    val r2 = uncover(p2,4,4,uncoveredMap1,mineMap1,nbs1)
    assert(r2(0)(3) == false)
    assert(r2(1)(3) == false)
    assert(r2(0)(2) == false)
    assert(r2(1)(2) == false)
    assert(r2(0)(0) == false)
    assert(r2(0)(1) == true)
    assert(r2(1)(0) == false)
    assert(r2(1)(1) == false)
    assert(r2(2)(0) == false)
    assert(r2(2)(1) == false)
    assert(r2(2)(2) == false)
    assert(r2(2)(3) == false)
    assert(r2(3)(0) == false)
    assert(r2(3)(1) == false)
    assert(r2(3)(2) == false)
    assert(r2(3)(3) == false)

    val p3 = Point(1,2)
    val r3 = uncover(p3,4,4,uncoveredMap1,mineMap1,nbs1)
    assert(r3(0)(3) == true)
    assert(r3(1)(3) == true)
    assert(r3(0)(2) == true)
    assert(r3(1)(2) == true)
    assert(r3(0)(0) == false)
    assert(r3(0)(1) == false)
    assert(r3(1)(0) == false)
    assert(r3(1)(1) == false)
    assert(r3(2)(0) == false)
    assert(r3(2)(1) == false)
    assert(r3(2)(2) == false)
    assert(r3(2)(3) == false)
    assert(r3(3)(0) == false)
    assert(r3(3)(1) == false)
    assert(r3(3)(2) == false)
    assert(r3(3)(3) == false)
  }

  test("testUcoverPositions"){
    val r = uncoverPositions(
      Vector(Vector(true,false,false),Vector(false,true,false),Vector(false,false,false)),
      Vector(Vector(false,true,false),Vector(false,false,false),Vector(false,false,false))
    )

    assert(r(0)(0) == true)
    assert(r(0)(1) == true)
    assert(r(0)(2) == false)
    assert(r(1)(0) == false)
    assert(r(1)(1) == true)
    assert(r(1)(2) == false)
    assert(r(2)(0) == false)
    assert(r(2)(1) == false)
    assert(r(2)(2) == false)
  }

  test("testUpdate2d") {
    val r = update2d(Vector((Point(),7),(Point(2,2),0),(Point(3,5),9)),Vector(Vector(1,1,1),Vector(2,2,2),Vector(3,3,3),Vector(4,4,4,4,4,4)))
    assert(r(0)(0) == 7)
    assert(r(2)(2) == 0)
    assert(r(3)(5) == 9)
  }

  test("testRandomLocations") {

  }

  test("testXyFromIndex") {
    assert(xyFromIndex(0,Rect(0,0)) == None)
    val v = xyFromIndex(10,Rect(10,10)).get
    assert(v.x == 0 && v.y == 1)

    val v2 = xyFromIndex(3,Rect(2,2)).get
    assert(v2.x == 1 && v2.y == 1)

    val v3 = xyFromIndex(15,Rect(4,4)).get
    assert(v3.x == 3 && v3.y == 3)

    val v4 = xyFromIndex(23,Rect(5,5)).get
    assert(v4.x == 3 && v4.y == 4)

    val v5 = xyFromIndex(77,Rect(10,10)).get
    assert(v5.x == 7 && v5.y == 7)
  }

  test("testGetNeighbourCoordinates") {
    val x = getNeighbourCoordinates(Point(0,0),100,100)
    assert(x.contains(Point(1,0)))
    assert(x.contains(Point(1,1)))
    assert(x.contains(Point(0,1)))

    val x2 = getNeighbourCoordinates(Point(7,9),100,100)
    assert(x2.contains(Point(6,8)))
    assert(x2.contains(Point(6,9)))
    assert(x2.contains(Point(6,10)))
    assert(x2.contains(Point(7,10)))
    assert(x2.contains(Point(7,8)))
    assert(x2.contains(Point(8,8)))
    assert(x2.contains(Point(8,9)))
    assert(x2.contains(Point(8,10)))
    assert(!x2.contains(Point(7,9)))
  }

  test("testCalculateNeighbourMap"){
    val r = calculateNeighbourMap(Vector((Vector(Point(0,1),Point(1,1),Point(1,0)))),Vector(Vector(0,0),Vector(0,0)))
    assert(r(0)(0) == 0)
    assert(r(0)(1) == 1)
    assert(r(1)(0) == 1)
    assert(r(1)(1) == 1)

    val r2 = calculateNeighbourMap(
      Vector(
          Vector(Point(0,0),Point(0,1),Point(0,2), Point(1,2),Point(1,0),Point(2,0),Point(2,1),Point(2,2))
      ,
        Vector(Point(1,0),Point(1,1),Point(1,2), Point(2,2),Point(2,0),Point(3,0),Point(3,1),Point(3,2)))
      ,
      Vector(Vector(0,0,0,0),Vector(0,0,0,0),Vector(0,0,0,0),Vector(0,0,0,0))
    )
    assert(r2(1)(1) == 1)
    assert(r2(2)(1) == 1)
    assert(r2(1)(2) == 2)
    assert(r2(3)(0) == 1)
    assert(r2(3)(2) == 1)
    assert(r2(2)(2) == 2)
    assert(r2(2)(0) == 2)

  }

  test("testIndexFromXY"){
    assert(indexFromXY(Point(0,0),Rect(10,10)) == 0)
    assert(indexFromXY(Point(9,9),Rect(10,10)) == 99)
    assert(indexFromXY(Point(9,0),Rect(10,10)) == 9)
    assert(indexFromXY(Point(5,5),Rect(10,10)) == 55)
    assert(indexFromXY(Point(1,5),Rect(10,10)) == 51)
    assert(indexFromXY(Point(0,5),Rect(10,10)) == 50)

    assert(indexFromXY(Point(0,0),Rect(10,5)) == 0)
    assert(indexFromXY(Point(9,9),Rect(10,5)) == 99)
    assert(indexFromXY(Point(9,0),Rect(10,5)) == 9)
    assert(indexFromXY(Point(0,0),Rect(10,5)) == 0)

    assert(indexFromXY(Point(9,9),Rect(5,10)) == 54)
    assert(indexFromXY(Point(9,0),Rect(5,10)) == 9)
    assert(indexFromXY(Point(5,5),Rect(5,10)) == 30)
  }

  test("testGetCell"){
    val p = getCell(224,230,Rect(20,20),Rect(40,40))
    assert(p.isDefined)
    assert(p.get.x == 11)
    assert(p.get.y == 11)

    val p2 = getCell(224,10,Rect(20,20),Rect(40,40))
    assert(p2.isDefined)
    assert(p2.get.x == 11)
    assert(p2.get.y == 0)

    val p3 = getCell(50,10,Rect(20,20),Rect(40,40))
    assert(p3.isDefined)
    assert(p3.get.x == 2)
    assert(p3.get.y == 0)
  }

  test("testUncoverMap") {
    val r = uncoverMap(Point(0,0),4,4,None,
      Vector(Vector(0,0,1,1),Vector(0,0,1,1),Vector(1,1,0,0),Vector(1,1,0,0)),
      Vector(Vector(true,false,false,false),Vector(false,false,false,false),Vector(false,false,false,false),Vector(false,false,false,false))
    )

    assert(r(0)(0) == true)
    assert(r(0)(1) == true)
    assert(r(1)(1) == true)
    assert(r(1)(0) == true)
    assert(r(2)(0) == false)
    assert(r(0)(2) == false)
    assert(r(0)(3) == false)

    /*
    x 0 1 1
    1 0 0 1
    1 1 0 1
     */
    val r2= uncoverMap(Point(0,2),4,3,None,
      Vector(Vector(1,1,0),Vector(1,0,0),Vector(0,0,1),Vector(1,1,1)),
      Vector(Vector(false,false,true),Vector(false,false,false),Vector(false,false,false),Vector(false,false,false))
    )

    assert(r2(0)(2) == true)
    assert(r2(1)(2) == true)
    assert(r2(1)(1) == true)
    assert(r2(2)(1) == true)
    assert(r2(2)(0) == true)
    assert(r2(0)(0) == false)
    assert(r2(0)(1) == false)
    assert(r2(1)(0) == false)
    assert(r2(2)(2) == false)
    assert(r2(3)(0) == false)
    assert(r2(3)(1) == false)
    assert(r2(3)(2) == false)


  }

}
