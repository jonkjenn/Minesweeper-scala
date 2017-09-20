import java.awt.image.BufferedImage
import java.io._
import java.awt.{Color}
import java.awt.Font

import scala.swing._
import scala.swing.event._
import scala.swing.event.MouseClicked
import scala.swing.Reactions

import scala.annotation.tailrec
import scala.util.Random

package com.jonkjenn.minesweeper {

  import java.awt.{Dimension, Stroke}

  object Helpers {

    def getCell(x:Int,y:Int,cellSize:Rect,cellCount:Rect): Option[Point] ={
      val p = Point(math.floor(x/cellSize.width).toInt,math.floor(y/cellSize.height).toInt)
      p match {
        case i if p.x < cellCount.width && p.y < cellCount.height && p.x >= 0 && p.y >= 0 => Option(i)
        case _ => Option.empty
      }
    }

    def createMap(width:Int,height:Int,mineCount:Int): Tuple4[Vector[Vector[Boolean]],Vector[Vector[Boolean]],Vector[Vector[Int]],Vector[Vector[Either[CellState,Int]]]] ={
      val falseMap = Vector.fill[Boolean](width, height)(false)
      val uncovered = Vector.fill[Boolean](width, height)(false)
      val zeroMap = Vector.fill[Int](width, height)(0)

      val mineLocations = xyFromIndex(randomLocations(mineCount, width * height),Rect(width,height))

      //create new mines with true at mine locations
      val mineMap = update2d(mineLocations.map(x => (x,true)),falseMap)

      //calculate neighbours
      val neighbours = calculateNeighbourMap( mineLocations.map(x => getNeighbourCoordinates(x, width, height)) ,zeroMap)

      /*print2D(uncovered)
      print2D(mineMap)
      print2D(neighbours)*/

      //
      val data:Vector[Vector[Either[CellState,Int]]] = convertData(uncovered,mineMap,neighbours)
      Tuple4[Vector[Vector[Boolean]],Vector[Vector[Boolean]],Vector[Vector[Int]],Vector[Vector[Either[CellState,Int]]]](uncovered,mineMap,neighbours,data)
    }

    def convertData(uncovered:Vector[Vector[Boolean]],mineMap:Vector[Vector[Boolean]],neighbours:Vector[Vector[Int]]): Vector[Vector[Either[CellState,Int]]] ={

        //Vector.fill[Either[CellState,Int]](width,height)(Left(Initial))
        (for (x <- uncovered.indices) yield {
          (for (y <- uncovered(x).indices) yield {
            uncovered(x)(y) match {
              case true if mineMap(x)(y) => Left(Mine)
              case true if neighbours(x)(y) == 0 => Left(Discovered)
              case true if neighbours(x)(y) > 0 => Right(neighbours(x)(y))
              case false => Left(Initial)
            }
          }).toVector
        }).toVector
      }


    def print2D[A](data: Iterable[Iterable[A]]): Unit ={
      data.transpose.foreach(
        i => {
          i.foreach(
            j => scala.Console.print(j.toString + " ")
          )
          scala.Console.print("\n")
        }
      )
    }

    def calculateNeighbourMap(mineData:Vector[Vector[Point]],zeroMap: Vector[Vector[Int]]): Vector[Vector[Int]] =
    {
      mineData.foldLeft(zeroMap) { (z, i) =>
            i.foldLeft(z) { (w, j) =>
              w.updated(j.x, w(j.x).updated(j.y, w(j.x)(j.y) + 1))
            }
        }
    }

    //2D update
    def update2d[A](pointvals: Vector[Tuple2[Point, A]], map: Vector[Vector[A]]): Vector[Vector[A]] = {
      pointvals.foldLeft(map)((z, i) => z.updated(i._1.x, z(i._1.x).updated(i._1.y, i._2)))
    }

    def uncoverMap(p: Point, width: Int, height: Int, parent: Option[Point], nbs: Vector[Vector[Int]], uncovered: Vector[Vector[Boolean]]): Vector[Vector[Boolean]] = {
      val n = getNeighbourCoordinates(p, width, height, parent)
      val v = values(n, nbs)
      //How many neighbours at each point
      val uco = values(n, uncovered)
      //If are uncovered
      val filtered = (n, v, uco).zipped.toVector.filter(x => x._2 == 0 && x._3 == false).map(x => x._1) //Keep only those with 0 neighbours

      //val updated = update2d(filtered.zipAll(Vector[Boolean](), Point(), true), uncovered) //Updated uncovered
      val updated = update2d(n.zipAll(Vector[Boolean](),Point(),true), uncovered)

      return filtered
        .foldLeft(updated)((z, i) =>
          uncoverMap(i, width, height, Option(p), nbs, z)
        )

      //filtered.flatMap( x => uncoverMap(x,width,height,Option(p),nbs,vec++filtered))//repeat on every 0 neighbour
      //filtered.fold(_)((z,i) => zeroNeighbours(i,width,height,p,nbs,vec))
    }

    def getNeighbourCoordinates(p: Point, width: Int, height: Int, ignore: Option[Point] = Option.empty): Vector[Point] = {
      return (for (i <- p.x - 1 until p.x + 2;
                   j <- p.y - 1 until p.y + 2
                   if !(i == p.x && j == p.y)
                     && (ignore.isEmpty || !(i == ignore.get.x && j == ignore.get.y))
                     && i >= 0 && j >= 0
                     && i < width && j < height
      ) yield {
        Point(i, j)
      }).toVector
    }

    def randomLocations(numRandom: Int, numTotal: Int): Vector[Int] = {
      val vals: Vector[Int] = (for (x <- 0 until numTotal) yield {
        x
      }).toVector

      //numbers from 1-numTotal
      def recurve(numResult: Int, result: Vector[Int], values: Vector[Int]): Vector[Int] = {
        if (result.length >= numResult) {
          return result
        }
        val r = Random.nextInt(values.length)
        recurve(numResult, result :+ values(r), values.diff(Seq(values(r)))) //move a random value from values over to result
      }

      return recurve(numRandom, Vector.empty, vals)
    }

    /*def createMap(): Unit = {
      //cells = (for {y <- 0 until height; x <- 0 until width} yield (random.nextBoolean())).toArray.grouped(height).toArray
    }*/

    def xyFromIndex(index: Vector[Int], rect: Rect): Vector[Point] = {
      rect match{
        case Rect(h,w) if h == 0 && w == 0 => return Vector[Point]()
        case _ => index.map( x => Point(x % rect.width, math.floor(x.toFloat / rect.width).toInt))
      }
    }

    def xyFromIndex(index: Int, rect: Rect): Option[Point] = {
      rect match{
        case Rect(h,w) if h == 0 && w == 0 => return Option.empty//
        case _ => Option(Point(index % rect.width, math.floor(index.toFloat / rect.width).toInt))
      }
    }

    def indexFromXY(p:Point,rect:Rect): Int ={
      p.x + p.y*rect.width
    }

    def uncoverPositions(mineMap:Vector[Vector[Boolean]],uncovered: Vector[Vector[Boolean]]): Vector[Vector[Boolean]] ={
      return uncovered.zip(mineMap).map {
        a => {
          a._1.zip(a._2).map {
            b => b._1 || b._2
          }
        }
      }
    }

    def uncover(pos: Point, width: Int, height: Int, uncovered: Vector[Vector[Boolean]], mineMap: Vector[Vector[Boolean]], nbs: Vector[Vector[Int]]): Vector[Vector[Boolean]] = {
      //Already uncovered
      if(uncovered(pos.x)(pos.y)){
        return uncovered
      }

      //Uncover the current position since not mine
      val uncoveredCurrent = update2d(Vector((pos, true)), uncovered)

      //Uncover all mines
      if(mineMap(pos.x)(pos.y)){
          return uncoverPositions(mineMap,uncoveredCurrent)
      }

      nbs(pos.x)(pos.y) match {
        //Only uncover current
        case x if x > 0 => {
          return uncoveredCurrent
        }
        case 0 => {
          //uncover all neighbours with 0 mine-neighbours
          return uncoverMap(pos, width, height, Option.empty, nbs, uncoveredCurrent) //.foldLeft(uncovered)((z,i) => z.updated(i.x,z(i.x).updated(i.y,true)))
        }
      }

    }

    //Get the values of the 2d map at all the 2d point locations, no out of bound checks
    def values[A](points: Vector[Point], map: Vector[Vector[A]]): Vector[A] = {
      return points.foldLeft(Vector[A]()) {
        (z, i) => z :+ map(i.x)(i.y)
      }
    }

  }

      //v.foreach(x=>println(x))


      /*neighbours.foreach(
      i => {
        i.foreach(
          j => scala.Console.print(j.toString + " ")
        )
        scala.Console.print("\n")
      }
    )*/

  case class Point(x: Int = 0, y: Int = 0)

  case class Rect(width: Int, height: Int)

  sealed trait CellState
  case object Initial extends CellState
  case object Flagged extends CellState
  case object Mine extends CellState
  case object Discovered extends CellState
  /*object CellState extends Enumeration {
    type CellState = Value
    val initial, flagged, mine, discovered = values
  }*/

  class Cell(state: CellState, neighbours: Int)

  object GUI extends SimpleSwingApplication {
    //, val data:Vector[Vector[Either[CellState,Int]]]) extends JPanel {
    import com.jonkjenn.minesweeper.Helpers._

    val width: Int = 30
    val height: Int = 30
    val mineCount: Int = 100
    val cellSize = Rect(20, 20)

    var uncovered: Vector[Vector[Boolean]] = Vector.empty
    var neighbours: Vector[Vector[Int]] = Vector.empty
    var mineMap: Vector[Vector[Boolean]] = Vector.empty

    var data: Vector[Vector[Either[CellState, Int]]] = Vector.empty

    var hl: Point = Point(-1, -1)

    val markedMines: TextField = new TextField() {
      editable = false
    }
    val timer: TextField = new TextField("0") {
      editable = false
    }
    val canvas = new Canvas(cellSize, Rect(width, height)) {}

    var state = 0

    var startTime: Long = 0

    val t = new java.util.Timer()
    val task = new java.util.TimerTask {
      override def run(): Unit = {
        startTime match {
          case 0 => startTime = System.nanoTime()
          case t => timer.text = (math.round(((System.nanoTime() - t) * math.pow(10, -9))).toLong).toString
        }
      }
    }

    def updateMap(): Unit = {
      val d = createMap(width, height, mineCount)
      uncovered = d._1
      mineMap = d._2
      neighbours = d._3

      canvas.data(convertData(
        uncovered,
        mineMap,
        neighbours
      ))
      try {
        t.cancel()
      }catch{case _: Throwable =>}
      t.purge()
      timer.text = "0"
      state = 0
    }

    def top = new MainFrame {

      contents = new BoxPanel(Orientation.Vertical) {
        contents += new BoxPanel(Orientation.Horizontal) {
          maximumSize = new Dimension(500, 50)
          contents += markedMines
          contents += new Button("Smiley") {
            reactions += {
              case ButtonClicked(_) => {
                updateMap()
              }
            }
          }
          contents += timer
        }
        contents += canvas
      }
      //preferredSize = new Dimension(cellSize.width*width,cellSize.height*height)

      menuBar = new MenuBar {
        contents += new Menu("Game") {
          contents += new MenuItem(Action("New game") {
            updateMap()
          })
        }
      }


      size = new Dimension(canvas.size.width + 10, canvas.size.height + 10)

      listenTo(canvas.mouse.clicks, canvas.mouse.moves, canvas.keys)

      val d = createMap(width, height, mineCount)
      uncovered = d._1
      mineMap = d._2
      neighbours = d._3

      reactions += {
        case KeyPressed(_, Key.F2, _, _) => {
          updateMap()
        }
        case MouseMoved(_, point, _) => {
          getCell(point.x, point.y, cellSize, Rect(width, height)) match {
            case Some(point) if point != canvas.hl => {
              canvas.hl = Some(Point(point.x, point.y))
              canvas.repaint()
            }
            case None =>
          }
        }
        case MouseReleased(_, point, _, _, _) => {
          state match {
            case -1 =>
            case 0 => {
              state = 1
              try {
                t.scheduleAtFixedRate(task, 0, 1000)
              }catch{case e:IllegalStateException =>}
              checkMouseClick(point)
            }
            case 1 =>{
              checkMouseClick(point)
            }
          }

        }
      }
      canvas.data(d._4)
    }

    def checkMouseClick(point:java.awt.Point): Unit ={
      val m = getCell(point.x, point.y, cellSize, Rect(width, height))
      m match {
        case Some(p) => {
          mineMap(p.x)(p.y) match{
            case true if state == 1=> {
              state = -1
              try {
                t.cancel()
                t.purge()
              }catch{case _:Throwable =>}
            }
            case _ =>
          }
          uncovered = uncover(m.get, width, height, uncovered, mineMap, neighbours)
          canvas.data(convertData(
            uncovered,
            mineMap,
            neighbours
          ))
        }
        case None =>
      }
    }
  }
    //(cellSize:Rect,numcells: Rect)
    class Canvas(cellSize:Rect,numcells: Rect) extends Panel{
    //val img = new BufferedImage(cellSize.width*numcells.width,cellSize.height*numcells.height,BufferedImage.TYPE_INT_RGB)
    var data:Vector[Vector[Either[CellState,Int]]] = Vector.empty

    var hl:Option[Point] = Option.empty

    focusable = true

    override def paintComponent(g: Graphics2D): Unit ={
      requestFocus
      for(x <- data.indices; y<- data(x).indices){
        val dc = Point(x*cellSize.width,y*cellSize.height)
        data(x)(y) match{
          case Left(i) =>{
            hl match{
              case Some(hl) if hl.x == x && hl.y == y => {
                drawCellState(g,dc,i,true)
              }
              case _ => drawCellState(g,dc,i)
            }
          }
          case Right(i) =>{
            drawCellNumber(g,dc,i)
          }
        }
      }
    }

    def data(d: Vector[Vector[Either[CellState,Int]]]): Unit ={
      data = d

      /*data.foreach(
        i => {
          i.foreach(
            j => scala.Console.print(j.toString + " ")
          )
          scala.Console.print("\n")
        }
      )*/

      repaint()
    }

    def drawCellNumber(g:Graphics2D,cellPos:Point,n:Int):Unit={
      g.setColor(new Color(220, 220, 220))
      g.fillRect(cellPos.x,cellPos.y,cellSize.width,cellSize.height)
      n match{
        case 1 => g.setColor(Color.blue)
        case 2 => g.setColor(new Color(30, 90, 30))
        case 3 => g.setColor(Color.red)
        case 4 => g.setColor(new Color(2,30,66))
        case 5 => g.setColor(new Color(35,0,9))
        case 6 => g.setColor(new Color(53, 191, 182))
        case 7 => g.setColor(Color.black)
        case 8 => g.setColor(Color.gray)
      }
      g.setFont(g.getFont.deriveFont(Font.BOLD, 14f))
      g.drawString(n.toString,cellPos.x+5,cellPos.y+cellSize.height-2)
      g.setColor(Color.darkGray)
      g.drawRect(cellPos.x,cellPos.y,cellSize.width,cellSize.height)
    }

    def drawCellState(g:Graphics2D,cellPos:Point,state:CellState,highlighted:Boolean = false): Unit ={

      state match {
        case Initial =>{
          highlighted match{
            case true => g.setColor(new Color(180, 180, 180))
            case _ => g.setColor(new Color(200, 200, 200))
          }
          g.fillRect(cellPos.x,cellPos.y,cellSize.width,cellSize.height)
          g.setColor(new Color(160, 160, 160))
          g.drawRect(cellPos.x,cellPos.y,cellSize.width,cellSize.height)
        }
        case Discovered =>{
          g.setColor(new Color(220, 220, 220))
          g.fillRect(cellPos.x,cellPos.y,cellSize.width,cellSize.height)
          g.setColor(new Color(100, 100, 100))
          g.drawRect(cellPos.x,cellPos.y,cellSize.width,cellSize.height)
        }
        case Flagged =>{
          g.setColor(Color.red)
          g.fillRect(cellPos.x,cellPos.y,cellSize.width,cellSize.height)
        }
        case Mine =>{
          g.setColor(Color.black)
          g.fillRect(cellPos.x,cellPos.y,cellSize.width,cellSize.height)
        }
      }
    }
  }
}
