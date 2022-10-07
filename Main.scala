import javax.swing._
import java.awt.{List => _, _}
import java.awt.event._
import scala.util.Random
import scala.util.chaining._
import Shared._

/** Recreate the basic version of the classic Nokia 3310 Snake, no extras
  * Behaviour taken from observing https://helpfulsheep.com/snake/
  */
object Main {
  def main(args: Array[String]): Unit = {
    val gui = Gui.start

    //TODO should I just use Vector[Point] instead of Set



   /*
    var state = State.initial

    SlowDown = 45

    while (true) {
      Thread.sleep(FrameRate) // TODO this is pretty rudimentary
      state = state.evolve(gui.getInput)
      gui.update(state)
    }
    // */

//    /*
    var state: State =
      State(
        Vector.range(0, 20).map(x => Origin.move(Point.left.times(x))),
        Point.right,
        Point(0, 0)
      )

    def turn(input: Option[Point]) = {
      Thread.sleep(200)
      state = state.evolve(input)
      gui.update(state)
    }

    gui.update(state)
    turn(Some(Point.up))
    turn(None)
    turn(None)
    turn(Some(Point.right))
    turn(None)
    turn(Some(Point.down))
    turn(None)
    turn(Some(Point.right))
    turn(None)
    turn(None)
    turn(None)
    turn(None)


    Thread.sleep(60000)
    // */
  }
}

object Shared {
  // The snake doesn't the same relative dimensions of the original game,
  // because it uses fixed size sprites, whereas the game uses variable
  // ones, the dimensions of the game area are calculated so that the
  // snake eats itself at the same score as the original
  val Dimensions = Point(36, 17)
  val BitMapSize = 5

  val FrameRate = 1000 / 120
  val SlowDown = 1
  val Scale = 5

  val FullScale = Scale * BitMapSize

  val DisplaySize = Dimensions.times(FullScale)

  val Origin = Dimensions.divideBy(2)
  val SnakeSize = 6

  val PauseOnLoss = 120
  val FlickerDown = 20
  val FlickerUp = 30

  def p[A](v: A): Unit =
    scala.concurrent.Future(println(v))(scala.concurrent.ExecutionContext.global)


  val d = Point.right
  /*
   *    ++
   *   ++
   *   +  +
   *   ++++
   */
  val p = Vector(
    Point(10, 10),
    Point(9, 10),
    Point(9, 11),
    Point(8, 11),
    Point(8, 12),
    Point(8, 13),
    Point(9, 13),
    Point(10, 13),
    Point(10, 14),
    Point(9, 14)
  )

}

case class State(
    snake: Vector[Point],
    direction: Point,
    apple: Point,
    eaten: Vector[Point] = Vector.empty,
    score: Int = 0,
    lostAt: Long = 0,
    time: Long = 0,
    drawSnake: Boolean = true
) {
  def evolve(nextDirection: Option[Point]): State = {
    def move = {
      val directionNow =
        nextDirection.filter(_ != direction.opposite).getOrElse(direction)

      // TODO duplicate points when about to fully digest an apple
      val advanceOrGrow =
        copy(
          snake = snake.head.move(directionNow).wrap(Dimensions) +: {
            if (eaten.nonEmpty && snake.head == eaten.head) snake
            else snake.init
          },
          direction = directionNow
        )

      val discardEaten =
        if (eaten.nonEmpty && advanceOrGrow.snake.last == eaten.last)
          advanceOrGrow.copy(eaten = eaten.init)
        else advanceOrGrow


      val eat =
        if (discardEaten.snake.head == apple)
          discardEaten.copy(
            apple = State.newApple(discardEaten.snake),
            eaten = discardEaten.apple +: discardEaten.eaten,
            score = discardEaten.score + 9 // TODO score is score + speed value (1-9), should I keep it to 9?
          )
        else discardEaten


      val checkLoss =
        if (eat.snake.tail.contains(eat.snake.head)) this.copy(lostAt = time)
        else eat

      if (time % SlowDown == 0) checkLoss
      else this
    }

    def flickerOnLoss = {
      val flicker =
        (time / FlickerDown) % ((FlickerDown + FlickerUp) / FlickerDown) == 0

      if (time - lostAt > PauseOnLoss) State.initial
      else if (!flicker) copy(drawSnake = true)
      else copy(drawSnake = false)
    }

    if (lostAt > 0) flickerOnLoss
    else move
  }.copy(time = time + 1)

  def render: Set[Point] = {
    val directions =
      snake.sliding(2).toVector.map {
        case Vector(Point(x, y), Point(xx, yy)) =>
          Point((x - xx).sign, (y - yy).sign)
        case _ => sys.error("impossible")
      }

    // TODO corners code fails to wrap around
    // TODO corners sprites not perfect
    // GOOD: right-up, down-right, down-left, right-down
    val renderedSnake: Set[Point] = if (drawSnake) {
      State.head(direction).at(snake.head) ++
      State.tail.at(snake.last) ++
      snake.sliding(3).toVector.flatMap {
        case Vector(p0, p1, p2) =>
          val dir1 = Point((p1.x - p0.x).sign, (p1.y - p0.y).sign)
          val dir2 = Point((p2.x - p1.x).sign, (p2.y - p1.y).sign)
          if (eaten.contains(p1)) State.eatenApple(dir1).at(p1) // TODO angular apples?
          else if (dir1 == dir2) State.body(dir1).at(p1)
          else State.corners(dir2.opposite -> dir1.opposite).at(p1)
        case _ => sys.error("impossible")
      }.toSet

      // snake.tail.init..zip(directions).flatMap { case (p, direction) =>
      //   if (eaten.contains(p)) State.eatenApple(direction).at(p)
      //   else State.body(direction).at(p)
      // }.toSet
    } else Set.empty
    val renderedApple = State.apple.at(apple)

    (renderedSnake ++ renderedApple)
      .flatMap(_.times(Scale).square(Scale))
  }
}
object State {
  def initial: State = {
    val snake =
      Vector.range(0, SnakeSize).map(x => Origin.move(Point.left.times(x)))
    State(snake, Point.right, newApple(snake))
  }

  def newApple(snake: Vector[Point]): Point = {
    val apple = Point(
      Random.nextInt(Dimensions.x),
      Random.nextInt(Dimensions.y)
    )

    if (snake.contains(apple)) newApple(snake)
    else apple
  }

    val apple = """
-----
--*--
-*-*-
--*--
-----
""".pipe(Bitmap.parse)

  val head = """
-----
--*--
-*-**
*****
-----
""".pipe(Bitmap.parse).pipe(rotations)


  val body = """
-----
-----
-****
****-
-----
""".pipe(Bitmap.parse).pipe(rotations)

  // TODO add better sprite for tail, maybe just shave from top band
  val tail = """
-----
--*--
-***-
--*--
-----
""".pipe(Bitmap.parse)


  val corner = """
--*--
--**-
--**-
****-
-----
""".pipe(Bitmap.parse)

  val eatenApple = """
-----
-***-
*---*
-***-
-----
""".pipe(Bitmap.parse).pipe(rotations)

  val altApple = """
--*--
-***-
**-**
-***-
--*--
"""

  // GOOD with original sprites: right-up, down-right, down-left, right-down

  // going towards the head
  val rightUp = """
-----
---*-
--**-
-***-
-----
""".pipe(Bitmap.parse)

  val rightDown = """
-----
****-
--**-
--**-
--*--
""".pipe(Bitmap.parse)

    val downRight = """
-*---
-*---
-****
-***-
-----
""".pipe(Bitmap.parse)

  val downLeft = """
---*-
---*-
****-
-***-
-----
""".pipe(Bitmap.parse)

  //
  val leftDown = """
-----
-****
-**--
-**--
--*--
""".pipe(Bitmap.parse)

  val leftUp = """
--*--
-**--
-**--
-****
-----
""".pipe(Bitmap.parse)


  val upRight = """
-----
-***-
-****
-*---
-*---
""".pipe(Bitmap.parse)

  val upLeft = """
-----
-***-
****-
---*-
---*-
""".pipe(Bitmap.parse)


  val corners: Map[(Point, Point), Bitmap] = Map(
    (Point.right, Point.up) -> rightUp,
    (Point.right, Point.down) -> rightDown,
    (Point.left, Point.down) -> leftDown,
    (Point.left, Point.up) -> leftUp,
    (Point.down, Point.right) -> downRight,
    (Point.down, Point.left) -> downLeft,
    (Point.up, Point.right) -> upRight,
    (Point.up, Point.left) -> upLeft
  )


  // This is rudimentary, since rotation isn't relative, but that's
  // how the original game does it
  def rotations(bitmap: Bitmap): Map[Point, Bitmap] =
    Map(
      Point.right -> bitmap,
      Point.left -> bitmap.mirror,
      Point.up -> bitmap.rotate(-1),
      Point.down -> bitmap.rotate(1).mirror
    )
}

case class Point(x: Int, y: Int) {
  def move(to: Point): Point =
    Point(x + to.x, y + to.y)

  def times(k: Int): Point =
    Point(x * k, y * k)

  def divideBy(k: Int): Point =
    Point(x / k, y / k)

  def opposite: Point =
    times(-1)

  def square(side: Int): Set[Point] =
    0.to(side).flatMap(x => 0.to(side).map(y => move(Point(x, y)))).toSet

  def wrap(limit: Point) =
    Point(Point.wrap(x, limit.x), Point.wrap(y, limit.y))
}
object Point {
  def up: Point = Point(0, -1)
  def down: Point = Point(0, 1)
  def left: Point = Point(-1, 0)
  def right: Point = Point(1, 0)

  def directions: Map[String, Point] = Map(
    "UP" -> up,
    "DOWN" -> down,
    "LEFT" -> left,
    "RIGHT" -> right
  )

  def wrap(n: Int, limit: Int): Int =
    n.sign.min(0).abs * limit + (n % limit)
}

// TODO
// Fixed size bitmaps are too restrictive and the real game doesn't use
// them. The alternative appears very complex though

/** 5x5 bitmaps */
case class Bitmap(points: Set[Point]) {
  val size = BitMapSize - 1

  def at(p: Point): Set[Point] =
    points.map(p.times(BitMapSize).move(_))

  /** rotate by 90 degrees, n times, +/- = clockwise/anticlockwise */
  def rotate(n: Int): Bitmap = Bitmap {
    def go(points: Set[Point], times: Int): Set[Point] =
      if (times == 0) points
      else go(points.map(p => Point(size - p.y, p.x)), times - 1)

    go(points, Point.wrap(n, 4))
  }

  /** Mirrors, direction akin to turning a page */
  def mirror: Bitmap =
    Bitmap(points.map(p => Point(size - p.x, p.y)))
}
object Bitmap {
  /**
    * Takes a bitmap string, with '*' meaning bit set
    */
  def parse(spec: String): Bitmap = Bitmap {
    val matrix = spec.trim.split('\n').map(_.toVector)

    assert(matrix.length == BitMapSize)
    assert(matrix.forall(_.length == matrix.length))

    matrix.zipWithIndex.flatMap { case (line, y) =>
      line.zipWithIndex.map {
        case ('*', x) => Some(Point(x, y))
        case _ => None
      }
    }.flatten.toSet
  }
}


class Gui extends JPanel {

  // Writes from event listeners which run on single EDT thread: no atomics needed
  // Reads from main thread: volatile needed
  @volatile private var input: Option[Point] = None
  // All reads and writes from single EDT thread: can be standard references
  private var image: Set[Point] = Set()
  private val score = new JLabel("Score")

  Point.directions.keys.foreach { direction =>
    def add(name: String)(action: AbstractAction) = {
      getActionMap.put(name, action)
      getInputMap.put(KeyStroke.getKeyStroke(name), name)
    }

    add(direction) { _ => input = Point.directions.get(direction) }
    add(s"released $direction") { _ => input = None }
  }

  setLayout(new BorderLayout)
  add(new Canvas, BorderLayout.CENTER)
  add(score, BorderLayout.NORTH)

  def getInput: Option[Point] = input

  def update(state: State): Unit =
    SwingUtilities.invokeLater { () =>
      image = state.render
      score.setText(s"${DisplaySize.x} x ${DisplaySize.y} ${state.score} (${state.snake.head.x}, ${state.snake.head.y}) (${state.apple.x}, ${state.apple.y})")
      repaint()
    }

  class Canvas extends JComponent {
    // TODO build proper image instead
    override def paintComponent(g: Graphics) =
      image.foreach { point =>
        g.drawLine(point.x, point.y, point.x, point.y)
      }

    override def getPreferredSize =
      Dimension(DisplaySize.x, DisplaySize.y)
  }
}
object Gui {
  def start: Gui = {
    val gui = new Gui
    SwingUtilities.invokeLater { () =>
      val app = new JFrame("Snake")
      app.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
      app.setResizable(false)
      app.add(gui)
      app.pack
      app.setLocationRelativeTo(
        null
      ) // centers the window if called after `pack`
      app.setVisible(true)
    }
    gui
  }
}
