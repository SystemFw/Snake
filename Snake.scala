import javax.swing._
import java.awt.{List => _, _}
import java.awt.event._
import scala.util.Random
import scala.util.chaining._
import Shared._

/** Recreate the basic version of the classic Nokia 3310 Snake, no extras
  * Behaviour taken from observing https://helpfulsheep.com/snake/
  *
  * To run: scala-cli Snake.scala
  */
object Main {
  def main(args: Array[String]): Unit = {
    val gui = Gui.start

    var state = State.initial

    while (true) {
      Thread.sleep(FrameRate) // TODO this is pretty rudimentary
      state = state.evolve(gui.getInput)
      gui.update(state)
    }
  }
}

object Shared {
  val Resolution = Point(84, 48)
  val BitMapSize = 4
  val FrameRate = 1000 / 120
  val Scale = 2

  val Dimensions = Resolution.divideBy(BitMapSize)
  val Origin = Dimensions.divideBy(2)
  val SnakeSize = 6
  val SlowDown = 12
  val PauseOnLoss = 150
  val FlickerDown = 20
  val FlickerUp = 30

  val DisplaySize = Resolution.times(Scale)
  val BackgroundColor = Color(170, 220, 0)
  val BorderSize = 10
  val ScoreBorderSize = 6
  val CanvasBorderSize = 2
}

case class State(
    snake: Vector[Point],
    direction: Point,
    apple: Point,
    eaten: Vector[Point] = Vector.empty,
    score: Int = 0,
    lostAt: Long = 0,
    time: Long = 0,
    drawSnake: Boolean = true,
    openMouth: Boolean = false
) {
  def evolve(nextDirection: Option[Point]): State = {
    def move = {
      val directionNow =
        nextDirection.filter(_ != direction.opposite).getOrElse(direction)

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

      val aboutToEat =
        if (
          discardEaten.snake.head.move(directionNow).wrap(Dimensions) == apple
        )
          discardEaten.copy(openMouth = true)
        else discardEaten.copy(openMouth = false)

      val eat =
        if (aboutToEat.snake.head == apple)
          aboutToEat.copy(
            apple = State.newApple(aboutToEat.snake),
            eaten = aboutToEat.apple +: aboutToEat.eaten,
            score = aboutToEat.score + 9
          )
        else aboutToEat

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

  def render: Vector[Point] = {
    val renderedSnake: Vector[Point] = if (drawSnake) {
      val head =
        (if (!openMouth) State.head else State.head)//State.eatingHead)
          .apply(direction)
          .at(snake.head)

      val body =
        snake.sliding(3).flatMap {
          case Vector(p0, p1, p2) =>
            def direction(head: Point, tail: Point) = {
              val p = Point((head.x - tail.x), (head.y - tail.y))
              val wrapped = Point(p.x.sign, p.y.sign)

              if (p.x != wrapped.x || p.y != wrapped.y) wrapped.opposite
              else p
            }

            val dir1 = direction(p0, p1)
            val dir2 = direction(p1, p2)

            // val body =
            //   if (eaten.contains(p1))
            //     State.eatenApple.at(p1)
            //   else if (dir1.x == dir2.x || dir1.y == dir2.y)
            //     State.body(dir1).at(p1)
            //   else
            //     State.corners(dir2 -> dir1).at(p1)

            val body =
                State.body(dir1).at(p1)


            val tail =
              if (p2 == snake.last) State.body(dir2).at(p2) else Vector.empty

            body ++ tail
          case _ => sys.error("impossible")
        }

      head ++ body
    } else Vector.empty

    (renderedSnake ++ State.apple.at(apple))
      .flatMap(_.times(Scale).square(Scale))
  }

  def renderScore: String =
    String.format("%04d", score)
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
-*--
*-*-
-*--
----
""".pipe(Bitmap.parse)

  val eatenApple = """
--*--
-***-
**-**
-***-
--*--
""".pipe(Bitmap.parse)

  // This is rudimentary, since rotation isn't relative, but that's
  // how the original game does it
  def rotations(bitmap: Bitmap): Map[Point, Bitmap] =
    Map(
      Point.right -> bitmap,
      Point.left -> bitmap.mirror,
      Point.up -> bitmap.rotate(-1),
      Point.down -> bitmap.rotate(1).mirror
    )

  val head = """
*---
-**-
***-
----
""".pipe(Bitmap.parse).pipe(rotations)

  val eatingHead = """
-----
--*-*
-*-*-
****-
----*
""".pipe(Bitmap.parse).pipe(rotations)

  val body = """
----
**-*
*-**
----
""".pipe(Bitmap.parse).pipe(rotations)

  // directions relative to going towards the head from the tail
  val corners: Map[(Point, Point), Bitmap] = Map(
    (Point.right, Point.up) -> """
--*--
--**-
-***-
****-
-----
""",
    (Point.up, Point.right) -> """
-----
-----
--***
--**-
---*-
""",
    (Point.right, Point.down) -> """
-----
-----
-***-
****-
--*--
""",
    (Point.down, Point.left) -> """
---*-
--**-
****-
-***-
-----
""",
    (Point.left, Point.down) -> """
-----
-----
--**-
--***
--*--
""",
    (Point.down, Point.right) -> """
---*-
--**-
--***
--**-
-----
""",
    (Point.left, Point.up) -> """
--*--
--**-
--**-
--***
-----
""",
    (Point.up, Point.left) -> """
-----
-----
****-
-***-
---*-
"""
  ).view.mapValues(Bitmap.parse).toMap
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

  def square(side: Int): Vector[Point] =
    0.until(side).flatMap(x => 0.until(side).map(y => move(Point(x, y)))).toVector

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


// case class Bitmap(points: Vector[Point]) {
//   def at(p: Point): Vector[Point] =
//     points.map(p.times(BitMapSize).move(_))
// }
// object Bitmap {
//   def parse(mask: String) =
//     mask
//       .filterNot(_.isWhitespace)
//       .zipWithIndex
//       .collect { case ('*', i) => Point(i % BitMapSize, i / BitMapSize) }
//       .pipe(Bitmap(_))
// }

// TODO
// Fixed size bitmaps are too restrictive and the real game doesn't use
// them. The alternative appears very complex though: i.e. full-on vectors,
// shapes, bounding boxes, etc
/** 5x5 bitmaps */
case class Bitmap(points: Vector[Point]) {
  val size = BitMapSize - 1

  def at(p: Point): Vector[Point] =
    points.map(p.times(BitMapSize).move(_))

  /** rotate by 90 degrees, n times, +/- = clockwise/anticlockwise */
  def rotate(n: Int): Bitmap = Bitmap {
    def go(points: Vector[Point], times: Int): Vector[Point] =
      if (times == 0) points
      else go(points.map(p => Point(size - p.y, p.x)), times - 1)

    go(points, Point.wrap(n, 4))
  }

  /** Mirrors, direction akin to turning a page */
  def mirror: Bitmap =
    Bitmap(points.map(p => Point(size - p.x, p.y)))
}
object Bitmap {

  /** Takes a bitmap string, with '*' meaning bit set
    */
  def parse(spec: String): Bitmap = Bitmap {
    val matrix = spec.trim.split('\n').map(_.toVector)

    // assert(matrix.length == BitMapSize)
    // assert(matrix.forall(_.length == matrix.length))

    matrix.zipWithIndex
      .flatMap { case (line, y) =>
        line.zipWithIndex.map {
          case ('*', x) => Some(Point(x, y))
          case _        => None
        }
      }
      .flatten
      .toVector
  }
}

class Gui extends JPanel {

  // Writes from event listeners which run on single EDT thread: no atomics needed
  // Reads from main thread: volatile needed
  @volatile private var input: Option[Point] = None
  // All reads and writes from single EDT thread: can be standard references
  private var image: Vector[Point] = Vector()
  private val score = new JLabel("Score")

  Point.directions.keys.foreach { direction =>
    def add(name: String)(action: AbstractAction) = {
      getActionMap.put(name, action)
      getInputMap.put(KeyStroke.getKeyStroke(name), name)
    }

    add(direction) { _ => input = Point.directions.get(direction) }
    add(s"released $direction") { _ => input = None }
  }

  setBorder(
    BorderFactory.createEmptyBorder(
      BorderSize,
      BorderSize,
      BorderSize,
      BorderSize
    )
  )
  setBackground(BackgroundColor)
  setLayout(new BorderLayout)
  add(new Canvas, BorderLayout.CENTER)
  score.setBorder(
    BorderFactory.createCompoundBorder(
      BorderFactory.createEmptyBorder(0, 0, ScoreBorderSize, 0),
      BorderFactory.createMatteBorder(0, 0, CanvasBorderSize, 0, Color.black)
    )
  )
  add(score, BorderLayout.NORTH)

  def getInput: Option[Point] = input

  def update(state: State): Unit =
    SwingUtilities.invokeLater { () =>
      image = state.render
      score.setText(state.renderScore)
      repaint()
    }

  class Canvas extends JComponent {
    setBorder(BorderFactory.createLineBorder(Color.black, CanvasBorderSize))

    // TODO build proper image instead
    override def paintComponent(g: Graphics) =
      image.foreach { point =>
        g.drawLine(
          point.x + CanvasBorderSize,
          point.y + CanvasBorderSize,
          point.x + CanvasBorderSize,
          point.y + CanvasBorderSize
        )
      }

    override def getPreferredSize =
      Dimension(
        DisplaySize.x + 2 * CanvasBorderSize,
        DisplaySize.y + 2 * CanvasBorderSize
      )
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


