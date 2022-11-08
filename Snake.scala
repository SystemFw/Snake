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
  val BitMapSize = 4 // TODO maybe rename to sprite size
  val FrameRate = 1000 / 120
  val Scale = 2

  val Dimensions = Resolution.times(1 / BitMapSize.toDouble)
  val Origin = Dimensions.times(0.5)
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
  // TODO make input not Optional
  def evolve(nextDirection: Option[Point]): State = {
    def move = {
      // TODO track direction with separate class
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
        (if (!openMouth) State.head else State.headOpen)
          .apply(direction)
          .at(snake.head)

      // TODO compute tail in advance

      // TODO once direction is tracked, this should be a sliding(2), and simpler
      val body =
        snake.sliding(3).flatMap {
          case Vector(p0, p1, p2) =>
            def direction(head: Point, tail: Point) = {
              val p = Point((head.x - tail.x), (head.y - tail.y))
              val wrapped = Point(p.x.sign, p.y.sign)

              if (p.x != wrapped.x || p.y != wrapped.y) wrapped.opposite
              else p
            }

            // TODO better names for these, e.g. "from", "to"
            val dir1 = direction(p0, p1)
            val dir2 = direction(p1, p2)
            val from = direction(p1, p2)
            val to = direction(p0, p1)

            // TODO full corners
            val body =
              if (eaten.contains(p1))
                State.bodyFull(to).at(p1)
              else if (to.x == from.x || to.y == from.y)
                State.body(to).at(p1)
              else
                State.turn(from -> to).at(p1)

            val tail =
              if (p2 == snake.last) State.tail(from).at(p2) else Vector.empty

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

  def bitmap(mask: String) = {
    val img = Bitmap.parse(mask)
    Map(
      Point.right -> img,
      Point.left -> img.mirrorY,
      Point.up -> img.anti,
      Point.down -> img.clock.mirrorY
    )
  }

  def cornerBitmap(mask: String) = {
    val img = Bitmap.parse(mask)
    Map(
      (Point.right, Point.up) -> img,
      (Point.down, Point.left) -> img,
      (Point.down, Point.right) -> img.mirrorY,
      (Point.left, Point.up) -> img.mirrorY,
      (Point.right, Point.down) -> img.anti,
      (Point.up, Point.left) -> img.anti,
      (Point.up, Point.right) -> img.clock.mirrorX,
      (Point.left, Point.down) -> img.clock.mirrorX
     )
  }

  val apple = """
-*--
*-*-
-*--
----
""".pipe(Bitmap.parse)

  val head = """
*---
-**-
***-
----
""".pipe(bitmap)

  val headOpen = """
*-*-
-*--
**--
--*-
""".pipe(bitmap)

  val body = """
----
**-*
*-**
----
""".pipe(bitmap)

    val bodyFull = """
-**-
**-*
*-**
-**-
""".pipe(bitmap)

  val tail = """
----
--**
****
----
""".pipe(bitmap)

  val turn = """
-**-
*-*-
**--
----
""".pipe(cornerBitmap)
}

case class Point(x: Int, y: Int) {
  def move(to: Point): Point =
    Point(x + to.x, y + to.y)

  def times(k: Double): Point =
    Point((x * k).toInt, (y * k).toInt)

  def opposite: Point =
    times(-1)

  def square(side: Int): Vector[Point] =
    0.until(side).flatMap(x => 0.until(side).map(y => move(Point(x, y)))).toVector

  def wrap(limit: Point) = {
    def f(n: Int, limit: Int) = n.sign.min(0).abs * limit + (n % limit)
    Point(f(x, limit.x), f(y, limit.y))
  }
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
}

// TODO maybe rename to sprite, depends on name of (position, direction) entity
case class Bitmap(points: Vector[Point]) {
  val size = BitMapSize - 1

  def at(p: Point): Vector[Point] =
    points.map(p.times(BitMapSize).move(_))

  def clock: Bitmap =
    Bitmap(points.map(p => Point(size - p.y, p.x)))

  def anti: Bitmap =
    Bitmap(points.map(p => Point(p.y, size - p.x)))

  def mirrorY: Bitmap =
    Bitmap(points.map(p => Point(size - p.x, p.y)))

  def mirrorX: Bitmap =
    Bitmap(points.map(p => Point(p.x, size - p.y)))
}
object Bitmap {
  /** Takes a bitmap string, with '*' meaning bit set */
  def parse(mask: String) =
    mask
      .filterNot(_.isWhitespace)
      .zipWithIndex
      .collect { case ('*', i) => Point(i % BitMapSize, i / BitMapSize) }
      .pipe(points => Bitmap(points.toVector))
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
