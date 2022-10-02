import javax.swing._
import java.awt.{List => _, _}
import java.awt.event._
import scala.util.Random
import scala.util.chaining._
import Shared._

/** Recreate the basic version of the classic Nokia 3310 Snake, no extras
  */
object Main {
  def main(args: Array[String]): Unit = {
    val gui = Gui.start

    // var state = State.initial

    // while (true) {
    //   Thread.sleep(FrameRate) // TODO this is pretty rudimentary
    //   state = state.evolve(gui.getInput)
    //   gui.update(state)
    // }

    SlowDown = 1
    var state: State =
      State(
        Vector.range(0, SnakeSize).map(x => Origin.move(Point.left.times(x))),
        Point.right,
        Origin.move(Point.right.times(3))
      )

    def turn(input: Option[Point]) = {
      Thread.sleep(1200)
      state = state.evolve(input)
      gui.update(state)
    }

    gui.update(state)
    turn(None)
    turn(None)
    turn(None)
    turn(None)
    turn(None)
    turn(None)
    turn(None)
    turn(Some(Point.up))
    turn(None)
    Thread.sleep(60000)
  }
}

object Shared {
  val Dimensions = Point(24, 14)
  val BitMapSize = 5

  val FrameRate = 1000 / 120
  var SlowDown = 12
  val Scale = 2

  val FullScale = Scale * BitMapSize

  val DisplaySize = Dimensions.times(FullScale)

  val Origin = Dimensions.divideBy(2)
  val SnakeSize = 6

  val PauseOnLoss = 120
  val FlickerDown = 20
  val FlickerUp = 30

  def p[A](v: A): Unit =
    scala.concurrent.Future(println(v))(scala.concurrent.ExecutionContext.global)
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

      val advance = copy(
        snake = snake.head.move(directionNow).wrap(Dimensions) +: snake.init,
        direction = directionNow
      )

      // TODO there is a flicker if you change direction just as the body is growing
      // TODO in which frame should this happen? Question is whether you can die by stumbling
      // onto the tail you've just grown, and how to show this. Perhaps it should assert on snake
      // rather than advance. Or perhaps growing should happen before moving
      val grow =
        if (eaten.nonEmpty && advance.snake.last == eaten.last)
          advance.copy(
            snake = advance.snake :+ advance.snake.last.move(directionNow.opposite),
            eaten = advance.eaten.init
          )
        else advance

      val eat =
        if (grow.snake.head == apple)
          grow.copy(
            apple = State.newApple(grow.snake),
            eaten = grow.apple +: grow.eaten,
            score = grow.score + 9
          )
        else grow

      val checkLoss =
        if (snake.contains(eat.snake.head)) this.copy(lostAt = time)
        else eat

      // TODO check that this logic doesn't affect the responsiveness
      // of the game, the slowdown should perhaps only happen when there
      // is no change of direction
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
    val renderedSnake = if (drawSnake) {
      State.heads(direction).at(snake.head) ++
      snake.tail.flatMap(State.body.at).toSet
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

    val apple =
    """
-----
--*--
-*-*-
--*--
-----
""".pipe(Bitmap.parse)

  val headRight =
    """
-----
--*--
-*-**
*****
-----
""".pipe(Bitmap.parse)

  val body =
  """
-----
--*--
-***-
--*--
-----
""".pipe(Bitmap.parse)

  // TODO this is broken/rudimentary, rotation should be relative
  val heads = Map(Point.up -> headRight.rotate(-1), Point.down -> headRight.rotate(1), Point.right -> headRight, Point.left -> headRight.rotate(-1).rotate(-1))
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
    Point(
      x.sign.min(0).abs * limit.x + (x % limit.x),
      y.sign.min(0).abs * limit.y + (y % limit.y)
    )
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

/** 5x5 bitmaps */
case class Bitmap(points: Set[Point]) {
  def at(p: Point): Set[Point] =
    points.map(p.times(BitMapSize).move(_))

  // TODO rotate by multiples of direction
  def rotate(direction: Int): Bitmap =
    if (direction == 0) this
    else Bitmap {
      points.map { case Point(x, y) =>
        def formula(coord: Int, direction: Int) = {
          val size = BitMapSize - 1
          val sign = direction.sign
          sign.max(0) * size + (-sign) * coord
        }

        Point(formula(y, direction), formula(x, -direction))
      }
    }
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
