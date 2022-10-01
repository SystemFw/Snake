import javax.swing._
import java.awt.{List => _, _}
import java.awt.event._
import scala.util.Random
import Shared._

/** Recreate the basic version of the classic Nokia 3310 Snake, no extras
  */
object Main {
  def main(args: Array[String]): Unit = {
    val gui = Gui.start

    var state = State.debug

    while (true) {
      Thread.sleep(FrameRate) // TODO this is pretty rudimentary
      state = state.evolve(gui.getInput)
      gui.update(state)
    }
  }
}

object Shared {
  val FrameRate = 1000 / 60
  val SlowDown = 4 // TODO is 3 better?
  val Scale = 3
  val BitMapSize = 5
  val FullScale = Scale * BitMapSize
   // TODO express size in terms of scale
  val Size = 70
  val Dimensions = Point(Size, Size / 4 * 3)
  val DisplaySize = Dimensions.times(FullScale)

  val Origin = Dimensions.times(0.5)
  val SnakeSize = 20

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
        snake = snake.head.move(directionNow) +: snake.init,
        direction = directionNow
      )

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
        if (snake.contains(eat.snake.head)) eat.copy(lostAt = time)
        else eat

      val wrap =
        checkLoss.copy(snake = checkLoss.snake.map(_.wrap(Dimensions)))

      if (time % SlowDown == 0) wrap
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

  // TODO sprites?
  def render: Set[Point] =
    (if (drawSnake) snake.toSet else Set())
      .+(apple)
      .flatMap { p =>
        //  0 1 2 3 4
        // 0
        // 1    *
        // 2  * * *
        // 3    *
        // 4
        Set(
          Point(1,2),
          Point(2,1),
          Point(2,2),
          Point(2,3),
          Point(3,2)
        ).map(p.times(BitMapSize).move(_))
      }.flatMap(_.times(Scale).square(Scale))
}
object State {
  def debug: State = {
    val snake =
      Vector.range(0, SnakeSize).map(x => Point(30, 26).move(Point.left.times(x)))
    State(snake, Point.right, Point(50, 25))
  }

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
}

case class Point(x: Int, y: Int) {
  def move(to: Point): Point =
    Point(x + to.x, y + to.y)

  def times(k: Float): Point =
    Point((x * k).toInt, (y * k).toInt)

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
      score.setText(s"${DisplaySize.x} x ${DisplaySize.y} Score: ${state.score} Head: (${state.snake.head.x}, ${state.snake.head.y}) Apple: (${state.apple.x}, ${state.apple.y})")
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
