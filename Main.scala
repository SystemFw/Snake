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

    var state = State.initial

    while (true) {
      Thread.sleep(frameRate) // TODO this is pretty rudimentary
      state = state.evolve(gui.getInput)
      gui.update(state)
    }
  }
}

object Shared {
  // TODO maybe we can go back to the scaling afterwards idea,
  // but the dimensions have to be scaled as well

  lazy val frameRate = 1000 / 60
  lazy val X = 50
  lazy val Y = (X / 4 * 3)
  lazy val scaledX = X * scale + (scale - 1)
  lazy val scaledY = Y * scale + (scale - 1)

  lazy val scale = 5
  lazy val slowdown = 2
  // lazy val scale = 10 // TODO for debug purposes
  // lazy val slowdown = 10 // TODO for debug purposes
  lazy val snakeSize = 20
  lazy val origin = Point(X / 2 - snakeSize * 2, Y / 2 - scale)
  lazy val pauseOnLoss = 120
  lazy val flickerDown = 20
  lazy val flickerUp = 30
  println(s"$X $Y, $scaledX , $scaledY")

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
    render: Set[Point] = Set.empty
) {
  def evolve(nextDirection: Option[Point]): State = {
    def move = {
      val directionNow =
        nextDirection.filter(_ != direction.opposite).getOrElse(direction)

      val advance = copy(
        snake = snake.head.move(directionNow) +: snake.init,
        direction = directionNow
      ).tick.rendered

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

      val wrapped =
        copy(snake = snake.map(_.wrap(Point(X, Y))))

      if (time % slowdown == 0) checkLoss
      else tick
    }

    def flickerOnLoss = {
      val flicker =
        (time / flickerDown) % ((flickerDown + flickerUp) / flickerDown) == 0

      if (time - lostAt > pauseOnLoss) State.initial
      else if (!flicker) rendered.tick
      else copy(render = apple.scaled).tick
    }

    p(snake.head)
    if (lostAt > 0) flickerOnLoss
    else move
  }

  def tick: State = copy(time = time + 1)
  def rendered: State = copy(render = (snake.toSet + apple).flatMap(_.scaled)) // TODO sprites?
}
object State {
  def initial: State = {
    val snake =
      Vector.range(0, snakeSize).map(x => origin.move(Point.left.times(x)))
    State(snake, Point.right, newApple(snake)).rendered
  }

  def newApple(snake: Vector[Point]): Point = {
    val apple = Point(Random.nextInt(X), Random.nextInt(Y))
    if (snake.contains(apple)) newApple(snake)
    else apple
  }
}

case class Point(x: Int, y: Int) {
  def move(to: Point): Point =
    Point(x + to.x, y + to.y)

  def times(k: Int): Point =
    Point(x * k, y * k)

  def opposite: Point =
    times(-1)

  def square(size: Int): Set[Point] =
    0.to(size).flatMap(x => 0.to(size).map(y => this.move(Point(x, y)))).toSet

  def scaled: Set[Point] =
    times(scale).square(scale - 1)
    //square(scale - 1)
    //   .map { _.move(origin.times(-scale + 1)) }

  def round(n: Int) =
    Point(x / n * n, y / n * n)

  def wrap(dimension: Point) =
    Point(
      x.sign.min(0).abs * dimension.x + (x % dimension.x),
      y.sign.min(0).abs * dimension.y + (y % dimension.y)
    )

}
object Point {
  // TODO in rare cases still possible to "split" the snake or the apple
  // Wraps around dimensions
  // def apply(x: Int, y: Int): Point =
  //   new Point(
  //     x.sign.min(0).abs * X + (x % X),
  //     y.sign.min(0).abs * Y + (y % Y)
  //   )

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
      score.setText(s"Score: ${state.score}")
      repaint()
    }

  class Canvas extends JComponent {
    // TODO build proper image instead
    override def paintComponent(g: Graphics) = {
      // List.range(0, X + scale, scale).foreach { x =>
      //   g.drawLine(x, 0, x, Y)
      // }
      // List.range(0, Y + scale, scale).foreach { y =>
      //   g.drawLine(0, y, X, y)
      // }
      image.foreach { point =>
        g.drawLine(point.x, point.y, point.x, point.y)
      }
    }

    override def getPreferredSize = Dimension(scaledX, scaledY)
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
