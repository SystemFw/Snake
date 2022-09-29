import javax.swing._
import java.awt._
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
  val frameRate = 1000 / 60
  val X = 500
  val Y = X / 4 * 3
  val scale = 5
  val snakeSize = 20
  val speed = 2
  val origin = Point(X / 2 - snakeSize * 2, Y / 2 - scale)
  val pauseOnLoss = 120
  val flickerDown = 20
  val flickerUp = 30
}

case class State(
    snake: Vector[Point],
    direction: Point,
    apple: Point,
    score: Int = 0,
    lostAt: Long = 0,
    time: Long = 0,
    render: Set[Point] = Set.empty
) {
  // TODO apples, they get created at every turn including the first
  // snake grows once the apple has gone through its body
  // there is no minimum distance between apple and snake
  def evolve(nextDirection: Option[Point]): State = {
    def move = {
      val directionNow =
        nextDirection.filter(_ != direction.opposite).getOrElse(direction)
      val headNow = snake.head.move(directionNow)
      val snakeNow = headNow +: snake.init
      val stateNow = copy(
        snake = snakeNow,
        direction = directionNow,
      ).tick.rendered

      if (snake.contains(headNow)) stateNow.copy(lostAt = time)
      // TODO next apple should be spawned immediately after eating
      // TODO there could be multiple eaten apples before the snake grows
      // TODO score should be updated immediately after eating
      else if (snakeNow.last.scaled.intersect(apple.scaled).nonEmpty) {
        val grownSnake = snakeNow :+ snakeNow.last.move(directionNow.opposite)
        val appleNow = State.newApple(grownSnake)
        stateNow.copy(snake = grownSnake, apple = appleNow, score = score + 9)
      } else stateNow
    }

    def flickerOnLoss = {
      val flicker =
        (time / flickerDown) % ((flickerDown + flickerUp) / flickerDown) == 0

      if (time - lostAt > pauseOnLoss) State.initial
      else if (!flicker) rendered.tick
      else copy(render = apple.scaled).tick
    }

    if (lostAt > 0) flickerOnLoss
    else if (time % speed == 0) move
    else tick
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
      .map { _.move(origin.times(-scale + 1)) }

}
object Point {
  // TODO in rare cases still possible to "split" the snake or the apple
  // Wraps around dimensions
  def apply(x: Int, y: Int): Point =
    new Point(
      x.sign.min(0).abs * X + (x % X),
      y.sign.min(0).abs * Y + (y % Y)
    )

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
    override def paintComponent(g: Graphics) =
      image.foreach { point =>
        g.drawLine(point.x, point.y, point.x, point.y)
      }

    override def getPreferredSize = Dimension(X, Y)
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
