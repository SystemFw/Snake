import javax.swing._
import java.awt._
import java.awt.event._
import scala.util.chaining._
import Shared._

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
  val origin = Point(X / 2 - snakeSize * 2, Y / 2 - scale)
  val pauseOnLoss = 120
  val flickerDown = 20
  val flickerUp = 30

  def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] =
    fa.flatMap(a => fb.map(b => f(a, b)))
}

case class State(
    snake: Vector[Point] = Vector.empty,
    direction: Option[Cmd] = None,
    score: Int = 0,
    lostAt: Long = 0,
    time: Long = 0,
    render: Set[Point] = Set.empty
) {
  // TODO apples
  def evolve(cmd: Option[Cmd]): State = {

    def move(direction: Cmd): State = {
      val headNow = snake.head.move(direction.toPoint)
      val newState = State(
        headNow +: snake.init,
        Option(direction),
        score + 1 // TODO proper treatment of score
      ).tick.rendered

      if (snake.contains(headNow)) newState.copy(lostAt = time)
      else newState
    }

    val directionNow =
      map2(direction, cmd) { (current, next) =>
        if (next != current.opposite) next // change direction
        else current
      }.orElse(direction) // Keep moving
        .orElse(cmd) // start moving
        .map(move)
        .getOrElse(State.initial) // initial state

    val flickerOnLoss =
      if ((time - lostAt) > pauseOnLoss) State.initial
      else
        {
          if (
            (time / flickerDown) % ((flickerDown + flickerUp) / flickerDown) == 0
          )
            copy(render = Set())
          else rendered
        }.tick

    if (lostAt > 0) flickerOnLoss
    else directionNow
  }

  def tick: State = copy(time = time + 1)
  def rendered: State = copy(render = Point.scaled(snake.toSet))

}
object State {
  val initial: State =
    Vector
      .range(0, snakeSize)
      .map(x => origin.move(Right.toPoint.times(x)))
      .pipe(snake => State(snake).rendered)
}

case class Point(x: Int, y: Int) {
  def move(to: Point): Point =
    Point(x + to.x, y + to.y)

  def times(k: Int): Point =
    Point(x * k, y * k)

  def square(size: Int): Set[Point] =
    0.to(size).flatMap(x => 0.to(size).map(y => this.move(Point(x, y)))).toSet
}
object Point {
  // Wraps around dimensions
  def apply(x: Int, y: Int): Point =
    new Point(
      x.sign.min(0).abs * X + (x % X),
      y.sign.min(0).abs * Y + (y % Y)
    )

  def scaled(points: Set[Point]): Set[Point] =
    points
      .flatMap { _.times(scale).square(scale - 1) }
      .map { _.move(origin.times(-scale + 1)) }
}

sealed trait Cmd {
  def toPoint = this match {
    case Up    => Point(0, -1)
    case Down  => Point(0, 1)
    case Left  => Point(-1, 0)
    case Right => Point(1, 0)
  }

  def opposite: Cmd = this match {
    case Up    => Down
    case Down  => Up
    case Left  => Right
    case Right => Left
  }
}
case object Up extends Cmd
case object Down extends Cmd
case object Left extends Cmd
case object Right extends Cmd

class Gui extends JPanel {

  // Writes from event listeners which run on single EDT thread: no atomics needed
  // Reads from main thread: volatile needed
  @volatile private var input: Option[Cmd] = None
  // All read and writes from single EDT thread: can be standard references
  private var image: Set[Point] = Set()
  private val score = new JLabel("Score")

  Vector[Cmd](Up, Down, Left, Right).foreach { cmd =>
    def add(name: String)(action: AbstractAction) = {
      getActionMap.put(name, action)
      getInputMap.put(KeyStroke.getKeyStroke(name), name)
    }

    val name = cmd.toString.toUpperCase
    add(name) { _ =>
      input = Some(cmd)
    }

    val released = s"released $name"
    add(released) { _ =>
      // TODO conditional to avoid overwriting if another button has been pressed already?
      input = None
      ()
    }
  }

  setLayout(new BorderLayout)
  add(new Canvas, BorderLayout.CENTER)
  add(score, BorderLayout.NORTH)

  def getInput: Option[Cmd] = input

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
