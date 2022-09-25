import javax.swing._
import java.awt._
import java.awt.event._
import scala.util.chaining._
import Params._

object Main {
  def main(args: Array[String]): Unit = {
    val gui = Gui.start

    var state = State.initial

    while(true) {
      Thread.sleep(frameRate) // TODO this is pretty rudimentary
      state = state.evolve(gui.getInput)
      gui.update(state)
    }
  }
}

object Params {
  val frameRate = 1000 / 60
  val X = 500
  val Y = X / 4 * 3
  val scale = 5
  val initialSnakeSize = 20
  val initialSnakePosition = Point(X / 2 - initialSnakeSize * 2, Y / 2 - scale)

}

case class State(
  snake: Vector[Point],
  direction: Option[Cmd],
  score: Int,
  lost: Boolean,
  time: Long
) {
  // TODO ban touching itself
  def evolve(cmd: Option[Cmd]): State = {

    val directionNow = (direction, cmd) match {
      case (None, None) => None
      case (Some(currentDirection), None) => Some(currentDirection)
      case (None, Some(newDirection)) => Some(newDirection)
      case (Some(currentDirection), Some(newDirection)) =>
        if (newDirection != currentDirection.opposite) Some(newDirection)
        else Some(currentDirection)
    }

    def move(direction: Cmd): State = {
      val headNow = snake.head.move(direction.toPoint)
      if (snake.contains(headNow)) State.initial
      else State(
        headNow +: snake.init,
        Option(direction),
        score + 1,
        false,
        time + 1
      )
    }


    directionNow.map(move).getOrElse(State.initial)
  }

  def render: Set[Point] =
    snake
      .flatMap { _.scaleBy(scale).square(scale - 1) }
      .map { _.move(initialSnakePosition.scaleBy(-scale + 1)) }
      .toSet

  private def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] =
    for {
      a <- fa
      b <- fb
    } yield f(a, b)
}
object State {
  val initial: State =
    Vector
      .range(0, initialSnakeSize)
      .map(x => initialSnakePosition.move(Right.toPoint.scaleBy(x)))
      .pipe(snake => State(snake, None, 0, false, 0))
}

case class Point(x: Int, y: Int) {
  def move(to: Point): Point =
    Point(x + to.x, y + to.y)

  def scaleBy(k: Int): Point =
    Point(x*k, y*k)

  def square(size: Int): Set[Point] =
    0.to(size).flatMap { x =>
      0.to(size).map(y => this.move(Point(x, y)))
    }.toSet
}
object Point {
  // Wraps around dimensions
  def apply(x: Int, y: Int): Point =
    new Point(
      x.sign.min(0).abs * X + (x % X),
      y.sign.min(0).abs * Y + (y % Y)
    )
}

sealed trait Cmd {
  def toPoint = this match {
    case Up => Point(0, -1)
    case Down => Point(0, 1)
    case Left => Point(-1, 0)
    case Right => Point(1, 0)
  }

  def opposite: Cmd = this match {
    case Up => Down
    case Down => Up
    case Left => Right
    case Right => Left
  }
}
case object Up    extends Cmd
case object Down  extends Cmd
case object Left  extends Cmd
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
  add(new Canvas,  BorderLayout.CENTER)
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
      app.setLocationRelativeTo(null) // centers the window if called after `pack`
      app.setVisible(true)
    }
    gui
  }

}
