import javax.swing._
import java.awt._
import java.awt.event._
import scala.util.chaining._
import Params._

object Main {
  def main(args: Array[String]): Unit = {
    val gui = Gui.start

    var state = State.create

    while(true) {
      Thread.sleep(frameRate) // TODO this is pretty rudimentary
      gui.getInput.foreach { input => state = state.evolve(input) }
      gui.update(state)
    }
  }
}

object Params {
  val frameRate = 1000 / 60
  val X = 500
  val Y = X / 4 * 3
  val origin = Point(250, 250)
  val scale = 5
  val initialSnakeSize = 20
}

case class State(snake: Vector[Point], direction: Cmd, score: Int) {

  // TODO ban touching itself
  def evolve(cmd: Cmd): State = {
    val directionNow = if (cmd == direction.opposite) direction else cmd
    State(
      (snake.head.move(directionNow.toPoint) +: snake.init),
      directionNow,
      score + 1
    )
  }

  def render: Set[Point] =
    snake
      .flatMap { _.scaleBy(scale).square(scale - 1) }
      .map { _.move(origin.scaleBy(-scale + 1)) }
      .toSet
}
object State {
  def create: State =
    Vector
      .range(0, initialSnakeSize)
      .map(x => origin.move(Right.toPoint.scaleBy(x)))
      .pipe(snake => State(snake, Left, 0))
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
  /* Wraps around dimensions */
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

  @volatile private var input: Option[Cmd] = None
  // Updated from EDT thread, no volatile needed
  private var image: Set[Point] = Set()
  private val score = new JLabel("Score")

  Vector[Cmd](Up, Down, Left, Right).foreach { cmd =>
    val name = cmd.toString.toUpperCase
    val action: AbstractAction = _ => input = Some(cmd)
    getActionMap.put(name, action)
    getInputMap.put(KeyStroke.getKeyStroke(name), name)
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
