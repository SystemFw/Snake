import javax.swing._
import java.awt._
import java.awt.event._
import scala.util.chaining._
import Params._

object Main {
  def main(args: Array[String]): Unit = {
    val gui = Gui.start(dimension)

    var state = State.create

    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.Future
    while(true) {
      Thread.sleep(frameRate) // TODO this is pretty rudimentary
      gui.getInput.foreach { in =>
        state = evolve(state, in)
      }
      gui.draw(state.render)
    }
  }


  def evolve(state: State, input: Cmd): State =
    state.move(input)
}

object Params {
  val frameRate = 1000 / 60
  val dimension: Point = {
    val x = 500
    new Point(x, x / 4 * 3)
  }
  val origin = Point(250, 250)
  val scale = 5
  val initialSnakeSize = 20
}

case class State(snake: Vector[Point], direction: Cmd) {

  // TODO ban touching itself
  def move(cmd: Cmd): State = {
    val directionNow = if (cmd == direction.opposite) direction else cmd
    State(
      (snake.head.move(directionNow.toPoint) +: snake.init),
      directionNow
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
      .pipe(snake => State(snake, Left))
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
      x.sign.min(0).abs * dimension.x + (x % dimension.x),
      y.sign.min(0).abs * dimension.y + (y % dimension.y)
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

class Gui extends JComponent {

  @volatile private var input: Option[Cmd] = None
  private var image: Set[Point] = Set()

  def getInput: Option[Cmd] = input

  def draw(newImage: Set[Point]): Unit =
    SwingUtilities.invokeLater { () =>
      image = newImage
      repaint()
    }

  def onKey(e: KeyEvent): Unit =
    e.getKeyCode match {
      case KeyEvent.VK_UP =>
        input = Some(Up)
      case KeyEvent.VK_DOWN =>
        input = Some(Down)
      case KeyEvent.VK_LEFT =>
        input = Some(Left)
      case KeyEvent.VK_RIGHT =>
        input = Some(Right)
      case _  => ()
    }

  // TODO build proper image instead
  override def paintComponent(g: Graphics) =
    image.foreach { point =>
      g.drawLine(point.x, point.y, point.x, point.y)
    }

  override def getPreferredSize = Dimension(dimension.x, dimension.y)
}
object Gui {
  def start(dimension: Point): Gui = {
    val gui = new Gui
    SwingUtilities.invokeLater { () =>
      val app = new JFrame("Snake")
      app.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
      app.setResizable(false)
      // TODO no longer works
      app.setLocationRelativeTo(null) // centers

      app.addKeyListener { // TODO consider keybindings instead
        new KeyListener {
          def keyPressed(e: KeyEvent): Unit = gui.onKey(e)
          def keyReleased(e: KeyEvent): Unit = ()
          def keyTyped(e: KeyEvent): Unit = ()
        }
      }
      app.add(gui)
      app.pack
      app.setVisible(true)
    }
    gui
  }

}
