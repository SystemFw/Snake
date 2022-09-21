import javax.swing.{JComponent, JFrame, SwingUtilities, WindowConstants}
import java.awt.{Graphics}
import java.awt.event.{KeyEvent, KeyListener}
import scala.util.chaining._

object Main {
  def main(args: Array[String]): Unit = {
    val gui = Gui.start(dimension)

    var state = Snake.at(initial)

    while(true) {
      Thread.sleep(frameRate)
      gui.getInput.foreach { in =>
        state = evolve(state, in)
      }
      gui.draw(state.render(dimension, 3, initial))
    }
  }

  val frameRate = 1000 / 60
  val dimension: Point = {
    val x = 500
    Point(x, x / 4 * 3)
  }
  val initial = Point(250, 250)

  def evolve(state: State, input: Cmd): State =
    state.move(input.toPoint)
}

type State = Snake

object Params {
  val framerate = ???
  val dimension = ???
  val origin = ???
  val scale = ???
  val initialSnakeSize = ???
}

case class Snake(body: Vector[Point]) {

  //TODO it moves, but the logic is not correct
  // TODO ban touching itself
  // TODO ban head going backward
  def move(to: Point) = Snake {
    body.head.move(to) +: body.init
  }

  // TODO should it wrap before or after scaling?
  // currently it's kinda broken
  def render(dimension: Point, k: Int, origin: Point): Set[Point] =
    body
      .flatMap(_.scale(k).square(k -1))
      .map(_.move(origin.scale(-(k-1))).wrap(dimension))
      .toSet
}
object Snake {
  def at(pos: Point): Snake =
    Vector
      .range(0, 20)
      .map(x => pos.move(Right.toPoint.scale(x)))
      .pipe(Snake.apply)
}


sealed trait Cmd {
  def toPoint = this match {
    case Up => Point(0, -1)
    case Down => Point(0, 1)
    case Left => Point(-1, 0)
    case Right => Point(1, 0)
  }
}
case object Up    extends Cmd
case object Down  extends Cmd
case object Left  extends Cmd
case object Right extends Cmd

case class Point(x: Int, y: Int) {
  def move(to: Point): Point =
    Point(x + to.x, y + to.y)

  def scale(k: Int): Point =
    Point(x*k, y*k)

  def wrap(dimension: Point): Point =
    Point(
      if (x < 0) dimension.x - x.abs else x % dimension.x,
      if (y < 0) dimension.y - y.abs else y % dimension.y
    )

  def square(size: Int): Set[Point] =
    0.to(size).flatMap { x =>
      0.to(size).map(y => this.move(Point(x, y)))
    }.toSet
}

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
  override def paint(g: Graphics) =
    image.foreach { point =>
      g.drawLine(point.x, point.y, point.x, point.y)
    }
}
object Gui {
  def start(dimension: Point): Gui = {
    val gui = new Gui
    SwingUtilities.invokeLater { () =>
      val app = new JFrame("Snake")
      app.setSize(dimension.x, dimension.y)
      app.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
      app.setResizable(false)
      app.setLocationRelativeTo(null) // centers

      app.addKeyListener {
        new KeyListener {
          def keyPressed(e: KeyEvent): Unit = gui.onKey(e)
          def keyReleased(e: KeyEvent): Unit = ()
          def keyTyped(e: KeyEvent): Unit = ()
        }
      }
      app.add(gui)
      app.setVisible(true)
    }
    gui
  }
}
