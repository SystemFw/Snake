import javax.swing.{JComponent, JFrame, SwingUtilities, WindowConstants}
import java.awt.{Graphics}
import java.awt.event.{KeyEvent, KeyListener}

object Main {
  def main(args: Array[String]): Unit = {
    val gui = Gui.start(dimension)

    val initial = Point(250, 250)
    def square(initial: Point) =
      0.to(20).flatMap { x =>
        0.to(20).map { y =>
          initial.move(Point(x, y))
        }
      }.toSet

    val snake =
      List.range(0, 4, step = 20)

    var state = square(initial)

    while(true) {
      Thread.sleep(frameRate)
      gui.getInput.foreach { in =>
        state = evolve(state, in)
      }
      gui.draw(state)
    }
  }

  val frameRate = 1000 / 60
  val dimension: Point = {
    val x = 500
      Point(x, x / 4 * 3)
  }

  // TODO wrapping is incorrect, should wrap up a whole square
  // if any point of it is outside the boundary
  // Also, the horizonal wrapping in that case shows half the square
  // on each side, whereas the vertical doesn't, there probably is some
  // hidden space
  def evolve(points: Set[Point], input: Cmd): Set[Point] =
    points.map(p => p.move(input.toPoint.scale(3)).wrap(dimension))
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


