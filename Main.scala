import javax.swing._
import java.awt._
import java.awt.event._

object Main {
  def main(args: Array[String]): Unit = {
    val frameRate = 1000 / 60
    val x = 500
    val y = x / 4 * 3

    val gui = Gui.start(x, y)

    var state = Point(250, 250)

    while(true) {
      Thread.sleep(frameRate)
      gui.getInput.foreach { in =>
        state = evolve(state, in)
      }
      gui.draw(Set(state))
    }
  }

  def evolve(point: Point, input: Cmd): Point =
    point.move(input.toPoint)

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
      g.fillRect(point.x, point.y, 20, 20)
    }
}
object Gui {
  def start(x: Int, y: Int): Gui = {
    val gui = new Gui
    SwingUtilities.invokeLater { () =>
      val app = new JFrame("Snake")
      app.setSize(x, y)
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


