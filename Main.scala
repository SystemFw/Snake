import javax.swing.{JComponent, JFrame, SwingUtilities, WindowConstants}
import java.awt.{Graphics}
import java.awt.event.{KeyEvent, KeyListener}

object Main {
  def main(args: Array[String]): Unit = {
    val gui = Gui.start(dimension)

    var state = State(Snake.at(initial))

    while(true) {
      Thread.sleep(frameRate)
      gui.getInput.foreach { in =>
        state = evolve(state, in)
      }
      gui.draw(state.render.map(_.wrap(dimension))) // TODO wrapping should go back to game logic
    }
  }

  val frameRate = 1000 / 60
  val dimension: Point = {
    val x = 500
    Point(x, x / 4 * 3)
  }
  val initial = Point(250, 250)

  // TODO wrapping is incorrect, should wrap up a whole square
  // if any point of it is outside the boundary
  // Also, the horizonal wrapping in that case shows half the square
  // on each side, whereas the vertical doesn't, there probably is some
  // hidden space
  def evolve(state: State, input: Cmd): State = State {
    state.snake.move(input.toPoint.scale(3))
  }
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

case class Square(points: Set[Point]) {
  def move(to: Point): Square =
    Square(points.map(_.move(to)))

  def render: Set[Point] = points
}
object Square {
  def at(pos: Point, size: Int): Square = Square {
    0.to(size).flatMap { x =>
      0.to(size).map(y => pos.move(Point(x, y)))
    }.toSet
  }
}

case class Snake(body: Vector[Square]) {
  def move(to: Point): Snake = Snake {
    body.head.move(to) +: body.init
  }

  def render: Set[Point] = body.toSet.flatMap(_.render)
}

object Snake {
  def at(pos: Point): Snake = Snake {
    val size = 4
    val squareSize = 20

    Vector
      .iterate(pos, size)(p => p.move(Point(squareSize, 0)))
      .map(p => Square.at(p, squareSize))
  }
}

case class State(snake: Snake) {
  def render: Set[Point] =
    snake.render
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


