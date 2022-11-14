import javax.swing._
import java.awt.{List => _, _}
import java.awt.event._
import scala.util.Random
import scala.util.chaining._
import Main._

object Main {
  def main(args: Array[String]): Unit =
    Gui.start

  val BorderSize = 7 // Border is necessary for the canvas to display fully. Scale invariant
  val X = 150
}

class Gui extends JPanel {
  val canvas = new JComponent {
     override def paintComponent(g: Graphics) =
       g.setColor(Color.red)
       g.drawLine(0,0,0,0)
       g.drawLine(X, X, X, X)

      override def getPreferredSize =
        Dimension(X, X)
  }

  setBorder(BorderFactory.createEmptyBorder(7, 7, 7, 7))
  setBackground(Color.blue)
  setLayout(new BorderLayout)
  add(canvas, BorderLayout.CENTER)
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