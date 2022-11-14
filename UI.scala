import javax.swing._
import java.awt.{List => _, _}
import java.awt.event._
import scala.util.Random
import scala.util.chaining._
import Main._

object Main {
  def main(args: Array[String]): Unit =
    Gui.start

  val BorderSize = 10
  val CanvasBorderSize = 2
  val X = 150
}

class Gui extends JPanel {
  private def emptyBorder(size: Int) =
    BorderFactory.createEmptyBorder(size, size, size, size)

  private val canvas = {
    val canvas = new JComponent {
      override def paintComponent(g: Graphics) =
        g.setColor(Color.red)
        g.drawLine(25, 25, 25, 25)

      override def getPreferredSize =
        Dimension(X, X)
    }

    val panel = new JPanel
    val border = BorderFactory.createCompoundBorder(
      BorderFactory.createLineBorder(Color.green, CanvasBorderSize),
      emptyBorder(CanvasBorderSize)
    )
    panel.setBorder(border)
    panel.setBackground(Color.blue)
    panel.setLayout(new BorderLayout)
    panel.add(canvas, BorderLayout.CENTER)
    panel
  }

  setBorder(emptyBorder(BorderSize))
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
