import javax.swing._
import java.awt.{List => _, _}
import java.awt.event._
import scala.util.Random
import scala.util.chaining._
import Shared._

/** Recreate the basic version of the classic Nokia 3310 Snake, no extras
  * Behaviour taken from observing https://helpfulsheep.com/snake/
  *
  * To run: scala-cli Snake.scala
  */
object Main {
  def main(args: Array[String]): Unit = {
    val gui = Gui.start

    var state = State.initial
    var input = state.snake.head.direction

    while (true) {
      Thread.sleep(FrameRate) // TODO this is pretty rudimentary
      input = gui.getInput.getOrElse(input)
      state = state.evolve(input)
      gui.update(state)
    }
  }
}

object Shared {
  val Resolution = Point(84, 48)
  val SpriteSize = 4
  val FrameRate = 1000 / 120
  val Scale = 2

  val Dimensions = Resolution.times(1 / SpriteSize.toDouble)
  val Centre = Dimensions.times(0.5)
  val SnakeSize = 6
  val SlowDown = 12
  val PauseOnLoss = 150
  val FlickerDown = 20
  val FlickerUp = 30

  val DisplaySize = Resolution.times(Scale)
  val BackgroundColor = Color(170, 220, 0)
  val BorderSize = 10
  val ScoreBorderSize = 6
  val CanvasBorderSize = 2
}

case class State(
    snake: Vector[Entity],
    apple: Entity,
    eaten: Vector[Entity] = Vector.empty,
    score: Int = 0,
    lostAt: Long = 0,
    time: Long = 0,
    drawSnake: Boolean = true,
    openMouth: Boolean = false
) {

  def evolve(next: Point): State = {
    def move = {
      val advanceOrGrow =
        copy(
          snake = snake.head.move(next) +: {
            if (eaten.nonEmpty && snake.head.hits(eaten.head)) snake
            else snake.init
          }
        )

      val discardEaten =
        if (eaten.nonEmpty && advanceOrGrow.snake.last.hits(eaten.last))
          advanceOrGrow.copy(eaten = eaten.init)
        else advanceOrGrow

      val aboutToEat =
        if (discardEaten.snake.head.move(next).hits(apple))
          discardEaten.copy(openMouth = true)
        else discardEaten.copy(openMouth = false)

      val eat =
        if (aboutToEat.snake.head.hits(apple))
          aboutToEat.copy(
            apple = State.newApple(aboutToEat.snake),
            eaten = aboutToEat.apple +: aboutToEat.eaten,
            score = aboutToEat.score + 9
          )
        else aboutToEat

      val checkLoss =
        if (eat.snake.tail.exists(eat.snake.head.hits))
          this.copy(lostAt = time)
        else eat

      if (time % SlowDown == 0) checkLoss
      else this
    }

    def flickerOnLoss = {
      val flicker =
        (time / FlickerDown) % ((FlickerDown + FlickerUp) / FlickerDown) == 0

      if (time - lostAt > PauseOnLoss) State.initial
      else if (!flicker) copy(drawSnake = true)
      else copy(drawSnake = false)
    }

    if (lostAt > 0) flickerOnLoss
    else move
  }.copy(time = time + 1)

  def render: Vector[Point] = {
    val renderedSnake: Vector[Point] = if (drawSnake) {
      val head =
        (if (!openMouth) State.head else State.headOpen)
          .apply(snake.head.direction)
          .at(snake.head.position)

      val tail =
        State.tail(snake.init.last.direction).at(snake.last.position)

      // TODO full corners
      val body =
        snake.init.sliding(2).flatMap {
          case Vector(headward, tailward) =>
            if (eaten.exists(tailward.hits)) {
              if (tailward.direction == headward.direction)
                State.bodyFull(tailward.direction).at(tailward.position)
              else
                State
                  .turnFull(tailward.direction -> headward.direction)
                  .at(tailward.position)
            } else {
              if (tailward.direction == headward.direction)
                State.body(tailward.direction).at(tailward.position)
              else
                State
                  .turn(tailward.direction -> headward.direction)
                  .at(tailward.position)

            }


            // if (eaten.exists(tailward.hits))
            //   State.bodyFull(tailward.direction).at(tailward.position)
            // else if (tailward.direction == headward.direction)
            //   State.body(tailward.direction).at(tailward.position)
            // else
            //   State
            //     .turn(tailward.direction -> headward.direction)
            //     .at(tailward.position)
          case _ => sys.error("impossible")
        }

      head ++ body ++ tail
    } else Vector.empty

    val renderedApple = State.apple.at(apple.position)

    (renderedSnake ++ renderedApple).flatMap(_.times(Scale).square(Scale))
  }

  def renderScore: String =
    String.format("%04d", score)
}
object State {
  def initial: State = {
    val snake =
      Vector
        .range(0, SnakeSize)
        .map(x => Entity(Centre.move(Point.left.times(x)), Point.right))

    State(snake, newApple(snake))
  }

  def newApple(snake: Vector[Entity]): Entity = {
    val apple = Entity(
      Point(
        Random.nextInt(Dimensions.x),
        Random.nextInt(Dimensions.y)
      ),
      Point(0, 0)
    )

    if (snake.exists(_.hits(apple))) newApple(snake)
    else apple
  }

  def sprite(mask: String) = {
    val sprite = Sprite.parse(mask)
    Map(
      Point.right -> sprite,
      Point.left -> sprite.mirrorY,
      Point.up -> sprite.anti,
      Point.down -> sprite.clock.mirrorY
    )
  }

  def cornerSprite(mask: String) = {
    val sprite = Sprite.parse(mask)
    Map(
      (Point.right, Point.up) -> sprite,
      (Point.down, Point.left) -> sprite,
      (Point.down, Point.right) -> sprite.mirrorY,
      (Point.left, Point.up) -> sprite.mirrorY,
      (Point.right, Point.down) -> sprite.anti,
      (Point.up, Point.left) -> sprite.anti,
      (Point.up, Point.right) -> sprite.clock.mirrorX,
      (Point.left, Point.down) -> sprite.clock.mirrorX
    )
  }

  val apple = """
-*--
*-*-
-*--
----
""".pipe(Sprite.parse)

  val head = """
*---
-**-
***-
----
""".pipe(sprite)

  val headOpen = """
*-*-
-*--
**--
--*-
""".pipe(sprite)

  val body = """
----
**-*
*-**
----
""".pipe(sprite)

  val bodyFull = """
-**-
**-*
*-**
-**-
""".pipe(sprite)

  val tail = """
----
--**
****
----
""".pipe(sprite)

  val turn = """
-**-
*-*-
**--
----
""".pipe(cornerSprite)

  val turnFull = """
***-
*-*-
**--
----
""".pipe(cornerSprite)

}

case class Entity(position: Point, direction: Point) {
  def move(next: Point) = {
    val whereNow = if (next.direction != direction.opposite) next else direction
    Entity(position.move(whereNow).wrap(Dimensions), whereNow.direction)
  }

  def hits(target: Entity) = position == target.position
}

case class Sprite(points: Vector[Point]) {
  val size = SpriteSize - 1

  def at(p: Point): Vector[Point] =
    points.map(p.times(SpriteSize).move(_))

  def clock: Sprite =
    Sprite(points.map(p => Point(size - p.y, p.x)))

  def anti: Sprite =
    Sprite(points.map(p => Point(p.y, size - p.x)))

  def mirrorY: Sprite =
    Sprite(points.map(p => Point(size - p.x, p.y)))

  def mirrorX: Sprite =
    Sprite(points.map(p => Point(p.x, size - p.y)))
}
object Sprite {

  /** Takes a 4x4 sprite string, with '*' meaning bit set and any other
    * non-whitespace character meaning bit unset.
    */
  def parse(mask: String) =
    mask
      .filterNot(_.isWhitespace)
      .zipWithIndex
      .collect { case ('*', i) => Point(i % SpriteSize, i / SpriteSize) }
      .pipe(points => Sprite(points.toVector))
}

case class Point(x: Int, y: Int) {
  def move(to: Point): Point = Point(x + to.x, y + to.y)

  def times(k: Double): Point = Point((x * k).toInt, (y * k).toInt)

  def opposite: Point = times(-1)

  def direction: Point = Point(x.sign, y.sign)

  def square(side: Int): Vector[Point] =
    0.until(side)
      .flatMap(x => 0.until(side).map(y => move(Point(x, y))))
      .toVector

  def wrap(limit: Point) = {
    def f(n: Int, limit: Int) = n.sign.min(0).abs * limit + (n % limit)
    Point(f(x, limit.x), f(y, limit.y))
  }
}
object Point {
  def up: Point = Point(0, -1)
  def down: Point = Point(0, 1)
  def left: Point = Point(-1, 0)
  def right: Point = Point(1, 0)

  def directions: Map[String, Point] = Map(
    "UP" -> up,
    "DOWN" -> down,
    "LEFT" -> left,
    "RIGHT" -> right
  )
}

class Gui extends JPanel {

  // Writes from event listeners which run on single EDT thread: no atomics needed
  // Reads from main thread: volatile needed
  @volatile private var input: Option[Point] = None
  // All reads and writes from single EDT thread: can be standard references
  private var image: Vector[Point] = Vector()
  private val score = new JLabel("Score")

  Point.directions.keys.foreach { direction =>
    def add(name: String)(action: AbstractAction) = {
      getActionMap.put(name, action)
      getInputMap.put(KeyStroke.getKeyStroke(name), name)
    }

    add(direction) { _ => input = Point.directions.get(direction) }
    add(s"released $direction") { _ => input = None }
  }

  setBorder(
    BorderFactory.createEmptyBorder(
      BorderSize,
      BorderSize,
      BorderSize,
      BorderSize
    )
  )
  setBackground(BackgroundColor)
  setLayout(new BorderLayout)
  add(new Canvas, BorderLayout.CENTER)
  score.setBorder(
    BorderFactory.createCompoundBorder(
      BorderFactory.createEmptyBorder(0, 0, ScoreBorderSize, 0),
      BorderFactory.createMatteBorder(0, 0, CanvasBorderSize, 0, Color.black)
    )
  )
  add(score, BorderLayout.NORTH)

  def getInput: Option[Point] = input

  def update(state: State): Unit =
    SwingUtilities.invokeLater { () =>
      image = state.render
      score.setText(state.renderScore)
      repaint()
    }

  class Canvas extends JComponent {
    setBorder(BorderFactory.createLineBorder(Color.black, CanvasBorderSize))

    // TODO build proper image instead
    override def paintComponent(g: Graphics) =
      image.foreach { point =>
        g.drawLine(
          point.x + CanvasBorderSize,
          point.y + CanvasBorderSize,
          point.x + CanvasBorderSize,
          point.y + CanvasBorderSize
        )
      }

    override def getPreferredSize =
      Dimension(
        DisplaySize.x + 2 * CanvasBorderSize,
        DisplaySize.y + 2 * CanvasBorderSize
      )
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
