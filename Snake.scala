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

    while (true) {
      Thread.sleep(FrameRate) // TODO this is pretty rudimentary
      state = state.evolve(gui.getInput)
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

// TODO restarting after loss shouldn't remember the last direction
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

  def evolve(next: Option[Point]): State = {
    def move =
      if (time % SlowDown != 0) this
      else {
        val directionNow =
          next
            .filter(_ != snake.head.direction.opposite)
            .getOrElse(snake.head.direction)

        val headNow = snake.head.move(directionNow)

        val hasEaten = eaten.headOption.exists(snake.head.hits)
        val eating = headNow.hits(apple)
        val aboutToEat = headNow.move(directionNow).hits(apple)
        val swallowed = eaten.lastOption.exists(snake.last.hits)
        val dead = snake.tail.exists(headNow.hits)

        val snakeNow = headNow +: (if (hasEaten) snake else snake.init)

        val eatenNow =
          (if (eating) Vector(apple) else Vector()) ++
          (if (swallowed) eaten.init else eaten)

        val (appleNow,scoreNow) =
          if (eating) (State.newApple(snakeNow), score + 9) else (apple, score)

        if (dead) copy(lostAt = time)
        else copy(
          snake = snakeNow,
          apple = appleNow,
          eaten = eatenNow,
          score = scoreNow,
          openMouth = aboutToEat
        )
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
    val renderedApple = State.apple.at(apple.position) ++ {
      val positions = Vector(Point(3, 4), Point(4, 4))
      val sprites: Vector[Sprite] =
        Sprite.parseRow(State.monster)
      sprites.zip(positions).flatMap { case (sprite: Sprite, position: Point) => sprite.at(position) }
    }

    val renderedSnake =
      if (!drawSnake) Vector.empty
      else {
        val head =
          (if (!openMouth) State.head else State.headOpen)
            .apply(snake.head.direction)
            .at(snake.head.position)

        val tail =
          State.tail.apply(snake.init.last.direction).at(snake.last.position)

        val body =
          snake.init.sliding(2).flatMap {
            case Vector(headward, tailward) =>
              val (body, turn) =
                if (eaten.exists(tailward.hits))
                  (State.bodyFull, State.turnFull)
                else (State.body, State.turn)

              if (tailward.direction == headward.direction)
                body.apply(tailward.direction).at(tailward.position)
              else
                turn
                  .apply(tailward.direction -> headward.direction)
                  .at(tailward.position)

            case _ => sys.error("impossible")
          }

        head ++ body ++ tail
      }

    (renderedSnake ++ renderedApple).flatMap(_.times(Scale).square(Scale))
  }

  def renderScore: String =
    String.format("%04d", score)
}
object State {
  // TODO monsters appear after 5 eaten apples, and then
  // every 5 + choose [1, 2, 3] eaten apples, and disappear after 20 steps
  val monster = """
********
********
********
*******-
"""

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
""".pipe(Sprite.parse(_))

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
  def move(to: Point) =
    Entity(position.move(to).wrap(Dimensions), to.direction)

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
  /** Parses the input as a 4x4 sprite, with '*' meaning bit set and
    * any other non-whitespace character meaning bit unset.
    */
  def parse(mask: String, tileIndex: Int = 0, tiles: Int = 1) = Sprite {
    val input = mask.filterNot(_.isWhitespace)
    Vector.range(0, SpriteSize * SpriteSize).flatMap { i =>
      val x = i % SpriteSize
      val y = i / SpriteSize
      val target = (tileIndex * SpriteSize + x) + (y * SpriteSize)
      Option.when(input(target) == '*')(Point(x, y))
    }
  }

  /** Parses the input as a row of 4x4 sprites */
  def parseRow(mask: String): Vector[Sprite] = {
    val input = mask.filterNot(_.isWhitespace)
    val size = input.length / (SpriteSize * SpriteSize)

    Vector.range(0, size).map(tileIndex => parse(input, tileIndex, size))
  }
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
