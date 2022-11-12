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
  val Dimensions = Point(22, 13)
  val SpriteSize = 4
  val FrameRate = 1000 / 120
  val Scale = 2


  val Centre = Dimensions.times(0.5)
  val SnakeSize = 7
  val SlowDown = 12
  val PauseOnLoss = 150
  val FlickerDown = 20
  val FlickerUp = 30

  val DisplaySize = Dimensions.times(SpriteSize).times(Scale)
  val BackgroundColor = Color(170, 220, 0)
  val BorderSize = 10
  val ScoreBorderSize = 6
  val CanvasBorderSize = 2
}

// TODO maybe review the extent to which Entity is used, most of the
// uses are based on position, could use entity as dumb data, change
// headNow, eaten, apple and monster back to Points, write a collision
// detection with intersection and more the wrapping to Point.move
// or maybe just have hits take a Point again
case class State(
    snake: Vector[Entity],
    apple: Entity,
    eaten: Vector[Entity] = Vector.empty,
    score: Int = 0,
    lostAt: Long = 0,
    time: Long = 0,
    drawSnake: Boolean = true,
    openMouth: Boolean = false, // food
    monster: Vector[Entity] = Vector(),
    monsterTimer: Int = 20,
    nextMonster: Int = 5 // TODO + choose[1, 3] after first time
    // TODO random monster sprite every time
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
        val eatingApple = Option.when(headNow.hits(apple))(apple)
        val eatingMonster = monster.filter(headNow.hits)
        val aboutToEat =
          (apple +: monster).exists(headNow.move(directionNow).hits)
        val swallowed = eaten.lastOption.exists(snake.last.hits)
        val dead = snake.tail.exists(headNow.hits)

        val snakeNow = headNow +: (if (hasEaten) snake else snake.init)

        val eatenNow =
          eatingApple.toVector ++ eatingMonster.toVector ++ (if (swallowed)
                                                               eaten.init
                                                             else eaten)
        val appleNow =
          if (eatingApple.nonEmpty) State.newApple(snakeNow) else apple

        val monsterNow =
          if (eatingMonster.nonEmpty) State.newMonster(snakeNow, appleNow)
          else monster

        val scoreNow =
          if (eatingApple.nonEmpty) score + 9
          else if (eatingMonster.nonEmpty) score + 9 // TODO separate score for  monster
          else score

        if (dead) copy(lostAt = time)
        else
          copy(
            snake = snakeNow,
            apple = appleNow,
            eaten = eatenNow,
            score = scoreNow,
            openMouth = aboutToEat,
            monster = monsterNow
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
    val renderedFood = State.apple.at(apple.position) ++ {
      val monsterSprite = Sprite.parseRow(State.monster)
      monsterSprite.zip(monster.map(_.position)).flatMap {
        case (sprite: Sprite, position: Point) => sprite.at(position)
      }
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

    (renderedSnake ++ renderedFood).flatMap(_.times(Scale).square(Scale))
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

    val apple = newApple(snake)

    val monster = newMonster(snake, apple)

    State(snake = snake, apple = apple, monster = monster)
  }

  def newApple(snake: Vector[Entity]): Entity = {
    val apple = Point(
      Random.nextInt(Dimensions.x),
      Random.nextInt(Dimensions.y)
    ).pipe(Entity.static)

    if (snake.exists(_.hits(apple))) newApple(snake)
    else apple
  }

  // TODO dedicated Vector[Entity] vs Vector[Entity] collision detection?
  def newMonster(snake: Vector[Entity], apple: Entity): Vector[Entity] = {
    val size = 2
    val point = Point(
      Random.nextInt(Dimensions.x) / size * size,
      Random.nextInt(Dimensions.y)
    )

    val monster =
      Vector(point, point.move(Point.right)).map(Entity.static)

    val collision =
      monster.exists(p => (apple +: snake).exists(p.hits))
    if (collision) newMonster(snake, apple) else monster
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
object Entity {
  def static(position: Point) = Entity(position, Point(0, 0))
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

  /** Parses the input as a 4x4 sprite, with '*' meaning bit set and any other
    * non-whitespace character meaning bit unset.
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

  private val score = {
    val label = new JLabel("Score")
    val border = BorderFactory.createCompoundBorder(
      BorderFactory.createEmptyBorder(0, 0, ScoreBorderSize, 0),
      BorderFactory.createMatteBorder(0, 0, CanvasBorderSize, 0, Color.black)
    )
    label.setBorder(border)
    label
  }

  setBackground(BackgroundColor)
  setBorder(emptyBorder(BorderSize))
  setLayout(new BorderLayout)

  add(score, BorderLayout.NORTH)
  add(Canvas.create, BorderLayout.CENTER)

  Point.directions.keys.foreach { direction =>
    def add(name: String)(action: AbstractAction) = {
      getActionMap.put(name, action)
      getInputMap.put(KeyStroke.getKeyStroke(name), name)
    }

    add(direction) { _ => input = Point.directions.get(direction) }
    add(s"released $direction") { _ => input = None }
  }

  def getInput: Option[Point] = input

  def update(state: State): Unit =
    SwingUtilities.invokeLater { () =>
      image = state.render
      score.setText(state.renderScore)
      repaint()
    }

  class Canvas extends JComponent {
    // TODO build proper image instead
    override def paintComponent(g: Graphics) =
      image.foreach(point => g.drawLine(point.x, point.y, point.x, point.y))

    override def getPreferredSize =
      Dimension(DisplaySize.x, DisplaySize.y)
  }
  object Canvas {
    def create = {
      val panel = new JPanel
      val size = CanvasBorderSize
      val border = BorderFactory.createCompoundBorder(
        BorderFactory.createLineBorder(Color.black, size),
        emptyBorder(size)
      )
      panel.setBorder(border)
      panel.setBackground(BackgroundColor)
      panel.setLayout(new BorderLayout)
      panel.add(new Canvas, BorderLayout.CENTER)
      panel
    }
  }

  private def emptyBorder(size: Int) =
    BorderFactory.createEmptyBorder(size, size, size, size)
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
