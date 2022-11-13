import javax.swing._
import java.awt.{List => _, _}
import java.awt.event._
import scala.util.Random
import scala.util.chaining._
import Main._

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
      Thread.sleep(Tick)
      state = state.evolve(gui.getInput)
      gui.update(state)
    }
  }

  val Dimensions = Point(22, 13)
  val SpriteSize = 4
  val Scale = 2

  val Tick = 88
  val FlickerEvery = 3
  val FlickerFor = 8

  val Centre = Dimensions.times(0.5)
  val SnakeSize = 7

  val Level = 9
  val MonsterTimer = 20
  val MonsterSpawnIn = 5
  val MonsterSpawnRandom = 3

  val DisplaySize = Dimensions.times(SpriteSize).times(Scale)
  val BackgroundColor = Color(170, 220, 0)
  val BorderSize = 10
  val ScoreBorderSize = 6
  val CanvasBorderSize = 2
}

class Gui extends JPanel {
  // Writes from event listeners which run on single EDT thread: no atomics needed
  // Reads from main thread: volatile needed
  @volatile private var input: Option[Point] = None
  // All reads and writes from single EDT thread: can be standard references
  private var image: Vector[Point] = Vector()

  private def emptyBorder(size: Int) =
    BorderFactory.createEmptyBorder(size, size, size, size)

  // TODO classic game displays even borders and numbers "manually"
  private val score = {
    val label = new JLabel("Score")
    val border = BorderFactory.createCompoundBorder(
      BorderFactory.createEmptyBorder(0, 0, ScoreBorderSize, 0),
      BorderFactory.createMatteBorder(0, 0, CanvasBorderSize, 0, Color.black)
    )
    label.setBorder(border)
    label
  }

  private val canvas = {
    val canvas = new JComponent {
      // TODO build proper image instead
      override def paintComponent(g: Graphics) =
        image.foreach(point =>
          g.drawLine(
            point.x + CanvasBorderSize,
            point.y + CanvasBorderSize,
            point.x + CanvasBorderSize,
            point.y + CanvasBorderSize
          )
        )

      override def getPreferredSize =
        Dimension(
          DisplaySize.x + CanvasBorderSize,
          DisplaySize.y + CanvasBorderSize
        )
    }

    val panel = new JPanel
    val border = BorderFactory.createCompoundBorder(
      BorderFactory.createLineBorder(Color.black, CanvasBorderSize),
      emptyBorder(CanvasBorderSize)
    )
    panel.setBorder(border)
    panel.setBackground(BackgroundColor)
    panel.setLayout(new BorderLayout)
    panel.add(canvas, BorderLayout.CENTER)
    panel
  }

  setBackground(BackgroundColor)
  setBorder(emptyBorder(BorderSize))
  setLayout(new BorderLayout)

  add(score, BorderLayout.NORTH)
  add(canvas, BorderLayout.CENTER)

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

// TODO maybe review the extent to which Entity is used, most of the
// uses are based on position, could use entity as dumb data, change
// headNow, eaten, apple and monster back to Points, write a collision
// detection with intersection and more the wrapping to Point.move
// or maybe just have hits take a Point again
// a directions: Vector[Point] seems also doable, updated in one place only
case class State(
    snake: Vector[Entity],
    apple: Entity,
    eaten: Vector[Entity] = Vector.empty,
    score: Int = 0,
    lostAt: Long = 0,
    time: Long = 0,
    drawSnake: Boolean = true,
    openMouth: Boolean = false,
    monster: Vector[Entity] = Vector(),
    monsterSprite: Vector[Sprite] = Vector(),
    monsterTTL: Int = MonsterTimer,
    monsterSpawnIn: Int = MonsterSpawnIn
) {

  def evolve(next: Option[Point]): State = {
    def move = {
      val directionNow =
        next
          .filter(_ != snake.head.direction.opposite)
          .getOrElse(snake.head.direction)

      val headNow = snake.head.move(directionNow)

      val hasEaten = eaten.headOption.exists(snake.head.hits)
      val eatingApple = headNow.hits(apple)
      val eatingMonster = monster.exists(headNow.hits)
      val swallowed = eaten.lastOption.exists(snake.last.hits)
      val dead = snake.tail.exists(headNow.hits)
      val aboutToEat =
        (apple +: monster).exists(headNow.move(directionNow).hits)

      val snakeNow = headNow +: (if (hasEaten) snake else snake.init)

      val eatenNow =
        Vector(headNow).filter(_ => eatingApple || eatingMonster) ++
          (if (swallowed) eaten.init else eaten)

      val appleNow =
        if (eatingApple) State.newApple(snakeNow) else apple

      val scoreNow =
        if (eatingApple) score + Level
        else if (eatingMonster)
          // Magic formula due to observation
          score + 5 * (Level + 10) - 2 * (20 - monsterTTL) - (Level - 2)
        else score

      val ((monsterNow, monsterSpriteNow), monsterSpawnInNow, monsterTTLNow) =
        if (monster.isEmpty) {
          val monsterSpawnInNow =
            if(eatingApple) monsterSpawnIn - 1 else monsterSpawnIn
          if (monsterSpawnInNow == 0)
            (
              State.newMonster(snakeNow, appleNow),
              MonsterSpawnIn + Random.nextInt(MonsterSpawnRandom),
              monsterTTL
            )
          else ((monster, monsterSprite), monsterSpawnInNow, monsterTTL)
        } else {
          val monsterTTLNow = monsterTTL - 1
          if (eatingMonster || monsterTTL == 0)
            ((Vector.empty, Vector.empty), monsterSpawnIn, MonsterTimer)
          else ((monster, monsterSprite), monsterSpawnIn, monsterTTLNow)
        }

      if (dead) copy(lostAt = time)
      else
        copy(
          snake = snakeNow,
          apple = appleNow,
          eaten = eatenNow,
          score = scoreNow,
          openMouth = aboutToEat,
          monster = monsterNow,
          monsterSprite = monsterSpriteNow,
          monsterSpawnIn = monsterSpawnInNow,
          monsterTTL = monsterTTLNow
        )
    }

    def flickerOnLoss = {
      val flicker = (time / FlickerEvery) % 2 == 0

      if (time - lostAt > FlickerFor * FlickerEvery) State.initial
      else if (!flicker) copy(drawSnake = true)
      else copy(drawSnake = false)
    }

    if (lostAt > 0) flickerOnLoss
    else move
  }.copy(time = time + 1)

  def render: Vector[Point] = {
    val renderedFood =
      Sprite.apple.at(apple.position) ++
        monsterSprite
          .zip(monster)
          .flatMap { case (sprite, p) => sprite.at(p.position) }

    val renderedSnake =
      if (!drawSnake) Vector.empty
      else {
        val head =
          (if (!openMouth) Sprite.head else Sprite.headOpen)
            .apply(snake.head.direction)
            .at(snake.head.position)

        val tail =
          Sprite.tail.apply(snake.init.last.direction).at(snake.last.position)

        val body =
          snake.init.sliding(2).flatMap {
            case Vector(headward, tailward) =>
              val (body, turn) =
                if (eaten.exists(tailward.hits))
                  (Sprite.bodyFull, Sprite.turnFull)
                else (Sprite.body, Sprite.turn)

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

  // TODO show monster timer properly
  def renderScore: String =
    String.format("%04d", score) ++ " ".repeat(34) ++ (if (monster.nonEmpty)
                                                         monsterTTL.toString
                                                       else "")
}
object State {
  def initial: State = {
    val snake =
      Vector
        .range(0, SnakeSize)
        .map(x => Entity(Centre.move(Point.left.times(x)), Point.right))

    val apple = newApple(snake)
    State(snake = snake, apple = apple)
  }

  def newApple(snake: Vector[Entity]): Entity = {
    val apple = Point(
      Random.nextInt(Dimensions.x),
      Random.nextInt(Dimensions.y)
    ).pipe(Entity.static)

    if (snake.exists(_.hits(apple))) newApple(snake)
    else apple
  }

  def newMonster(
      snake: Vector[Entity],
      apple: Entity
  ): (Vector[Entity], Vector[Sprite]) = {
    val size = 2
    val point = Point(
      Random.nextInt(Dimensions.x) / size * size,
      Random.nextInt(Dimensions.y)
    )

    val monster =
      Vector(point, point.move(Point.right)).map(Entity.static)

    val collision =
      monster.exists(p => (apple +: snake).exists(p.hits))

    val sprite = Random.shuffle(Sprite.monsters).head

    if (collision) newMonster(snake, apple) else (monster, sprite)
  }
}

case class Entity(position: Point, direction: Point) {
  def move(to: Point) =
    Entity(position.move(to).wrap(Dimensions), to.direction)

  def hits(target: Entity) = position == target.position
}
object Entity {
  def static(position: Point) = Entity(position, Point(0, 0))
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
      val target = (tileIndex * SpriteSize + x) + (y * SpriteSize * tiles)
      Option.when(input(target) == '*')(Point(x, y))
    }
  }

  /** Parses the input as a row of 4x4 sprites */
  def parseRow(mask: String): Vector[Sprite] = {
    val input = mask.filterNot(_.isWhitespace)
    val size = input.length / (SpriteSize * SpriteSize)

    Vector.range(0, size).map(tileIndex => parse(input, tileIndex, size))
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
""".pipe(parse(_))

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

  val monsters = Vector(
    """
--**----
-*-**-*-
*******-
--****--
""",
    """
--****--
********
*-****-*
*-*--*-*
""",
    """
-*-*-*--
*-*****-
********
--*--*--
""",
    """
--------
*-------
********
-*-*-*-*
""",
    """
**---*--
**--***-
--******
----*-*-
""",
    """
----**--
*--**-*-
*-*****-
-*******
"""
  ).map(parseRow)
}
