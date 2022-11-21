//> using scala "2.13.10"

import javax.swing._
import java.awt.{List => _, _}
import java.awt.event._
import java.util.concurrent.atomic.AtomicReference
import scala.util.Random
import scala.util.chaining._
import Main._

// TODO remove after readme
/** Recreate the basic version of the classic Nokia 3310 Snake, no extras
  * Behaviour taken from observing https://helpfulsheep.com/snake/
  *
  * To run: scala-cli Snake.scala
  */
object Main {
  def main(args: Array[String]): Unit = {
    val gui = Gui.start

    var state = State.initial
    // TODO first frame takes a while to draw
    while (true) {
      state = state.evolve(gui.getInput)
      gui.update(state)
      Thread.sleep(FPS)
    }
  }

  val FPS = 1000 / 60
  val Dimensions = Point(22, 13)
  val SpriteSize = 4
  val Scale = 2

  val Center = Dimensions.times(0.5)
  val SnakeSize = 7
  val Velocities = Vector(41, 29, 23, 18, 14, 11, 8, 6, 5)
  val Flicker = 16
  val FlickerFor = 10
  val DefaultLevel = Level(9) // TODO maybe use level 8 instead
  val MonsterTTL = 20
  val MonsterSpawnIn = 5
  val MonsterSpawnRandom = 3

  val BackgroundColor = new Color(170, 220, 0)
  val FullDimensions = Dimensions.times(SpriteSize)
  val Margin = 4
  val DigitSize = Point(SpriteSize, 2 * SpriteSize)
  val UpperLine = 3
  val Border = 2
}

class Gui extends JPanel {
  private val input: AtomicReference[Input] = new AtomicReference(NoInput)
  private var image: Vector[Point] = Vector() // All reads and writes from EDT

  private val canvas = new JComponent {
    // TODO build proper image instead
    override def paintComponent(g: Graphics) =
      image.foreach(point => g.drawLine(point.x, point.y, point.x, point.y))

    override def getPreferredSize = new Dimension(
      (FullDimensions.x + 2 * (Margin + Border)) * Scale,
      (FullDimensions.y + 2 * (Margin + Border) + DigitSize.y + UpperLine) * Scale
    )
  }

  setBackground(BackgroundColor)
  setLayout(new BorderLayout)
  add(canvas, BorderLayout.CENTER)

  private def onKey(name: String, value: Input) = {
    val action: AbstractAction = _ => input.set(value)
    getActionMap.put(name, action)
    getInputMap.put(KeyStroke.getKeyStroke(name), name)
  }

  Direction.values.foreach { case (name, direction) => onKey(name, direction) }
  Level.values.zipWithIndex.foreach { case (level, n) =>
    onKey((n + 1).toString, level)
  }
  onKey(Pause.key, Pause)

  def getInput: Input = input.getAndSet(NoInput)

  def update(state: State): Unit =
    SwingUtilities.invokeLater { () =>
      image = state.render
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

// TODO add Pause and NoInput <-- remove Option
sealed trait Input
case class Direction(value: Point) extends Input
object Direction {
  def values: Map[String, Direction] = Map(
    "UP" -> Direction(Point.up),
    "DOWN" -> Direction(Point.down),
    "LEFT" -> Direction(Point.left),
    "RIGHT" -> Direction(Point.right)
  )
}
case class Level(value: Int) extends Input {
  def velocity: Int = Velocities(value - 1)
}
object Level {
  val values = 1.to(9).map(Level.apply)
}
case object NoInput extends Input
case object Pause extends Input {
  val key = "SPACE"
}

case class State(
    score: Int = 0,
    level: Level = DefaultLevel,
    recordedInput: Input = NoInput,
    paused: Boolean = false,
    snake: Vector[Entity],
    apple: Entity,
    eaten: Vector[Entity] = Vector.empty,
    dead: Boolean = false,
    openMouth: Boolean = false,
    monster: Vector[Entity] = Vector(),
    monsterSprite: Vector[Sprite] = Vector(),
    monsterTTL: Int = MonsterTTL,
    monsterSpawnIn: Int = MonsterSpawnIn,
    flickers: Long = 0,
    drawSnake: Boolean = true,
    velocity: Int = DefaultLevel.velocity,
    time: Int = 0
) {

  def evolve(input: Input): State = {

    def move(next: Direction) = {
      val directionNow =
        if (next.value != snake.head.direction.opposite) next.value
        else snake.head.direction

      val headNow = snake.head.move(directionNow)

      val hasEaten = eaten.headOption.exists(snake.head.hits)
      val eatingApple = headNow.hits(apple)
      val eatingMonster = monster.exists(headNow.hits)
      val swallowed = eaten.lastOption.exists(snake.last.hits)
      val dead = snake.tail.exists(headNow.hits)
      val aboutToEat = // TODO opens when wrapping around, probably shouldn't
        (apple +: monster).exists(headNow.move(directionNow).hits)

      val snakeNow = headNow +: (if (hasEaten) snake else snake.init)

      val eatenNow =
        Vector(headNow).filter(_ => eatingApple || eatingMonster) ++
          (if (swallowed) eaten.init else eaten)

      val appleNow =
        if (eatingApple) State.newApple(snakeNow) else apple

      val scoreNow = {
        val level = this.level.value
        if (eatingApple) score + level
        else if (eatingMonster) // Magic formula due to observation
          score + 5 * (level + 10) - 2 * (MonsterTTL - monsterTTL) - (level - 2)
        else score
      }

      val ((monsterNow, monsterSpriteNow), monsterSpawnInNow, monsterTTLNow) =
        if (monster.isEmpty) {
          val monsterSpawnInNow =
            if (eatingApple) monsterSpawnIn - 1 else monsterSpawnIn
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
            ((Vector.empty, Vector.empty), monsterSpawnIn, MonsterTTL)
          else ((monster, monsterSprite), monsterSpawnIn, monsterTTLNow)
        }

      if (dead) copy(dead = true, velocity = Flicker)
      else
        copy(
          score = scoreNow,
          snake = snakeNow,
          apple = appleNow,
          eaten = eatenNow,
          openMouth = aboutToEat,
          monster = monsterNow,
          monsterSprite = monsterSpriteNow,
          monsterSpawnIn = monsterSpawnInNow,
          monsterTTL = monsterTTLNow
        )
    }

    def flicker =
      if (flickers > FlickerFor) State.initial
      else if (flickers % 2 != 0)
        copy(drawSnake = true, flickers = flickers + 1)
      else copy(drawSnake = false, flickers = flickers + 1)

    val actualInput =
      if (input == NoInput) recordedInput else input

    if (time % velocity != 0) copy(recordedInput = actualInput)
    else if (dead) flicker
    else
      {
        actualInput match {
          case direction: Direction => move(direction).copy(paused = false)
          case level: Level => copy(level = level, velocity = level.velocity, paused = false)
          case NoInput =>
            if (paused) this else move(Direction(snake.head.direction))
          case Pause => copy(paused = !paused)
        }
      }.copy(recordedInput = NoInput)
  }.copy(time = time + 1)

  def render: Vector[Point] = {
    // TODO inner margin for snake asymetric (visible with up-down motion)
    val entities = {
      val apple = Sprite.apple.at(this.apple.position)

      val monster =
        monsterSprite
          .zip(this.monster)
          .flatMap { case (sprite, p) => sprite.at(p.position) }

      val head =
        (if (!openMouth) Sprite.head else Sprite.headOpen)
          .apply(this.snake.head.direction)
          .at(this.snake.head.position)

      val tail =
        Sprite.tail
          .apply(this.snake.init.last.direction)
          .at(this.snake.last.position)

      val body =
        this.snake.init
          .sliding(2)
          .collect { case Vector(headward, tailward) =>
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
          }
          .flatten

      val snake =
        if (!drawSnake) Vector.empty
        else head ++ tail ++ body

      val offset =
        Point(Margin + Border, Margin + Border + DigitSize.y + UpperLine)

      (apple ++ monster ++ snake).map(_.move(offset))
    }

    def renderDigits(target: Int, offset: Point, precision: Int) = {
      val points =
        0.until(precision)
          .toVector
          .map(i => Point.right.times(i))
          .map(p => Vector(p, p.move(Point.down)))

      val sprites =
        String
          .format(s"%0${precision}d", target)
          .toVector
          .map(n => Sprite.digits(n.asDigit))

      points
        .zip(sprites)
        .flatMap { case (points, digits) =>
          digits.zip(points).flatMap { case (sprite, p) => sprite.at(p) }
        }
        .map(_.move(offset))
    }

    val scoreline = {
      val score =
        renderDigits(this.score, offset = Point(Margin, Margin), precision = 4)

      // TODO should I display 00 or not? I think the original game does not
      val monsterTimer = if (monster.nonEmpty) {
        val precision = 2
        val ttlOffset = Point(
          Margin + Border + FullDimensions.x - precision * DigitSize.x,
          Margin
        )
        val spriteOffset =
          ttlOffset.move(Point(-precision * DigitSize.x, SpriteSize / 2))

        renderDigits(monsterTTL, ttlOffset, precision) ++
          monsterSprite.zipWithIndex
            .flatMap { case (sprite, x) => sprite.at(Point(x, 0)) }
            .map(_.move(spriteOffset))
      } else Vector.empty

      score ++ monsterTimer
    }

    val borders = {
      val lineOffset = Point(Margin, Margin + DigitSize.y)
      val edgeOffset = lineOffset.move(Point(0, UpperLine))
      val X = FullDimensions.x + 2 * Border
      val Y = FullDimensions.y + 2 * Border

      val line =
        0
          .to(FullDimensions.x + 2 * Border)
          .map(x => Point(x, 0))
          .map(p => p.move(lineOffset))

      val border = for {
        x <- 0.to(X).toVector // inclusive
        y <- 0.to(Y).toVector
        if (x == 0 || x == X) || (y == 0 || y == Y)
      } yield Point(x, y).move(edgeOffset)

      border ++ line
    }

    (entities ++ scoreline ++ borders).flatMap(_.times(Scale).square(Scale))
  }
}
object State {
  def initial: State = {
    val snake =
      Vector
        .range(0, SnakeSize)
        .map(x => Entity(Center.move(Point.left.times(x)), Point.right))

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
    val monster = Vector(point, point.move(Point.right)).map(Entity.static)
    val collision = monster.exists(p => (apple +: snake).exists(p.hits))
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

  // TODO see if I can replicate the pixel-y squares the phone had
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

  def sprite(mask: String): Map[Point, Sprite] = {
    val sprite = Sprite.parse(mask)
    Map(
      Point.right -> sprite,
      Point.left -> sprite.mirrorY,
      Point.up -> sprite.anti,
      Point.down -> sprite.clock.mirrorY
    )
  }

  def cornerSprite(mask: String): Map[(Point, Point), Sprite] = {
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

  val apple: Sprite = """
-*--
*-*-
-*--
----
""".pipe(parse(_))

  val head: Map[Point, Sprite] = """
*---
-**-
***-
----
""".pipe(sprite)

  val headOpen: Map[Point, Sprite] = """
*-*-
-*--
**--
--*-
""".pipe(sprite)

  val body: Map[Point, Sprite] = """
----
**-*
*-**
----
""".pipe(sprite)

  val bodyFull: Map[Point, Sprite] = """
-**-
**-*
*-**
-**-
""".pipe(sprite)

  val tail: Map[Point, Sprite] = """
----
--**
****
----
""".pipe(sprite)

  val turn: Map[(Point, Point), Sprite] = """
-**-
*-*-
**--
----
""".pipe(cornerSprite)

  val turnFull: Map[(Point, Point), Sprite] = """
***-
*-*-
**--
----
""".pipe(cornerSprite)

  val monsters: Vector[Vector[Sprite]] =
    """
--**---- --****-- -*-*-*-- -------- **---*-- ----**--
-*-**-*- ******** *-*****- *------- **--***- *--**-*-
*******- *-****-* ******** ******** --****** *-*****-
--****-- *-*--*-* --*--*-- -*-*-*-* ----*-*- -*******
""".pipe(parseRow).grouped(2).toVector

  val digits: Vector[Vector[Sprite]] = """
----------------------------------------
-***--*--***-***-*-*-***-***-***-***-***
-*-*-**----*---*-*-*-*---*-----*-*-*-*-*
-*-*--*--***-***-***-***-***--*--***-***
-*-*--*--*-----*---*---*-*-*--*--*-*---*
-***--*--***-***---*-***-***--*--***-***
----------------------------------------
----------------------------------------
""".pipe { digits =>
    val (firstHalf, secondHalf) = digits.splitAt(digits.length / 2)
    val (topRow, bottomRow) = (parseRow(firstHalf), parseRow(secondHalf))
    topRow.zip(bottomRow).map { case (top, bottom) => Vector(top, bottom) }
  }
}
