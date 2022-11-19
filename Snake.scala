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
    // TODO first frame takes a while to draw
    while (true) {
      Thread.sleep(Tick)
      state = state.evolve(gui.getInput)
      gui.update(state)
    }
  }

  val Dimensions = Point(22, 13)
  val SpriteSize = 4
  val Scale = 2

  // ticks = List(658, 478, 378, 298, 228, 178, 138, 108, 88)
  // TODO maybe use level 8 instead (108), should adjust Flicker and Level is so
  val Tick = 88
  val FlickerEvery = 3
  val FlickerFor = 8

  val Center = Dimensions.times(0.5)
  val SnakeSize = 7

  // TODO it would be cool to change level dynamically my pressing numbers
  // (0 or maybe space is pause)
  val Level = 9
  val MonsterTTL = 20
  val MonsterSpawnIn = 5
  val MonsterSpawnRandom = 3

  val BackgroundColor = Color(170, 220, 0)
  val FullDimensions = Dimensions.times(SpriteSize)
  val Margin = 4
  val DigitSize = Point(SpriteSize, 2 * SpriteSize)
  val UpperLine = 3
  val Border = 2

  // TODO inner margin for snake asymetric (visible with up-down motion)
  // TODO move these to the appropriate places, at point of use
  // Note these get scaled at the point of use


  val EdgeOffset = Point(
    Margin,
    Margin + DigitSize.y + UpperLine
  )

  val LineOffset = Point(
    Margin,
    Margin + DigitSize.y
  )

  val ScoreOffset = Point(
    Margin,
    Margin
  )

  // TODO replace 2 with precision after moving all these to point of use
  val MonsterTTLOffset = Point(
    Margin + Border + FullDimensions.x - 2 * DigitSize.x,
    Margin
  )

  val MonsterSpriteOffset = Point(
    MonsterTTLOffset.x - 2 * DigitSize.x,
    MonsterTTLOffset.y + SpriteSize / 2
  )
}

class Gui extends JPanel {
  // Writes from event listeners which run on single EDT thread: no atomics needed
  // Reads from main thread: volatile needed
  @volatile private var input: Option[Point] = None
  // All reads and writes from single EDT thread: can be standard references
  private var image: Vector[Point] = Vector()

  private val canvas = new JComponent {
    // TODO build proper image instead
    override def paintComponent(g: Graphics) =
      image.foreach(point => g.drawLine(point.x, point.y, point.x, point.y))

    override def getPreferredSize = Dimension(
      (FullDimensions.x + 2 * (Margin + Border)) * Scale,
      (FullDimensions.y + 2 * (Margin + Border) + DigitSize.y + UpperLine) * Scale
    )
  }

  setBackground(BackgroundColor)
  setLayout(new BorderLayout)
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

  // TODO repaint() already calls invokeLater
  // could make image volatile and remove the invokeLater?
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
    monsterTTL: Int = MonsterTTL,
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
          score + 5 * (Level + 10) - 2 * (MonsterTTL - monsterTTL) - (Level - 2)
        else score

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

        val body = // TODO collect flatten to void the case _
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

    val renderedEntities = {
      val snakeOffset =
        Point(Margin + Border, Margin + Border + DigitSize.y + UpperLine)

      (renderedSnake ++ renderedFood).map(_.move(snakeOffset))
    }

    // TODO abstract this into a helper
    val renderedScore = {
      val p = Point(0, 0) // DigitOffset
      val precision = 4

      val points =
        0.until(precision)
          .toVector
          .map(i => p.move(Point.right.times(i)))
          .map(p => Vector(p, p.move(Point.down)))

      val sprites =
        String
          .format(s"%0${precision}d", score)
          .toVector
          .map(n => Sprite.digits(n.asDigit))

      points
        .zip(sprites)
        .flatMap { (points, digits) =>
          digits.zip(points).flatMap { case (sprite, p) => sprite.at(p) }
        }
    }.map(_.move(ScoreOffset))

    // TODO should I display 00 or not? I think the original game does not
    val renderedMonsterTTL = {
      val p = Point(0, 0) // DigitOffset
      val precision = 2

      val points =
        0.until(precision)
          .toVector
          .map(i => p.move(Point.right.times(i)))
          .map(p => Vector(p, p.move(Point.down)))

      val sprites =
        String
          .format(s"%0${precision}d", monsterTTL)
          .toVector
          .map(n => Sprite.digits(n.asDigit))

      if (monster.nonEmpty)
        points
          .zip(sprites)
          .flatMap { (points, digits) =>
            digits.zip(points).flatMap { case (sprite, p) => sprite.at(p) }
          }
      else Vector.empty
    }.map(_.move(MonsterTTLOffset))

    val renderedMonsterSprite = {
      if (monster.nonEmpty)
        Vector(Point(0, 0), Point(1, 0)).zip(monsterSprite).flatMap {
          case (p, sprite) => sprite.at(p)
        }
        else Vector.empty
    }.map(_.move(MonsterSpriteOffset))

    // TODO both edge and line can be moved to state object, they are static
    val renderedEdge = {
      val X = FullDimensions.x + 2 * Border
      val Y = FullDimensions.y + 2 * Border

      for {
        x <- 0.to(X).toVector // inclusive
        y <- 0.to(Y).toVector
        if (x == 0 || x == X) || (y == 0 || y == Y)
      } yield Point(x, y)
    }.map(_.move(EdgeOffset))

    val renderedLine =
      0
        .to(FullDimensions.x + 2 * Border)
        .map(x => Point(x, 0))
        .map(_.move(LineOffset))

    // TODO Refactor
    (renderedEntities ++ renderedScore ++ renderedLine ++ renderedEdge ++ renderedMonsterTTL ++ renderedMonsterSprite).flatMap(_.times(Scale).square(Scale))
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

    // TODO remove after debug
    // val apple = Random.shuffle(
    //   Vector(
    //     Point(0, 0),
    //     Point(21, 0),
    //     Point(0, 12),
    //     Point(21, 12))
    // ).head.pipe(Entity.static)
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
