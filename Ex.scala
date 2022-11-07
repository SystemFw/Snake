//> using lib "com.lihaoyi::os-lib:0.8.1"

object Main {
  def main(args: Array[String]): Unit =
    val wd = os.pwd / "img2"
    os.makeDir(wd)
    urls.foreach { name =>
      os.proc("curl", "-L" , s"$base$name").spawn(stdout = wd / name)
    }

  val base = "https://helpfulsheep.com/snake/images/"

  val urls = List(
    "bodyd.png",
    "bodyfullr.png",
    "food1.png",
    "headopenl.png",
    "nibble.png",
    "tailu.png",
    "maze0.png",
  )
  // val urls = List(
  //   "headu.png",
  //   "headd.png",
  //   "headl.png",
  //   "headr.png",
  //   "headopenu.png",
  //   "headopend.png",
  //   "headopenl.png",
  //   "headopenr.png",
  //   "bodyu.png",
  //   "bodyd.png",
  //   "bodyl.png",
  //   "bodyr.png",
  //   "bodyfullu.png",
  //   "bodyfulld.png",
  //   "bodyfulll.png",
  //   "bodyfullr.png",
  //   "turn2.png",
  //   "turn1.png",
  //   "turn1.png",
  //   "turn0.png",
  //   "turn3.png",
  //   "turn0.png",
  //   "turn2.png",
  //   "turn3.png",
  //   "turnfull2.png",
  //   "turnfull1.png",
  //   "turnfull1.png",
  //   "turnfull0.png",
  //   "turnfull3.png",
  //   "turnfull0.png",
  //   "turnfull2.png",
  //   "turnfull3.png",
  //   "tailu.png",
  //   "taild.png",
  //   "taill.png",
  //   "tailr.png",
  //   "nibble.png",
  //   "food0.png",
  //   "food1.png",
  //   "food2.png",
  //   "food3.png",
  //   "food4.png",
  //   "food5.png",
  //   "maze0.png",
  //   "maze1.png",
  //   "maze2.png",
  //   "maze3.png",
  //   "maze4.png",
  //   "maze5.png",
  //   "maze6.png",
  //   "maze7.png"
  // )

}
