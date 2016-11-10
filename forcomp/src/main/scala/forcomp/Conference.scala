package forcomp


object Conference {

  val slot1 = Time(9,0)
  val slot2 = Time(10,30)

  val slot3 = Time(13,0)
  val slot4 = Time(15,0)

  val aws   = Track(Seq(
    Talk("CloudFormation",            Set("Cloud", "Automation"), slot1),
    Talk("Using Scala in AWS Lambda", Set("Platforms", "Scala"),  slot2),
    Talk("Kinesis autoscaling",       Set("Cloud", "Automation"), slot3),
    Talk("Echo Programming with JS",  Set("Platforms", "JS"),     slot4)
  ))

  val scala = Track(Seq(
    Talk("Scala Native",                     Set("Scala", "Platforms"),       slot1),
    Talk("Shapeless in practice",            Set("Scala", "Libraries", "Foundations"), slot2),
    Talk("Scala.JS",                         Set("Platforms", "Scala", "JS"), slot3),
    Talk("Dependent Object Types explained", Set("Scala", "Foundations"),              slot4)
  ))

  val js    = Track(Seq(
    Talk("Orchestrating Service Deployments with Node", Set("Automation", "JS"),  slot1),
    Talk("Currying in JS",                              Set("Foundations", "JS"), slot2),
    Talk("GraphQL - Quo Vadis?",                        Set("Libraries"),         slot3)
  ))

  val schedule = Schedule(Set(aws,scala,js))
}

case class Talk(title: String, themes: Set[String], time: Time)

case class Track(talks: Seq[Talk]) {

  /** all talks in this track on the theme */
  def talksOn(theme: String): Seq[Talk] = talks.filter(_.themes.contains(theme))

  /** the talk in this track at the given time */
  def talkAt(time: Time): Option[Talk] = talks.find(_.time == time)

  /** all themes of talks in this track */
  def themes: Set[String] = talks.to[Set].flatMap(_.themes)

}

case class Schedule(tracks: Set[Track]) {

  // all talks
  def talks: Set[Talk] = tracks.flatMap(_.talks)

  // all talks at the given time on the theme
  def talksOn(time: Time, theme: String): Set[Talk] = for {
    track <- tracks
    talk <- track.talkAt(time)
    if talk.themes.contains(theme)
  } yield talk

  // counts how often a theme occurs overall
  // Optional task; write tests along with the implementation.
   def themeCount: Map[String, Int] = {
    for {
      (theme,list) <- talks.to[Seq].flatMap(_.themes).groupBy(identity)
    } yield (theme, list.size)
//    allThemes.groupBy(identity).map(p => (p._1, p._2.size))
  }

  // ranking of themes in descending order of occurrence in this schedule.
  // Optional task; write tests along with the implementation.
   def themeRank: List[String] = {
    themeCount.toList.sortBy(_._2).reverse.map(_._1)
  }
}

/**
  * helper class representing time as hours and minutes.
  */
case class Time(hours: Int = 0, minutes: Int = 0) {

  override def toString = f"$hours%02d:$minutes%02d"

}
