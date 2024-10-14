import scala.language.implicitConversions

trait FilterCond {def eval(r: Row): Option[Boolean]}

case class Field(colName: String, predicate: String => Boolean) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = {
    r.get(colName) match
      case None => None
      case Some(x) => Some(predicate(x))
  }
}

case class Compound(op: (Boolean, Boolean) => Boolean, conditions: List[FilterCond]) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = {
    Some(conditions.tail.foldLeft(conditions.head.eval(r).get)((acc : Boolean, f : FilterCond) => op(acc, f.eval(r).get)))
  }
}

case class Not(f: FilterCond) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = {
    f.eval(r) match
      case None => None
      case Some(x) => Some(!x)
  }
}

def And(f1: FilterCond, f2: FilterCond): FilterCond = Compound(_&&_, List(f1,f2))
def Or(f1: FilterCond, f2: FilterCond): FilterCond = Compound(_||_, List(f1, f2))
def Equal(f1: FilterCond, f2: FilterCond): FilterCond = Compound(_==_, List(f1, f2))

case class Any(fs: List[FilterCond]) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = {
    Some(fs.foldLeft(false)((acc : Boolean, f : FilterCond) => acc || f.eval(r).get))
  }
}

case class All(fs: List[FilterCond]) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = {
    Some(fs.foldLeft(true)((acc : Boolean, f : FilterCond) => acc && f.eval(r).get))
  }
}

implicit def tuple2Field(t: (String, String => Boolean)): Field = Field(t._1, t._2)

extension (f: FilterCond) {
  def ===(other: FilterCond) = Equal(f, other)
  def &&(other: FilterCond) = And(f, other)
  def ||(other: FilterCond) = Or(f, other)
  def !! = Not(f)
}