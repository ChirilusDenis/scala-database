
case class Database(tables: List[Table]) {
  override def toString: String = tables.map(_.tableName).toString()

  def create(tableName: String): Database = {
    tables.find(_.tableName.equals(tableName)) match
      case None => new Database(tables :+ new Table(tableName, List()))
      case Some(_) => new Database(tables)
  }

  def drop(tableName: String): Database = {
    new Database(tables.filterNot(_.tableName.equals(tableName)))
  }

  def selectTables(tableNames: List[String]): Option[Database] = {
    if (tableNames.forall(s => tables.map(_.tableName).contains(s))) {
      Some(new Database(tables.filter((t : Table) => tableNames.contains(t.tableName))))
    } else None
  }

  def myCombine(big : Map[String, String], small : Map[String, String]) : Map[String, String] = {
    val com = big.keys.toList.filter(s => small.keys.toList.contains(s) && !big(s).equals("") && !big(s).equals(small(s)))
    (big ++ small).map((k : String, v : String) => if(com.contains(k)) {
      (k, big(k) + ";" + v)
    } else {
      (k, v)
    })
  }

  def join(table1: String, c1: String, table2: String, c2: String): Option[Table] =
  {
    (tables.find(_.tableName.equals(table1)), tables.find(_.tableName.equals(table2))) match
      case (Some(t1), Some(t2)) =>
          val comNames = Table(t1.tableName, t1.tableData.filter(r1 => t2.tableData.find(r2 => r2(c2).equals(r1(c1))) match
            case Some(_) => true
            case None => false
            )).select(List(c1))
          val com1 = Table(t1.tableName, t1.select(List(c1)).tableData.foldLeft(comNames : Table)(( acc: Table, r1: Row) =>
            acc.tableData.find(r => r(c1).equals(r1(c1))) match
              case None => acc.insert(Map(c1 -> r1(c1)))
              case _ => acc).tableData)
          val com2 = Table(t1.tableName, t2.select(List(c2)).tableData.foldLeft(com1 : Table)((acc : Table, r2 : Row) =>
            acc.tableData.find(r => r(c1).equals(r2(c2))) match
              case None => acc.insert(Map(c1 -> r2(c2)))
              case _ => acc).tableData)
          val S = com2.tableData.map(r => r ++ t1.tableData.head.keys.toList.filterNot(s => s.equals(c1)).map((_, "")).toMap)
            .map(r => r ++ t2.tableData.head.keys.toList.filterNot(s => s.equals(c2)).map((_, "")).toMap)
            .map(r => t1.tableData.find(r1 => r1(c1).equals(r(c1))) match
              case Some(x) => myCombine(r, x.removed(c1))
              case None => r)
            .map(r => t2.tableData.find(r2 => r2(c2).equals(r(c1))) match
              case Some(x) => myCombine(r, x.removed(c2))
              case None => r)
          
          Some(Table(t1.tableName, S))
      case _ => None
  }

  // Implement indexing here
  def apply(i : Int) : Table = tables(i)
}
