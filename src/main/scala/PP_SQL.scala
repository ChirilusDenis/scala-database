import scala.language.implicitConversions

trait PP_SQL_DB{
  def eval: Option[Database]
}

case class CreateTable(database: Database, tableName: String) extends PP_SQL_DB{
  def eval: Option[Database] = {
    Some(database.create(tableName))
  }
}

case class DropTable(database: Database, tableName: String) extends PP_SQL_DB{
  def eval: Option[Database] = {
    Some(database.drop(tableName))
  }
}

implicit def PP_SQL_DB_Create_Drop(t: (Option[Database], String, String)): Option[PP_SQL_DB] = {
  t._1 match
    case Some(db) => if(t._2.equals("CREATE")) {
      Some(CreateTable(db, t._3))
    } else if (t._2.equals("DROP")) {
      Some(DropTable(db, t._3))
    } else {
      None
    }
    case None => None
}

case class SelectTables(database: Database, tableNames: List[String]) extends PP_SQL_DB{
  def eval: Option[Database] = {
    database.selectTables(tableNames)
  }
}

implicit def PP_SQL_DB_Select(t: (Option[Database], String, List[String])): Option[PP_SQL_DB] = {
  t._1 match
    case Some(db) => if(t._2.equals("SELECT")) Some(SelectTables(db, t._3))
      else None
    case None => None
}

case class JoinTables(database: Database, table1: String, column1: String, table2: String, column2: String) extends PP_SQL_DB{
  def eval: Option[Database] = {
    database.join(table1, column1, table2, column2) match
      case Some(x) => Some(Database(List(x)))
      case None => None
  }
}

implicit def PP_SQL_DB_Join(t: (Option[Database], String, String, String, String, String)): Option[PP_SQL_DB] = {
  t._1 match
    case Some(db) => if(t._2.equals("JOIN")) {
      Some(JoinTables(db, t._3, t._4, t._5, t._6))
    } else None
    case None => None
}

trait PP_SQL_Table{
  def eval: Option[Table]
}

case class InsertRow(table:Table, values: Tabular) extends PP_SQL_Table{
  def eval: Option[Table] = {
    Some(values.foldLeft(table)((acc : Table, r) => acc.insert(r)))
  }
}

implicit def PP_SQL_Table_Insert(t: (Option[Table], String, Tabular)): Option[PP_SQL_Table] = {
  t._1 match
    case Some(tab) => if(t._2.equals("INSERT")) {
      Some(InsertRow(tab, t._3))
    } else None
    case None => None
}

case class UpdateRow(table: Table, condition: FilterCond, updates: Map[String, String]) extends PP_SQL_Table{
  def eval: Option[Table] = {
    Some(table.update(condition, updates))
  }
}

implicit def PP_SQL_Table_Update(t: (Option[Table], String, FilterCond, Map[String, String])): Option[PP_SQL_Table] = {
  t._1 match
    case Some(tab) => if(t._2.equals("UPDATE")) {
      Some(UpdateRow(tab, t._3, t._4))
    } else None
    case None => None
}

case class SortTable(table: Table, column: String) extends PP_SQL_Table{
  def eval: Option[Table] = {
    if (table.tableData.head.keys.toList.contains(column)) {
      Some(table.sort(column))
    } else {
      None
    }
  }
}

implicit def PP_SQL_Table_Sort(t: (Option[Table], String, String)): Option[PP_SQL_Table] = {
  t._1 match
    case Some(tab) => if (t._2.equals("SORT")) {
      Some(SortTable(tab, t._3))
    } else None
    case None => None
}

case class DeleteRow(table: Table, row: Row) extends PP_SQL_Table{
  def eval: Option[Table] = {
    Some(table.delete(row))
  }
}

implicit def PP_SQL_Table_Delete(t: (Option[Table], String, Row)): Option[PP_SQL_Table] = {
  t._1 match
    case Some(tab) => if(t._2.equals("DELETE")) {
      Some(DeleteRow(tab, t._3))
    } else None
    case None => None
}

case class FilterRows(table: Table, condition: FilterCond) extends PP_SQL_Table{
  def eval: Option[Table] = {
    Some(table.filter(condition))
  }
}

implicit def PP_SQL_Table_Filter(t: (Option[Table], String, FilterCond)): Option[PP_SQL_Table] = {
  t._1 match
    case Some(tab) => if(t._2.equals("FILTER")) {
      Some(FilterRows(tab, t._3))
    } else None
    case None => None
}

case class SelectColumns(table: Table, columns: List[String]) extends PP_SQL_Table{
  def eval: Option[Table] = {
    Some(table.select(columns))
  }
}

implicit def PP_SQL_Table_Select(t: (Option[Table], String, List[String])): Option[PP_SQL_Table] = {
  t._1 match
    case Some(tab) => if(t._2.equals("EXTRACT")) {
      Some(SelectColumns(tab, t._3))
    } else None
    case None => None
}

def queryT(p: Option[PP_SQL_Table]): Option[Table] = {
  p match
    case Some(q) => q.eval
    case None => None
}
def queryDB(p: Option[PP_SQL_DB]): Option[Database] = {
  p match
    case Some(q) => q.eval
    case None => None
}