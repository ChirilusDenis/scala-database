type Row = Map[String, String]
type Tabular = List[Row]

case class Table (tableName: String, tableData: Tabular) {

  override def toString: String = {
    val one = tableData.head.keys.mkString("", ",", "\n")
    tableData.foldLeft(one)((acc : String, row : Row) => acc.concat(row.values.mkString("",",","\n"))).dropRight(1)
  }

  def insert(row: Row): Table = {
    if (tableData.contains(row)) {
      new Table(tableName, tableData)
    }else new Table(tableName, tableData :+ row)
  }

  def delete(row: Row): Table = {
    new Table(tableName, tableData.filterNot(_.equals(row)))
  }

  def sort(column: String): Table = {
    new Table(tableName, tableData.sortWith((r1 : Row, r2 : Row) => r1(column).compareTo(r2(column)) < 0))
  }

  def update(f: FilterCond, updates: Map[String, String]): Table = {
    new Table(tableName, tableData.map((r : Row) => if (f.eval(r).get) {
      r ++ updates
    }
    else r))
  }

  def filter(f: FilterCond): Table = new Table(tableName, tableData.filter(f.eval(_).get))

  def select(columns: List[String]): Table = {
    new Table(tableName, tableData.map(_.filter((pair : (String, String)) => columns.contains(pair._1))))
  }

  def header: List[String] = tableData.head.keys.toList
  def data: Tabular = tableData
  def name: String = tableName
}

object Table {
  def apply(name: String, s: String): Table = {
    val csv = s.split("\n").map(_.split(","))
    val head = csv.head.toList
    val tail = csv.tail.toList.map(_.toList)
    new Table(name, tail.map(head.zip(_).toMap))
  }
}

extension (table: Table) {
  def apply(i: Int): Table = new Table(table.tableName, List(table.tableData(i))) // Implement indexing here, find the right function to override
}
