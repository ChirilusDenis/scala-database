object Queries {

  def killJackSparrow(t: Table): Option[Table] =
    queryT((Some(t), "FILTER", Field("name", !_.equals("Jack"))))

  def insertLinesThenSort(db: Database): Option[Table] =
    queryT((queryT((Some(queryDB((queryDB((Some(db), "CREATE", "Inserted Fellas")),"SELECT", List("Inserted Fellas")) ).get(0)), "INSERT", List(Map("name" ->"Ana", "age" ->"93", "CNP"->"455550555"), Map("name" ->"Diana", "age" ->"33", "CNP"->"255532142"), Map("name" ->"Tatiana", "age" ->"55", "CNP"->"655532132"), Map("name" ->"Rosmaria", "age" ->"12", "CNP"->"855532172")))), "SORT", "age"))

  def youngAdultHobbiesJ(db: Database): Option[Table] =
    queryT( (queryT((Some(queryDB(queryDB((Some(db), "JOIN", "People", "name", "Hobbies", "name")), "SELECT", List("People")).get(0)), "FILTER", Field("age", _.compareTo("25") < 0) && Field("name", _.startsWith("J")) && Field("hobby", !_.equals("")))), "EXTRACT", List("name", "hobby")))
}
