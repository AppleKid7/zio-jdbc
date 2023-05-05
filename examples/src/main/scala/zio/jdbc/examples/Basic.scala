package zio.jdbc.examples

import zio._
import zio.jdbc._

object Basic {
  val age = 42

  val ex0: SqlFragment = sql"create table if not exists users(name varchar(255), age int)"

  // Creating SQL statements using interpolation:
  val ex1: SqlFragment = sql"select * from users where age = $age"

  // Selecting into tuples:
  val ex2: Query[(String, Int)] = sql"select name, age from users".query[(String, Int)]

  // Inserting from tuples:
  val ex3: SqlFragment = sql"insert into users (name, age)".values(("John", 42))

  // Inserting from case class
  val ex4: SqlFragment = sql"insert into users (name, age)".values(User("John", 42))

  // dropping table
  val ex5: SqlFragment = sql"drop table if exists users"

  // Composing requests:
  val keyColumn                                  = "key"
  val valueColumn                                = "value"
  def ex4(offset: Long): Query[(String, String)] =
    (s"select $keyColumn, $valueColumn from events" ++ sql"where id > $offset").query[(String, String)]

  // Executing statements:
  val res1: ZIO[ZConnectionPool, Throwable, Option[(String, Int)]] =
    transaction {
      sql"select name, age from users where name = 'Sherlock Holmes'".query[(String, Int)].selectOne
    }
}
