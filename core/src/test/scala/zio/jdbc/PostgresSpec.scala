package zio.jdbc

import com.dimafeng.testcontainers.PostgreSQLContainer
import io.github.scottweaver.zio.testcontainers.postgres.ZPostgreSQLContainer
import zio._
import zio.schema._
import zio.test.TestAspect._
import zio.test._

import java.sql.Connection
import io.github.scottweaver.models.JdbcInfo
import org.postgresql.ds.PGSimpleDataSource

object PostgresSpec extends ZIOSpecDefault {

  case class Props(host: String, port: Int, databaseName: String, props: Map[String, String])

  val zConnectionPoolProps: ZLayer[
    ZPostgreSQLContainer.Settings with JdbcInfo with Connection with PGSimpleDataSource with PostgreSQLContainer,
    Nothing,
    Props
  ] = ZLayer.apply(for {
    settings   <- ZIO.service[ZPostgreSQLContainer.Settings]
    _          <- ZIO.service[JdbcInfo]
    _          <- ZIO.service[Connection]
    dataSource <- ZIO.service[PGSimpleDataSource]
    container  <- ZIO.service[PostgreSQLContainer]
    props       = Map(
                    "user"     -> settings.username,
                    "password" -> settings.password
                  )
  } yield Props(container.host, dataSource.getPortNumbers()(0), dataSource.getDatabaseName(), props))

  val connectionPool: ZIO[Props, Nothing, ZLayer[ZConnectionPoolConfig, Nothing, ZConnectionPool]] =
    for {
      props <- ZIO.service[Props]
    } yield ZConnectionPool
      .postgres(
        props.host,
        props.port,
        props.databaseName,
        props.props
      )
      .orDie

  val connectionPoolLayer: ZLayer[Props with ZConnectionPoolConfig, Nothing, ZConnectionPool] =
    ZLayer.fromZIO(connectionPool).flatten

  case class GenericTransaction(data: zio.json.ast.Json)
  object GenericTransaction {
    implicit val schemaJson: Schema[zio.json.ast.Json] = zio.schema.codec.json.schemaJson

    // implicit val schema: Schema[GenericTransaction] = DeriveSchema.gen[GenericTransaction]
    implicit val schema: Schema[GenericTransaction] =
      Schema.CaseClass1[zio.json.ast.Json, GenericTransaction](
        TypeId.parse(classOf[GenericTransaction].getName),
        zio.schema.Schema.Field[GenericTransaction, zio.json.ast.Json]("data", schemaJson, get0 = _.data, set0 = (x, v) => x.copy(data = v)),
        GenericTransaction.apply
      )
    val deriver: Deriver[JdbcEncoder] = JdbcEncoder.deriver
//    implicit val jsonJdbcEncoder: JdbcEncoder[zio.json.ast.Json] = Derive.derive[JdbcEncoder, zio.json.ast.Json](deriver)(schemaJson)
    implicit val jsonJdbcEncoder: JdbcEncoder[zio.json.ast.Json] = JdbcEncoder.fromSchema[zio.json.ast.Json]
//    implicit val jsonJdbcEncoder: JdbcEncoder[zio.json.ast.Json] = JdbcEncoder.fromSchema[zio.json.ast.Json](schemaJson)
    implicit val jdbcEncoder: JdbcEncoder[GenericTransaction] = Derive.derive[JdbcEncoder, GenericTransaction](deriver)(schema)
//    implicit val jdbcEncoder: JdbcEncoder[GenericTransaction] = JdbcEncoder.fromSchema(schema)

    implicit val jdbcDecoder: JdbcDecoder[GenericTransaction] = JdbcDecoder.fromSchema(schema)
  }

  val transactionTransfer: GenericTransaction = GenericTransaction(
    zio.json.ast.Json.Obj(
      "destination" -> zio.json.ast.Json.Str("000000001"),
      "origin" -> zio.json.ast.Json.Str("000000000")
    )
  )

  val createTransactions: ZIO[ZConnectionPool, Throwable, Unit] =
    transaction {
      sql"""
      create table transactions (
        id serial not null primary key,
        data json not null
      )
      """.execute
    }

  val selectTransaction: SqlFragment =
    sql"select data from transactions where data ->> 'destination' = '000000001'"

  val insertTransfer: ZIO[ZConnectionPool, Throwable, UpdateResult[Long]] =
    transaction {
//      sql"insert into transactions values (default, ${data.toJson}::json)".insertWithKeys
      // val sql = SqlFragment.insertInto("transactions")("data").values(transactionTransfer)(GenericTransaction.jdbcEncoder)
      val sql = SqlFragment.insertInto("transactions")("data").values(transactionTransfer.data)(GenericTransaction.jsonJdbcEncoder)
      println(s"@@@@ sql: $sql")
      sql.insertWithKeys
    }

  def spec: Spec[TestEnvironment, Any] =
    suite("PostgresSpec integration tests") {
      suite("Postgresql JSON field test") {
        test("create Json table") {
          for {
            _ <- createTransactions
          } yield assertCompletes
        } +
          test("insert Json") {
            for {
              _      <- createTransactions
              result <- insertTransfer
              _ <- Console.printLine(s"@@@@ inserting json: ${result.toString}").debug
            } yield assertTrue(result.rowsUpdated == 1L, result.updatedKeys.nonEmpty)
          } +
          test("json schema-derived") {
            for {
              _     <- createTransactions *> insertTransfer
              value <- transaction {
                        // val to = transactionTransfer.data.to
                        // val destination = "000000001"
                         //sql"select data from transactions where data ~* \'to:\s*${to}\'"
                         //sql"select data from transactions where regexp_like(data, \'to:${to}\')"
                        //  sql"select data from transactions where json_value(data, $$.to) = ${to}"
                        // sql"select data from transactions where data ->> 'destination' = ${destination}"
                        //   .query[GenericTransaction](
                        //     GenericTransaction.jdbcDecoder
                        //   )
                        //   .selectOne
                         selectTransaction.query[GenericTransaction](
                          GenericTransaction.jdbcDecoder  
                         ).selectOne
                       }
              _ <- ZIO.debug(s"@@@@@ value: $value")
            } yield assertTrue(value.contains(transactionTransfer))
          }
      }
    }.provide(
      ZPostgreSQLContainer.live,
      ZLayer.succeed(ZConnectionPoolConfig.default),
      zConnectionPoolProps,
      connectionPoolLayer,
      ZPostgreSQLContainer.Settings.default
    ) @@ sequential
}
