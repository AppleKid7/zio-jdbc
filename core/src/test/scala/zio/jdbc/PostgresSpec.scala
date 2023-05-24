package zio.jdbc

import com.dimafeng.testcontainers.PostgreSQLContainer
import io.github.scottweaver.zio.testcontainers.postgres.ZPostgreSQLContainer
import zio._
import zio.json._
import zio.json.ast.Json
// import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._
import java.sql.Connection
import io.github.scottweaver.models.JdbcInfo
import org.postgresql.ds.PGSimpleDataSource

// test("json schema-derived") {
//                 for {
//                   _     <- /*createJsonAlias *>*/ createTransactions *> insertTransfer
//                   value <- transaction {
//                              val to = transactionTransfer.data
//                                .asInstanceOf[Json.JObject]
//                                .fields
//                                .toMap
//                                .get("to")
//                                .map(_.asInstanceOf[Json.JString].s)
//                                .getOrElse("")
//                              //sql"select data from transactions where data ~* \'to:\s*${to}\'"
//                              //sql"select data from transactions where regexp_like(data, \'to:${to}\')"
//                              // sql"select data from transactions where data->$$.to = ${to}"
//                              sql"select data from transactions where json_value(data, $$.to) = ${to}"
//                                .query[GenericTransaction](
//                                  GenericTransaction.jdbcDecoder
//                                )
//                                .selectOne
//                            }
//                   _     <- Console.printLine(s"@@@@ value: $value")
//                 } yield assertTrue(value.contains(GenericTransaction(transactionTransfer.data)))
//               }

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

  final case class GenericTransaction(data: Json)
  object GenericTransaction {
    implicit val encoder: JsonEncoder[GenericTransaction] =
      DeriveJsonEncoder.gen[GenericTransaction]

    implicit val decoder: JsonDecoder[GenericTransaction] =
      DeriveJsonDecoder.gen[GenericTransaction]
  }

  val transactionTransfer: GenericTransaction = GenericTransaction(
    Json.Obj(
      Chunk(
        ("to", Json.Str("000000000")),
        ("from", Json.Str("000000001"))
      )
    )
  )

  val createTransactions: ZIO[ZConnectionPool with Any, Throwable, Unit] =
    transaction {
      sql"""
      create table transactions (
        id serial not null primary key,
        data json not null
      )
      """.execute
    }

  val insertTransfer: ZIO[ZConnectionPool with Any, Throwable, UpdateResult] =
    transaction {
      val data = transactionTransfer.data
      sql"insert into transactions values (default, ${data.toJson}::json)".insert
    }

  def spec: Spec[TestEnvironment, Any] =
    suite("ZConnectionPoolSpec integration tests") {
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
            } yield assertTrue(result.rowsUpdated == 1L) && assertTrue(result.updatedKeys.nonEmpty)
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
