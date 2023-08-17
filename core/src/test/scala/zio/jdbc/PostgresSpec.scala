package zio.jdbc

import com.dimafeng.testcontainers.PostgreSQLContainer
import io.github.scottweaver.zio.testcontainers.postgres.ZPostgreSQLContainer
import zio._
import zio.json._
import zio.schema._
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

  sealed trait Json
  object Json {
    case object JNull                                extends Json
    case class JString(s: String)                    extends Json
    case class JNumber(l: Int)                       extends Json
    case class JDecimal(d: Double)                   extends Json
    case class JObject(fields: List[(String, Json)]) extends Json
    case class JArray(fields: List[Json])            extends Json

    // implicit lazy val schema: Schema.Enum6[_, _, _, _, _, _, Json] =
    //   DeriveSchema.gen[Json]
    implicit lazy val schema: Schema[Json] = DeriveSchema.gen[Json]

    implicit val encoder: JsonEncoder[Json] =
      DeriveJsonEncoder.gen[Json]

    implicit val decoder: JsonDecoder[Json] =
      DeriveJsonDecoder.gen[Json]

    implicit val jdbcDecoder = JdbcDecoder.fromSchema(schema)
  }

  final case class GenericTransaction1(data: Json)
  object GenericTransaction1 {
    implicit val encoder: JsonEncoder[GenericTransaction1] =
      DeriveJsonEncoder.gen[GenericTransaction1]

    implicit val decoder: JsonDecoder[GenericTransaction1] =
      DeriveJsonDecoder.gen[GenericTransaction1]

    val schema = DeriveSchema.gen[GenericTransaction1]


    // val schema: Schema[GenericTransaction] =
    //   Schema.CaseClass1[Json, GenericTransaction](
    //     TypeId.parse(classOf[GenericTransaction].getName),
    //     Field("data", Schema[Json], get0 = _.data, set0 = (x, v) => x.copy(data = v)),
    //     GenericTransaction.apply
    //   )
    // val decoder = zio.schema.codec.JsonCodec.JsonDecoder

    // implicit val jdbcDecoder = JdbcDecoder.fromSchema(schema)
    import Json._
    // implicit val jdbcDecoder: JdbcDecoder[GenericTransaction1] =
    //   JdbcDecoder[Json]().map[GenericTransaction1](t => GenericTransaction1(t))
    implicit val jdbcDecoder = JdbcDecoder.fromSchema(schema)

    implicit val jdbcEncoder: JdbcEncoder[GenericTransaction1] = (value: GenericTransaction1) => {
      val data = value.data
      sql"""${data.toJson}"""
    }
  }

  final case class Data(to: String, from: String)
  object Data {
    implicit val encoder: JsonEncoder[Data] =
      DeriveJsonEncoder.gen[Data]

    implicit val decoder: JsonDecoder[Data] =
      DeriveJsonDecoder.gen[Data]

    implicit val jsonSchema = zio.schema.codec.JsonCodec

    val schema = DeriveSchema.gen[Data]

    implicit val jdbcDecoder = JdbcDecoder.fromSchema(schema)
  }

  final case class GenericTransaction2(data: Data)
  object GenericTransaction2 {
    implicit val encoder: JsonEncoder[GenericTransaction2] =
      DeriveJsonEncoder.gen[GenericTransaction2]

    implicit val decoder: JsonDecoder[GenericTransaction2] =
      DeriveJsonDecoder.gen[GenericTransaction2]

    implicit val jsonSchema = zio.schema.codec.JsonCodec

    // val schema: Schema[GenericTransaction] =
    //   Schema.CaseClass1[Json, GenericTransaction](
    //     TypeId.parse(classOf[GenericTransaction].getName),
    //     Field("data", Schema[Json], get0 = _.data, set0 = (x, v) => x.copy(data = v)),
    //     GenericTransaction.apply
    //   )
    val schema = DeriveSchema.gen[GenericTransaction2]

    implicit val jdbcDecoder = JdbcDecoder.fromSchema(schema)
  }

  // val transactionTransfer: GenericTransaction = GenericTransaction(
  //   Json.Obj(
  //     Chunk(
  //       ("to", Json.Str("000000000")),
  //       ("from", Json.Str("000000001"))
  //     )
  //   )
  // )

  // val transactionTransfer: GenericTransaction2 = GenericTransaction2(Data("000000000", "000000001"))
  val transactionTransfer: GenericTransaction1 = GenericTransaction1(
    Json.JObject(
      List(
        ("to", Json.JString("000000000")),
        ("from", Json.JString("000000001"))
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
      println(data.toJson)
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
          } +
          test("json schema-derived") {
                for {
                  _     <- createTransactions *> insertTransfer
                  value <- transaction {
                            // val to = transactionTransfer.data.to
                            val to = "000000000"
                             //sql"select data from transactions where data ~* \'to:\s*${to}\'"
                             //sql"select data from transactions where regexp_like(data, \'to:${to}\')"
                            //  sql"select data from transactions where json_value(data, $$.to) = ${to}"
                            sql"select data from transactions where data ->> 'to' = ${to}"
                               .query[GenericTransaction1](
                                 GenericTransaction1.jdbcDecoder
                               )
                               .selectAll
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
