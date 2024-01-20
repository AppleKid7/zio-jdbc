/*
 * Copyright 2022 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package zio.jdbc

import zio.Chunk
import zio.schema.{ Schema, StandardType }

import scala.reflect.ClassTag

/**
 * A type class that describes the ability to convert a value of type `A` into
 * a fragment of SQL. This is useful for forming SQL insert statements.
 */
trait JdbcEncoder[-A] {
  def encode(value: A): SqlFragment

  final def contramap[B](f: B => A): JdbcEncoder[B] = value => encode(f(value))
}

object JdbcEncoder extends JdbcEncoder0LowPriorityImplicits {
  def apply[A](implicit encoder: JdbcEncoder[A]): JdbcEncoder[A] = encoder

  implicit val intEncoder: JdbcEncoder[Int]                               = value => sql"$value"
  implicit val longEncoder: JdbcEncoder[Long]                             = value => sql"$value"
  implicit val doubleEncoder: JdbcEncoder[Double]                         = value => sql"$value"
  implicit val charEncoder: JdbcEncoder[Char]                             = value => sql"$value"
  implicit val stringEncoder: JdbcEncoder[String]                         = value => sql"$value"
  implicit val booleanEncoder: JdbcEncoder[Boolean]                       = value => sql"$value"
  implicit val bigIntEncoder: JdbcEncoder[java.math.BigInteger]           = value => sql"$value"
  implicit val bigDecimalEncoder: JdbcEncoder[java.math.BigDecimal]       = value => sql"$value"
  implicit val bigDecimalEncoderScala: JdbcEncoder[scala.math.BigDecimal] = value => sql"$value"
  implicit val shortEncoder: JdbcEncoder[Short]                           = value => sql"$value"
  implicit val floatEncoder: JdbcEncoder[Float]                           = value => sql"$value"
  implicit val byteEncoder: JdbcEncoder[Byte]                             = value => sql"$value"
  implicit val byteArrayEncoder: JdbcEncoder[Array[Byte]]                 = value => sql"$value"
  implicit val byteChunkEncoder: JdbcEncoder[Chunk[Byte]]                 = value => sql"$value"
  implicit val blobEncoder: JdbcEncoder[java.sql.Blob]                    = value => sql"$value"
  implicit val uuidEncoder: JdbcEncoder[java.util.UUID]                   = value => sql"$value"
//  implicit val jsonEncoder: JdbcEncoder[zio.json.ast.Json]                = value => sql"${value.toJson}::json"

  implicit def singleParamEncoder[A: SqlFragment.Setter]: JdbcEncoder[A] = value => sql"$value"

  // TODO: review for cases like Option of a tuple
  def optionEncoder[A](implicit encoder: JdbcEncoder[A]): JdbcEncoder[Option[A]] =
    value => value.fold(SqlFragment.nullLiteral)(encoder.encode)

  implicit def tuple2Encoder[A: JdbcEncoder, B: JdbcEncoder]: JdbcEncoder[(A, B)] =
    tuple => JdbcEncoder[A].encode(tuple._1) ++ SqlFragment.comma ++ JdbcEncoder[B].encode(tuple._2)

  implicit def tuple3Encoder[A: JdbcEncoder, B: JdbcEncoder, C: JdbcEncoder]: JdbcEncoder[(A, B, C)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) ++ SqlFragment.comma ++ JdbcEncoder[B].encode(
        tuple._2
      ) ++ SqlFragment.comma ++ JdbcEncoder[C].encode(tuple._3)

  implicit def tuple4Encoder[A: JdbcEncoder, B: JdbcEncoder, C: JdbcEncoder, D: JdbcEncoder]
    : JdbcEncoder[(A, B, C, D)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) ++ SqlFragment.comma ++ JdbcEncoder[B].encode(
        tuple._2
      ) ++ SqlFragment.comma ++ JdbcEncoder[C].encode(tuple._3) ++ SqlFragment.comma ++ JdbcEncoder[D].encode(
        tuple._4
      )

  implicit def tuple5Encoder[A: JdbcEncoder, B: JdbcEncoder, C: JdbcEncoder, D: JdbcEncoder, E: JdbcEncoder]
    : JdbcEncoder[(A, B, C, D, E)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) ++ SqlFragment.comma ++ JdbcEncoder[B].encode(
        tuple._2
      ) ++ SqlFragment.comma ++ JdbcEncoder[C].encode(tuple._3) ++ SqlFragment.comma ++ JdbcEncoder[D].encode(
        tuple._4
      ) ++ SqlFragment.comma ++ JdbcEncoder[E].encode(tuple._5)

  implicit def tuple6Encoder[
    A: JdbcEncoder,
    B: JdbcEncoder,
    C: JdbcEncoder,
    D: JdbcEncoder,
    E: JdbcEncoder,
    F: JdbcEncoder
  ]: JdbcEncoder[(A, B, C, D, E, F)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) ++ SqlFragment.comma ++ JdbcEncoder[B].encode(
        tuple._2
      ) ++ SqlFragment.comma ++ JdbcEncoder[C].encode(tuple._3) ++ SqlFragment.comma ++ JdbcEncoder[D].encode(
        tuple._4
      ) ++ SqlFragment.comma ++ JdbcEncoder[E].encode(tuple._5) ++ SqlFragment.comma ++ JdbcEncoder[F].encode(
        tuple._6
      )

  implicit def tuple7Encoder[
    A: JdbcEncoder,
    B: JdbcEncoder,
    C: JdbcEncoder,
    D: JdbcEncoder,
    E: JdbcEncoder,
    F: JdbcEncoder,
    G: JdbcEncoder
  ]: JdbcEncoder[(A, B, C, D, E, F, G)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) ++ SqlFragment.comma ++ JdbcEncoder[B].encode(
        tuple._2
      ) ++ SqlFragment.comma ++ JdbcEncoder[C].encode(tuple._3) ++ SqlFragment.comma ++ JdbcEncoder[D].encode(
        tuple._4
      ) ++ SqlFragment.comma ++ JdbcEncoder[E].encode(tuple._5) ++ SqlFragment.comma ++ JdbcEncoder[F].encode(
        tuple._6
      ) ++ SqlFragment.comma ++ JdbcEncoder[G].encode(tuple._7)

  implicit def tuple8Encoder[
    A: JdbcEncoder,
    B: JdbcEncoder,
    C: JdbcEncoder,
    D: JdbcEncoder,
    E: JdbcEncoder,
    F: JdbcEncoder,
    G: JdbcEncoder,
    H: JdbcEncoder
  ]: JdbcEncoder[(A, B, C, D, E, F, G, H)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) ++ SqlFragment.comma ++ JdbcEncoder[B].encode(
        tuple._2
      ) ++ SqlFragment.comma ++ JdbcEncoder[C].encode(tuple._3) ++ SqlFragment.comma ++ JdbcEncoder[D].encode(
        tuple._4
      ) ++ SqlFragment.comma ++ JdbcEncoder[E].encode(tuple._5) ++ SqlFragment.comma ++ JdbcEncoder[F].encode(
        tuple._6
      ) ++ SqlFragment.comma ++ JdbcEncoder[G].encode(tuple._7) ++ SqlFragment.comma ++ JdbcEncoder[H].encode(
        tuple._8
      )

  implicit def tuple9Encoder[
    A: JdbcEncoder,
    B: JdbcEncoder,
    C: JdbcEncoder,
    D: JdbcEncoder,
    E: JdbcEncoder,
    F: JdbcEncoder,
    G: JdbcEncoder,
    H: JdbcEncoder,
    I: JdbcEncoder
  ]: JdbcEncoder[(A, B, C, D, E, F, G, H, I)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) ++ SqlFragment.comma ++ JdbcEncoder[B].encode(
        tuple._2
      ) ++ SqlFragment.comma ++ JdbcEncoder[C].encode(tuple._3) ++ SqlFragment.comma ++ JdbcEncoder[D].encode(
        tuple._4
      ) ++ SqlFragment.comma ++ JdbcEncoder[E].encode(tuple._5) ++ SqlFragment.comma ++ JdbcEncoder[F].encode(
        tuple._6
      ) ++ SqlFragment.comma ++ JdbcEncoder[G].encode(tuple._7) ++ SqlFragment.comma ++ JdbcEncoder[H].encode(
        tuple._8
      ) ++ SqlFragment.comma ++ JdbcEncoder[I].encode(tuple._9)

  implicit def tuple10Encoder[
    A: JdbcEncoder,
    B: JdbcEncoder,
    C: JdbcEncoder,
    D: JdbcEncoder,
    E: JdbcEncoder,
    F: JdbcEncoder,
    G: JdbcEncoder,
    H: JdbcEncoder,
    I: JdbcEncoder,
    J: JdbcEncoder
  ]: JdbcEncoder[(A, B, C, D, E, F, G, H, I, J)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) ++ SqlFragment.comma ++ JdbcEncoder[B].encode(
        tuple._2
      ) ++ SqlFragment.comma ++ JdbcEncoder[C].encode(tuple._3) ++ SqlFragment.comma ++ JdbcEncoder[D].encode(
        tuple._4
      ) ++ SqlFragment.comma ++ JdbcEncoder[E].encode(tuple._5) ++ SqlFragment.comma ++ JdbcEncoder[F].encode(
        tuple._6
      ) ++ SqlFragment.comma ++ JdbcEncoder[G].encode(tuple._7) ++ SqlFragment.comma ++ JdbcEncoder[H].encode(
        tuple._8
      ) ++ SqlFragment.comma ++ JdbcEncoder[I].encode(tuple._9) ++ SqlFragment.comma ++ JdbcEncoder[J].encode(
        tuple._10
      )

  implicit def tuple11Encoder[
    A: JdbcEncoder,
    B: JdbcEncoder,
    C: JdbcEncoder,
    D: JdbcEncoder,
    E: JdbcEncoder,
    F: JdbcEncoder,
    G: JdbcEncoder,
    H: JdbcEncoder,
    I: JdbcEncoder,
    J: JdbcEncoder,
    K: JdbcEncoder
  ]: JdbcEncoder[(A, B, C, D, E, F, G, H, I, J, K)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) ++ SqlFragment.comma ++ JdbcEncoder[B].encode(
        tuple._2
      ) ++ SqlFragment.comma ++ JdbcEncoder[C].encode(tuple._3) ++ SqlFragment.comma ++ JdbcEncoder[D].encode(
        tuple._4
      ) ++ SqlFragment.comma ++ JdbcEncoder[E].encode(tuple._5) ++ SqlFragment.comma ++ JdbcEncoder[F].encode(
        tuple._6
      ) ++ SqlFragment.comma ++ JdbcEncoder[G].encode(tuple._7) ++ SqlFragment.comma ++ JdbcEncoder[H].encode(
        tuple._8
      ) ++ SqlFragment.comma ++ JdbcEncoder[I].encode(tuple._9) ++ SqlFragment.comma ++ JdbcEncoder[J].encode(
        tuple._10
      ) ++ SqlFragment.comma ++ JdbcEncoder[K].encode(tuple._11)

  implicit def tuple12Encoder[
    A: JdbcEncoder,
    B: JdbcEncoder,
    C: JdbcEncoder,
    D: JdbcEncoder,
    E: JdbcEncoder,
    F: JdbcEncoder,
    G: JdbcEncoder,
    H: JdbcEncoder,
    I: JdbcEncoder,
    J: JdbcEncoder,
    K: JdbcEncoder,
    L: JdbcEncoder
  ]: JdbcEncoder[(A, B, C, D, E, F, G, H, I, J, K, L)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) ++ SqlFragment.comma ++ JdbcEncoder[B].encode(
        tuple._2
      ) ++ SqlFragment.comma ++ JdbcEncoder[C].encode(tuple._3) ++ SqlFragment.comma ++ JdbcEncoder[D].encode(
        tuple._4
      ) ++ SqlFragment.comma ++ JdbcEncoder[E].encode(tuple._5) ++ SqlFragment.comma ++ JdbcEncoder[F].encode(
        tuple._6
      ) ++ SqlFragment.comma ++ JdbcEncoder[G].encode(tuple._7) ++ SqlFragment.comma ++ JdbcEncoder[H].encode(
        tuple._8
      ) ++ SqlFragment.comma ++ JdbcEncoder[I].encode(tuple._9) ++ SqlFragment.comma ++ JdbcEncoder[J].encode(
        tuple._10
      ) ++ SqlFragment.comma ++ JdbcEncoder[K].encode(tuple._11) ++ SqlFragment.comma ++ JdbcEncoder[L].encode(
        tuple._12
      )

  implicit def tuple13Encoder[
    A: JdbcEncoder,
    B: JdbcEncoder,
    C: JdbcEncoder,
    D: JdbcEncoder,
    E: JdbcEncoder,
    F: JdbcEncoder,
    G: JdbcEncoder,
    H: JdbcEncoder,
    I: JdbcEncoder,
    J: JdbcEncoder,
    K: JdbcEncoder,
    L: JdbcEncoder,
    M: JdbcEncoder
  ]: JdbcEncoder[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) ++ SqlFragment.comma ++ JdbcEncoder[B].encode(
        tuple._2
      ) ++ SqlFragment.comma ++ JdbcEncoder[C].encode(tuple._3) ++ SqlFragment.comma ++ JdbcEncoder[D].encode(
        tuple._4
      ) ++ SqlFragment.comma ++ JdbcEncoder[E].encode(tuple._5) ++ SqlFragment.comma ++ JdbcEncoder[F].encode(
        tuple._6
      ) ++ SqlFragment.comma ++ JdbcEncoder[G].encode(tuple._7) ++ SqlFragment.comma ++ JdbcEncoder[H].encode(
        tuple._8
      ) ++ SqlFragment.comma ++ JdbcEncoder[I].encode(tuple._9) ++ SqlFragment.comma ++ JdbcEncoder[J].encode(
        tuple._10
      ) ++ SqlFragment.comma ++ JdbcEncoder[K].encode(tuple._11) ++ SqlFragment.comma ++ JdbcEncoder[L].encode(
        tuple._12
      ) ++ SqlFragment.comma ++ JdbcEncoder[M].encode(tuple._13)

  implicit def tuple14Encoder[
    A: JdbcEncoder,
    B: JdbcEncoder,
    C: JdbcEncoder,
    D: JdbcEncoder,
    E: JdbcEncoder,
    F: JdbcEncoder,
    G: JdbcEncoder,
    H: JdbcEncoder,
    I: JdbcEncoder,
    J: JdbcEncoder,
    K: JdbcEncoder,
    L: JdbcEncoder,
    M: JdbcEncoder,
    N: JdbcEncoder
  ]: JdbcEncoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) ++ SqlFragment.comma ++ JdbcEncoder[B].encode(
        tuple._2
      ) ++ SqlFragment.comma ++ JdbcEncoder[C].encode(tuple._3) ++ SqlFragment.comma ++ JdbcEncoder[D].encode(
        tuple._4
      ) ++ SqlFragment.comma ++ JdbcEncoder[E].encode(tuple._5) ++ SqlFragment.comma ++ JdbcEncoder[F].encode(
        tuple._6
      ) ++ SqlFragment.comma ++ JdbcEncoder[G].encode(tuple._7) ++ SqlFragment.comma ++ JdbcEncoder[H].encode(
        tuple._8
      ) ++ SqlFragment.comma ++ JdbcEncoder[I].encode(tuple._9) ++ SqlFragment.comma ++ JdbcEncoder[J].encode(
        tuple._10
      ) ++ SqlFragment.comma ++ JdbcEncoder[K].encode(tuple._11) ++ SqlFragment.comma ++ JdbcEncoder[L].encode(
        tuple._12
      ) ++ SqlFragment.comma ++ JdbcEncoder[M].encode(tuple._13) ++ SqlFragment.comma ++ JdbcEncoder[N].encode(
        tuple._14
      )

  implicit def tuple15Encoder[
    A: JdbcEncoder,
    B: JdbcEncoder,
    C: JdbcEncoder,
    D: JdbcEncoder,
    E: JdbcEncoder,
    F: JdbcEncoder,
    G: JdbcEncoder,
    H: JdbcEncoder,
    I: JdbcEncoder,
    J: JdbcEncoder,
    K: JdbcEncoder,
    L: JdbcEncoder,
    M: JdbcEncoder,
    N: JdbcEncoder,
    O: JdbcEncoder
  ]: JdbcEncoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) ++ SqlFragment.comma ++ JdbcEncoder[B].encode(
        tuple._2
      ) ++ SqlFragment.comma ++ JdbcEncoder[C].encode(tuple._3) ++ SqlFragment.comma ++ JdbcEncoder[D].encode(
        tuple._4
      ) ++ SqlFragment.comma ++ JdbcEncoder[E].encode(tuple._5) ++ SqlFragment.comma ++ JdbcEncoder[F].encode(
        tuple._6
      ) ++ SqlFragment.comma ++ JdbcEncoder[G].encode(tuple._7) ++ SqlFragment.comma ++ JdbcEncoder[H].encode(
        tuple._8
      ) ++ SqlFragment.comma ++ JdbcEncoder[I].encode(tuple._9) ++ SqlFragment.comma ++ JdbcEncoder[J].encode(
        tuple._10
      ) ++ SqlFragment.comma ++ JdbcEncoder[K].encode(tuple._11) ++ SqlFragment.comma ++ JdbcEncoder[L].encode(
        tuple._12
      ) ++ SqlFragment.comma ++ JdbcEncoder[M].encode(tuple._13) ++ SqlFragment.comma ++ JdbcEncoder[N].encode(
        tuple._14
      ) ++ SqlFragment.comma ++ JdbcEncoder[O].encode(tuple._15)

  implicit def tuple16Encoder[
    A: JdbcEncoder,
    B: JdbcEncoder,
    C: JdbcEncoder,
    D: JdbcEncoder,
    E: JdbcEncoder,
    F: JdbcEncoder,
    G: JdbcEncoder,
    H: JdbcEncoder,
    I: JdbcEncoder,
    J: JdbcEncoder,
    K: JdbcEncoder,
    L: JdbcEncoder,
    M: JdbcEncoder,
    N: JdbcEncoder,
    O: JdbcEncoder,
    P: JdbcEncoder
  ]: JdbcEncoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) ++ SqlFragment.comma ++ JdbcEncoder[B].encode(
        tuple._2
      ) ++ SqlFragment.comma ++ JdbcEncoder[C].encode(tuple._3) ++ SqlFragment.comma ++ JdbcEncoder[D].encode(
        tuple._4
      ) ++ SqlFragment.comma ++ JdbcEncoder[E].encode(tuple._5) ++ SqlFragment.comma ++ JdbcEncoder[F].encode(
        tuple._6
      ) ++ SqlFragment.comma ++ JdbcEncoder[G].encode(tuple._7) ++ SqlFragment.comma ++ JdbcEncoder[H].encode(
        tuple._8
      ) ++ SqlFragment.comma ++ JdbcEncoder[I].encode(tuple._9) ++ SqlFragment.comma ++ JdbcEncoder[J].encode(
        tuple._10
      ) ++ SqlFragment.comma ++ JdbcEncoder[K].encode(tuple._11) ++ SqlFragment.comma ++ JdbcEncoder[L].encode(
        tuple._12
      ) ++ SqlFragment.comma ++ JdbcEncoder[M].encode(tuple._13) ++ SqlFragment.comma ++ JdbcEncoder[N].encode(
        tuple._14
      ) ++ SqlFragment.comma ++ JdbcEncoder[O].encode(tuple._15) ++ SqlFragment.comma ++ JdbcEncoder[P].encode(
        tuple._16
      )

  implicit def tuple17Encoder[
    A: JdbcEncoder,
    B: JdbcEncoder,
    C: JdbcEncoder,
    D: JdbcEncoder,
    E: JdbcEncoder,
    F: JdbcEncoder,
    G: JdbcEncoder,
    H: JdbcEncoder,
    I: JdbcEncoder,
    J: JdbcEncoder,
    K: JdbcEncoder,
    L: JdbcEncoder,
    M: JdbcEncoder,
    N: JdbcEncoder,
    O: JdbcEncoder,
    P: JdbcEncoder,
    Q: JdbcEncoder
  ]: JdbcEncoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) ++ SqlFragment.comma ++ JdbcEncoder[B].encode(
        tuple._2
      ) ++ SqlFragment.comma ++ JdbcEncoder[C].encode(tuple._3) ++ SqlFragment.comma ++ JdbcEncoder[D].encode(
        tuple._4
      ) ++ SqlFragment.comma ++ JdbcEncoder[E].encode(tuple._5) ++ SqlFragment.comma ++ JdbcEncoder[F].encode(
        tuple._6
      ) ++ SqlFragment.comma ++ JdbcEncoder[G].encode(tuple._7) ++ SqlFragment.comma ++ JdbcEncoder[H].encode(
        tuple._8
      ) ++ SqlFragment.comma ++ JdbcEncoder[I].encode(tuple._9) ++ SqlFragment.comma ++ JdbcEncoder[J].encode(
        tuple._10
      ) ++ SqlFragment.comma ++ JdbcEncoder[K].encode(tuple._11) ++ SqlFragment.comma ++ JdbcEncoder[L].encode(
        tuple._12
      ) ++ SqlFragment.comma ++ JdbcEncoder[M].encode(tuple._13) ++ SqlFragment.comma ++ JdbcEncoder[N].encode(
        tuple._14
      ) ++ SqlFragment.comma ++ JdbcEncoder[O].encode(tuple._15) ++ SqlFragment.comma ++ JdbcEncoder[P].encode(
        tuple._16
      ) ++ SqlFragment.comma ++ JdbcEncoder[Q].encode(tuple._17)

  implicit def tuple18Encoder[
    A: JdbcEncoder,
    B: JdbcEncoder,
    C: JdbcEncoder,
    D: JdbcEncoder,
    E: JdbcEncoder,
    F: JdbcEncoder,
    G: JdbcEncoder,
    H: JdbcEncoder,
    I: JdbcEncoder,
    J: JdbcEncoder,
    K: JdbcEncoder,
    L: JdbcEncoder,
    M: JdbcEncoder,
    N: JdbcEncoder,
    O: JdbcEncoder,
    P: JdbcEncoder,
    Q: JdbcEncoder,
    R: JdbcEncoder
  ]: JdbcEncoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) ++ SqlFragment.comma ++ JdbcEncoder[B].encode(
        tuple._2
      ) ++ SqlFragment.comma ++ JdbcEncoder[C].encode(tuple._3) ++ SqlFragment.comma ++ JdbcEncoder[D].encode(
        tuple._4
      ) ++ SqlFragment.comma ++ JdbcEncoder[E].encode(tuple._5) ++ SqlFragment.comma ++ JdbcEncoder[F].encode(
        tuple._6
      ) ++ SqlFragment.comma ++ JdbcEncoder[G].encode(tuple._7) ++ SqlFragment.comma ++ JdbcEncoder[H].encode(
        tuple._8
      ) ++ SqlFragment.comma ++ JdbcEncoder[I].encode(tuple._9) ++ SqlFragment.comma ++ JdbcEncoder[J].encode(
        tuple._10
      ) ++ SqlFragment.comma ++ JdbcEncoder[K].encode(tuple._11) ++ SqlFragment.comma ++ JdbcEncoder[L].encode(
        tuple._12
      ) ++ SqlFragment.comma ++ JdbcEncoder[M].encode(tuple._13) ++ SqlFragment.comma ++ JdbcEncoder[N].encode(
        tuple._14
      ) ++ SqlFragment.comma ++ JdbcEncoder[O].encode(tuple._15) ++ SqlFragment.comma ++ JdbcEncoder[P].encode(
        tuple._16
      ) ++ SqlFragment.comma ++ JdbcEncoder[Q].encode(tuple._17) ++ SqlFragment.comma ++ JdbcEncoder[R].encode(
        tuple._18
      )

  implicit def tuple19Encoder[
    A: JdbcEncoder,
    B: JdbcEncoder,
    C: JdbcEncoder,
    D: JdbcEncoder,
    E: JdbcEncoder,
    F: JdbcEncoder,
    G: JdbcEncoder,
    H: JdbcEncoder,
    I: JdbcEncoder,
    J: JdbcEncoder,
    K: JdbcEncoder,
    L: JdbcEncoder,
    M: JdbcEncoder,
    N: JdbcEncoder,
    O: JdbcEncoder,
    P: JdbcEncoder,
    Q: JdbcEncoder,
    R: JdbcEncoder,
    S: JdbcEncoder
  ]: JdbcEncoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) ++ SqlFragment.comma ++ JdbcEncoder[B].encode(
        tuple._2
      ) ++ SqlFragment.comma ++ JdbcEncoder[C].encode(tuple._3) ++ SqlFragment.comma ++ JdbcEncoder[D].encode(
        tuple._4
      ) ++ SqlFragment.comma ++ JdbcEncoder[E].encode(tuple._5) ++ SqlFragment.comma ++ JdbcEncoder[F].encode(
        tuple._6
      ) ++ SqlFragment.comma ++ JdbcEncoder[G].encode(tuple._7) ++ SqlFragment.comma ++ JdbcEncoder[H].encode(
        tuple._8
      ) ++ SqlFragment.comma ++ JdbcEncoder[I].encode(tuple._9) ++ SqlFragment.comma ++ JdbcEncoder[J].encode(
        tuple._10
      ) ++ SqlFragment.comma ++ JdbcEncoder[K].encode(tuple._11) ++ SqlFragment.comma ++ JdbcEncoder[L].encode(
        tuple._12
      ) ++ SqlFragment.comma ++ JdbcEncoder[M].encode(tuple._13) ++ SqlFragment.comma ++ JdbcEncoder[N].encode(
        tuple._14
      ) ++ SqlFragment.comma ++ JdbcEncoder[O].encode(tuple._15) ++ SqlFragment.comma ++ JdbcEncoder[P].encode(
        tuple._16
      ) ++ SqlFragment.comma ++ JdbcEncoder[Q].encode(tuple._17) ++ SqlFragment.comma ++ JdbcEncoder[R].encode(
        tuple._18
      ) ++ SqlFragment.comma ++ JdbcEncoder[S].encode(tuple._19)

  implicit def tuple20Encoder[
    A: JdbcEncoder,
    B: JdbcEncoder,
    C: JdbcEncoder,
    D: JdbcEncoder,
    E: JdbcEncoder,
    F: JdbcEncoder,
    G: JdbcEncoder,
    H: JdbcEncoder,
    I: JdbcEncoder,
    J: JdbcEncoder,
    K: JdbcEncoder,
    L: JdbcEncoder,
    M: JdbcEncoder,
    N: JdbcEncoder,
    O: JdbcEncoder,
    P: JdbcEncoder,
    Q: JdbcEncoder,
    R: JdbcEncoder,
    S: JdbcEncoder,
    T: JdbcEncoder
  ]: JdbcEncoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) ++ SqlFragment.comma ++ JdbcEncoder[B].encode(
        tuple._2
      ) ++ SqlFragment.comma ++ JdbcEncoder[C].encode(tuple._3) ++ SqlFragment.comma ++ JdbcEncoder[D].encode(
        tuple._4
      ) ++ SqlFragment.comma ++ JdbcEncoder[E].encode(tuple._5) ++ SqlFragment.comma ++ JdbcEncoder[F].encode(
        tuple._6
      ) ++ SqlFragment.comma ++ JdbcEncoder[G].encode(tuple._7) ++ SqlFragment.comma ++ JdbcEncoder[H].encode(
        tuple._8
      ) ++ SqlFragment.comma ++ JdbcEncoder[I].encode(tuple._9) ++ SqlFragment.comma ++ JdbcEncoder[J].encode(
        tuple._10
      ) ++ SqlFragment.comma ++ JdbcEncoder[K].encode(tuple._11) ++ SqlFragment.comma ++ JdbcEncoder[L].encode(
        tuple._12
      ) ++ SqlFragment.comma ++ JdbcEncoder[M].encode(tuple._13) ++ SqlFragment.comma ++ JdbcEncoder[N].encode(
        tuple._14
      ) ++ SqlFragment.comma ++ JdbcEncoder[O].encode(tuple._15) ++ SqlFragment.comma ++ JdbcEncoder[P].encode(
        tuple._16
      ) ++ SqlFragment.comma ++ JdbcEncoder[Q].encode(tuple._17) ++ SqlFragment.comma ++ JdbcEncoder[R].encode(
        tuple._18
      ) ++ SqlFragment.comma ++ JdbcEncoder[S].encode(tuple._19) ++ SqlFragment.comma ++ JdbcEncoder[T].encode(
        tuple._20
      )

  implicit def tuple21Encoder[
    A: JdbcEncoder,
    B: JdbcEncoder,
    C: JdbcEncoder,
    D: JdbcEncoder,
    E: JdbcEncoder,
    F: JdbcEncoder,
    G: JdbcEncoder,
    H: JdbcEncoder,
    I: JdbcEncoder,
    J: JdbcEncoder,
    K: JdbcEncoder,
    L: JdbcEncoder,
    M: JdbcEncoder,
    N: JdbcEncoder,
    O: JdbcEncoder,
    P: JdbcEncoder,
    Q: JdbcEncoder,
    R: JdbcEncoder,
    S: JdbcEncoder,
    T: JdbcEncoder,
    U: JdbcEncoder
  ]: JdbcEncoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) ++ SqlFragment.comma ++ JdbcEncoder[B].encode(
        tuple._2
      ) ++ SqlFragment.comma ++ JdbcEncoder[C].encode(tuple._3) ++ SqlFragment.comma ++ JdbcEncoder[D].encode(
        tuple._4
      ) ++ SqlFragment.comma ++ JdbcEncoder[E].encode(tuple._5) ++ SqlFragment.comma ++ JdbcEncoder[F].encode(
        tuple._6
      ) ++ SqlFragment.comma ++ JdbcEncoder[G].encode(tuple._7) ++ SqlFragment.comma ++ JdbcEncoder[H].encode(
        tuple._8
      ) ++ SqlFragment.comma ++ JdbcEncoder[I].encode(tuple._9) ++ SqlFragment.comma ++ JdbcEncoder[J].encode(
        tuple._10
      ) ++ SqlFragment.comma ++ JdbcEncoder[K].encode(tuple._11) ++ SqlFragment.comma ++ JdbcEncoder[L].encode(
        tuple._12
      ) ++ SqlFragment.comma ++ JdbcEncoder[M].encode(tuple._13) ++ SqlFragment.comma ++ JdbcEncoder[N].encode(
        tuple._14
      ) ++ SqlFragment.comma ++ JdbcEncoder[O].encode(tuple._15) ++ SqlFragment.comma ++ JdbcEncoder[P].encode(
        tuple._16
      ) ++ SqlFragment.comma ++ JdbcEncoder[Q].encode(tuple._17) ++ SqlFragment.comma ++ JdbcEncoder[R].encode(
        tuple._18
      ) ++ SqlFragment.comma ++ JdbcEncoder[S].encode(tuple._19) ++ SqlFragment.comma ++ JdbcEncoder[T].encode(
        tuple._20
      ) ++ SqlFragment.comma ++ JdbcEncoder[U].encode(tuple._21)

  implicit def tuple22Encoder[
    A: JdbcEncoder,
    B: JdbcEncoder,
    C: JdbcEncoder,
    D: JdbcEncoder,
    E: JdbcEncoder,
    F: JdbcEncoder,
    G: JdbcEncoder,
    H: JdbcEncoder,
    I: JdbcEncoder,
    J: JdbcEncoder,
    K: JdbcEncoder,
    L: JdbcEncoder,
    M: JdbcEncoder,
    N: JdbcEncoder,
    O: JdbcEncoder,
    P: JdbcEncoder,
    Q: JdbcEncoder,
    R: JdbcEncoder,
    S: JdbcEncoder,
    T: JdbcEncoder,
    U: JdbcEncoder,
    V: JdbcEncoder
  ]: JdbcEncoder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
    tuple =>
      JdbcEncoder[A].encode(tuple._1) ++ SqlFragment.comma ++ JdbcEncoder[B].encode(
        tuple._2
      ) ++ SqlFragment.comma ++ JdbcEncoder[C].encode(tuple._3) ++ SqlFragment.comma ++ JdbcEncoder[D].encode(
        tuple._4
      ) ++ SqlFragment.comma ++ JdbcEncoder[E].encode(tuple._5) ++ SqlFragment.comma ++ JdbcEncoder[F].encode(
        tuple._6
      ) ++ SqlFragment.comma ++ JdbcEncoder[G].encode(tuple._7) ++ SqlFragment.comma ++ JdbcEncoder[H].encode(
        tuple._8
      ) ++ SqlFragment.comma ++ JdbcEncoder[I].encode(tuple._9) ++ SqlFragment.comma ++ JdbcEncoder[J].encode(
        tuple._10
      ) ++ SqlFragment.comma ++ JdbcEncoder[K].encode(tuple._11) ++ SqlFragment.comma ++ JdbcEncoder[L].encode(
        tuple._12
      ) ++ SqlFragment.comma ++ JdbcEncoder[M].encode(tuple._13) ++ SqlFragment.comma ++ JdbcEncoder[N].encode(
        tuple._14
      ) ++ SqlFragment.comma ++ JdbcEncoder[O].encode(tuple._15) ++ SqlFragment.comma ++ JdbcEncoder[P].encode(
        tuple._16
      ) ++ SqlFragment.comma ++ JdbcEncoder[Q].encode(tuple._17) ++ SqlFragment.comma ++ JdbcEncoder[R].encode(
        tuple._18
      ) ++ SqlFragment.comma ++ JdbcEncoder[S].encode(tuple._19) ++ SqlFragment.comma ++ JdbcEncoder[T].encode(
        tuple._20
      ) ++ SqlFragment.comma ++ JdbcEncoder[U].encode(tuple._21) ++ SqlFragment.comma ++ JdbcEncoder[V].encode(
        tuple._22
      )
}

trait JdbcEncoder0LowPriorityImplicits { self =>
  private[jdbc] def primitiveCodec[A](standardType: StandardType[A]): JdbcEncoder[A] =
    standardType match {
      case StandardType.StringType     => JdbcEncoder.stringEncoder
      case StandardType.BoolType       => JdbcEncoder.booleanEncoder
      case StandardType.ShortType      => JdbcEncoder.shortEncoder
      case StandardType.IntType        => JdbcEncoder.intEncoder
      case StandardType.LongType       => JdbcEncoder.longEncoder
      case StandardType.FloatType      => JdbcEncoder.floatEncoder
      case StandardType.DoubleType     => JdbcEncoder.doubleEncoder
      case StandardType.CharType       => JdbcEncoder.charEncoder
      case StandardType.BigIntegerType => JdbcEncoder.bigIntEncoder
      case StandardType.BinaryType     => JdbcEncoder.byteChunkEncoder
      case StandardType.BigDecimalType => JdbcEncoder.bigDecimalEncoder
      case StandardType.UUIDType       => JdbcEncoder.uuidEncoder
      // TODO: Standard Types which are missing are the date time types, not sure what would be the best way to handle them
      case _                           => throw JdbcEncoderError(s"Unsupported type: $standardType", new IllegalArgumentException)
    }

  import zio.schema.Deriver
//  private sealed trait JdbcEncoderDeriver[F[_]] extends Deriver[F[_]] {
//    def deriveUnknown[A: ClassTag](summoned: => Option[F[A]]): F[A] = {
//      val classTag = implicitly[ClassTag[A]]
//      println(s"===== arrived at deriveUnknown with classTag: $classTag =====")
//      if (classTag == scala.reflect.classTag[zio.json.ast.Json]) {
//        new JdbcEncoder[zio.json.ast.Json] {
//          override def encode(value: zio.json.ast.Json): SqlFragment =
//            sql"""'${value.toString}'"""
//        }.asInstanceOf[F[A]]
//      } else super.deriveUnknown(summoned)
//    }
//  }
  val deriver = new Deriver[JdbcEncoder] {
    // TODO: replace sql"""""" with SqlFragment.nullLiteral
    // ", " with SqlFragment.comma and append with ++
    def deriveRecord[A](
      record: Schema.Record[A],
      fields: => Chunk[Deriver.WrappedF[JdbcEncoder, _]],
      summoned: => Option[JdbcEncoder[A]]
    ): JdbcEncoder[A] = (value: A) => {

      record.fields.zip(fields).foldLeft[SqlFragment](sql"""""""") {
        case (acc, (Schema.Field(_, _, _, _, get, _), field)) =>
          println("==== Arrived at deriveRecord ====")
          acc ++ (if (acc == sql"""""""") sql"""""""" else sql""", """) ++
            sql"""${
              field.unwrap
                .asInstanceOf[JdbcEncoder[Any]]
                .encode(get(value))
            }"""
      }
    }

    def deriveTransformedRecord[A, B](
      record: Schema.Record[A],
      transform: Schema.Transform[A, B, _],
      fields: => Chunk[Deriver.WrappedF[JdbcEncoder, _]],
      summoned: => Option[JdbcEncoder[B]]
    ): JdbcEncoder[B] = (value: B) =>
      record.fields.zip(fields).foldLeft[SqlFragment](sql"""""""") {
        case (acc, (Schema.Field(_, _, _, _, get, _), field)) =>
          transform.g(value) match {
            case Right(value) =>
              acc ++ (if (acc == sql"""""""") sql"""""""" else sql""", """) ++ sql"""${field.unwrap
                .asInstanceOf[JdbcEncoder[Any]]
                .encode(get(value))}"""
            case Left(_)      => SqlFragment.nullLiteral
          }
      }

    def deriveEnum[A](
      `enum`: Schema.Enum[A],
      cases: => Chunk[Deriver.WrappedF[JdbcEncoder, _]],
      summoned: => Option[JdbcEncoder[A]]
    ): JdbcEncoder[A] = (value: A) =>
      `enum`.cases.zip(cases).foldLeft[SqlFragment](sql""""""") {
        case (acc, (enumCase @ Schema.Case(_, _, unsafeDeconstruct, _, isCase, _), c)) =>
          if (isCase(value))
            acc ++ (if (acc == sql"""""""") "" else sql""", """) ++ sql"""${c.unwrap
              .asInstanceOf[JdbcEncoder[Any]]
              .encode(unsafeDeconstruct(value))}"""
          else
            acc
      }

    def derivePrimitive[A](
      st: StandardType[A],
      summoned: => Option[JdbcEncoder[A]]
    ): JdbcEncoder[A] = (value: A) => primitiveCodec(st).encode(value)

    def deriveOption[A](
      option: Schema.Optional[A],
      inner: => JdbcEncoder[A],
      summoned: => Option[JdbcEncoder[Option[A]]]
    ): JdbcEncoder[Option[A]] = {
      case None        => sql"""""""
      case Some(value) =>
        inner.encode(value)
    }

    def deriveSequence[C[_], A](
      sequence: Schema.Sequence[C[A], A, _],
      inner: => JdbcEncoder[A],
      summoned: => Option[JdbcEncoder[C[A]]]
    ): JdbcEncoder[C[A]] = (values: C[A]) =>
      sequence.toChunk(values).foldLeft(sql""""""") { case (acc, value) =>
        acc ++ (if (acc == sql"""""""") sql"""""""" else sql""", """) ++ sql"""${inner.encode(value)}"""
      }

    def deriveMap[K, V](
      map: Schema.Map[K, V],
      key: => JdbcEncoder[K],
      value: => JdbcEncoder[V],
      summoned: => Option[JdbcEncoder[Map[K, V]]]
    ): JdbcEncoder[Map[K, V]] = (values: Map[K, V]) =>
      values.foldLeft(SqlFragment.nullLiteral) { case (acc, (k, v)) =>
        acc ++ (if (acc == SqlFragment.nullLiteral) SqlFragment.nullLiteral else SqlFragment.comma) ++ sql"""${key
          .encode(k)}: ${value.encode(v)}"""
      }

    override def deriveUnknown[A: ClassTag](
      summoned: => Option[JdbcEncoder[A]]
    ): JdbcEncoder[A] = {
      val classTag = implicitly[ClassTag[A]]
      println(s"===== arrived at deriveUnknown with classTag: $classTag =====")
      if (classTag == scala.reflect.classTag[zio.json.ast.Json]) {
        new JdbcEncoder[zio.json.ast.Json] {
          override def encode(value: zio.json.ast.Json): SqlFragment =
            sql"""'${value.toString}'"""
        }.asInstanceOf[JdbcEncoder[Any]]
      } else super.deriveUnknown(summoned)
    }
  }.autoAcceptSummoned

  //scalafmt: { maxColumn = 325, optIn.configStyleArguments = false }
  def fromSchema[A: ClassTag](implicit schema: Schema[A]): JdbcEncoder[A] = {
    schema match {
      case Schema.Primitive(standardType, _) =>
        primitiveCodec(standardType)
      case schemaOpt: Schema.Optional[a]        =>
        implicit val optSchema: Schema[a] = schemaOpt.schema
        implicit val ct: ClassTag[a] = ClassTag[a](classOf[Any])
        JdbcEncoder.optionEncoder(self.fromSchema[a])
      case schemaTuple: Schema.Tuple2[a, b]     =>
        implicit val leftSchema: Schema[a] = schemaTuple.left
        implicit val cta: ClassTag[a] = ClassTag[a](classOf[Any])
        implicit val rightSchema: Schema[b] = schemaTuple.right
        implicit val ctb: ClassTag[b] = ClassTag[b](classOf[Any])
        JdbcEncoder.tuple2Encoder(self.fromSchema[a], self.fromSchema[b])
      // format: off
      case x@(
        _: Schema.CaseClass1[_, _] |
        _: Schema.CaseClass2[_, _, _] |
        _: Schema.CaseClass3[_, _, _, _] |
        _: Schema.CaseClass4[_, _, _, _, _] |
        _: Schema.CaseClass5[_, _, _, _, _, _] |
        _: Schema.CaseClass6[_, _, _, _, _, _, _] |
        _: Schema.CaseClass7[_, _, _, _, _, _, _, _] |
        _: Schema.CaseClass8[_, _, _, _, _, _, _, _, _] |
        _: Schema.CaseClass9[_, _, _, _, _, _, _, _, _, _] |
        _: Schema.CaseClass10[_, _, _, _, _, _, _, _, _, _, _] |
        _: Schema.CaseClass11[_, _, _, _, _, _, _, _, _, _, _, _] |
        _: Schema.CaseClass12[_, _, _, _, _, _, _, _, _, _, _, _, _] |
        _: Schema.CaseClass13[_, _, _, _, _, _, _, _, _, _, _, _, _, _] |
        _: Schema.CaseClass14[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _] |
        _: Schema.CaseClass15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] |
        _: Schema.CaseClass16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] |
        _: Schema.CaseClass17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] |
        _: Schema.CaseClass18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] |
        _: Schema.CaseClass19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] |
        _: Schema.CaseClass20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] |
        _: Schema.CaseClass21[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] |
        _: Schema.CaseClass22[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]
        ) =>
        // format: on
        caseClassEncoder(x.asInstanceOf[Schema.Record[A]].fields)
      case _                                 =>
        val classTag = implicitly[ClassTag[A]].runtimeClass
        println(s"@@@@ classTag: $classTag")
        if (classTag == classOf[zio.json.ast.Json])
          jsonEncoder().asInstanceOf[JdbcEncoder[A]]
        else
          throw JdbcEncoderError(s"Failed to encode schema ${schema}", new IllegalArgumentException)
    }
  }

  // private[jdbc] def caseClassEncoder[A: ClassTag](fields: Chunk[Schema.Field[A, _]])(implicit schema: Schema[A]): JdbcEncoder[A] = { (a: A) =>
  //   fields.map { f =>
  //     val encoder = self.fromSchema[f.FieldType](f.schema)
  //     encoder.encode(f.get(a))
  //   }.reduce(_ ++ SqlFragment.comma ++ _)
  // }
  private[jdbc] def caseClassEncoder[A: ClassTag](fields: Chunk[Schema.Field[A, _]])(implicit schema: Schema[A]): JdbcEncoder[A] = { (a: A) =>
    fields.map { field =>
      val fieldSchema = field.schema.asInstanceOf[Schema[Any]] // Adjust type as necessary
      encodeField[A, Any](field.asInstanceOf[Schema.Field[A, Any]], a)(implicitly[ClassTag[Any]], fieldSchema)
    }.reduce(_ ++ SqlFragment.comma ++ _)
  }

  private[jdbc] def encodeField[A, B : ClassTag](field: Schema.Field[A, B], value: A)(implicit schema: Schema[B]): SqlFragment = {
    val encoder = self.fromSchema[B]
    encoder.encode(field.get(value))
  }

//  private[jdbc] def jsonTransformEncoder[A, B](in: Schema[DynamicValue])(implicit toJson: B => Either[String, B]): JdbcEncoder[B] = {
//    println("@@@@ did we get into jsonTransformEncoder?")
//    (b : B) =>
//      val encoder: JdbcEncoder[A] = in match {
//        case enum: Schema.Enum[_] => throw JdbcEncoderError(s"Failed to encode schema ${enum}", new IllegalArgumentException)
//        case record: Schema.Record[_] => throw JdbcEncoderError(s"Failed to encode schema ${record}", new IllegalArgumentException)
//        case collection: Schema.Collection[_, _] => throw JdbcEncoderError(s"Failed to encode schema ${collection}", new IllegalArgumentException)
//        case Schema.Transform(schema, f, g, annotations, identity) => throw JdbcEncoderError(s"Failed to encode schema ${schema}", new IllegalArgumentException)
//        case primitive@Schema.Primitive(standardType, annotations) => throw JdbcEncoderError(s"Failed to encode schema ${primitive}", new IllegalArgumentException)
////        case optional@Schema.Optional(schema, annotations) => throw JdbcEncoderError(s"Failed to encode schema ${optional}", new IllegalArgumentException)
//        case fail@Schema.Fail(message, annotations) => throw JdbcEncoderError(s"Failed to encode schema ${fail}", new IllegalArgumentException)
////        case Schema.Tuple2(left, right, annotations) => ???
////        case Schema.Either(left, right, annotations) => ???
////        case Schema.Fallback(left, right, fullDecode, annotations) => ???
//        case l@Schema.Lazy(schema0) =>  throw JdbcEncoderError(s"Failed to encode schema ${l}", new IllegalArgumentException)// self.fromSchema(schema0.asInstanceOf[Schema[Any]])
//        case Schema.Dynamic(annotations) => self.fromSchema(in.asInstanceOf[Schema[Any]])
//      }
//      toJson(b) match {
//        case Right(b) => encoder.encode(b) //sql"$a::json"
//        case Left(error) => throw JdbcEncoderError(s"[jsonEncoder] Failed to convert $a to json: $error", new IllegalArgumentException)
//      }
//  }
  private[jdbc] def jsonEncoder(): JdbcEncoder[zio.json.ast.Json] =
    (value: zio.json.ast.Json) => sql"${value.toString}::json"
}
