package com.dwolla.sqlserver.init

import io.circe._
import io.circe.generic.semiauto._
import io.circe.refined._
import shapeless.syntax.std.product._


case class DatabaseMetadata(host: Host,
                            port: Port,
                            name: Database,
                            username: MasterDatabaseUsername,
                            password: MasterDatabasePassword,
                            secretIds: List[SecretId],
                            trustServerCert: Boolean,
                           )

object DatabaseMetadata {
  implicit val DecodeDatabaseMetadata: Decoder[DatabaseMetadata] =
    Decoder.forProduct7("Host",
      "Port",
      "DatabaseName",
      "MasterDatabaseUsername",
      "MasterDatabasePassword",
      "UserConnectionSecrets",
      "TrustServerCert")(DatabaseMetadata.apply)

  implicit val EncodeDatabaseMetadata: Encoder[DatabaseMetadata] =
    Encoder.forProduct7("Host",
      "Port",
      "DatabaseName",
      "MasterDatabaseUsername",
      "MasterDatabasePassword",
      "UserConnectionSecrets",
      "TrustServerCert")(_.toTuple)
}

case class UserConnectionInfo(database: Database,
                              host: Host,
                              port: Port,
                              user: Username,
                              password: Password,
                             )

object UserConnectionInfo {
  implicit val UserConnectionInfoCodec: Codec[UserConnectionInfo] = deriveCodec[UserConnectionInfo]
}
