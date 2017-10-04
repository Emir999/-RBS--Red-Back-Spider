package db.migration.sqlite

import java.sql.Connection

import org.flywaydb.core.api.migration.jdbc.JdbcMigration
import scalikejdbc._

class V1_1__AutoVacuum extends JdbcMigration {
  override def migrate(connection: Connection): Unit = {
    connection.rollback()
    connection.setAutoCommit(true)

    using(connection.createStatement()) { stm =>
      stm.execute("pragma auto_vacuum=FULL")
    }

    using(connection.createStatement()) { stm =>
      stm.execute("vacuum")
    }

    connection.setAutoCommit(false)
  }
}
