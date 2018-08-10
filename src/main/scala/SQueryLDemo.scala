import org.squeryl.adapters.DB2Adapter  
import org.squeryl.{Session, SessionFactory} 
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.annotations.Column
import org.squeryl.Schema

class TmPrmObject(val id: Int,
                  var org: String,
                  @Column("PARAM_CLASS")
                  var paramClass: String,
                  @Column("PARAM_KEY")
                  var paramKey: String,
                  @Column("PARAM_OBJECT")
                  var paramObject: String,
                  @Column("JPA_VERSION")
                  var jpaVersion: Option[Int],
                  @Column("MTN_TIMESTAMP")
                  var mtnTimestamp: Option[String],
                  @Column("MTN_USER")
                  var mtnUser: Option[String])
object BmpSchema extends Schema {
    val tmPrmObjects = table[TmPrmObject]("TM_PRM_OBJECT")
}

object SQUERYL {
    def main(args: Array[String]): Unit = {
        Class.forName("com.ibm.db2.jcc.DB2Driver")
        SessionFactory.concreteFactory = Some(() =>
            Session.create(java.sql.DriverManager.getConnection("jdbc:db2://localhost:50001/aicdb", "bmp", "bmp123"), new DB2Adapter))

        inTransaction {
            val pos = from(BmpSchema.tmPrmObjects) {po => where(po.org === "000000000003") select(po)}
            for (po <- pos)
                println("CLASS=" + po.paramClass + ", KEY=" + po.paramKey + ", OBJECT=" + po.paramObject)
        }
        inTransaction {
            val clzzs = from(BmpSchema.tmPrmObjects) {po => where(po.org === "000000000003") select(&(po.paramClass))} .distinct
            for (clz <- clzzs)
                println("CLASS=" + clz)
        }
    }
}

