package com.allinfinance.tools.param.util

import java.sql.{Connection, DriverManager, ResultSet, Statement}
import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.xml.DomDriver
import java.lang.reflect._
import java.util.jar._
import java.util.zip._

class JdbcConnection(val jdbcUrl: String, val jdbcUserName: String, val jdbcPassword: String) {
    val conn = {
        Class.forName("com.ibm.db2.jcc.DB2Driver")
        DriverManager.getConnection(jdbcUrl, jdbcUserName, jdbcPassword)
    }

    def withConnection[T](f: java.sql.Connection => T): Either[String, T] = {
        try {
            Right(f(conn))
        }
        catch {
            case e: Throwable =>
                import java.io.{PrintWriter, StringWriter}
                Console.out.println(e)
                val sw = new StringWriter
                e.printStackTrace(new PrintWriter(sw))
                Left(sw.toString)
        }
    }
}

object JdbcConnection {
    def apply(jdbcUrl: String, jdbcUserName: String, jdbcPassword: String) = new JdbcConnection(jdbcUrl, jdbcUserName, jdbcPassword)
}

class PrmObject(var id: Int, var org: String, val paramClass: Class[_], var paramKey: String, var paramObject: AnyRef, var jpaVersion: Int, var mtnTimeStamp: String, var mtnUser: String) {
    def this(org: String, paramClass: Class[_], paramKey: String) = {
        this(0, org, paramClass, paramKey, null, 0, null, null)
    }
    def copyFrom(other: PrmObject) {
        id = other.id
        paramObject = other.paramObject
        jpaVersion = other.jpaVersion
        mtnTimeStamp = other.mtnTimeStamp
        mtnUser = other.mtnUser
    }
    override def toString = "id[" + id + "] " + 
                            "org[" + org + "] " +
                            "class[" + paramClass + "] " +
                            "key[" + paramKey + "] "
}
object PrmObject {
    def getOrgs(conn: java.sql.Connection): List[String] = {
        // Configure to be Read Only
        val statement = conn.createStatement(ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)

        // Execute Query
        val rs = statement.executeQuery("select distinct ORG from BMP.TM_PRM_OBJECT order by ORG")

        var rows = List[String]()
        // Iterate Over ResultSet
        while (rs.next) {
            try {
                val org = rs.getString("ORG")
                rows = org :: rows
            }
            catch {
                case e: Throwable =>
                    Console.out.println(e)
            }
        }
        rs.close
        statement.close
        rows.reverse
    }

    def getClasses(conn: java.sql.Connection, org: String, classLoader: ClassLoader = null): List[Class[_]] = {
        // Configure to be Read Only
        val statement = conn.createStatement(ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)

        // Execute Query
        val rs = statement.executeQuery("select distinct PARAM_CLASS from BMP.TM_PRM_OBJECT where ORG='" + org + "' order by PARAM_CLASS")

        val cl = if (classLoader == null) this.getClass.getClassLoader else classLoader
        var rows = List[Class[_]]()
        // Iterate Over ResultSet
        while (rs.next) {
            try {
                val clazz = Class.forName(rs.getString("PARAM_CLASS"), true, cl)
                rows = clazz :: rows
            }
            catch {
                case e: Throwable =>
                    Console.out.println("load class[" + rs.getString("PARAM_CLASS") + "] failed")
                    Console.out.println(e)
            }
        }
        rs.close
        statement.close
        rows.reverse
    }

    def getObjKeys(conn: java.sql.Connection, org: String, clazz: Class[_]): List[String] = {
        // Configure to be Read Only
        val statement = conn.createStatement(ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)

        // Execute Query
        val rs = statement.executeQuery("select PARAM_KEY from BMP.TM_PRM_OBJECT where ORG='" + org + "' and PARAM_CLASS='" + clazz.getName + "' order by PARAM_KEY")

        var rows = List[String]()
        // Iterate Over ResultSet
        while (rs.next) {
            try {
                val key = rs.getString("PARAM_KEY")
                rows = key :: rows
            }
            catch {
                case e: Throwable =>
                    Console.out.println(e)
            }
        }
        rs.close
        statement.close
        rows.reverse
    }

    def getObj(conn: java.sql.Connection, org: String, clazz: Class[_], key: String): PrmObject = {
        // Configure to be Read Only
        val statement = conn.createStatement(ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)

        // Execute Query
        val rs = statement.executeQuery("select ID, PARAM_KEY, PARAM_OBJECT, JPA_VERSION, MTN_TIMESTAMP, MTN_USER from BMP.TM_PRM_OBJECT where ORG='" + org + "' and PARAM_CLASS='" + clazz.getName + "' and PARAM_KEY='" + key + "'")

        val xstream = new XStream(new DomDriver)
        xstream.setClassLoader(clazz.getClassLoader)
        // Iterate Over ResultSet
        if (rs.next) {
            val id = rs.getString("ID").toInt
            val key = rs.getString("PARAM_KEY")
            val obj =
                try {
                    xstream.fromXML(rs.getString("PARAM_OBJECT"))
                }
                catch {
                    case e: Throwable =>
                        Console.out.println(e)
//                    e.printStackTrace
                        clazz.newInstance.asInstanceOf[AnyRef]
                }
            val jpaVersion = rs.getString("JPA_VERSION").toInt
            val mtnTimestamp = rs.getString("MTN_TIMESTAMP")
            val mtnUser = rs.getString("MTN_USER")
            rs.close
            statement.close
            new PrmObject(id, org, clazz, key, obj, jpaVersion, mtnTimestamp, mtnUser)
        }
        else {
            rs.close
            statement.close
            throw new IllegalArgumentException("select obj failed")
        }
    }

    def getObjs(conn: java.sql.Connection, clazz: Class[_], where: String): List[PrmObject] = {
        // Configure to be Read Only
        val statement = conn.createStatement(ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)

        // Execute Query
        val sql = "select ID, ORG, PARAM_KEY, PARAM_OBJECT, JPA_VERSION, MTN_TIMESTAMP, MTN_USER from BMP.TM_PRM_OBJECT where PARAM_CLASS='" + clazz.getName + "' and (" + where + ")"
        val rs = statement.executeQuery(sql)

        val xstream = new XStream(new DomDriver)
        xstream.setClassLoader(clazz.getClassLoader)
        var rows = List[PrmObject]()
        // Iterate Over ResultSet
        while (rs.next) {
            try {
                val id = rs.getString("ID").toInt
                val org = rs.getString("ORG")
                val key = rs.getString("PARAM_KEY")
                val obj =
                    try {
                        xstream.fromXML(rs.getString("PARAM_OBJECT"))
                    }
                    catch {
                        case e: Throwable =>
                            Console.out.println(e)
//                            e.printStackTrace
                            clazz.newInstance.asInstanceOf[AnyRef]
                    }
                val jpaVersion = rs.getString("JPA_VERSION").toInt
                val mtnTimestamp = rs.getString("MTN_TIMESTAMP")
                val mtnUser = rs.getString("MTN_USER")
                val prmObject = new PrmObject(id, org, clazz, key, obj, jpaVersion, mtnTimestamp, mtnUser)
                rows = prmObject :: rows
            }
            catch {
                case e: Throwable =>
                    Console.out.println(e)
            }
        }
        rs.close
        statement.close
        rows.reverse
    }

    def insert(conn: java.sql.Connection, prmObject: PrmObject): Unit = {
        val xstream = new XStream(new DomDriver)
        xstream.setClassLoader(prmObject.paramClass.getClassLoader)
        // Configure to be Read Only
        var sql = "insert into BMP.TM_PRM_OBJECT (ORG, PARAM_CLASS, PARAM_KEY, PARAM_OBJECT, JPA_VERSION"
        var questionMarks = "?, ?, ?, ?, ?"
        if (prmObject.mtnTimeStamp != null) {
            sql += ", MTN_TIMESTAMP"
            questionMarks += ", ?"
        }
        if (prmObject.mtnUser != null) {
            sql += ", MTN_USER"
            questionMarks += ", ?"
        }
        sql = sql + ") values (" + questionMarks + ")"
        val statement = conn.prepareStatement(sql, Statement.RETURN_GENERATED_KEYS)
        statement.setString(1, prmObject.org)
        statement.setString(2, prmObject.paramClass.getName)
        statement.setString(3, prmObject.paramKey)
        statement.setString(4, xstream.toXML(prmObject.paramObject))
        statement.setInt(5, prmObject.jpaVersion)
        var index = 6
        if (prmObject.mtnTimeStamp != null) {
            statement.setString(index, prmObject.mtnTimeStamp)
            index += 1
        }
        if (prmObject.mtnUser != null) {
            statement.setString(index, prmObject.mtnUser)
            index += 1
        }

Console.out.println("insert(" + prmObject.org + "," + prmObject.paramClass.getName + "," + prmObject.paramKey + "," + xstream.toXML(prmObject.paramObject) + "," + prmObject.jpaVersion);
        // Execute Insert
        statement.executeUpdate
        val keys = statement.getGeneratedKeys
        if (keys.next) {
            prmObject.id = keys.getInt(1)
        }
        statement.close
    }
    def insert(conn: java.sql.Connection, prmObjects: Seq[PrmObject]): Unit = {
        val xstream = new XStream(new DomDriver)
        xstream.setClassLoader(prmObjects(0).paramClass.getClassLoader)
        // Configure to be Read Only
        var sql = "insert into BMP.TM_PRM_OBJECT (ORG, PARAM_CLASS, PARAM_KEY, PARAM_OBJECT, JPA_VERSION"
        var questionMarks = "?, ?, ?, ?, ?"
        sql = sql + ") values (" + questionMarks + ")"
        val statement = conn.prepareStatement(sql, Statement.RETURN_GENERATED_KEYS)
        for (prmObject <- prmObjects) {
            Console.out.println("insert ORG(" + prmObject.org + "), PARAM_KEY(" + prmObject.paramKey + ")")
            statement.setString(1, prmObject.org)
            statement.setString(2, prmObject.paramClass.getName)
            statement.setString(3, prmObject.paramKey)
            statement.setString(4, xstream.toXML(prmObject.paramObject))
            statement.setInt(5, prmObject.jpaVersion)
Console.out.println("insert(" + prmObject.org + "," + prmObject.paramClass.getName + "," + prmObject.paramKey + "," + xstream.toXML(prmObject.paramObject) + "," + prmObject.jpaVersion);

            // Execute Insert
            statement.executeUpdate
        }
        statement.close
    }

    def update(conn: java.sql.Connection, prmObject: PrmObject): Unit = {
        val xstream = new XStream(new DomDriver)
        xstream.setClassLoader(prmObject.paramClass.getClassLoader)
        // Configure to be Read Only
        val statement = conn.prepareStatement("update BMP.TM_PRM_OBJECT set PARAM_OBJECT=? where ORG=? and PARAM_CLASS=? and PARAM_KEY=?")
        statement.setString(1, xstream.toXML(prmObject.paramObject))
        statement.setString(2, prmObject.org)
        statement.setString(3, prmObject.paramClass.getName)
        statement.setString(4, prmObject.paramKey)

        // Execute Insert
        statement.executeUpdate
        statement.close
    }
    def update(conn: java.sql.Connection, prmObjects: Seq[PrmObject]): Unit = {
        val xstream = new XStream(new DomDriver)
        xstream.setClassLoader(prmObjects(0).paramClass.getClassLoader)
        // Configure to be Read Only
        val statement = conn.prepareStatement("update BMP.TM_PRM_OBJECT set PARAM_OBJECT=? where ORG=? and PARAM_CLASS=? and PARAM_KEY=?")
        for (prmObject <- prmObjects) {
            Console.out.println("update ORG(" + prmObject.org + "), PARAM_KEY(" + prmObject.paramKey + ")")
            statement.setString(1, xstream.toXML(prmObject.paramObject))
            statement.setString(2, prmObject.org)
            statement.setString(3, prmObject.paramClass.getName)
            statement.setString(4, prmObject.paramKey)

            // Execute Insert
            statement.executeUpdate
        }
        statement.close
    }

    def delete(conn: java.sql.Connection, prmObject: PrmObject): Unit = {
        val statement = conn.prepareStatement("delete from BMP.TM_PRM_OBJECT where ORG=? and PARAM_CLASS=? and PARAM_KEY=?")
        statement.setString(1, prmObject.org)
        statement.setString(2, prmObject.paramClass.getName)
        statement.setString(3, prmObject.paramKey)

        // Execute delete
        statement.executeUpdate
        statement.close
    }

}

