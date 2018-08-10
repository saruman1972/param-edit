package com.allinfinance.tools.param.util

import scala.collection.JavaConversions._
import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.xml.DomDriver
import java.io.{StringWriter, PrintWriter}

object Stmt {
    import JsonDef._
    import Expr._
    import ObjDef._
    import ObjTreeNode.{VerNode, VerConfig, VerConfigs, OrgNode}

    def getFilteredObjs(vc: VersionClass, where: Where): Either[String, List[PrmObject]] = {
        vc.verNode.conn.withConnection {c => PrmObject.getObjs(c, vc.clazz.get, where.sqlWhere.get)} match {
            case Right(objs) =>
                Right(objs.filter {o => where.satisfied(o.paramObject)})
            case left @ Left(_) => left
        }
    }
    
    case class Where(val exp: RelExp, val having: Option[RelExp])
 {
        lazy val sqlWhere: Option[String] =
            exp match {
                case ae @ AndExp(left, right) if (isSimpleExp(left) && isSimpleExp(right)) =>
                    Some(ae.toString)
                case e if isSimpleExp(e) => Some(e.toString)
                case _ => None
            }
        def isSimpleExp(e: RelExp): Boolean = e match {
            case BasicExp(_, field, _) if (field.isSqlColumn) => true
            case InExp(field, _) if (field.isSqlColumn) => true
            case NotExp(InExp(field, _)) if (field.isSqlColumn) => true
            case _ => false
        }
        def typeChecked(vc: VersionClass): Unit = {
            if (sqlWhere == None) throw new IllegalArgumentException("where should only contain ORG and PARAM_KEY")
            having.map {_.typeChecked(vc)}
        }
        def satisfied(obj: AnyRef): Boolean = having match {
            case None => true
            case Some(e) => e.satisfied(obj)
        }
    }
    sealed trait Statement {
        def resolveType: Unit = {}
        protected def doEval: String

        def eval: String = {
            try {
                doEval
            }
            catch {
                case e: Throwable =>
                    val sw = new StringWriter
                    e.printStackTrace(new PrintWriter(sw))
                    sw.toString
            }
        }
    }
    case class ExitStmt() extends Statement {
        override def doEval: String = ""
    }
    var debugON: Boolean = false
    val xstream = new XStream(new DomDriver)
    case class XmlToJsonStmt(vc: VersionClass, val xml: String) extends Statement {
        override def doEval: String = {
            vc.verNode.ensureConnected
            val xstream = new XStream(new DomDriver)
            xstream.setClassLoader(vc.clazz.get.getClassLoader)
            try {
                val obj = xstream.fromXML(xml)
println(obj)
println(obj.getClass)
println(ObjType(obj.getClass))
                println(ObjType(obj.getClass).toJson(obj))
            }
            catch {
                case e: Throwable => e.printStackTrace
            }
            ""
        }
    }
    case class ListOrgsStmt(val version: String) extends Statement
{
        val verNode: Option[VerNode] = VersionClass.verMap.get(version)
        override def doEval: String = verNode match {
            case Some(vn) =>
                vn.ensureConnected
                val orgs = "ORG\n------------\n" + vn.children.map {x => x.asInstanceOf[OrgNode].org} .mkString("\n")
                println(orgs)
                orgs
            case None =>
                val errmsg = "invalid version[" + version + "]."
                println(errmsg)
                errmsg
        }
    }
    case class DebugSwitchStmt(val on: Boolean) extends Statement 
{
        override def doEval: String = {
            debugON = on
            if (on) {
                println("debug is on")
            }
            else {
                println("debug is off")
            }
            ""
        }
    }
    case class VersionDefStmt(json: JsonCompound) extends Statement
 {
        var vc: VerConfig = null
        override def resolveType: Unit = {
            for (name <- json.elmt.get("name");
                 if name.isInstanceOf[JsonString];
                 jdbcUrl <- json.elmt.get("jdbcUrl");
                 if jdbcUrl.isInstanceOf[JsonString];
                 jdbcUserName <- json.elmt.get("jdbcUserName");
                 if jdbcUserName.isInstanceOf[JsonString];
                 jdbcPassword <- json.elmt.get("jdbcPassword");
                 if jdbcPassword.isInstanceOf[JsonString];
                 libPath <- json.elmt.get("libPath");
                 if libPath.isInstanceOf[JsonString]
             ) {
                vc = VerConfig(name.asInstanceOf[JsonString].elmt, 
                                   jdbcUrl.asInstanceOf[JsonString].elmt, 
                                   jdbcUserName.asInstanceOf[JsonString].elmt, 
                                   jdbcPassword.asInstanceOf[JsonString].elmt, 
                                   libPath.asInstanceOf[JsonString].elmt)
                VerConfigs.add(vc)
            }
        }

        override def doEval: String = {
            println("version [" + vc + "] defined")
            ""
        }
    }

    case class DeleteStmt(val vc: VersionClass, val where: Where) extends Statement
{
        override def resolveType: Unit = {
            where.typeChecked(vc)
        }

        override def doEval: String = {
            vc.verNode.ensureConnected
            getFilteredObjs(vc, where) match {
                case Right(objs) =>
                    objs.foreach {o =>
                        if (debugON) {
                            Console.out.println("deleting: ORG='" + o.org + "', PARAM_KEY='" + o.paramKey + "'")
                        }
                                  vc.verNode.conn.withConnection {c => PrmObject.delete(c, o)}
                              }
                    objs.size.toString + " objects deleted."
                case Left(e) => e
            }
        }
    }

    case class SelectStmt(val fields: List[Field], val vc: VersionClass, val where: Where) extends Statement 
{
        override def resolveType: Unit = {
            fields.map {f =>
                f.resolveType(vc)
            }
            where.typeChecked(vc)
        }

        override def doEval: String = {
            vc.verNode.ensureConnected
            Console.out.println("ORG\tPARAM_KEY\t" + fields.mkString("\t"))
            getFilteredObjs(vc, where) match {
                case Right(objs) =>
                    objs.foreach {obj =>
                        val vs = fields map {f => (f, f.eval(obj.paramObject))}
                                  // check if rows are mismatched
                                  val rows = vs.filter {x => x._2.size != 1} match {
                                      case Nil => 1 // all fields are single value, no need to check
                                      case ls => // check to see if they all have the same size and same initial
                                          if ((ls.map {x => x._2.size} .filter {x => x != ls(0)._2.size} .size > 0) || 
                                              (ls.map {x => x._1.literals.init} .filter {x => x != ls(0)._1.literals.init} .size > 0))
                                              throw new IllegalArgumentException("columns with different rows returned")
                                      ls(0)._2.size
                                  }
                                  val finalVS = vs map {case (f, v) => v.size match {
                                      case 1 => List.fill(rows)(v(0))
                                      case _ => v
                                  }}
                                  for (i <- 0 to rows-1) {
                                      Console.out.println("'" + obj.org + "', '" + obj.paramKey + "', " + (for (pairs <- finalVS; val (x, ot) = pairs(i)) yield ot.toJson(x)).mkString(", "))
                                  }
                              }
                    objs.size.toString + " objects fetched."
                case Left(e) => e
            }
        }
    }
    case class UpdateStmt(val dstVc: VersionClass, val dstWhere: Where, val srcVc: Option[VersionClass], val srcWhere: Option[Where], val set: List[SetPair]) extends Statement 
{
        val (sqlCols, setValuePairs) = set.partition {sp => ((sp.field.toString == "ORG") || (sp.field.toString == "PARAM_KEY"))}
        override def resolveType: Unit = {
            dstWhere.typeChecked(dstVc)
            srcWhere.map {_.typeChecked(srcVc.get)}
            for (pair <- setValuePairs) {
                pair match {
                    case fv: FVPair => fv.typeChecked(dstVc)
                    case ff: FFPair => ff.typeChecked(dstVc, srcVc.getOrElse(null))
                }
            }
        }

        def getSrcObjs(default: List[PrmObject]): Either[String, List[PrmObject]] = {
            srcVc match {
                case Some(vc) =>
                    vc.verNode.ensureConnected
                    getFilteredObjs(vc, srcWhere.get)
                case None =>
                    Right(default)
            }
        }
        override def doEval: String = {
            dstVc.verNode.ensureConnected
            getFilteredObjs(dstVc, dstWhere) match {
                case Right(dstObjs) =>
                    getSrcObjs(dstObjs) match {
                        case Right(srcObjs) =>
                            // sanity check
                            srcObjs.size match {
                                case 0 => "no records found"
                                case 1 => _doEval(srcObjs, dstObjs)
                                case n => dstObjs.size match {
                                    case 0 => "no records found"
                                    case 1 => "multiple src records update into one dst record"
                                    case m =>
                                        if (n != m) "src records and dst records not match"
                                        else _doEval(srcObjs, dstObjs)
                                }
                            }
                        case Left(e) => e
                    }
                case Left(e) => e
            }
        }
        def _doEval(srcObjs: List[PrmObject], dstObjs: List[PrmObject]): String = {
            val src: List[PrmObject] = srcObjs.size match {
                case 1 => List.fill(dstObjs.size)(srcObjs(0))
                case _ => srcObjs
            }
            for ((srcObj, dstObj) <- srcObjs.zip(dstObjs)) {
                for (col <- sqlCols) {
                    col.field.toString.toUpperCase match {
                        case "ORG" => col match {
                            case FVPair(_, value) => dstObj.org = value.toString
                            case FFPair(_, f) => dstObj.org = f.eval(srcObj.paramObject).toString
                        }
                        case "PARAM_KEY" => col match {
                            case FVPair(_, value) => dstObj.paramKey = value.toString
                            case FFPair(_, f) => dstObj.paramKey = f.eval(srcObj.paramObject).toString
                        }
                    }
                }
                for (pair <- setValuePairs) {
                    pair match {
                        case fv: FVPair => fv.eval(dstObj.paramObject)
                        case ff: FFPair => ff.eval(dstObj.paramObject, srcObj.paramObject)
                    }
                }

                if (debugON) {
                    Console.out.println(xstream.toXML(dstObj.paramObject))
                }
            }
            dstVc.verNode.conn.withConnection {c => PrmObject.update(c, dstObjs)} match {
                case Right(_) =>
                    dstObjs.size.toString + " objects updated."
                case Left(e) => e
            }
        }
    }
    case class CopyFromStmt(val dstVc: VersionClass, val dstWhere: Where, val srcVc: VersionClass, val srcWhere: Where, val alter: Option[List[SetPair]]) extends Statement {
        val (sqlCols, setValuePairs) = alter.map {_.partition {sp => ((sp.field.toString == "ORG") || (sp.field.toString == "PARAM_KEY"))}} match {
            case Some((a,b)) => (Some(a), Some(b))
                case None => (None, None)
        }
        override def resolveType: Unit = {
            dstWhere.typeChecked(dstVc)
            srcWhere.typeChecked(srcVc)
            for (pairs <- alter; pair <- pairs) {
                pair match {
                    case fv: FVPair => fv.typeChecked(dstVc)
                    case ff: FFPair => ff.typeChecked(dstVc, srcVc)
                }
            }
        }

        override def doEval: String = {
            dstVc.verNode.ensureConnected
            srcVc.verNode.ensureConnected
            getFilteredObjs(dstVc, dstWhere) match {
                case Right(dstObjs) =>
                    getFilteredObjs(srcVc, srcWhere) match {
                        case Right(srcObjs) =>
                            // sanity check
                            srcObjs.size match {
                                case 0 => "no records found"
                                case 1 => _doEval(srcObjs, dstObjs)
                                case n => dstObjs.size match {
                                    case 0 => "no records found"
                                    case 1 => "multiple src records update into one dst record"
                                    case m =>
                                        if (n != m) "src records and dst records not match"
                                        else _doEval(srcObjs, dstObjs)
                                }
                            }
                        case Left(e) => e
                    }
                    case Left(e) => e
            }
        }
        def _doEval(srcObjs: List[PrmObject], dstObjs: List[PrmObject]): String = {
            val src: List[PrmObject] = srcObjs.size match {
                case 1 => List.fill(dstObjs.size)(srcObjs(0))
                case _ => srcObjs
            }
            for ((srcObj, dstObj) <- srcObjs.zip(dstObjs)) {
                dstObj.paramObject = ObjType(dstObj.paramObject.getClass).clone(srcObj.paramObject, ObjType(srcObj.paramObject.getClass))
                // replicate param key
                dstObj.paramKey = srcObj.paramKey
                for (cols <- sqlCols; col <- cols) {
                    col.field.toString.toUpperCase match {
                        case "ORG" => col match {
                            case FVPair(_, value) => dstObj.org = value.toString
                            case FFPair(_, f) => dstObj.org = f.eval(srcObj.paramObject).toString
                        }
                        case "PARAM_KEY" => col match {
                            case FVPair(_, value) => dstObj.paramKey = value.toString
                            case FFPair(_, f) => dstObj.paramKey = f.eval(srcObj.paramObject).toString
                        }
                    }
                }
                for (pairs <- setValuePairs; pair <- pairs) {
                    pair match {
                        case fv: FVPair => fv.eval(dstObj.paramObject)
                        case ff: FFPair => ff.eval(dstObj.paramObject, srcObj.paramObject)
                    }
                }

                if (debugON) {
                    Console.out.println(xstream.toXML(dstObj.paramObject))
                }
            }
            dstVc.verNode.conn.withConnection {c => PrmObject.update(c, dstObjs)} match {
                case Right(_) =>
                    dstObjs.size.toString + " objects updated."
                case Left(e) => e
            }
        }
    }
    case class InsertStmt(val vc: VersionClass, val fvs: List[FVPair]) extends Statement {
        val (sqlCols, fieldValuePairs) = fvs.partition {sp => ((sp.field.toString == "ORG") || (sp.field.toString == "PARAM_KEY"))}
        override def resolveType: Unit = {
            for (pair <- fieldValuePairs) {
                pair.typeChecked(vc)
            }
        }

        override def doEval: String = {
            var dstOrg: String = null
            var paramKey: String = null
            for (col <- sqlCols) {
                col.field.toString.toUpperCase match {
                    case "ORG" => dstOrg = col.value.asInstanceOf[JsonString].elmt
                    case "PARAM_KEY" => paramKey = col.value.asInstanceOf[JsonString].elmt
                }
            }
            if (dstOrg == null) throw new IllegalArgumentException("ORG missing")
            if (paramKey == null) throw new IllegalArgumentException("PARAM_KEY missing")

            vc.verNode.ensureConnected
            val dstObj = new PrmObject(dstOrg, vc.clazz.get, paramKey)
            dstObj.paramObject = ObjType(vc.clazz.get).newInstance
            for (pair <- fieldValuePairs) {
                pair.eval(dstObj.paramObject)
            }
            vc.verNode.conn.withConnection {c => PrmObject.insert(c, dstObj)} match {
                case Right(_) =>
                    if (debugON) {
                        Console.out.println(xstream.toXML(dstObj.paramObject))
                    }
                    "object inserted."
                case Left(e) => e
            }
        }
    }
    case class InsertUpdateStmt(vc: VersionClass, fvs: List[FVPair]) extends Statement{
        import Expr.RelOp._
        val (sqlCols, fieldValuePairs) = fvs.partition {sp => ((sp.field.toString == "ORG") || (sp.field.toString == "PARAM_KEY"))}
        override def resolveType: Unit = {
            for (pair <- fieldValuePairs) {
                pair.typeChecked(vc)
            }
        }

        private val insertStmt = InsertStmt(vc, fvs)
        override def doEval: String = {
            val where: Where = {
                vc.verNode.ensureConnected
                val orgVal = sqlCols.find {col => col.field.toString.toUpperCase == "ORG"}.map {case FVPair(_, v) => v}.get
                val keyVal = sqlCols.find {col => col.field.toString.toUpperCase == "PARAM_KEY"}.map {case FVPair(_, v) => v}.get
                val orgExp = BasicExp(EQ, Field(List(Literal("ORG", List.empty[AnyRef]))), orgVal)
                val keyExp = BasicExp(EQ, Field(List(Literal("PARAM_KEY", List.empty[AnyRef]))), keyVal)
                Where(AndExp(orgExp, keyExp), None)
            }
            var dstOrg: String = null
            var paramKey: String = null
            for (col <- sqlCols) {
                col.field.toString.toUpperCase match {
                    case "ORG" => dstOrg = col.value.asInstanceOf[JsonString].elmt
                    case "PARAM_KEY" => paramKey = col.value.asInstanceOf[JsonString].elmt
                }
            }
            if (dstOrg == null) throw new IllegalArgumentException("ORG missing")
            if (paramKey == null) throw new IllegalArgumentException("PARAM_KEY missing")
            getFilteredObjs(vc, where) match {
                case Right(objs) =>
                    objs.size match {
                        case 0 =>
                            insertStmt.doEval
                        case _ =>
                            (for (obj <- objs) yield {
                                val dstObj = new PrmObject(dstOrg, vc.clazz.get, paramKey)
                                dstObj.paramObject = ObjType(vc.clazz.get).newInstance
                                for (pair <- fieldValuePairs) {
                                    pair match {
                                        case fv: FVPair => fv.eval(dstObj.paramObject)
                                    }
                                }

                                if (debugON) {
                                    Console.out.println(xstream.toXML(dstObj.paramObject))
                                }
                                vc.verNode.conn.withConnection {c => PrmObject.update(c, dstObj)} match {
                                    case Right(_) =>
                                        "object updated."
                                    case Left(e) => e.toString
                                }
                            } ) mkString "\n"
                        }
                case Left(_) =>
                    insertStmt.doEval
            }
        }
    }
    case class InsertFromStmt(val dstVc: VersionClass, val srcVc: VersionClass, val srcWhere: Where, val alter: Option[List[SetPair]]) extends Statement {
        val (sqlCols, fieldValuePairs) = alter.map {_.partition {sp => ((sp.field.toString == "ORG") || (sp.field.toString == "PARAM_KEY"))}} match {
            case Some((a,b)) => (Some(a), Some(b))
                case None => (None, None)
        }
        override def resolveType: Unit = {
            srcWhere.typeChecked(srcVc)
            for (pairs <- fieldValuePairs; pair <- pairs) {
                pair match {
                    case fv: FVPair => fv.typeChecked(dstVc)
                    case ff: FFPair => ff.typeChecked(dstVc, srcVc)
                }
            }
        }

        override def doEval: String = {
            dstVc.verNode.ensureConnected
            srcVc.verNode.ensureConnected
            getFilteredObjs(srcVc, srcWhere) match {
                case Right(srcObjs) =>
                    val dstObjs = for (srcObj <- srcObjs) yield {
                        val dstObj = new PrmObject(srcObj.org, dstVc.clazz.get, srcObj.paramKey)
                        dstObj.paramObject = ObjType(dstVc.clazz.get).clone(srcObj.paramObject, ObjType(srcObj.paramObject.getClass))
                        // replicate org
                        dstObj.org = srcObj.org
                        // replicate param key
                        dstObj.paramKey = srcObj.paramKey
                        for (cols <- sqlCols; col <- cols) {
                            col.field.toString.toUpperCase match {
                                case "ORG" => col match {
                                    case FVPair(_, value) => dstObj.org = value.toString
                                    case FFPair(_, f) => dstObj.org = f.eval(srcObj.paramObject).toString
                                }
                                case "PARAM_KEY" => col match {
                                    case FVPair(_, value) => dstObj.paramKey = value.toString
                                    case FFPair(_, f) => dstObj.paramKey = f.eval(srcObj.paramObject).toString
                                }
                            }
                        }
                        for (pairs <- fieldValuePairs; pair <- pairs) {
                            pair match {
                                case fv: FVPair => fv.eval(dstObj.paramObject)
                                case ff: FFPair => ff.eval(dstObj.paramObject, srcObj.paramObject)
                            }
                        }

                        if (debugON) {
                            Console.out.println(xstream.toXML(dstObj.paramObject))
                        }
                        dstObj
                    }
                    if (dstObjs.size == 0)
                        "no records found"
                    else {
                        dstVc.verNode.conn.withConnection {c => PrmObject.insert(c, dstObjs)} match {
                            case Right(_) =>
                                srcObjs.size.toString + " objects inserted."
                            case Left(e) => e
                        }
                    }
                case Left(e) => e
            }
        }
    }
}

