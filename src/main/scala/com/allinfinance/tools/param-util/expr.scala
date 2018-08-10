package com.allinfinance.tools.param.util

import scala.collection.JavaConversions._

object Expr {
    import ObjDef._
    import ObjTreeNode.{VerNode, VerConfig, VerConfigs}
    import JsonDef._

    object RelOp extends Enumeration {
        type RelOp = Value
        val EQ, NE, LT, LE, GT, GE = Value
    }
    import RelOp._

    class VersionClass(val version: String, val className: String) {
        var alias: String = null
        def setAlias(a: String) {
            alias = a
            VersionClass.aliasMap(a) = this
        }
        val verNode: VerNode = VerConfigs.nodes.find {n => n.verConfig.name == version}.get
        lazy val clazz: Option[Class[_]] = 
            try {
                Some(Class.forName(className, true, verNode.getClassLoader))
            }
            catch {
                case e: Throwable =>
                    Console.out.println(e)
                    e.printStackTrace
                    None
            }
    }
    object VersionClass {
        def apply(version: String, className: String) = {
            val vc = new VersionClass(version, className)
            currentClass = vc
            activeVersions += vc.verNode
            vc
        }

        // global state
        val verMap = {
            VerConfigs.load
            Map() ++ VerConfigs.nodes.map(n => (n.verConfig.name, n))
        }
        val activeVersions = scala.collection.mutable.HashSet[VerNode]()
        var currentClass: VersionClass = null
        var aliasMap = scala.collection.mutable.HashMap[String, VersionClass]()
        def clearAlias: Unit = {
            currentClass = null
            aliasMap = scala.collection.mutable.HashMap[String, VersionClass]()
        }
    }

    case class Literal(val id: String, private val orgIndexes: List[AnyRef]=List.empty[AnyRef]) {
        var indexes: List[AnyRef] = List[AnyRef]()
        var objTypes: List[ObjType] = null
        lazy val actualObjType: ObjType = objTypes(indexes.size)
        var fd: java.lang.reflect.Field = null
        private var objClass: Class[_] = null
        def clazz = objClass
        def clazz_=(clz: Class[_]): Unit = {
            def getObjTypeHelper(objType: ObjType): List[ObjType] = objType match {
                case m : JavaMap => m :: getObjTypeHelper(m.valueType)
                case l : JavaList => l :: getObjTypeHelper(l.valueType)
                case _ => List(objType)
            }
            def resolveIndexes: Unit = {
                indexes = objTypes.zip(orgIndexes).foldLeft(List[AnyRef]()) {case (list, (ot, idx)) =>
                    val i = ot match {
                        case m: JavaMap =>
                            m.keyType match {
                                case e @ JavaEnum(_) => e.valueOf(idx)
                                case JavaInteger => JavaInteger.valueOf(idx)
                                case JavaBigDecimal => JavaBigDecimal.valueOf(idx)
                                case kt if kt.sameType(idx) => idx
                                case _ => throw new IllegalArgumentException("invalid index type[" + idx + "]")
                            }
                        case l: JavaList =>
                            JavaInteger.valueOf(idx)
                        case _ => null
                    }
                    if (i == null) throw new IllegalArgumentException("invalid index type[" + idx + "]")
                    list :+ i
                }
            }
            objClass = clz
//            fd = clz.getField(id)
//            if (fd == null) throw new IllegalArgumentException("fd is null")
            FieldCache.get(clz, id) match {
                case Some(x) =>
                    fd = x
                    objTypes = getObjTypeHelper(ObjType(fd.getGenericType))
                    if (objTypes.size <= orgIndexes.size) throw new IllegalArgumentException("invalid indexes")
                case _ =>
                    throw new IllegalArgumentException("undefined field[" + id + "] in class[" + clz + "]")
            }
            resolveIndexes
        }

        def realID: String = FieldCache.get(clazz, id) match {
            case Some(x) => x.getName
            case _ => id
        }
        // for chinese field name used in sql, use realID to do an extra lookup
        override def toString: String = realID + indexes.map {"[" + _ + "]"} .mkString
        lazy val isCollection: Boolean = (objTypes.size > 1) && (objTypes.size-1 > indexes.size)
        def evalHelper(obj: AnyRef, idxs: Seq[AnyRef]): (AnyRef, AnyRef => Unit) = {
            objTypes.zip(idxs).foldLeft((fd.get(obj): AnyRef, {x: AnyRef => fd.set(obj, x)}: AnyRef => Unit)) {(pair1, pair2) =>
                val (collection, addFunc) = pair1
                val (ov, idx) = pair2
                ov match {
                    case m @ JavaMap(p) =>
                        val coll =
                            if (collection == null) {
                                val c = m.newInstance.asInstanceOf[java.util.Map[AnyRef, AnyRef]]
                                addFunc(c)
                                c
                            }
                            else collection.asInstanceOf[java.util.Map[AnyRef, AnyRef]]
                        val key = m.keyType.valueOf(idx.asInstanceOf[AnyRef])
                        (coll.get(key), {x: AnyRef => coll.put(key, x)})
                    case l @ JavaList(p) =>
                        val coll =
                            if (collection == null) {
                                val c = l.newInstance.asInstanceOf[java.util.List[AnyRef]]
                                addFunc(c)
                                c
                            }
                            else collection.asInstanceOf[java.util.List[AnyRef]]
                        val i: Int = idx.asInstanceOf[java.lang.Integer]
                        if (i >= coll.size)
                            ( null, {x: AnyRef =>
                                        // Ugly: java.util.List does not allow adding element at random place. so we need to to a little trick, add some null to the end
                                        (coll.size to i-1) foreach (_ => coll.add(null))
                                        coll.add(x)
                                    }
                            )
                        else
                            (coll.get(i), {x: AnyRef => coll.set(i, x)})
                    case _ =>
                        throw new IllegalArgumentException("\"" + id + "\" invalid index")
                }
            }
        }
        def setVal(obj: AnyRef, value: AnyRef, otherType: ObjType): Unit = {
            val (_, addFunc) = evalHelper(obj, indexes)
            val v = objTypes.last.clone(value, otherType)
            addFunc(v)
        }
        def setVal(obj: AnyRef, value: AnyRef): Unit = {
            val (_, addFunc) = evalHelper(obj, indexes)
            addFunc(value)
        }
        def eval(obj: AnyRef): AnyRef = {
            val (v, _) = evalHelper(obj, indexes)
            v
        }
        def evalLeft(obj: AnyRef) = evalHelper(obj, indexes)
    }

    case class Field(val literals: List[Literal]) {
        var vc: VersionClass = null
        def isSqlColumn: Boolean = (literals.size == 1) && ((literals(0).id == "ORG") || (literals(0).id == "PARAM_KEY"))
        override def toString: String = literals.map{_.toString}.mkString(".")
        override def equals(other: Any) = other match {
            case that: Field => literals == that.literals
            case _ => false
        }
        lazy val actualObjType: ObjType = literals.last.actualObjType
        def resolveType(versionClass: VersionClass): Unit = {
            vc = versionClass
            literals.foldLeft(ObjType(vc.clazz.get)) {case (objType, literal) => 
                objType match {
                    case JavaCompound(clz) =>
                        literal.clazz = clz
                        literal.objTypes.last
                    case _ => throw new IllegalArgumentException("class[" + vc.clazz.get + "],field(" + toString + "): invalid compound class")
                }
            }
        }

        def flatten[A](list: List[Any]): List[A] = {
            var acc = List[A]()
            list foreach (entry => entry match {
                case a: List[Any] => acc = acc ::: flatten(a)
                case b: A => acc = acc :+ b
            })
            acc
        }
        def expandObj(obj: AnyRef, f: AnyRef => Unit, objType: ObjType): List[Any] = objType match {
            // map need to sort by key
            case m: JavaMap => (for ((k,v) <- obj.asInstanceOf[java.util.Map[AnyRef, AnyRef]]) yield expandObj(v, f, m.valueType)).toList
            case l: JavaList => (for (o <- obj.asInstanceOf[java.util.List[AnyRef]]) yield expandObj(o, f, l.valueType)).toList
            case _ => List(((obj, f), objType))
        }
        def evalHelper(obj: AnyRef, lits: List[Literal]): List[((AnyRef, AnyRef => Unit), ObjType)] = {
            lits.zipWithIndex.foldLeft(List(((obj, {x: AnyRef => x}: AnyRef => Unit), ObjType(obj.getClass)))) {(os, literal_with_index) =>
                val (literal, idx) = literal_with_index
                flatten[((AnyRef, AnyRef => Unit), ObjType)](for (((o, addFunc), ot) <- os;
                     val v =
                         if (o == null) {
                             val v = literal.clazz.newInstance.asInstanceOf[AnyRef]
                             addFunc(v)
                             v
                         } else o
                ) yield {
                    val (nv, f) = literal.evalLeft(v)
                    if (idx == lits.size-1)
                        ((nv, f), literal.actualObjType)
                    else
                        expandObj(nv, f, literal.actualObjType)
                })
            }
        }

        def setVal(obj: AnyRef, value: AnyRef, otherType: ObjType): Unit = {
            val v = actualObjType.clone(value, otherType)
            evalHelper(obj, literals).foreach {case ((_, addFunc), _) => addFunc(v)}
        }
        def setVal(obj: AnyRef, value: Json): Unit = {
            val v = actualObjType.clone(value)
            evalHelper(obj, literals).foreach {case ((_, addFunc), _) => addFunc(v)}
        }

        def eval(obj: AnyRef): List[(AnyRef, ObjType)] = {
            evalHelper(obj, literals).map {case ((v, _), ot) => (v, ot)}
        }
    }
    sealed trait SetPair {
        val field: Field
    }
    // enum value can only be required in typeCheck rather than in parsing, so value should be var
    case class FVPair(val field: Field, val value: Json) extends SetPair {
        def typeChecked(vc: VersionClass): Boolean = {
            field.resolveType(vc)
            field.actualObjType match {
                case e @ JavaEnum(_) => value match {
                    case JsonNull => true
                    case JsonString(s) =>
                        val v = e.valueOf(s)
                        if (v == null) throw new IllegalArgumentException("invalid enum value[" + s + "] for [" + e.clazz + "]")
                        true
                    case _ => false
                }
                case o => o.sameType(value)
            }
        }
        def eval(obj: AnyRef): Unit = {
            field.setVal(obj, value)
        }
        override def toString: String = field.toString + "=" + value.toString
    }
    case class FFPair(val field: Field, val value: Field) extends SetPair {
        def typeChecked(dstVC: VersionClass, srcVC: VersionClass): Boolean = {
            field.resolveType(dstVC)
            value.resolveType(srcVC)
            field.actualObjType.sameType(value.actualObjType)
        }
        def eval(dstObj: AnyRef, srcObj: AnyRef): Unit = {
            field.setVal(dstObj, value.eval(srcObj), value.actualObjType)
        }
        override def toString: String = field.toString + "=" + value.toString
    }

    sealed trait RelExp {
        def satisfied(obj: AnyRef): Boolean
        def typeChecked(vc: VersionClass): Boolean
    }
    case class BasicExp(val op: RelOp, val field: Field, var value: AnyRef) extends RelExp {
        override def toString: String = field.toString + (op match {
            case EQ => "="
            case NE => "<>"
            case LT => "<"
            case LE => "<="
            case GT => ">"
            case GE => ">="
        }) + (value match {
            case s: String => "'" + s + "'"
            case _ => value.toString
        })
        override def satisfied(obj: AnyRef): Boolean = {
            val v: AnyRef = field.eval(obj).headOption match {
                case Some((x, _)) => x
                case _ => null
            }
            val result = field.actualObjType.compareTo(v, value)
            op match {
                case EQ => result == 0
                case NE => result != 0
                case LT => result < 0
                case LE => result <= 0
                case GT => result > 0
                case GE => result >= 0
            }
        }
        override def typeChecked(vc: VersionClass) = {
            field.resolveType(vc)
            field.actualObjType match {
                case e @ JavaEnum(_) =>
                    if (value == null)
                        true
                    else {
                        if (value == null) {
                            val v = e.valueOf(value)
                            if (v == null) throw new IllegalArgumentException("invalid enum value[" + value + "] for [" + e.clazz + "]")
                            value = v
                        }
                        value != null
                    }
                case o =>
                    o.sameType(if (value == null) null else ObjType(value.getClass))
            }
        }
    }
    case class InExp(val field: Field, val values: collection.mutable.ListBuffer[AnyRef]) extends RelExp {
        override def toString: String = field.toString + " in (" + values.mkString(",") + ")"
        override def satisfied(obj: AnyRef) = {
            val v: AnyRef = field.eval(obj).headOption match {
                case Some((x, _)) => x
                case _ => null
            }
            values.find {value => field.actualObjType.compareTo(v, value) == 0} match {
                case Some(_) => true
                case None => false
            }
        }
        override def typeChecked(vc: VersionClass) = {
            field.resolveType(vc)
            values.zipWithIndex.forall {case (v, idx) =>
                field.actualObjType match {
                    case e @ JavaEnum(_) =>
                        values(idx) = e.valueOf(v)
                        values(idx) != null
                    case o =>
                        o.sameType(if (v == null) null else ObjType(v.getClass))
                }
            }
        }
    }
    case class NotExp(val exp: RelExp) extends RelExp {
        override def toString: String = "not (" + exp + ")"
        override def satisfied(obj: AnyRef) = !exp.satisfied(obj)
        override def typeChecked(vc: VersionClass) = exp.typeChecked(vc)
    }
    case class AndExp(val left: RelExp, val right: RelExp) extends RelExp {
        override def toString: String = left.toString + " and " + right.toString
        override def satisfied(obj: AnyRef) = left.satisfied(obj) && right.satisfied(obj)
        override def typeChecked(vc: VersionClass) = left.typeChecked(vc) && right.typeChecked(vc)
    }
    case class OrExp(val left: RelExp, val right: RelExp) extends RelExp {
        override def toString: String = left.toString + " or " + right.toString
        override def satisfied(obj: AnyRef) = left.satisfied(obj) || right.satisfied(obj)
        override def typeChecked(vc: VersionClass) = left.typeChecked(vc) && right.typeChecked(vc)
    }
}
