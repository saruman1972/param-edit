package com.allinfinance.tools.param.util

object JsonDef {
    object Json {
        def apply(obj: AnyRef) = obj match {
            case null => JsonNull
            case s: String => JsonString(s)
            case b: java.lang.Boolean => JsonBoolean(b)
            case i: java.lang.Integer => JsonInteger(i)
            case bd: java.math.BigDecimal => JsonBigDecimal(bd)
            case d: java.util.Date => JsonDate(d)
            case e if e.getClass.isEnum => JsonEnum(e)
        }
    }

    sealed trait Json {
        override def toString: String = if (value == null) "(null)" else value.toString
        def value: AnyRef
    }
    case object JsonNull extends Json {
        override def value: AnyRef = null
        override def toString: String = "null"
    }
    case class JsonString(val elmt: String) extends Json {
        override def value: AnyRef = elmt.asInstanceOf[AnyRef]
        override def toString: String = "'" + elmt + "'"
    }
    case class JsonBoolean(val elmt: java.lang.Boolean) extends Json {
        override def value: AnyRef = elmt.asInstanceOf[AnyRef]
        override def toString: String = elmt.toString
    }
    case class JsonInteger(val elmt: java.lang.Integer) extends Json {
        override def value: AnyRef = elmt.asInstanceOf[AnyRef]
        override def toString: String = elmt.toString
    }
    case class JsonBigDecimal(val elmt: java.math.BigDecimal) extends Json {
        override def value: AnyRef = elmt.asInstanceOf[AnyRef]
        override def toString: String = elmt.toString
    }
    case class JsonDate(val elmt: java.util.Date) extends Json {
        override def value: AnyRef = elmt.asInstanceOf[AnyRef]
        override def toString: String = elmt.toString
    }
    case class JsonEnum(val elmt: AnyRef) extends Json {
        override def value: AnyRef = elmt.asInstanceOf[AnyRef]
        override def toString: String = "'" + elmt + "'"
    }
    case class JsonSet(val elmt: Set[Json]) extends Json {
        override def value: AnyRef = elmt.asInstanceOf[AnyRef]
        override def toString: String = "@" + (for (v <- elmt) yield v).mkString(", ") + "@"
    }
    case class JsonMap(val elmt: Map[Json, Json]) extends Json {
        override def value: AnyRef = elmt.asInstanceOf[AnyRef]
        override def toString: String = "{" + (for ((k, v) <- elmt) yield {k + ":" + v}).mkString(", ") + "}"
    }
    case class JsonList(val elmt: List[Json]) extends Json {
        override def value: AnyRef = elmt.asInstanceOf[AnyRef]
        override def toString: String = "[" + elmt.mkString(", ") + "]"
    }
    case class JsonCompound(val elmt: Map[String, Json]) extends Json {
        override def value: AnyRef = elmt.asInstanceOf[AnyRef]
        override def toString: String = "<" + (for ((k, v) <- elmt) yield {k + ":" + v}).mkString(", ") + ">"
    }
}
