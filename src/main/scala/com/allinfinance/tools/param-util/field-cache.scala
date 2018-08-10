package com.allinfinance.tools.param.util

import collection.JavaConversions._
import ObjDef.ObjType

object FieldCache {
    import scala.collection.mutable.{Map => MutableMap}

    case class PropertyInfo(val name: String, val precision: Int, val length: Int, val hint: String)
    case class FieldWrapper(val field: java.lang.reflect.Field,
                            val objType: ObjType,
                            val name: String,
                            val chineseName: String,
                            val propertyInfo: Option[PropertyInfo]) {
        def get(obj: AnyRef): AnyRef = field.get(obj)
        def set(obj: AnyRef, value: AnyRef): Unit = field.set(obj, value)
    }
    case class ClassWrapper(val clazz: Class[_],
                            val fieldWrappers: Vector[FieldWrapper],
                            val fieldWrapperMap: Map[String, FieldWrapper])
    val cache = MutableMap.empty[Class[_], ClassWrapper]
    def getClassWrapper(clazz: Class[_]): Option[ClassWrapper] =
        if (clazz == null)
            None
        else
            Some(cache.getOrElseUpdate(clazz, allFields(clazz)))
    def getFieldWrapper(clazz: Class[_], fname: String): Option[FieldWrapper] = 
        for (cw <- getClassWrapper(clazz); fw <- cw.fieldWrapperMap.get(fname)) yield fw

    def get(clazz: Class[_], fname: String) = getFieldWrapper(clazz, fname).map {_.field}
    def getPropertyInfo(clazz: Class[_], fname: String) = for (fw <- getFieldWrapper(clazz, fname); pi <- fw.propertyInfo) yield pi
    def chineseName(clazz: Class[_], fname: String) = getFieldWrapper(clazz, fname).map {_.chineseName}
    def getFieldWrappers(clazz: Class[_]): Vector[FieldWrapper] = getClassWrapper(clazz) match {
        case Some(cw) => cw.fieldWrappers
        case None => Vector.empty[FieldWrapper]
    }
    private def allFields(clazz: Class[_]): ClassWrapper = {
        def recurFields(clz: Class[_]): List[java.lang.reflect.Field] = {
            val fds = clz.getDeclaredFields.filter {fd => !java.lang.reflect.Modifier.isStatic(fd.getModifiers)}.toList
            fds.foreach {_.setAccessible(true)}
            val superFds = {
                val superClazz = try {
                    clz.getGenericSuperclass.asInstanceOf[Class[_]]
                }
                catch {case e: Throwable =>
                    clz.getGenericSuperclass.asInstanceOf[java.lang.reflect.ParameterizedType].getRawType.asInstanceOf[Class[_]]
                   }
                if ((superClazz == null) || (superClazz.getCanonicalName == "java.lang.Object"))
                    List.empty[java.lang.reflect.Field]
                else
                    recurFields(superClazz)
            }
            fds ++ superFds
        }
        val annClazz = Class.forName("com.allinfinance.yak.support.meta.PropertyInfo", true, clazz.getClassLoader)
        val fds = recurFields(clazz)
        val (crfl,crfm) = fds.foldLeft((Vector.empty[FieldWrapper], Map.empty[String,FieldWrapper])) {
            case ((fl,fm), fd) =>
                fd.setAccessible(true)
            val ot = ObjType(fd.getGenericType)
            fd.getDeclaredAnnotations.toList.filter {_.annotationType.getName == "com.allinfinance.yak.support.meta.PropertyInfo"} match {
                case prop :: _ =>
                    val name = annClazz.getMethod("name").invoke(prop).asInstanceOf[String]
                    val precision = annClazz.getMethod("precision").invoke(prop).asInstanceOf[Int]
                    val length = annClazz.getMethod("length").invoke(prop).asInstanceOf[Int]
                    val hint = annClazz.getMethod("hint").invoke(prop).asInstanceOf[String]
                    val propertyInfo = PropertyInfo(name, precision, length, hint)
                    val fw = FieldWrapper(fd, ot, fd.getName, name, Some(propertyInfo))
                    (fl :+ fw, fm + (fd.getName -> fw) + (name -> fw))
                case _ =>
                    val fw = FieldWrapper(fd, ot, fd.getName, fd.getName, None)
                    (fl :+ fw, fm + (fd.getName -> fw))
            }
        }
        ClassWrapper(clazz,crfl,crfm)
    }
}


