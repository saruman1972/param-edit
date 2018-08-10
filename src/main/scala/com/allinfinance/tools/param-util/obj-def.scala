package com.allinfinance.tools.param.util

import scala.swing._
import scala.swing.event._
import collection.JavaConversions._

import java.text.{DecimalFormat, SimpleDateFormat}
import javax.swing.JFormattedTextField
import javax.swing.text.MaskFormatter
import java.awt.event.FocusAdapter

import JsonDef._

case class MaskEditDone(override val source: MaskedTextField) extends ValueChanged(source)
class MaskedTextField(mask: String) extends FormattedTextField(null)
 {
    override lazy val peer: JFormattedTextField = new JFormattedTextField(new MaskFormatter(mask)) with SuperMixin
import java.awt.Font
val ft = new Font( "Monospaced", Font.PLAIN, 12 )
peer.setFont(ft)

    private lazy val actionListener = Swing.ActionListener { e =>
        publish(MaskEditDone(MaskedTextField.this))
                                                        }

    protected override def onFirstSubscribe() {
        super.onFirstSubscribe
        peer.addActionListener(actionListener)
        peer.addFocusListener(new FocusAdapter {
            override def focusLost(e: java.awt.event.FocusEvent) { publish(MaskEditDone(MaskedTextField.this)) }
        })
    }

    protected override def onLastUnsubscribe() {
        super.onLastUnsubscribe
        peer.removeActionListener(actionListener)
    }
}

object ObjDef 
{
    import FieldCache._

    sealed abstract class ObjType 
{
        val clazz: Class[_]
        def editor: Component = null
        def clone(other: AnyRef, otherType: ObjType): AnyRef
        def clone(other: Json): AnyRef
        def newInstance: AnyRef
        def preconfig(prop: Option[PropertyInfo]) { }
        def setEditValue(value: AnyRef) { }
        def getEditValue: AnyRef = null
        def editable: Boolean = false
        def format(value: AnyRef): String = if (value == null) "(null)" else value.toString
        def compareTo(value: Any, target: Any): Int = throw new IllegalArgumentException("comparison undefined on object")
        // valueOf: for map key
        def valueOf(value: AnyRef): AnyRef = null
        def sameType(otherType: ObjType): Boolean
        def sameType(other: AnyRef): Boolean = false
        def sameType(other: Json): Boolean

        def toJson(obj: AnyRef): Json
        def isCompound: Boolean = false
    }

    object ObjType
 {
        def apply(clazz: Class[_]): ObjType = {
            if (clazz.isEnum)
                JavaEnum(clazz)
            else if (clazz == classOf[java.lang.String])
                JavaString
            else if (clazz == classOf[java.lang.Boolean])
                JavaBoolean
            else if (clazz == classOf[Boolean])
                JavaBoolean
            else if (clazz == classOf[java.lang.Integer])
                JavaInteger
            else if (clazz == classOf[Int])
                JavaInteger
            else if (clazz == classOf[java.math.BigDecimal])
                JavaBigDecimal
            else if (clazz == classOf[java.util.Date])
                JavaDate
            else
                JavaCompound(clazz)
        }
        def apply(tp: java.lang.reflect.Type): ObjType = {
            tp match {
                case parameterizedType: java.lang.reflect.ParameterizedType =>
                    if (parameterizedType.getRawType == classOf[java.util.Map[_,_]])
                        JavaMap(parameterizedType)
                    else if (parameterizedType.getRawType == classOf[java.util.Set[_]])
                        JavaSet(parameterizedType)
                    else if (parameterizedType.getRawType == classOf[java.util.List[_]])
                        JavaList(parameterizedType)
                    else {
                        try {
                            // hack for AbstractRuleSet<?>
                            val arsClz = parameterizedType.getRawType.asInstanceOf[Class[_]]
                            if (arsClz.getCanonicalName == "com.allinfinance.yak.drools.client.AbstractRuleSet") {
                                val simpleRuleSetClz = Class.forName("com.allinfinance.yak.drools.client.SimpleRuleSet", true, arsClz.getClassLoader)
                                apply(simpleRuleSetClz)
                            }
                            else
                                throw new IllegalArgumentException("unrecognized type" + parameterizedType)
                        }
                        catch {case e: Throwable =>
                            e.printStackTrace
                            throw new IllegalArgumentException("unrecognized type" + parameterizedType)
                           }
                    }
                case clazz: Class[_] => apply(clazz)
            }
        }
    }
    lazy val textField = new TextField
 {
        private var max = 10
        var pattern: scala.util.matching.Regex = null
        val doc = new javax.swing.text.PlainDocument {
            override def insertString(index: Int, s: String, a: javax.swing.text.AttributeSet) {
                val newString = getText(0, index) + s + getText(index, getLength-index)
                if (pattern != null)
                    for (_ <- pattern.findFirstIn(newString)) {
                        if (newString.length <= maxLength)
                            super.insertString(index, s, a)
                    }
                else if (newString.length <= maxLength)
                    super.insertString(index, s, a)
            }
            override def remove(index: Int, len: Int) {
                val newString = getText(0, index) + getText(index+len, getLength-index-len)
                if (pattern != null)
                    for (_ <- pattern.findFirstIn(newString)) {
                        super.remove(index, len)
                    }
                else
                    super.remove(index, len)
            }
        }

import java.awt.Font
val ft = new Font( "Monospaced", Font.PLAIN, 12 )
peer.setFont(ft)
        peer.setDocument(doc)
        peer.setColumns(maxLength)

        def maxLength: Int = max
        def maxLength_=(value: Int): Int = {
            val old = max
            max = value
            peer.setColumns(max)
            old
        }

        override def text_=(s: String): Unit = {
            val tempPattern = pattern
            pattern = null
            doc.remove(0, doc.getLength)
            super.text = s
            pattern = tempPattern
        }
    }

    case object JavaString extends ObjType 
{
        val clazz = classOf[java.lang.String]
        override def newInstance: AnyRef = ""
        override def clone(other: AnyRef, otherType: ObjType): AnyRef = if (other == null) null else other.toString
        override def clone(other: Json): AnyRef = other match {
            case JsonNull => null
            case JsonString(s) => s
            case _ => other.toString
        }
        override def toJson(obj: AnyRef): Json = JsonString(obj.asInstanceOf[String])

        override def editor: Component = textField
        override def preconfig(prop: Option[PropertyInfo]) {
            prop.map {p =>
                textField.maxLength = p.length
                textField.pattern = null
                textField.horizontalAlignment = Alignment.Left
            }
        }
        override def setEditValue(value: AnyRef) { textField.text = value.asInstanceOf[String] }
        override def getEditValue: AnyRef = if (textField.text == "(null)")
                                                null
                                            else
                                                textField.text

        override def compareTo(value: Any, target: Any): Int =
            if (value == null) {
                if (target == null) 0 else -1
            }
            else if (target == null)
                1
            else
                value.asInstanceOf[String].compareTo(target.toString)
        override def valueOf(value: AnyRef): AnyRef = {
            if (value == null)
                null
            else
                value.toString
        }
        override def sameType(otherType: ObjType): Boolean = otherType == JavaString
        override def sameType(other: AnyRef): Boolean = (other == null) || other.isInstanceOf[String]
        override def sameType(other: Json): Boolean = other match {
            case JsonNull => true
            case JsonString(_) => true
            case _ => false
        }
    }
    case object JavaBoolean extends ObjType 
{
        val clazz = classOf[java.lang.Boolean]
        override def newInstance: AnyRef = new java.lang.Boolean(false)
        override def clone(other: AnyRef, objType: ObjType): AnyRef = if (other == null) null else {
            objType match {
                case JavaBoolean => other
                case _ => null
            }
        }
        override def clone(other: Json): AnyRef = other match {
            case JsonNull => null
            case JsonBoolean(b) => b
            case JsonString(s) => s match {
                case "是" => new java.lang.Boolean("true")
                case "否" => new java.lang.Boolean("false")
                case _ => new java.lang.Boolean("false")
            }
            case _ => null
        }
        override def toJson(obj: AnyRef): Json = JsonBoolean(obj.asInstanceOf[java.lang.Boolean])

        private lazy val checkBox = new CheckBox
        override def editor: Component = checkBox
        override def setEditValue(value: AnyRef) { checkBox.selected = value.asInstanceOf[Boolean] }
        override def getEditValue: AnyRef = new java.lang.Boolean(checkBox.selected)

        override def compareTo(value: Any, target: Any): Int =
            if (value == null) {
                if (target == null) 0 else -1
            }
            else if (target == null)
                1
            else
                value.asInstanceOf[java.lang.Boolean].compareTo(new java.lang.Boolean(target.toString))
        override def valueOf(value: AnyRef): AnyRef = {
            if (value == null)
                null
            else
                new java.lang.Boolean(value.toString)
        }
        override def sameType(otherType: ObjType): Boolean = otherType == JavaBoolean
        override def sameType(other: AnyRef): Boolean = (other == null) || other.isInstanceOf[java.lang.Boolean]
        override def sameType(other: Json): Boolean = other match {
            case JsonNull => true
            case JsonBoolean(_) => true
            case _ => false
        }
    }

    case object JavaInteger extends ObjType 
{
        val clazz = classOf[java.lang.Integer]
        override def newInstance: AnyRef = new java.lang.Integer(0)
        override def clone(other: AnyRef, objType: ObjType): AnyRef = if (other == null) null else {
            objType match {
                case JavaInteger => other
                case JavaBigDecimal => 
                    try {
                        new java.lang.Integer(other.asInstanceOf[java.math.BigDecimal].intValueExact)
                    }
                    catch {
                        case _: Throwable => null
                    }
                case _ => null
            }
        }
        val pattern = """^[0-9]+""".r
        override def clone(other: Json): AnyRef = other match {
            case JsonNull => null
            case JsonInteger(i) => i
            case JsonBigDecimal(bd) => try {new java.lang.Integer(bd.intValueExact)} catch {case _: Throwable => null}
            case JsonString(s) => try {
                pattern.findFirstIn(s) match {
                    case Some(v) => java.lang.Integer.valueOf(v)
                    case None => null
                }
            } catch {case _: Throwable => null}
            case _ => null
        }
        override def toJson(obj: AnyRef): Json = JsonInteger(obj.asInstanceOf[java.lang.Integer])

        override def editor: Component = textField
        override def preconfig(prop: Option[PropertyInfo]) {
            prop.map {p =>
                textField.maxLength = p.length
                textField.pattern = "^[0-9]*$".r
                textField.horizontalAlignment = Alignment.Right
            }
        }
        override def setEditValue(value: AnyRef) {
            textField.text = ""
            textField.text = 
                if (value == null) "" else value.toString
        }
        override def getEditValue: AnyRef = try {
            new java.lang.Integer(textField.text)
        }
        catch {
            case _: Throwable => null
        }

        override def compareTo(value: Any, target: Any): Int =
            if (value == null) {
                if (target == null) 0 else -1
            }
            else if (target == null)
                1
            else
                value.asInstanceOf[java.lang.Integer].compareTo(new java.lang.Integer(target.toString))
        override def valueOf(value: AnyRef): AnyRef = {
            if (value == null)
                null
            else
                new java.lang.Integer(value.toString)
        }
        override def sameType(otherType: ObjType): Boolean = otherType == JavaInteger
        override def sameType(other: AnyRef): Boolean = (other == null) || (other match {
            case _: java.lang.Integer => true
            case bd: java.math.BigDecimal =>
                try {
                    bd.intValueExact
                    true
                }
                catch {
                    case _: Throwable => false
                }
            case _ => false
        })
        override def sameType(other: Json): Boolean = other match {
            case JsonNull => true
            case JsonInteger(_) => true
            case JsonBigDecimal(_) => true
            case _ => false
        }
    }
    case object JavaBigDecimal extends ObjType 
{
        val clazz = classOf[java.math.BigDecimal]
        override def newInstance: AnyRef = new java.math.BigDecimal(0.00)
        override def clone(other: AnyRef, objType: ObjType): AnyRef = if (other == null) null else {
            objType match {
                case JavaInteger => new java.math.BigDecimal(other.asInstanceOf[java.lang.Integer])
                case JavaBigDecimal => other
                case _ => null
            }
        }
        override def clone(other: Json): AnyRef = other match {
            case JsonNull => null
            case JsonInteger(i) => new java.math.BigDecimal(i)
            case JsonBigDecimal(bd) => bd
            case JsonString(s) => try {new java.math.BigDecimal(s)} catch {case _: Throwable => null}
            case _ => null
        }
        override def toJson(obj: AnyRef): Json = JsonBigDecimal(obj.asInstanceOf[java.math.BigDecimal])

//        lazy val textField = new FormattedTextField(new DecimalFormat("#.0###############"))
        override def editor: Component = textField
        override def preconfig(prop: Option[PropertyInfo]) {
            prop.map {p =>
                textField.maxLength = p.length
                textField.pattern = "^[0-9]*(\\.[0-9]*)?$".r
                textField.horizontalAlignment = Alignment.Right
            }
        }
        override def setEditValue(value: AnyRef) { textField.text = if (value == null) "" else value.toString }
        override def getEditValue: AnyRef = try {
            new java.math.BigDecimal(textField.text)
        }
        catch {
            case _: Throwable => null
        }

        override def compareTo(value: Any, target: Any): Int =
            if (value == null) {
                if (target == null) 0 else -1
            }
            else if (target == null)
                1
            else
                value.asInstanceOf[java.math.BigDecimal].compareTo(new java.math.BigDecimal(target.toString))
        override def valueOf(value: AnyRef): AnyRef = {
            if (value == null)
                null
            else
                new java.math.BigDecimal(value.toString)
        }
        override def sameType(otherType: ObjType): Boolean = otherType match {
            case JavaInteger => true
            case JavaBigDecimal => true
            case _ => false
        }
        override def sameType(other: AnyRef): Boolean = (other == null) || other.isInstanceOf[java.math.BigDecimal]
        override def sameType(other: Json): Boolean = other match {
            case JsonNull => true
            case JsonInteger(_) => true
            case JsonBigDecimal(_) => true
            case _ => false
        }
    }
    case object JavaDate extends ObjType 
{
        val clazz = classOf[java.util.Date]
        override def newInstance: AnyRef = dateFormat.parse("1900/01/01")
        override def clone(other: AnyRef, objType: ObjType): AnyRef = if (other == null) null else {
            objType match {
                case JavaDate => new java.util.Date(other.asInstanceOf[java.util.Date].getTime)
                case _ => null
            }
        }
        override def clone(other: Json): AnyRef = other match {
            case JsonNull => null
            case JsonDate(d) => d
            case JsonString(s) => try {dateFormat.parse(s)} catch {case _: Throwable => dateFormat2.parse(s)}
            case _ => null
        }
        override def toJson(obj: AnyRef): Json = JsonDate(obj.asInstanceOf[java.util.Date])

        val dateFormat = new java.text.SimpleDateFormat("yyyy/mm/dd")
        val dateFormat2 = new java.text.SimpleDateFormat("yyyy-mm-dd")
        override def format(value: AnyRef): String = if (value == null) "" else dateFormat.format(value.asInstanceOf[java.util.Date])
        lazy val dateEditor = new MaskedTextField("####/##/##")
        override def editor: Component = dateEditor
        override def preconfig(prop: Option[PropertyInfo]) {
            // textField.maxLength = 10
            // textField.pattern = "^[0-9]{0,4}/[0-9]{0,2}/[0-9]{0,2}$".r
            // textField.horizontalAlignment = Alignment.Left
        }
        override def setEditValue(value: AnyRef) {
            // textField.text = ""
            // textField.text = if (value == null) "//" else dateFormat.format(value.asInstanceOf[java.util.Date])
            dateEditor.text = if (value == null) "//" else dateFormat.format(value.asInstanceOf[java.util.Date])
        }
        override def getEditValue: AnyRef = try {
//            dateFormat.parse(textField.text)
            dateFormat.parse(dateEditor.text)
        }
        catch {
            case _: Throwable => null
        }

        override def compareTo(value: Any, target: Any): Int =
            if (value == null) {
                if (target == null) 0 else -1
            }
            else if (target == null)
                1
            else
                value.asInstanceOf[java.util.Date].compareTo(dateFormat.parse(target.toString))
        override def valueOf(value: AnyRef): AnyRef = {
            if (value == null)
                null
            else
                dateFormat.parse(value.toString)
        }
        override def sameType(otherType: ObjType): Boolean = otherType == JavaDate
        override def sameType(other: AnyRef): Boolean = (other == null) || other.isInstanceOf[java.util.Date]
        override def sameType(other: Json): Boolean = other match {
            case JsonNull => true
            case JsonDate(_) => true
            case _ => false
        }
    }
    class JavaEnum(val clazz: Class[_]) extends ObjType {
        import com.allinfinance.tools.param.util.EnumUtil
        class EnumWrapper(val value: AnyRef, val desc: String) {
            override def toString = value + "|" + desc
        }
        lazy val enums = {
            val annotationClass = Class.forName("com.allinfinance.yak.support.meta.EnumInfo", true, clazz.getClassLoader)
            val method = annotationClass.getMethod("value")
            for(annotation <- clazz.getDeclaredAnnotations;
                if annotation.annotationType.getName == "com.allinfinance.yak.support.meta.EnumInfo";
                desc <- method.invoke(annotation).asInstanceOf[Array[String]].toList) yield {
                    // can't make it type checked
                    //                EnumWrapper(Enum.valueOf(clazz, desc.split('|')(0)), desc)
                    // new EnumWrapper(EnumUtil.valueOf(clazz, desc.split('|')(0)), desc)
                    val x = desc.split('|')
                    (EnumUtil.valueOf(clazz, x(0)), x(1))
                }
        }.toList
        lazy val enumWrappers = enums.map {x => new EnumWrapper(x._1, x._2)}
        lazy val enumDescMap = Map[AnyRef, EnumWrapper]() ++ enumWrappers.map(wrapper => (wrapper.value -> wrapper))
        lazy val enumValueMap = Map[String, EnumWrapper]() ++ enumWrappers.map(wrapper => (wrapper.desc, wrapper)) ++ enumWrappers.map(wrapper => (wrapper.value.toString, wrapper))
        override def format(value: AnyRef): String = enumDescMap.get(value).map{_.toString}.getOrElse("(null)")
        override def newInstance: AnyRef = null
        override def clone(other: AnyRef, objType: ObjType): AnyRef = if (other == null) null else {
            objType match {
                case JavaEnum(e) => if (e.getName != clazz.getName) null else valueOf(other)
                case _ => null
            }
        }
        override def clone(other: Json): AnyRef = other match {
            case JsonNull => null
            case JsonString(s) => valueOf(s)
            case JsonEnum(e) => valueOf(e)
            case _ => null
        }
        override def toJson(obj: AnyRef): Json = JsonEnum(obj)

        private lazy val comboBox = new ComboBox(enumWrappers)
        override def editor: Component = comboBox
        override def setEditValue(value: AnyRef) {
            comboBox.selection.item = enumDescMap.getOrElse(value, null)
        }
        override def getEditValue: AnyRef = 
            try {
                comboBox.selection.item.value
            }
            catch { case _: Throwable => null }

        val methodCompareTo: java.lang.reflect.Method = clazz.getMethods.toList.find(m => m.getName == "compareTo").get
        override def compareTo(value: Any, target: Any): Int =
            if (value == null) {
                if (target == null) 0 else -1
            }
            else if (target == null)
                1
            else
                methodCompareTo.invoke(value, EnumUtil.valueOf(clazz, target.toString)).asInstanceOf[java.lang.Integer]
        override def valueOf(value: AnyRef): AnyRef = {
            if (value == null)
                null
            else
                try {
                    EnumUtil.valueOf(clazz, value.toString)
                }
                catch {
                    case _: Throwable =>
                        enumValueMap.get(value.toString) match {
                            case Some(wrapper) => wrapper.value
                            case _ => null
                    }
                }
        }
        override def sameType(otherType: ObjType): Boolean = otherType match {
            case JavaString => true
            case JavaEnum(oc) => clazz.getName == oc.getName
            case _ => false
        }
        override def sameType(other: AnyRef): Boolean = (other == null) || (valueOf(other) != null)
        override def sameType(other: Json): Boolean = other match {
            case JsonNull => true
            case JsonEnum(e) => valueOf(e) != null
            case _ => false
        }
    }
    object JavaEnum {
        private val enumMap = scala.collection.mutable.Map[Class[_], JavaEnum]()
        def apply(clazz: Class[_]) = enumMap.getOrElseUpdate(clazz, new JavaEnum(clazz))
        def unapply(e: JavaEnum): Option[Class[_]] = Some(e.clazz)
    }
    case class JavaSet(val parameterizedType: java.lang.reflect.ParameterizedType) extends ObjType 
{
        val clazz = classOf[java.util.Map[AnyRef, AnyRef]]
        lazy val valueType: ObjType = ObjType(parameterizedType.getActualTypeArguments.apply(0))
        override def newInstance: AnyRef = new java.util.HashMap[AnyRef, AnyRef]
        override def clone(other: AnyRef, objType: ObjType): AnyRef = if (other == null) null else {
            objType match {
                case st @ JavaSet(_) =>
                    val dstSet = new java.util.HashSet[AnyRef]
                    for (sv <- other.asInstanceOf[java.util.Set[AnyRef]]) {
                        dstSet.add(valueType.clone(sv, st.valueType))
                    }
                    dstSet
                case _ => null
            }
        }
        override def clone(other: Json): AnyRef = other match {
            case JsonNull => null
            case JsonSet(s) =>
                val dstSet = new java.util.HashSet[AnyRef]
                for (v <- s)
                    dstSet.add(valueType.clone(v))
                dstSet
            case _ => null
        }
        override def toJson(obj: AnyRef): Json =
            JsonSet(Set[Json]() ++ 
                    (for (v <- obj.asInstanceOf[java.util.Set[AnyRef]]) yield valueType.toJson(v)))

        // other: another JavaMap
        override def sameType(otherType: ObjType): Boolean = otherType match {
            case st @ JavaSet(_) => valueType.sameType(st.valueType)
            case _ => false
        }
        override def sameType(other: AnyRef): Boolean = false
        override def sameType(other: Json): Boolean = other match {
            case JsonNull => true
            case JsonSet(s) => s.forall {case v => valueType.sameType(v)}
            case _ => false
        }
        override def isCompound: Boolean = true
    }
    case class JavaMap(val parameterizedType: java.lang.reflect.ParameterizedType) extends ObjType 
{
        val clazz = classOf[java.util.Map[AnyRef, AnyRef]]
        lazy val keyType: ObjType = ObjType(parameterizedType.getActualTypeArguments.apply(0))
        lazy val valueType: ObjType = ObjType(parameterizedType.getActualTypeArguments.apply(1))
        override def newInstance: AnyRef = new java.util.HashMap[AnyRef, AnyRef]
        override def clone(other: AnyRef, objType: ObjType): AnyRef = if (other == null) null else {
            objType match {
                case st @ JavaMap(_) =>
                    val dstMap = new java.util.HashMap[AnyRef, AnyRef]
                    for ((sk, sv) <- other.asInstanceOf[java.util.Map[AnyRef, AnyRef]]) {
                        val dk = keyType.clone(sk, st.keyType)
                        if (dk != null)
                            dstMap.put(dk, valueType.clone(sv, st.valueType))
                    }
                    dstMap
                case _ => null
            }
        }
        override def clone(other: Json): AnyRef = other match {
            case JsonNull => null
            case JsonMap(m) =>
                val dstMap = new java.util.HashMap[AnyRef, AnyRef]
                for ((k,v) <- m)
                    dstMap.put(keyType.clone(k), valueType.clone(v))
                dstMap
            case _ => null
        }
        override def toJson(obj: AnyRef): Json =
            JsonMap(Map[Json, Json]() ++ 
                    (for ((k,v) <- obj.asInstanceOf[java.util.Map[AnyRef, AnyRef]]) yield (keyType.toJson(k), valueType.toJson(v))))

        // other: another JavaMap
        override def sameType(otherType: ObjType): Boolean = otherType match {
            case st @ JavaMap(_) => keyType.sameType(st.keyType) && valueType.sameType(st.valueType)
            case _ => false
        }
        override def sameType(other: AnyRef): Boolean = false
        override def sameType(other: Json): Boolean = other match {
            case JsonNull => true
            case JsonMap(m) => m.forall {case (k,v) => keyType.sameType(k) && valueType.sameType(v)}
            case _ => false
        }
        override def isCompound: Boolean = true
    }
    case class JavaList(val parameterizedType: java.lang.reflect.ParameterizedType) extends ObjType 
{
        val clazz = classOf[java.util.List[AnyRef]]
        lazy val valueType: ObjType = ObjType(parameterizedType.getActualTypeArguments.apply(0))
        override def newInstance: AnyRef = new java.util.ArrayList[AnyRef]
        override def clone(other: AnyRef, objType: ObjType): AnyRef = if (other == null) null else {
            objType match {
                case st @ JavaList(_) =>
                    val dstList = new java.util.ArrayList[AnyRef]
                    for (sv <- other.asInstanceOf[java.util.List[AnyRef]])
                        dstList.add(valueType.clone(sv, st.valueType))
                    dstList
                case _ => null
            }
        }
        override def clone(other: Json): AnyRef = other match {
            case JsonNull => null
            case JsonList(l) =>
                val dstList = new java.util.ArrayList[AnyRef]
                for (v <- l)
                    dstList.add(valueType.clone(v))
                dstList
            case _ => null
        }
        override def toJson(obj: AnyRef): Json =
            JsonList(for (o <- obj.asInstanceOf[java.util.List[AnyRef]].toList) yield valueType.toJson(o))

        // other: another JavaList
        override def sameType(otherType: ObjType): Boolean = otherType match {
            case st @ JavaList(_) => valueType.sameType(st.valueType)
            case _ => false
        }
        override def sameType(other: AnyRef): Boolean = false
        override def sameType(other: Json): Boolean = other match {
            case JsonNull => true
            case JsonList(l) => l.forall {valueType.sameType}
            case _ => false
        }
        override def isCompound: Boolean = true
    }

    case class JavaCompound(val clazz: Class[_]) extends ObjType 
{
        override def newInstance: AnyRef = clazz.newInstance.asInstanceOf[AnyRef]
        override def clone(other: AnyRef, objType: ObjType): AnyRef = if (other == null) null else {
            objType match {
                case JavaCompound(srcClazz) =>
                    val obj = clazz.newInstance
                    if (clazz.getName == srcClazz.getName) {
                        for (dstFw <- FieldCache.getFieldWrappers(clazz)) {
                            FieldCache.getFieldWrapper(srcClazz, dstFw.name).foreach {
                                srcFw =>
                                    val srcValue = srcFw.field.get(other)
                                    dstFw.field.set(obj, dstFw.objType.clone(srcValue, srcFw.objType))
                            }
                        }
                    }
                    obj.asInstanceOf[AnyRef]
                case _ => null
            }
        }
        override def clone(other: Json): AnyRef = other match {
            case JsonNull => null
            case JsonCompound(c) =>
                val obj = clazz.newInstance.asInstanceOf[AnyRef]
                for ((f, v) <- c) {
                    FieldCache.getFieldWrapper(clazz, f) match {
                        case Some(fw) => fw.set(obj, fw.objType.clone(v))
                        case None => throw new IllegalArgumentException("invalid field[" + f + "] for class[" + clazz + "]")
                    }
                }
                obj
            case _ => null
        }
        override def toJson(obj: AnyRef): Json =
            JsonCompound(Map[String, Json]() ++
                         (for (fw <- FieldCache.getFieldWrappers(clazz);
                              val v = fw.get(obj);
                              if (v != null)) yield (fw.name, {println(fw);fw.objType.toJson(v)})))

        // other: another JavaCompound
        override def sameType(otherType: ObjType): Boolean = otherType match {
            case JavaCompound(c) => clazz.getName == c.getName
            case _ => false
        }
        override def sameType(other: AnyRef): Boolean = false
        override def sameType(other: Json): Boolean = other match {
            case JsonNull => true
            case JsonCompound(c) => c.forall {case (f,v) => (f != JsonNull) && JavaString.sameType(f) && {
                FieldCache.getFieldWrapper(clazz, f) match {
                    case Some(fw) => fw.objType.sameType(v)
                    case None => false
                }
            }}
            case _ => false
        }
        override def isCompound: Boolean = true
    }
}

