package com.allinfinance.tools.param.util

import scala.swing._
import scala.collection.mutable.ArrayBuffer
import collection.JavaConversions._

import ObjDef._
import FieldCache._

object ObjTreeNode {
    abstract class MyWorker(message: String) extends javax.swing.SwingWorker[Unit, Unit] {
        val progressMonitor = new javax.swing.ProgressMonitor(null, message, "", 0, 100)
        addPropertyChangeListener(new java.beans.PropertyChangeListener {
            override def propertyChange(evt: java.beans.PropertyChangeEvent) {
                if (evt.getPropertyName == "progress") {
                    val i = evt.getNewValue.asInstanceOf[java.lang.Integer]
                    progressMonitor.setProgress(i)
                    progressMonitor.setNote("Completed %02d%%.\n".format(i))
                    if (progressMonitor.isCanceled()) {
                        cancel(true)  // task.cancel(true)
                    }
                }
            }
        })
        override def done {
            progressMonitor.close
        }
    }

    abstract class Node {
        val children: ArrayBuffer[Node] = ArrayBuffer.empty[Node]
        var parent: Node = null
        def apply(idx: Int): Node = children(idx)
        def add(node: Node) {
            children += node
            node.parent = this
        }
        def add(idx: Int, node: Node) {
            children.insert(idx, node)
            node.parent = this
        }
        def remove(node: Node) {
            children -= node
            node.parent = null
        }
        def remove(index: Int): Node = {
            val n = children.remove(index)
            n.parent = null
            n
        }
        private var dirtyStatus = false
        def dirty = dirtyStatus
        def dirty_=(dt: Boolean): Unit = {
            dirtyStatus = dt
            if (dt && (parent != null))
                parent.dirty = dt
        }

        var deleteQueue: ArrayBuffer[Node] = ArrayBuffer.empty[Node]
        def flushChanges: Unit = {
            dirty = false
            for (n <- children) n.flushChanges
        }
    }

    // wrap anything for editing
    case class WrapNode(val value: AnyRef) extends Node with ValueTrait {
        override def toString: String = if (value != null) value.toString else ""
        val objType: ObjType = null
        override def valueOf: AnyRef = value
        override def update(value: AnyRef): Unit = {}
    }
    case class VerConfig(val name: String, val jdbcUrl: String, val jdbcUserName: String, val jdbcPassword: String, val libPath: String) {
        def this() = this("","","","","")
    }
    case class VerNode(var verConfig: VerConfig) extends Node {
        override def toString = verConfig.name

        var conn: JdbcConnection = null
        var classLoader: ClassLoader = null

        def ensureConnected: Unit = {
            getConnection
            getClassLoader
        }

        def getConnection: JdbcConnection = {
            if (conn == null) {
                conn = JdbcConnection(verConfig.jdbcUrl, verConfig.jdbcUserName, verConfig.jdbcPassword)
                conn.withConnection {c => PrmObject.getOrgs(c)} match {
                    case Right(orgs) =>
                        orgs.foreach {org => add(OrgNode(org, this)) }
                    case Left(_) =>
                }
                dirty = false
            }
            conn
        }

        def getClassLoader: ClassLoader = {
            if (classLoader == null) {
                val files = (new java.io.File(verConfig.libPath)).listFiles
                val jars = for (file <- files; if (!file.isDirectory && file.getName.endsWith(".jar"))) yield file
                classLoader = new java.net.URLClassLoader(jars.map{jar => new java.net.URL("file:" + jar)}, this.getClass.getClassLoader)
            }
            classLoader
        }

        def disconnect: Unit = {
            conn = null
            classLoader = null
            dirty = false
        }

        override def flushChanges: Unit = {
            for (n <- children; if n.dirty)
                n.asInstanceOf[OrgNode].flushChanges
            for (orgN <- deleteQueue)
                orgN.asInstanceOf[OrgNode].flushChanges
            dirty = false
        }

        import java.util.jar._
        import java.util.zip._
        def escapeClassName(clazz : String) : String = {
            "\\.class$".r.replaceAllIn("/".r.replaceAllIn(clazz, "."), "")
        }

        def classInJar(jar : java.io.File) : List[String] = {
            def namesInJar(zins : ZipInputStream) : List[String] = {
                val ze = zins.getNextEntry()
                if (ze == null) {
                    Nil
                }
                else {
                    ze.getName :: namesInJar(zins)
                }
            }
            
            val fins = new java.io.FileInputStream(jar)
            val zins = new ZipInputStream(fins)
            val names = namesInJar(zins).filter(s => s.endsWith("class")).map(escapeClassName)
            zins.close()
            fins.close()
            
            names
        }

        def getClasses: List[String] = {
            val files = (new java.io.File(verConfig.libPath)).listFiles
            val clzs = for (file <- files;
                            if (!file.isDirectory && file.getName.matches("[a-z]*-param-def-.*.jar"));
                            clz <- classInJar(file);
                            if (!clz.matches("com\\.allinfinance\\.[a-z]*\\.param\\.def\\.enums.*"))
                        ) yield clz
            clzs.toList.sorted
        }
    }
    object VerConfigs {
        class VerConfigs {
            val entries: java.util.List[VerConfig] = new java.util.ArrayList[VerConfig]()
        }

        val fileName = "verConfigs.xml"
        val nodes = new collection.mutable.ArrayBuffer[VerNode]()

        import com.thoughtworks.xstream.XStream
        import com.thoughtworks.xstream.io.xml.DomDriver
        
        val xstream = new XStream(new DomDriver)
        xstream.setClassLoader(this.getClass.getClassLoader)
        xstream.alias("VerConfig", classOf[VerConfig])
        xstream.alias("VerConfigs", classOf[VerConfigs])

        def flush: Unit = {
            val verConfigs = new VerConfigs
            nodes.map {n => verConfigs.entries += n.verConfig}
            val s = xstream.toXML(verConfigs)
            Some(new java.io.PrintWriter(fileName)).foreach{p =>
                p.write(s)
                p.close
            }
        }
        def load: Unit = {
            val s = scala.io.Source.fromFile(fileName).mkString
            val verConfigs = xstream.fromXML(s).asInstanceOf[VerConfigs]
            verConfigs.entries.map {nodes += VerNode(_)}
        }
        def add(vc: VerConfig): Unit = {
            val vn = VerNode(vc)
            nodes += vn
        }
    }
    class OrgNode(val verNode: VerNode) extends Node {
        parent = verNode
        private var _org: String = null
        def org: String = _org
        def org_=(o: String): Unit = {
            dirty = true
            _org = o
            for (cn <- children; on <- cn.children) {
                cn.asInstanceOf[ClassNode].dirty = true
                on.asInstanceOf[ObjNode].org = org
            }
        }

        override def toString = org
        var loaded = false
        def ensureLoaded: Unit = {
            if (loaded == false) {
                parent.asInstanceOf[VerNode].conn.withConnection {c => PrmObject.getClasses(c, org, parent.asInstanceOf[VerNode].classLoader)} match {
                    case Right(classes) =>
                        classes.foreach {clazz => add(ClassNode(clazz, this)) }
                    case Left(_) =>
                }
                loaded = true
            }
        }

        override def flushChanges: Unit = {
            doDelete
            for (n <- children; if n.dirty)
                n.asInstanceOf[ClassNode].flushChanges
            dirty = false
        }
        def doDelete: Unit = {
            val conn = parent.asInstanceOf[VerNode].conn
            for (cn <- deleteQueue) {
                cn.asInstanceOf[ClassNode].doDelete(conn)
            }
            deleteQueue.reduceToSize(0)
        }

        def removeAll: Unit = {
            ensureLoaded
            children.foreach {cn => cn.asInstanceOf[ClassNode].removeAll}
            children.reduceToSize(0)
            dirty = true
        }
    }
    object OrgNode {
        def apply(org: String, parent: VerNode) = {
            val on = new OrgNode(parent)
            on.org = org
            on.dirty = false
            on
        }
        def unapply(on: OrgNode): Option[String] = Some(on.org)
    }
    class ClassNode(val clazz: Class[_], orgNode: OrgNode) extends Node {
        parent = orgNode
        override def toString = clazz.getName
        lazy val objType = ObjType(clazz)
        def org: String = parent.asInstanceOf[OrgNode].org

        var loaded = false
        def ensureLoaded: Unit = {
            if (loaded == false) {
                parent.parent.asInstanceOf[VerNode].conn.withConnection {c => PrmObject.getObjKeys(c, parent.asInstanceOf[OrgNode].org, clazz)} match {
                    case Right(keys) =>
                        keys.foreach {key => 
                            add(ObjNode(new PrmObject(parent.asInstanceOf[OrgNode].org, clazz, key), this))
                                  }
                    case Left(_) =>
                 }
                loaded = true
            }
        }

        override def flushChanges: Unit = {
            val conn = parent.parent.asInstanceOf[VerNode].conn
            doDelete(conn)
            val task = new MyWorker("update class[" + clazz + "] changes") {
                override def doInBackground: Unit = {
                    var i = 1
                    try {
                        setProgress(0)
                        for (n <- children; if n.dirty) {
                            if (isCancelled) throw new IllegalArgumentException("cancelled")
                            n.asInstanceOf[ObjNode].flushChanges
                            i += 1
                            setProgress(math.min(i*100/children.size, 100))
                        }
                    }
                    catch {case e: Throwable => }
                }
            }
            task.execute
            dirty = false
        }
        def doDelete(conn: JdbcConnection): Unit = {
            val task = new MyWorker("delete class[" + clazz + "]") {
                override def doInBackground: Unit = {
                    var i = 1
                    try {
                        setProgress(0)
                        for (n <- deleteQueue) {
                            if (isCancelled) throw new IllegalArgumentException("cancelled")
                            conn.withConnection {c => PrmObject.delete(c, n.asInstanceOf[ObjNode].prmObject)}
                            i += 1
                            setProgress(math.min(i*100/deleteQueue.size, 100))
                        }
                    }
                    catch {case e: Throwable => }
                    deleteQueue.remove(0, i-1)
                }
            }
            task.execute
        }

        def removeAll: Unit = {
            ensureLoaded
            children.foreach {n => deleteQueue += n.asInstanceOf[ObjNode]}
            children.reduceToSize(0)
            dirty = true
        }
    }
    object ClassNode {
        def apply(clazz: Class[_], parent: OrgNode) = new ClassNode(clazz, parent)
        def unapply(cn: ClassNode): Option[Class[_]] = Some(cn.clazz)
     }
    trait EditLabelTrait {
        def editLabel: String
    }
    trait ValueTrait {node: Node =>
        val objType: ObjType
        def valueOf: AnyRef
        def update(value: AnyRef): Unit

        def cloneFrom(other: ValueTrait): Unit = {
            removeAll
            update(objType.clone(other.valueOf, other.objType))
            node.dirty = true
            createChildNodes
        }
        def removeAll: Unit = {}
        def createChildNodes: Unit = {}
    }
    trait CompoundTrait extends ValueTrait {node: Node =>
        override def removeAll: Unit = node.children.reduceToSize(0)
        override def createChildNodes: Unit = {
            if (valueOf != null)
                FieldCache.getFieldWrappers(objType.clazz).foreach {fw => node.add(FieldNode(fw, node))}
        }
    }
    trait SetTrait extends ValueTrait {node: Node =>
        def valueType: ObjType = objType.asInstanceOf[JavaSet].valueType

        override def removeAll: Unit = node.children.reduceToSize(0)
        override def createChildNodes: Unit = {
            val s = valueOf
            if (s != null) {
                for (v <- s.asInstanceOf[java.util.Set[AnyRef]])
                    node.add(SetEntryNode(v, valueType, node))
            }
        }
    }
    trait MapTrait extends ValueTrait {node: Node =>
        def keyType: ObjType = objType.asInstanceOf[JavaMap].keyType
        def valueType: ObjType = objType.asInstanceOf[JavaMap].valueType

        override def removeAll: Unit = node.children.reduceToSize(0)
        override def createChildNodes: Unit = {
            val m = valueOf
            if (m != null) {
                for ((k, v) <- m.asInstanceOf[java.util.Map[AnyRef, AnyRef]])
                    node.add(MapEntryNode(k, valueType, node))
            }
        }
    }
    trait ListTrait extends ValueTrait {node: Node =>
        def valueType: ObjType = objType.asInstanceOf[JavaList].valueType

        override def removeAll: Unit = node.children.reduceToSize(0)
        override def createChildNodes: Unit = {
            val l = valueOf
            if (l != null) {
                for (index <- 0 to l.asInstanceOf[java.util.List[AnyRef]].size-1)
                    node.add(ListEntryNode(index, valueType, node))
            }
        }

        def relocate(from: Int, to: Int): Unit = {
            val l = valueOf.asInstanceOf[java.util.List[AnyRef]]
            val dstIndex = 
                if (from < to)
                    to - 1
                else
                    to
            val v = l.remove(from)
            l.add(dstIndex, v)
            // refresh index
            for ((ln, index) <- node.children.zipWithIndex)
                ln.asInstanceOf[ListEntryNode].index = index
        }
    }
    class ObjNode(val prmObject: PrmObject, classNode: ClassNode) extends Node with ValueTrait with CompoundTrait {
        parent = classNode
        prmObject.org = parent.asInstanceOf[ClassNode].org

        override def toString = prmObject.paramKey

        def obj: AnyRef = prmObject.paramObject
        override def valueOf: AnyRef = prmObject.paramObject
        override def update(other: AnyRef): Unit = prmObject.paramObject = other
        lazy val objType = parent.asInstanceOf[ClassNode].objType

        var loaded = false
        var newBorn = false

        def ensureLoaded: Unit = {
            if (loaded == false) {
                parent.parent.parent.asInstanceOf[VerNode].conn.withConnection {c => PrmObject.getObj(c, prmObject.org, prmObject.paramClass, prmObject.paramKey)} match {
                    case Right(other) =>
                        prmObject.copyFrom(other)
                        createChildNodes
                        loaded = true
                    case Left(_) =>
                }
            }
        }

        var oldOrg: String = null
        var oldKey: String = null

        def org = prmObject.org
        def org_=(org: String) {
            if (oldOrg != prmObject.org) {
                oldOrg = prmObject.org
                prmObject.org = org
                dirty = true
            }
        }

        def key = prmObject.paramKey
        def key_=(key: String) {
            if (oldKey != prmObject.paramKey) {
                oldKey = prmObject.paramKey
                prmObject.paramKey = key
                dirty = true
            }
        }

        override def flushChanges: Unit = {
            val conn = parent.parent.parent.asInstanceOf[VerNode].conn
            if (newBorn == true) {
                conn.withConnection {c => PrmObject.insert(c, prmObject)}
            }
            else {
                if ((oldKey != null) || (oldOrg != null)) {
                    val obj = new PrmObject(if (oldOrg != null) oldOrg else prmObject.org,
                                            prmObject.paramClass,
                                            if (oldKey != null) oldKey else prmObject.paramKey)
                    conn.withConnection {c => PrmObject.delete(c, obj)}
                    conn.withConnection {c => PrmObject.insert(c, prmObject)}
                }
                else {
                    conn.withConnection {c => PrmObject.update(c, prmObject)}
                }
            }
            dirty = false
            super.flushChanges
        }
    }
    object ObjNode {
        def apply(prmObject: PrmObject, parent: ClassNode) = new ObjNode(prmObject, parent)
        def apply(other: ObjNode, parent: ClassNode) = {
            val dstObj = new PrmObject(0, parent.org, parent.clazz, other.key, parent.objType.clone(other.valueOf, other.objType), 0, null, null)
            val on = new ObjNode(dstObj, parent)
            on.loaded = true
            on.newBorn = true
            on.dirty = true
            on.createChildNodes
            on
        }
        def unapply(on: ObjNode): Option[AnyRef] = Some(on.obj)
    }
    class FieldNode(val fw: FieldCache.FieldWrapper, pt: Node) extends Node with ValueTrait with EditLabelTrait {
        parent = pt
        val fd: java.lang.reflect.Field = fw.field
        val objType: ObjType = fw.objType
        val parentObj: AnyRef = parent.asInstanceOf[ValueTrait].valueOf
        if (valueOf != null)
            createChildNodes

        val propertyInfo: Option[PropertyInfo] = fw.propertyInfo
        override def toString = editLabel + objType.format(fd.get(parentObj))
        override def valueOf: AnyRef = fd.get(parentObj).asInstanceOf[AnyRef]
        override def editLabel: String = fd.getName + ":"
        override def update(value: AnyRef) { fd.set(parentObj, value) }
    }
    object FieldNode {
        def apply(fd: java.lang.reflect.Field, node: Node): FieldNode = apply(FieldCache.getFieldWrapper(fd.getDeclaringClass, fd.getName).get, node)
        def apply(fw: FieldCache.FieldWrapper, node: Node): FieldNode = {
            fw.objType match {
                case JavaSet(_) => new FieldNode(fw, node) with SetTrait
                case JavaMap(_) => new FieldNode(fw, node) with MapTrait
                case JavaList(_) => new FieldNode(fw, node) with ListTrait
                case JavaCompound(_) => new FieldNode(fw, node) with CompoundTrait
                case _ => new FieldNode(fw, node)
            }
        }
        def unapply(fn: FieldNode): Option[FieldCache.FieldWrapper] = Some(fn.fw)
    }
    class SetEntryNode(val key: AnyRef, val objType: ObjType, pt: Node) extends Node with ValueTrait with EditLabelTrait {
        parent = pt
        lazy val parentSet = parent.asInstanceOf[ValueTrait].valueOf.asInstanceOf[java.util.Set[AnyRef]]
        if (valueOf != null)
            createChildNodes

        override def toString = editLabel + objType.format(key)
        override def valueOf: AnyRef = key
        override def editLabel: String = key + "="
        override def update(v: AnyRef) { parentSet.add(v) }
    }
    object SetEntryNode {
        def apply(key: AnyRef, objType: ObjType, node: Node): SetEntryNode = 
            objType match {
                case JavaSet(_) => new SetEntryNode(key, objType, node) with SetTrait
                case JavaMap(_) => new SetEntryNode(key, objType, node) with MapTrait
                case JavaList(_) => new SetEntryNode(key, objType, node) with ListTrait
                case JavaCompound(_) => new SetEntryNode(key, objType, node) with CompoundTrait
                case _ => new SetEntryNode(key, objType, node)
            }
        def unapply(mn: MapEntryNode): Option[AnyRef] = Some(mn.key)
    }
    class MapEntryNode(var key: AnyRef, val objType: ObjType, pt: Node) extends Node with ValueTrait with EditLabelTrait {
        parent = pt
        lazy val parentMap = parent.asInstanceOf[ValueTrait].valueOf.asInstanceOf[java.util.Map[AnyRef, AnyRef]]
        if (valueOf != null)
            createChildNodes

        override def toString = editLabel + objType.format(parentMap.get(key))
        override def valueOf: AnyRef = parentMap.get(key).asInstanceOf[AnyRef]
        override def editLabel: String = key + "="
        override def update(v: AnyRef) { parentMap.put(key, v) }
    }
    object MapEntryNode {
        def apply(key: AnyRef, objType: ObjType, node: Node): MapEntryNode = 
            objType match {
                case JavaSet(_) => new MapEntryNode(key, objType, node) with SetTrait
                case JavaMap(_) => new MapEntryNode(key, objType, node) with MapTrait
                case JavaList(_) => new MapEntryNode(key, objType, node) with ListTrait
                case JavaCompound(_) => new MapEntryNode(key, objType, node) with CompoundTrait
                case _ => new MapEntryNode(key, objType, node)
            }
        def unapply(mn: MapEntryNode): Option[AnyRef] = Some(mn.key)
    }
    class ListEntryNode(var index: Int, val objType: ObjType, pt: Node) extends Node with ValueTrait with EditLabelTrait {
        parent = pt
        lazy val parentList = parent.asInstanceOf[ValueTrait].valueOf.asInstanceOf[java.util.List[AnyRef]]
        if (valueOf != null)
            createChildNodes

        override def toString: String = editLabel + objType.format(parentList.get(index))
        override def valueOf: AnyRef = parentList.get(index).asInstanceOf[AnyRef]
        override def editLabel: String = index + "="
        override def update(v: AnyRef) { parentList.set(index, v) }
    }
    object ListEntryNode {
        def apply(index: Int, objType: ObjType, node: Node): ListEntryNode =
            objType match {
                case JavaSet(_) => new ListEntryNode(index, objType, node) with SetTrait
                case JavaMap(_) => new ListEntryNode(index, objType, node) with MapTrait
                case JavaList(_) => new ListEntryNode(index, objType, node) with ListTrait
                case JavaCompound(_) => new ListEntryNode(index, objType, node) with CompoundTrait
                case _ => new ListEntryNode(index, objType, node)
            }
    }
}

