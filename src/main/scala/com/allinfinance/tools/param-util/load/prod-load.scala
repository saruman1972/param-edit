package com.allinfinance.tools.param.util

import org.apache.poi.ss.usermodel.{Sheet, Row, Cell, Workbook}
import org.apache.poi.xssf.usermodel._
import java.io.FileInputStream
import scala.collection.JavaConversions._

import ObjTreeNode.{VerConfigs, VerNode, MyWorker}

object XlsParsers {
    import JsonDef._
    import ObjDef._
    import Expr._

    sealed trait Token
    case class TagObject(name: String, keys: List[String]) extends Token
    case class TagCompField(name: String) extends Token
    case class TagEnd(name: String) extends Token
    case class TagList() extends Token
    case class TagMap() extends Token
    case class TagMapList() extends Token
    case class TagFlatMap() extends Token
    case class TagMapMap() extends Token
    case class TagCells(value: List[Option[String]]) extends Token
    case class TagHeader(value: List[Option[String]]) extends Token

    sealed trait Header2
    case class SimpleHeader(value: Option[String]) extends Header2
    case class SpanHeader(values: List[Option[String]], col: Option[String], firstCol: Int) extends Header2
    case class TagHeader2(value: List[Header2]) extends Token

    def cellValue(cell: Cell): Option[String] = {
        try {
            cell.setCellType(Cell.CELL_TYPE_STRING)
            cell.getStringCellValue match {
                case "" => None
                case v => Some(v.dropWhile(_.isWhitespace))
            }
        }
        catch {
            case _: Throwable => try {
                Some("%f".format(cell.getNumericCellValue))
            }
            catch {
                case _: Throwable => None
            }
        }
    }

    def comments(cell: Cell): Map[String,String] = {
        val Pattern = """(.*)=(.*)""".r
        cell.getCellComment match {
            case null => Map.empty[String,String]
            case cmt =>
                cmt.getString.getString.split("\n").foldLeft(Map.empty[String,String]) {
                    (m, x) => x match {
                        case Pattern(key,value) => m + (key -> value)
                        case _ => m
                    }
                }
        }
    }

    def realCellValue(cell: Cell): Option[String] = comments(cell).get("fd") match {
        case v @ Some(_) => v
        case _ => cellValue(cell)
    }

    def tokens(sheet: Sheet): List[Token] = {
        import scala.collection.immutable.Stack
        def tagToEnd(token: Token): TagEnd = token match {
            case TagObject(n, _) => TagEnd(n)
            case TagCompField(n) => TagEnd(n)
            case _ => TagEnd("error")
        }
        def tagStackToEnd(stack: Stack[Token]): List[Token] = stack.toList.map {tagToEnd}

        val t =
            sheet.iterator.foldLeft((List.empty[Token], TagEnd(""): Token, Stack.empty[Token])) {
                case ((acc,prev_tag,end_stack), row) =>
                    val cells = row.cellIterator.toList
                    if (cells.length > 0) {
                        val cmts = comments(cells.head)
                        if (cmts.contains("class")) { // restart an object, clear all previous end tag
                            val keys = cmts.get("keys") match {
                                case Some(ks) => ks.split('|').toList
                                case _ => List.empty[String]
                            }
                            val tag = TagObject(cmts("class"), keys)
                            if (cmts.get("type") == Some("list"))
                                (acc ++ tagStackToEnd(end_stack) :+ tag :+ TagList(), TagList(), Stack[Token](tag))
                            else
                                (acc ++ tagStackToEnd(end_stack) :+ tag, tag, Stack[Token](tag))
                        }
                        else if (cmts.contains("cfd")) {
                            val tag = TagCompField(cmts("cfd"))
                            val coll = cmts.get("type") match {
                                case Some("list")    => TagList()
                                case Some("map")     => TagMap()
                                case Some("maplist") => TagMapList()
                                case Some("flatmap") => TagFlatMap()
                                case Some("mapmap")  => TagMapMap()
                                case _ => TagEnd("") // it's a hack
                            }
                            val end_tag =
                                if (end_stack.isEmpty)
                                    ""
                                else
                                    cmts.getOrElse("end", end_stack.top match {case TagCompField(n) => n case _ => ""})
                            val top_idx = end_stack.indexWhere {t => t match {
                                case TagObject(n, _) => n == end_tag
                                case TagCompField(n) => n == end_tag
                                case _ => false
                            }
                                                            }
                            val (ends, new_stack) = end_stack.splitAt(top_idx + 1)

                            coll match {
                                case TagEnd(_) => (acc ++ tagStackToEnd(ends) :+ tag, coll, new_stack.push(tag))
                                case _ => (acc ++ tagStackToEnd(ends) :+ tag :+ coll, coll, new_stack.push(tag))
                            }
                        }
                        else {
                            prev_tag match {
                                case TagMap() | TagMapList() | TagMapMap() =>
                                    val tag = TagHeader(cells.map {realCellValue})
                                    (acc :+ tag, tag, end_stack)
                                case TagList() =>
                                    val regions = (0 to (sheet.getNumMergedRegions - 1)).map {sheet.getMergedRegion}
                                    val horizontalSpans = regions.filter {r => (row.getRowNum == r.getFirstRow) && (r.getFirstRow == r.getLastRow)} map {r => (r.getFirstColumn, r.getLastColumn)}
                                    val verticalSpans = regions.filter {r => (row.getRowNum == r.getFirstRow) && (r.getFirstColumn == r.getLastColumn)}

                                    if ((verticalSpans.size > 0) && (horizontalSpans.size > 0)) {
                                        val cells2 = sheet.getRow(row.getRowNum+1).cellIterator.toList
                                        val tag = TagHeader2(cells2.map {realCellValue} .zipWithIndex.foldLeft (scala.collection.mutable.ArrayBuffer[Header2]()) {
                                            case (acc, (cell,idx)) => horizontalSpans.find {case (firstCol,lastCol) => (idx >= firstCol) && (idx <= lastCol)} match {
                                                case Some((firstCol,_)) => acc.last match {
                                                    case SimpleHeader(_) => acc :+ SpanHeader(List(cell), realCellValue(cells(firstCol)), firstCol)
                                                    case SpanHeader(values, col, fc) =>
                                                        if (fc == firstCol) {
                                                            acc(acc.size - 1) = SpanHeader(values :+ cell, col, fc)
                                                            acc
                                                        }
                                                        else
                                                            acc :+ SpanHeader(List(cell), realCellValue(cells(firstCol)), firstCol)
                                                }
                                                case None => acc :+ SimpleHeader(realCellValue(cells(idx)))  // 普通cell，使用前一行的header
                                            }
                                        }.toList)
                                        (acc :+ tag, tag, end_stack)
                                    }
                                    else {
                                        val tag = TagHeader(cells.map {realCellValue})
                                        (acc :+ tag, tag, end_stack)
                                    }
                                case TagHeader2(_) => (acc, TagEnd(""), end_stack) // header2已经在TagList()里面处理过，这里直接跳过当前行
                                case _ =>
                                    val tag = TagCells(cells.map {realCellValue}) 
                                    (acc :+ tag, tag, end_stack)
                            }
                        }
                    }
                    else
                        (acc, prev_tag, end_stack)
            }

        t._1 ++ tagStackToEnd(t._3)
    }

    case class |+|[+A, +B](_1: A, _2: B) {
        override def toString = "(" + _1 + " |+| " + _2 + ")"
    }

    trait XlsParser[+A] {self =>
        def run(input: Seq[Token]): Result[A]
        def map[B](f: A => B): XlsParser[B] = XlsParser((input: Seq[Token]) => {
            self.run(input) match {
                case Success(x, n) => Success(f(x), n)
                case failure @ Failure(_, _) => failure
            }
        })
        def flatMap[B](f: A => XlsParser[B]): XlsParser[B] = XlsParser((input: Seq[Token]) => {
            self.run(input) match {
                case Success(x, n) =>
                    f(x).run(input.drop(n)) match {
                        case Success(x1, n1) => Success(x1, n+n1)
                        case failure => failure
                    }
                case failure @ Failure(_, _) => failure
            }
        })

        def |+|[B](p0: => XlsParser[B]): XlsParser[|+|[A,B]] = {
            lazy val p = p0
            for (a <- this; b <- p) yield new |+|(a,b)
        }

        def ^^[B](f: A => B): XlsParser[B] = map(f)

        def | [B >: A](p0: => XlsParser[B]): XlsParser[B] = {
            lazy val p = p0
            XlsParser((input: Seq[Token]) => {
                self.run(input) match {
                    case Success(x, n) => Success(x, n)
                    case _ => p.run(input)
                }
            })
        }
    }
    trait Result[+A]
    case class Success[+A](get: A, tokensConsumed: Int) extends Result[A]
    case class Failure(get: String, isAttemped: Boolean) extends Result[Nothing]

    object XlsParser {
        def apply[A](f: Seq[Token] => Result[A]): XlsParser[A] = new XlsParser[A] {
            def run(input: Seq[Token]): Result[A] = f(input)
        }
    }

    private def pairs(cells: List[Option[String]]): List[(String, JsonString)] = 
        filterNone((for (f :: v :: _ <- cells.sliding(2,2))
                       yield (f, v) match {
                           case (Some(f1), Some(v1)) => Some((f1, JsonString(v1)))
                           case _ => None    
                       }).toList)

    def fieldRow: XlsParser[List[(String, JsonString)]] = cellsTAG ^^ {
        case cells => pairs(cells)
    }

    def mapEntryRow: XlsParser[List[(JsonString, JsonString)]] = cellsTAG ^^ {
        case cells => pairs(cells).map {case (k,v) => JsonString(k) -> v}
    }

    // def sequence[A](as: List[Option[A]]): Option[List[A]] = {
    //     as.foldLeft(Some(List.empty[A]): Option[List[A]]) {(acc, a) => acc flatMap (os => a.map(_ :: os))} .map(_.reverse)
    // }

    def unit[A](a: A): XlsParser[A] = XlsParser(_ => Success(a,0))
    def unit[A](oa: Option[A]): XlsParser[A] = oa match {
        case Some(a) => XlsParser(_ => Success(a, 0))
        case None => XlsParser(_ => Failure("", false))
    }

    def headerTAG: XlsParser[List[Option[String]]] = XlsParser((input: Seq[Token]) => input.headOption match {
        case Some(TagHeader(hl)) => Success(hl, 1)
        case Some(tag) => Failure("collection header expected, but [" + tag + "] was given", false)
        case None => Failure("end of stream - in headerTAG", false)
    })

    def headerTAG2: XlsParser[List[Header2]] = XlsParser((input: Seq[Token]) => input.headOption match {
        case Some(TagHeader2(hl)) => Success(hl, 1)
        case Some(tag) => Failure("collection header2 expected, but [" + tag + "] was given", false)
        case None => Failure("end of stream - in headerTAG2", false)
    })

    def cellsTAG: XlsParser[List[Option[String]]] = XlsParser((input: Seq[Token]) => input.headOption match {
        case Some(TagCells(cl)) => Success(cl, 1)
        case Some(tag) => Failure("cells expected, but [" + tag + "] was given", false)
        case None => Failure("end of stream - in cellsTAG", false)
    })

    def objectTAG: XlsParser[TagObject] = XlsParser((input: Seq[Token]) => input.headOption match {
        case Some(o: TagObject) => Success(o, 1)
        case Some(tag) => Failure("object expected, but [" + tag + "] was given", false)
        case None => Failure("end of stream - in objectTAG", false)
    })

    def compoundTAG: XlsParser[TagCompField] = XlsParser((input: Seq[Token]) => input.headOption match {
        case Some(cfd: TagCompField) => Success(cfd, 1)
        case Some(tag) => Failure("compound field expected, but [" + tag + "] was given", false)
        case None => Failure("end of stream - in compoundTAG", false)
    })

    def listTAG: XlsParser[TagList] = XlsParser ((input: Seq[Token]) => input.headOption match {
        case Some(l: TagList) => Success(l, 1)
        case Some(tag) => Failure("list expected, but [" + tag + "] was given", false)
        case None => Failure("end of stream - in listTAG", false)
    })

    def mapTAG: XlsParser[TagMap] = XlsParser ((input: Seq[Token]) => input.headOption match {
        case Some(m: TagMap) => Success(m, 1)
        case Some(tag) => Failure("map expected, but [" + tag + "] was given", false)
        case None => Failure("end of stream - in mapTAG", false)
    })

    def mapListTAG: XlsParser[TagMapList] = XlsParser ((input: Seq[Token]) => input.headOption match {
        case Some(ml: TagMapList) => Success(ml, 1)
        case Some(tag) => Failure("mapList expected, but [" + tag + "] was given", false)
        case None => Failure("end of stream - in mapListTAG", false)
    })

    def flatMapTAG: XlsParser[TagFlatMap] = XlsParser ((input: Seq[Token]) => input.headOption match {
        case Some(fm: TagFlatMap) => Success(fm, 1)
        case Some(tag) => Failure("flatMap expected, but [" + tag + "] was given", false)
        case None => Failure("end of stream - in flatMapTAG", false)
    })

    def mapMapTAG: XlsParser[TagMapMap] = XlsParser ((input: Seq[Token]) => input.headOption match {
        case Some(mm: TagMapMap) => Success(mm, 1)
        case Some(tag) => Failure("mapMap expected, but [" + tag + "] was given", false)
        case None => Failure("end of stream - in mapMapTAG", false)
    })

    def endTAG: XlsParser[TagEnd] = XlsParser ((input: Seq[Token]) => input.headOption match {
        case Some(end: TagEnd) => Success(end, 1)
        case Some(tag) => Failure("end expected, but [" + tag + "] was given", false)
        case None => Failure("end of stream - in endTAG", false)
    })

    def many0[A](p: => XlsParser[A]): XlsParser[List[A]] = XlsParser((input: Seq[Token]) => {
        p.run(input) match {
            case Success(x, n) =>
                val Success(xs, n1) = many0(p).run(input.drop(n))
                Success(x :: xs, n+n1) : Result[List[A]]
            case f => Success(List.empty[A], 0) : Result[List[A]]
        }
    })

    def many[A](p: => XlsParser[A]): XlsParser[List[A]] =(p |+| many0(p)) ^^ {
        case p |+| ps => p :: ps
    }

    def removeTailNone[A](oas: List[Option[A]]): List[Option[A]] = oas.reverse.dropWhile {_.isEmpty} .reverse
    def filterNone[A](oas: List[Option[A]]): List[A] = {
        oas.foldLeft(List.empty[A]) {case (acc, oa) =>
            oa match {
                case Some(a) => acc :+ a
                case None => acc
            }
        }
    }
    def collection: XlsParser[Json] = clistWithNestedCompound | clist | cmap | cflatmap | cmaplist | cmapmap

    def clist: XlsParser[JsonList] = (listTAG |+| headerTAG |+| many(cellsTAG)) ^^ {
        case _ |+| header |+| entriesList => removeTailNone(header).length match {
            case 1 => // primitive list
                JsonList(filterNone(entriesList.map {
                                        case Some(v) :: _ => Some(JsonString(v))
                                        case _ => None
                                    })
                     )
            case _ => // object list
                JsonList(entriesList.map {
                            entries =>
                                JsonCompound(Map() ++
                                             filterNone(header.zip(entries).map {
                                                 case (Some(col), Some(v)) => Some(col -> JsonString(v))
                                                 case _ => None
                                             })
                                         )
                        })
        }
    }

    def nestedList(header: List[Option[String]], entries: List[Option[String]]): JsonList = {
        header.find(_.isEmpty) match {
            case Some(_) =>
                header.tail.find {_ != header.head} match {
                    case Some(_) => nestedCompoundList(header, entries)
                    case None => nestedPrimitiveList(entries)
                }
            case None => nestedPrimitiveList(entries)
        }
    }
    def nestedPrimitiveList(entries: List[Option[String]]): JsonList = {
        JsonList(entries.foldLeft(List.empty[Json]) {
            case (acc, Some(x)) => acc :+ JsonString(x)
            case (acc, None) => acc :+ JsonNull
        })
    }
    def nestedCompoundList(header: List[Option[String]], entries: List[Option[String]]): JsonList = {
        val (objs,cols) = header.zip(entries).foldLeft((List.empty[Json], Map.empty[String,Json])) {
            case ((acc,cols), (Some(col),entry)) =>
                cols.contains(col) match {
                    case true => // find duplicate, start a new entry
                        (acc :+ JsonCompound(cols), Map.empty[String,Json])
                    case false => entry match {
                        case Some(x) => (acc, cols + (col -> JsonString(x)))
                        case None => (acc, cols + (col -> JsonNull))
                    }
                }
        }
        JsonList(objs :+ JsonCompound(cols))
    }

    def clistWithNestedCompound: XlsParser[JsonList] = (listTAG |+| headerTAG2 |+| many(cellsTAG)) ^^ {
        case _ |+| header |+| entriesList => header.length match {
            case 1 => // list of primitive list with only 1 element, not supported, should never happen
                throw new IllegalArgumentException("header has no span[" + headerTAG2 + "]")
            case _ => // compound list
                JsonList(entriesList.map {
                            entries =>
                                JsonCompound(Map() ++
                                             filterNone(header.foldLeft((List.empty[Option[(String,Json)]], 0)) {
                                                 case ((acc,idx), h) => h match {
                                                     case SimpleHeader(Some(col)) => (acc :+ entries(idx).map (v => (col -> JsonString(v))), idx+1)
                                                     case SimpleHeader(None) => (acc, idx+1)
                                                     case SpanHeader(hs, Some(col), _) => (acc :+ Some(col -> nestedCompoundList(hs, entries.slice(idx,idx+hs.size))), idx+hs.size)
                                                     case SpanHeader(hs, None, _) => (acc, idx+hs.size)
                                                 }
                                             }._1)
                                         )
                        })
        }
    }

    // def cmapWithNestedCompound: XlsParser[JsonMap] = (listTAG |+| headerTAG2 |+| many(cellsTAG)) ^^ {
    // }

    // def cmapListWithNestedCompound: XlsParser[JsonMap] = (listTAG |+| headerTAG2 |+| many(cellsTAG)) ^^ {
    // }

    private def genPrimitiveJsonMapEntry(entries: List[Option[String]]): Option[(JsonString,JsonString)] =
        entries match {
            case Some(k) :: Some(v) :: _ => Some(JsonString(k) -> JsonString(v))
            case _ => None
        }
    private def genObjectJsonMapEntry(header: List[Option[String]], entries: List[Option[String]]): Option[(JsonString,Json)] =
        entries match {
            case Some(k) :: _ =>
                val v = JsonCompound(Map() ++
                                     filterNone(header.tail.zip(entries.tail).map {
                                         case (Some(col), Some(v)) => Some(col -> JsonString(v))
                                         case _ => None
                                     })
                                 )
                Some(JsonString(k) -> v)
            case _ => None
        }
    def cmap: XlsParser[JsonMap] = (mapTAG |+| headerTAG |+| many(cellsTAG)) ^^ {
        case _ |+| header |+| entriesList => removeTailNone(header).length match {
            case 0 | 1 => throw new IllegalArgumentException("map should contain 1 key column and at least 1 value column")
            case 2 => // primitive map
                JsonMap(Map() ++ filterNone(entriesList.map {genPrimitiveJsonMapEntry}))
            case _ => // object map
                JsonMap(Map() ++ filterNone(entriesList.map {genObjectJsonMapEntry(header, _)}))
        }
    }

    def cflatmap: XlsParser[JsonMap] = (flatMapTAG |+| many(mapEntryRow)) ^^ {
        case _ |+| entriesList =>
            JsonMap(entriesList.foldLeft(Map.empty[Json, Json]) {(acc, m) => acc ++ m})
    }

    def cmaplist: XlsParser[JsonMap] = (mapListTAG |+| headerTAG |+| many(cellsTAG)) ^^ {
        case _ |+| header |+| entriesList => removeTailNone(header).length match {
            case 0 | 1 => throw new IllegalArgumentException("mapList should contain 1 key column and at least 1 value column")
            case 2 => // primitive mapList
                val kvs: List[(JsonString,JsonString)] = filterNone(entriesList.map {genPrimitiveJsonMapEntry})
                val m = kvs.foldLeft(Map.empty[Json, Json]) {
                    case (acc, (k,v)) => acc + (k -> JsonList(acc.getOrElse(k, JsonList(List.empty[Json])).asInstanceOf[JsonList].elmt :+ v))
                }
                JsonMap(m)
            case _ => // object mapList
                val kvs: List[(JsonString,Json)] = filterNone(entriesList.map {genObjectJsonMapEntry(header, _)})
                val m = kvs.foldLeft(Map.empty[Json, Json]) {
                    case (acc, (k,v)) => acc + (k -> JsonList(acc.getOrElse(k, JsonList(List.empty[Json])).asInstanceOf[JsonList].elmt :+ v))
                }
                JsonMap(m)
        }
    }

    private def genMapMapEntry(header: List[Option[String]], entries: List[Option[String]]): Option[(Json, Json)] = entries match {
        case Some(k1) :: _ =>
            val m = JsonMap(Map() ++
                            filterNone(header.tail.zip(entries.tail).map {
                                case (Some(k2), Some(v)) => Some(JsonString(k2) -> JsonString(v))
                                case _ => None
                            })
                        )
            Some(JsonString(k1), m)
        case _ => None
    }
    def cmapmap: XlsParser[JsonMap] = (mapMapTAG |+| headerTAG |+| many(cellsTAG)) ^^ {
        case _ |+| header |+| entriesList => header.length match {
            case 0 | 1 => throw new IllegalArgumentException("mapMap should contain 1 key column and at least 1 value column")
            case _ => // only support primitive mapMap
                JsonMap(Map() ++ filterNone(entriesList.map {genMapMapEntry(header, _)}))
        }
    }

    def compoundField: XlsParser[List[(String, Json)]] = ((compoundTAG |+| fields |+| endTAG) ^^ {
        case tag |+| fds |+| _ => List(tag.name -> JsonCompound(Map() ++ fds))
    }) | ((compoundTAG |+| collection |+| endTAG) ^^ {
        case tag |+| coll |+| _ => List(tag.name -> coll)
    })

    def fields: XlsParser[List[(String, Json)]] = many(fieldRow | compoundField) ^^ {
        case fds => fds.flatten
    }

    def objList: XlsParser[List[(TagObject, List[FVPair])]] = (objectTAG |+| listTAG |+| headerTAG |+| many(cellsTAG) |+| endTAG) ^^ {
        case tag |+| _ |+| header |+| entriesList |+| _ => entriesList.map {
            entries =>
                 val kvs = filterNone(header.zip(entries).map {
                     case (Some(col), Some(v)) => Some(FVPair(Field(List(Literal(col))), JsonString(v)))
                     case _ => None
                 })
                (tag, kvs)
        }
    } | (objectTAG |+| listTAG |+| headerTAG2 |+| many(cellsTAG) |+| endTAG) ^^ {
        case tag |+| _ |+| header |+| entriesList |+| _ => entriesList.map {
            entries =>
                val kvs = filterNone(header.foldLeft((List.empty[Option[(String,Json)]], 0)) {
                    case ((acc,idx), h) => h match {
                        case SimpleHeader(Some(col)) => (acc :+ entries(idx).map (v => (col -> JsonString(v))), idx+1)
                        case SimpleHeader(None) => (acc, idx+1)
                        case SpanHeader(hs, Some(col), _) => (acc :+ Some(col -> nestedCompoundList(hs, entries.slice(idx,idx+hs.size))), idx+hs.size)
                        case SpanHeader(hs, None, _) => (acc, idx+hs.size)
                    }
                }._1)
                (tag, kvs.map {case (col,value) => FVPair(Field(List(Literal(col))), value)})
        }
    }


    def prmObject: XlsParser[(TagObject, List[FVPair])] = (objectTAG |+| fields |+| endTAG) ^^ {
        case tag |+| fds |+| _ =>
            (tag, fds.map {case (f, v) => FVPair(Field(List(Literal(f))), v)})
    }

    def objects: XlsParser[List[(TagObject, List[FVPair])]] = many(prmObject) | objList

}

object ProductLoad {
    import XlsParsers._
    import Expr.{VersionClass, FVPair}
    import ObjDef.{ObjType}
    import com.thoughtworks.xstream.XStream
    import com.thoughtworks.xstream.io.xml.DomDriver

    val xstream = new XStream(new DomDriver)
    def genParamKey(obj: AnyRef, keys: List[String]): Either[String,String] = {
        val clazz = obj.getClass
        val errors = scala.collection.mutable.ArrayBuffer.empty[String]
        val paramKey = keys.map {
            fname =>
                FieldCache.get(clazz, fname) match {
                    case Some(fd) => fd.get(obj)
                    case _ =>
                        errors.append("invalid key field[" + fname + "]")
                        ""
                }
        } .mkString("|")
        if (errors.length > 0)
            Left[String,String](errors.mkString("\n"))
        else if (paramKey.length == 0)
            Right[String,String]("*")
        else
            Right[String,String](paramKey)
    }

    def printStackTrace(e: java.lang.Throwable): String = {
        val sw = new java.io.StringWriter
        e.printStackTrace(new java.io.PrintWriter(sw))
        sw.toString
    }
    def genObject(org: String, version: String, cname: String, keys: List[String], fvs: List[FVPair]): Either[String, (PrmObject,String)] = {
        try {
            val vc = VersionClass(version, cname)
            val obj = ObjType(vc.clazz.get).newInstance
            val errors = scala.collection.mutable.ArrayBuffer.empty[String]
            for (pair <- fvs) {
                try {
                    pair.typeChecked(vc)
                    pair.eval(obj)
                }
                catch {
                    case e: Throwable => errors.append(printStackTrace(e))
                }
            }
// println(xstream.toXML(obj))
            genParamKey(obj, keys) match {
                case Left(pke) =>
                    errors.append(pke)
                    Left[String, (PrmObject,String)](errors.mkString("\n"))
                case Right(pk) =>
                    if (errors.length > 0)
                        Left[String, (PrmObject,String)](errors.mkString("\n"))
                    else {
                        val sql = "insert_update {" + version + "}." + cname + " values ORG='" + org + "', PARAM_KEY='" + pk + "', " + fvs.mkString(",")
                        Right[String, (PrmObject,String)](new PrmObject(org, vc.clazz.get, pk), sql)
                    }
            }
        }
        catch {
            case e: Throwable => Left[String, (PrmObject,String)](printStackTrace(e))
        }
    }

    def genLoadStmts(excelName: String, org: String, version: String): Either[String,List[String]] = {
        val ws = new XSSFWorkbook(new FileInputStream(excelName))
        val sheets = 0.to(ws.getNumberOfSheets-1).toList.map {idx => ws.getSheetAt(idx).getSheetName} .filter {name => name.startsWith("导入-")}
println(sheets)
        sheets.map {
            sheet => 
println(tokens(ws.getSheet(sheet)))
objects.run(tokens(ws.getSheet(sheet))) match {
                case Success(os, _) =>
                    os.map {
                        case (tag, fvs) =>
//println(fvs)
                            genObject(org, version, tag.name, tag.keys, fvs)
                    }
                case Failure(e, _) =>
                    List(Left[String, (PrmObject,String)](e))
            }
        }.flatten.foldLeft (Right[String,List[String]](List.empty[String]): Either[String,List[String]]) {
            case (Left(errs), Left(err)) => Left[String,List[String]](errs + "\n" + err)
            case (acc @ Left(_), Right(_)) => acc
            case (Right(sqls), Left(err)) => Left[String,List[String]](err)
            case (Right(sqls), Right((prmObj,sql))) => Right[String,List[String]](sqls :+ sql)
        }
    }
}

