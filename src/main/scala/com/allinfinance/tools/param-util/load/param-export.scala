package com.allinfinance.tools.param.util

import org.apache.poi.ss.usermodel.{Sheet, Row, Cell, Workbook, Font, CellStyle, Drawing, Comment}
import org.apache.poi.xssf.usermodel._
import org.apache.poi.hssf.util.{HSSFColor, CellRangeAddress}
import java.io.FileInputStream
import scala.collection.JavaConversions._

import ObjTreeNode.{VerConfigs, VerNode, OrgNode, ClassNode, ObjNode, MyWorker}
import ObjDef._

object ParamExport {
    case class SheetDesc(name: String, clazzes: Seq[(String,String,Boolean)])
    def export(path: String, org: String, verNode: VerNode, sheetDescs: Seq[SheetDesc])(callback: (SheetDesc,Int) => Unit): Unit = {
        val orgNode = OrgNode(org, verNode)
        val workbook = new XSSFWorkbook
        val styles = {
            val boldFont = workbook.createFont
            boldFont.setFontHeightInPoints(9)
            boldFont.setFontName("微软雅黑")
            boldFont.setBoldweight(Font.BOLDWEIGHT_BOLD)
            val boldRedFont = workbook.createFont
            boldRedFont.setFontHeightInPoints(9)
            boldRedFont.setColor(HSSFColor.RED.index)
            boldRedFont.setFontName("微软雅黑")
            boldRedFont.setBoldweight(Font.BOLDWEIGHT_BOLD)
            val normalFont = workbook.createFont
            normalFont.setFontHeightInPoints(9)
            normalFont.setFontName("微软雅黑")
            normalFont.setBoldweight(Font.BOLDWEIGHT_NORMAL)
            val styleClazz = workbook.createCellStyle
            styleClazz.setFont(boldRedFont)
            styleClazz.setBorderTop(CellStyle.BORDER_THIN)
            styleClazz.setBorderLeft(CellStyle.BORDER_THIN)
            styleClazz.setBorderBottom(CellStyle.BORDER_THIN)
            styleClazz.setBorderRight(CellStyle.BORDER_THIN)
            val styleCompoundFDName = workbook.createCellStyle
            styleCompoundFDName.setFont(boldRedFont)
            styleCompoundFDName.setAlignment(CellStyle.ALIGN_CENTER)
            styleCompoundFDName.setBorderTop(CellStyle.BORDER_THIN)
            styleCompoundFDName.setBorderLeft(CellStyle.BORDER_THIN)
            styleCompoundFDName.setBorderBottom(CellStyle.BORDER_THIN)
            styleCompoundFDName.setBorderRight(CellStyle.BORDER_THIN)
            val styleSimpleFDName = workbook.createCellStyle
            styleSimpleFDName.setFont(boldFont)
            styleSimpleFDName.setFillPattern(CellStyle.SOLID_FOREGROUND)
            styleSimpleFDName.setFillForegroundColor(HSSFColor.RED.index)
            styleSimpleFDName.setAlignment(CellStyle.ALIGN_RIGHT)
            styleSimpleFDName.setBorderTop(CellStyle.BORDER_THIN)
            styleSimpleFDName.setBorderLeft(CellStyle.BORDER_THIN)
            styleSimpleFDName.setBorderBottom(CellStyle.BORDER_THIN)
            styleSimpleFDName.setBorderRight(CellStyle.BORDER_THIN)
            val styleMapKey = workbook.createCellStyle
            styleMapKey.setFont(normalFont)
            styleMapKey.setFillPattern(CellStyle.SOLID_FOREGROUND)
            styleMapKey.setFillForegroundColor(HSSFColor.GREY_50_PERCENT.index)
            styleMapKey.setBorderTop(CellStyle.BORDER_THIN)
            styleMapKey.setBorderLeft(CellStyle.BORDER_THIN)
            styleMapKey.setBorderBottom(CellStyle.BORDER_THIN)
            styleMapKey.setBorderRight(CellStyle.BORDER_THIN)
            val styleFDValue = workbook.createCellStyle
            styleFDValue.setFont(normalFont)
            styleFDValue.setFillPattern(CellStyle.SOLID_FOREGROUND)
            styleFDValue.setFillForegroundColor(HSSFColor.BRIGHT_GREEN.index)
            styleFDValue.setBorderTop(CellStyle.BORDER_THIN)
            styleFDValue.setBorderLeft(CellStyle.BORDER_THIN)
            styleFDValue.setBorderBottom(CellStyle.BORDER_THIN)
            styleFDValue.setBorderRight(CellStyle.BORDER_THIN)

            Map("clazz" -> styleClazz, "compound" -> styleCompoundFDName, "name" -> styleSimpleFDName, "key" -> styleMapKey, "value" -> styleFDValue)
        }
        sheetDescs.zipWithIndex.foreach {
            case (sheetDesc,idx) =>
                val sheet = workbook.createSheet(sheetDesc.name)
println("sheet[" + sheetDesc.name + "]=" + sheet)
                val drawing = sheet.createDrawingPatriarch
                sheetDesc.clazzes.foreach {
                    case (clazz,desc,isList) =>
println("clazz[" + clazz + "]")
                        val clazzNode = ClassNode(Class.forName(clazz, true, verNode.classLoader), orgNode)
                        clazzNode.ensureLoaded
                        exportClazz(sheet, desc, clazzNode, isList, styles, drawing)
                        // add a blank line for clarity
                        val row = sheet.createRow(sheet.getPhysicalNumberOfRows)
                        row.createCell(0).setCellValue(" ")
                }

                val maxCol = sheet.rowIterator.toList.foldLeft(-1) {
                    case (col,row) => if (row.getLastCellNum > col) row.getLastCellNum else col
                }
                0.to(maxCol).foreach {sheet.autoSizeColumn}

                callback(sheetDesc,idx)
        }

        val os = new java.io.FileOutputStream(path)
        workbook.write(os)
        os.flush
        os.close
    }

    def exportClazz(sheet: Sheet, desc: String, cn: ClassNode, isList: Boolean, styles: Map[String, CellStyle], drawing: Drawing): Unit = {
        if (cn.children.size == 0) {
            // create a dummy object
            val prmObject = new PrmObject(cn.parent.asInstanceOf[OrgNode].org, cn.clazz, "")
            prmObject.paramObject = ObjType(cn.clazz).newInstance
            val on = ObjNode(prmObject, cn)
            on.createChildNodes
        }
        val objs = cn.children.map {
            x =>
                val on = x.asInstanceOf[ObjNode]
                on.ensureLoaded
                on.prmObject.paramObject
        }
        if (isList)
            exportObjectList(sheet, desc, objs, cn.clazz, styles, drawing)
        else {
            objs.foreach {
                x =>
                    exportObject(sheet, desc, x, cn.clazz, styles, drawing)
            }
        }
    }

    def generateSimpleName(row: Row, col: Int, name: String, styles: Map[String, CellStyle]): Unit = {
        val cellName = row.createCell(col)
        cellName.setCellType(Cell.CELL_TYPE_STRING)
        cellName.setCellValue(name)
        cellName.setCellStyle(styles("name"))
    }
    def generateFlatmapKeyCell(row: Row, col: Int, key: AnyRef, ot: ObjType, styles: Map[String, CellStyle]): Unit = {
        generateValueCell(row, col, key, ot, styles("name"))
    }
    def generateKeyCell(row: Row, col: Int, key: AnyRef, ot: ObjType, styles: Map[String, CellStyle]): Unit = {
        generateValueCell(row, col, key, ot, styles("key"))
    }
    def generateValueCell(row: Row, col: Int, value: AnyRef, ot: ObjType, styles: Map[String, CellStyle]): Unit = {
        generateValueCell(row, col, value, ot, styles("value"))
    }
    def generateHeader(row: Row, col: Int, value: AnyRef, ot: ObjType, styles: Map[String, CellStyle]): Unit = {
        generateValueCell(row, col, value, ot, styles("name"))
    }
    def generateValueCell(row: Row, col: Int, value: AnyRef, ot: ObjType, style: CellStyle): Unit = {
        val cellValue = row.createCell(col)
        cellValue.setCellType(Cell.CELL_TYPE_STRING)
        cellValue.setCellValue(value match {
            case null => ""
            case x => ot match {
                case JavaBoolean => if (x.asInstanceOf[java.lang.Boolean]) "是" else "否"
//                case enum: JavaEnum => enum.enumDescMap.get(x).get.desc
                case enum: JavaEnum => enum.enumDescMap.get(x) match {
                    case Some(e) => e.desc
                    case None =>
println(x)
println(enum.enumDescMap)
 x.toString
                }
                case JavaInteger | JavaBigDecimal => x.toString
                case _ => x.toString
            }
        })
        cellValue.setCellStyle(style)
    }
    def exportSimpleField(row: Row, idx: Int, obj: AnyRef, fw: FieldCache.FieldWrapper, styles: Map[String, CellStyle], drawing: Drawing): Unit = {
        generateSimpleName(row, idx, fw.chineseName, styles)
        generateValueCell(row, idx+1, fw.get(obj), fw.objType, styles)
    }
    def instanceFields(clazz: Class[_]) = FieldCache.getFieldWrappers(clazz)
    def exportFlatCompound(sheet: Sheet, clazz: Class[_], obj: AnyRef, styles: Map[String, CellStyle], drawing: Drawing): Option[String] = {
        val row = sheet.createRow(sheet.getPhysicalNumberOfRows)
        exportFlatCompound(row, 0, clazz, obj, styles, drawing)
        None
    }
    def exportFlatCompound(row: Row, startColumn: Int, clazz: Class[_], obj: AnyRef, styles: Map[String, CellStyle], drawing: Drawing): Option[String] = {
        val fws = instanceFields(clazz)
        checkFlatCompound(fws)
        fws.foldLeft(0) {
            case (idx, fw) =>
                fw.objType match {
                    case jl: JavaList =>
                        fw.get(obj) match {
                            case null => idx
                            case v => v.asInstanceOf[java.util.List[AnyRef]].foldLeft(idx) {
                                case (acc, x) => jl.valueType match {
                                    case JavaCompound(clz) => instanceFields(clz).foldLeft(acc) {
                                        case (i, fw) => generateValueCell(row, i+startColumn, fw.get(x), fw.objType, styles); i+1
                                    }
                                    case ot => generateValueCell(row, acc+startColumn, x, ot, styles); acc+1
                                }
                            }
                        }
                    case jm: JavaMap =>
                        fw.get(obj) match {
                            case null => idx
                            case v => v.asInstanceOf[java.util.Map[AnyRef,AnyRef]].foldLeft(idx) {
                                case (acc,(k,v)) => jm.valueType match {
                                    case JavaCompound(clz) => instanceFields(clz).foldLeft(acc) {
                                        case (i, fw) =>
                                            generateKeyCell(row, i, k, jm.keyType, styles)
                                            generateValueCell(row, i+1+startColumn, fw.get(v), fw.objType, styles); i+2
                                    }
                                    case ot =>
                                        generateKeyCell(row, acc, k, jm.keyType, styles)
                                        generateValueCell(row, acc+1+startColumn, v, ot, styles); acc+2
                                }
                            }
                        }
                    case js: JavaSet =>
                        throw new IllegalArgumentException("nested set not implemented yet, field[" + fw.name + "]")
                    case JavaCompound(_) =>
                        throw new IllegalArgumentException("exceed maximum nested compound level[2], field[" + fw.name + "]")
                    case ot =>
                        generateValueCell(row, idx+startColumn, fw.get(obj), ot, styles)
                        idx+1
                }
        }
        None
    }
    def checkFlatCompound(fws: Seq[FieldCache.FieldWrapper]): Unit = {
        fws.filter {fw => fw.objType.isCompound} .foreach {
            fw =>
                fw.objType match {
                    case jl: JavaList => jl.valueType match {
                        case JavaCompound(clazz) =>
                            if (instanceFields(clazz).filter {fw => fw.objType.isCompound} .size > 0)
                                throw new IllegalArgumentException("exceed maximum nested compound level[2], field[" + fw.name + "]")
                        case x if (x.isCompound) =>
                            throw new IllegalArgumentException("exceed maximum nested compound level[2], field[" + fw.name + "]")
                        case _ =>
                    }
                    case jm: JavaMap => jm.valueType match {
                        case JavaCompound(clazz) =>
                            // if (instanceFields(clazz).filter {fw => fw.objType.isCompound} .size > 0)
                            //     throw new IllegalArgumentException("exceed maximum nested compound level[2], field[" + fw.name + "]")
                        case x if (x.isCompound) =>
                            throw new IllegalArgumentException("exceed maximum nested compound level[2], field[" + fw.name + "]")
                        case _ =>
                    }
                    case js: JavaSet => js.valueType match {
                        case JavaCompound(clazz) =>
                            if (instanceFields(clazz).filter {fd => fw.objType.isCompound} .size > 0)
                                throw new IllegalArgumentException("exceed maximum nested compound level[2], field[" + fw.name + "]")
                        case x if (x.isCompound) =>
                            throw new IllegalArgumentException("exceed maximum nested compound level[2], field[" + fw.name + "]")
                        case _ =>
                    }
                    case JavaCompound(_) =>
                        throw new IllegalArgumentException("exceed maximum nested compound level[2], field[" + fw.name + "]")
                    case _ =>
                }
        }
    }
    def exportFlatCompoundHeader(row: Row, startColumn: Int, clazz: Class[_], obj: AnyRef, styles: Map[String, CellStyle], drawing: Drawing): Unit = {
        val fws = instanceFields(clazz)
        checkFlatCompound(fws)
        // get max cnt for each nested compound
        val maxCnt = fws.zipWithIndex.map {
            case (fw,idx) =>
                fw.objType match {
                    case jl: JavaList => fw.get(obj) match {
                        case null => 0
                        case v => v.asInstanceOf[java.util.List[AnyRef]].size * (jl.valueType match {
                            case JavaCompound(clz) => instanceFields(clz).size
                            case _ => 1
                        })
                    }
                    case jm: JavaMap => fw.get(obj) match {
                        case null => 0
                        case v => v.asInstanceOf[java.util.Map[AnyRef,AnyRef]].size * (1 + (jm.valueType match {
                            case JavaCompound(clz) => instanceFields(clz).size
                            case _ => 1
                        }))
                    }
                    case js: JavaSet =>
                        throw new IllegalArgumentException("nested set not implemented yet, field[" + fw.name + "]")
                    case JavaCompound(_) =>
                        throw new IllegalArgumentException("exceed maximum nested compound level[2], field[" + fw.name + "]")
                    case ot =>
                        generateValueCell(row, idx+startColumn, fw.get(obj), ot, styles)
                }
        }
    }
    def exportObjectList(sheet: Sheet, desc: String, objs: java.util.List[AnyRef], clazz: Class[_], styles: Map[String, CellStyle], drawing: Drawing): Unit = {
        val rowName = sheet.createRow(sheet.getPhysicalNumberOfRows)
        val cellName = rowName.createCell(0)
        cellName.setCellType(Cell.CELL_TYPE_STRING)
        cellName.setCellValue(desc)
        cellName.setCellStyle(styles("clazz"))
        val comment = drawing.createCellComment(drawing.createAnchor(0,0,0,0,1,rowName.getRowNum,4,rowName.getRowNum+3))
        comment.setString(new XSSFRichTextString("class=" + clazz.getCanonicalName))
        comment.setString(new XSSFRichTextString("keys="))
        comment.setString(new XSSFRichTextString("type=list"))
        comment.setAuthor("AIC")
        cellName.setCellComment(comment)
        sheet.addMergedRegion(new CellRangeAddress(rowName.getRowNum, rowName.getRowNum, 0, 6))

        exportJavaList(sheet, objs, ObjType(clazz), styles, drawing)
    }
    def exportCompoundField(sheet: Sheet, obj: AnyRef, fw: FieldCache.FieldWrapper, styles: Map[String, CellStyle], drawing: Drawing): Unit = {
        val rowName = sheet.createRow(sheet.getPhysicalNumberOfRows)
        val cellName = rowName.createCell(0)
        cellName.setCellType(Cell.CELL_TYPE_STRING)
        cellName.setCellValue(fw.chineseName)
        cellName.setCellStyle(styles("compound"))
        sheet.addMergedRegion(new CellRangeAddress(rowName.getRowNum, rowName.getRowNum, 0, 6))

        val collType = exportCompound(sheet, fw.get(obj), fw.objType, styles, drawing)
        val comment = drawing.createCellComment(drawing.createAnchor(0,0,0,0,1,rowName.getRowNum,4,rowName.getRowNum+3))
        comment.setAuthor("AIC")
        cellName.setCellComment(comment)
        collType match {
            case Some(t) =>
                comment.setString(new XSSFRichTextString("cfd=" + fw.name + "\ntype=" + t))
            case None =>
                comment.setString(new XSSFRichTextString("cfd=" + fw.name))
        }
    }

    def exportJavaList(sheet: Sheet, values: java.util.List[AnyRef], valueType: ObjType, styles: Map[String, CellStyle], drawing: Drawing): Option[String] = {
        valueType match {
            case JavaCompound(clz) =>
                // generate header
                val row = sheet.createRow(sheet.getPhysicalNumberOfRows)
                val fws = FieldCache.getFieldWrappers(clz)
                fws.zipWithIndex.foreach {
                    case (fw,idx) =>
                        generateHeader(row, idx, fw.chineseName, JavaString, styles)
                }
                values.foreach {exportFlatCompound(sheet, clz, _, styles, drawing)}
            case ot if ot.isCompound => throw new IllegalArgumentException("nested compound in list not implemented, field[" + ot.clazz + "]")
            case ot =>
                // generate header
                val row = sheet.createRow(sheet.getPhysicalNumberOfRows)
                generateHeader(row, 0, "values", JavaString, styles)
                values.foreach {
                    y => 
                        val row = sheet.createRow(sheet.getPhysicalNumberOfRows)
                        generateValueCell(row, 0, y, ot, styles)
                }
        }
        Some("list")
    }
    def exportJavaMap(sheet: Sheet, values: java.util.Map[AnyRef,AnyRef], keyType: ObjType, valueType: ObjType, styles: Map[String, CellStyle], drawing: Drawing): Option[String] = {
        valueType match {
            case jl: JavaList => jl.valueType match {
                case JavaCompound(clz) => 
                    // generate values
//                    if (instanceFields(clz).filter {fd => ObjType(fd.getGenericType).isCompound} .size > 0)
//                        throw new IllegalArgumentException("exceed maximum nested compound level[2], field[" + clz + "]")
//                    else {
{
                        // generate header
                        val row = sheet.createRow(sheet.getPhysicalNumberOfRows)
//                        exportFlatCompoundHeader(row, 1, clz, obj, styles, drawing)
                        // genereate value
                        values.foreach {
                            case (k,v) => v.asInstanceOf[java.util.List[AnyRef]].foreach {
                                x =>
                                    val row = sheet.createRow(sheet.getPhysicalNumberOfRows)
                                    generateKeyCell(row, 0, k, keyType, styles)
                                    exportFlatCompound(row, 1, clz, x, styles, drawing)
                            }
                        }
                        Some("maplist")
                    }
                case ot if ot.isCompound => throw new IllegalArgumentException("nested compound in map not implemented, field[" + ot + "]")
                case ot =>
                    // generate header
                    val row = sheet.createRow(sheet.getPhysicalNumberOfRows)
                    generateHeader(row, 0, "key", JavaString, styles)
                    generateHeader(row, 1, "value", JavaString, styles)
                    // genereate value
                    values.foreach {
                        case (k,v) =>
                            val row = sheet.createRow(sheet.getPhysicalNumberOfRows)
                            generateKeyCell(row, 0, k, keyType, styles)
                            generateValueCell(row, 1, v, ot, styles)
                    }
                    Some("maplist")
            }
            case jm: JavaMap =>
                if (jm.valueType.isCompound)
                    throw new IllegalArgumentException("nested compound in map not implemented, field[" + jm.valueType.clazz + "]")
                else {
                    // generate header
                    val keys = values.foldLeft(Set.empty[AnyRef]) {
                        case (acc,(k,v)) => acc ++ v.asInstanceOf[java.util.Map[AnyRef,AnyRef]].keySet
                    }.toList
                    val row = sheet.createRow(sheet.getPhysicalNumberOfRows)
                    keys.zipWithIndex.foreach {
                        case (k,idx) => generateHeader(row, idx+1, k, jm.keyType, styles)
                    }
                    values.foreach {
                        case (k,v) =>
                            val row = sheet.createRow(sheet.getPhysicalNumberOfRows)
                            generateKeyCell(row, 0, k, keyType, styles)
                            v.asInstanceOf[java.util.Map[AnyRef,AnyRef]].foreach {
                                case (k2,v2) =>
                                    val idx = keys.indexOf(k2)
                                    generateValueCell(row, idx+1, v2, jm.valueType, styles)
                        }
                    }
                    Some("mapmap")
                }
            case js: JavaSet => throw new IllegalArgumentException("nested set not implementd, field[" + js + "]")
            case JavaCompound(clz) =>
                // generate header
                val row = sheet.createRow(sheet.getPhysicalNumberOfRows)
                instanceFields(clz).zipWithIndex.foreach {
                    case (fw,idx) => generateHeader(row, idx+1, fw.chineseName, JavaString, styles)
                }
                values.foreach {
                    case (k,v) =>
                        val row = sheet.createRow(sheet.getPhysicalNumberOfRows)
                        generateKeyCell(row, 0, k, keyType, styles)
                        exportFlatCompound(row, 1, clz, v, styles, drawing)
                }
                Some("map")
            case _ =>
                values.grouped(3).foreach {
                    case kvs =>
                        val row = sheet.createRow(sheet.getPhysicalNumberOfRows)
                        kvs.zipWithIndex.foreach {
                            case ((k,v),idx) => 
                                generateFlatmapKeyCell(row, idx*2, k, keyType, styles)
                                generateValueCell(row, idx*2+1, v, valueType, styles)
                    }
                }
                Some("flatmap")
        }
    }
    def exportCompound(sheet: Sheet, value: AnyRef, ot: ObjType, styles: Map[String, CellStyle], drawing: Drawing): Option[String] = {
        value match {
            case null =>
                None
            case x: AnyRef =>
                ot match {
                    case jl: JavaList => exportJavaList(sheet, x.asInstanceOf[java.util.List[AnyRef]], jl.valueType, styles, drawing)
                    case jm: JavaMap => exportJavaMap(sheet, x.asInstanceOf[java.util.Map[AnyRef,AnyRef]], jm.keyType, jm.valueType, styles, drawing)
                    case js: JavaSet =>
                        throw new IllegalArgumentException("set not implemented yet, field[" + js + "]")
                        Some("set")
                    case JavaCompound(clz) =>
                        val fws = FieldCache.getFieldWrappers(clz)
                        val (compoundFWS,simpleFWS) = fws.partition {_.objType.isCompound}
                        simpleFWS.grouped(3) foreach {
                            fws =>
                                val row = sheet.createRow(sheet.getPhysicalNumberOfRows)
                                fws.zipWithIndex.foreach {
                                case (fw,idx) =>
                                    exportSimpleField(row, idx*2, value, fw, styles, drawing)
                                }
                        }
                        compoundFWS.foreach {
                            fw =>
                                exportCompoundField(sheet, value, fw, styles, drawing)
                        }
                        None
                    case x => throw new IllegalArgumentException("unknown compound type[" + x + "]")
                }
        }
    }
    def exportObject(sheet: Sheet, desc: String, obj: AnyRef, clazz: Class[_], styles: Map[String, CellStyle], drawing: Drawing): Unit = {
        val row = sheet.createRow(sheet.getPhysicalNumberOfRows)
        val cellClazz = row.createCell(0)
        cellClazz.setCellType(Cell.CELL_TYPE_STRING)
        cellClazz.setCellValue(desc)
        cellClazz.setCellStyle(styles("clazz"))
        val comment = drawing.createCellComment(drawing.createAnchor(0,0,0,0,1,row.getRowNum,4,row.getRowNum+3))
        comment.setString(new XSSFRichTextString("class=" + clazz.getCanonicalName))
        comment.setAuthor("AIC")
        cellClazz.setCellComment(comment)
        sheet.addMergedRegion(new CellRangeAddress(row.getRowNum, row.getRowNum, 0, 6))

        exportCompound(sheet, obj, ObjType(clazz), styles, drawing)

    }
}

