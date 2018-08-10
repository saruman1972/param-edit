package com.allinfinance.tools.param.util

import org.apache.poi.hssf.usermodel.HSSFWorkbook
import org.apache.poi.xssf.usermodel.XSSFWorkbook
import org.apache.poi.ss.usermodel.{Sheet, Row, Cell}
import java.io.FileInputStream
import scala.collection.JavaConversions._

import ObjTreeNode.{VerConfigs, VerNode, MyWorker}

object GlLoad {
    def generateStmts(sheet: Sheet, org: String, version: String)(f: (Row, String, String) => Option[String]): List[String] = {
        sheet.iterator.toList.tail.foldLeft(List.empty[String]) {(acc, row) =>
            f(row, org, version) match {
                case Some(sql) => sql :: acc
                case None => acc
            }
        } reverse
    }

    def cellValue(cell: Cell): Option[String] = {
        try {
            cell.setCellType(Cell.CELL_TYPE_STRING)
            cell.getStringCellValue match {
                case "" => None
                case v => 
Some(v.dropWhile(_.isWhitespace))
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


        // val v = try {
        //     cell.getNumericCellValue
        // }
        // catch {
        //     case _ => try {
        //         cell.getStringCellValue
        //     }
        //     catch {
        //         case e =>
        //             println("row[" + cell.getRow.getRowNum + "] col[" + cell.getColumnIndex + "] failed")
        //             e.printStackTrace
        //     }
        // }
        // v match {
        //     case _: Double => Some("%.0f".format(v))
        //     case _ => v.toString.trim match {
        //         case "" => None
        //         case s => Some(s)
        //     }
        // }
    }

    val fieldNameWithIdx = List("ntDbSubject", "ntDbRedFlag", "ntCrSubject", "ntCrRedFlag", "ntDbSubjectOs", "ntDbRedFlagOs", "ntCrSubjectOs", "ntCrRedFlagOs", 
                                "stDbSubject", "stDbRedFlag", "stCrSubject", "stCrRedFlag",
                                "woDbSubject", "woDbRedFlag", "woCrSubject", "woCrRedFlag") zip (4 to 19)

    def genTxngltStmt(row: Row, org: String, version: String): Option[String] = {
        for (txnCd <- cellValue(row.getCell(0));
             currCd <- cellValue(row.getCell(1));
             ageGroup <- cellValue(row.getCell(2));
             bnpGroup <- cellValue(row.getCell(3));
             val paramKey = txnCd + "|" + currCd + "|" + ageGroup + "|" + bnpGroup;
             val sql = "insert_update {" + version + "}.com.allinfinance.glp.param.def.TxnGlt values ORG='" + org + "', PARAM_KEY='" + paramKey + "', txnCd='" + txnCd + "', currCd='" + currCd + "', ageGroup='" + ageGroup + "', bnpGroup='" + bnpGroup + "'")
            yield(
                fieldNameWithIdx.foldLeft(sql) {(acc, pair) =>
                    cellValue(row.getCell(pair._2)) match {
                        case Some(cell) =>
                            acc + ", " + pair._1 + "='" + cell + "'"
                        case None => acc
                    }
                                                   }
            )
    }

    val typeMap = Map("资产类" -> Map("TYPE" -> "A",
                                      "BAL_DIR" -> "D",
                                      "AMT_DIR" -> "D"),
                      "负债类" -> Map("TYPE" -> "B",
                                      "BAL_DIR" -> "C",
                                      "AMT_DIR" -> "C"),
                      "损益类" -> Map("TYPE" -> "C"),
                      "共同类" -> Map("TYPE" -> "D",
                                      "BAL_DIR" -> "T",
                                      "AMT_DIR" -> "T"),
                      "表外"   -> Map("TYPE" -> "E"))
    val balDirMap = Map("借" -> "D",
                        "贷" -> "C",
                        "收" -> "D",
                        "付" -> "C")
    def genSubjectStmt(row: Row, org: String, version: String): Option[String] = {
//println(row.getCell(0).toString,row.getCell(1).toString);
        for (desc <- cellValue(row.getCell(0));
             subjectType <- cellValue(row.getCell(1));
             val typeRef = typeMap(subjectType);
             balDir <- cellValue(row.getCell(2));
             val balDbCrFlag = typeRef.getOrElse("BAL_DIR", balDirMap(balDir));
             dbcrFlag <- cellValue(row.getCell(3));
             val amtDbCrFlag = typeRef.getOrElse("AMT_DIR", if (dbcrFlag == "是") "T" else balDbCrFlag);
             subject <- cellValue(row.getCell(9));
             val sql = "insert_update {" + version + "}.com.allinfinance.glp.param.def.Subject values ORG='" + org + "', PARAM_KEY='" + subject + "', subject='" + subject + "', description='" + desc + "', balDbCrFlag='" + balDbCrFlag + "', amtDbCrFlag='" + amtDbCrFlag + "', type='" + typeRef("TYPE") + "', currCd='156'")
            yield sql
             
    }

    def genLoadStmts(excelName: String, org: String, version: String): Either[String,List[String]] = {
        val ws = try {
            new HSSFWorkbook(new FileInputStream(excelName))
        }
        catch {case e: Throwable => new XSSFWorkbook(new FileInputStream(excelName))}
        Right[String,List[String]](generateStmts(ws.getSheetAt(0), org, version)(genSubjectStmt) ++
                                   generateStmts(ws.getSheetAt(5), org, version)(genTxngltStmt))
    }

}

