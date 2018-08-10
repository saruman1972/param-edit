import com.allinfinance.tools.param.util.ObjTreeNode.{VerConfig, VerNode}

import org.apache.poi.ss.usermodel.{Sheet, Row, Cell, Workbook, Font, CellStyle, Drawing, Comment}
import org.apache.poi.xssf.usermodel._
import org.apache.poi.hssf.util.{HSSFColor, CellRangeAddress}
import java.io.FileInputStream
import scala.collection.JavaConversions._


object PoiDemo {
    def haha: Unit = {
        val workbook = new XSSFWorkbook
        val sheet = workbook.createSheet("abcdef")
        val drawing = sheet.createDrawingPatriarch
println(sheet.getRow(0))
        val row = sheet.createRow(sheet.getLastRowNum)
        val cell = row.createCell(0)
println("cell=",cell)
        cell.setCellValue("aaaaaaaaaaaaaaaa")
println(sheet.getLastRowNum)
        val os = new java.io.FileOutputStream("zzzz.xlsx")
        workbook.write(os)
        os.flush
        os.close
    }
    def main(args: Array[String]): Unit = {
        import com.allinfinance.tools.param.util.ParamExport._
        val verNode = VerNode(VerConfig("local-1.1.5","jdbc:db2://localhost:50001/aicdb","glp","glp123","ver1.1.3.SNAPSHOT"))
        verNode.ensureConnected
        verNode.getClassLoader
        val sheets = List(
            SheetDesc("总体机构参数", List(("com.allinfinance.bmp.param.def.Organization","机构参数",false))),
            SheetDesc("业务机构参数", List(("com.allinfinance.cps.param.def.Organization","机构参数",false))),
            SheetDesc("产品参数", List(
                                        ("com.allinfinance.bmp.param.def.Product","产品参数",false),
                                        ("com.allinfinance.cps.param.def.ProductCredit","产品参数",false),
                                        ("com.allinfinance.cps.param.def.AuthProduct","产品参数",false))
                      ),
            SheetDesc("账户参数信息明细", List(("com.allinfinance.cps.param.def.AccountAttribute","账户参数",false))),
            SheetDesc("信用计划", List(("com.allinfinance.cps.param.def.PlanTemplate", "信用计划",false))),
            SheetDesc("交易代码", List(("com.allinfinance.cps.param.def.TxnCd", "交易代码",true))),
            SheetDesc("利率表", List(("com.allinfinance.cps.param.def.InterestTable", "利率表", true))),
            SheetDesc("还款顺序", List(("com.allinfinance.cps.param.def.PaymentHierarchy", "还款顺序", false))),
            SheetDesc("锁定码", List(("com.allinfinance.cps.param.def.BlockCode", "锁定码", true))),
            SheetDesc("信息模板", List(("com.allinfinance.bmp.param.def.BMPMessageTemplate", "信息模板", true))),
            SheetDesc("分期计划", List(("com.allinfinance.cps.param.def.LoanPlan", "分期计划", false))),
            SheetDesc("卡面代码", List(("com.allinfinance.bmp.param.def.PhyCardCd", "卡面代码", true))),
            SheetDesc("行部关系", List(("com.allinfinance.bmp.param.def.Branch", "行部关系", true)))
        )
        export("zzzz.xlsx", "111111111", verNode, sheets) {
            case (sheetDesc,idx) =>
                println("export sheet[" + sheetDesc.name, "]")
        }
//        haha
    }
}

