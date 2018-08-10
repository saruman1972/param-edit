package com.allinfinance.tools.param.util

import scala.util.parsing.combinator._
import scala.collection.JavaConversions._

object Parser {
    import JsonDef._
    import ObjDef._
    import Expr._
    import RelOp._
    import Stmt._

    class CmdParser extends JavaTokenParsers {

        def cmds: Parser[List[Statement]] = repsep(cmd, ";")
        def cmd: Parser[Statement] = delete | select | update | copy | insert | exit | verDef | debug | list_orgs | xml_to_json

        def xml_to_json: Parser[Statement] = "convert" ~ "to" ~ "json" ~ "for" ~ clazzWithAlias ~ "from" ~ quoted_string ^^ {
            case "convert" ~ "to" ~ "json" ~ "for" ~ vc ~ "from" ~ x => XmlToJsonStmt(vc, x)
        }
        def quoted_string: Parser[String] = ("'"+"""([^'\p{Cntrl}\\]|\\[\\'bfnrt]|\\u[a-fA-F0-9]{4})*"""+"'").r ^^ {x => x.substring(1, x.size-1)}
        def list_orgs: Parser[Statement] = "list" ~ "orgs" ~ "for" ~> version ^^ {ver => ListOrgsStmt(ver)}
        def debug: Parser[Statement] = "debug" ~> on_off ^^ {x => DebugSwitchStmt(x)}
        def on_off: Parser[Boolean] = (
            "on" ^^ {x => true}
            | "off" ^^ {x => false}
        )
        def exit: Parser[Statement] = "exit" ^^ {_ => ExitStmt()}
        def verDef: Parser[Statement] = "version" ~ "=" ~> jsonObj ^^ {x => VersionDefStmt(x)}

        def select: Parser[Statement] =
            "select" ~ repsep(field, ",") ~ "from" ~ clazzWithAlias ~ where ^^ {
                case _ ~ fs ~ _ ~ vc ~ w =>
                    VersionClass.clearAlias
                    SelectStmt(fs, vc, w)
            }

        def delete: Parser[Statement] =
            "delete" ~ clazzWithAlias ~ where ^^ {
                case _ ~ vc ~ w =>
                    VersionClass.clearAlias
                    DeleteStmt(vc, w)
            }

        def update: Parser[Statement] = (
            "update" ~ clazzWithAlias ~ where ~ "set" ~ repsep(fv_pair, ",") ^^ {
                case _ ~ vc ~ w ~ _ ~ fvs =>
                    VersionClass.clearAlias
                    UpdateStmt(vc, w, None, None, fvs)
            }
            | "update" ~ clazzWithAlias ~ where ~ "from" ~ clazzWithAlias ~ where ~ "set" ~ repsep(set_pair, ",") ^^ {
                case _ ~ dstVc ~ dstWhere ~ _ ~ srcVc ~ srcWhere ~ _ ~ sps =>
                    VersionClass.clearAlias
                    UpdateStmt(dstVc, dstWhere, Some(srcVc), Some(srcWhere), sps)
            }
        )

        def copy: Parser[Statement] = (
            "copy" ~ clazzWithAlias ~ where ~ "from" ~ clazzWithAlias ~ where ~ "alter" ~ repsep(set_pair, ",") ^^ {
                case _ ~ dstVc ~ dstWhere ~ _ ~ srcVc ~ srcWhere ~ _ ~ sps =>
                    VersionClass.clearAlias
                    CopyFromStmt(dstVc, dstWhere, srcVc, srcWhere, Some(sps))
            }
            | "copy" ~ clazzWithAlias ~ where ~ "from" ~ clazzWithAlias ~ where ^^ {
                case _ ~ dstVc ~ dstWhere ~ _ ~ srcVc ~ srcWhere =>
                    VersionClass.clearAlias
                    CopyFromStmt(dstVc, dstWhere, srcVc, srcWhere, None)
            }
        )

        def insert: Parser[Statement] = (
            "insert" ~ clazzWithVersion ~ "values" ~ repsep(fv_pair, ",") ^^ {
                case _ ~ vc ~ _ ~ fvs =>
                    VersionClass.clearAlias
                    InsertStmt(vc, fvs)
            }
            | "insert_update" ~ clazzWithVersion ~ "values" ~ repsep(fv_pair, ",") ^^ {
                case _ ~ vc ~ _ ~ fvs =>
                    VersionClass.clearAlias
                    InsertUpdateStmt(vc, fvs)
            }
            | "insert" ~ clazzWithAlias ~ "from" ~ clazzWithAlias ~ where ~ "alter" ~ repsep(set_pair, ",") ^^ {
                case _ ~ dstVc ~ _ ~ srcVc ~ srcWhere ~ _ ~ ffps =>
                    VersionClass.clearAlias
                    InsertFromStmt(dstVc, srcVc, srcWhere, Some(ffps))
            }
            | "insert" ~ clazzWithAlias ~ "from" ~ clazzWithAlias ~ where ^^ {
                case _ ~ dstVc ~ _ ~ srcVc ~ srcWhere =>
                    VersionClass.clearAlias
                    InsertFromStmt(dstVc, srcVc, srcWhere, None)
            }
        )

        def where: Parser[Where] = (
            "where" ~ exp ~ "having" ~ exp ^^ {
                case _ ~ exp ~ _ ~ having =>
                    Where(exp, Some(having))
            }
            | "where" ~> exp ^^ {
                case exp =>
                    Where(exp, None)
            }
        )

        def clazzWithAlias: Parser[VersionClass] = (
            clazzWithVersion ~ "as" ~ id ^^ {case cv ~ _ ~ alias => cv.setAlias(alias); cv}
            | clazzWithVersion
        )
        def clazzWithVersion: Parser[VersionClass] = "{" ~ version ~ "}." ~ clazz ^^ {case _ ~ v ~ _ ~ c => VersionClass(v, c)}
        def version: Parser[String] = "[-a-zA-Z0-9_.]*".r
        def clazz: Parser[String] = "[a-zA-Z][a-zA-Z0-9]*\\.[a-zA-Z][a-zA-Z0-9]*(\\.[a-zA-Z][a-zA-Z0-9]*)*".r

        def index: Parser[AnyRef] = "[" ~> value <~ "]"
        def literal: Parser[Literal] = id ~ rep(index) ^^ {case literal ~ indexes => Literal(literal, indexes)}
        def field: Parser[Field] = (
            "[" ~ id ~ "]." ~ repsep(literal, ".") ^^ {case _ ~ alias ~ _ ~ literals => Field(literals)}
            | repsep(literal, ".") ^^ {case literals => Field(literals)}
        )
        def fv_pair: Parser[FVPair] = field  ~ "=" ~ json ^^ {case f ~ _ ~ v => FVPair(f, v)}
        def set_pair: Parser[SetPair] = (
            field ~ "=" ~ json ^^ {case f ~ _ ~ v => FVPair(f, v)}
            | field ~ "=" ~ field ^^ {case src ~ _ ~ dst => FFPair(src, dst)}
        )

        def exp: Parser[RelExp] = orExp
        def orExp: Parser[RelExp] = (
            andExp ~ "or" ~ andExp ^^ {case left ~ _ ~ right => OrExp(left, right)}
            | andExp
        )
        def andExp: Parser[RelExp] = (
            notExp ~ "and" ~ notExp ^^ {case left ~ _ ~ right => AndExp(left, right)}
            | notExp
        )
        def notExp: Parser[RelExp] = (
            "not" ~> simpleExp ^^ {case e => NotExp(e)}
            | simpleExp
        )
        def simpleExp: Parser[RelExp] = (
            field ~ "=" ~ value ^^ {case f ~ _ ~ v => BasicExp(EQ, f, v)}
            | field ~ "<>" ~ value ^^ {case f ~ _ ~ v => BasicExp(NE, f, v)}
            | field ~ "<" ~ value ^^ {case f ~ _ ~ v => BasicExp(LT, f, v)}
            | field ~ "<=" ~ value ^^ {case f ~ _ ~ v => BasicExp(LE, f, v)}
            | field ~ ">" ~ value ^^ {case f ~ _ ~ v => BasicExp(GT, f, v)}
            | field ~ ">=" ~ value ^^ {case f ~ _ ~ v => BasicExp(GE, f, v)}
            | field ~ "in" ~ "(" ~ repsep(value, ",") ~ ")" ^^ {case f ~ _ ~ _ ~ vs ~ _ => InExp(f, collection.mutable.ListBuffer[AnyRef]() ++ vs)}
            | field ~ "not in" ~ "(" ~ repsep(value, ",") ~ ")" ^^ {case f ~ _ ~ _ ~ vs ~ _ => NotExp(InExp(f, collection.mutable.ListBuffer[AnyRef]() ++ vs))}
            | "(" ~> exp <~ ")"
        )

        def id: Parser[String] = """[a-zA-Z]\w*""".r
        def value: Parser[AnyRef] = (
            "null" ^^ (x => null)
            | "true" ^^ {x => JavaBoolean.valueOf(x)}
            | "false" ^^ {x => JavaBoolean.valueOf(x)}
//            | ("'"+"""([^'\p{Cntrl}\\]|\\[\\'bfnrt]|\\u[a-fA-F0-9]{4})*"""+"'").r ^^ {x => x.substring(1, x.size-1)}
            | ("'"+"""([^']|\\[\\'bfnrt]|\\u[a-fA-F0-9]{4})*"""+"'").r ^^ {x => x.substring(1, x.size-1)}
            | """\d\d\d\d/\d\d/\d\d""".r ^^ {x => JavaDate.valueOf(x)}
            | decimalNumber ^^ {x => JavaBigDecimal.valueOf(x)}
        )

        def json: Parser[Json] = (
            value ^^ {x => Json(x)}
            | jsonObj
            | jsonArray
            | jsonMap
            | jsonSet
        )
        def jsonObj: Parser[JsonCompound] = "<" ~> repsep(objMember, ",") <~ ">" ^^ {x => JsonCompound(Map[String, Json]() ++ x)}
        def objMember: Parser[(String, Json)] = id ~ ":" ~ json ^^ {case id ~ _ ~ json => (id, json)}
        def jsonArray: Parser[JsonList] = "[" ~> repsep(json, ",") <~ "]" ^^ {x => JsonList(x)}
        def jsonMap: Parser[JsonMap] = "{" ~> repsep(mapMember, ",") <~ "}" ^^ {x => JsonMap(Map[Json, Json]() ++ x)}
        def mapMember: Parser[(Json, Json)] = value ~ ":" ~ json ^^ {case v ~ _ ~ json => (Json(v), json)}
        def jsonSet: Parser[JsonSet] = "@[" ~> repsep(json, ",") <~ "@]" ^^ {x => JsonSet(x.toSet)}
    }
}

