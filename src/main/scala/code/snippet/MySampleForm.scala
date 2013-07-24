package code.snippet

import scala.xml.{ NodeSeq, Text }
import net.liftweb.util._
import net.liftweb.common._
import java.util.Date
import code.lib._
import Helpers._
import net.liftweb.http.{ SHtml, S }
import net.liftweb.http.js.JsCmds
import scala.collection.mutable.ArrayBuffer
import net.liftweb.http.Templates
import net.liftweb.json.JsonDSL._
import net.liftweb.json.JsonAST._
import net.liftweb.http.JsonResponse
import net.liftweb.http.LiftRules
import net.liftweb.http.js.JE

final case class SampleItem(id: Long, name: String)

class MySampleForm extends Loggable {
  lazy val sampleItems = List(SampleItem(1, "Item #1"), SampleItem(2, "Item #2"), SampleItem(3, "Item #3"), SampleItem(4, "And one more to be sure"), SampleItem(5, "Well, the last one"), SampleItem(6, "Stop me please!"))
  lazy val selectedItemIds = ArrayBuffer[Long]()
  protected var dataTableId = ""
  //  protected var dataTableColumnDef: Seq[String] = Seq()

  def renderItem(item: SampleItem): NodeSeq => NodeSeq = {
    ":checkbox" #> SHtml.checkbox(selectedItemIds.contains(item.id), checked => {
      if (checked) {
        logger.info("Selected item ID: " + item.id)
        selectedItemIds += item.id
      }
    }) &
      ".item-name" #> item.name &
      ".item-id" #> item.id
  }

  def renderDataTable(in: NodeSeq): NodeSeq = {
    val rowTemplate = ((in \\ "tr").filter(_.attribute("data-row-template").isDefined) \ "td")
    val renderRowFunction = (ignore: String) => {
      val json = ("iTotalRecords" -> sampleItems.length) ~
        ("iTotalDisplayRecords" -> sampleItems.length) ~
        ("sEcho" -> S.param("sEcho").dmap(0)(_.toInt)) ~
        ("aaData" -> sampleItems.map(
          r => renderItem(r).apply(rowTemplate).map(
            t => JString(t.child.map(a => Html5.toString(a)).reduce((a, b) => a + b)))))
      JsonResponse(json)
    }

    S.fmapFunc(S.SFuncHolder(renderRowFunction)) { func =>
      val where: String = S.encodeURL(S.contextPath + "/" + LiftRules.ajaxPath + "?" + func + "=foo")

      //      val id = idOpt getOrElse Helpers.nextFuncName
      dataTableId = S.attr("data-table-id") getOrElse Helpers.nextFuncName
      val jsonConfigFn = S.attr("data-table-config-fn")
      val defaultConfig = """{ bProcessing: true, bServerSide: true, sAjaxSource: """ + where.encJs + "}"
      val config = jsonConfigFn match {
        case Full(s) => """jQuery.extend({}, """ + s + """(), """ + defaultConfig + """)"""
        case _ => defaultConfig
      }
      val datatableOptions = JE.JsRaw(config)

      val onLoad = JE.JsRaw("""
        jQuery(document).ready(function() {
			$("#""" + dataTableId + """").dataTable(""" + datatableOptions.toJsCmd + """);
		});""").cmd

      in ++ <head_merge>
              { JsCmds.Script(onLoad) }
            </head_merge>

    }
  }

  def render(in: NodeSeq): NodeSeq = {
    (
      "#test-button" #> SHtml.ajaxSubmit("Test it!", () => {
        logger.info("Form submitted with selectedItemIds: " + selectedItemIds)
        JsCmds.SetHtml("test-results", <p>Selected items: { selectedItemIds }</p>)
      })).apply(renderDataTable(in))
  }

}
